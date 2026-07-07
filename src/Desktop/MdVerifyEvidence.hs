{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Desktop.MdVerifyEvidence
  ( VerifyConfig(..)
  , verifyEvidenceNdjsonBytes
  , verifyEvidenceFile
  ) where

import Control.Monad (forM_, when)
import Data.Aeson (Value)
import qualified Data.Aeson as A
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Char (isSpace)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE
import Data.Vector ((!?), Vector)
import qualified Data.Vector as V
import Data.Word (Word8)
import qualified Data.Scientific as Sci
import System.Directory (doesFileExist)
import System.FilePath ((</>))

import MnemonicManifold.SHA256 (sha256)

data VerifyConfig = VerifyConfig
  { vcRoot :: FilePath
  , vcStrict :: Bool
  } deriving (Eq, Show)

verifyEvidenceFile :: VerifyConfig -> FilePath -> IO (Either Text ())
verifyEvidenceFile cfg path = do
  bs <- BL.readFile path
  verifyEvidenceNdjsonBytes cfg bs

verifyEvidenceNdjsonBytes :: VerifyConfig -> BL.ByteString -> IO (Either Text ())
verifyEvidenceNdjsonBytes VerifyConfig{..} input = do
  let ls = BL.split 10 input
  go Map.empty 1 ls
  where
    go :: Map FilePath BS.ByteString -> Int -> [BL.ByteString] -> IO (Either Text ())
    go cache _ [] = pure (Right ())
    go cache lineNo (l:rest)
      | BL.all isSpaceW8 l = go cache (lineNo + 1) rest
      | otherwise =
          case A.eitherDecode l :: Either String Value of
            Left err ->
              if vcStrict
                then pure (Left (mkErr lineNo ("invalid NDJSON record: " <> T.pack err)))
                else go cache (lineNo + 1) rest
            Right v ->
              case v of
                A.Object o -> do
                  case extractEvidence o of
                    Nothing ->
                      if vcStrict
                        then pure (Left (mkErr lineNo "missing evidence/evidence_md"))
                        else go cache (lineNo + 1) rest
                    Just ev -> do
                      case parseEvidence ev of
                        Left e ->
                          pure (Left (mkErr lineNo ("invalid evidence: " <> e)))
                        Right evi -> do
                          (cache', mDocBytes) <- loadDoc vcRoot cache (evDocPath evi)
                          case mDocBytes of
                            Nothing ->
                              if vcStrict
                                then pure (Left (mkErr lineNo ("missing source file: " <> T.pack (evDocPath evi))))
                                else go cache' (lineNo + 1) rest
                            Just docBytes -> do
                              case verifyOne docBytes o evi of
                                Left e -> pure (Left (mkErr lineNo e))
                                Right () -> go cache' (lineNo + 1) rest
                _ ->
                  if vcStrict
                    then pure (Left (mkErr lineNo "record must be a JSON object"))
                    else go cache (lineNo + 1) rest

    mkErr :: Int -> Text -> Text
    mkErr ln msg = "ndjson:" <> T.pack (show ln) <> ": " <> msg

data EvidenceInfo = EvidenceInfo
  { evDocPath :: FilePath
  , evSpanStart :: Int
  , evSpanEnd :: Int
  , evBlockLang :: Text
  , evArrayIndex :: Maybe Int
  } deriving (Eq, Show)

extractEvidence :: KM.KeyMap Value -> Maybe Value
extractEvidence o =
  case KM.lookup "evidence_md" o of
    Just v -> Just v
    Nothing -> KM.lookup "evidence" o

parseEvidence :: Value -> Either Text EvidenceInfo
parseEvidence = \case
  A.Object o -> do
    docPath <- reqText o "doc_path"
    spanStart <- reqInt o "span_start"
    spanEnd <- reqInt o "span_end"
    blockLang <- reqText o "block_lang"
    let arrIdx = optInt o "array_index"
    pure
      EvidenceInfo
        { evDocPath = T.unpack docPath
        , evSpanStart = spanStart
        , evSpanEnd = spanEnd
        , evBlockLang = blockLang
        , evArrayIndex = arrIdx
        }
  _ -> Left "evidence must be an object"
  where
    reqText o k = case KM.lookup (K.fromText k) o of
      Just (A.String t) -> Right t
      _ -> Left ("missing/invalid " <> k)
    reqInt o k = case KM.lookup (K.fromText k) o of
      Just (A.Number n) ->
        case Sci.toBoundedInteger n of
          Just i -> Right i
          Nothing -> Left ("non-integer " <> k)
      _ -> Left ("missing/invalid " <> k)
    optInt o k = case KM.lookup (K.fromText k) o of
      Just (A.Number n) -> Sci.toBoundedInteger n
      _ -> Nothing

loadDoc :: FilePath -> Map FilePath BS.ByteString -> FilePath -> IO (Map FilePath BS.ByteString, Maybe BS.ByteString)
loadDoc root cache relPath = do
  case Map.lookup relPath cache of
    Just bs -> pure (cache, Just bs)
    Nothing -> do
      let absPath = root </> relPath
      exists <- doesFileExist absPath
      if not exists
        then pure (cache, Nothing)
        else do
          bs <- BS.readFile absPath
          pure (Map.insert relPath bs cache, Just bs)

verifyOne :: BS.ByteString -> KM.KeyMap Value -> EvidenceInfo -> Either Text ()
verifyOne docBytes recObj EvidenceInfo{..} = do
  when (evSpanStart < 0 || evSpanEnd < evSpanStart) $
    Left "invalid span bounds"
  when (evSpanEnd > BS.length docBytes) $
    Left "span_end beyond file length"

  let slice = BS.take (evSpanEnd - evSpanStart) (BS.drop evSpanStart docBytes)

  case (KM.lookup "event" recObj, evBlockLang) of
    (Just (A.String "hash"), "hash") ->
      case KM.lookup "value" recObj of
        Just (A.String v) ->
          let got = T.strip (decodeLenient slice)
          in if got == v then Right () else Left "hash evidence slice does not match value"
        _ -> Left "hash record missing value"
    (Just (A.String "paragraph"), "prose") ->
      case KM.lookup "text" recObj of
        Just (A.String t) ->
          let got = decodeLenient slice
          in if got == t then Right () else Left "prose evidence slice does not match paragraph text"
        _ -> Left "paragraph record missing text"
    (Just (A.String "canvas.block"), "canvas") ->
      case KM.lookup "canvas_sha256" recObj of
        Just (A.String t) ->
          let expected = "sha256:" <> hex (sha256 slice)
          in if t == expected then Right () else Left "canvas_sha256 does not match sha256(evidence slice)"
        _ -> Left "canvas.block record missing canvas_sha256"
    _ ->
      case evArrayIndex of
        Nothing -> do
          parsed <- parseJsonValue slice
          compareAsObject parsed
        Just idx -> do
          parsed <- parseJsonValue slice
          case parsed of
            A.Array arr ->
              case arr !? idx of
                Nothing -> Left "array_index out of bounds for evidence slice"
                Just v -> compareAsObject v
            _ -> Left "evidence slice is not a JSON array but array_index is present"
  where
    decodeLenient = TE.decodeUtf8With TEE.lenientDecode

    parseJsonValue bs =
      case A.eitherDecodeStrict' bs of
        Left err -> Left ("evidence slice is not valid JSON: " <> T.pack err)
        Right v -> Right v

    compareAsObject parsed =
      case parsed of
        A.Object o2 ->
          let recSans = stripEvidence recObj
          in if A.Object recSans == A.Object o2
               then Right ()
               else Left "evidence slice JSON does not match record (ignoring evidence/evidence_md)"
        _ -> Left "evidence slice JSON is not an object"

stripEvidence :: KM.KeyMap Value -> KM.KeyMap Value
stripEvidence = KM.delete "evidence_md" . KM.delete "evidence"

hex :: BS.ByteString -> Text
hex bs = T.concat (map byteHex (BS.unpack bs))
  where
    byteHex w =
      let digits = "0123456789abcdef"
          hi = fromIntegral (w `div` 16)
          lo = fromIntegral (w `mod` 16)
      in T.pack [digits !! hi, digits !! lo]

isSpaceW8 :: Word8 -> Bool
isSpaceW8 w = w == 32 || w == 9 || w == 13
