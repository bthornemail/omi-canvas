{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module MnemonicManifold.Canon
  ( Evidence(..)
  , CanonTriple(..)
  , decodeCanonTriples
  ) where

import Control.Applicative ((<|>))
import Data.Aeson ((.:), (.:?), FromJSON(..))
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T

import MnemonicManifold.Spec (Triple(..), Versions(..))
import MnemonicManifold.Brackets (bracketDepth)

data Evidence = Evidence
  { evDocBytes :: Int
  , evDocLines :: Int
  , evSpanStart :: Int
  , evSpanEnd :: Int
  , evLineLength :: Int
  } deriving (Eq, Show)

data CanonTriple = CanonTriple
  { ctDoc :: Text
  , ctVersions :: Versions
  , ctTriple :: Triple
  , ctEvidence :: Evidence
  , ctOrder :: Maybe Int
  , ctSubjectRefDepth :: Int
  , ctPredicateRefDepth :: Int
  , ctObjectRefDepth :: Int
  } deriving (Eq, Show)

data InputContext = InputContext
  { icDocId :: Text
  , icDocBytes :: Int
  , icDocLines :: Int
  } deriving (Eq, Show)

decodeCanonTriples :: Bool -> Text -> BL.ByteString -> Either Text [CanonTriple]
decodeCanonTriples strictMode docId input =
  let allLines = BL.lines input
      ctx = InputContext
        { icDocId = docId
        , icDocBytes = fromIntegral (BL.length input)
        , icDocLines = length allLines
        }
      items = zip3 [1..] (computeOffsets allLines) allLines
  in foldl (accum strictMode ctx) (Right []) items

computeOffsets :: [BL.ByteString] -> [(Int, Int, Int)]
computeOffsets = snd . foldl step (0, [])
  where
    step (off, acc) line =
      let start = off
          len = fromIntegral (BL.length line)
          end = start + len
      in (end + 1, acc ++ [(start, end, len)])

accum
  :: Bool
  -> InputContext
  -> Either Text [CanonTriple]
  -> (Int, (Int, Int, Int), BL.ByteString)
  -> Either Text [CanonTriple]
accum True ctx (Right acc) item = do
  case decodeLine True ctx item of
    Left err
      | "empty line" `T.isInfixOf` err -> Right acc
      | "skip " `T.isInfixOf` err -> Right acc
      | otherwise -> Left err
    Right ct -> Right (acc ++ [ct])
accum True _ (Left err) _ = Left err
accum False ctx (Right acc) item =
  case decodeLine False ctx item of
    Left _ -> Right acc
    Right ct -> Right (acc ++ [ct])
accum False _ (Left err) _ = Left err

decodeLine :: Bool -> InputContext -> (Int, (Int, Int, Int), BL.ByteString) -> Either Text CanonTriple
decodeLine strictMode ctx (lineNo, (spanStart, spanEnd, lineLen), rawLine)
  | BL.null rawLine =
      Left (prefix <> "empty line")
  | otherwise =
      case A.eitherDecode rawLine of
        Left err ->
          if strictMode
            then Left (prefix <> "invalid JSON: " <> T.pack err)
            else Left (prefix <> "skip invalid JSON")
        Right v
          | isSkippableMeta v -> Left (prefix <> "skip meta event")
          | otherwise ->
          case A.parseMaybe (parseCanonTriple ctx fallbackEv) v of
            Nothing ->
              if strictMode
                then Left (prefix <> "unrecognized canon record")
                else Left (prefix <> "skip unrecognized record")
            Just ct -> Right ct
  where
    prefix = "line " <> T.pack (show lineNo) <> ": "
    fallbackEv = Evidence
      { evDocBytes = icDocBytes ctx
      , evDocLines = icDocLines ctx
      , evSpanStart = spanStart
      , evSpanEnd = spanEnd
      , evLineLength = lineLen
      }

isSkippableMeta :: A.Value -> Bool
isSkippableMeta = \case
  A.Object o ->
    case A.parseMaybe (\oo -> (oo .:? "event" :: A.Parser (Maybe Text))) o of
      Nothing -> False
      Just Nothing -> False
      Just (Just ev) ->
        ev `elem` ["canon","series_start","series_end","canon_complete","speaker.canon.start","speaker.source.start"]
        || (not (hasAny o ["text","quote","description"]) && ev `elem` ["canon","series_start","series_end","canon_complete"])
  _ -> False
  where
    hasAny o ks = any (`KM.member` o) ks

parseCanonTriple :: InputContext -> Evidence -> A.Value -> A.Parser CanonTriple
parseCanonTriple ctx fallbackEv = A.withObject "canon" $ \o -> do
  docId <- parseDocId o
  versions <- parseVersions o
  order <- o .:? "order"
  subjectDepth0 <- o .:? "subject_ref_depth"
  predicateDepth0 <- o .:? "predicate_ref_depth"
  objectDepth0 <- o .:? "object_ref_depth"
  triple <- parseSPORecord o <|> parseNestedSPO o <|> parseEventTriple o
  evidence <- parseEvidence o <|> pure fallbackEv
  let sd = fromMaybe (bracketDepth (tSubject triple)) subjectDepth0
      pd = fromMaybe (bracketDepth (tPredicate triple)) predicateDepth0
      od = fromMaybe (bracketDepth (tObject triple)) objectDepth0
  pure $ CanonTriple
    { ctDoc = fromMaybe (icDocId ctx) docId
    , ctVersions = versions
    , ctTriple = triple
    , ctEvidence = evidence
    , ctOrder = order
    , ctSubjectRefDepth = sd
    , ctPredicateRefDepth = pd
    , ctObjectRefDepth = od
    }

parseDocId :: A.Object -> A.Parser (Maybe Text)
parseDocId o = do
  mv <- o .:? "doc" :: A.Parser (Maybe A.Value)
  case mv of
    Just (A.String t) -> pure (Just t)
    Just (A.Object d) -> d .:? "path"
    _ -> pure Nothing

parseVersions :: A.Object -> A.Parser Versions
parseVersions o = do
  lexV <- o .:? "lexicon_version"
  parV <- o .:? "parser_version"
  pure $ Versions
    { lexiconVersion = fromMaybe "canon.v1" lexV
    , parserVersion = fromMaybe "canon.v1" parV
    }

parseSPORecord :: A.Object -> A.Parser Triple
parseSPORecord o =
  Triple
    <$> o .: "subject"
    <*> o .: "predicate"
    <*> o .: "object"

parseNestedSPO :: A.Object -> A.Parser Triple
parseNestedSPO o = do
  t <- o .: "triple"
  A.withObject "triple" (\to -> Triple <$> to .: "subject" <*> to .: "predicate" <*> to .: "object") t

parseEventTriple :: A.Object -> A.Parser Triple
parseEventTriple o = do
  ev <- o .: "event"
  obj <- pickText o
  mSeries <- o .:? "series"
  mArticle <- o .:? "article"
  mId <- o .:? "id"
  mName <- o .:? "name"
  mSpeaker <- o .:? "speaker"
  mVoice <- o .:? "voice"
  mType <- o .:? "type"
  let predParts =
        catMaybes
          [ render "series" <$> mSeries
          , render "article" <$> mArticle
          , render "id" <$> mId
          , render "name" <$> mName
          , render "speaker" <$> mSpeaker
          , render "voice" <$> mVoice
          , render "type" <$> mType
          ]
      p = T.intercalate "|" predParts
  pure $ Triple ev p obj
  where
    pickText obj =
      (obj .: "text") <|> (obj .: "quote") <|> (obj .: "description")
    render k v = k <> "=" <> v

parseEvidence :: A.Object -> A.Parser Evidence
parseEvidence o = do
  eVal <- o .: "evidence"
  A.withObject "evidence" (\e -> Evidence
    <$> e .: "doc_bytes"
    <*> e .: "doc_lines"
    <*> e .: "span_start"
    <*> e .: "span_end"
    <*> (fromMaybe 0 <$> (e .:? "line_length"))
    ) eVal
