{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Desktop.MdExtract
  ( ExtractMode(..)
  , ExtractConfig(..)
  , extractNdjsonFromMarkdown
  , extractNdjsonFromTree
  ) where

import Control.Monad (forM, forM_, when)
import Data.Aeson (Value)
import Data.Aeson ((.=))
import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Char (isSpace)
import Data.Foldable (toList)
import Data.List (sort)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE
import System.Directory
import System.FilePath
import qualified Data.ByteString as BS
import qualified Data.Vector as V
import MnemonicManifold.SHA256 (sha256)

data ExtractMode
  = ModeNdjsonOnly
  | ModeAll
  deriving (Eq, Show)

data ExtractConfig = ExtractConfig
  { ecRoot :: FilePath
  , ecOut :: FilePath
  , ecStrict :: Bool
  , ecMode :: ExtractMode
  , ecLangs :: [Text]
  , ecAggregate :: Bool
  , ecLooseNdjson :: Bool
  , ecCanonFilter :: Bool
  , ecEmitProseEvents :: Bool
  , ecEmitCanvasPointers :: Bool
  } deriving (Eq, Show)

-- | Extract NDJSON records from fenced blocks in a Markdown file.
-- Output is canonicalized NDJSON: each emitted record is parsed as JSON then re-encoded with aeson.
extractNdjsonFromMarkdown :: Bool -> Bool -> Bool -> Bool -> [Text] -> FilePath -> Text -> Either Text BL.ByteString
extractNdjsonFromMarkdown strictMode looseNdjson canonFilter emitProseEvents allowedLangs relPath markdown =
  fmap (\(ndjsonOut, _canvasBlocks, _canvasPointers) -> ndjsonOut) $
    extractNdjsonFromBytes strictMode looseNdjson canonFilter emitProseEvents allowedLangs relPath (TE.encodeUtf8 markdown)

-- | Walk ecRoot and extract from all *.md files. Writes:
--   <out>/ndjson/all.ndjson (if ecAggregate)
--   <out>/ndjson/<relpath>.ndjson (always)
extractNdjsonFromTree :: ExtractConfig -> IO ()
extractNdjsonFromTree ExtractConfig{..} = do
  mdFiles <- sort <$> findMdFiles ecRoot
  createDirectoryIfMissing True (ecOut </> "ndjson")
  when (ecMode == ModeAll) $ createDirectoryIfMissing True (ecOut </> "canvas")

  let langsToUse =
        case ecMode of
          ModeNdjsonOnly -> filter (`elem` ["ndjson","jsonl","jsonlines"]) (map normalizeLang ecLangs)
          ModeAll -> map normalizeLang ecLangs
      canvasEnabled = "canvas" `elem` langsToUse

  canvasPointersAcc <- fmap concat $ forM mdFiles $ \absPath -> do
    let rel = makeRelative ecRoot absPath
    rawBytes <- BS.readFile absPath
    case extractNdjsonFromBytes ecStrict ecLooseNdjson ecCanonFilter ecEmitProseEvents langsToUse rel rawBytes of
      Left err ->
        if ecStrict
          then ioError (userError (T.unpack err))
          else do
            -- In non-strict mode, still write an empty file for reproducibility.
            let outPath = ecOut </> "ndjson" </> addExtension rel "ndjson"
            createDirectoryIfMissing True (takeDirectory outPath)
            BL.writeFile outPath BL.empty
            pure []
      Right (ndjson, canvasBlocks, canvasPointers) -> do
        let outPath = ecOut </> "ndjson" </> addExtension rel "ndjson"
        createDirectoryIfMissing True (takeDirectory outPath)
        BL.writeFile outPath ndjson
        when (ecMode == ModeAll) $ do
          when canvasEnabled $
            forM_ canvasBlocks $ \(blockIndex, canvasValue) -> do
              let canvasOut =
                    ecOut </> "canvas" </> addExtension (rel <> ".block" <> show blockIndex) "canvas.json"
              createDirectoryIfMissing True (takeDirectory canvasOut)
              BL.writeFile canvasOut (A.encode canvasValue)
        pure canvasPointers

  when ecAggregate $ do
    let allOut = ecOut </> "ndjson" </> "all.ndjson"
        -- rebuild from per-file outputs to ensure stable ordering
    perOuts <- forM mdFiles $ \absPath -> do
      let rel = makeRelative ecRoot absPath
          outPath = ecOut </> "ndjson" </> addExtension rel "ndjson"
      exists <- doesFileExist outPath
      if exists then BL.readFile outPath else pure BL.empty
    let combined = BL.unlines (filter (not . BL.null) perOuts)
    BL.writeFile allOut combined

  when ecEmitCanvasPointers $ do
    let outPath = ecOut </> "ndjson" </> "canvas.blocks.ndjson"
    BL.writeFile outPath (BL.unlines (map A.encode canvasPointersAcc))

findMdFiles :: FilePath -> IO [FilePath]
findMdFiles root = go root
  where
    go dir = do
      entries <- listDirectory dir
      paths <- forM entries $ \e -> do
        let p = dir </> e
        isDir <- doesDirectoryExist p
        if isDir
          then if shouldSkipDir e then pure [] else go p
          else pure [p | takeExtension p == ".md"]
      pure (concat paths)

    shouldSkipDir name =
      name `elem` [".git", "dist-newstyle", "node_modules", "build", ".obsidian"]

data Fence = Fence
  { fLang :: Text
  , fStartLine :: Int
  , fLines :: [Text]
  } deriving (Eq, Show)

data LineInfo = LineInfo
  { liNo :: Int
  , liStart :: Int
  , liLen :: Int
  , liBytes :: BS.ByteString
  , liText :: Text
  } deriving (Eq, Show)

data FenceInfo = FenceInfo
  { fiLang :: Text
  , fiBlockIndex :: Int
  , fiOpenLine :: Int
  , fiContentStartLine :: Int
  , fiContentEndLine :: Int
  , fiLines :: [LineInfo]
  } deriving (Eq, Show)

data CanvasBlock = CanvasBlock
  { cbBlockIndex :: Int
  , cbValue :: Value
  , cbContentStartLine :: Int
  , cbContentEndLine :: Int
  , cbSpanStart :: Int
  , cbSpanLen :: Int
  , cbRawBytes :: BS.ByteString
  } deriving (Eq, Show)

extractNdjsonFromBytes :: Bool -> Bool -> Bool -> Bool -> [Text] -> FilePath -> BS.ByteString -> Either Text (BL.ByteString, [(Int, Value)], [Value])
extractNdjsonFromBytes strictMode looseNdjson canonFilter emitProseEvents allowedLangs relPath rawBytes = do
  let docBytes = BS.length rawBytes
      lineInfos = splitLines relPath rawBytes
      docLines = length lineInfos
      langs = map normalizeLang allowedLangs
  (fences, inFenceLines) <- parseFences strictMode relPath lineInfos

  extracted <- concat <$> traverse (fenceToRecords strictMode relPath docBytes docLines langs) fences
  looseRecs <-
    if looseNdjson
      then extractLooseNdjsonRecords strictMode relPath docBytes docLines lineInfos inFenceLines
      else Right []
  proseRecs <-
    if emitProseEvents
      then extractProseEventRecords relPath docBytes docLines lineInfos inFenceLines
      else Right []

  canvasBlocks <-
    if "canvas" `elem` langs
      then extractCanvasBlocks strictMode relPath fences
      else Right []
  let canvasValues = [(cbBlockIndex cb, cbValue cb) | cb <- canvasBlocks]
      canvasPointers = [mkCanvasPointer relPath docBytes docLines cb | cb <- canvasBlocks]

  let allRecords = applyCanonFilter canonFilter (extracted <> looseRecs <> proseRecs)
      ndjsonOut = BL.unlines (map A.encode allRecords)
  pure (ndjsonOut, canvasValues, canvasPointers)

splitLines :: FilePath -> BS.ByteString -> [LineInfo]
splitLines _ bs = go 1 0 bs
  where
    go _ _ b | BS.null b = []
    go n off b =
      let (line, rest) = BS.break (== 10) b
          len = BS.length line
          text = case TE.decodeUtf8' line of
            Right t -> t
            Left _ -> TE.decodeUtf8With TEE.lenientDecode line
          li = LineInfo n off len line text
      in if BS.null rest
           then [li]
           else li : go (n + 1) (off + len + 1) (BS.drop 1 rest)

parseFences :: Bool -> FilePath -> [LineInfo] -> Either Text ([FenceInfo], [Bool])
parseFences strictMode relPath lis =
  let (fences, inFenceFlags, st, _, _) =
        foldl step ([], [], Nothing, 0 :: Int, False) lis
      result = (reverse fences, reverse inFenceFlags)
  in case st of
      Nothing -> Right result
      Just (_lang, openLine, _cur) ->
        if strictMode
          then Left (T.pack relPath <> ":" <> T.pack (show openLine) <> ": unclosed fence")
          else Right result
  where
    step (accF, accFlags, st, blockIndex, inFence) li =
      case st of
        Nothing ->
          case fenceStart (liText li) of
            Nothing -> (accF, False : accFlags, Nothing, blockIndex, False)
            Just lang ->
              ( accF
              , True : accFlags
              , Just (lang, liNo li, [])
              , blockIndex
              , True
              )
        Just (lang, openLine, cur) ->
          if fenceEnd (liText li)
            then
              let contentLines = reverse cur
                  contentStart = openLine + 1
                  contentEnd = liNo li - 1
                  fi = FenceInfo (normalizeLang lang) blockIndex openLine contentStart contentEnd contentLines
              in (fi : accF, True : accFlags, Nothing, blockIndex + 1, False)
            else (accF, True : accFlags, Just (lang, openLine, li : cur), blockIndex, True)

fenceStart :: Text -> Maybe Text
fenceStart t =
  let s = T.dropWhile isSpace t
  in if "```" `T.isPrefixOf` s
       then
         let rest = T.strip (T.drop 3 s)
         in if T.null rest
              then Just ""
              else Just (T.toLower rest)
       else Nothing

fenceEnd :: Text -> Bool
fenceEnd t = T.strip t == "```"

fenceToRecords :: Bool -> FilePath -> Int -> Int -> [Text] -> FenceInfo -> Either Text [Value]
fenceToRecords strictMode relPath docBytes docLines allowedLangs FenceInfo{..} =
  case fiLang of
    lang
      | not (null allowedLangs) && lang `notElem` allowedLangs -> Right []
      | lang `elem` ["ndjson","jsonl","jsonlines"] ->
          parseNdjsonLines strictMode relPath docBytes docLines fiLang fiBlockIndex fiContentStartLine fiContentEndLine fiLines
      | lang == "json" ->
          parseJsonBlock strictMode relPath docBytes docLines fiLang fiBlockIndex fiContentStartLine fiContentEndLine fiLines
      | lang == "hash" ->
          parseHashLines relPath docBytes docLines fiLang fiBlockIndex fiContentStartLine fiContentEndLine fiLines
      | lang == "canvas" ->
          Right []
      | otherwise -> Right []

normalizeLang :: Text -> Text
normalizeLang = T.toLower . T.takeWhile (not . isSpace)

parseNdjsonLines :: Bool -> FilePath -> Int -> Int -> Text -> Int -> Int -> Int -> [LineInfo] -> Either Text [Value]
parseNdjsonLines strictMode relPath docBytes docLines blockLang blockIndex blockLineStart blockLineEnd ls =
  fmap catMaybes $ traverse go (zip [0 :: Int ..] ls)
  where
    go (i, li) =
      let lineNo = liNo li
          t = T.strip (liText li)
      in if T.null t || isComment t
           then Right Nothing
           else case A.eitherDecodeStrict' (encodeUtf8 t) of
             Left err ->
               if strictMode
                 then Left (mkErr lineNo ("invalid JSON in ndjson block: " <> T.pack err))
                 else Right Nothing
             Right v ->
               case attachEvidence (mkEvidence relPath docBytes docLines blockLang blockIndex blockLineStart blockLineEnd (Just (i + 1)) Nothing (liStart li) (liLen li)) v of
                 Left e -> if strictMode then Left e else Right Nothing
                 Right o -> Right (Just o)

    mkErr l msg =
      T.pack relPath <> ":" <> T.pack (show l) <> ": block " <> T.pack (show blockIndex) <> ": " <> msg

isComment :: Text -> Bool
isComment t = "#" `T.isPrefixOf` t || "//" `T.isPrefixOf` t

parseJsonBlock :: Bool -> FilePath -> Int -> Int -> Text -> Int -> Int -> Int -> [LineInfo] -> Either Text [Value]
parseJsonBlock strictMode relPath docBytes docLines blockLang blockIndex blockLineStart blockLineEnd ls =
  case A.eitherDecodeStrict' (blockBytes ls) of
    Left err ->
      -- Some repos label NDJSON as ```json. If parsing as a single JSON
      -- value fails, fall back to line-by-line parsing.
      case parseNdjsonLines strictMode relPath docBytes docLines blockLang blockIndex blockLineStart blockLineEnd ls of
        Right vals | not (null vals) -> Right vals
        Right _ ->
          if strictMode
            then Left (mkErr startLine ("invalid JSON block: " <> T.pack err))
            else Right []
        Left _ ->
          if strictMode
            then Left (mkErr startLine ("invalid JSON block: " <> T.pack err))
            else Right []
    Right v -> case v of
      A.Object _ ->
        case attachEvidence (mkEvidence relPath docBytes docLines blockLang blockIndex blockLineStart blockLineEnd Nothing Nothing spanStart spanLen) v of
          Left e -> if strictMode then Left e else Right []
          Right o -> Right [o]
      A.Array arr ->
        fmap catMaybes $ traverse (oneArray spanStart spanLen) (zip [0 :: Int ..] (V.toList arr))
      _ ->
        if strictMode
          then Left (mkErr startLine "json block must be object or array")
          else Right []
  where
    startLine = blockLineStart
    mkErr l msg = T.pack relPath <> ":" <> T.pack (show l) <> ": block " <> T.pack (show blockIndex) <> ": " <> msg
    (spanStart, spanLen) =
      case ls of
        [] -> (0, 0)
        (x:_) ->
          let endLi = last ls
              start = liStart x
              end = liStart endLi + liLen endLi
          in (start, end - start)

    oneArray sStart sLen (idx, item) =
      case item of
        A.Object _ ->
          case attachEvidence (mkEvidence relPath docBytes docLines blockLang blockIndex blockLineStart blockLineEnd Nothing (Just idx) sStart sLen) item of
            Left _ -> if strictMode then Left (mkErr startLine "json array element must be object") else Right Nothing
            Right o -> Right (Just o)
        _ ->
          if strictMode then Left (mkErr startLine "json array element must be object") else Right Nothing

parseHashLines :: FilePath -> Int -> Int -> Text -> Int -> Int -> Int -> [LineInfo] -> Either Text [Value]
parseHashLines relPath docBytes docLines blockLang blockIndex blockLineStart blockLineEnd ls =
  fmap catMaybes $ traverse mk (zip [0 :: Int ..] ls)
  where
    mk (i, li) =
      let t = T.strip (liText li)
          lineNo = liNo li
      in if T.null t || isComment t
           then Right Nothing
           else
             let v = A.object
                   [ "event" .= ("hash" :: Text)
                   , "doc" .= relPath
                   , "block_index" .= blockIndex
                   , "line" .= lineNo
                   , "value" .= t
                   ]
             in case attachEvidence (mkEvidence relPath docBytes docLines blockLang blockIndex blockLineStart blockLineEnd (Just (i + 1)) Nothing (liStart li) (liLen li)) v of
                  Left e -> Left e
                  Right o -> Right (Just o)

encodeUtf8 :: Text -> BS.ByteString
encodeUtf8 = TE.encodeUtf8

extractLooseNdjsonRecords :: Bool -> FilePath -> Int -> Int -> [LineInfo] -> [Bool] -> Either Text [Value]
extractLooseNdjsonRecords strictMode relPath docBytes docLines lis inFenceFlags =
  fmap catMaybes $ traverse one (zip lis inFenceFlags)
  where
    one (li, inFence) =
      if inFence
        then Right Nothing
        else
          let t = T.strip (liText li)
          in if looksLikeJson t
               then case A.eitherDecodeStrict' (encodeUtf8 t) of
                 Left err ->
                   if strictMode
                     then Left (mkErr (liNo li) ("invalid JSON on loose line: " <> T.pack err))
                     else Right Nothing
                 Right v ->
                   case attachEvidence (mkEvidence relPath docBytes docLines "loose" (-1) (liNo li) (liNo li) (Just 1) Nothing (liStart li) (liLen li)) v of
                     Left e -> if strictMode then Left e else Right Nothing
                     Right o -> Right (Just o)
               else Right Nothing

    looksLikeJson t =
      -- Heuristic for "loose NDJSON": only attempt JSON parsing on lines that
      -- look like JSON objects (to avoid catching editorial markup like `{, Start ...}`).
      T.isPrefixOf "{" t && T.isSuffixOf "}" t && ("\":"
        `T.isInfixOf` t)

    mkErr l msg = T.pack relPath <> ":" <> T.pack (show l) <> ": " <> msg

blockBytes :: [LineInfo] -> BS.ByteString
blockBytes ls = BS.intercalate "\n" (map liBytes ls)

extractProseEventRecords :: FilePath -> Int -> Int -> [LineInfo] -> [Bool] -> Either Text [Value]
extractProseEventRecords relPath docBytes docLines lis inFenceFlags =
  Right (go 1 (zip lis inFenceFlags))
  where
    seriesName = detectSeries relPath
    articleName = T.pack (takeBaseName relPath)

    go _ [] = []
    go n xs =
      case dropWhile shouldSkip xs of
        [] -> []
        rest ->
          let (para, tail') = span isParaLine rest
          in if null para
               then go n tail'
               else mkPara n para : go (n + 1) tail'

    shouldSkip (li, inFence) =
      inFence || T.null (T.strip (liText li))

    isParaLine (li, inFence) =
      (not inFence) && (not (T.null (T.strip (liText li))))

    mkPara idx para =
      let ls0 = map fst para
          startLi = head ls0
          endLi = last ls0
          spanStart = liStart startLi
          spanEnd = liStart endLi + liLen endLi
          spanLen = spanEnd - spanStart
          txt = T.intercalate "\n" (map (T.stripEnd . liText) ls0)
          v =
            A.object
              [ "event" .= ("paragraph" :: Text)
              , "text" .= txt
              , "doc" .= relPath
              , "series" .= seriesName
              , "article" .= articleName
              , "id" .= ("p" <> T.pack (show idx))
              , "order" .= idx
              , "parser_version" .= ("md.prose.v1" :: Text)
              ]
          ev =
            mkEvidence
              relPath
              docBytes
              docLines
              "prose"
              (-2)
              (liNo startLi)
              (liNo endLi)
              (Just idx)
              Nothing
              spanStart
              spanLen
      in case attachEvidence ev v of
          Left _ -> v
          Right o -> o

detectSeries :: FilePath -> Text
detectSeries rel =
  case dropWhile (/= "narrative-series") (splitDirectories rel) of
    ("narrative-series" : series : _) -> T.pack series
    _ -> ""

mkEvidence
  :: FilePath
  -> Int
  -> Int
  -> Text
  -> Int
  -> Int
  -> Int
  -> Maybe Int
  -> Maybe Int
  -> Int
  -> Int
  -> Value
mkEvidence relPath docBytes docLines blockLang blockIndex blockLineStart blockLineEnd mLineNo mArrayIndex spanStart spanLen =
  let base =
        [ "doc_path" .= relPath
        , "doc_bytes" .= docBytes
        , "doc_lines" .= docLines
        , "block_lang" .= blockLang
        , "block_index" .= blockIndex
        , "block_line_start" .= blockLineStart
        , "block_line_end" .= blockLineEnd
        , "span_start" .= spanStart
        , "span_end" .= (spanStart + spanLen)
        , "line_length" .= spanLen
        ]
      extras =
        catMaybes
          [ ("line_no" .=) <$> mLineNo
          , ("array_index" .=) <$> mArrayIndex
          ]
  in A.object (base <> extras)

attachEvidence :: Value -> Value -> Either Text Value
attachEvidence evidence = \case
  A.Object o ->
    if KM.member "evidence" o
      then Right (A.Object (KM.insert "evidence_md" evidence o))
      else Right (A.Object (KM.insert "evidence" evidence o))
  _ -> Left "extracted record must be a JSON object"

extractCanvasBlocks :: Bool -> FilePath -> [FenceInfo] -> Either Text [CanvasBlock]
extractCanvasBlocks strictMode relPath fences = do
  let canvasFences = [ f | f <- fences, fiLang f == "canvas" ]
  fmap catMaybes $ traverse one canvasFences
  where
    one FenceInfo{..} = do
      case fiLines of
        [] -> Right Nothing
        (x:_) -> do
          let endLi = last fiLines
              spanStart = liStart x
              spanLen = (liStart endLi + liLen endLi) - spanStart
              b = blockBytes fiLines
          case A.eitherDecodeStrict' b of
            Left err ->
              if strictMode then Left (T.pack relPath <> ":" <> T.pack (show fiOpenLine) <> ": invalid canvas JSON: " <> T.pack err) else Right Nothing
            Right v ->
              case v of
                A.Object _ ->
                  Right (Just (CanvasBlock fiBlockIndex v fiContentStartLine fiContentEndLine spanStart spanLen b))
                _ ->
                  if strictMode then Left (T.pack relPath <> ":" <> T.pack (show fiOpenLine) <> ": canvas block must be a JSON object") else Right Nothing

mkCanvasPointer :: FilePath -> Int -> Int -> CanvasBlock -> Value
mkCanvasPointer relPath docBytes docLines CanvasBlock{..} =
  let outRel = "canvas/" <> T.pack (addExtension (relPath <> ".block" <> show cbBlockIndex) "canvas.json")
      shaHex = "sha256:" <> hex (sha256 cbRawBytes)
      ev = mkEvidence relPath docBytes docLines "canvas" cbBlockIndex cbContentStartLine cbContentEndLine Nothing Nothing cbSpanStart cbSpanLen
  in A.object
        [ "event" .= ("canvas.block" :: Text)
        , "doc" .= relPath
        , "block_index" .= cbBlockIndex
        , "canvas_path" .= outRel
        , "canvas_sha256" .= shaHex
        , "evidence" .= ev
        ]

hex :: BS.ByteString -> Text
hex bs = T.concat (map byteHex (BS.unpack bs))
  where
    byteHex w =
      let digits = "0123456789abcdef"
          hi = fromIntegral (w `div` 16)
          lo = fromIntegral (w `mod` 16)
      in T.pack [digits !! hi, digits !! lo]

applyCanonFilter :: Bool -> [Value] -> [Value]
applyCanonFilter False = id
applyCanonFilter True = filter isCanonClause

isCanonClause :: Value -> Bool
isCanonClause = \case
  A.Object o -> isHash o || isSPO o || isNestedSPO o || isEventClause o
  _ -> False
  where
    hasAll o ks = all (`KM.member` o) ks
    hasAny o ks = any (`KM.member` o) ks

    isHash o =
      case KM.lookup "event" o of
        Just (A.String "hash") -> True
        _ -> False

    isSPO o = hasAll o ["subject","predicate","object"]

    isNestedSPO o =
      case KM.lookup "triple" o of
        Just (A.Object t) -> hasAll t ["subject","predicate","object"]
        _ -> False

    isEventClause o = hasAll o ["event"] && hasAny o ["text","quote","description"]
