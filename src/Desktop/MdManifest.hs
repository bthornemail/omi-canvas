{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Desktop.MdManifest
  ( ManifestOptions(..)
  , writeManifest
  ) where

import Control.Monad (forM, when)
import Data.Char (isSpace)
import Data.List (nub, sort, sortOn)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import System.Directory
  ( doesDirectoryExist
  , doesFileExist
  , getCurrentDirectory
  , listDirectory
  )
import System.Environment (lookupEnv)
import System.FilePath
  ( (</>)
  , makeRelative
  , splitDirectories
  , takeExtension
  )

import MnemonicManifold.JsonText
  ( jsonArray
  , jsonBool
  , jsonInt
  , jsonInteger
  , jsonNull
  , jsonObj
  , jsonText
  )
import MnemonicManifold.SHA256 (sha256)

data ManifestOptions = ManifestOptions
  { moRoot :: FilePath
  , moOut :: FilePath
  , moMode :: Text
  , moLangs :: [Text]
  , moStrict :: Bool
  , moAggregate :: Bool
  , moLooseNdjson :: Bool
  , moCanonFilter :: Bool
  , moEmitProseEvents :: Bool
  , moEmitCanvasPointers :: Bool
  , moEmitManifest :: Bool
  , moManifestPath :: FilePath
  , moIncludeGitHead :: Bool
  , moTimestamp :: Bool
  , moToolName :: Text
  , moToolVersion :: Text
  } deriving (Eq, Show)

writeManifest :: ManifestOptions -> IO ()
writeManifest ManifestOptions{..} = when moEmitManifest $ do
  inputPaths <- sort <$> findMdFiles moRoot
  inputs <- forM inputPaths $ \absPath -> do
    bs <- BS.readFile absPath
    let rel = makeRelative moRoot absPath
    pure (rel, fileStats bs)

  outputsAggregate <- getAggregateOutputs moOut moAggregate moEmitCanvasPointers
  outputFiles <- getPerFileOutputs moOut

  gitCommit <- if moIncludeGitHead then resolveGitHead moStrict else pure Nothing
  timeVal <- if moTimestamp then resolveSourceDateEpoch else pure Nothing

  let baseFields =
        [ ("version", jsonText "ulp.manifest.v0.2")
        , ("root", jsonText (T.pack moRoot))
        , ("inputs", jsonArray (map renderInput inputs))
        , ("outputs", renderOutputs outputsAggregate outputFiles)
        , ("config", renderConfig)
        , ("tool", renderTool gitCommit)
        , ("time", maybe jsonNull jsonInteger timeVal)
        ]
      baseText = jsonObj baseFields
      baseBytes = TE.encodeUtf8 baseText <> "\n"
      rootHex = hex (sha256 baseBytes)
      finalFields =
        case baseFields of
          (v@(k,_):rest) | k == "version" ->
            v : ("root_sha256", jsonText rootHex) : rest
          _ ->
            ("root_sha256", jsonText rootHex) : baseFields
      finalText = jsonObj finalFields
      outBytes = BL.fromStrict (TE.encodeUtf8 finalText) <> "\n"
  BL.writeFile moManifestPath outBytes
  where
    renderConfig =
      let langsSorted = sort (nub (map normalizeLang moLangs))
      in jsonObj
          [ ("mode", jsonText moMode)
          , ("langs", jsonArray (map jsonText langsSorted))
          , ("strict", jsonBool moStrict)
          , ("aggregate", jsonBool moAggregate)
          , ("loose_ndjson", jsonBool moLooseNdjson)
          , ("canon_filter", jsonBool moCanonFilter)
          , ("emit_prose_events", jsonBool moEmitProseEvents)
          , ("emit_canvas_pointers", jsonBool moEmitCanvasPointers)
          ]

    renderTool gitCommit =
      jsonObj
        [ ("name", jsonText moToolName)
        , ("version", jsonText moToolVersion)
        , ("git_commit", maybe jsonNull jsonText gitCommit)
        ]

renderInput :: (FilePath, FileStats) -> Text
renderInput (rel, FileStats{..}) =
  jsonObj
    [ ("path", jsonText (T.pack rel))
    , ("sha256", jsonText fsSha256Hex)
    , ("bytes", jsonInt fsBytes)
    , ("lines", jsonInt fsLines)
    ]

data FileStats = FileStats
  { fsSha256Hex :: Text
  , fsBytes :: Int
  , fsLines :: Int
  , fsRecords :: Maybe Int
  } deriving (Eq, Show)

fileStats :: BS.ByteString -> FileStats
fileStats bs =
  let bytes = BS.length bs
      linesCount = countLines bs
      shaHex = hex (sha256 bs)
  in FileStats shaHex bytes linesCount Nothing

ndjsonStats :: BS.ByteString -> FileStats
ndjsonStats bs =
  let s@FileStats{..} = fileStats bs
      records = countNonEmptyLines bs
  in s { fsRecords = Just records }

renderOutputs :: [(FilePath, FileStats)] -> [(FilePath, FileStats)] -> Text
renderOutputs aggregate files =
  jsonObj
    [ ("aggregate", jsonObj (map renderAggregateKV (sortOn fst aggregate)))
    , ("files", jsonArray (map renderOutputEntry (sortOn fst files)))
    ]
  where
    renderAggregateKV (p, st) = (T.pack p, renderOutputStats st)

renderOutputEntry :: (FilePath, FileStats) -> Text
renderOutputEntry (p, st) =
  jsonObj (("path", jsonText (T.pack p)) : renderOutputFields st)

renderOutputStats :: FileStats -> Text
renderOutputStats st = jsonObj (renderOutputFields st)

renderOutputFields :: FileStats -> [(Text, Text)]
renderOutputFields FileStats{..} =
  [ ("sha256", jsonText fsSha256Hex)
  , ("bytes", jsonInt fsBytes)
  , ("lines", jsonInt fsLines)
  ]
  <> case fsRecords of
      Nothing -> []
      Just n -> [("records", jsonInt n)]

getAggregateOutputs :: FilePath -> Bool -> Bool -> IO [(FilePath, FileStats)]
getAggregateOutputs out aggregate emitCanvasPointers = do
  let allPath = "ndjson/all.ndjson"
      ptrPath = "ndjson/canvas.blocks.ndjson"
  xs <- fmap catMaybes $ forM
    ( [ (aggregate, allPath, True)
      , (emitCanvasPointers, ptrPath, True)
      ]
    ) $ \(enabled, rel, isNdjson) -> do
      if not enabled
        then pure Nothing
        else do
          let absPath = out </> rel
          exists <- doesFileExist absPath
          if not exists
            then pure (Just (rel, if isNdjson then ndjsonStats BS.empty else fileStats BS.empty))
            else do
              bs <- BS.readFile absPath
              pure (Just (rel, if isNdjson then ndjsonStats bs else fileStats bs))
  pure xs

getPerFileOutputs :: FilePath -> IO [(FilePath, FileStats)]
getPerFileOutputs out = do
  ndjsonFiles <- sort <$> findFilesWithExt (out </> "ndjson") ".ndjson"
  canvasFiles <- sort <$> findFilesWithExt (out </> "canvas") ".json"
  ndjsonEntries <- fmap catMaybes $ forM ndjsonFiles $ \absPath -> do
    let rel = makeRelative out absPath
    if rel == "ndjson/all.ndjson" || rel == "ndjson/canvas.blocks.ndjson"
      then pure Nothing
      else do
        bs <- BS.readFile absPath
        pure (Just (rel, ndjsonStats bs))
  canvasEntries <- fmap catMaybes $ forM canvasFiles $ \absPath -> do
    let rel = makeRelative out absPath
    if takeExtension rel /= ".json"
      then pure Nothing
      else do
        bs <- BS.readFile absPath
        pure (Just (rel, fileStats bs))
  pure (ndjsonEntries <> canvasEntries)

findMdFiles :: FilePath -> IO [FilePath]
findMdFiles root = go root
  where
    go dir = do
      exists <- doesDirectoryExist dir
      if not exists then pure [] else do
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

findFilesWithExt :: FilePath -> String -> IO [FilePath]
findFilesWithExt root ext = go root
  where
    go dir = do
      exists <- doesDirectoryExist dir
      if not exists then pure [] else do
        entries <- listDirectory dir
        paths <- forM entries $ \e -> do
          let p = dir </> e
          isDir <- doesDirectoryExist p
          if isDir then go p else pure [p | takeExtension p == ext]
        pure (concat paths)

countLines :: BS.ByteString -> Int
countLines bs
  | BS.null bs = 0
  | otherwise =
      let n = BS.count 10 bs
      in if BS.last bs == 10 then n else n + 1

countNonEmptyLines :: BS.ByteString -> Int
countNonEmptyLines bs =
  length [() | l <- BS.split 10 bs, not (BS.all isSpaceW8 l)]
  where
    isSpaceW8 w = w == 32 || w == 9 || w == 13

normalizeLang :: Text -> Text
normalizeLang = T.toLower . T.takeWhile (not . isSpace)

hex :: BS.ByteString -> Text
hex bs = T.concat (map byteHex (BS.unpack bs))
  where
    byteHex w =
      let digits = "0123456789abcdef"
          hi = fromIntegral (w `div` 16)
          lo = fromIntegral (w `mod` 16)
      in T.pack [digits !! hi, digits !! lo]

resolveSourceDateEpoch :: IO (Maybe Integer)
resolveSourceDateEpoch = do
  v <- lookupEnv "SOURCE_DATE_EPOCH"
  case v of
    Nothing -> ioError (userError "SOURCE_DATE_EPOCH is required when --timestamp is set")
    Just s ->
      case reads s of
        [(n, rest)] | all isSpace rest -> pure (Just n)
        _ -> ioError (userError "Invalid SOURCE_DATE_EPOCH")

resolveGitHead :: Bool -> IO (Maybe Text)
resolveGitHead strictMode = do
  mGitDir <- findGitDir
  case mGitDir of
    Nothing ->
      if strictMode then ioError (userError "Unable to locate .git directory") else pure Nothing
    Just gitDir -> do
      let headPath = gitDir </> "HEAD"
      exists <- doesFileExist headPath
      if not exists
        then if strictMode then ioError (userError "Missing .git/HEAD") else pure Nothing
        else do
          headBytes <- BS.readFile headPath
          let headTxt = T.strip (TE.decodeUtf8 headBytes)
          case T.stripPrefix "ref:" headTxt of
            Just refRaw -> do
              let ref = T.unpack (T.strip refRaw)
                  refPath = gitDir </> ref
              refExists <- doesFileExist refPath
              if not refExists
                then if strictMode then ioError (userError "Unable to resolve .git HEAD ref") else pure Nothing
                else do
                  refBytes <- BS.readFile refPath
                  pure (Just (T.strip (TE.decodeUtf8 refBytes)))
            Nothing -> pure (Just headTxt)

findGitDir :: IO (Maybe FilePath)
findGitDir = do
  cwd <- getCurrentDirectory
  go (splitDirectories cwd)
  where
    go [] = pure Nothing
    go parts = do
      let dir = foldl1 (</>) parts
          gitDir = dir </> ".git"
      exists <- doesDirectoryExist gitDir
      if exists then pure (Just gitDir) else go (initSafe parts)

    initSafe xs = case xs of
      [] -> []
      _ -> init xs
