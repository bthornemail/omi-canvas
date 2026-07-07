-- File: app/Main.hs
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Desktop.CanvasEDSL hiding (Left, Right)
import Desktop.CanvasEDSL (Side(Top, Bottom))
import Desktop.TreeCanvas
import qualified Desktop.CanvasEDSL as C
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Char8 as BSC
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Maybe (catMaybes, fromMaybe, isJust)
import Data.Char (isSpace)
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hPutStrLn, stderr)
import System.Directory (doesFileExist, createDirectoryIfMissing)
import System.FilePath ((</>), takeDirectory)
import Text.Read (readMaybe)
import Control.Monad (when, forM_)
import Control.Concurrent (threadDelay)
import Data.List (sortBy, intercalate)
import Data.Function (on)
import qualified Data.Map as Map
import Data.Aeson (encode, decode)
import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as KM
import Data.Foldable (asum)
import Options.Applicative
import Data.Version (showVersion)
import Paths_json_canvas_cli (version)
import MnemonicManifold.Canon (decodeCanonTriples)
import MnemonicManifold.Emit (BuildRootInfo(..), EmitOptions(..), emitStaticFanoEvents, emitClauseEvents)
import Desktop.MdExtract (ExtractConfig(..), ExtractMode(..), extractNdjsonFromTree)
import Desktop.MdManifest (ManifestOptions(..), writeManifest)
import Desktop.MdVerifyEvidence (VerifyConfig(..), verifyEvidenceNdjsonBytes)

-- | Command line options
data Options = Options
  { optCommand :: Command
  , optVerbose :: Bool
  } deriving (Show)

data Command
  = Create CreateOptions
  | View ViewOptions
  | List ListOptions
  | Export ExportOptions
  | Import ImportOptions
  | Stream StreamOptions
  | Validate ValidateOptions
  | Query QueryOptions
  | Transform TransformOptions
  | Stats StatsOptions
  | FromTree TreeOptions
  | WatchTree WatchOptions
  | MnemonicManifold MnemonicManifoldCommand
  | Md MdCommand
  deriving (Show)

data MdCommand
  = MdExtract MdExtractOptions
  | MdVerifyEvidence MdVerifyEvidenceOptions
  deriving (Show)

data MdExtractOptions = MdExtractOptions
  { mdRoot :: FilePath
  , mdOut :: FilePath
  , mdStrict :: Bool
  , mdMode :: ExtractMode
  , mdLangs :: String
  , mdAggregate :: Bool
  , mdLooseNdjson :: Bool
  , mdCanonFilter :: Bool
  , mdEmitProseEvents :: Bool
  , mdEmitCanvasPointers :: Bool
  , mdEmitManifest :: Bool
  , mdManifestPath :: Maybe FilePath
  , mdIncludeGitHead :: Bool
  , mdTimestamp :: Bool
  } deriving (Show)

data MdVerifyEvidenceOptions = MdVerifyEvidenceOptions
  { mveIn :: FilePath
  , mveRoot :: FilePath
  , mveStrict :: Bool
  } deriving (Show)

data MnemonicManifoldCommand
  = MMEmit MMEmitOptions
  deriving (Show)

data MMEmitOptions = MMEmitOptions
  { mmIn :: FilePath
  , mmOut :: FilePath
  , mmEmitStatic :: Bool
  , mmStrict :: Bool
  , mmCentroid :: Bool
  , mmManifest :: Maybe FilePath
  , mmBuildRootSha256 :: Maybe String
  } deriving (Show)

data CreateOptions = CreateOptions
  { createOutput :: FilePath
  , createNodes :: [NodeSpec]
  , createEdges :: [EdgeSpec]
  , createTitle :: Maybe String
  } deriving (Show)

data NodeSpec = NodeSpec
  { nsId :: String
  , nsType :: String
  , nsX :: Int
  , nsY :: Int
  , nsWidth :: Int
  , nsHeight :: Int
  , nsContent :: Maybe String
  , nsColor :: Maybe String
  , nsLabel :: Maybe String
  , nsBackground :: Maybe String
  } deriving (Show)

data EdgeSpec = EdgeSpec
  { esId :: String
  , esFrom :: String
  , esTo :: String
  , esFromSide :: Maybe String
  , esToSide :: Maybe String
  , esLabel :: Maybe String
  , esColor :: Maybe String
  , esBidirectional :: Bool
  } deriving (Show)

data ViewOptions = ViewOptions
  { viewInput :: FilePath
  , viewFormat :: OutputFormat
  , viewOutput :: Maybe FilePath
  } deriving (Show)

data ListOptions = ListOptions
  { listInput :: FilePath
  , listType :: Maybe String
  , listSortBy :: Maybe String
  , listFilter :: Maybe String
  } deriving (Show)

data ExportOptions = ExportOptions
  { exportInput :: FilePath
  , exportFormat :: ExportFormat
  , exportOutput :: FilePath
  } deriving (Show)

data ImportOptions = ImportOptions
  { importInput :: FilePath
  , importFormat :: ImportFormat
  , importOutput :: FilePath
  } deriving (Show)

data StreamOptions = StreamOptions
  { streamInput :: Maybe FilePath
  , streamOutput :: Maybe FilePath
  , streamEvents :: FilePath
  , streamWatch :: Bool
  } deriving (Show)

data ValidateOptions = ValidateOptions
  { validateInput :: FilePath
  , validateStrict :: Bool
  } deriving (Show)

data QueryOptions = QueryOptions
  { queryInput :: FilePath
  , queryExpression :: String
  , queryOutputFormat :: OutputFormat
  } deriving (Show)

data TransformOptions = TransformOptions
  { transformInput :: FilePath
  , transformOutput :: FilePath
  , transformOperations :: [TransformOp]
  } deriving (Show)

data TransformOp
  = MoveNodes [(String, Int, Int)]
  | ResizeNodes [(String, Int, Int)]
  | RecolorNodes [(String, String)]
  | AddEdges [EdgeSpec]
  | RemoveNodes [String]
  | RemoveEdges [String]
  | AutoLayout LayoutAlgorithm
  deriving (Show)

data LayoutAlgorithm
  = LayoutVertical Int Int  -- ^ spacing, startY
  | LayoutHorizontal Int Int -- ^ spacing, startX
  | LayoutGrid Int Int      -- ^ cols, cellSize
  deriving (Show)

data StatsOptions = StatsOptions
  { statsInput :: FilePath
  , statsDetailed :: Bool
  } deriving (Show)

data OutputFormat
  = FormatText
  | FormatJSON
  | FormatNDJSON
  | FormatDot
  | FormatSVG
  | FormatPNG
  deriving (Show, Eq)

data ExportFormat
  = ExportPNG
  | ExportSVG
  | ExportPDF
  | ExportMarkdown
  | ExportOrgMode
  | ExportHTML
  deriving (Show, Eq)

data ImportFormat
  = ImportMarkdown
  | ImportOrgMode
  | ImportCSV
  | ImportGraphviz
  deriving (Show, Eq)

-- | Parser for command line options
parseOptions :: ParserInfo Options
parseOptions = info (parserHelper <**> helper <**> versionOpt)
  ( fullDesc
  <> progDesc "JSON Canvas CLI tool - Create, view, and manipulate JSON Canvas files"
  <> header "json-canvas - A tool for working with JSON Canvas diagrams"
  )

versionOpt :: Parser (a -> a)
versionOpt =
  infoOption
    (showVersion version)
    (long "version" <> help "Show version")

parserHelper :: Parser Options
parserHelper = Options
  <$> parseCommand
  <*> switch (long "verbose" <> short 'v' <> help "Verbose output")

parseCommand :: Parser Command
parseCommand = subparser
  ( command "create" (info (Create <$> parseCreate) (progDesc "Create a new canvas"))
  <> command "view" (info (View <$> parseView) (progDesc "View a canvas"))
  <> command "list" (info (List <$> parseList) (progDesc "List nodes/edges"))
  <> command "export" (info (Export <$> parseExport) (progDesc "Export to other formats"))
  <> command "import" (info (Import <$> parseImport) (progDesc "Import from other formats"))
  <> command "stream" (info (Stream <$> parseStream) (progDesc "Process NDJSON event stream"))
  <> command "validate" (info (Validate <$> parseValidate) (progDesc "Validate a canvas"))
  <> command "query" (info (Query <$> parseQuery) (progDesc "Query a canvas"))
  <> command "transform" (info (Transform <$> parseTransform) (progDesc "Transform a canvas"))
  <> command "stats" (info (Stats <$> parseStats) (progDesc "Show canvas statistics"))
  <> command "from-tree" (info (FromTree <$> parseFromTree) (progDesc "Generate canvas from directory tree"))
  <> command "watch" (info (WatchTree <$> parseWatch) (progDesc "Watch directory for changes"))
  <> command "mnemonic-manifold" (info (MnemonicManifold <$> parseMnemonicManifold) (progDesc "Mnemonic-manifold pipeline"))
  <> command "md" (info (Md <$> parseMd) (progDesc "Markdown utilities"))
  <> command "help" (info (pure $ Create defaultCreateOptions) (progDesc "Show help"))
  )

parseMd :: Parser MdCommand
parseMd = subparser
  ( command "extract" (info (MdExtract <$> parseMdExtract) (progDesc "Extract fenced NDJSON/JSON blocks from Markdown"))
 <> command "verify-evidence" (info (MdVerifyEvidence <$> parseMdVerifyEvidence) (progDesc "Verify evidence spans against source bytes"))
  )

parseMdExtract :: Parser MdExtractOptions
parseMdExtract = MdExtractOptions
  <$> strOption (long "root" <> value "." <> metavar "DIR" <> help "Root directory to scan for .md files")
  <*> strOption (long "out" <> value "build/extract" <> metavar "DIR" <> help "Output directory")
  <*> switch (long "strict" <> help "Fail on invalid JSON/unclosed fences")
  <*> option autoMode (long "mode" <> value ModeNdjsonOnly <> help "Mode: ndjson-only|all")
  <*> strOption (long "langs" <> value "ndjson,jsonl,jsonlines,json,hash" <> help "Comma-separated fence langs to extract")
  <*> asum
        [ flag' True (long "aggregate" <> help "Write aggregated ndjson/all.ndjson (default: true)")
        , flag' False (long "no-aggregate" <> help "Disable writing aggregated ndjson/all.ndjson")
        , pure True
        ]
  <*> switch (long "loose-ndjson" <> help "Also parse loose JSON lines outside fences")
  <*> switch (long "canon-filter" <> help "Only emit records compatible with mnemonic-manifold canon decoding")
  <*> switch (long "emit-prose-events" <> help "Emit canon event records from Markdown prose (paragraphs outside fences)")
  <*> switch (long "emit-canvas-pointers" <> help "Write ndjson/canvas.blocks.ndjson pointer records for extracted ```canvas blocks (requires --mode all and 'canvas' in --langs)")
  <*> switch (long "emit-manifest" <> help "Write a content-addressed manifest.json alongside extracted outputs")
  <*> optional (strOption (long "manifest-path" <> metavar "FILE" <> help "Manifest output path (default: <out>/manifest.json)"))
  <*> switch (long "include-git-head" <> help "Include resolved .git HEAD commit hash in the manifest (optional metadata)")
  <*> switch (long "timestamp" <> help "Set manifest time from SOURCE_DATE_EPOCH (fails if missing/invalid)")
  where
    autoMode = maybeReader $ \s -> case s of
      "ndjson-only" -> Just ModeNdjsonOnly
      "all" -> Just ModeAll
      _ -> Nothing

parseMdVerifyEvidence :: Parser MdVerifyEvidenceOptions
parseMdVerifyEvidence = MdVerifyEvidenceOptions
  <$> strOption (long "in" <> value "-" <> metavar "FILE" <> help "Input NDJSON file ('-' for stdin)")
  <*> strOption (long "root" <> value "." <> metavar "DIR" <> help "Docs root directory (used to resolve evidence.doc_path)")
  <*> asum
        [ flag' True (long "strict" <> help "Fail on first mismatch / missing evidence (default: true)")
        , flag' False (long "no-strict" <> help "Disable strict failure; skip records that can't be verified")
        , pure True
        ]

parseMnemonicManifold :: Parser MnemonicManifoldCommand
parseMnemonicManifold = subparser
  ( command "emit" (info (MMEmit <$> parseMMEmit) (progDesc "Emit Canvas NDJSON events from canon NDJSON"))
  )

parseMMEmit :: Parser MMEmitOptions
parseMMEmit = MMEmitOptions
  <$> strOption (long "in" <> value "-" <> metavar "FILE" <> help "Input NDJSON file ('-' for stdin)")
  <*> strOption (long "out" <> value "-" <> metavar "FILE" <> help "Output Canvas event NDJSON ('-' for stdout)")
  <*> parseDefaultTrue "emit-static" "no-emit-static" "Emit static Fano nodes/lines"
  <*> parseDefaultTrue "strict" "no-strict" "Fail on unknown/invalid input lines"
  <*> switch (long "centroid" <> help "Emit an observer node with derived closure fields")
  <*> optional (strOption (long "manifest" <> metavar "FILE" <> help "Manifest.json to read root_sha256 from (used for build.root overlay edges)"))
  <*> optional (strOption (long "build-root-sha256" <> metavar "HEX" <> help "Override build root sha256 hex (64 hex chars) for build.root overlay edges"))
  where
    parseDefaultTrue pos neg h =
      asum
        [ flag' True (long pos <> help (h ++ " (default: true)"))
        , flag' False (long neg <> help ("Disable: " ++ h))
        , pure True
        ]

defaultCreateOptions :: CreateOptions
defaultCreateOptions = CreateOptions
  { createOutput = "canvas.json"
  , createNodes = []
  , createEdges = []
  , createTitle = Nothing
  }

parseCreate :: Parser CreateOptions
parseCreate = CreateOptions
  <$> strOption (long "output" <> short 'o' <> value "canvas.json" <> help "Output file")
  <*> many parseNodeSpec
  <*> many parseEdgeSpec
  <*> optional (strOption (long "title" <> help "Canvas title"))

parseNodeSpec :: Parser NodeSpec
parseNodeSpec = NodeSpec
  <$> strOption (long "node-id" <> help "Node ID")
  <*> strOption (long "node-type" <> help "Node type (text|file|link|group)")
  <*> option auto (long "node-x" <> help "X position")
  <*> option auto (long "node-y" <> help "Y position")
  <*> option auto (long "node-width" <> value 240 <> help "Width")
  <*> option auto (long "node-height" <> value 240 <> help "Height")
  <*> optional (strOption (long "node-content" <> help "Content (text/url/file path)"))
  <*> optional (strOption (long "node-color" <> help "Color (hex or preset 1-6)"))
  <*> optional (strOption (long "node-label" <> help "Label (for groups)"))
  <*> optional (strOption (long "node-background" <> help "Background image path"))

parseEdgeSpec :: Parser EdgeSpec
parseEdgeSpec = EdgeSpec
  <$> strOption (long "edge-id" <> help "Edge ID")
  <*> strOption (long "from" <> help "From node ID")
  <*> strOption (long "to" <> help "To node ID")
  <*> optional (strOption (long "from-side" <> help "From side (top|right|bottom|left)"))
  <*> optional (strOption (long "to-side" <> help "To side (top|right|bottom|left)"))
  <*> optional (strOption (long "edge-label" <> help "Edge label"))
  <*> optional (strOption (long "edge-color" <> help "Edge color"))
  <*> switch (long "bidirectional" <> help "Bidirectional arrow")

parseView :: Parser ViewOptions
parseView = ViewOptions
  <$> argument str (metavar "FILE" <> help "Input canvas file")
  <*> parseOutputFormat
  <*> optional (strOption (long "output" <> short 'o' <> help "Output file"))

parseOutputFormat :: Parser OutputFormat
parseOutputFormat = option autoFormat
  ( long "format" <> short 'f' <> value FormatText
  <> help "Output format (text|json|ndjson|dot|svg|png)" )
  where
    autoFormat = maybeReader $ \s -> case s of
      "text" -> Just FormatText
      "json" -> Just FormatJSON
      "ndjson" -> Just FormatNDJSON
      "dot" -> Just FormatDot
      "svg" -> Just FormatSVG
      "png" -> Just FormatPNG
      _ -> Nothing

parseList :: Parser ListOptions
parseList = ListOptions
  <$> argument str (metavar "FILE" <> help "Input canvas file")
  <*> optional (strOption (long "type" <> help "Filter by type (nodes|edges|text|file|link|group)"))
  <*> optional (strOption (long "sort" <> help "Sort by (id|type|x|y|size)"))
  <*> optional (strOption (long "filter" <> help "Filter expression"))

parseExport :: Parser ExportOptions
parseExport = ExportOptions
  <$> argument str (metavar "FILE" <> help "Input canvas file")
  <*> parseExportFormat
  <*> strOption (long "output" <> short 'o' <> help "Output file")

parseExportFormat :: Parser ExportFormat
parseExportFormat = option autoExport
  ( long "format" <> short 'f' <> help "Export format (png|svg|pdf|md|org|html)" )
  where
    autoExport = maybeReader $ \s -> case s of
      "png" -> Just ExportPNG
      "svg" -> Just ExportSVG
      "pdf" -> Just ExportPDF
      "md" -> Just ExportMarkdown
      "org" -> Just ExportOrgMode
      "html" -> Just ExportHTML
      _ -> Nothing

parseImport :: Parser ImportOptions
parseImport = ImportOptions
  <$> argument str (metavar "FILE" <> help "Input file")
  <*> parseImportFormat
  <*> strOption (long "output" <> short 'o' <> help "Output canvas file")

parseImportFormat :: Parser ImportFormat
parseImportFormat = option autoImport
  ( long "format" <> short 'f' <> help "Import format (md|org|csv|dot)" )
  where
    autoImport = maybeReader $ \s -> case s of
      "md" -> Just ImportMarkdown
      "org" -> Just ImportOrgMode
      "csv" -> Just ImportCSV
      "dot" -> Just ImportGraphviz
      _ -> Nothing

parseStream :: Parser StreamOptions
parseStream = StreamOptions
  <$> optional (strOption (long "input" <> short 'i' <> help "Initial canvas file"))
  <*> optional (strOption (long "output" <> short 'o' <> help "Output canvas file"))
  <*> strOption (long "events" <> help "NDJSON events file")
  <*> switch (long "watch" <> help "Watch for file changes")

parseValidate :: Parser ValidateOptions
parseValidate = ValidateOptions
  <$> argument str (metavar "FILE" <> help "Input canvas file")
  <*> switch (long "strict" <> help "Strict validation")

parseQuery :: Parser QueryOptions
parseQuery = QueryOptions
  <$> argument str (metavar "FILE" <> help "Input canvas file")
  <*> strOption (long "query" <> short 'q' <> help "Query expression")
  <*> parseOutputFormat

parseTransform :: Parser TransformOptions
parseTransform = TransformOptions
  <$> argument str (metavar "FILE" <> help "Input canvas file")
  <*> strOption (long "output" <> short 'o' <> help "Output file")
  <*> many parseTransformOp

parseTransformOp :: Parser TransformOp
parseTransformOp = subparser
  ( command "move" (info (MoveNodes <$> some parseMove) (progDesc "Move nodes"))
  <> command "resize" (info (ResizeNodes <$> some parseResize) (progDesc "Resize nodes"))
  <> command "recolor" (info (RecolorNodes <$> some parseRecolor) (progDesc "Recolor nodes"))
  <> command "add-edge" (info (AddEdges <$> some parseEdgeSpec) (progDesc "Add edges"))
  <> command "remove-nodes" (info (RemoveNodes <$> some (strArgument (metavar "NODE-ID"))) (progDesc "Remove nodes"))
  <> command "remove-edges" (info (RemoveEdges <$> some (strArgument (metavar "EDGE-ID"))) (progDesc "Remove edges"))
  <> command "layout" (info (AutoLayout <$> parseLayout) (progDesc "Auto-layout nodes"))
  )

parseMove :: Parser (String, Int, Int)
parseMove = (,,)
  <$> strOption (long "id" <> help "Node ID")
  <*> option auto (long "x" <> help "New X position")
  <*> option auto (long "y" <> help "New Y position")

parseResize :: Parser (String, Int, Int)
parseResize = (,,)
  <$> strOption (long "id" <> help "Node ID")
  <*> option auto (long "width" <> help "New width")
  <*> option auto (long "height" <> help "New height")

parseRecolor :: Parser (String, String)
parseRecolor = (,)
  <$> strOption (long "id" <> help "Node ID")
  <*> strOption (long "color" <> help "New color")

parseLayout :: Parser LayoutAlgorithm
parseLayout = subparser
  ( command "vertical" (info (LayoutVertical
      <$> option auto (long "spacing" <> value 80)
      <*> option auto (long "start-y" <> value 0))
      (progDesc "Vertical layout"))
  <> command "horizontal" (info (LayoutHorizontal
      <$> option auto (long "spacing" <> value 80)
      <*> option auto (long "start-x" <> value 0))
      (progDesc "Horizontal layout"))
  <> command "grid" (info (LayoutGrid
      <$> option auto (long "cols" <> value 3)
      <*> option auto (long "cell-size" <> value 240))
      (progDesc "Grid layout"))
  )

parseStats :: Parser StatsOptions
parseStats = StatsOptions
  <$> argument str (metavar "FILE" <> help "Input canvas file")
  <*> switch (long "detailed" <> help "Show detailed statistics")

parseFromTree :: Parser TreeOptions
parseFromTree = TreeOptions
  <$> argument str (metavar "DIRECTORY" <> help "Directory to visualize")
  <*> strOption (long "output" <> short 'o' <> value "tree.json" <> help "Output canvas file")
  <*> optional (option auto (long "depth" <> short 'd' <> help "Maximum depth to traverse"))
  <*> many (strOption (long "exclude" <> help "Patterns to exclude"))
  <*> switch (long "hidden" <> help "Include hidden files/directories")
  <*> option auto (long "node-size" <> value 200 <> help "Base node size in pixels")
  <*> option auto (long "spacing" <> value 80 <> help "Spacing between nodes")
  <*> parseTreeLayout
  <*> parseColorBy
  <*> switch (long "edges" <> help "Show edges between parent/child")
  <*> optional (option auto (long "max-nodes" <> help "Maximum number of nodes"))

parseTreeLayout :: Parser TreeLayout
parseTreeLayout = option autoLayout
  ( long "layout" <> value TreeLayoutVertical
  <> help "Layout: vertical, horizontal, radial, indented" )
  where
    autoLayout = maybeReader $ \s -> case s of
      "vertical" -> Just TreeLayoutVertical
      "horizontal" -> Just TreeLayoutHorizontal
      "radial" -> Just TreeLayoutRadial
      "indented" -> Just TreeLayoutIndented
      _ -> Nothing

parseColorBy :: Parser ColorBy
parseColorBy = option autoColor
  ( long "color-by" <> value ColorByType
  <> help "Color by: size, type, depth, age" )
  where
    autoColor = maybeReader $ \s -> case s of
      "size" -> Just ColorBySize
      "type" -> Just ColorByType
      "depth" -> Just ColorByDepth
      "age" -> Just ColorByAge
      _ -> Nothing

parseWatch :: Parser WatchOptions
parseWatch = WatchOptions
  <$> argument str (metavar "DIRECTORY" <> help "Directory to watch")
  <*> strOption (long "output" <> short 'o' <> value "watch.json" <> help "Output canvas file")
  <*> option auto (long "interval" <> short 'i' <> value 5 <> help "Update interval in seconds")
  <*> parseFromTree

-- | Main entry point
main :: IO ()
main = do
  opts <- execParser parseOptions
  runCommand opts

runCommand :: Options -> IO ()
runCommand opts@Options{..} = case optCommand of
  Create createOpts -> runCreate optVerbose createOpts
  View viewOpts -> runView viewOpts
  List listOpts -> runList listOpts
  Export exportOpts -> runExport exportOpts
  Import importOpts -> runImport importOpts
  Stream streamOpts -> runStream optVerbose streamOpts
  Validate validateOpts -> runValidate validateOpts
  Query queryOpts -> runQuery queryOpts
  Transform transformOpts -> runTransform transformOpts
  Stats statsOpts -> runStats statsOpts
  FromTree treeOpts -> runFromTree treeOpts
  WatchTree watchOpts -> runWatchTree watchOpts
  MnemonicManifold mmCmd -> runMnemonicManifold mmCmd
  Md mdCmd -> runMd mdCmd

runMd :: MdCommand -> IO ()
runMd = \case
  MdExtract MdExtractOptions{..} -> do
    let cfg = ExtractConfig
          { ecRoot = mdRoot
          , ecOut = mdOut
          , ecStrict = mdStrict
          , ecMode = mdMode
          , ecLangs = map (T.pack . trim) (splitComma mdLangs)
          , ecAggregate = mdAggregate
          , ecLooseNdjson = mdLooseNdjson
          , ecCanonFilter = mdCanonFilter
          , ecEmitProseEvents = mdEmitProseEvents
          , ecEmitCanvasPointers = mdEmitCanvasPointers
          }
    extractNdjsonFromTree cfg
    let manifestPath = fromMaybe (mdOut </> "manifest.json") mdManifestPath
        mopts =
          ManifestOptions
            { moRoot = mdRoot
            , moOut = mdOut
            , moMode = case mdMode of
                ModeNdjsonOnly -> "ndjson-only"
                ModeAll -> "all"
            , moLangs = map (T.pack . trim) (splitComma mdLangs)
            , moStrict = mdStrict
            , moAggregate = mdAggregate
            , moLooseNdjson = mdLooseNdjson
            , moCanonFilter = mdCanonFilter
            , moEmitProseEvents = mdEmitProseEvents
            , moEmitCanvasPointers = mdEmitCanvasPointers
            , moEmitManifest = mdEmitManifest
            , moManifestPath = manifestPath
            , moIncludeGitHead = mdIncludeGitHead
            , moTimestamp = mdTimestamp
            , moToolName = "json-canvas"
            , moToolVersion = T.pack (showVersion version)
            }
    writeManifest mopts
  MdVerifyEvidence MdVerifyEvidenceOptions{..} -> do
    input <- if mveIn == "-" then BL.getContents else BL.readFile mveIn
    res <- verifyEvidenceNdjsonBytes (VerifyConfig mveRoot mveStrict) input
    case res of
      Left err -> die (T.unpack err)
      Right () -> pure ()
  where
    splitComma s = case break (== ',') s of
      (a, "") -> [a]
      (a, _ : rest) -> a : splitComma rest

    trim = f . f
      where f = reverse . dropWhile isSpace

runMnemonicManifold :: MnemonicManifoldCommand -> IO ()
runMnemonicManifold = \case
  MMEmit MMEmitOptions{..} -> do
    input <- if mmIn == "-" then BL.getContents else BL.readFile mmIn
    let docId = T.pack (if mmIn == "-" then "stdin" else mmIn)
    triples <- case decodeCanonTriples mmStrict docId input of
      Left err -> die (T.unpack err)
      Right ts -> pure ts
    buildRoot <- resolveBuildRoot mmStrict mmManifest mmBuildRootSha256
    let opts = EmitOptions { eoEmitStatic = mmEmitStatic, eoCentroid = mmCentroid, eoBuildRoot = buildRoot }
        events =
          (if mmEmitStatic then emitStaticFanoEvents else []) <>
          concatMap (emitClauseEvents opts) triples
        outBytes = encodeNDJSON events
    if mmOut == "-" then BL.putStr outBytes else BL.writeFile mmOut outBytes

resolveBuildRoot :: Bool -> Maybe FilePath -> Maybe String -> IO (Maybe BuildRootInfo)
resolveBuildRoot strictMode mManifest mOverride =
  case mOverride of
    Just hexStr -> do
      mh <- requireSha256Hex strictMode (T.pack hexStr)
      pure (BuildRootInfo <$> mh <*> pure (T.pack <$> mManifest))
    Nothing ->
      case mManifest of
        Nothing -> pure Nothing
        Just p -> do
          bs <- BL.readFile p
          case A.decode bs :: Maybe A.Value of
            Nothing ->
              if strictMode then die ("invalid manifest json: " <> p) else pure Nothing
            Just v -> do
              let mh = case v of
                    A.Object o -> case KM.lookup "root_sha256" o of
                      Just (A.String t) -> Just t
                      _ -> Nothing
                    _ -> Nothing
              case mh of
                Nothing ->
                  if strictMode then die ("manifest missing root_sha256: " <> p) else pure Nothing
                Just t -> do
                  mh' <- requireSha256Hex strictMode t
                  pure (BuildRootInfo <$> mh' <*> pure (Just (T.pack p)))

requireSha256Hex :: Bool -> Text -> IO (Maybe Text)
requireSha256Hex strictMode t =
  case parseSha256Hex t of
    Just s -> pure (Just s)
    Nothing ->
      if strictMode
        then die ("invalid sha256 hex (expected 64 hex chars): " <> T.unpack t)
        else pure Nothing

parseSha256Hex :: Text -> Maybe Text
parseSha256Hex t =
  let s = T.toLower (T.strip t)
      okLen = T.length s == 64
      okChars = T.all (\c -> ('0' <= c && c <= '9') || ('a' <= c && c <= 'f')) s
  in if okLen && okChars then Just s else Nothing

-- | Create a new canvas
runCreate :: Bool -> CreateOptions -> IO ()
runCreate verbose CreateOptions{..} = do
  when verbose $ putStrLn $ "Creating canvas with " ++ show (length createNodes) ++ " nodes"
  
  let nodes = map nodeSpecToNode createNodes
      edges = map edgeSpecToEdge createEdges
      canvas' = canvas nodes edges
      output = createOutput
  
  BL.writeFile output (encode canvas')
  putStrLn $ "Canvas written to: " ++ output

nodeSpecToNode :: NodeSpec -> Node
nodeSpecToNode NodeSpec{..} =
  let nid = NodeId (T.pack nsId)
      pos = (nsX, nsY)
      size = (nsWidth, nsHeight)
      baseNode = case nsType of
        "text" -> textNode nid pos size (T.pack $ fromMaybe "" nsContent)
        "file" -> fileNode nid pos size (T.pack $ fromMaybe "" nsContent)
        "link" -> linkNode nid pos size (T.pack $ fromMaybe "" nsContent)
        "group" -> groupNode nid pos size (T.pack $ fromMaybe "" nsLabel)
        _ -> textNode nid pos size (T.pack $ fromMaybe "" nsContent)
  in baseNode
    { color = parseColor <$> nsColor
    , nodeBackground = T.pack <$> nsBackground
    , nodeLabel = if nsType == "group" 
                  then T.pack <$> nsLabel 
                  else Nothing
    }

edgeSpecToEdge :: EdgeSpec -> Edge
edgeSpecToEdge EdgeSpec{..} =
  let eid = EdgeId (T.pack esId)
      from = NodeId (T.pack esFrom)
      to = NodeId (T.pack esTo)
      fromSide = parseSide <$> esFromSide
      toSide = parseSide <$> esToSide
      baseEdge = if esBidirectional
                 then fromMaybe (edge eid from to) $ do
                      fSide <- fromSide
                      tSide <- toSide
                      pure $ bidirectional eid from fSide to tSide
                 else fromMaybe (edge eid from to) $ do
                      fSide <- fromSide
                      tSide <- toSide
                      pure $ flow eid from fSide to tSide
  in baseEdge
    { fromSide = parseSide <$> esFromSide
    , toSide = parseSide <$> esToSide
    , edgeColor = parseColor <$> esColor
    , edgeLabel = T.pack <$> esLabel
    }

parseSide :: String -> Side
parseSide "top" = Top
parseSide "right" = C.Right
parseSide "bottom" = Bottom
parseSide "left" = C.Left
parseSide _ = Top

parseColor :: String -> CanvasColor
parseColor s
  | length s == 1 && s >= "1" && s <= "6" = PresetColor (read s)
  | otherwise = HexColor (T.pack s)

-- | View a canvas
runView :: ViewOptions -> IO ()
runView ViewOptions{..} = do
  content <- BL.readFile viewInput
  case decode content of
    Nothing -> die "Invalid JSON Canvas file"
    Just canvas' -> do
      output <- case viewOutput of
        Just f -> return f
        Nothing -> return $ viewInput ++ "." ++ show viewFormat
      
      case viewFormat of
        FormatText -> viewAsText canvas' output
        FormatJSON -> BL.writeFile output content
        FormatNDJSON -> viewAsNDJSON canvas' output
        FormatDot -> viewAsDot canvas' output
        FormatSVG -> viewAsSVG canvas' output
        FormatPNG -> viewAsPNG canvas' output

viewAsText :: Canvas -> FilePath -> IO ()
viewAsText Canvas{..} output = do
  let header = "JSON Canvas\n" ++ replicate 40 '=' ++ "\n"
      nodeList = "Nodes (" ++ show (length nodes) ++ "):\n" ++ 
                 unlines (map formatNode nodes)
      edgeList = "Edges (" ++ show (length edges) ++ "):\n" ++ 
                 unlines (map formatEdge edges)
  if output == "-"
    then putStrLn $ header ++ nodeList ++ "\n" ++ edgeList
    else writeFile output $ header ++ nodeList ++ "\n" ++ edgeList

formatNode :: Node -> String
formatNode Node{..} = 
  "  " ++ T.unpack (unNodeId nodeId) ++ 
  " [" ++ show nodeType ++ "] at (" ++ show x ++ "," ++ show y ++ 
  ") size " ++ show width ++ "x" ++ show height

formatEdge :: Edge -> String
formatEdge Edge{..} = 
  "  " ++ T.unpack (unEdgeId edgeId) ++ ": " ++
  T.unpack (unNodeId fromNode) ++ " -> " ++ T.unpack (unNodeId toNode)

viewAsNDJSON :: Canvas -> FilePath -> IO ()
viewAsNDJSON canvas output = do
  let events = [EvSnapshot canvas]
      ndjson = encodeNDJSON events
  BL.writeFile output ndjson

viewAsDot :: Canvas -> FilePath -> IO ()
viewAsDot Canvas{..} output = do
  let header = "digraph Canvas {\n  rankdir=LR;\n  node [shape=box, style=filled];\n"
      nodes' = map nodeToDot nodes
      edges' = map edgeToDot edges
      footer = "}\n"
      dot = header ++ unlines nodes' ++ unlines edges' ++ footer
  writeFile output dot

nodeToDot :: Node -> String
nodeToDot Node{..} = 
  "  \"" ++ T.unpack (unNodeId nodeId) ++ "\" [label=\"" ++ 
  label ++ "\", pos=\"" ++ show x ++ "," ++ show y ++ "!\"];"
  where
    label = case nodeType of
      NText -> fromMaybe "" (fmap T.unpack nodeText)
      NFile -> fromMaybe "" (fmap T.unpack nodeFile)
      NLink -> fromMaybe "" (fmap T.unpack nodeUrl)
      NGroup -> fromMaybe "" (fmap T.unpack nodeLabel)

edgeToDot :: Edge -> String
edgeToDot Edge{..} = 
  "  \"" ++ T.unpack (unNodeId fromNode) ++ "\" -> \"" ++ 
  T.unpack (unNodeId toNode) ++ "\" [label=\"" ++ 
  fromMaybe "" (fmap T.unpack edgeLabel) ++ "\"];"

viewAsSVG :: Canvas -> FilePath -> IO ()
viewAsSVG canvas output = do
  -- In a real implementation, you'd use a library like diagrams or graphviz
  -- to render SVG. Here's a simple placeholder:
  let svg = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" ++
            "<svg xmlns=\"http://www.w3.org/2000/svg\" version=\"1.1\">\n" ++
            "  <rect width=\"100%\" height=\"100%\" fill=\"white\"/>\n" ++
            "  <text x=\"10\" y=\"20\" font-family=\"Arial\" font-size=\"12\">" ++
            "SVG rendering not implemented in this example</text>\n" ++
            "</svg>"
  writeFile output svg

viewAsPNG :: Canvas -> FilePath -> IO ()
viewAsPNG canvas output = do
  putStrLn "PNG export requires additional libraries (e.g., graphviz-cairo)"
  putStrLn "Creating placeholder..."
  writeFile output "PNG rendering not implemented in this example"

-- | List nodes/edges
runList :: ListOptions -> IO ()
runList ListOptions{..} = do
  content <- BL.readFile listInput
  case decode content of
    Nothing -> die "Invalid JSON Canvas file"
    Just Canvas{..} -> do
      let filtered = case listType of
            Just "nodes" -> map formatNode nodes
            Just "edges" -> map formatEdge edges
            Just t | t `elem` ["text","file","link","group"] ->
                map formatNode $ filter ((== parseNodeType t) . nodeType) nodes
            _ -> map formatNode nodes ++ map formatEdge edges
      
      let sorted = case listSortBy of
            Just "id" -> sortBy (compare `on` id) filtered
            Just "type" -> sortBy (compare `on` type') filtered
            _ -> filtered
      
      mapM_ putStrLn sorted
  where
    parseNodeType "text" = NText
    parseNodeType "file" = NFile
    parseNodeType "link" = NLink
    parseNodeType "group" = NGroup
    parseNodeType _ = NText
    
    type' s = takeWhile (/= ' ') (drop 2 s)

-- | Export to other formats
runExport :: ExportOptions -> IO ()
runExport ExportOptions{..} = do
  content <- BL.readFile exportInput
  case decode content of
    Nothing -> die "Invalid JSON Canvas file"
    Just canvas' -> do
      case exportFormat of
        ExportPNG -> viewAsPNG canvas' exportOutput
        ExportSVG -> viewAsSVG canvas' exportOutput
        ExportPDF -> putStrLn "PDF export not implemented"
        ExportMarkdown -> exportMarkdown canvas' exportOutput
        ExportOrgMode -> exportOrgMode canvas' exportOutput
        ExportHTML -> exportHTML canvas' exportOutput

exportMarkdown :: Canvas -> FilePath -> IO ()
exportMarkdown Canvas{..} output = do
  let header = "# JSON Canvas Export\n\n"
      nodes' = "## Nodes\n\n" ++ unlines (map markdownNode nodes)
      edges' = "## Edges\n\n" ++ unlines (map markdownEdge edges)
  writeFile output $ header ++ nodes' ++ "\n" ++ edges'

markdownNode :: Node -> String
markdownNode Node{..} = 
  "- **" ++ T.unpack (unNodeId nodeId) ++ "** (" ++ show nodeType ++ 
  "): at (" ++ show x ++ "," ++ show y ++ ")"

markdownEdge :: Edge -> String
markdownEdge Edge{..} = 
  "- " ++ T.unpack (unNodeId fromNode) ++ " → " ++ T.unpack (unNodeId toNode)

exportOrgMode :: Canvas -> FilePath -> IO ()
exportOrgMode canvas output = do
  -- Simplified org-mode export
  exportMarkdown canvas output  -- Placeholder

exportHTML :: Canvas -> FilePath -> IO ()
exportHTML Canvas{..} output = do
  let html = "<!DOCTYPE html>\n<html>\n<head>\n" ++
             "<title>JSON Canvas</title>\n" ++
             "<style>body { font-family: Arial; }</style>\n" ++
             "</head>\n<body>\n" ++
             "<h1>JSON Canvas</h1>\n" ++
             "<h2>Nodes</h2>\n<ul>\n" ++
             unlines (map htmlNode nodes) ++
             "</ul>\n<h2>Edges</h2>\n<ul>\n" ++
             unlines (map htmlEdge edges) ++
             "</ul>\n</body>\n</html>"
  writeFile output html

htmlNode :: Node -> String
htmlNode Node{..} = 
  "  <li><b>" ++ T.unpack (unNodeId nodeId) ++ "</b> (" ++ show nodeType ++ ")</li>"

htmlEdge :: Edge -> String
htmlEdge Edge{..} = 
  "  <li>" ++ T.unpack (unNodeId fromNode) ++ " → " ++ T.unpack (unNodeId toNode) ++ "</li>"

-- | Import from other formats
runImport :: ImportOptions -> IO ()
runImport ImportOptions{..} = do
  content <- readFile importInput
  canvas' <- case importFormat of
    ImportMarkdown -> importMarkdown content
    ImportOrgMode -> importOrgMode content
    ImportCSV -> importCSV content
    ImportGraphviz -> importGraphviz content
  BL.writeFile importOutput (encode canvas')
  putStrLn $ "Imported to: " ++ importOutput

importMarkdown :: String -> IO Canvas
importMarkdown content = do
  -- Simplified markdown import - would need proper parsing
  pure $ Canvas [] []

importOrgMode :: String -> IO Canvas
importOrgMode content = importMarkdown content  -- Placeholder

importCSV :: String -> IO Canvas
importCSV content = do
  -- Simple CSV parser for node lists
  let lines' = filter (not . null) $ lines content
      nodes = map csvLineToNode $ drop 1 lines'  -- Skip header
  pure $ Canvas nodes []
  where
    csvLineToNode line = 
      let fields = split ',' line
      in case fields of
        (id':typ:x':y':w:h:_) ->
          let nid = NodeId (T.pack id')
              pos = (read x', read y')
              size = (read w, read h)
          in case typ of
            "text" -> textNode nid pos size (T.pack "")
            _ -> textNode nid pos size (T.pack "")
        _ -> textNode (NodeId "error") (0,0) (240,240) (T.pack "")
    
    split :: Char -> String -> [String]
    split c s = case dropWhile (== c) s of
      "" -> []
      s' -> let (w, s'') = break (== c) s'
            in w : split c (drop 1 s'')

importGraphviz :: String -> IO Canvas
importGraphviz content = do
  -- Graphviz import would need proper DOT parsing
  pure $ Canvas [] []

-- | Process NDJSON event stream
runStream :: Bool -> StreamOptions -> IO ()
runStream verbose StreamOptions{..} = do
  initialCanvas <- case streamInput of
    Just f -> do
      content <- BL.readFile f
      case decode content of
        Nothing -> die "Invalid initial canvas file"
        Just c -> pure c
    Nothing -> pure emptyCanvas
  
  eventsContent <- BL.readFile streamEvents
  case decodeNDJSON eventsContent of
    Prelude.Left err -> die $ "Error parsing events: " ++ err
    Prelude.Right events -> do
      let finalCanvas = foldEvents initialCanvas events
      case streamOutput of
        Just f -> BL.writeFile f (encode finalCanvas)
        Nothing -> BL.putStr (encode finalCanvas)
      
      when verbose $ 
        putStrLn $ "Applied " ++ show (length events) ++ " events"

-- | Validate a canvas
runValidate :: ValidateOptions -> IO ()
runValidate ValidateOptions{..} = do
  content <- BL.readFile validateInput
  case decode content of
    Nothing -> die "Invalid JSON Canvas file"
    Just canvas'@Canvas{..} -> do
      let errors = validateCanvas canvas' validateStrict
      if null errors
        then putStrLn "✓ Canvas is valid"
        else do
          putStrLn "✗ Canvas has issues:"
          mapM_ (putStrLn . ("  - " ++)) errors
          exitFailure

validateCanvas :: Canvas -> Bool -> [String]
validateCanvas Canvas{..} strict = 
  let nodeErrors = concatMap validateNode nodes
      edgeErrors = concatMap validateEdge edges
      idErrors = validateIds nodes edges strict
  in nodeErrors ++ edgeErrors ++ idErrors

validateNode :: Node -> [String]
validateNode Node{..} = 
  [ "Node " ++ T.unpack (unNodeId nodeId) ++ " missing required content" 
  | case nodeType of
      NText -> isNothing nodeText
      NFile -> isNothing nodeFile
      NLink -> isNothing nodeUrl
      NGroup -> False
  ] ++
  [ "Node " ++ T.unpack (unNodeId nodeId) ++ " has invalid dimensions"
  | width <= 0 || height <= 0
  ]

validateEdge :: Edge -> [String]
validateEdge Edge{..} = []

validateIds :: [Node] -> [Edge] -> Bool -> [String]
validateIds nodes edges strict =
  let nodeIds = map nodeId nodes
      edgeIds = map edgeId edges
      duplicateNodes = findDuplicates nodeIds
      duplicateEdges = findDuplicates edgeIds
      missingNodes = [ "Edge " ++ T.unpack (unEdgeId edgeId) ++ 
                      " references missing node " ++ T.unpack (unNodeId nid)
                    | Edge{..} <- edges
                    , nid <- [fromNode, toNode]
                    , nid `notElem` nodeIds
                    ]
  in (if strict 
      then map ("Duplicate node ID: " ++) duplicateNodes ++
           map ("Duplicate edge ID: " ++) duplicateEdges
      else []) ++ missingNodes

findDuplicates :: (Eq a, Show a) => [a] -> [String]
findDuplicates xs = 
  [ show x | x <- xs, length (filter (== x) xs) > 1 ]

-- | Query a canvas
runQuery :: QueryOptions -> IO ()
runQuery QueryOptions{..} = do
  content <- BL.readFile queryInput
  case decode content of
    Nothing -> die "Invalid JSON Canvas file"
    Just canvas' -> do
      let results = evaluateQuery canvas' queryExpression
      case queryOutputFormat of
        FormatText -> mapM_ putStrLn results
        FormatJSON -> BL.putStrLn (encode results)
        _ -> putStrLn "Unsupported output format for query"

evaluateQuery :: Canvas -> String -> [String]
evaluateQuery Canvas{..} query = 
  case words query of
    ["nodes", "where", "type", "=", t] ->
      map (T.unpack . unNodeId . nodeId) $
      filter ((== parseNodeType t) . nodeType) nodes
    ["nodes", "with", "color"] ->
      map (T.unpack . unNodeId . nodeId) $
      filter (isJust . color) nodes
    ["nodes", "larger", "than", size] ->
      let threshold = read size
      in map (T.unpack . unNodeId . nodeId) $
         filter (\n -> width n * height n > threshold) nodes
    ["edges", "from", nid] ->
      map (T.unpack . unEdgeId . edgeId) $
      filter ((== NodeId (T.pack nid)) . fromNode) edges
    ["edges", "to", nid] ->
      map (T.unpack . unEdgeId . edgeId) $
      filter ((== NodeId (T.pack nid)) . toNode) edges
    _ -> ["Unknown query"]
  where
    parseNodeType "text" = NText
    parseNodeType "file" = NFile
    parseNodeType "link" = NLink
    parseNodeType "group" = NGroup
    parseNodeType _ = NText

-- | Transform a canvas
runTransform :: TransformOptions -> IO ()
runTransform TransformOptions{..} = do
  content <- BL.readFile transformInput
  case decode content of
    Nothing -> die "Invalid JSON Canvas file"
    Just canvas' -> do
      let transformed = applyTransforms canvas' transformOperations
      BL.writeFile transformOutput (encode transformed)
      putStrLn $ "Transformed canvas written to: " ++ transformOutput

applyTransforms :: Canvas -> [TransformOp] -> Canvas
applyTransforms canvas = foldl applyTransform canvas

applyTransform :: Canvas -> TransformOp -> Canvas
applyTransform canvas@Canvas{..} = \case
  MoveNodes moves ->
    let moveNode n = case lookup (T.unpack (unNodeId (nodeId n))) moves' of
          Just (dx, dy) -> n { x = x n + dx, y = y n + dy }
          Nothing -> n
        moves' = map (\(id',dx,dy) -> (id', (dx,dy))) moves
    in canvas { nodes = map moveNode nodes }
  
  ResizeNodes resizes ->
    let resizeNode n = case lookup (T.unpack (unNodeId (nodeId n))) resizes' of
          Just (w',h') -> n { width = w', height = h' }
          Nothing -> n
        resizes' = map (\(id',w,h) -> (id', (w,h))) resizes
    in canvas { nodes = map resizeNode nodes }
  
  RecolorNodes recolors ->
    let recolorNode n = case lookup (T.unpack (unNodeId (nodeId n))) recolors' of
          Just c -> n { color = Just $ parseColor c }
          Nothing -> n
        recolors' = map (\(id',c) -> (id',c)) recolors
    in canvas { nodes = map recolorNode nodes }
  
  AddEdges edgeSpecs ->
    let newEdges = map edgeSpecToEdge edgeSpecs
    in canvas { edges = edges ++ newEdges }
  
  RemoveNodes nodeIds ->
    let ids = map NodeId (T.pack <$> nodeIds)
    in canvas 
      { nodes = filter (\n -> nodeId n `notElem` ids) nodes
      , edges = filter (\e -> fromNode e `notElem` ids && toNode e `notElem` ids) edges
      }
  
  RemoveEdges edgeIds ->
    let ids = map EdgeId (T.pack <$> edgeIds)
    in canvas { edges = filter (\e -> edgeId e `notElem` ids) edges }
  
  AutoLayout layout -> autoLayout canvas layout

autoLayout :: Canvas -> LayoutAlgorithm -> Canvas
autoLayout canvas@Canvas{..} = \case
  LayoutVertical spacing startY ->
    let sorted = sortBy (compare `on` nodeId) nodes
        placed = zipWith placeNode sorted [startY, startY + spacing + 240 ..]
        placeNode n y' = n { x = 0, y = y' }
    in canvas { nodes = placed }
  
  LayoutHorizontal spacing startX ->
    let sorted = sortBy (compare `on` nodeId) nodes
        placed = zipWith placeNode sorted [startX, startX + spacing + 240 ..]
        placeNode n x' = n { x = x', y = 0 }
    in canvas { nodes = placed }
  
  LayoutGrid cols cellSize ->
    let sorted = sortBy (compare `on` nodeId) nodes
        placed = zipWith placeNode sorted [0..]
        placeNode n i = 
          let row = i `div` cols
              col = i `mod` cols
          in n { x = col * (cellSize + 80), y = row * (cellSize + 80) }
    in canvas { nodes = placed }

-- | Show canvas statistics
runStats :: StatsOptions -> IO ()
runStats StatsOptions{..} = do
  content <- BL.readFile statsInput
  case decode content of
    Nothing -> die "Invalid JSON Canvas file"
    Just canvas@Canvas{..} -> do
      let stats = calculateStats canvas
      putStrLn $ "JSON Canvas Statistics: " ++ statsInput
      putStrLn $ replicate 40 '-'
      putStrLn $ "Total nodes: " ++ show (length nodes)
      putStrLn $ "  - Text nodes: " ++ show (countNodes NText nodes)
      putStrLn $ "  - File nodes: " ++ show (countNodes NFile nodes)
      putStrLn $ "  - Link nodes: " ++ show (countNodes NLink nodes)
      putStrLn $ "  - Group nodes: " ++ show (countNodes NGroup nodes)
      putStrLn $ "Total edges: " ++ show (length edges)
      
      if not (null nodes)
        then do
          let area = sum $ map (\n -> width n * height n) nodes
          putStrLn $ "Total area: " ++ show area ++ " px²"
          let (minX, maxX, minY, maxY) = boundingBox nodes
          putStrLn $ "Bounding box: (" ++ show minX ++ "," ++ show minY ++ 
                     ") to (" ++ show maxX ++ "," ++ show maxY ++ ")"
        else pure ()
      
      when statsDetailed $ do
        putStrLn "\nNode details:"
        mapM_ (putStrLn . ("  - " ++) . formatNode) nodes
        putStrLn "\nEdge details:"
        mapM_ (putStrLn . ("  - " ++) . formatEdge) edges

countNodes :: NodeType -> [Node] -> Int
countNodes nt = length . filter ((== nt) . nodeType)

boundingBox :: [Node] -> (Int, Int, Int, Int)
boundingBox [] = (0,0,0,0)
boundingBox ns =
  ( minimum (map x ns)
  , maximum (map x ns)
  , minimum (map y ns)
  , maximum (map y ns)
  )

calculateStats :: Canvas -> ()
calculateStats = const ()

-- | Generate canvas from directory tree
runFromTree :: TreeOptions -> IO ()
runFromTree opts@TreeOptions{..} = do
  putStrLn $ "Generating tree visualization for: " ++ treePath
  putStrLn $ "Output: " ++ treeOutput
  canvas <- directoryToCanvas treePath opts
  BL.writeFile treeOutput (encode canvas)
  putStrLn $ "Canvas written to: " ++ treeOutput
  putStrLn $ "Nodes: " ++ show (length (nodes canvas))
  putStrLn $ "Edges: " ++ show (length (edges canvas))

-- | Watch directory for changes
runWatchTree :: WatchOptions -> IO ()
runWatchTree opts = do
  putStrLn $ "Watching directory: " ++ watchPath opts
  putStrLn $ "Output: " ++ watchOutput opts
  putStrLn $ "Update interval: " ++ show (watchInterval opts) ++ " seconds"
  putStrLn "Press Ctrl+C to stop"
  loop opts
  where
    loop opts = do
      canvas <- directoryToCanvas (watchPath opts) (watchTreeOptions opts)
      BL.writeFile (watchOutput opts) (encode canvas)
      putStrLn "Updated..."
      threadDelay (watchInterval opts * 1000000)
      loop opts

-- | Helper functions
die :: String -> IO a
die msg = do
  hPutStrLn stderr msg
  exitFailure

isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing _ = False
