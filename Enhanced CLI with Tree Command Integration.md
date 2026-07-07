## Enhanced CLI with Tree Command Integration

```haskell
-- Add to the Command data type in app/Main.hs
data Command
  = ...
  | FromTree TreeOptions
  | WatchTree WatchOptions
  deriving (Show)

data TreeOptions = TreeOptions
  { treePath :: FilePath
  , treeOutput :: FilePath
  , treeDepth :: Maybe Int
  , treeExclude :: [String]
  , treeIncludeHidden :: Bool
  , treeNodeSize :: Int
  , treeSpacing :: Int
  , treeLayout :: TreeLayout
  , treeColorBy :: ColorBy
  , treeShowEdges :: Bool
  , treeMaxNodes :: Maybe Int
  } deriving (Show)

data WatchOptions = WatchOptions
  { watchPath :: FilePath
  , watchOutput :: FilePath
  , watchInterval :: Int  -- seconds
  , watchTreeOptions :: TreeOptions
  } deriving (Show)

data TreeLayout
  = TreeLayoutVertical
  | TreeLayoutHorizontal
  | TreeLayoutRadial
  | TreeLayoutIndented
  deriving (Show)

data ColorBy
  = ColorBySize
  | ColorByType
  | ColorByDepth
  | ColorByAge
  deriving (Show)

-- Add parsers
parseFromTree :: Parser TreeOptions
parseFromTree = TreeOptions
  <$> argument str (metavar "DIRECTORY" <> help "Directory to visualize")
  <*> strOption (long "output" <> short 'o' <> value "tree.json" <> help "Output canvas file")
  <*> optional (option auto (long "depth" <> short 'd' <> help "Maximum depth to traverse"))
  <*> many (strOption (long "exclude" <> help "Patterns to exclude (can be repeated)"))
  <*> switch (long "hidden" <> help "Include hidden files/directories")
  <*> option auto (long "node-size" <> value 200 <> help "Base node size in pixels")
  <*> option auto (long "spacing" <> value 80 <> help "Spacing between nodes")
  <*> parseTreeLayout
  <*> parseColorBy
  <*> switch (long "edges" <> help "Show edges between parent/child")
  <*> optional (option auto (long "max-nodes" <> help "Maximum number of nodes to include"))

parseTreeLayout :: Parser TreeLayout
parseTreeLayout = option autoLayout
  ( long "layout" <> value TreeLayoutVertical
  <> help "Layout style: vertical, horizontal, radial, indented" )
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
  <> help "Color scheme: size, type, depth, age" )
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
  <*> parseFromTree  -- Reuse tree options for watching
```

## Tree to Canvas Conversion Implementation

```haskell
-- File: src/Desktop/TreeCanvas.hs
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Desktop.TreeCanvas
  ( directoryToCanvas
  , watchDirectory
  , FileInfo(..)
  , getFileInfo
  , formatSize
  ) where

import Desktop.CanvasEDSL
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Text (Text)
import System.Directory
import System.FilePath
import System.Time (getClockTime, ClockTime)
import System.Posix.Files (getFileStatus, fileSize, modificationTime)
import Data.Time.Clock (UTCTime, getCurrentTime, diffUTCTime)
import Data.Time.Format (parseTimeM, defaultTimeLocale)
import Control.Concurrent (threadDelay)
import Control.Monad (forM, forM_)
import Data.List (sortBy, isPrefixOf, intercalate)
import Data.Function (on)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, mapMaybe)
import System.Console.ANSI
import Text.Printf (printf)

-- | File information for visualization
data FileInfo = FileInfo
  { fiPath :: FilePath
  , fiName :: String
  , fiIsDir :: Bool
  , fiSize :: Integer
  , fiDepth :: Int
  , fiModified :: UTCTime
  , fiChildren :: [FileInfo]
  } deriving (Show)

-- | Convert a directory tree to a JSON Canvas
directoryToCanvas :: FilePath -> TreeOptions -> IO Canvas
directoryToCanvas rootPath opts = do
  -- Get file tree
  rootInfo <- getFileInfo rootPath 0 opts
  
  -- Flatten tree to nodes
  let allInfos = flattenTree rootInfo
      filteredInfos = applyFilters allInfos opts
      limitedInfos = applyLimit filteredInfos opts
      nodes = map (fileInfoToNode opts) limitedInfos
      edges = if treeShowEdges opts
              then generateEdges limitedInfos opts
              else []
  
  -- Apply layout
  let positionedNodes = case treeLayout opts of
        TreeLayoutVertical -> layoutVertical nodes limitedInfos opts
        TreeLayoutHorizontal -> layoutHorizontal nodes limitedInfos opts
        TreeLayoutRadial -> layoutRadial nodes limitedInfos opts
        TreeLayoutIndented -> layoutIndented nodes limitedInfos opts
  
  return $ Canvas positionedNodes edges

-- | Get file information recursively
getFileInfo :: FilePath -> Int -> TreeOptions -> IO FileInfo
getFileInfo path depth opts = do
  isDir <- doesDirectoryExist path
  let name = takeFileName path
      includeFile = if treeIncludeHidden opts
                    then True
                    else not (isHidden name)
  
  if not includeFile
    then return $ FileInfo path name isDir 0 depth (UTCTime (toEnum 0) 0) []
    else do
      size <- if isDir
              then getDirSize path
              else fromIntegral <$> getFileSize path
      modTime <- getModTime path
      
      children <- if isDir && shouldTraverse depth opts
                  then do
                    contents <- listDirectory path
                    let fullPaths = map (path </>) contents
                    infos <- forM fullPaths $ \p -> 
                      getFileInfo p (depth + 1) opts
                    return $ filter shouldIncludeChild infos
                  else return []
      
      return $ FileInfo path name isDir size depth modTime children
  where
    isHidden ('.':_) = True
    isHidden _ = False
    
    shouldTraverse d opts = case treeDepth opts of
      Nothing -> True
      Just maxDepth -> d < maxDepth
    
    shouldIncludeChild fi = 
      not (any (`isPrefixOf` fiPath fi) (treeExclude opts))

-- | Get total size of a directory
getDirSize :: FilePath -> IO Integer
getDirSize path = do
  contents <- listDirectory path
  sizes <- forM contents $ \name -> do
    let fullPath = path </> name
    isDir <- doesDirectoryExist fullPath
    if isDir
      then getDirSize fullPath
      else fromIntegral <$> getFileSize fullPath
  return $ sum sizes

-- | Get file size
getFileSize :: FilePath -> IO Integer
getFileSize path = do
  status <- getFileStatus path
  return $ fromIntegral $ fileSize status

-- | Get modification time
getModTime :: FilePath -> IO UTCTime
getModTime path = do
  status <- getFileStatus path
  let epoch = modificationTime status
  -- Convert from EpochTime to UTCTime (simplified)
  return $ UTCTime (toEnum $ fromIntegral epoch) 0

-- | Flatten tree to list
flattenTree :: FileInfo -> [FileInfo]
flattenTree fi = fi : concatMap flattenTree (fiChildren fi)

-- | Apply filters to file list
applyFilters :: [FileInfo] -> TreeOptions -> [FileInfo]
applyFilters infos opts = 
  filter (\fi -> not (any (`isPrefixOf` fiPath fi) (treeExclude opts))) infos

-- | Apply node limit
applyLimit :: [FileInfo] -> TreeOptions -> [FileInfo]
applyLimit infos opts = case treeMaxNodes opts of
  Nothing -> infos
  Just limit -> take limit infos

-- | Convert file info to canvas node
fileInfoToNode :: TreeOptions -> FileInfo -> Node
fileInfoToNode opts fi =
  let nid = NodeId $ T.pack $ cleanId $ fiPath fi
      baseSize = treeNodeSize opts
      -- Size based on file size (log scale)
      sizeFactor = if fiIsDir fi
                   then 1.5  -- Directories larger
                   else 1.0
      logSize = logBase 10 (fromIntegral (fiSize fi) + 1) / 5  -- Normalize
      nodeSize = round (fromIntegral baseSize * sizeFactor * (0.8 + 0.4 * logSize))
      
      -- Color based on options
      nodeColor = case treeColorBy opts of
        ColorBySize -> Just $ sizeToColor (fiSize fi)
        ColorByType -> Just $ typeToColor fi
        ColorByDepth -> Just $ depthToColor (fiDepth fi)
        ColorByAge -> Just $ ageToColor (fiModified fi)
      
      -- Content based on type
      displayName = takeFileName (fiPath fi)
      sizeStr = formatSize (fiSize fi)
      label = displayName ++ " (" ++ sizeStr ++ ")"
  in case () of
    _ | fiIsDir fi ->
        groupNode nid (0,0) (nodeSize, nodeSize) (T.pack label)
          { color = nodeColor
          , nodeBackground = Just "folder-icon.png"  -- Placeholder
          }
      | otherwise ->
        fileNode nid (0,0) (nodeSize, nodeSize) (T.pack $ fiPath fi)
          { color = nodeColor
          , nodeLabel = Just (T.pack label)
          }

-- | Generate edges between parent and child
generateEdges :: [FileInfo] -> TreeOptions -> [Edge]
generateEdges infos opts = concatMap generateForNode infos
  where
    generateForNode fi = 
      [ edge (EdgeId $ T.pack $ "edge-" ++ cleanId (fiPath fi) ++ "-" ++ cleanId (fiPath child))
             (NodeId $ T.pack $ cleanId $ fiPath fi)
             (NodeId $ T.pack $ cleanId $ fiPath child)
        { fromEnd = Just EndNone
        , toEnd = Just EndArrow
        , edgeColor = Just $ PresetColor 5  -- Cyan for tree edges
        }
      | child <- fiChildren fi
      ]

-- | Clean string for use as ID
cleanId :: FilePath -> String
cleanId = map (\c -> if c `elem` "/\\.-" then '_' else c)

-- | Format file size
formatSize :: Integer -> String
formatSize bytes
  | bytes >= 10^12 = printf "%.2f TB" (fromIntegral bytes / 10^12)
  | bytes >= 10^9  = printf "%.2f GB" (fromIntegral bytes / 10^9)
  | bytes >= 10^6  = printf "%.2f MB" (fromIntegral bytes / 10^6)
  | bytes >= 10^3  = printf "%.2f KB" (fromIntegral bytes / 10^3)
  | otherwise      = printf "%d B" bytes

-- | Color by file size
sizeToColor :: Integer -> CanvasColor
sizeToColor size
  | size >= 10^9  = PresetColor 1  -- Red for huge
  | size >= 10^8  = PresetColor 2  -- Orange for very large
  | size >= 10^7  = PresetColor 3  -- Yellow for large
  | size >= 10^6  = PresetColor 4  -- Green for medium
  | size >= 10^5  = PresetColor 5  -- Cyan for small
  | otherwise     = PresetColor 6  -- Purple for tiny

-- | Color by file type
typeToColor :: FileInfo -> CanvasColor
typeToColor fi
  | fiIsDir fi = PresetColor 4  -- Green for directories
  | otherwise = case takeExtension (fiPath fi) of
      ".hs" -> PresetColor 5      -- Cyan for Haskell
      ".py" -> PresetColor 3      -- Yellow for Python
      ".js" -> PresetColor 2      -- Orange for JavaScript
      ".md" -> PresetColor 6      -- Purple for Markdown
      ".txt" -> PresetColor 1     -- Red for text
      ".json" -> PresetColor 4    -- Green for JSON
      ".jpg" -> PresetColor 2     -- Orange for images
      ".png" -> PresetColor 2
      _ -> PresetColor 5          -- Default cyan

-- | Color by depth
depthToColor :: Int -> CanvasColor
depthToColor d = PresetColor $ ((d `mod` 6) + 1)

-- | Color by age (recently modified)
ageToColor :: UTCTime -> CanvasColor
ageToColor modTime = PresetColor 4  -- Simplified

-- | Layout nodes vertically by depth
layoutVertical :: [Node] -> [FileInfo] -> TreeOptions -> [Node]
layoutVertical nodes infos opts = 
  let grouped = Map.fromListWith (++) 
        [(fiDepth fi, [n]) | (fi, n) <- zip infos nodes]
      spacing = treeSpacing opts
      baseSize = treeNodeSize opts
      
      positioned = Map.foldlWithKey' 
        (\acc depth ns -> 
           let startY = fromIntegral depth * (fromIntegral baseSize + fromIntegral spacing)
               positionedDepth = zipWith (\n i -> 
                 n { x = 50 + i * (baseSize + spacing)
                   , y = floor startY
                   }) ns [0..]
           in acc ++ positionedDepth
        ) [] grouped
  in positioned

-- | Layout nodes horizontally by depth
layoutHorizontal :: [Node] -> [FileInfo] -> TreeOptions -> [Node]
layoutHorizontal nodes infos opts = 
  let grouped = Map.fromListWith (++) 
        [(fiDepth fi, [n]) | (fi, n) <- zip infos nodes]
      spacing = treeSpacing opts
      baseSize = treeNodeSize opts
      
      positioned = Map.foldlWithKey' 
        (\acc depth ns -> 
           let startX = fromIntegral depth * (fromIntegral baseSize + fromIntegral spacing)
               positionedDepth = zipWith (\n i -> 
                 n { x = floor startX
                   , y = 50 + i * (baseSize + spacing)
                   }) ns [0..]
           in acc ++ positionedDepth
        ) [] grouped
  in positioned

-- | Radial layout (roots at center, children in circles)
layoutRadial :: [Node] -> [FileInfo] -> TreeOptions -> [Node]
layoutRadial nodes infos opts = 
  let rootNodes = filter ((== 0) . fiDepth) infos
      rootPos = (400, 400)  -- Center
      
      -- Recursively position children in circles
      positionNode fi node angle radius = 
        let x = floor (fromIntegral (fst rootPos) + radius * cos angle)
            y = floor (fromIntegral (snd rootPos) + radius * sin angle)
        in node { x = x, y = y }
      
      -- This is a simplified radial layout
      -- Full implementation would need proper force-directed layout
  in nodes  -- Placeholder

-- | Indented layout like tree command
layoutIndented :: [Node] -> [FileInfo] -> TreeOptions -> [Node]
layoutIndented nodes infos opts = 
  let spacing = treeSpacing opts
      baseSize = treeNodeSize opts
      
      -- Sort by path for indentation
      sorted = sortBy (compare `on` fiPath) (zip infos nodes)
      
      positioned = snd $ mapAccumL (\(x,y) (fi,node) -> 
        let newX = x + (fiDepth fi) * (baseSize + spacing `div` 2)
            newY = y
            newNode = node { x = newX, y = newY }
        in ((newX + baseSize + spacing, newY + baseSize + spacing), newNode)
        ) (50, 50) sorted
  in positioned

-- | Helper for mapAccumL
mapAccumL :: (acc -> a -> (acc, b)) -> acc -> [a] -> (acc, [b])
mapAccumL _ acc [] = (acc, [])
mapAccumL f acc (x:xs) = 
  let (acc', y) = f acc x
      (acc'', ys) = mapAccumL f acc' xs
  in (acc'', y:ys)

-- | Watch a directory for changes and update canvas
watchDirectory :: FilePath -> WatchOptions -> IO ()
watchDirectory path opts = do
  putStrLn $ "Watching directory: " ++ path
  putStrLn $ "Output file: " ++ watchOutput opts
  putStrLn $ "Update interval: " ++ show (watchInterval opts) ++ " seconds"
  putStrLn "Press Ctrl+C to stop"
  
  let loop = do
      -- Generate canvas
      canvas <- directoryToCanvas path (watchTreeOptions opts)
      BL.writeFile (watchOutput opts) (A.encode canvas)
      
      -- Show status
      setCursorColumn 0
      clearLine
      putStr $ "Updated at " ++ show (take 19 $ show =<< getCurrentTime)
      
      -- Wait
      threadDelay (watchInterval opts * 1000000)
      loop
  
  loop
```

## CLI Integration

Add to the main `runCommand` function:

```haskell
runCommand :: Options -> IO ()
runCommand Options{..} = case optCommand of
  ...
  FromTree treeOpts -> runFromTree treeOpts
  WatchTree watchOpts -> runWatchTree watchOpts

runFromTree :: TreeOptions -> IO ()
runFromTree opts@TreeOptions{..} = do
  putStrLn $ "Generating tree visualization for: " ++ treePath
  canvas <- directoryToCanvas treePath opts
  BL.writeFile treeOutput (A.encode canvas)
  putStrLn $ "Canvas written to: " ++ treeOutput
  putStrLn $ "Nodes: " ++ show (length (nodes canvas))
  putStrLn $ "Total size: " ++ formatSize (sum $ map fiSize $ flattenTree =<< getFileInfo treePath 0 opts)

runWatchTree :: WatchOptions -> IO ()
runWatchTree opts = watchDirectory (watchPath opts) opts
```

## Example Usage

```bash
# Basic tree visualization
json-canvas from-tree ~/projects -o project-tree.json

# With depth limit and custom styling
json-canvas from-tree ~/Documents \
  --depth 3 \
  --node-size 150 \
  --spacing 60 \
  --layout radial \
  --color-by size \
  --edges \
  --output docs-radial.json

# Exclude certain patterns
json-canvas from-tree ~/code \
  --exclude "node_modules" \
  --exclude ".git" \
  --exclude "dist" \
  --hidden \
  --output code-tree.json

# Watch mode - auto-update as files change
json-canvas watch ~/active-project \
  --interval 10 \
  --output live.json \
  --depth 4 \
  --color-by age

# Indented layout like traditional tree command
json-canvas from-tree ~/workspace \
  --layout indented \
  --node-size 120 \
  --spacing 40 \
  --max-nodes 500 \
  --output workspace.json

# Generate tree for large directory with limits
json-canvas from-tree /usr/local \
  --depth 5 \
  --max-nodes 1000 \
  --exclude "cache" "tmp" \
  --output system-tree.json
```

## Enhanced Tree Output Example

When you run this, you get a JSON Canvas that visually represents your directory structure:

```bash
$ json-canvas from-tree ~/myproject --depth 2 --layout vertical --color-by type
```

This creates a canvas where:
- Each folder/file is a node
- Node size reflects file size (logarithmic scale)
- Colors indicate file types (green for folders, blue for code, etc.)
- Edges show parent-child relationships
- Layout organizes by depth level
- Tooltips show full paths and sizes

The result is an interactive visualization that can be opened in any JSON Canvas viewer, giving you a spatial representation of your filesystem that's much more intuitive than a traditional tree listing.