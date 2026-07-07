{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Desktop.TreeCanvas
  ( directoryToCanvas
  , TreeOptions(..)
  , WatchOptions(..)
  , TreeLayout(..)
  , ColorBy(..)
  , FileInfo(..)
  , getFileInfo
  , formatSize
  ) where

import Desktop.CanvasEDSL
import qualified Data.Text as T
import Data.Text (Text)
import System.Directory
import System.FilePath
import Data.Time.Clock (UTCTime, getCurrentTime)
import Control.Concurrent (threadDelay)
import Control.Exception (catch, SomeException)
import Control.Monad (forM, forM_, when)
import Data.List (sortBy, isPrefixOf)
import Data.Function (on)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Text.Printf (printf)
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy.Char8 as BL

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
  , watchInterval :: Int
  , watchTreeOptions :: TreeOptions
  } deriving (Show)

data TreeLayout
  = TreeLayoutVertical
  | TreeLayoutHorizontal
  | TreeLayoutRadial
  | TreeLayoutIndented
  deriving (Show, Eq)

data ColorBy
  = ColorBySize
  | ColorByType
  | ColorByDepth
  | ColorByAge
  deriving (Show, Eq)

data FileInfo = FileInfo
  { fiPath :: FilePath
  , fiName :: String
  , fiIsDir :: Bool
  , fiSize :: Integer
  , fiDepth :: Int
  , fiModified :: UTCTime
  , fiChildren :: [FileInfo]
  } deriving (Show)

directoryToCanvas :: FilePath -> TreeOptions -> IO Canvas
directoryToCanvas rootPath opts = do
  rootInfo <- getFileInfo rootPath 0 opts
  
  let allInfos = flattenTree rootInfo
      filteredInfos = applyFilters allInfos opts
      limitedInfos = applyLimit filteredInfos opts
      nodes = map (fileInfoToNode opts) limitedInfos
      edges = if treeShowEdges opts
              then generateEdges limitedInfos
              else []
  
  let positionedNodes = case treeLayout opts of
        TreeLayoutVertical -> layoutVertical nodes limitedInfos opts
        TreeLayoutHorizontal -> layoutHorizontal nodes limitedInfos opts
        TreeLayoutRadial -> layoutRadial nodes limitedInfos opts
        TreeLayoutIndented -> layoutIndented nodes limitedInfos opts
  
  return $ Canvas positionedNodes edges

getFileInfo :: FilePath -> Int -> TreeOptions -> IO FileInfo
getFileInfo path depth opts = do
  isDir <- doesDirectoryExist path
  let name = takeFileName path
      includeFile = if treeIncludeHidden opts
                    then True
                    else not (isHidden name)
  
  if not includeFile || isExcluded path opts
    then return $ FileInfo path name False 0 depth (error "no time") []
    else do
      size <- if isDir
              then getDirSize path
              else (fromIntegral <$> getFileSize path) `catch` (\(_ :: SomeException) -> return 0)
      modTime <- getModTime path
      
      children <- if isDir && shouldTraverse depth opts
                  then do
                    contents <- listDirectory path
                    let fullPaths = map (path </>) contents
                    infos <- forM fullPaths $ \p -> 
                      getFileInfo p (depth + 1) opts
                    return $ filter (not . null . fiPath) infos
                  else return []
      
      return $ FileInfo path name isDir size depth modTime children
  where
    isHidden ('.':_) = True
    isHidden _ = False
    
    isExcluded p opts = any (`isPrefixOf` p) (treeExclude opts)
    
    shouldTraverse d opts = case treeDepth opts of
      Nothing -> True
      Just maxDepth -> d < maxDepth

getDirSize :: FilePath -> IO Integer
getDirSize path = do
  contents <- listDirectory path
  sizes <- forM contents $ \name -> do
    let fullPath = path </> name
    isDir <- doesDirectoryExist fullPath
    if isDir
      then getDirSize fullPath
      else getFileSize fullPath `catch` (\(_ :: SomeException) -> return 0)
  return $ sum sizes

getModTime :: FilePath -> IO UTCTime
getModTime _ = getCurrentTime

flattenTree :: FileInfo -> [FileInfo]
flattenTree fi = fi : concatMap flattenTree (fiChildren fi)

applyFilters :: [FileInfo] -> TreeOptions -> [FileInfo]
applyFilters infos opts = 
  filter (\fi -> not (any (`isPrefixOf` fiPath fi) (treeExclude opts))) infos

applyLimit :: [FileInfo] -> TreeOptions -> [FileInfo]
applyLimit infos opts = case treeMaxNodes opts of
  Nothing -> infos
  Just limit -> take limit infos

fileInfoToNode :: TreeOptions -> FileInfo -> Node
fileInfoToNode opts fi =
  let nid = NodeId $ T.pack $ cleanId $ fiPath fi
      baseSize = treeNodeSize opts
      sizeFactor = if fiIsDir fi
                   then 1.2
                   else 1.0
      logSize = if fiSize fi > 0 
                then logBase 10 (fromIntegral (fiSize fi) + 1) / 5 
                else 0
      nodeSize = round (fromIntegral baseSize * sizeFactor * (0.8 + 0.4 * logSize))
      
      nodeColor = case treeColorBy opts of
        ColorBySize -> Just $ sizeToColor (fiSize fi)
        ColorByType -> Just $ typeToColor fi
        ColorByDepth -> Just $ depthToColor (fiDepth fi)
        ColorByAge -> Just $ ageToColor (fiModified fi)
      
      displayName = takeFileName (fiPath fi)
      label = displayName
  in if fiIsDir fi
     then (groupNode nid (0,0) (nodeSize, nodeSize `div` 2) (T.pack label)) { color = nodeColor }
     else (fileNode nid (0,0) (nodeSize, nodeSize `div` 2) (T.pack $ fiPath fi)) { color = nodeColor, nodeLabel = Just (T.pack label) }

generateEdges :: [FileInfo] -> [Edge]
generateEdges infos = concatMap generateForNode infos
  where
    generateForNode :: FileInfo -> [Edge]
    generateForNode fi = map (makeEdge fi) (fiChildren fi)
    
    makeEdge :: FileInfo -> FileInfo -> Edge
    makeEdge parent child = 
      let baseEdge = flow (EdgeId $ T.pack $ "e-" ++ cleanId (fiPath parent) ++ "-" ++ cleanId (fiPath child))
                 (NodeId $ T.pack $ cleanId $ fiPath parent)
                 Top
                 (NodeId $ T.pack $ cleanId $ fiPath child)
                 Bottom
      in baseEdge { edgeColor = Just $ PresetColor 5 }

cleanId :: FilePath -> String
cleanId = map (\c -> if c `elem` ("/\\.- " :: String) then '_' else c)

formatSize :: Integer -> String
formatSize bytes
  | bytes >= 10^12 = show (fromIntegral bytes / 10^12 :: Double) ++ "T"
  | bytes >= 10^9  = show (fromIntegral bytes / 10^9 :: Double) ++ "G"
  | bytes >= 10^6  = show (fromIntegral bytes / 10^6 :: Double) ++ "M"
  | bytes >= 10^3  = show (fromIntegral bytes / 10^3 :: Double) ++ "K"
  | otherwise      = show bytes

sizeToColor :: Integer -> CanvasColor
sizeToColor size
  | size >= 10^9  = PresetColor 1
  | size >= 10^8  = PresetColor 2
  | size >= 10^7  = PresetColor 3
  | size >= 10^6  = PresetColor 4
  | size >= 10^5  = PresetColor 5
  | otherwise     = PresetColor 6

typeToColor :: FileInfo -> CanvasColor
typeToColor fi
  | fiIsDir fi = PresetColor 4
  | otherwise = case takeExtension (fiPath fi) of
      ".hs" -> PresetColor 5
      ".py" -> PresetColor 3
      ".js" -> PresetColor 2
      ".ts" -> PresetColor 2
      ".md" -> PresetColor 6
      ".txt" -> PresetColor 1
      ".json" -> PresetColor 4
      ".cabal" -> PresetColor 5
      _ -> PresetColor 5

depthToColor :: Int -> CanvasColor
depthToColor d = PresetColor $ ((d `mod` 6) + 1)

ageToColor :: UTCTime -> CanvasColor
ageToColor _ = PresetColor 4

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
                 n { x = 50 + i * (baseSize + spacing `div` 2)
                   , y = floor startY
                   }) ns [0..]
           in acc ++ positionedDepth
        ) [] grouped
  in positioned

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
                   , y = 50 + i * (baseSize + spacing `div` 2)
                   }) ns [0..]
           in acc ++ positionedDepth
        ) [] grouped
  in positioned

layoutRadial :: [Node] -> [FileInfo] -> TreeOptions -> [Node]
layoutRadial nodes infos _ = 
  let paired = zip infos nodes
  in map snd paired

layoutIndented :: [Node] -> [FileInfo] -> TreeOptions -> [Node]
layoutIndented nodes infos _ = 
  let spacing = 80
      baseSize = 200
      paired = zip infos nodes
      sorted = sortBy (compare `on` (fiPath . fst)) paired
      
      positioned = go 50 sorted
      go _ [] = []
      go y ((fi,node):rest) = 
        let newX = 50 + (fiDepth fi) * (baseSize `div` 2)
            newNode = node { x = newX, y = y }
        in newNode : go (y + baseSize + spacing `div` 2) rest
  in positioned
