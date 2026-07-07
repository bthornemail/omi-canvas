#!/usr/bin/env runhaskell

{-# LANGUAGE OverloadedStrings #-}

import Desktop.CanvasEDSL
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (listDirectory)
import System.FilePath ((</>))
import Control.Monad (forM, filterM)
import System.Directory (doesDirectoryExist, doesFileExist)

data TreeItem = TreeNode Text [(TreeItem, Int)] | TreeFile Text
  deriving (Show)

main :: IO ()
main = do
  putStrLn "Building folder graph for /home/main/devops..."
  tree <- buildTree "/home/main/devops" 0
  let canvas = treeToCanvas tree
  BL.writeFile "/home/main/devops/dev-canvas/folder-graph.canvas" (encodePretty canvas)
  putStrLn "Created /home/main/devops/dev-canvas/folder-graph.canvas"

buildTree :: FilePath -> Int -> IO TreeItem
buildTree path depth
  | depth > 3 = return $ TreeNode (T.pack (last (splitPath path))) []
  | otherwise = do
      isDir <- doesDirectoryExist path
      if isDir
        then do
          entries <- listDirectory path
          let filtered = filter (\e -> not (elem e [".git", "node_modules", "dist-newstyle", ".obsidian", ".claude", ".cursor", "Glossary", ".metadata", ".metadata-kernel", "__pycache__", ".pytest_cache", "venv", ".lsp", ".codacy", ".github", ".vscode", ".clj-kondo"])) entries
          children <- forM filtered $ \entry -> do
            let fullPath = path </> entry
            child <- buildTree fullPath (depth + 1)
            return (child, depth + 1)
          return $ TreeNode (T.pack (last (splitPath path))) children
        else return $ TreeFile (T.pack (last (splitPath path)))

splitPath :: FilePath -> [String]
splitPath p = case break (== '/') p of
  (a, "") -> [a]
  (a, rest) -> a : splitPath (tail rest)

treeToCanvas :: TreeItem -> Canvas
treeToCanvas tree = canvas nodes edges
  where
    (nodes, edges, _, _) = buildGraph tree 0 0 (0, 0)

buildGraph :: TreeItem -> Int -> Int -> (Int, Int) -> ([Node], [Edge], Int, Int)
buildGraph (TreeNode name children) x offset (nextId, nextEdge) = 
  let 
    nodeId = "node" <> T.pack (show nextId)
    currentNode = groupNode (T.pack nodeId) (x, offset) (200, 60) name `withColor` Just (PresetColor (mod nextId 6 + 1))
    
    (childNodes, childEdges, _, finalNextId, finalNextEdge, maxY) = 
      foldl (\(ns, es, off', nid, eid, my) (child, _) ->
        let (cns, ces, off'', _, eid', my') = buildGraph child (x + 250) off' (nid, eid)
        in (ns ++ cns, es ++ ces, off'', nid, eid', max my my')
      ) ([], [], offset + 80, nextId + 1, nextEdge, offset) children
    
    allNodes = currentNode : childNodes
    allEdges = childEdges
    
    newOffset = offset + 80
  in (allNodes, allEdges, newOffset, finalNextId, finalNextEdge, maxY)
buildGraph (TreeFile name) x offset (nextId, nextEdge) =
  let
    nodeId = "node" <> T.pack (show nextId)
    node = fileNode (T.pack nodeId) (x, offset) (200, 50) name `withColor` Just (PresetColor (mod nextId 6 + 1))
  in ([node], [], offset + 60, nextId + 1, nextEdge, offset + 60)
