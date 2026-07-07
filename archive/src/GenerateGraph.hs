#!/usr/bin/env runhaskell

{-# LANGUAGE OverloadedStrings #-}

import Desktop.CanvasEDSL
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Text (Text)
import qualified Data.Text as T

main :: IO ()
main = do
  let canvas = buildFolderGraph
  BL.writeFile "folder-graph.canvas" (encodePretty canvas)
  putStrLn "Created folder-graph.canvas"

buildFolderGraph :: Canvas
buildFolderGraph = canvas nodes edges
  where
    nodes = 
      [ groupNode "dev-canvas" (0,0) (300,80) "dev-canvas"
          `withColor` Just (PresetColor 1)
      , groupNode "src" (50,120) (200,60) "src/"
          `withColor` Just (PresetColor 2)
      , groupNode "app" (300,120) (200,60) "app/"
          `withColor` Just (PresetColor 2)
      , groupNode "Desktop" (80,220) (200,60) "Desktop/"
          `withColor` Just (PresetColor 3)
      , fileNode "CanvasEDSL.hs" (120,320) (240,80) "CanvasEDSL.hs"
          `withColor` Just (PresetColor 4)
      , fileNode "Main.hs" (340,220) (240,80) "Main.hs"
          `withColor` Just (PresetColor 4)
      , fileNode "cabal" (50,420) (240,80) "json-canvas-cli.cabal"
          `withColor` Just (PresetColor 5)
      , fileNode "User_Guide" (320,420) (240,80) "User_Guide.md"
          `withColor` Just (PresetColor 5)
      , fileNode "Tree_Integration" (600,420) (280,80) "Enhanced CLI with Tree..."
          `withColor` Just (PresetColor 5)
      , fileNode "DevOPS" (600,0) (240,80) "DevOPS.canvas"
          `withColor` Just (PresetColor 6)
      ]

    edges =
      [ edge "e1" "dev-canvas" "src"
      , edge "e2" "dev-canvas" "app"
      , edge "e3" "dev-canvas" "DevOPS"
      , edge "e4" "src" "Desktop"
      , edge "e5" "Desktop" "CanvasEDSL.hs"
      , edge "e6" "app" "Main.hs"
      , edge "e7" "dev-canvas" "cabal"
      , edge "e8" "dev-canvas" "User_Guide"
      , edge "e9" "dev-canvas" "Tree_Integration"
      ]
