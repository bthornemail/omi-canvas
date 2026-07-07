{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.ByteString.Lazy as BL
import Control.Monad (when)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Aeson as A
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Vector as V
import System.Directory (copyFile, createDirectoryIfMissing, doesDirectoryExist, doesFileExist, removePathForcibly)
import System.FilePath ((</>))
import System.Exit (exitFailure)

import Desktop.MdExtract (ExtractConfig (..), ExtractMode (..), extractNdjsonFromMarkdown, extractNdjsonFromTree)
import Desktop.MdManifest (ManifestOptions (..), writeManifest)
import Desktop.MdVerifyEvidence (VerifyConfig (..), verifyEvidenceNdjsonBytes)

assertEq :: String -> BL.ByteString -> BL.ByteString -> IO ()
assertEq label expected actual =
  if expected == actual
    then pure ()
    else do
      putStrLn ("FAIL: " ++ label)
      putStrLn ("expected bytes: " ++ show (BL.length expected))
      putStrLn ("actual bytes:   " ++ show (BL.length actual))
      exitFailure

main :: IO ()
main = do
  md <- TIO.readFile "test/vectors/md-sample.md"
  golden <- BL.readFile "test/vectors/md-extract.golden.ndjson"

  out1 <- case extractNdjsonFromMarkdown True False False False ["ndjson","jsonl","jsonlines","json","hash"] "md-sample.md" md of
    Left err -> do
      putStrLn ("FAIL: extract: " ++ T.unpack err)
      exitFailure
    Right bs -> pure bs

  out2 <- case extractNdjsonFromMarkdown True False False False ["ndjson","jsonl","jsonlines","json","hash"] "md-sample.md" md of
    Left err -> do
      putStrLn ("FAIL: extract determinism: " ++ T.unpack err)
      exitFailure
    Right bs -> pure bs

  assertEq "golden" golden out1
  assertEq "determinism" out1 out2

  -- Prose → canon event extraction (paragraphs outside fences)
  mdProse <- TIO.readFile "test/vectors/md-prose-sample.md"
  proseGolden <- BL.readFile "test/vectors/md-prose-extract.golden.ndjson"
  proseOut <- case extractNdjsonFromMarkdown True False False True ["ndjson"] "md-prose-sample.md" mdProse of
    Left err -> do
      putStrLn ("FAIL: prose extract: " ++ T.unpack err)
      exitFailure
    Right bs -> pure bs
  assertEq "prose golden" proseGolden proseOut
  vProse <- verifyEvidenceNdjsonBytes (VerifyConfig "test/vectors" True) proseOut
  case vProse of
    Left err -> do
      putStrLn ("FAIL: prose verify-evidence: " ++ T.unpack err)
      exitFailure
    Right () -> pure ()

  -- Evidence verification should succeed against the on-disk source bytes.
  v1 <- verifyEvidenceNdjsonBytes (VerifyConfig "test/vectors" True) out1
  case v1 of
    Left err -> do
      putStrLn ("FAIL: verify-evidence: " ++ T.unpack err)
      exitFailure
    Right () -> pure ()

  let bad = "```ndjson\n{not json}\n```\n"
  case extractNdjsonFromMarkdown True False False False ["ndjson"] "bad.md" bad of
    Left _ -> pure ()
    Right _ -> do
      putStrLn "FAIL: strict should reject invalid JSON line in ndjson block"
      exitFailure

  let unclosed = "```ndjson\n{\"a\":1}\n"
  case extractNdjsonFromMarkdown True False False False ["ndjson"] "unclosed.md" unclosed of
    Left _ -> pure ()
    Right _ -> do
      putStrLn "FAIL: strict should reject unclosed fence"
      exitFailure

  -- Negative: tamper with span_start so verification fails.
  let ls = filter (not . BL.null) (BL.split 10 out1)
  case ls of
    [] -> do
      putStrLn "FAIL: expected extracted NDJSON records"
      exitFailure
    (firstLine:restLines) ->
      case A.decode firstLine :: Maybe A.Value of
        Nothing -> do
          putStrLn "FAIL: could not decode first NDJSON line"
          exitFailure
        Just (A.Object o) ->
          case KM.lookup (K.fromText "evidence") o of
            Just (A.Object ev) ->
              case KM.lookup (K.fromText "span_start") ev of
                Just (A.Number n) ->
                  let ev' = KM.insert (K.fromText "span_start") (A.Number (n + 1)) ev
                      o' = KM.insert (K.fromText "evidence") (A.Object ev') o
                      tampered = BL.intercalate "\n" (A.encode (A.Object o') : restLines) <> "\n"
                  in do
                    v2 <- verifyEvidenceNdjsonBytes (VerifyConfig "test/vectors" True) tampered
                    case v2 of
                      Left _ -> pure ()
                      Right () -> do
                        putStrLn "FAIL: verify-evidence should reject tampered span_start"
                        exitFailure
                _ -> do
                  putStrLn "FAIL: expected evidence.span_start"
                  exitFailure
            _ -> do
              putStrLn "FAIL: expected evidence object on first record"
              exitFailure
        Just _ -> do
          putStrLn "FAIL: expected first NDJSON line to be an object"
          exitFailure

  -- Canvas pointers (tree extraction)
  canvasGolden <- BL.readFile "test/vectors/md-canvas-pointers.golden.ndjson"
  manifestGolden <- BL.readFile "test/vectors/md-manifest.golden.json"
  let tmpBase = "dist-newstyle" </> "tmp-md-extract-canvas"
      root = tmpBase </> "root"
      out = tmpBase </> "out"
  exists <- doesDirectoryExist tmpBase
  when exists $ removePathForcibly tmpBase
  createDirectoryIfMissing True root
  copyFile "test/vectors/md-canvas-sample.md" (root </> "md-canvas-sample.md")

  extractNdjsonFromTree
    ExtractConfig
      { ecRoot = root
      , ecOut = out
      , ecStrict = True
      , ecMode = ModeAll
      , ecLangs = ["canvas"]
      , ecAggregate = True
      , ecLooseNdjson = False
      , ecCanonFilter = False
      , ecEmitProseEvents = False
      , ecEmitCanvasPointers = True
      }

  pointers <- BL.readFile (out </> "ndjson" </> "canvas.blocks.ndjson")
  assertEq "canvas pointers golden" canvasGolden pointers

  canvasOutExists <- doesFileExist (out </> "canvas" </> "md-canvas-sample.md.block0.canvas.json")
  if canvasOutExists
    then pure ()
    else do
      putStrLn "FAIL: expected extracted canvas JSON file to exist"
      exitFailure

  writeManifest
    ManifestOptions
      { moRoot = root
      , moOut = out
      , moMode = "all"
      , moLangs = ["canvas"]
      , moStrict = True
      , moAggregate = True
      , moLooseNdjson = False
      , moCanonFilter = False
      , moEmitProseEvents = False
      , moEmitCanvasPointers = True
      , moEmitManifest = True
      , moManifestPath = out </> "manifest.json"
      , moIncludeGitHead = False
      , moTimestamp = False
      , moToolName = "json-canvas"
      , moToolVersion = "0.1.0.0"
      }

  manifest <- BL.readFile (out </> "manifest.json")
  assertEq "manifest golden" manifestGolden manifest
