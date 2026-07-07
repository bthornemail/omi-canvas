{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Aeson as A
import Data.Aeson ((.=), object)
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.Environment (getArgs, lookupEnv)
import Control.Applicative ((<|>))
import System.Process (readProcess)
import ULP.Canonical (sha256Hex, stableJson)
import ULP.Merge (chooseTip, mergeCommits)
import ULP.NDJSON (decodeFile)
import ULP.Runtime
import ULP.Storage (ensureRoot, loadLog, logPath, writeLog)
import ULP.Types
import ULP.Validate (validateCommit)

nowMs :: IO Int64
nowMs = round . (* 1000) <$> getPOSIXTime

trim :: String -> String
trim = f . f
  where
    f = reverse . dropWhile (`elem` ['\n', '\r', ' ', '\t'])

defaultOptionsIO :: FilePath -> IO RuntimeOptions
defaultOptionsIO rootDir = do
  signerScript <- lookupEnv "ULP_TEST_SIGNER_JS"
  verifierScript <- lookupEnv "ULP_TEST_VERIFIER_JS"
  expectedSigner <- lookupEnv "ULP_TEST_EXPECTED_SIGNER"
  expectedAddress <- lookupEnv "ULP_TEST_EXPECTED_ADDRESS"
  let expectedIdentity = fromMaybe "" (expectedSigner <|> expectedAddress)

  let signerHook =
        fmap
          (\script c msg -> do
              out <- readProcess "node" [script, T.unpack msg] ""
              pure (T.pack (trim out))
          )
          signerScript

      verifierHook =
        fmap
          (\script c msg -> do
              out <- readProcess "node" [script, T.unpack msg, T.unpack (sig c), expectedIdentity] ""
              pure (trim out == "true")
          )
          verifierScript

  pure
    RuntimeOptions
      { clock = nowMs
      , counterStart = 0
      , signer = signerHook
      , verifier = verifierHook
      , storageRoot = rootDir
      }

validateSequence :: ValidationOptions -> [CommitEvent] -> IO ([ValidationResult], [CommitEvent])
validateSequence opts = go Nothing [] []
  where
    go _ accR accC [] = pure (reverse accR, reverse accC)
    go prevValid accR accC (c : cs) = do
      r <- validateCommit opts prevValid c
      let nextPrev = if valid r then Just c else prevValid
          nextCommits = if valid r then (c : accC) else accC
      go nextPrev (r : accR) nextCommits cs

faceStatusText :: FaceStatus -> Text
faceStatusText Pass = "pass"
faceStatusText Fail = "fail"
faceStatusText Unknown = "unknown"

fingerprintValue :: [CommitEvent] -> [ValidationResult] -> A.Value
fingerprintValue commits results =
  let tipCommit = chooseTip commits
      validCount = length (filter valid results)
      invalidCount = length results - validCount
      facesStatus = maybe [] (map (faceStatusText . status) . faces) tipCommit
      tipHash = fmap self_hash tipCommit
      tipMerkleRoot = tipCommit >>= merkle >>= (Just . root)
      centroidVal = fmap centroid tipCommit
      stopMetricVal = fmap stop_metric centroidVal
      closureVal = fmap closure_ratio centroidVal
      sabbathVal = fmap sabbath centroidVal
      base =
        object
          [ "valid_count" .= validCount
          , "invalid_count" .= invalidCount
          , "tip_self_hash" .= tipHash
          , "tip_merkle_root" .= tipMerkleRoot
          , "stop_metric" .= stopMetricVal
          , "closure_ratio" .= closureVal
          , "sabbath" .= sabbathVal
          , "faces_status" .= facesStatus
          ]
      hashVal = sha256Hex (stableJson base)
   in object
        [ "valid_count" .= validCount
        , "invalid_count" .= invalidCount
        , "tip_self_hash" .= tipHash
        , "tip_merkle_root" .= tipMerkleRoot
        , "stop_metric" .= stopMetricVal
        , "closure_ratio" .= closureVal
        , "sabbath" .= sabbathVal
        , "faces_status" .= facesStatus
        , "fingerprint_hash" .= hashVal
        ]

runValidate :: Runtime -> FilePath -> IO ()
runValidate rt fp = do
  commits <- decodeFile fp
  let opts = ValidationOptions {signatureVerifier = verifier (options rt), invariantChecker = Nothing}
  (rs, _) <- validateSequence opts commits
  putStrLn ("validated records: " ++ show (length rs))
  putStrLn ("valid count: " ++ show (length (filter valid rs)))
  mapM_ (\r -> if valid r then pure () else putStrLn ("invalid errors: " ++ show (errors r))) rs

runFingerprint :: Runtime -> FilePath -> IO ()
runFingerprint rt fp = do
  commits <- decodeFile fp
  let opts = ValidationOptions {signatureVerifier = verifier (options rt), invariantChecker = Nothing}
  (rs, validCommits) <- validateSequence opts commits
  LBS.putStrLn (A.encode (fingerprintValue validCommits rs))

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["init", "--root", rootDir] -> do
      ensureRoot rootDir
      putStrLn ("initialized: " ++ rootDir)

    ["commit", "--root", rootDir, "--type", _ty] -> do
      opts <- defaultOptionsIO rootDir
      rt <- initRuntime opts
      c <- appendRuntimeCommit rt Commit
      putStrLn ("commit: " ++ show (cid c))

    ["validate", "--root", rootDir] -> do
      opts <- defaultOptionsIO rootDir
      rt <- initRuntime opts
      runValidate rt (logPath rootDir)

    ["merge", "--root", rootDir, "--from", remote] -> do
      local <- loadLog rootDir
      remoteC <- decodeFile remote
      let merged = mergeCommits local remoteC
      writeLog rootDir merged
      putStrLn ("merged commits: " ++ show (length merged))
      case chooseTip merged of
        Nothing -> putStrLn "tip: none"
        Just c -> putStrLn ("tip: " ++ show (cid c))

    ["tip", "--root", rootDir] -> do
      cs <- loadLog rootDir
      case chooseTip cs of
        Nothing -> putStrLn "tip: none"
        Just c -> putStrLn ("tip: " ++ show (cid c))

    ["replay", "--root", rootDir] -> do
      cs <- loadLog rootDir
      putStrLn ("replayed commits: " ++ show (length cs))

    ["fingerprint", "--root", rootDir] -> do
      opts <- defaultOptionsIO rootDir
      rt <- initRuntime opts
      runFingerprint rt (logPath rootDir)

    ["fingerprint", "--root", rootDir, "--log", fp] -> do
      opts <- defaultOptionsIO rootDir
      rt <- initRuntime opts
      runFingerprint rt fp

    _ -> do
      putStrLn "Usage:"
      putStrLn "  ulp-runtime init --root <dir>"
      putStrLn "  ulp-runtime commit --root <dir> --type commit"
      putStrLn "  ulp-runtime validate --root <dir>"
      putStrLn "  ulp-runtime merge --root <dir> --from <remote.ndjson>"
      putStrLn "  ulp-runtime tip --root <dir>"
      putStrLn "  ulp-runtime replay --root <dir>"
      putStrLn "  ulp-runtime fingerprint --root <dir> [--log <path>]"
