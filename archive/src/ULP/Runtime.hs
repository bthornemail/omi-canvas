{-# LANGUAGE OverloadedStrings #-}

module ULP.Runtime
  ( Runtime(..)
  , initRuntime
  , appendRuntimeCommit
  , validateLog
  ) where

import qualified Data.Text as T
import ULP.Canonical
import ULP.Merkle
import ULP.NDJSON (decodeFile)
import ULP.Storage
import ULP.Types
import ULP.Validate

newtype Runtime = Runtime
  { options :: RuntimeOptions
  }

initRuntime :: RuntimeOptions -> IO Runtime
initRuntime opts = do
  ensureRoot (storageRoot opts)
  pure (Runtime opts)

mkDefaultCentroid :: CentroidState
mkDefaultCentroid = CentroidState {stop_metric = 0, closure_ratio = 0, sabbath = False, reason = "init"}

appendRuntimeCommit :: Runtime -> CommitType -> IO CommitEvent
appendRuntimeCommit rt ty = do
  let opts = options rt
      rootDir = storageRoot opts
  prior <- loadLog rootDir
  ts <- clock opts
  let lcNext = case reverse prior of
        [] -> counterStart opts + 1
        p : _ -> maybe (counterStart opts + 1) (+ 1) (lc p)
      prev = case reverse prior of
        [] -> Nothing
        p : _ -> Just (self_hash p)
      base =
        CommitEvent
          { cid = T.pack ("cmt-" ++ show lcNext ++ "-" ++ show ts)
          , t = ts
          , lc = Just lcNext
          , ctype = ty
          , parents = maybe [] (\p -> [cid p]) (if null prior then Nothing else Just (last prior))
          , identities = Nothing
          , vertex = Nothing
          , edges = []
          , faces = []
          , centroid = mkDefaultCentroid
          , cstatus = Pending
          , prev_hash = prev
          , merkle = Nothing
          , self_hash = ""
          , sig = "unsigned-local"
          }
      withMerkle = base {merkle = Just (computeCommitMerkle base)}
      hashed = withMerkle {self_hash = sha256Hex (canonicalPayload withMerkle)}

  signed <-
    case signer opts of
      Nothing -> pure hashed
      Just s -> do
        sigText <- s hashed (getSigningMessage hashed)
        pure hashed {sig = sigText}

  appendCommit rootDir signed
  pure signed

validateLog :: Runtime -> FilePath -> IO [ValidationResult]
validateLog rt fp = do
  commits <- decodeFile fp
  let opts = ValidationOptions {signatureVerifier = verifier (options rt), invariantChecker = Nothing}
  go opts Nothing commits
  where
    go _ _ [] = pure []
    go opts prev (c : cs) = do
      v <- validateCommit opts prev c
      rest <- go opts (Just c) cs
      pure (v : rest)
