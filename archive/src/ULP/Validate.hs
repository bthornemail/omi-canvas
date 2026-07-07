{-# LANGUAGE OverloadedStrings #-}

module ULP.Validate
  ( validateCommit
  ) where

import ULP.Canonical
import ULP.Merkle
import ULP.Types

validateCommit :: ValidationOptions -> Maybe CommitEvent -> CommitEvent -> IO ValidationResult
validateCommit opts prev c = do
  let errs0 = []
      errs1 = if maybe True (>= 0) (lc c) then errs0 else "invalid:lc" : errs0
      errs2 =
        case prev of
          Nothing -> if prev_hash c == Nothing then errs1 else "invalid:genesis_prev_hash" : errs1
          Just p -> if prev_hash c == Just (self_hash p) then errs1 else "invalid:prev_hash" : errs1
      expectedHash = sha256Hex (canonicalPayload c)
      errs3 = if expectedHash == self_hash c then errs2 else "invalid:self_hash" : errs2
      errs4 = if validateCommitMerkle c then errs3 else "invalid:merkle" : errs3

  sigOk <-
    case signatureVerifier opts of
      Nothing -> pure (sig c /= "")
      Just vf -> vf c (getSigningMessage c)

  invOk <-
    case invariantChecker opts of
      Nothing -> pure True
      Just f -> f c

  let errs5 = if sigOk then errs4 else ("invalid:sig" : errs4)
      errs6 = if invOk then errs5 else ("invalid:invariants" : errs5)

  pure ValidationResult {valid = null errs6, errors = reverse errs6}
