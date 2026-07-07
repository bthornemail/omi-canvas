{-# LANGUAGE OverloadedStrings #-}

module ULP.Storage
  ( ensureRoot
  , logPath
  , loadLog
  , appendCommit
  , writeLog
  ) where

import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath ((</>))
import qualified Data.ByteString.Lazy.Char8 as LBS
import ULP.NDJSON
import ULP.Types

ensureRoot :: FilePath -> IO ()
ensureRoot rootDir = createDirectoryIfMissing True rootDir

logPath :: FilePath -> FilePath
logPath rootDir = rootDir </> "log.ndjson"

loadLog :: FilePath -> IO [CommitEvent]
loadLog rootDir = do
  let fp = logPath rootDir
  exists <- doesFileExist fp
  if exists then decodeFile fp else pure []

appendCommit :: FilePath -> CommitEvent -> IO ()
appendCommit rootDir c = appendLine (logPath rootDir) c

writeLog :: FilePath -> [CommitEvent] -> IO ()
writeLog rootDir commits = writeFileBytes (logPath rootDir) (encodeAll commits)
  where
    writeFileBytes fp content = do
      ensureRoot rootDir
      -- ByteString lazy write for deterministic full-log replace after merge.
      LBS.writeFile fp content
