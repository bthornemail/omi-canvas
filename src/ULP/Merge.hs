{-# LANGUAGE OverloadedStrings #-}

module ULP.Merge
  ( mergeCommits
  , chooseTip
  ) where

import Data.Function (on)
import Data.List (maximumBy, nubBy, sortBy)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import ULP.Types

commitKey :: CommitEvent -> Text
commitKey = self_hash

dedupeByHash :: [CommitEvent] -> [CommitEvent]
dedupeByHash = nubBy ((==) `on` commitKey)

lcOf :: CommitEvent -> Int
lcOf c = maybe (-1) id (lc c)

parentRanks :: [CommitEvent] -> M.Map Text Int
parentRanks commits = snd (foldl go (M.empty, M.empty) commits)
  where
    byId = M.fromList [(cid c, c) | c <- commits]
    go (memo, ranks) c =
      let (memo', r) = rankFor memo S.empty c
       in (memo', M.insert (cid c) r ranks)
    rankFor memo visiting c =
      let k = cid c
       in case M.lookup k memo of
            Just r -> (memo, r)
            Nothing ->
              if S.member k visiting
                then (M.insert k 0 memo, 0)
                else
                  let visiting' = S.insert k visiting
                      pids = parents c
                      step (m, acc) pid =
                        case M.lookup pid byId of
                          Nothing -> (m, 0 : acc)
                          Just pc ->
                            let (m', pr) = rankFor m visiting' pc
                             in (m', pr : acc)
                      (memo', prs) = foldl step (memo, []) pids
                      r = if null prs then 0 else 1 + maximum prs
                   in (M.insert k r memo', r)

compareCommit :: M.Map Text Int -> CommitEvent -> CommitEvent -> Ordering
compareCommit ranks a b =
  compare (M.findWithDefault 0 (cid a) ranks) (M.findWithDefault 0 (cid b) ranks)
    <> compare (lcOf a) (lcOf b)
    <> compare (t a) (t b)
    <> compare (cid a) (cid b)

mergeCommits :: [CommitEvent] -> [CommitEvent] -> [CommitEvent]
mergeCommits local remote =
  let merged = dedupeByHash (local ++ remote)
      ranks = parentRanks merged
   in sortBy (compareCommit ranks) merged

chooseTip :: [CommitEvent] -> Maybe CommitEvent
chooseTip [] = Nothing
chooseTip commits =
  let sealed = filter (== Sealed) (map cstatus commits)
      pool = if null sealed then commits else filter ((== Sealed) . cstatus) commits
   in Just $ maximumBy compareTip pool

compareTip :: CommitEvent -> CommitEvent -> Ordering
compareTip a b =
  compare (lcOf a) (lcOf b)
    <> compare (t a) (t b)
    <> compare (T.unpack (self_hash b)) (T.unpack (self_hash a))
