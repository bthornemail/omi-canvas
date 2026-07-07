{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module MnemonicManifold.Brackets
  ( bracketDepth
  , stripBalancedBrackets
  ) where

import Data.Text (Text)
import qualified Data.Text as T

-- | Count maximal balanced wrapper depth where each wrapper is a single
-- leading '[' and trailing ']'.
--
-- Examples:
--   bracketDepth "X"      == 0
--   bracketDepth "[X]"    == 1
--   bracketDepth "[[X]]"  == 2
bracketDepth :: Text -> Int
bracketDepth = fst . stripBalancedBrackets

-- | Strip maximal balanced bracket wrappers and return (depth, innerText).
-- The returned innerText is the raw inner content (not trimmed).
stripBalancedBrackets :: Text -> (Int, Text)
stripBalancedBrackets = go 0
  where
    go !d t
      | T.length t >= 2 && T.head t == '[' && T.last t == ']' =
          go (d + 1) (T.dropEnd 1 (T.drop 1 t))
      | otherwise = (d, t)
