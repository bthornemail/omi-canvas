{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module MnemonicManifold.Spec
  ( Versions(..)
  , Triple(..)
  , Point(..)
  , Line(..)
  , allPoints
  , allLines
  , pointBitsText
  , hashS
  , hashP
  , hashO
  , pointValue
  , lineInvariantHolds
  , closureSatisfiedLines
  , closureTotalLines
  , sabbath
  , stopUnsatisfiedLines
  ) where

import Data.Bits (xor, testBit)
import Data.Word (Word64)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as BS
import MnemonicManifold.SHA256 (sha256U64BE)

data Versions = Versions
  { lexiconVersion :: Text
  , parserVersion :: Text
  } deriving (Eq, Show)

data Triple = Triple
  { tSubject :: Text
  , tPredicate :: Text
  , tObject :: Text
  } deriving (Eq, Show)

newtype Point = Point { unPoint :: Word64 }
  deriving (Eq, Ord, Show)

data Line = Line
  { lineName :: Text
  , linePoints :: (Point, Point, Point)
  } deriving (Eq, Show)

allPoints :: [Point]
allPoints = map Point [4,2,1,6,5,3,7] -- "100","010","001","110","101","011","111"

pointBitsText :: Point -> Text
pointBitsText (Point n) =
  let a = if testBit n 2 then '1' else '0'
      b = if testBit n 1 then '1' else '0'
      c = if testBit n 0 then '1' else '0'
  in T.pack [a,b,c]

allLines :: [Line]
allLines =
  [ Line "L001-010-011" (Point 1, Point 2, Point 3)
  , Line "L001-100-101" (Point 1, Point 4, Point 5)
  , Line "L010-100-110" (Point 2, Point 4, Point 6)
  , Line "L011-100-111" (Point 3, Point 4, Point 7)
  , Line "L010-101-111" (Point 2, Point 5, Point 7)
  , Line "L001-110-111" (Point 1, Point 6, Point 7)
  , Line "L011-101-110" (Point 3, Point 5, Point 6)
  ]

hashDomainSeparated :: Text -> Text -> Versions -> Word64
hashDomainSeparated domain value Versions{..} =
  let bytes = TE.encodeUtf8 (domain <> "|" <> value <> "|" <> lexiconVersion <> "|" <> parserVersion)
  in sha256U64BE bytes

hashS :: Versions -> Text -> Word64
hashS v s = hashDomainSeparated "S" s v

hashP :: Versions -> Text -> Word64
hashP v p = hashDomainSeparated "P" p v

hashO :: Versions -> Text -> Word64
hashO v o = hashDomainSeparated "O" o v

pointValue :: Versions -> Triple -> Point -> Word64
pointValue v Triple{..} (Point p) =
  let a = hashS v tSubject
      b = hashO v tObject
      c = hashP v tPredicate
      va = if testBit p 2 then a else 0
      vb = if testBit p 1 then b else 0
      vc = if testBit p 0 then c else 0
  in va `xor` vb `xor` vc

lineInvariantHolds :: Versions -> Triple -> Line -> Bool
lineInvariantHolds v tr (Line _ (p,q,r)) =
  pointValue v tr p `xor` pointValue v tr q `xor` pointValue v tr r == 0

closureTotalLines :: Int
closureTotalLines = length allLines

closureSatisfiedLines :: Versions -> Triple -> Int
closureSatisfiedLines v tr = length (filter id (map (lineInvariantHolds v tr) allLines))

sabbath :: Versions -> Triple -> Bool
sabbath v tr = closureSatisfiedLines v tr == closureTotalLines

stopUnsatisfiedLines :: Versions -> Triple -> Int
stopUnsatisfiedLines v tr = closureTotalLines - closureSatisfiedLines v tr
