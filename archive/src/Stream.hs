-- Semantic Basis Protocol — Haskell eDSL stubs
-- These types describe the same record schema as the JS stream layer.
-- Use Data.Aeson for JSON, Conduit/Pipes for streaming.
--
-- The key property: every transformation is a pure function over
-- the stream. No side effects except the final Atomics.store equivalent.

{-# LANGUAGE OverloadedStrings #-}

module SemanticBasis.Stream where

import Data.Aeson
import Data.Text (Text)
import Data.Map  (Map)

-- The atomic transaction boundary is the newline.
-- Each record is a complete, self-describing JSON object.

data Face = Agency | Ethics | Logic
  deriving (Show, Eq, Ord)

data Coords = Coords
  { coordX :: Double  -- Intent / Subject closure magnitude
  , coordY :: Double  -- Event / Predicate closure magnitude
  , coordZ :: Double  -- Incidence / Object closure magnitude
  , coordW :: Double  -- WordNet depth coefficient (sharding key)
  } deriving (Show, Eq)

data RecordType
  = Header
  | Node
  | Edge
  | Group
  | Meta
  | Flush
  deriving (Show, Eq)

data BasisDeclaration = BasisDeclaration
  { basisHash        :: Text
  , wordnetHash      :: Text
  , corpusMerkleRoot :: Text
  , protocolVersion  :: Text
  , goldenTwelve     :: [Text]
  } deriving (Show)

-- Every record in the stream carries its basis reference.
-- Records with mismatched basisHash are in a different semantic space.
data StreamRecord = StreamRecord
  { recordType  :: RecordType
  , recordId    :: Text
  , basisRef    :: Text        -- must match BasisDeclaration.basisHash
  , face        :: Maybe Face
  , coords      :: Maybe Coords
  , gridPos     :: (Int, Int)  -- (row, col) in 16x16 SAB
  , sabIndex    :: Int         -- gridPos[0] * 16 + gridPos[1]
  , canvasAttrs :: Map Text Value
  } deriving (Show)

-- The stream is a list of atomic records.
-- Consume until Flush.
type NDJSONStream = [StreamRecord]

-- Pure transformation: NDJSON record → JSON Canvas node or edge
-- This is the eDSL kernel. Compile to JS/WASM/AWK from here.
toCanvasNode :: StreamRecord -> Maybe Value
toCanvasNode r = case recordType r of
  Node  -> Just $ object [ "id" .= recordId r, "type" .= ("text" :: Text) ]
  Edge  -> Just $ object [ "id" .= recordId r, "type" .= ("edge" :: Text) ]
  Group -> Just $ object [ "id" .= recordId r, "type" .= ("group" :: Text) ]
  _     -> Nothing

-- Validate basis: all records in a stream must share one basisHash.
-- Returns Left for mismatch, Right for valid.
validateBasis :: Text -> StreamRecord -> Either Text StreamRecord
validateBasis expected r
  | basisRef r == expected = Right r
  | otherwise = Left $
      "BASIS MISMATCH: expected " <> expected <> " got " <> basisRef r

-- Atomics.store equivalent: write record to SAB position.
-- In Haskell this would use STM or IORef; here it is the type signature.
atomicStore :: Int -> StreamRecord -> IO ()
atomicStore index record = pure ()
  -- In practice: writeIORef sabArray[index] record
  -- or: atomically $ writeTVar sabTVar[index] record
