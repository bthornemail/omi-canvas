{-# LANGUAGE NoImplicitPrelude #-}

module OMI.Canvas
  ( CanvasNode
  , CanvasEdge
  , Canvas
  , ProjectionFace
  , projectNode
  , projectEdge
  , projectCanvas
  , projectProjectionFace
  , nodeId
  , edgeSource
  , edgeTarget
  , edgeFromSlot
  , edgeToSlot
  , edgeProofPolarity
  , edgeOperator
  , projectionRelation
  , mkCanvasNode
  , mkCanvasEdge
  , mkConstitutionalEdge
  ) where

import OMI.Kernel
import OMI.Core
import OMI.Relation

-- CanvasNode is a Relation interpreted as a graph node
--   Word32[0] = OMI---IMO identity
--   Word32[1] = x position
--   Word32[2] = y position
--   Word16[0..7] = relation carrier fields
newtype CanvasNode = CanvasNode Relation

-- CanvasEdge is a Relation interpreted as a directed edge
--   Word32[0] = source node ID
--   Word32[1] = target node ID
--   Word16[0] = from citation slot
--   Word16[1] = to citation slot
--   Word16[2] = proof polarity
--   Word16[3] = Wittgenstein operator attribution
newtype CanvasEdge = CanvasEdge Relation

-- Canvas is a Relation interpreted as a graph projection
--   Word32[0] = node count
--   Word32[1] = edge count
--   Word16[0..7] = shallow graph properties
--   Word32[2..3] = reserved
newtype Canvas = Canvas Relation

-- ProjectionFace is the typed canvas-facing projection of an accepted
-- blackboard state. JSON Canvas encoding is intentionally outside this stage.
newtype ProjectionFace = ProjectionFace Relation

-- Project a Node from a Relation
projectNode :: Relation -> CanvasNode
projectNode rel = CanvasNode rel

-- Project an Edge from a Relation
projectEdge :: Relation -> CanvasEdge
projectEdge rel = CanvasEdge rel

-- Project a Canvas from a Relation
projectCanvas :: Relation -> Canvas
projectCanvas rel = Canvas rel

projectProjectionFace :: Relation -> ProjectionFace
projectProjectionFace rel = ProjectionFace rel

-- Extract node identifier
nodeId :: CanvasNode -> Word32
nodeId (CanvasNode (Relation _ _ _ _ _ _ _ _ a _ _ _)) = a

-- Extract edge endpoints
edgeSource :: CanvasEdge -> Word32
edgeSource (CanvasEdge (Relation _ _ _ _ _ _ _ _ a _ _ _)) = a

edgeTarget :: CanvasEdge -> Word32
edgeTarget (CanvasEdge (Relation _ _ _ _ _ _ _ _ _ b _ _)) = b

edgeFromSlot :: CanvasEdge -> Word16
edgeFromSlot (CanvasEdge rel) = relW16a rel

edgeToSlot :: CanvasEdge -> Word16
edgeToSlot (CanvasEdge rel) = relW16b rel

edgeProofPolarity :: CanvasEdge -> Word16
edgeProofPolarity (CanvasEdge rel) = relW16c rel

edgeOperator :: CanvasEdge -> Word16
edgeOperator (CanvasEdge rel) = relW16d rel

projectionRelation :: ProjectionFace -> Relation
projectionRelation (ProjectionFace rel) = rel

-- Constructors
mkCanvasNode :: Word32 -> Relation -> CanvasNode
mkCanvasNode nid props =
  CanvasNode (setNodeId props nid)

mkCanvasEdge :: Word32 -> Word32 -> CanvasEdge
mkCanvasEdge src tgt =
  CanvasEdge (setEdgeSrc (setEdgeTgt nullRel tgt) src)

mkConstitutionalEdge :: Word32 -> Word32 -> Word16 -> Word16 -> Word16 -> Word16 -> CanvasEdge
mkConstitutionalEdge src tgt fromSlot toSlot proof operator =
  CanvasEdge
    (setEdgeOperator
      (setEdgeProofPolarity
        (setEdgeToSlot
          (setEdgeFromSlot
            (setEdgeSrc (setEdgeTgt nullRel tgt) src)
            fromSlot)
          toSlot)
        proof)
      operator)

setNodeId :: Relation -> Word32 -> Relation
setNodeId (Relation a b c d e f g h _ j k l) nid =
  Relation a b c d e f g h nid j k l

setEdgeSrc :: Relation -> Word32 -> Relation
setEdgeSrc (Relation a b c d e f g h _ j k l) src =
  Relation a b c d e f g h src j k l

setEdgeTgt :: Relation -> Word32 -> Relation
setEdgeTgt (Relation a b c d e f g h i _ k l) tgt =
  Relation a b c d e f g h i tgt k l

setEdgeFromSlot :: Relation -> Word16 -> Relation
setEdgeFromSlot = setW16a

setEdgeToSlot :: Relation -> Word16 -> Relation
setEdgeToSlot = setW16b

setEdgeProofPolarity :: Relation -> Word16 -> Relation
setEdgeProofPolarity = setW16c

setEdgeOperator :: Relation -> Word16 -> Relation
setEdgeOperator = setW16d
