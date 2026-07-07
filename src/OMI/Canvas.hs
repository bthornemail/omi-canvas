{-# LANGUAGE NoImplicitPrelude #-}

module OMI.Canvas where

import OMI.Kernel
import OMI.Core

data Bool = Fls | Tru

andB :: Bool -> Bool -> Bool
andB Tru Tru = Tru
andB _ _ = Fls

-- CanvasNode is a Relation interpreted as a graph node
--   Word32[0] = node identifier
--   Word32[1] = x position
--   Word32[2] = y position
--   Word16[0..7] = relation carrier fields
newtype CanvasNode = CanvasNode Relation

-- CanvasEdge is a Relation interpreted as a directed edge
--   Word32[0] = source node ID
--   Word32[1] = target node ID
--   Word32[2] = edge weight / label
newtype CanvasEdge = CanvasEdge Relation

-- Canvas is a Relation interpreted as a graph projection
--   Word32[0] = node count
--   Word32[1] = edge count
--   Word16[0..7] = shallow graph properties
--   Word32[2..3] = reserved
newtype Canvas = Canvas Relation

-- Project a Node from a Relation
projectNode :: Relation -> CanvasNode
projectNode rel = CanvasNode rel

-- Project an Edge from a Relation
projectEdge :: Relation -> CanvasEdge
projectEdge rel = CanvasEdge rel

-- Project a Canvas from a Relation
projectCanvas :: Relation -> Canvas
projectCanvas rel = Canvas rel

-- Extract node identifier
nodeId :: CanvasNode -> Word32
nodeId (CanvasNode (Relation _ _ _ _ _ _ _ _ a _ _ _)) = a

-- Extract edge endpoints
edgeSource :: CanvasEdge -> Word32
edgeSource (CanvasEdge (Relation _ _ _ _ _ _ _ _ a _ _ _)) = a

edgeTarget :: CanvasEdge -> Word32
edgeTarget (CanvasEdge (Relation _ _ _ _ _ _ _ _ _ b _ _)) = b

-- Constructors
mkCanvasNode :: Word32 -> Relation -> CanvasNode
mkCanvasNode nid props =
  CanvasNode (setNodeId nullRel nid)

mkCanvasEdge :: Word32 -> Word32 -> CanvasEdge
mkCanvasEdge src tgt =
  CanvasEdge (setEdgeSrc (setEdgeTgt nullRel tgt) src)

setNodeId :: Relation -> Word32 -> Relation
setNodeId (Relation a b c d e f g h _ j k l) nid =
  Relation a b c d e f g h nid j k l

setEdgeSrc :: Relation -> Word32 -> Relation
setEdgeSrc (Relation a b c d e f g h _ j k l) src =
  Relation a b c d e f g h src j k l

setEdgeTgt :: Relation -> Word32 -> Relation
setEdgeTgt (Relation a b c d e f g h i _ k l) tgt =
  Relation a b c d e f g h i tgt k l
