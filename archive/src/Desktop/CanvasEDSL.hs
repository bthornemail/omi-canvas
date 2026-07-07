{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Desktop.CanvasEDSL
-- JSON Canvas Spec 1.0 (https://jsoncanvas.org/spec/1.0/)
--
-- Complete EDSL for building and parsing JSON Canvas diagrams with
-- NDJSON streaming support.
module Desktop.CanvasEDSL
  ( -- * Canvas
    Canvas(..)
  , Node(..)
  , Edge(..)
  , NodeType(..)
  , Side(..)
  , EndShape(..)
  , CanvasColor(..)

    -- * IDs
  , NodeId(..)
  , EdgeId(..)

    -- * EDSL builders
  , emptyCanvas
  , canvas
  , textNode
  , fileNode
  , linkNode
  , groupNode
  , packetNode
  , withColor
  , withBackground
  , withBackgroundStyle
  , withSubpath
  , withEdgeColor
  , withEdgeLabel

    -- * Covariant/Contravariant link combinators
  , fifoTop
  , fifoBottom
  , portLeft
  , portRight
  , flow
  , bidirectional
  , edge

    -- * NDJSON (Newline Delimited JSON)
  , CanvasEvent(..)
  , encodeNDJSON
  , decodeNDJSON
  , parseNDJSON
  , eventToCanvas
  , applyEvent
  , foldEvents

    -- * Color utilities
  , presetColor
  , hexColor
  , isPresetColor
  ) where

import GHC.Generics (Generic)
import Data.Aeson (ToJSON(..), FromJSON(..), (.=), (.:), (.:?), withObject, withText)
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import Data.Aeson.Types ((.!=))
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Text (Text)
import qualified Data.Text as T
import Data.Maybe (catMaybes)
import Control.Applicative ((<|>))
import Prelude hiding (Left, Right)

-- ----------------------------
-- IDs
-- ----------------------------

newtype NodeId = NodeId { unNodeId :: Text }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON NodeId where
  toJSON (NodeId t) = A.String t

instance FromJSON NodeId where
  parseJSON = withText "NodeId" $ pure . NodeId

newtype EdgeId = EdgeId { unEdgeId :: Text }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON EdgeId where
  toJSON (EdgeId t) = A.String t

instance FromJSON EdgeId where
  parseJSON = withText "EdgeId" $ pure . EdgeId

-- ----------------------------
-- Canvas Color
-- ----------------------------

data CanvasColor
  = PresetColor Int  -- ^ 1-6 for preset colors
  | HexColor Text    -- ^ "#RRGGBB" or "#RGB" format
  deriving (Eq, Show, Generic)

instance ToJSON CanvasColor where
  toJSON (PresetColor n) = A.String (T.pack (show n))
  toJSON (HexColor h) = A.String h

instance FromJSON CanvasColor where
  parseJSON = withText "CanvasColor" $ \t ->
    if T.length t == 1 && T.head t >= '1' && T.head t <= '6'
      then pure $ PresetColor (read (T.unpack t))
      else pure $ HexColor t

presetColor :: Int -> Maybe CanvasColor
presetColor n
  | n >= 1 && n <= 6 = Just (PresetColor n)
  | otherwise = Nothing

hexColor :: Text -> Maybe CanvasColor
hexColor t
  | T.length t `elem` [4,7] && T.head t == '#' = Just (HexColor t)
  | otherwise = Nothing

isPresetColor :: CanvasColor -> Bool
isPresetColor (PresetColor _) = True
isPresetColor _ = False

-- ----------------------------
-- JSON Canvas: Canvas
-- ----------------------------

data Canvas = Canvas
  { nodes :: [Node]
  , edges :: [Edge]
  } deriving (Eq, Show, Generic)

emptyCanvas :: Canvas
emptyCanvas = Canvas [] []

instance ToJSON Canvas where
  toJSON Canvas{..} = A.object $ catMaybes
    [ if null nodes then Nothing else Just ("nodes" .= nodes)
    , if null edges then Nothing else Just ("edges" .= edges)
    ]

instance FromJSON Canvas where
  parseJSON = withObject "Canvas" $ \v -> Canvas
    <$> v .:? "nodes" .!= []
    <*> v .:? "edges" .!= []

-- ----------------------------
-- JSON Canvas: Nodes
-- ----------------------------

data NodeType
  = NText
  | NFile
  | NLink
  | NGroup
  deriving (Eq, Show, Generic)

instance ToJSON NodeType where
  toJSON = \case
    NText  -> A.String "text"
    NFile  -> A.String "file"
    NLink  -> A.String "link"
    NGroup -> A.String "group"

instance FromJSON NodeType where
  parseJSON = withText "NodeType" $ \case
    "text"  -> pure NText
    "file"  -> pure NFile
    "link"  -> pure NLink
    "group" -> pure NGroup
    t       -> fail $ "Invalid node type: " ++ T.unpack t

data Node = Node
  { nodeId        :: NodeId
  , nodeType      :: NodeType
  , x             :: Int
  , y             :: Int
  , width         :: Int
  , height        :: Int
  , color         :: Maybe CanvasColor
  -- type-specific payload
  , nodeText      :: Maybe Text      -- for text nodes
  , nodeFile      :: Maybe Text      -- for file nodes
  , nodeSubpath   :: Maybe Text      -- for file nodes
  , nodeUrl       :: Maybe Text      -- for link nodes
  , nodeLabel     :: Maybe Text      -- for group nodes
  , nodeBackground :: Maybe Text     -- for group nodes
  , nodeBackgroundStyle :: Maybe Text -- for group nodes
  } deriving (Eq, Show, Generic)

instance ToJSON Node where
  toJSON Node{..} =
    A.object $ base ++ typeSpecific
    where
      base =
        [ "id"     .= nodeId
        , "type"   .= nodeType
        , "x"      .= x
        , "y"      .= y
        , "width"  .= width
        , "height" .= height
        ] ++ maybe [] (\c -> ["color" .= c]) color

      typeSpecific = case nodeType of
        NText  -> maybe [] (\t -> ["text" .= t]) nodeText
        NFile  -> catMaybes
          [ ("file" .=) <$> nodeFile
          , ("subpath" .=) <$> nodeSubpath
          ]
        NLink  -> maybe [] (\u -> ["url" .= u]) nodeUrl
        NGroup -> catMaybes
          [ ("label" .=) <$> nodeLabel
          , ("background" .=) <$> nodeBackground
          , ("backgroundStyle" .=) <$> nodeBackgroundStyle
          ]

instance FromJSON Node where
  parseJSON = withObject "Node" $ \v -> do
    nodeId <- v .: "id"
    nodeType <- v .: "type"
    x <- v .: "x"
    y <- v .: "y"
    width <- v .: "width"
    height <- v .: "height"
    color <- v .:? "color"
    
    let parseText = Node nodeId nodeType x y width height color
          <$> v .:? "text"
          <*> pure Nothing
          <*> pure Nothing
          <*> pure Nothing
          <*> pure Nothing
          <*> pure Nothing
          <*> pure Nothing
        parseFile = Node nodeId nodeType x y width height color
          <$> pure Nothing
          <*> v .:? "file"
          <*> v .:? "subpath"
          <*> pure Nothing
          <*> pure Nothing
          <*> pure Nothing
          <*> pure Nothing
        parseLink = Node nodeId nodeType x y width height color
          <$> pure Nothing
          <*> pure Nothing
          <*> pure Nothing
          <*> v .:? "url"
          <*> pure Nothing
          <*> pure Nothing
          <*> pure Nothing
        parseGroup = Node nodeId nodeType x y width height color
          <$> pure Nothing
          <*> pure Nothing
          <*> pure Nothing
          <*> pure Nothing
          <*> v .:? "label"
          <*> v .:? "background"
          <*> v .:? "backgroundStyle"
    
    case nodeType of
      NText  -> parseText
      NFile  -> parseFile
      NLink  -> parseLink
      NGroup -> parseGroup

-- ----------------------------
-- JSON Canvas: Edges
-- ----------------------------

data Side = Top | Right | Bottom | Left
  deriving (Eq, Show, Generic)

instance ToJSON Side where
  toJSON = \case
    Top    -> A.String "top"
    Right  -> A.String "right"
    Bottom -> A.String "bottom"
    Left   -> A.String "left"

instance FromJSON Side where
  parseJSON = withText "Side" $ \case
    "top"    -> pure Top
    "right"  -> pure Right
    "bottom" -> pure Bottom
    "left"   -> pure Left
    t        -> fail $ "Invalid side: " ++ T.unpack t
    "bottom" -> pure Bottom
    "left"   -> pure Left
    t        -> fail $ "Invalid side: " ++ T.unpack t

data EndShape = EndNone | EndArrow
  deriving (Eq, Show, Generic)

instance ToJSON EndShape where
  toJSON = \case
    EndNone  -> A.String "none"
    EndArrow -> A.String "arrow"

instance FromJSON EndShape where
  parseJSON = withText "EndShape" $ \case
    "none"  -> pure EndNone
    "arrow" -> pure EndArrow
    t       -> fail $ "Invalid end shape: " ++ T.unpack t

data Edge = Edge
  { edgeId      :: EdgeId
  , fromNode    :: NodeId
  , fromSide    :: Maybe Side
  , fromEnd     :: Maybe EndShape
  , toNode      :: NodeId
  , toSide      :: Maybe Side
  , toEnd       :: Maybe EndShape
  , edgeColor   :: Maybe CanvasColor
  , edgeLabel   :: Maybe Text
  } deriving (Eq, Show, Generic)

instance ToJSON Edge where
  toJSON Edge{..} =
    A.object $ catMaybes
      [ Just ("id" .= edgeId)
      , Just ("fromNode" .= fromNode)
      , ("fromSide" .=) <$> fromSide
      , ("fromEnd" .=) <$> fromEnd
      , Just ("toNode" .= toNode)
      , ("toSide" .=) <$> toSide
      , ("toEnd" .=) <$> toEnd
      , ("color" .=) <$> edgeColor
      , ("label" .=) <$> edgeLabel
      ]

instance FromJSON Edge where
  parseJSON = withObject "Edge" $ \v -> Edge
    <$> v .: "id"
    <*> v .: "fromNode"
    <*> v .:? "fromSide"
    <*> v .:? "fromEnd"
    <*> v .: "toNode"
    <*> v .:? "toSide"
    <*> v .:? "toEnd"
    <*> v .:? "color"
    <*> v .:? "label"

-- ----------------------------
-- EDSL: Node Builders
-- ----------------------------

canvas :: [Node] -> [Edge] -> Canvas
canvas = Canvas

textNode :: NodeId -> (Int,Int) -> (Int,Int) -> Text -> Node
textNode nid (px,py) (w,h) t = Node
  { nodeId = nid
  , nodeType = NText
  , x = px
  , y = py
  , width = w
  , height = h
  , color = Nothing
  , nodeText = Just t
  , nodeFile = Nothing
  , nodeSubpath = Nothing
  , nodeUrl = Nothing
  , nodeLabel = Nothing
  , nodeBackground = Nothing
  , nodeBackgroundStyle = Nothing
  }

fileNode :: NodeId -> (Int,Int) -> (Int,Int) -> Text -> Node
fileNode nid (px,py) (w,h) f = Node
  { nodeId = nid
  , nodeType = NFile
  , x = px
  , y = py
  , width = w
  , height = h
  , color = Nothing
  , nodeText = Nothing
  , nodeFile = Just f
  , nodeSubpath = Nothing
  , nodeUrl = Nothing
  , nodeLabel = Nothing
  , nodeBackground = Nothing
  , nodeBackgroundStyle = Nothing
  }

linkNode :: NodeId -> (Int,Int) -> (Int,Int) -> Text -> Node
linkNode nid (px,py) (w,h) u = Node
  { nodeId = nid
  , nodeType = NLink
  , x = px
  , y = py
  , width = w
  , height = h
  , color = Nothing
  , nodeText = Nothing
  , nodeFile = Nothing
  , nodeSubpath = Nothing
  , nodeUrl = Just u
  , nodeLabel = Nothing
  , nodeBackground = Nothing
  , nodeBackgroundStyle = Nothing
  }

groupNode :: NodeId -> (Int,Int) -> (Int,Int) -> Text -> Node
groupNode nid (px,py) (w,h) lab = Node
  { nodeId = nid
  , nodeType = NGroup
  , x = px
  , y = py
  , width = w
  , height = h
  , color = Nothing
  , nodeText = Nothing
  , nodeFile = Nothing
  , nodeSubpath = Nothing
  , nodeUrl = Nothing
  , nodeLabel = Just lab
  , nodeBackground = Nothing
  , nodeBackgroundStyle = Nothing
  }

-- | Packet node for messages (convenience wrapper around textNode)
packetNode :: NodeId -> (Int,Int) -> (Int,Int) -> Text -> Text -> Node
packetNode nid pos size pktType payload =
  textNode nid pos size (pktType <> "\n\n" <> payload)

-- | Add color to a node
withColor :: Maybe CanvasColor -> Node -> Node
withColor c n = n { color = c }

-- | Add background to a group node
withBackground :: Maybe Text -> Node -> Node
withBackground bg n = n { nodeBackground = bg }

-- | Add background style to a group node
withBackgroundStyle :: Maybe Text -> Node -> Node
withBackgroundStyle bs n = n { nodeBackgroundStyle = bs }

-- | Add subpath to a file node
withSubpath :: Maybe Text -> Node -> Node
withSubpath sp n = n { nodeSubpath = sp }

-- ----------------------------
-- EDSL: Edge Builders
-- ----------------------------

edge :: EdgeId -> NodeId -> NodeId -> Edge
edge eid a b = Edge
  { edgeId = eid
  , fromNode = a
  , fromSide = Nothing
  , fromEnd = Nothing
  , toNode = b
  , toSide = Nothing
  , toEnd = Nothing
  , edgeColor = Nothing
  , edgeLabel = Nothing
  }

flow :: EdgeId -> NodeId -> Side -> NodeId -> Side -> Edge
flow eid a aSide b bSide = Edge
  { edgeId = eid
  , fromNode = a
  , fromSide = Just aSide
  , fromEnd = Just EndNone
  , toNode = b
  , toSide = Just bSide
  , toEnd = Just EndArrow
  , edgeColor = Nothing
  , edgeLabel = Nothing
  }

bidirectional :: EdgeId -> NodeId -> Side -> NodeId -> Side -> Edge
bidirectional eid a aSide b bSide = Edge
  { edgeId = eid
  , fromNode = a
  , fromSide = Just aSide
  , fromEnd = Just EndArrow
  , toNode = b
  , toSide = Just bSide
  , toEnd = Just EndArrow
  , edgeColor = Nothing
  , edgeLabel = Nothing
  }

fifoTop :: EdgeId -> NodeId -> NodeId -> Edge
fifoTop eid from to = flow eid from Top to Bottom

fifoBottom :: EdgeId -> NodeId -> NodeId -> Edge
fifoBottom eid from to = flow eid from Bottom to Top

portLeft :: EdgeId -> NodeId -> NodeId -> Edge
portLeft eid from to = flow eid from Left to Right

portRight :: EdgeId -> NodeId -> NodeId -> Edge
portRight eid from to = flow eid from Right to Left

withEdgeColor :: Maybe CanvasColor -> Edge -> Edge
withEdgeColor c e = e { edgeColor = c }

withEdgeLabel :: Maybe Text -> Edge -> Edge
withEdgeLabel l e = e { edgeLabel = l }

-- ----------------------------
-- NDJSON streaming (events)
-- ----------------------------

-- | NDJSON event stream for incremental updates
data CanvasEvent
  = EvAddNode Node
  | EvUpdateNode Node  -- ^ Replace existing node
  | EvRemoveNode NodeId
  | EvAddEdge Edge
  | EvUpdateEdge Edge  -- ^ Replace existing edge
  | EvRemoveEdge EdgeId
  | EvSnapshot Canvas
  deriving (Eq, Show, Generic)

instance ToJSON CanvasEvent where
  toJSON = \case
    EvAddNode n -> A.object
      [ "schema" .= ("ulp.canvas.event.v0.1" :: Text)
      , "op"     .= ("addNode" :: Text)
      , "node"   .= n
      ]
    EvUpdateNode n -> A.object
      [ "schema" .= ("ulp.canvas.event.v0.1" :: Text)
      , "op"     .= ("updateNode" :: Text)
      , "node"   .= n
      ]
    EvRemoveNode nid -> A.object
      [ "schema" .= ("ulp.canvas.event.v0.1" :: Text)
      , "op"     .= ("removeNode" :: Text)
      , "nodeId" .= nid
      ]
    EvAddEdge e -> A.object
      [ "schema" .= ("ulp.canvas.event.v0.1" :: Text)
      , "op"     .= ("addEdge" :: Text)
      , "edge"   .= e
      ]
    EvUpdateEdge e -> A.object
      [ "schema" .= ("ulp.canvas.event.v0.1" :: Text)
      , "op"     .= ("updateEdge" :: Text)
      , "edge"   .= e
      ]
    EvRemoveEdge eid -> A.object
      [ "schema" .= ("ulp.canvas.event.v0.1" :: Text)
      , "op"     .= ("removeEdge" :: Text)
      , "edgeId" .= eid
      ]
    EvSnapshot c -> A.object
      [ "schema" .= ("ulp.canvas.event.v0.1" :: Text)
      , "op"     .= ("snapshot" :: Text)
      , "canvas" .= c
      ]

instance FromJSON CanvasEvent where
  parseJSON = withObject "CanvasEvent" $ \v -> do
    op <- v .: "op" :: A.Parser Text
    case op of
      "addNode"    -> EvAddNode <$> v .: "node"
      "updateNode" -> EvUpdateNode <$> v .: "node"
      "removeNode" -> EvRemoveNode <$> v .: "nodeId"
      "addEdge"    -> EvAddEdge <$> v .: "edge"
      "updateEdge" -> EvUpdateEdge <$> v .: "edge"
      "removeEdge" -> EvRemoveEdge <$> v .: "edgeId"
      "snapshot"   -> EvSnapshot <$> v .: "canvas"
      _            -> fail $ "Unknown operation: " ++ T.unpack op

-- | Encode events as NDJSON (one JSON object per line)
encodeNDJSON :: [CanvasEvent] -> BL.ByteString
encodeNDJSON = BL.unlines . map A.encode

-- | Parse NDJSON into a list of events
decodeNDJSON :: BL.ByteString -> Either String [CanvasEvent]
decodeNDJSON = mapM A.eitherDecode . BL.lines

-- | Parse NDJSON, ignoring invalid lines
parseNDJSON :: BL.ByteString -> [CanvasEvent]
parseNDJSON = mapMaybe (A.decode . BL.fromStrict . BL.toStrict) . BL.lines
  where
    mapMaybe _ [] = []
    mapMaybe f (x:xs) = case f x of
      Just v  -> v : mapMaybe f xs
      Nothing -> mapMaybe f xs

-- | Apply a single event to a canvas
applyEvent :: Canvas -> CanvasEvent -> Canvas
applyEvent canvas@Canvas{..} = \case
  EvAddNode n -> canvas { nodes = nodes ++ [n] }
  EvUpdateNode n -> canvas { nodes = map (\node -> if nodeId node == nodeId n then n else node) nodes }
  EvRemoveNode nid -> canvas { nodes = filter ((/= nid) . nodeId) nodes }
  EvAddEdge e -> canvas { edges = edges ++ [e] }
  EvUpdateEdge e -> canvas { edges = map (\edge -> if edgeId edge == edgeId e then e else edge) edges }
  EvRemoveEdge eid -> canvas { edges = filter ((/= eid) . edgeId) edges }
  EvSnapshot c -> c

-- | Fold a list of events into a canvas
foldEvents :: Canvas -> [CanvasEvent] -> Canvas
foldEvents = foldl applyEvent

-- | Convert an event list to a canvas (starting from empty)
eventToCanvas :: [CanvasEvent] -> Canvas
eventToCanvas = foldEvents emptyCanvas