module OMI.Canvas.JSON
  ( JsonCanvas
  , JsonCanvasNode
  , JsonCanvasEdge
  , projectionFaceToJsonCanvas
  , attestationToJsonCanvas
  , encodeJsonCanvas
  , jsonCanvasNodes
  , jsonCanvasEdges
  , jsonNodeId
  , jsonNodeText
  , jsonNodeColor
  , jsonEdgeFromNode
  , jsonEdgeToNode
  , jsonEdgeFromSide
  , jsonEdgeToSide
  , jsonEdgeToEnd
  , jsonEdgeLabel
  ) where

import Prelude

import OMI.Canvas
import OMI.Kernel
import OMI.Pipeline
import OMI.Relation

data JsonCanvas = JsonCanvas [JsonCanvasNode] [JsonCanvasEdge]

data JsonCanvasNode = JsonCanvasNode String String String

data JsonCanvasEdge = JsonCanvasEdge String String String String String String

projectionFaceToJsonCanvas :: ProjectionFace -> JsonCanvas
projectionFaceToJsonCanvas face =
  let rel = projectionRelation face
      identity = word32Hex (relW32a rel)
      color = laneColor (relW16a rel)
      node = JsonCanvasNode identity ("o---o/---/?---?@---@ " ++ relationLabel rel) color
      edge = JsonCanvasEdge identity identity
               (slotSide (relW16a rel))
               (slotSide (relW16b rel))
               "arrow"
               ("operator:" ++ word16Hex (relW16d rel))
  in JsonCanvas [node] [edge]

attestationToJsonCanvas :: Attestation -> JsonCanvas
attestationToJsonCanvas att =
  projectionFaceToJsonCanvas (projectProjectionFace (attestationRelation att))

encodeJsonCanvas :: JsonCanvas -> String
encodeJsonCanvas canvas =
  "{\n  \"nodes\": ["
  ++ joinWith "," (map encodeNode (jsonCanvasNodes canvas))
  ++ "],\n  \"edges\": ["
  ++ joinWith "," (map encodeEdge (jsonCanvasEdges canvas))
  ++ "]\n}\n"

jsonCanvasNodes :: JsonCanvas -> [JsonCanvasNode]
jsonCanvasNodes (JsonCanvas nodes _) = nodes

jsonCanvasEdges :: JsonCanvas -> [JsonCanvasEdge]
jsonCanvasEdges (JsonCanvas _ edges) = edges

jsonNodeId :: JsonCanvasNode -> String
jsonNodeId (JsonCanvasNode nodeId _ _) = nodeId

jsonNodeText :: JsonCanvasNode -> String
jsonNodeText (JsonCanvasNode _ text _) = text

jsonNodeColor :: JsonCanvasNode -> String
jsonNodeColor (JsonCanvasNode _ _ color) = color

jsonEdgeFromNode :: JsonCanvasEdge -> String
jsonEdgeFromNode (JsonCanvasEdge fromNode _ _ _ _ _) = fromNode

jsonEdgeToNode :: JsonCanvasEdge -> String
jsonEdgeToNode (JsonCanvasEdge _ toNode _ _ _ _) = toNode

jsonEdgeFromSide :: JsonCanvasEdge -> String
jsonEdgeFromSide (JsonCanvasEdge _ _ fromSide _ _ _) = fromSide

jsonEdgeToSide :: JsonCanvasEdge -> String
jsonEdgeToSide (JsonCanvasEdge _ _ _ toSide _ _) = toSide

jsonEdgeToEnd :: JsonCanvasEdge -> String
jsonEdgeToEnd (JsonCanvasEdge _ _ _ _ toEnd _) = toEnd

jsonEdgeLabel :: JsonCanvasEdge -> String
jsonEdgeLabel (JsonCanvasEdge _ _ _ _ _ label) = label

encodeNode :: JsonCanvasNode -> String
encodeNode node =
  "\n    {\"id\":\"" ++ esc (jsonNodeId node)
  ++ "\",\"type\":\"text\",\"text\":\"" ++ esc (jsonNodeText node)
  ++ "\",\"color\":\"" ++ esc (jsonNodeColor node)
  ++ "\"}"

encodeEdge :: JsonCanvasEdge -> String
encodeEdge edge =
  "\n    {\"fromNode\":\"" ++ esc (jsonEdgeFromNode edge)
  ++ "\",\"toNode\":\"" ++ esc (jsonEdgeToNode edge)
  ++ "\",\"fromSide\":\"" ++ esc (jsonEdgeFromSide edge)
  ++ "\",\"toSide\":\"" ++ esc (jsonEdgeToSide edge)
  ++ "\",\"toEnd\":\"" ++ esc (jsonEdgeToEnd edge)
  ++ "\",\"label\":\"" ++ esc (jsonEdgeLabel edge)
  ++ "\"}"

relationLabel :: Relation -> String
relationLabel rel =
  "identity=" ++ word32Hex (relW32a rel)
  ++ " slot=" ++ word16Hex (relW16a rel)

laneColor :: Word16 -> String
laneColor word =
  case last (word16Hex word) of
    '0' -> "1"
    '1' -> "2"
    '2' -> "3"
    '3' -> "4"
    '4' -> "5"
    '5' -> "6"
    _ -> "1"

slotSide :: Word16 -> String
slotSide word =
  case last (word16Hex word) of
    '0' -> "left"
    '1' -> "right"
    '2' -> "top"
    '3' -> "bottom"
    _ -> "right"

word32Hex :: Word32 -> String
word32Hex (W32 a b) = word16Hex a ++ word16Hex b

word16Hex :: Word16 -> String
word16Hex (W16 a b) = byteHex a ++ byteHex b

byteHex :: Byte -> String
byteHex (B hi lo) = [nibbleHex hi, nibbleHex lo]

nibbleHex :: Nibble -> Char
nibbleHex (N a b c d) = hexDigit (bitValue a * 8 + bitValue b * 4 + bitValue c * 2 + bitValue d)

bitValue :: Bit -> Int
bitValue O = 0
bitValue I = 1

hexDigit :: Int -> Char
hexDigit 0 = '0'
hexDigit 1 = '1'
hexDigit 2 = '2'
hexDigit 3 = '3'
hexDigit 4 = '4'
hexDigit 5 = '5'
hexDigit 6 = '6'
hexDigit 7 = '7'
hexDigit 8 = '8'
hexDigit 9 = '9'
hexDigit 10 = 'a'
hexDigit 11 = 'b'
hexDigit 12 = 'c'
hexDigit 13 = 'd'
hexDigit 14 = 'e'
hexDigit _ = 'f'

joinWith :: String -> [String] -> String
joinWith _ [] = ""
joinWith _ [x] = x
joinWith sep (x:xs) = x ++ sep ++ joinWith sep xs

esc :: String -> String
esc [] = []
esc ('"':xs) = '\\' : '"' : esc xs
esc ('\\':xs) = '\\' : '\\' : esc xs
esc ('\n':xs) = '\\' : 'n' : esc xs
esc (x:xs) = x : esc xs
