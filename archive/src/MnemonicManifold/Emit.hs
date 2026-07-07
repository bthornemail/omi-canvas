{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module MnemonicManifold.Emit
  ( BuildRootInfo(..)
  , EmitOptions(..)
  , emitStaticFanoEvents
  , emitClauseEvents
  ) where

import Desktop.CanvasEDSL
import Data.Text (Text)
import qualified Data.Text as T
import Data.Bits (xor)
import Data.Word (Word64)
import qualified Data.Text.Encoding as TE
import Data.Maybe (catMaybes)

import MnemonicManifold.Canon (CanonTriple(..), Evidence(..))
import MnemonicManifold.Spec
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
  , sabbath
  , closureSatisfiedLines
  , closureTotalLines
  , stopUnsatisfiedLines
  )
import MnemonicManifold.JsonText
import MnemonicManifold.Ids (shortHashHex16)
import MnemonicManifold.SHA256 (sha256U64BE)
import MnemonicManifold.Brackets (stripBalancedBrackets)

data EmitOptions = EmitOptions
  { eoEmitStatic :: Bool
  , eoCentroid :: Bool
  , eoBuildRoot :: Maybe BuildRootInfo
  } deriving (Eq, Show)

data BuildRootInfo = BuildRootInfo
  { brSha256Hex :: Text
  , brManifestPath :: Maybe Text
  } deriving (Eq, Show)

emitStaticFanoEvents :: [CanvasEvent]
emitStaticFanoEvents =
  map (EvAddNode . pointNode) (zip allPoints [0..]) <>
  map (EvAddNode . lineNode) (zip allLines [0..]) <>
  [ EvAddNode orderFeatureNode
  , EvAddNode (refFeatureNode refSubjectNodeId "ref.subject" 300)
  , EvAddNode (refFeatureNode refObjectNodeId "ref.object" 380)
  , EvAddNode (refFeatureNode refPredicateNodeId "ref.predicate" 460)
  , EvAddNode (refFeatureNode groupOrderNodeId "group_order" 540)
  , EvAddNode (refFeatureNode buildRootNodeId "build.root" 620)
  ]
  where
    pointNode (p, i) =
      let nid = pointNodeId p
          pos = (100 + i * 220, 40)
          size = (200, 60)
          payload = jsonObj
            [ ("kind", jsonText "mnemonic.fano.point")
            , ("bits", jsonText (pointBitsText p))
            ]
      in withColor (Just (PresetColor 2)) (textNode nid pos size payload)

    lineNode (Line name (p,q,r), i) =
      let nid = lineNodeId name
          pos = (100 + i * 220, 140)
          size = (200, 60)
          payload = jsonObj
            [ ("kind", jsonText "mnemonic.fano.line")
            , ("name", jsonText name)
            , ("points", jsonArray (map (jsonText . pointBitsText) [p,q,r]))
            ]
      in withColor (Just (PresetColor 5)) (textNode nid pos size payload)

    orderFeatureNode =
      let nid = orderFeatureNodeId
          pos = (40, 260)
          size = (240, 60)
          payload = jsonObj
            [ ("kind", jsonText "mnemonic.feature")
            , ("name", jsonText "order")
            ]
      in withColor (Just (PresetColor 4)) (textNode nid pos size payload)

    refFeatureNode nid name y =
      let pos = (40, y)
          size = (240, 60)
          payload = jsonObj
            [ ("kind", jsonText "mnemonic.feature")
            , ("name", jsonText name)
            ]
      in withColor (Just (PresetColor 4)) (textNode nid pos size payload)

emitClauseEvents :: EmitOptions -> CanonTriple -> [CanvasEvent]
emitClauseEvents EmitOptions{..} CanonTriple{..} =
  [EvAddNode clauseNode] <>
  pointEdges <>
  lineEdges <>
  orderEdges <>
  refEdges <>
  groupOrderEdges <>
  buildRootEdges <>
  centroidEvents
  where
    Evidence{..} = ctEvidence
    charLength = max 0 (evSpanEnd - evSpanStart)

    clauseNodeId :: NodeId
    clauseNodeId =
      let h = shortHashHex16 (ctDoc <> "|" <> T.pack (show evSpanStart) <> "|" <> T.pack (show evSpanEnd))
      in NodeId ("MM:CLAUSE:" <> h)

    clauseNode :: Node
    clauseNode =
      let payload = jsonObj
            [ ("kind", jsonText "mnemonic.clause")
            , ("doc", jsonText ctDoc)
            , ("evidence", jsonObj
                [ ("doc_bytes", jsonInt evDocBytes)
                , ("doc_lines", jsonInt evDocLines)
                , ("span_start", jsonInt evSpanStart)
                , ("span_end", jsonInt evSpanEnd)
                , ("line_length_bytes", jsonInt evLineLength)
                , ("char_length", jsonInt charLength)
                ])
            ]
      in withColor (Just (PresetColor 1)) (textNode clauseNodeId (evDocBytes, evDocLines) (evSpanStart, charLength) payload)

    a = hashS ctVersions (tSubject ctTriple)
    b = hashO ctVersions (tObject ctTriple)
    c = hashP ctVersions (tPredicate ctTriple)

    pointEdges :: [CanvasEvent]
    pointEdges = flip map allPoints $ \p ->
      let v = pointValue ctVersions ctTriple p
          payload = jsonObj
            [ ("kind", jsonText "mnemonic.point.value")
            , ("point", jsonText (pointBitsText p))
            , ("value_u64", jsonWord64 v)
            , ("generators", jsonObj
                [ ("A_S_u64", jsonWord64 a)
                , ("B_O_u64", jsonWord64 b)
                , ("C_P_u64", jsonWord64 c)
                ])
            , ("versions", jsonObj
                [ ("lexicon", jsonText (lexiconVersion ctVersions))
                , ("parser", jsonText (parserVersion ctVersions))
                ])
            ]
          eid = EdgeId ("MM:E:" <> shortHashHex16 (unNodeId clauseNodeId <> ":P:" <> pointBitsText p))
      in EvAddEdge $ withEdgeLabel (Just payload) (edge eid clauseNodeId (pointNodeId p))

    lineEdges :: [CanvasEvent]
    lineEdges = flip map allLines $ \(Line name (p,q,r)) ->
      let vp = pointValue ctVersions ctTriple p
          vq = pointValue ctVersions ctTriple q
          vr = pointValue ctVersions ctTriple r
          ok = (vp `xor` vq `xor` vr) == 0
          payload = jsonObj
            [ ("kind", jsonText "mnemonic.line.invariant")
            , ("line", jsonText name)
            , ("points", jsonArray (map (jsonText . pointBitsText) [p,q,r]))
            , ("xor_ok", jsonBool ok)
            ]
          eid = EdgeId ("MM:E:" <> shortHashHex16 (unNodeId clauseNodeId <> ":L:" <> name))
      in EvAddEdge $ withEdgeLabel (Just payload) (edge eid clauseNodeId (lineNodeId name))

    orderEdges :: [CanvasEvent]
    orderEdges = case ctOrder of
      Nothing -> []
      Just n ->
        let payload = jsonObj
              [ ("kind", jsonText "mnemonic.feature.order")
              , ("value", jsonInt n)
              ]
            eid = EdgeId ("MM:E:" <> shortHashHex16 (unNodeId clauseNodeId <> ":FEATURE:order"))
        in [EvAddEdge (withEdgeLabel (Just payload) (edge eid clauseNodeId orderFeatureNodeId))]

    refEdges :: [CanvasEvent]
    refEdges =
      catMaybes
        [ mkRefEdge "subject" ctSubjectRefDepth (tSubject ctTriple) refSubjectNodeId
        , mkRefEdge "object" ctObjectRefDepth (tObject ctTriple) refObjectNodeId
        , mkRefEdge "predicate" ctPredicateRefDepth (tPredicate ctTriple) refPredicateNodeId
        ]

    mkRefEdge :: Text -> Int -> Text -> NodeId -> Maybe CanvasEvent
    mkRefEdge field depth raw target
      | depth <= 0 = Nothing
      | otherwise =
          let (_d, innerRaw) = stripBalancedBrackets raw
              refKey = T.strip innerRaw
              refU64 = sha256U64BE (TE.encodeUtf8 refKey)
              payload = jsonObj $
                [ ("kind", jsonText "mnemonic.feature.ref")
                , ("field", jsonText field)
                , ("depth", jsonInt depth)
                , ("target_text", jsonText refKey)
                , ("ref_key", jsonText refKey)
                , ("ref_u64", jsonWord64 refU64)
                ] ++
                [ ("order", jsonInt n) | Just n <- [ctOrder] ]
              eid = EdgeId ("MM:E:" <> shortHashHex16 (unNodeId clauseNodeId <> ":FEATURE:ref:" <> field))
          in Just (EvAddEdge (withEdgeLabel (Just payload) (edge eid clauseNodeId target)))

    groupOrderEdges :: [CanvasEvent]
    groupOrderEdges =
      let d = maximum [ctSubjectRefDepth, ctPredicateRefDepth, ctObjectRefDepth]
      in if d <= 0
           then []
           else
             let payload = jsonObj
                   [ ("kind", jsonText "mnemonic.feature.group_order")
                   , ("value", jsonInt d)
                   , ("depths", jsonObj
                       [ ("subject", jsonInt ctSubjectRefDepth)
                       , ("predicate", jsonInt ctPredicateRefDepth)
                       , ("object", jsonInt ctObjectRefDepth)
                       ])
                   ]
                 eid = EdgeId ("MM:E:" <> shortHashHex16 (unNodeId clauseNodeId <> ":FEATURE:group_order"))
             in [EvAddEdge (withEdgeLabel (Just payload) (edge eid clauseNodeId groupOrderNodeId))]

    buildRootEdges :: [CanvasEvent]
    buildRootEdges = case eoBuildRoot of
      Nothing -> []
      Just BuildRootInfo{..} ->
        let payload = jsonObj $
              [ ("kind", jsonText "build.root")
              , ("sha256", jsonText brSha256Hex)
              ] ++
              [ ("manifest_path", jsonText p) | Just p <- [brManifestPath] ]
            eid = EdgeId ("MM:E:" <> shortHashHex16 (unNodeId clauseNodeId <> ":FEATURE:build.root:" <> brSha256Hex))
        in [EvAddEdge (withEdgeLabel (Just payload) (edge eid clauseNodeId buildRootNodeId))]

    centroidEvents :: [CanvasEvent]
    centroidEvents
      | not eoCentroid = []
      | otherwise =
          let nid = NodeId ("MM:OBSERVER:" <> shortHashHex16 (unNodeId clauseNodeId))
              payload = jsonObj
                [ ("kind", jsonText "mnemonic.observer")
                , ("closure_satisfied", jsonInt (closureSatisfiedLines ctVersions ctTriple))
                , ("closure_total", jsonInt closureTotalLines)
                , ("stop_unsatisfied", jsonInt (stopUnsatisfiedLines ctVersions ctTriple))
                , ("sabbath", jsonBool (sabbath ctVersions ctTriple))
                ]
              node = withColor (Just (PresetColor 6)) (textNode nid (evDocBytes, evDocLines + 40) (240, 80) payload)
              eid = EdgeId ("MM:E:" <> shortHashHex16 (unNodeId clauseNodeId <> ":OBSERVER"))
              e = withEdgeLabel (Just (jsonObj [("kind", jsonText "mnemonic.observer.link")])) (edge eid clauseNodeId nid)
          in [EvAddNode node, EvAddEdge e]

pointNodeId :: Point -> NodeId
pointNodeId p = NodeId ("MM:POINT:" <> pointBitsText p)

lineNodeId :: Text -> NodeId
lineNodeId name = NodeId ("MM:LINE:" <> name)

orderFeatureNodeId :: NodeId
orderFeatureNodeId = NodeId "MM:FEATURE:order"

refSubjectNodeId :: NodeId
refSubjectNodeId = NodeId "MM:FEATURE:ref.subject"

refObjectNodeId :: NodeId
refObjectNodeId = NodeId "MM:FEATURE:ref.object"

refPredicateNodeId :: NodeId
refPredicateNodeId = NodeId "MM:FEATURE:ref.predicate"

groupOrderNodeId :: NodeId
groupOrderNodeId = NodeId "MM:FEATURE:group_order"

buildRootNodeId :: NodeId
buildRootNodeId = NodeId "MM:FEATURE:build.root"
