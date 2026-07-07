{-# LANGUAGE NoImplicitPrelude #-}

module OMI.Pipeline
  ( Citation
  , Combinator
  , Delta
  , Blackboard
  , Attestation
  , citeDeclaration
  , selectGauge
  , gaugeToWittgenstein
  , classifyTruthGate
  , decisionFromGate
  , reduceKarnaugh
  , buildCombinator
  , applyDelta
  , constructBlackboard
  , constructBlackboardFromRelation
  , projectFace
  , attestProjection
  , resolveDeclaration
  , blackboardRelation
  , attestationRelation
  ) where

import qualified OMI.DecisionTable as DT
import OMI.Canvas
import OMI.Core
import OMI.DecisionTable
import OMI.Gauge
import OMI.Karnaugh
import OMI.Kernel
import OMI.Lisp hiding (DecisionTable, wittOperatorCode)
import OMI.Relation
  ( packAtom
  , relW16a
  , relW16b
  , relW16c
  , relW16d
  , relW32a
  , relW32c
  , setW16a
  )
import OMI.TruthGate
import OMI.Wittgenstein

data Citation = Citation Relation SExpr Gauge

data Combinator =
    Combinator KarnaughMap Relation
  | InvalidCombinator Relation

data Delta =
    Delta Combinator Relation
  | InvalidDelta Relation

data Blackboard =
    Blackboard Delta Relation
  | InvalidBlackboard Relation

newtype Attestation = Attestation Relation

citeDeclaration :: SExpr -> Citation
citeDeclaration expr = Citation (exprRelation expr) expr (selectGaugeFromExpr expr)

selectGauge :: Citation -> Gauge
selectGauge (Citation _ _ gauge) = gauge

gaugeToWittgenstein :: Gauge -> WittgensteinOperator
gaugeToWittgenstein = gaugeToOperator

classifyTruthGate :: Citation -> Gauge -> TruthGate
classifyTruthGate _ gauge = truthGateFromGauge gauge

decisionFromGate :: TruthGate -> DecisionTable
decisionFromGate = decisionFromTruthGate

reduceKarnaugh :: DecisionTable -> KarnaughMap
reduceKarnaugh = reduceKarnaughMap

buildCombinator :: KarnaughMap -> Combinator
buildCombinator km = Combinator km (operatorRelation (karnaughOperator km))

applyDelta :: Combinator -> Delta
applyDelta comb@(Combinator km rel) =
  Delta comb (setW16a rel (operatorWord16 (karnaughOperator km)))
applyDelta (InvalidCombinator rel) = InvalidDelta rel

constructBlackboard :: Delta -> Blackboard
constructBlackboard delta@(Delta _ rel) = Blackboard delta rel
constructBlackboard (InvalidDelta rel) = InvalidBlackboard rel

constructBlackboardFromRelation :: Relation -> Blackboard
constructBlackboardFromRelation rel = Blackboard (InvalidDelta rel) rel

blackboardRelation :: Blackboard -> Relation
blackboardRelation (Blackboard _ rel) = rel
blackboardRelation (InvalidBlackboard rel) = rel

projectFace :: Blackboard -> ProjectionFace
projectFace (Blackboard _ rel) = projectProjectionFace rel
projectFace (InvalidBlackboard rel) = projectProjectionFace rel

attestProjection :: ProjectionFace -> Attestation
attestProjection face = Attestation (projectionRelation face)

attestationRelation :: Attestation -> Relation
attestationRelation (Attestation rel) = rel

resolveDeclaration :: SExpr -> Attestation
resolveDeclaration expr =
  let citation = citeDeclaration expr
      gauge = selectGauge citation
      gate = classifyTruthGate citation gauge
      table = decisionFromGate gate
      kmap = reduceKarnaugh table
      comb = buildCombinator kmap
      delta = applyDelta comb
      board = constructBlackboard delta
      face = projectFace board
  in attestProjection face

selectGaugeFromExpr :: SExpr -> Gauge
selectGaugeFromExpr (SSym (b:_)) =
  case isGauge b of
    Tru -> mkGauge b
    Fls -> mkGauge gaugeFF
selectGaugeFromExpr (SCons (SSym (b:_)) _) =
  case isGauge b of
    Tru -> mkGauge b
    Fls -> mkGauge gaugeFF
selectGaugeFromExpr _ = mkGauge gaugeFF

exprRelation :: SExpr -> Relation
exprRelation SNil = nullRel
exprRelation (SSym bs) =
  case packAtom bs of
    Atom rel -> rel
exprRelation (SStr bs) =
  case packAtom bs of
    Atom rel -> rel
exprRelation (SCons left right) =
  let l = exprRelation left
      r = exprRelation right
  in Relation
       (relW16a l) (relW16b l) (relW16c l) (relW16d l)
       (relW16a r) (relW16b r) (relW16c r) (relW16d r)
       (relW32a l) (relW32a r) (relW32c l) (relW32c r)

operatorRelation :: WittgensteinOperator -> Relation
operatorRelation op = setW16a nullRel (operatorWord16 op)

operatorWord16 :: WittgensteinOperator -> Word16
operatorWord16 op = W16 byteNull (wittOperatorCode op)
