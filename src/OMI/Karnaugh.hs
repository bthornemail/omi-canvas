{-# LANGUAGE NoImplicitPrelude #-}

module OMI.Karnaugh
  ( KarnaughRegion
  , KarnaughMap
  , reduceKarnaughMap
  , rulesToRegions
  , karnaughOperator
  ) where

import OMI.DecisionTable
import OMI.Lisp hiding (DecisionTable)
import OMI.Wittgenstein

data KarnaughRegion = KarnaughRegion SExpr SExpr TruthVector

data KarnaughMap = KarnaughMap DecisionTable [KarnaughRegion]

reduceKarnaughMap :: DecisionTable -> KarnaughMap
reduceKarnaughMap dt =
  KarnaughMap dt (rulesToRegions (decisionRules dt) (wittTruthVector (decisionOperator dt)))

rulesToRegions :: [(SExpr, SExpr)] -> TruthVector -> [KarnaughRegion]
rulesToRegions [] _ = []
rulesToRegions ((condition, output):rest) truth =
  KarnaughRegion condition output truth : rulesToRegions rest truth

karnaughOperator :: KarnaughMap -> WittgensteinOperator
karnaughOperator (KarnaughMap dt _) = decisionOperator dt
