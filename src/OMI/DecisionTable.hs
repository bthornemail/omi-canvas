{-# LANGUAGE NoImplicitPrelude #-}

module OMI.DecisionTable
  ( DecisionTable
  , decisionName
  , decisionInputs
  , decisionOutput
  , decisionOperator
  , decisionRules
  , decisionGate
  , emptyDecisionTable
  , decisionTableFromSExpr
  , fromLispDecisionTable
  , decisionFromTruthGate
  , operatorFromSExpr
  ) where

import qualified OMI.Lisp as L
import OMI.Kernel
import OMI.Lisp hiding (DecisionTable)
import OMI.TruthGate
import OMI.Wittgenstein

data DecisionTable = DecisionTable
  { decisionName :: SExpr
  , decisionInputs :: [SExpr]
  , decisionOutput :: SExpr
  , decisionOperator :: WittgensteinOperator
  , decisionRules :: [(SExpr, SExpr)]
  , decisionGate :: TruthGateFamily
  }

emptyDecisionTable :: DecisionTable
emptyDecisionTable = DecisionTable SNil [] SNil witt15 [] InvalidGateFamily

decisionTableFromSExpr :: SExpr -> DecisionTable
decisionTableFromSExpr expr =
  case isDecisionTable expr of
    Tru -> fromLispDecisionTable (L.asDecisionTable expr)
    Fls -> emptyDecisionTable

fromLispDecisionTable :: L.DecisionTable -> DecisionTable
fromLispDecisionTable dt =
  DecisionTable
    (L.dtName dt)
    (L.dtInputs dt)
    (L.dtOutput dt)
    (operatorFromSExpr (L.dtOperator dt))
    (L.dtRules dt)
    RulesGate

decisionFromTruthGate :: TruthGate -> DecisionTable
decisionFromTruthGate gate =
  DecisionTable
    SNil
    []
    SNil
    (truthGateOperator gate)
    []
    (truthGateFamily gate)

operatorFromSExpr :: SExpr -> WittgensteinOperator
operatorFromSExpr (SSym (b:_)) =
  case isGaugeByte b of
    Tru -> wittOperatorFromByte b
    Fls -> invalidWittgensteinOperator b
operatorFromSExpr _ = witt15
