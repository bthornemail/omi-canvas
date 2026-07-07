module Main where

import Prelude

import qualified OMI.DecisionTable as DT
import qualified OMI.Gauge as G
import qualified OMI.Lisp as L
import qualified OMI.Pipeline as P
import qualified OMI.Wittgenstein as W
import OMI.Kernel

main :: IO ()
main = do
  testGaugeOperators
  testPreHeader
  testInvalidGauge
  testResolveDeclaration
  testDecisionTableExtraction
  testClosureAndContradiction

testGaugeOperators :: IO ()
testGaugeOperators =
  sequence_
    [ assertOperator "F0" L.gaugeF0 (code O O O O)
    , assertOperator "F1" L.gaugeF1 (code O O O I)
    , assertOperator "F2" L.gaugeF2 (code O O I O)
    , assertOperator "F3" L.gaugeF3 (code O O I I)
    , assertOperator "F4" L.gaugeF4 (code O I O O)
    , assertOperator "F5" L.gaugeF5 (code O I O I)
    , assertOperator "F6" L.gaugeF6 (code O I I O)
    , assertOperator "F7" L.gaugeF7 (code O I I I)
    , assertOperator "F8" L.gaugeF8 (code I O O O)
    , assertOperator "F9" L.gaugeF9 (code I O O I)
    , assertOperator "FA" L.gaugeFA (code I O I O)
    , assertOperator "FB" L.gaugeFB (code I O I I)
    , assertOperator "FC" L.gaugeFC (code I I O O)
    , assertOperator "FD" L.gaugeFD (code I I O I)
    , assertOperator "FE" L.gaugeFE (code I I I O)
    , assertOperator "FF" L.gaugeFF (code I I I I)
    ]

testPreHeader :: IO ()
testPreHeader =
  assertOmiTrue "canonical gauge pre-header did not match" $
    G.matchesCanonicalPreHeader G.canonicalGaugePreHeader

testInvalidGauge :: IO ()
testInvalidGauge =
  assertOmiTrue "non-F* gauge byte was accepted" $
    L.notB (G.gaugeIsValid (G.mkGauge L.byteNull))

testResolveDeclaration :: IO ()
testResolveDeclaration =
  case P.attestationRelation (P.resolveDeclaration minimalDeclaration) of
    Relation{} -> pure ()

testDecisionTableExtraction :: IO ()
testDecisionTableExtraction =
  case DT.decisionRules (DT.decisionTableFromSExpr decisionExpr) of
    [] -> fail "decision table did not extract rules"
    _ -> pure ()

testClosureAndContradiction :: IO ()
testClosureAndContradiction = do
  assertOmiTrue "FF did not produce tautology" $
    W.wittIsTautology (G.gaugeToOperator (G.mkGauge L.gaugeFF))
  assertOmiTrue "F0 did not produce contradiction" $
    W.wittIsContradiction (G.gaugeToOperator (G.mkGauge L.gaugeF0))

assertOperator :: String -> Byte -> Byte -> IO ()
assertOperator label gauge expected =
  let op = G.gaugeToOperator (G.mkGauge gauge)
  in assertOmiTrue ("operator mismatch for " ++ label) $
       L.eqByte (W.wittOperatorCode op) expected

assertOmiTrue :: String -> L.Bool -> IO ()
assertOmiTrue _ L.Tru = pure ()
assertOmiTrue label L.Fls = fail label

code :: Bit -> Bit -> Bit -> Bit -> Byte
code a b c d = L.mkByte O O O O a b c d

sym :: [Byte] -> L.SExpr
sym = L.SSym

pair :: L.SExpr -> L.SExpr -> L.SExpr
pair = L.SCons

list :: [L.SExpr] -> L.SExpr
list [] = L.SNil
list (x:xs) = L.SCons x (list xs)

minimalDeclaration :: L.SExpr
minimalDeclaration = pair (sym [L.gaugeFF]) L.SNil

decisionExpr :: L.SExpr
decisionExpr =
  pair
    (sym L.decisionTableSym)
    (list
      [ pair (sym L.nameTag) (sym [L.byteA])
      , pair (sym L.inputsTag) (list [sym [L.byteA]])
      , pair (sym L.outputTag) (sym [L.byteA])
      , pair (sym L.operatorTag) (sym [L.gaugeFF])
      , pair (sym L.rulesTag)
          (list [pair (sym [L.byteA]) (sym [L.byteA])])
      ])
