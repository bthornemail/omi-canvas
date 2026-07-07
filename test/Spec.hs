module Main where

import Prelude

import qualified OMI.Carrier as C
import qualified OMI.DecisionTable as DT
import qualified OMI.Gauge as G
import qualified OMI.Gossip.Types as GT
import qualified OMI.Lisp as L
import qualified OMI.Markdown as MD
import qualified OMI.Memory as M
import qualified OMI.Stream as Stream
import qualified OMI.Pipeline as P
import qualified OMI.Reconcile as R
import qualified OMI.Relation as Rel
import qualified OMI.Scope as S
import qualified OMI.VCS as VCS
import qualified OMI.Wittgenstein as W
import qualified OMI.Canvas.JSON as CJ
import OMI.Core
import OMI.Kernel

main :: IO ()
main = do
  testGaugeOperators
  testPreHeader
  testInvalidGauge
  testResolveDeclaration
  testDecisionTableExtraction
  testClosureAndContradiction
  testMarkdownExtraction
  testMarkdownTamperRejection
  testCanvasProjection
  testMemoryReconciliation
  testVcsReceiptedRecord
  testCarrierPrefix
  testAddressedFrame
  testCarrierCarCdrComposition
  testUnaryAdvanceAndXor
  testCarrierIgnoresEvidenceChecksumIdentity
  testStreamAndGossipAdapters

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

testMarkdownExtraction :: IO ()
testMarkdownExtraction = do
  let first = MD.extractMarkdown markdownSample
      second = MD.extractMarkdown markdownSample
  assertEqualInt "markdown extraction block count changed"
    (length (MD.markdownBlocks first))
    (length (MD.markdownBlocks second))
  assertEqualInt "markdown declaration count changed"
    (length (MD.markdownDeclarations first))
    (length (MD.markdownDeclarations second))
  case MD.markdownDeclarations first of
    [] -> fail "markdown did not find OMI-Lisp declarations"
    _ -> pure ()
  case MD.markdownFrontMatter first of
    [] -> fail "markdown did not find front matter"
    _ -> pure ()

testMarkdownTamperRejection :: IO ()
testMarkdownTamperRejection =
  case MD.markdownBlocks (MD.extractMarkdown markdownSample) of
    [] -> fail "markdown sample produced no evidence"
    block:_ -> do
      let evidence = MD.blockEvidence block
      assertHostTrue "evidence did not verify against original markdown" $
        MD.verifyEvidence markdownSample evidence
      assertHostFalse "tampered evidence was accepted" $
        MD.verifyEvidence tamperedMarkdownSample evidence
      let originalOmi = findMarkdownBlock MD.OmiLispBlock (MD.extractMarkdown markdownSample)
          changedOmi = findMarkdownBlock MD.OmiLispBlock (MD.extractMarkdown samePlaceDifferentEvidenceSample)
      case (originalOmi, changedOmi) of
        (Just original, Just changed) -> do
          assertRelation "checksum changed Markdown citation identity"
            (MD.evidenceIdentity (MD.blockEvidence original))
            (MD.evidenceIdentity (MD.blockEvidence changed))
          assertHostFalse "changed declaration bytes verified against original evidence" $
            MD.verifyEvidence samePlaceDifferentEvidenceSample (MD.blockEvidence original)
        _ -> fail "could not find OMI-Lisp block for identity/checksum regression"

testCanvasProjection :: IO ()
testCanvasProjection = do
  let att = P.resolveDeclaration minimalDeclaration
      first = CJ.encodeJsonCanvas (CJ.attestationToJsonCanvas att)
      second = CJ.encodeJsonCanvas (CJ.attestationToJsonCanvas att)
  assertEqualString "JSON Canvas projection is not deterministic" first second
  case CJ.jsonCanvasNodes (CJ.attestationToJsonCanvas att) of
    [] -> fail "JSON Canvas projection produced no nodes"
    _ -> pure ()

testMemoryReconciliation :: IO ()
testMemoryReconciliation = do
  let bitboard = M.mkBitboard nullRel [nullRel]
      bitblip = M.mkBitBlip nullRel nullRel nullRel
      boardA = M.resolveBlackboard bitboard bitblip
      boardB = M.resolveBlackboard bitboard bitblip
      state = R.reconcile M.EmmcUser bitboard bitblip
      witness = R.witnessVersion state
      att = R.attestReconciliation witness
  assertRelation "bitboard and bit-blip did not resolve deterministically"
    (P.blackboardRelation boardA)
    (P.blackboardRelation boardB)
  case P.attestationRelation att of
    Relation{} -> pure ()

testVcsReceiptedRecord :: IO ()
testVcsReceiptedRecord = do
  let bitboard = M.mkBitboard nullRel [nullRel]
      bitblip = M.mkBitBlip nullRel nullRel nullRel
      board = M.resolveBlackboard bitboard bitblip
      state = R.reconcile M.EmmcSecure bitboard bitblip
      witness = R.witnessVersion state
      att = R.attestReconciliation witness
      accepted = R.acceptReconciliation att witness
      parent = Receipt nullRel
      scope = S.mkScope [L.byteA] [L.byteA] [L.byteA] [L.byteA]
      record = VCS.recordReceiptedReconciliation
        parent scope M.EmmcSecure bitboard bitblip board accepted
  case VCS.vcsResult record of
    result ->
      case R.acceptedReceipt result of
        Receipt Relation{} -> pure ()

testCarrierPrefix :: IO ()
testCarrierPrefix =
  assertBytes "canonical carrier prefix mismatch"
    L.gaugePreHeader
    (C.carrierPrefixBytes C.canonicalCarrierPrefix)

testAddressedFrame :: IO ()
testAddressedFrame = do
  let frame = C.mkAddressedFrame sampleAddress (G.mkGauge L.gaugeFF) samplePlaceValue
  assertRelation "addressed frame did not preserve address"
    sampleAddress
    (C.addressedFrameAddress frame)
  assertRelation "addressed frame did not preserve place-value frame"
    samplePlaceValue
    (C.addressedFramePlaceValue frame)
  assertOmiTrue "addressed frame did not preserve canonical active gauge preheader" $
    G.matchesCanonicalPreHeader (C.addressedFrameGaugePreHeader frame)

testCarrierCarCdrComposition :: IO ()
testCarrierCarCdrComposition = do
  let carFrame = C.mkAddressedFrame sampleAddress (G.mkGauge L.gaugeFF) samplePlaceValue
      cdrFrame = C.mkAddressedFrame samplePlaceValue (G.mkGauge L.gaugeFF) sampleAddress
      fragment = C.attachCDR cdrFrame (C.attachCAR carFrame C.emptyCarrierFragment)
  assertRelation "CAR frame was not attached as a full addressed frame"
    (C.carrierCarFrame fragment)
    (C.carrierCarFrame (C.attachCAR carFrame C.emptyCarrierFragment))
  assertRelation "CDR frame was not attached as a full addressed continuation"
    (C.carrierCdrFrame fragment)
    (C.carrierCdrFrame (C.attachCDR cdrFrame C.emptyCarrierFragment))

testUnaryAdvanceAndXor :: IO ()
testUnaryAdvanceAndXor = do
  let advancedA = C.advanceUnary unaryOne
      advancedB = C.advanceUnary unaryOne
      zeroWitness = C.xorWitness advancedA advancedB
  assertWord32 "unary advance was not deterministic in high word"
    (C.unaryRegisterHi advancedA)
    (C.unaryRegisterHi advancedB)
  assertWord32 "unary advance was not deterministic in low word"
    (C.unaryRegisterLo advancedA)
    (C.unaryRegisterLo advancedB)
  assertWord32 "xor witness of equal unary registers did not clear high word"
    null32
    (C.unaryRegisterHi zeroWitness)
  assertWord32 "xor witness of equal unary registers did not clear low word"
    null32
    (C.unaryRegisterLo zeroWitness)

testCarrierIgnoresEvidenceChecksumIdentity :: IO ()
testCarrierIgnoresEvidenceChecksumIdentity = do
  let originalOmi = findMarkdownBlock MD.OmiLispBlock (MD.extractMarkdown markdownSample)
      changedOmi = findMarkdownBlock MD.OmiLispBlock (MD.extractMarkdown samePlaceDifferentEvidenceSample)
      carFrame = C.mkAddressedFrame sampleAddress (G.mkGauge L.gaugeFF) samplePlaceValue
      cdrFrame = C.mkAddressedFrame samplePlaceValue (G.mkGauge L.gaugeFF) sampleAddress
      fragment = C.attachCDR cdrFrame (C.attachCAR carFrame C.emptyCarrierFragment)
  case (originalOmi, changedOmi) of
    (Just original, Just changed) -> do
      assertRelation "evidence checksum changed evidence citation identity"
        (MD.evidenceIdentity (MD.blockEvidence original))
        (MD.evidenceIdentity (MD.blockEvidence changed))
      assertRelation "evidence checksum changed carrier fragment identity"
        (C.carrierFragmentRelation fragment)
        (C.carrierFragmentRelation fragment)
    _ -> fail "could not find OMI-Lisp block for carrier checksum regression"

testStreamAndGossipAdapters :: IO ()
testStreamAndGossipAdapters = do
  let payload = [L.byteA]
      streamBytes = C.carrierPrefixBytes C.canonicalCarrierPrefix ++ payload
      carFrame = C.mkAddressedFrame sampleAddress (G.mkGauge L.gaugeFF) samplePlaceValue
      fragment = C.attachCAR carFrame C.emptyCarrierFragment
  case Stream.recognizeCarrierPrefix streamBytes of
    Stream.RecognizedCarrier _ rest -> assertBytes "stream prefix stripped the wrong payload" payload rest
    Stream.UnrecognizedCarrier _ -> fail "canonical carrier prefix was not recognized"
  assertRelation "gossip message did not carry carrier fragment relation"
    (C.carrierFragmentRelation fragment)
    (GT.gossipFragmentRelation (GT.GossipCarrierFragment fragment))

assertOperator :: String -> Byte -> Byte -> IO ()
assertOperator label gauge expected =
  let op = G.gaugeToOperator (G.mkGauge gauge)
  in assertOmiTrue ("operator mismatch for " ++ label) $
       L.eqByte (W.wittOperatorCode op) expected

assertOmiTrue :: String -> L.Bool -> IO ()
assertOmiTrue _ L.Tru = pure ()
assertOmiTrue label L.Fls = fail label

assertHostTrue :: String -> Bool -> IO ()
assertHostTrue _ True = pure ()
assertHostTrue label False = fail label

assertHostFalse :: String -> Bool -> IO ()
assertHostFalse _ False = pure ()
assertHostFalse label True = fail label

assertEqualInt :: String -> Int -> Int -> IO ()
assertEqualInt _ a b | a == b = pure ()
assertEqualInt label _ _ = fail label

assertEqualString :: String -> String -> String -> IO ()
assertEqualString _ a b | a == b = pure ()
assertEqualString label _ _ = fail label

assertRelation :: String -> Relation -> Relation -> IO ()
assertRelation label a b =
  case Rel.eqRelation a b of
    Rel.Tru -> pure ()
    Rel.Fls -> fail label

assertWord32 :: String -> Word32 -> Word32 -> IO ()
assertWord32 label a b =
  case Rel.eqWord32 a b of
    Rel.Tru -> pure ()
    Rel.Fls -> fail label

assertBytes :: String -> [Byte] -> [Byte] -> IO ()
assertBytes _ [] [] = pure ()
assertBytes label (a:as) (b:bs) =
  case L.eqByte a b of
    L.Tru -> assertBytes label as bs
    L.Fls -> fail label
assertBytes label _ _ = fail label

findMarkdownBlock :: MD.MarkdownBlockKind -> MD.MarkdownExtraction -> Maybe MD.MarkdownBlock
findMarkdownBlock kind extraction =
  findByKind (MD.markdownBlocks extraction)
  where
    findByKind [] = Nothing
    findByKind (block:blocks)
      | MD.blockKind block == kind = Just block
      | otherwise = findByKind blocks

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

sampleAddress :: Relation
sampleAddress =
  Relation
    (W16 L.byteA L.byteA)
    null16
    null16
    null16
    null16
    null16
    null16
    null16
    null32
    null32
    null32
    null32

samplePlaceValue :: Relation
samplePlaceValue =
  Relation
    null16
    (W16 L.byteA L.byteA)
    null16
    null16
    null16
    null16
    null16
    null16
    null32
    null32
    null32
    null32

unaryOne :: C.UnaryRegister
unaryOne =
  C.mkUnaryRegister null32
    (W32 null16 (W16 L.byteNull (B (N O O O O) (N O O O I))))

markdownSample :: String
markdownSample =
  "---\n"
  ++ "fs: sample.md\n"
  ++ "---\n"
  ++ "\n"
  ++ "```omi-lisp fs=sample.md gs=rules rs=minimal us=a\n"
  ++ "(a)\n"
  ++ "```\n"
  ++ "\n"
  ++ "```decision-table fs=sample.md gs=rules rs=table us=a\n"
  ++ "(decision-table ((name . a) (inputs . (a)) (output . a) (truth-operator . FF) (rules . ((a . a)))))\n"
  ++ "```\n"
  ++ "\n"
  ++ "```canvas fs=sample.md gs=projection rs=face us=a\n"
  ++ "{\"nodes\":[]}\n"
  ++ "```\n"

tamperedMarkdownSample :: String
tamperedMarkdownSample =
  "-x-\n"
  ++ "fs: sample.md\n"
  ++ "---\n"
  ++ "\n"
  ++ "```omi-lisp fs=sample.md gs=rules rs=minimal us=a\n"
  ++ "(a)\n"
  ++ "```\n"
  ++ "\n"
  ++ "```decision-table fs=sample.md gs=rules rs=table us=a\n"
  ++ "(decision-table ((name . a) (inputs . (a)) (output . a) (truth-operator . FF) (rules . ((a . a)))))\n"
  ++ "```\n"
  ++ "\n"
  ++ "```canvas fs=sample.md gs=projection rs=face us=a\n"
  ++ "{\"nodes\":[]}\n"
  ++ "```\n"

samePlaceDifferentEvidenceSample :: String
samePlaceDifferentEvidenceSample =
  "---\n"
  ++ "fs: sample.md\n"
  ++ "---\n"
  ++ "\n"
  ++ "```omi-lisp fs=sample.md gs=rules rs=minimal us=a\n"
  ++ "(b)\n"
  ++ "```\n"
  ++ "\n"
  ++ "```decision-table fs=sample.md gs=rules rs=table us=a\n"
  ++ "(decision-table ((name . a) (inputs . (a)) (output . a) (truth-operator . FF) (rules . ((a . a)))))\n"
  ++ "```\n"
  ++ "\n"
  ++ "```canvas fs=sample.md gs=projection rs=face us=a\n"
  ++ "{\"nodes\":[]}\n"
  ++ "```\n"
