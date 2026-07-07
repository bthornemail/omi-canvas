{-# LANGUAGE NoImplicitPrelude #-}

module OMI.TruthGate
  ( TruthGateFamily(..)
  , TruthGateRow
  , TruthGate
  , rulesGateName
  , factsGateName
  , combinatorsGateName
  , closuresGateName
  , consGateName
  , truthGateFamilyName
  , truthGateFromGauge
  , truthGateOperator
  , truthGateFamily
  , byte1
  , byteConstructive
  ) where

import OMI.Gauge
import OMI.Kernel
import OMI.Lisp
import OMI.Wittgenstein

data TruthGateFamily =
    RulesGate
  | FactsGate
  | CombinatorsGate
  | ClosuresGate
  | ConsGate
  | InvalidGateFamily

data TruthGateRow = TruthGateRow
  { tgQuestion :: Byte
  , tgAlgorithm :: Byte
  , tgCitationSlot :: Byte
  , tgProofForm :: Byte
  , tgOperator :: WittgensteinOperator
  , tgTruthCode :: TruthVector
  }

data TruthGate =
    TruthGate TruthGateFamily TruthGateRow
  | InvalidTruthGate Byte

rulesGateName :: [Byte]
rulesGateName = [byteR, byteU, byteL, byteE, byteS]

factsGateName :: [Byte]
factsGateName = [byteF, byteA, byteC, byteT, byteS]

combinatorsGateName :: [Byte]
combinatorsGateName = [byteC, byteO, byteM, byteB, byteI, byteN, byteA, byteT, byteO, byteR, byteS]

closuresGateName :: [Byte]
closuresGateName = [byteC, byteL, byteO, byteS, byteU, byteR, byteE, byteS]

consGateName :: [Byte]
consGateName = [byteC, byteO, byteN, byteS]

truthGateFamilyName :: TruthGateFamily -> [Byte]
truthGateFamilyName RulesGate = rulesGateName
truthGateFamilyName FactsGate = factsGateName
truthGateFamilyName CombinatorsGate = combinatorsGateName
truthGateFamilyName ClosuresGate = closuresGateName
truthGateFamilyName ConsGate = consGateName
truthGateFamilyName InvalidGateFamily = []

truthGateFromGauge :: Gauge -> TruthGate
truthGateFromGauge g =
  case gaugeIsValid g of
    Fls -> InvalidTruthGate (gaugeByte g)
    Tru ->
      let op = gaugeToOperator g
      in TruthGate RulesGate
           (TruthGateRow byte1 byte1 byte1 byteConstructive op (wittTruthVector op))

truthGateOperator :: TruthGate -> WittgensteinOperator
truthGateOperator (TruthGate _ row) = tgOperator row
truthGateOperator (InvalidTruthGate b) = invalidWittgensteinOperator b

truthGateFamily :: TruthGate -> TruthGateFamily
truthGateFamily (TruthGate fam _) = fam
truthGateFamily (InvalidTruthGate _) = InvalidGateFamily

byte1 :: Byte
byte1 = mkByte O O I I O O O I

byteConstructive :: Byte
byteConstructive = byte1

byteC :: Byte
byteC = mkByte O I O O O O I I

byteB :: Byte
byteB = mkByte O I O O O O I O

byteE :: Byte
byteE = mkByte O I O O O I O I

byteF :: Byte
byteF = mkByte O I O O O I I O

byteI :: Byte
byteI = mkByte O I O O I O O I

byteL :: Byte
byteL = mkByte O I O O I I O O

byteM :: Byte
byteM = mkByte O I O O I I O I

byteN :: Byte
byteN = mkByte O I O O I I I O

byteO :: Byte
byteO = mkByte O I O O I I I I

byteR :: Byte
byteR = mkByte O I O I O O I O

byteS :: Byte
byteS = mkByte O I O I O O I I

byteT :: Byte
byteT = mkByte O I O I O I O O

byteU :: Byte
byteU = mkByte O I O I O I O I
