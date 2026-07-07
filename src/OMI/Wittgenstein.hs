{-# LANGUAGE NoImplicitPrelude #-}

module OMI.Wittgenstein
  ( TruthVector
  , WittgensteinOperator
  , wittOperatorCode
  , wittOperatorNibble
  , wittTruthVector
  , wittIsContradiction
  , wittIsTautology
  , wittOperatorFromNibble
  , wittOperatorFromByte
  , invalidWittgensteinOperator
  , nibbleToCodeByte
  , lowNibble
  , nibbleToTruthVector
  , witt00
  , witt01
  , witt02
  , witt03
  , witt04
  , witt05
  , witt06
  , witt07
  , witt08
  , witt09
  , witt10
  , witt11
  , witt12
  , witt13
  , witt14
  , witt15
  , isOperatorCode
  ) where

import OMI.Kernel
import OMI.Lisp hiding (wittIsContradiction, wittIsTautology, wittOperatorCode, wittTruthBits, wittTruthNibble)

data TruthVector = TruthVector Bit Bit Bit Bit

data WittgensteinOperator =
    WittgensteinOperator Byte Nibble TruthVector
  | InvalidWittgensteinOperator Byte

invalidWittgensteinOperator :: Byte -> WittgensteinOperator
invalidWittgensteinOperator = InvalidWittgensteinOperator

wittOperatorCode :: WittgensteinOperator -> Byte
wittOperatorCode (WittgensteinOperator code _ _) = code
wittOperatorCode (InvalidWittgensteinOperator code) = code

wittOperatorNibble :: WittgensteinOperator -> Nibble
wittOperatorNibble (WittgensteinOperator _ nib _) = nib
wittOperatorNibble (InvalidWittgensteinOperator _) = N O O O O

wittTruthVector :: WittgensteinOperator -> TruthVector
wittTruthVector (WittgensteinOperator _ _ truth) = truth
wittTruthVector (InvalidWittgensteinOperator _) = TruthVector O O O O

wittIsContradiction :: WittgensteinOperator -> Bool
wittIsContradiction op = eqNibble (wittOperatorNibble op) (N O O O O)

wittIsTautology :: WittgensteinOperator -> Bool
wittIsTautology op = eqNibble (wittOperatorNibble op) (N I I I I)

wittOperatorFromNibble :: Nibble -> WittgensteinOperator
wittOperatorFromNibble nib =
  WittgensteinOperator (nibbleToCodeByte nib) nib (nibbleToTruthVector nib)

wittOperatorFromByte :: Byte -> WittgensteinOperator
wittOperatorFromByte b =
  let nib = lowNibble b
  in wittOperatorFromNibble nib

nibbleToCodeByte :: Nibble -> Byte
nibbleToCodeByte (N a b c d) = mkByte O O O O a b c d

lowNibble :: Byte -> Nibble
lowNibble (B _ nib) = nib

nibbleToTruthVector :: Nibble -> TruthVector
nibbleToTruthVector (N a b c d) = TruthVector a b c d

witt00 :: WittgensteinOperator
witt00 = wittOperatorFromNibble (N O O O O)

witt01 :: WittgensteinOperator
witt01 = wittOperatorFromNibble (N O O O I)

witt02 :: WittgensteinOperator
witt02 = wittOperatorFromNibble (N O O I O)

witt03 :: WittgensteinOperator
witt03 = wittOperatorFromNibble (N O O I I)

witt04 :: WittgensteinOperator
witt04 = wittOperatorFromNibble (N O I O O)

witt05 :: WittgensteinOperator
witt05 = wittOperatorFromNibble (N O I O I)

witt06 :: WittgensteinOperator
witt06 = wittOperatorFromNibble (N O I I O)

witt07 :: WittgensteinOperator
witt07 = wittOperatorFromNibble (N O I I I)

witt08 :: WittgensteinOperator
witt08 = wittOperatorFromNibble (N I O O O)

witt09 :: WittgensteinOperator
witt09 = wittOperatorFromNibble (N I O O I)

witt10 :: WittgensteinOperator
witt10 = wittOperatorFromNibble (N I O I O)

witt11 :: WittgensteinOperator
witt11 = wittOperatorFromNibble (N I O I I)

witt12 :: WittgensteinOperator
witt12 = wittOperatorFromNibble (N I I O O)

witt13 :: WittgensteinOperator
witt13 = wittOperatorFromNibble (N I I O I)

witt14 :: WittgensteinOperator
witt14 = wittOperatorFromNibble (N I I I O)

witt15 :: WittgensteinOperator
witt15 = wittOperatorFromNibble (N I I I I)

isOperatorCode :: WittgensteinOperator -> Byte -> Bool
isOperatorCode op code = eqByte (wittOperatorCode op) code
