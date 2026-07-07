{-# LANGUAGE NoImplicitPrelude #-}

module OMI.Scope
  ( OmiScope
  , emptyScope
  , mkScope
  , scopeFs
  , scopeGs
  , scopeRs
  , scopeUs
  , scopeRelation
  ) where

import OMI.Core
import OMI.Kernel
import OMI.Relation

data OmiScope = OmiScope [Byte] [Byte] [Byte] [Byte] Relation

emptyScope :: OmiScope
emptyScope = OmiScope [] [] [] [] nullRel

mkScope :: [Byte] -> [Byte] -> [Byte] -> [Byte] -> OmiScope
mkScope fs gs rs us =
  OmiScope fs gs rs us
    (Relation
      (scopeWord fs) (scopeWord gs) (scopeWord rs) (scopeWord us)
      null16 null16 null16 null16
      null32 null32 null32 null32)

scopeFs :: OmiScope -> [Byte]
scopeFs (OmiScope fs _ _ _ _) = fs

scopeGs :: OmiScope -> [Byte]
scopeGs (OmiScope _ gs _ _ _) = gs

scopeRs :: OmiScope -> [Byte]
scopeRs (OmiScope _ _ rs _ _) = rs

scopeUs :: OmiScope -> [Byte]
scopeUs (OmiScope _ _ _ us _) = us

scopeRelation :: OmiScope -> Relation
scopeRelation (OmiScope _ _ _ _ rel) = rel

scopeWord :: [Byte] -> Word16
scopeWord (a:b:_) = W16 a b
scopeWord (a:_) = W16 a nullByte
scopeWord [] = null16
