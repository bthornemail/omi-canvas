{-# LANGUAGE NoImplicitPrelude #-}

module OMI.Core where

import OMI.Kernel

null16 :: Word16
null16 = W16 (B (N O O O O) (N O O O O)) (B (N O O O O) (N O O O O))

null32 :: Word32
null32 = W32 null16 null16

nullRel :: Relation
nullRel = Relation null16 null16 null16 null16
                  null16 null16 null16 null16
                  null32 null32 null32 null32

newtype Atom = Atom Relation

newtype Path = Path Relation

newtype Receipt = Receipt Relation

newtype Envelope = Envelope Relation
