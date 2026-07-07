{-# LANGUAGE NoImplicitPrelude #-}

module OMI.Kernel where

data Null = Null

data Bit = O | I

data Pair a b = Pair a b

cons :: a -> b -> Pair a b
cons a b = Pair a b

car :: Pair a b -> a
car (Pair a _) = a

cdr :: Pair a b -> b
cdr (Pair _ b) = b

data Nibble = N Bit Bit Bit Bit

data Byte = B Nibble Nibble

data Word16 = W16 Byte Byte

data Word32 = W32 Word16 Word16

data Relation = Relation
  Word16 Word16 Word16 Word16
  Word16 Word16 Word16 Word16
  Word32 Word32 Word32 Word32
