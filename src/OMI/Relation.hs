{-# LANGUAGE NoImplicitPrelude #-}

module OMI.Relation where

import OMI.Kernel
import OMI.Core

data Bool = Fls | Tru

ite :: Bool -> a -> a -> a
ite Tru x _ = x
ite Fls _ x = x

andB :: Bool -> Bool -> Bool
andB Tru Tru = Tru
andB _ _ = Fls

--
-- Field accessors
--

relW16a :: Relation -> Word16
relW16a (Relation a _ _ _ _ _ _ _ _ _ _ _) = a

relW16b :: Relation -> Word16
relW16b (Relation _ b _ _ _ _ _ _ _ _ _ _) = b

relW16c :: Relation -> Word16
relW16c (Relation _ _ c _ _ _ _ _ _ _ _ _) = c

relW16d :: Relation -> Word16
relW16d (Relation _ _ _ d _ _ _ _ _ _ _ _) = d

relW16e :: Relation -> Word16
relW16e (Relation _ _ _ _ e _ _ _ _ _ _ _) = e

relW16f :: Relation -> Word16
relW16f (Relation _ _ _ _ _ f _ _ _ _ _ _) = f

relW16g :: Relation -> Word16
relW16g (Relation _ _ _ _ _ _ g _ _ _ _ _) = g

relW16h :: Relation -> Word16
relW16h (Relation _ _ _ _ _ _ _ h _ _ _ _) = h

relW32a :: Relation -> Word32
relW32a (Relation _ _ _ _ _ _ _ _ a _ _ _) = a

relW32b :: Relation -> Word32
relW32b (Relation _ _ _ _ _ _ _ _ _ b _ _) = b

relW32c :: Relation -> Word32
relW32c (Relation _ _ _ _ _ _ _ _ _ _ c _) = c

relW32d :: Relation -> Word32
relW32d (Relation _ _ _ _ _ _ _ _ _ _ _ d) = d

--
-- Field setters
--

setW16a :: Relation -> Word16 -> Relation
setW16a (Relation _ b c d e f g h i j k l) a = Relation a b c d e f g h i j k l

setW16b :: Relation -> Word16 -> Relation
setW16b (Relation a _ c d e f g h i j k l) b = Relation a b c d e f g h i j k l

setW16c :: Relation -> Word16 -> Relation
setW16c (Relation a b _ d e f g h i j k l) c = Relation a b c d e f g h i j k l

setW16d :: Relation -> Word16 -> Relation
setW16d (Relation a b c _ e f g h i j k l) d = Relation a b c d e f g h i j k l

setW16e :: Relation -> Word16 -> Relation
setW16e (Relation a b c d _ f g h i j k l) e = Relation a b c d e f g h i j k l

setW16f :: Relation -> Word16 -> Relation
setW16f (Relation a b c d e _ g h i j k l) f = Relation a b c d e f g h i j k l

setW16g :: Relation -> Word16 -> Relation
setW16g (Relation a b c d e f _ h i j k l) g = Relation a b c d e f g h i j k l

setW16h :: Relation -> Word16 -> Relation
setW16h (Relation a b c d e f g _ i j k l) h = Relation a b c d e f g h i j k l

setW32a :: Relation -> Word32 -> Relation
setW32a (Relation a b c d e f g h _ j k l) i = Relation a b c d e f g h i j k l

setW32b :: Relation -> Word32 -> Relation
setW32b (Relation a b c d e f g h i _ k l) j = Relation a b c d e f g h i j k l

setW32c :: Relation -> Word32 -> Relation
setW32c (Relation a b c d e f g h i j _ l) k = Relation a b c d e f g h i j k l

setW32d :: Relation -> Word32 -> Relation
setW32d (Relation a b c d e f g h i j k _) l = Relation a b c d e f g h i j k l

--
-- Word comparison
--

eqWord16 :: Word16 -> Word16 -> Bool
eqWord16 (W16 a b) (W16 c d) = andB (eqWordByte a c) (eqWordByte b d)

eqWordByte :: Byte -> Byte -> Bool
eqWordByte (B a b) (B c d) = andB (eqWordNibble a c) (eqWordNibble b d)

eqWordNibble :: Nibble -> Nibble -> Bool
eqWordNibble (N a b c d) (N e f g h) =
  andB (andB (eqWordBit a e) (eqWordBit b f))
       (andB (eqWordBit c g) (eqWordBit d h))

eqWordBit :: Bit -> Bit -> Bool
eqWordBit O O = Tru
eqWordBit I I = Tru
eqWordBit _ _ = Fls

eqWord32 :: Word32 -> Word32 -> Bool
eqWord32 (W32 a b) (W32 c d) = andB (eqWord16 a c) (eqWord16 b d)

eqRelation :: Relation -> Relation -> Bool
eqRelation
  (Relation a1 b1 c1 d1 e1 f1 g1 h1 i1 j1 k1 l1)
  (Relation a2 b2 c2 d2 e2 f2 g2 h2 i2 j2 k2 l2) =
    andB (andB (eqWord16 a1 a2) (eqWord16 b1 b2))
    (andB (andB (eqWord16 c1 c2) (eqWord16 d1 d2))
    (andB (andB (eqWord16 e1 e2) (eqWord16 f1 f2))
    (andB (andB (eqWord16 g1 g2) (eqWord16 h1 h2))
    (andB (andB (eqWord32 i1 i2) (eqWord32 j1 j2))
          (andB (eqWord32 k1 k2) (eqWord32 l1 l2))))))

--
-- Atom encoding
--

packAtom :: [Byte] -> Atom
packAtom [] = Atom nullRel
packAtom (b1:b2:b3:b4:bs) =
  Atom (setW32a nullRel (W32 (W16 b1 b2) (W16 b3 b4)))
packAtom (b1:b2:b3:bs) =
  Atom (setW32a nullRel (W32 (W16 b1 b2) (W16 b3 nullByte)))
packAtom (b1:b2:bs) =
  Atom (setW32a nullRel (W32 (W16 b1 b2) (W16 nullByte nullByte)))
packAtom (b1:bs) =
  Atom (setW32a nullRel (W32 (W16 b1 nullByte) (W16 nullByte nullByte)))

nullByte :: Byte
nullByte = B (N O O O O) (N O O O O)
