{-# LANGUAGE NoImplicitPrelude #-}

module OMI.Carrier
  ( CarrierPrefix
  , AddressedFrame
  , UnaryRegister
  , CarrierFragment
  , CausalIndex
  , CarrierSymbol
  , canonicalCarrierPrefix
  , mkAddressedFrame
  , mkUnaryRegister
  , emptyUnaryRegister
  , emptyCarrierFragment
  , mkCausalIndex
  , mkCarrierSymbol
  , attachCAR
  , attachCDR
  , advanceUnary
  , xorWitness
  , carrierFragmentRelation
  , carrierPrefixBytes
  , addressedFrameAddress
  , addressedFrameGaugePreHeader
  , addressedFramePlaceValue
  , unaryRegisterHi
  , unaryRegisterLo
  , carrierCarFrame
  , carrierCdrFrame
  , carrierUnaryRegister
  , causalIndexWord
  , carrierSymbolWord
  ) where

import OMI.Core
import OMI.Gauge
import OMI.Kernel
import OMI.Lisp
  ( gaugePreHeader
  )
import OMI.Relation
  ( nullByte
  , relW16a
  , relW16b
  , relW16c
  , relW16d
  , relW16e
  , relW16f
  , relW16g
  , relW16h
  , relW32a
  , relW32b
  , relW32c
  , relW32d
  )

newtype CarrierPrefix = CarrierPrefix [Byte]

data AddressedFrame = AddressedFrame Relation GaugePreHeader Relation

data UnaryRegister = UnaryRegister Word32 Word32

data FrameSlot =
    EmptyFrame
  | FrameSlot AddressedFrame

data CarrierFragment =
  CarrierFragment CarrierPrefix FrameSlot FrameSlot UnaryRegister CausalIndex CarrierSymbol

newtype CausalIndex = CausalIndex Word32

newtype CarrierSymbol = CarrierSymbol Word16

canonicalCarrierPrefix :: CarrierPrefix
canonicalCarrierPrefix = CarrierPrefix gaugePreHeader

mkAddressedFrame :: Relation -> Gauge -> Relation -> AddressedFrame
mkAddressedFrame address gauge placeValue =
  AddressedFrame address (gaugeToPreHeader gauge) placeValue

mkUnaryRegister :: Word32 -> Word32 -> UnaryRegister
mkUnaryRegister = UnaryRegister

emptyUnaryRegister :: UnaryRegister
emptyUnaryRegister = UnaryRegister null32 null32

emptyCarrierFragment :: CarrierFragment
emptyCarrierFragment =
  CarrierFragment canonicalCarrierPrefix EmptyFrame EmptyFrame emptyUnaryRegister
    (CausalIndex null32)
    (CarrierSymbol null16)

mkCausalIndex :: Word32 -> CausalIndex
mkCausalIndex = CausalIndex

mkCarrierSymbol :: Word16 -> CarrierSymbol
mkCarrierSymbol = CarrierSymbol

attachCAR :: AddressedFrame -> CarrierFragment -> CarrierFragment
attachCAR frame (CarrierFragment prefix _ cdr reg idx sym) =
  CarrierFragment prefix (FrameSlot frame) cdr reg idx sym

attachCDR :: AddressedFrame -> CarrierFragment -> CarrierFragment
attachCDR frame (CarrierFragment prefix car _ reg idx sym) =
  CarrierFragment prefix car (FrameSlot frame) reg idx sym

advanceUnary :: UnaryRegister -> UnaryRegister
advanceUnary (UnaryRegister hi lo) =
  case shiftWord32Left lo of
    Shifted lo' carry ->
      let hiShift = shiftWord32Left hi
      in UnaryRegister (orWord32 (shiftedWord hiShift) (carryWord32 carry)) lo'

xorWitness :: UnaryRegister -> UnaryRegister -> UnaryRegister
xorWitness (UnaryRegister a b) (UnaryRegister c d) =
  UnaryRegister (xorWord32 a c) (xorWord32 b d)

carrierFragmentRelation :: CarrierFragment -> Relation
carrierFragmentRelation (CarrierFragment _ car cdr reg idx sym) =
  Relation
    (relW16a carRel)
    (relW16b cdrRel)
    (carrierSymbolWord sym)
    (relW16d idxRel)
    (relW16e carRel)
    (relW16f cdrRel)
    (relW16g regRel)
    (relW16h regRel)
    (relW32a carRel)
    (relW32a cdrRel)
    (unaryRegisterHi reg)
    (unaryRegisterLo reg)
  where
    carRel = frameSlotRelation car
    cdrRel = frameSlotRelation cdr
    regRel = unaryRelation reg
    idxRel = causalIndexRelation idx

carrierPrefixBytes :: CarrierPrefix -> [Byte]
carrierPrefixBytes (CarrierPrefix bs) = bs

addressedFrameAddress :: AddressedFrame -> Relation
addressedFrameAddress (AddressedFrame address _ _) = address

addressedFrameGaugePreHeader :: AddressedFrame -> GaugePreHeader
addressedFrameGaugePreHeader (AddressedFrame _ preHeader _) = preHeader

addressedFramePlaceValue :: AddressedFrame -> Relation
addressedFramePlaceValue (AddressedFrame _ _ placeValue) = placeValue

unaryRegisterHi :: UnaryRegister -> Word32
unaryRegisterHi (UnaryRegister hi _) = hi

unaryRegisterLo :: UnaryRegister -> Word32
unaryRegisterLo (UnaryRegister _ lo) = lo

carrierCarFrame :: CarrierFragment -> Relation
carrierCarFrame (CarrierFragment _ car _ _ _ _) = frameSlotRelation car

carrierCdrFrame :: CarrierFragment -> Relation
carrierCdrFrame (CarrierFragment _ _ cdr _ _ _) = frameSlotRelation cdr

carrierUnaryRegister :: CarrierFragment -> UnaryRegister
carrierUnaryRegister (CarrierFragment _ _ _ reg _ _) = reg

causalIndexWord :: CausalIndex -> Word32
causalIndexWord (CausalIndex word) = word

carrierSymbolWord :: CarrierSymbol -> Word16
carrierSymbolWord (CarrierSymbol word) = word

frameSlotRelation :: FrameSlot -> Relation
frameSlotRelation EmptyFrame = nullRel
frameSlotRelation (FrameSlot frame) = addressedFrameRelation frame

addressedFrameRelation :: AddressedFrame -> Relation
addressedFrameRelation (AddressedFrame address _ placeValue) =
  Relation
    (relW16a address)
    (relW16b placeValue)
    (relW16c address)
    (relW16d placeValue)
    (relW16e address)
    (relW16f placeValue)
    (relW16g address)
    (relW16h placeValue)
    (relW32a address)
    (relW32a placeValue)
    (relW32c address)
    (relW32c placeValue)

unaryRelation :: UnaryRegister -> Relation
unaryRelation (UnaryRegister hi lo) =
  Relation null16 null16 null16 null16 null16 null16 null16 null16 hi lo null32 null32

causalIndexRelation :: CausalIndex -> Relation
causalIndexRelation (CausalIndex word) =
  Relation null16 null16 null16 null16 null16 null16 null16 null16 word null32 null32 null32

data Shifted = Shifted Word32 Bit

shiftedWord :: Shifted -> Word32
shiftedWord (Shifted word _) = word

shiftWord32Left :: Word32 -> Shifted
shiftWord32Left (W32 hi lo) =
  case shiftWord16Left lo of
    Shifted16 lo' carry ->
      case shiftWord16Left hi of
        Shifted16 hi' carry' -> Shifted (W32 (orWord16 hi' (carryWord16 carry)) lo') carry'

data Shifted16 = Shifted16 Word16 Bit

shiftWord16Left :: Word16 -> Shifted16
shiftWord16Left (W16 hi lo) =
  case shiftByteLeft lo of
    Shifted8 lo' carry ->
      case shiftByteLeft hi of
        Shifted8 hi' carry' -> Shifted16 (W16 (orByte hi' (carryByte carry)) lo') carry'

data Shifted8 = Shifted8 Byte Bit

shiftByteLeft :: Byte -> Shifted8
shiftByteLeft (B hi lo) =
  case shiftNibbleLeft lo of
    Shifted4 lo' carry ->
      case shiftNibbleLeft hi of
        Shifted4 hi' carry' -> Shifted8 (B (orNibble hi' (carryNibble carry)) lo') carry'

data Shifted4 = Shifted4 Nibble Bit

shiftNibbleLeft :: Nibble -> Shifted4
shiftNibbleLeft (N a b c d) = Shifted4 (N b c d O) a

carryWord32 :: Bit -> Word32
carryWord32 b = W32 (carryWord16 b) null16

carryWord16 :: Bit -> Word16
carryWord16 b = W16 (carryByte b) nullByte

carryByte :: Bit -> Byte
carryByte b = B (carryNibble b) (N O O O O)

carryNibble :: Bit -> Nibble
carryNibble b = N b O O O

orWord32 :: Word32 -> Word32 -> Word32
orWord32 (W32 a b) (W32 c d) = W32 (orWord16 a c) (orWord16 b d)

orWord16 :: Word16 -> Word16 -> Word16
orWord16 (W16 a b) (W16 c d) = W16 (orByte a c) (orByte b d)

orByte :: Byte -> Byte -> Byte
orByte (B a b) (B c d) = B (orNibble a c) (orNibble b d)

orNibble :: Nibble -> Nibble -> Nibble
orNibble (N a b c d) (N e f g h) =
  N (orBit a e) (orBit b f) (orBit c g) (orBit d h)

orBit :: Bit -> Bit -> Bit
orBit I _ = I
orBit _ I = I
orBit _ _ = O

xorWord32 :: Word32 -> Word32 -> Word32
xorWord32 (W32 a b) (W32 c d) = W32 (xorWord16 a c) (xorWord16 b d)

xorWord16 :: Word16 -> Word16 -> Word16
xorWord16 (W16 a b) (W16 c d) = W16 (xorByte a c) (xorByte b d)

xorByte :: Byte -> Byte -> Byte
xorByte (B a b) (B c d) = B (xorNibble a c) (xorNibble b d)

xorNibble :: Nibble -> Nibble -> Nibble
xorNibble (N a b c d) (N e f g h) =
  N (xorBit a e) (xorBit b f) (xorBit c g) (xorBit d h)

xorBit :: Bit -> Bit -> Bit
xorBit O O = O
xorBit I I = O
xorBit _ _ = I
