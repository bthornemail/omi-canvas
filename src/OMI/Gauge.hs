{-# LANGUAGE NoImplicitPrelude #-}

module OMI.Gauge
  ( Gauge
  , GaugePreHeader
  , canonicalGaugePreHeader
  , isGauge
  , mkGauge
  , gaugeIsValid
  , gaugeByte
  , gaugeToOperator
  , gaugeToPreHeader
  , matchesCanonicalPreHeader
  , isCanonicalGaugeClosure
  ) where

import OMI.Kernel
import OMI.Lisp
import OMI.Wittgenstein

data Gauge =
    Gauge Byte
  | InvalidGauge Byte

newtype GaugePreHeader = GaugePreHeader [Byte]

canonicalGaugePreHeader :: GaugePreHeader
canonicalGaugePreHeader = GaugePreHeader gaugePreHeader

isGauge :: Byte -> Bool
isGauge = isGaugeByte

mkGauge :: Byte -> Gauge
mkGauge b =
  case isGauge b of
    Tru -> Gauge b
    Fls -> InvalidGauge b

gaugeByte :: Gauge -> Byte
gaugeByte (Gauge b) = b
gaugeByte (InvalidGauge b) = b

gaugeIsValid :: Gauge -> Bool
gaugeIsValid (Gauge _) = Tru
gaugeIsValid (InvalidGauge _) = Fls

gaugeToOperator :: Gauge -> WittgensteinOperator
gaugeToOperator (Gauge b) = wittOperatorFromByte b
gaugeToOperator (InvalidGauge b) = invalidWittgensteinOperator b

gaugeToPreHeader :: Gauge -> GaugePreHeader
gaugeToPreHeader g =
  let b = gaugeByte g
  in GaugePreHeader [b, gaugeNul, gaugeFs, gaugeGs, gaugeRs, gaugeUs, gaugeSp, b]

matchesCanonicalPreHeader :: GaugePreHeader -> Bool
matchesCanonicalPreHeader (GaugePreHeader bs) = matchGaugePreHeader bs

isCanonicalGaugeClosure :: Gauge -> Bool
isCanonicalGaugeClosure g = eqByte (gaugeByte g) gaugeFF
