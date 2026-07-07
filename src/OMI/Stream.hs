{-# LANGUAGE NoImplicitPrelude #-}

module OMI.Stream
  ( StreamRecognition(..)
  , recognizeCarrierPrefix
  , stripCarrierPrefix
  ) where

import OMI.Carrier
import OMI.Kernel
import qualified OMI.Lisp as L

data StreamRecognition =
    RecognizedCarrier CarrierPrefix [Byte]
  | UnrecognizedCarrier [Byte]

recognizeCarrierPrefix :: [Byte] -> StreamRecognition
recognizeCarrierPrefix input =
  case stripPrefix (carrierPrefixBytes canonicalCarrierPrefix) input of
    StripOk rest -> RecognizedCarrier canonicalCarrierPrefix rest
    StripFail -> UnrecognizedCarrier input

stripCarrierPrefix :: [Byte] -> [Byte]
stripCarrierPrefix input =
  case recognizeCarrierPrefix input of
    RecognizedCarrier _ rest -> rest
    UnrecognizedCarrier rest -> rest

data StripResult =
    StripOk [Byte]
  | StripFail

stripPrefix :: [Byte] -> [Byte] -> StripResult
stripPrefix [] rest = StripOk rest
stripPrefix (_:_) [] = StripFail
stripPrefix (p:ps) (x:xs) =
  case L.eqByte p x of
    L.Tru -> stripPrefix ps xs
    L.Fls -> StripFail
