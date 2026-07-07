{-# LANGUAGE NoImplicitPrelude #-}

module OMI.Net.Frame
  ( NetFrame
  , mkNetFrame
  , netFramePayload
  , netFramePayloadRelation
  ) where

import OMI.Core
import OMI.Kernel
import OMI.Relation

newtype NetFrame = NetFrame [Byte]

mkNetFrame :: [Byte] -> NetFrame
mkNetFrame = NetFrame

netFramePayload :: NetFrame -> [Byte]
netFramePayload (NetFrame payload) = payload

netFramePayloadRelation :: NetFrame -> Relation
netFramePayloadRelation (NetFrame payload) =
  case packAtom payload of
    Atom rel -> rel
