{-# LANGUAGE NoImplicitPrelude #-}

module OMI.Gossip.Types
  ( GossipMessage(..)
  , GossipReject(..)
  , gossipFragmentRelation
  ) where

import OMI.Carrier
import OMI.Kernel

data GossipMessage =
    GossipHello CausalIndex
  | GossipCarrierFragment CarrierFragment
  | GossipRejectMessage GossipReject

data GossipReject =
    RejectMalformed
  | RejectTooLarge
  | RejectUnsupportedCarrier

gossipFragmentRelation :: GossipMessage -> Relation
gossipFragmentRelation (GossipCarrierFragment fragment) = carrierFragmentRelation fragment
gossipFragmentRelation _ = nullRelation

nullRelation :: Relation
nullRelation =
  Relation
    (W16 nullByte nullByte)
    (W16 nullByte nullByte)
    (W16 nullByte nullByte)
    (W16 nullByte nullByte)
    (W16 nullByte nullByte)
    (W16 nullByte nullByte)
    (W16 nullByte nullByte)
    (W16 nullByte nullByte)
    (W32 (W16 nullByte nullByte) (W16 nullByte nullByte))
    (W32 (W16 nullByte nullByte) (W16 nullByte nullByte))
    (W32 (W16 nullByte nullByte) (W16 nullByte nullByte))
    (W32 (W16 nullByte nullByte) (W16 nullByte nullByte))

nullByte :: Byte
nullByte = B (N O O O O) (N O O O O)
