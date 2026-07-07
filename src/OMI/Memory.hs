{-# LANGUAGE NoImplicitPrelude #-}

module OMI.Memory
  ( Bitboard
  , BitBlip
  , MemoryFace
  , CarrierFace(..)
  , ReconcileState
  , VersionWitness
  , ReconcileResult(..)
  , mkBitboard
  , mkBitBlip
  , mkMemoryFace
  , bitboardRoot
  , bitboardVertices
  , bitboardWitnessRelation
  , bitBlipSource
  , bitBlipTarget
  , bitBlipTransition
  , bitBlipTransitionRelation
  , memoryFaceCarrier
  , memoryFaceRelation
  , reconcileStateFace
  , reconcileStateBlackboard
  , versionWitnessState
  , versionWitnessRelation
  , resolveBlackboard
  , stageCarrierFace
  , witnessReconcileState
  ) where

import OMI.Core
import OMI.Kernel
import OMI.Pipeline
import OMI.Relation

data Bitboard = Bitboard Relation [Relation]

data BitBlip = BitBlip Relation Relation Relation

data MemoryFace = MemoryFace CarrierFace Relation

data CarrierFace =
    FIFO
  | Inode
  | Mmap
  | EmmcBoot0
  | EmmcBoot1
  | EmmcSecure
  | EmmcUser

data ReconcileState = ReconcileState MemoryFace Blackboard Bitboard BitBlip

data VersionWitness = VersionWitness ReconcileState Relation

data ReconcileResult =
    ReconcileAccepted VersionWitness
  | ReconcileRejected Relation

mkBitboard :: Relation -> [Relation] -> Bitboard
mkBitboard = Bitboard

mkBitBlip :: Relation -> Relation -> Relation -> BitBlip
mkBitBlip = BitBlip

mkMemoryFace :: CarrierFace -> Relation -> MemoryFace
mkMemoryFace = MemoryFace

bitboardRoot :: Bitboard -> Relation
bitboardRoot (Bitboard rel _) = rel

bitboardVertices :: Bitboard -> [Relation]
bitboardVertices (Bitboard _ vertices) = vertices

bitboardWitnessRelation :: Bitboard -> Relation
bitboardWitnessRelation (Bitboard rel []) = rel
bitboardWitnessRelation (Bitboard rel (vertex:_)) =
  Relation
    (relW16a rel) (relW16b rel) (relW16c vertex) (relW16d vertex)
    (relW16e rel) (relW16f rel) (relW16g vertex) (relW16h vertex)
    (relW32a rel) (relW32b rel) (relW32c vertex) (relW32d vertex)

bitBlipSource :: BitBlip -> Relation
bitBlipSource (BitBlip source _ _) = source

bitBlipTarget :: BitBlip -> Relation
bitBlipTarget (BitBlip _ target _) = target

bitBlipTransition :: BitBlip -> Relation
bitBlipTransition (BitBlip _ _ transition) = transition

bitBlipTransitionRelation :: BitBlip -> Relation
bitBlipTransitionRelation (BitBlip source target transition) =
  Relation
    (relW16a source) (relW16b target) (relW16c transition) (relW16d transition)
    (relW16e source) (relW16f target) (relW16g transition) (relW16h transition)
    (relW32a source) (relW32b target) (relW32c transition) (relW32d transition)

memoryFaceCarrier :: MemoryFace -> CarrierFace
memoryFaceCarrier (MemoryFace carrier _) = carrier

memoryFaceRelation :: MemoryFace -> Relation
memoryFaceRelation (MemoryFace _ rel) = rel

reconcileStateFace :: ReconcileState -> MemoryFace
reconcileStateFace (ReconcileState face _ _ _) = face

reconcileStateBlackboard :: ReconcileState -> Blackboard
reconcileStateBlackboard (ReconcileState _ board _ _) = board

versionWitnessState :: VersionWitness -> ReconcileState
versionWitnessState (VersionWitness state _) = state

versionWitnessRelation :: VersionWitness -> Relation
versionWitnessRelation (VersionWitness _ rel) = rel

resolveBlackboard :: Bitboard -> BitBlip -> Blackboard
resolveBlackboard bitboard bitblip =
  constructBlackboardFromRelation
    (Relation
      (relW16a boardRel) (relW16b blipRel) (relW16c boardRel) (relW16d blipRel)
      (relW16e boardRel) (relW16f blipRel) (relW16g boardRel) (relW16h blipRel)
      (relW32a boardRel) (relW32b blipRel) (relW32c boardRel) (relW32d blipRel))
  where
    boardRel = bitboardWitnessRelation bitboard
    blipRel = bitBlipTransitionRelation bitblip

stageCarrierFace :: CarrierFace -> Blackboard -> Bitboard -> BitBlip -> ReconcileState
stageCarrierFace carrier board bitboard bitblip =
  ReconcileState (MemoryFace carrier (blackboardRelation board)) board bitboard bitblip

witnessReconcileState :: ReconcileState -> VersionWitness
witnessReconcileState state@(ReconcileState face board bitboard bitblip) =
  VersionWitness state
    (Relation
      (relW16a faceRel) (relW16b boardRel) (relW16c boardRel) (relW16d blipRel)
      (relW16e faceRel) (relW16f boardRel) (relW16g boardRel) (relW16h blipRel)
      (relW32a boardRel) (relW32b blipRel) (relW32c boardRel) (relW32d faceRel))
  where
    faceRel = memoryFaceRelation face
    boardRel = blackboardRelation board
    blipRel = bitBlipTransitionRelation bitblip
