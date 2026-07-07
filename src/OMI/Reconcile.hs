{-# LANGUAGE NoImplicitPrelude #-}

module OMI.Reconcile
  ( AcceptedReconciliation
  , buildBlackboardFace
  , reconcile
  , witnessVersion
  , attestReconciliation
  , acceptReconciliation
  , acceptedWitness
  , acceptedReceipt
  ) where

import OMI.Core
import OMI.Memory
import OMI.Pipeline

data AcceptedReconciliation = AcceptedReconciliation Attestation VersionWitness Receipt

buildBlackboardFace :: Bitboard -> BitBlip -> Blackboard
buildBlackboardFace = resolveBlackboard

reconcile :: CarrierFace -> Bitboard -> BitBlip -> ReconcileState
reconcile carrier bitboard bitblip =
  let board = resolveBlackboard bitboard bitblip
  in stageCarrierFace carrier board bitboard bitblip

witnessVersion :: ReconcileState -> VersionWitness
witnessVersion = witnessReconcileState

attestReconciliation :: VersionWitness -> Attestation
attestReconciliation witness =
  attestProjection (projectFace (reconcileStateBlackboard (versionWitnessState witness)))

acceptReconciliation :: Attestation -> VersionWitness -> AcceptedReconciliation
acceptReconciliation att witness =
  AcceptedReconciliation att witness (Receipt (attestationRelation att))

acceptedWitness :: AcceptedReconciliation -> VersionWitness
acceptedWitness (AcceptedReconciliation _ witness _) = witness

acceptedReceipt :: AcceptedReconciliation -> Receipt
acceptedReceipt (AcceptedReconciliation _ _ receipt) = receipt
