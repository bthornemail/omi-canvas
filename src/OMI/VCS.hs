{-# LANGUAGE NoImplicitPrelude #-}

module OMI.VCS
  ( VCSRecord
  , recordReceiptedReconciliation
  , vcsParentReceipt
  , vcsScope
  , vcsCarrierFace
  , vcsBitboardWitness
  , vcsBitBlipTransition
  , vcsBlackboardFace
  , vcsResult
  ) where

import OMI.Core
import OMI.Memory
import OMI.Pipeline
import OMI.Reconcile
import OMI.Scope

data VCSRecord = VCSRecord
  Receipt
  OmiScope
  CarrierFace
  Bitboard
  BitBlip
  Blackboard
  AcceptedReconciliation

recordReceiptedReconciliation ::
  Receipt ->
  OmiScope ->
  CarrierFace ->
  Bitboard ->
  BitBlip ->
  Blackboard ->
  AcceptedReconciliation ->
  VCSRecord
recordReceiptedReconciliation =
  VCSRecord

vcsParentReceipt :: VCSRecord -> Receipt
vcsParentReceipt (VCSRecord receipt _ _ _ _ _ _) = receipt

vcsScope :: VCSRecord -> OmiScope
vcsScope (VCSRecord _ scope _ _ _ _ _) = scope

vcsCarrierFace :: VCSRecord -> CarrierFace
vcsCarrierFace (VCSRecord _ _ carrier _ _ _ _) = carrier

vcsBitboardWitness :: VCSRecord -> Bitboard
vcsBitboardWitness (VCSRecord _ _ _ bitboard _ _ _) = bitboard

vcsBitBlipTransition :: VCSRecord -> BitBlip
vcsBitBlipTransition (VCSRecord _ _ _ _ bitblip _ _) = bitblip

vcsBlackboardFace :: VCSRecord -> Blackboard
vcsBlackboardFace (VCSRecord _ _ _ _ _ board _) = board

vcsResult :: VCSRecord -> AcceptedReconciliation
vcsResult (VCSRecord _ _ _ _ _ _ result) = result
