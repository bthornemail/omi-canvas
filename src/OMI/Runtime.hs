{-# LANGUAGE NoImplicitPrelude #-}

module OMI.Runtime
  ( RuntimeCapability(..)
  , RuntimeConfig
  , RuntimeNode
  , mkRuntimeConfig
  , mkRuntimeNode
  , runtimeCapabilities
  , runtimeNodeRelation
  ) where

import OMI.Kernel

data RuntimeCapability =
    RuntimeStream
  | RuntimeNetFrame
  | RuntimeGossip
  | RuntimeLocalStore

data RuntimeConfig = RuntimeConfig [RuntimeCapability]

data RuntimeNode = RuntimeNode Relation RuntimeConfig

mkRuntimeConfig :: [RuntimeCapability] -> RuntimeConfig
mkRuntimeConfig = RuntimeConfig

mkRuntimeNode :: Relation -> RuntimeConfig -> RuntimeNode
mkRuntimeNode = RuntimeNode

runtimeCapabilities :: RuntimeConfig -> [RuntimeCapability]
runtimeCapabilities (RuntimeConfig caps) = caps

runtimeNodeRelation :: RuntimeNode -> Relation
runtimeNodeRelation (RuntimeNode rel _) = rel
