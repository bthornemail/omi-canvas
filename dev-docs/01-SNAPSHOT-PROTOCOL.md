# Canonical Snapshot Protocol

**Origin**: Independent sub-project. The largest component in the repo.

A binary snapshot format and distributed scheduling protocol for
entity-component snapshots with shard-based work distribution.

## Architecture

```
Snapshot (tick + entities)
  ├── Encode/Decode — canonical binary (CSNP/CSPT) with SHA-256 trailer
  ├── Scheduler — cell-based work grouping, round-robin, budget-aware
  │     ├── Core        — scheduleStep algorithm
  │     ├── Encode/Decode — work set binary format
  │     ├── Union      — work set merge with conflict detection
  │     ├── Validate   — cell geometry & touch-set validation
  │     └── Network/   — gossip protocol messages, digest, authority, sim
  ├── Routing — shard-to-peer mapping via SHA-256 scoring
  │     ├── Core        — routeShard, validateContext
  │     ├── Encode/Decode — routing context binary format
  │     └── Types      — RoutingContext, RoutingParams, PeerId
  ├── Reconcile — CSPT section merging into canonical CSNP
  │     ├── Core        — merge logic
  │     └── Types      — Region, ReconcileError
  └── Universe — instruction-set VM for snapshot mutation
        ├── Core        — 6 opcodes (NOP, AdvanceTick, CreateEntity,
        │                 DeleteEntity, SetComponent, RemoveComponent)
        └── Types      — Instruction, Opcode, AuthorityMask, HaltReason
```

## Key Files

| File | Lines | Role |
|------|-------|------|
| `src/Snapshot/Types.hs` | ~120 | Core types: Snapshot, Entity, ComponentMap, Value, Hash |
| `src/Snapshot/Encode.hs` | ~300 | Canonical binary encoder: magic bytes, sorted entities, value tags, size limits, NFC validation, float normalization, trailing SHA-256 |
| `src/Snapshot/Decode.hs` | ~350 | Strict binary decoder: validates ordering, ranges, UTF-8, NFC, floats, hash |
| `src/Snapshot/Limits.hs` | ~25 | maxEntities=1M, maxStringBytes=16MB, maxSnapshotBytes=256MB, maxComponentPairs=1M |
| `src/Snapshot/Errors.hs` | ~80 | DecodeError (~25 constructors), EncodeError (~15 constructors) |
| `src/Snapshot/Scheduler/Core.hs` | ~180 | `scheduleStep`: cell rotation, budget accounting, touch-set conflict avoidance |
| `src/Snapshot/Scheduler/Types.hs` | ~90 | Cell, WorkItem, CanonicalWorkSet, SchedulerParams, ScheduleError |
| `src/Snapshot/Scheduler/Union.hs` | ~40 | Union of two work sets, dedup by WorkId, conflict detection |
| `src/Snapshot/Scheduler/Validate.hs` | ~90 | Cell geometry, no-overlap, no-dup-ID, touch-set range check |
| `src/Snapshot/Scheduler/Encode.hs` | ~35 | Work set binary encoder |
| `src/Snapshot/Scheduler/Decode.hs` | ~45 | Work set binary decoder |
| `src/Snapshot/Scheduler/Network/Types.hs` | ~50 | MessageType (4 kinds), NetError, PeerId |
| `src/Snapshot/Scheduler/Network/Encode.hs` | ~25 | 2-byte LE type tag + payload |
| `src/Snapshot/Scheduler/Network/Decode.hs` | ~25 | Type tag dispatch |
| `src/Snapshot/Scheduler/Network/Digest.hs` | ~70 | Canonical work digest build/decode |
| `src/Snapshot/Scheduler/Network/Authority.hs` | ~30 | Replica digest authority verification |
| `src/Snapshot/Scheduler/Network/Epoch.hs` | ~30 | Epoch adoption (monotonic advancement) |
| `src/Snapshot/Scheduler/Network/State.hs` | ~50 | Node state machine for protocol messages |
| `src/Snapshot/Scheduler/Network/Sim.hs` | ~80 | Convergence simulator: SimNode, stepSim, convergeSteps, expectedUnion |
| `src/Snapshot/Routing/Core.hs` | ~40 | routeShard: SHA-256(salt\|peer\|shard) scoring, top-N replicas |
| `src/Snapshot/Routing/Encode.hs` | ~30 | Routing context binary encoder |
| `src/Snapshot/Routing/Decode.hs` | ~40 | Routing context binary decoder |
| `src/Snapshot/Routing/Types.hs` | ~40 | RoutingParams, RoutingContext, RoutingError |
| `src/Snapshot/Reconcile/Core.hs` | ~60 | Merge CSPT sections into CSNP, region compatibility checks |
| `src/Snapshot/Reconcile/Types.hs` | ~25 | Region, ReconcileError |
| `src/Snapshot/Universe/Core.hs` | ~540 | VM core: instruction encode/decode, stream encode/decode, step function, entity CRUD, authority checks, value validation |
| `src/Snapshot/Universe/Types.hs` | ~60 | Instruction, Opcode, AuthorityMask, Result, HaltReason (12 codes) |
