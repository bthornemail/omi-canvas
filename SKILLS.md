# SKILLS

This file records repeatable workflows for `omi-canvas`.

## Haskell Type Engine Work

Use when changing the canonical Haskell pipeline modules.

1. Keep the module chain explicit:

```text
OMI.Gauge
  -> OMI.Wittgenstein
  -> OMI.TruthGate
  -> OMI.DecisionTable
  -> OMI.Karnaugh
  -> OMI.Pipeline
  -> OMI.Canvas
```

2. Preserve the semantic boundary:
   - constructors hidden at authority boundaries
   - no forged `Attestation`
   - no forged `ProjectionFace`
   - Canvas projects; it does not accept
   - TruthGate, DecisionTable, and Karnaugh classify, declare, and reduce
   - Pipeline is the attestation boundary

3. Keep core types OMI-native:
   - prefer `Bit`, `Nibble`, `Byte`, `Word16`, `Word32`, `Relation`, `SExpr`
   - define new local OMI types before using them
   - avoid adding external domain models to the core pipeline

4. Verify:

```sh
cabal build all
cabal test all
ghc -isrc -fforce-recomp -fno-code app/Main.hs
```

## Public API Review

Use before committing module-boundary changes.

1. Inspect explicit export lists.
2. Confirm boundary constructors are hidden.
3. Confirm public observers exist where tests or downstream code need read access.
4. Confirm no module outside `OMI.Pipeline` can directly forge acceptance.
5. Confirm `OMI.Canvas` exposes typed projection helpers only.

## OMI-Lisp And Decision Table Work

Use when changing `OMI.Lisp` or decision-table extraction.

1. Preserve `parseBytes :: [Byte] -> [SExpr]`.
2. Preserve the current decision-table S-expression shape.
3. Keep decision-table extraction declarative.
4. Route semantic interpretation through `OMI.DecisionTable`, `OMI.TruthGate`, and `OMI.Karnaugh`.
5. Add tests for malformed or non-decision-table input when changing parser behavior.

## Canvas Projection Work

Use when changing `OMI.Canvas` or `OMI.Canvas.JSON`.

1. Keep `ProjectionFace` typed and non-authoritative.
2. Keep JSON Canvas emission in the adapter boundary, not the core pipeline.
3. Keep node and edge accessors relation-backed.
4. Keep constitutional edge fields explicit:
   - source
   - target
   - citation slots
   - proof polarity
   - operator attribution

## Markdown Adapter Work

Use when changing `OMI.Markdown`.

1. Markdown finds declarations and evidence; it does not accept.
2. Extract front matter, OMI-Lisp fences, decision-table fences, canvas fences, evidence spans, and FS/GS/RS/US scope.
3. Evidence hashes/checksums are tamper metadata only, never citation identity.
4. Route parsed declarations through `OMI.Lisp` and the existing pipeline.

## Memory, Reconcile, And VCS Work

Use when changing `OMI.Memory`, `OMI.Reconcile`, or `OMI.VCS`.

1. Treat bitboards as vertex/place-value witnesses.
2. Treat bit-blips as edge/transition witnesses.
3. Treat carrier faces as staging labels only: FIFO, inode, mmap, eMMC BOOT0, eMMC BOOT1, eMMC SECURE, and eMMC USER.
4. Keep VCS record creation dependent on an accepted reconciliation value.

## Unary Carrier Fragment Work

Use when changing `OMI.Carrier`, `OMI.Stream`, `OMI.Net.Frame`, `OMI.Gossip.Types`, or `OMI.Runtime`.

1. Do not create a heavy `OmiFragment` record.
2. Carrier exchange is only prefix + CAR addressed frame + CDR addressed frame + active unary register.
3. `advanceUnary` is the narrow shift-like operation for stream staging.
4. `xorWitness` is the primary difference/composition witness.
5. Archive runtime, gossip, snapshot, scheduler, and store modules are references only; do not import their hash/WAL/snapshot authority into the pure core.

## Cross-Repository Boundary Work

Use when comparing this repo with neighboring OMI repos.

1. Treat `/home/main/omi/omi-isa` as the C/firmware/ISA reference.
2. Treat `/home/main/omi/omi-axioms` as the Coq proof boundary.
3. Do not edit either repo from an `omi-canvas` task unless explicitly requested.
4. Document any discovered mismatch as a follow-up rather than silently changing another repo.
