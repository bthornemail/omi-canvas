# AGENTS

This repository is the Haskell OMI Canvas type-engine workspace.

Treat `omi-canvas` as the typed projection and construction-pipeline layer. Do not turn Canvas into an authority layer.

## Ground Rules

- Keep this repository scoped to Haskell type-engine work unless explicitly told otherwise.
- Do not modify `/home/main/omi/omi-isa` or `/home/main/omi/omi-axioms` from this repo task.
- Preserve `NoImplicitPrelude` in existing OMI modules unless a change explicitly requires otherwise.
- Preserve the current package verification path:

```sh
cabal build all
cabal test all
ghc -isrc -fforce-recomp -fno-code app/Main.hs
```

- Keep `dist-newstyle/`, `.hi`, and `.o` artifacts out of git.

## Semantic Contract

The canonical module flow is:

```text
Gauge -> Wittgenstein -> TruthGate -> DecisionTable -> Karnaugh -> Pipeline -> Canvas
```

Maintain these boundaries:

- `Gauge` frames interpretation.
- `WittgensteinOperator` supplies the local truth alphabet.
- `TruthGate` classifies.
- `DecisionTable` declares a relation surface.
- `KarnaughMap` reduces that surface.
- `Pipeline` composes the construction path to attestation.
- `Canvas` projects accepted structure and does not accept structure.
- `Markdown` discovers evidence and declarations; it does not accept structure.
- `Canvas.JSON` displays accepted projection output; it does not validate, receipt, or attest.
- `Memory` and carrier faces stage witnesses; they do not accept.
- `VCS` records receipted reconciliation only.
- `Carrier` models prefix + CAR frame + CDR frame + unary register; it does not model a rich semantic object.
- `Stream`, `Net.Frame`, `Gossip.Types`, and `Runtime` are adapter shells only.

## API Rules

- Hide constructors at authority boundaries.
- Expose canonical stage functions rather than raw constructors when direct construction would bypass the pipeline.
- Public types should reduce to OMI-defined kernel/domain values or typed projection wrappers.
- New core types should be built from local OMI definitions such as `Bit`, `Nibble`, `Byte`, `Word16`, `Word32`, `Relation`, `SExpr`, and lists of those.
- Do not introduce JSON/Text/Map-heavy external domain models into the core pipeline.
- Keep adapter-only host types, such as `String` or JSON-shaped values, outside the v0 core modules.
- Do not use hashes, checksums, digests, or signatures as protocol identity. They may appear only as external evidence metadata.
- Do not introduce `OmiFragment`-style bundles with checksum, version vector, Reed-Solomon params, fragment metadata, or optional attestation as protocol identity.
- Treat archive Runtime, Net, Gossip, Snapshot, Scheduler, Store, and Server modules as references only unless explicitly asked to build an adapter around `OMI.Carrier`.

## Current Checkpoint

```text
OMI Canvas Type Engine v0
a97d66f Add Haskell OMI canvas type engine
```

This checkpoint establishes the typed API boundary. Future work should extend behavior without weakening constructor hiding or Canvas non-authority.

The current adapter milestone adds Markdown evidence extraction, JSON Canvas projection, bitboard/bit-blip memory witnessing, carrier faces, reconciliation, and VCS recording without making any of those layers authoritative.

The unary carrier milestone adds the lock:

```text
No heavy OmiFragment.
Use prefix + CAR frame + CDR frame + unary register.
Shift advances stream.
XOR witnesses difference.
Composition carries meaning.
Receipt accepts.
```
