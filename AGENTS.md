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

## Canonical OMI Notation

The canonical notation surface is `omi---imo?O_o`.

This notation is not arbitrary. It is derived from the byte table structure
and carries its own derivation history.

### Derivation chain

Byte table:
  0x1F US = hinge point (last hidden unit separator)
  0x2F /  = first printable separator
  0x3F ?  = witness/query separator
  0x4F O  = upper/norm Omicron
  0x5F _  = carrier/floor separator
  0x6F o  = lower/local omicron

The F-column projects tangentially from 0x1F into the printable branch.
?O_o is the readable branch of hidden US.

### 8-tuple

Q Σ L R δ s t r

Q = state space / identity context
Σ = alphabet / allowed symbols
L = left / source / beginning
R = right / target / ending
δ = transition / transformation
s = start value
t = accept / success / terminal output
r = reject / failure / alternative

Address cascade: S0=Q, S1=Σ, S2=L, S3=R, S4=δ, S5=s, S6=t, S7=r

Register compression:
  CAR = Q & Σ
  CDR = L & R
  PAYLOAD = δ & s
  MASK = t & r

### Null Ring

(NULL . NULL) is the folded null seed.

Byte ring:
  0x00 ^ 0x20 = 0x20
  0x20 ^ 0x7F = 0x5F
  0x7F ^ 0xFF = 0x80
  0xFF ^ 0x00 = 0xFF

Full witness closure: 0x20 ^ 0x5F ^ 0x80 ^ 0xFF = 0x00

### Agent rules

- Use omi---imo?O_o as the canonical notation surface
- Do not infer notation from older nearby files
- Do not promote #/ from carrier compatibility into OMI-native syntax
- Do not treat the 8-tuple as 8×16-bit unless discussing later lowering
- Do not treat the four dotted pairs as 4×32-bit integer widths
- Do not treat geometry drawings as authority
- Do not let projection, route, notation, or gauge become authority

### Authority order

Omnicron frames.
Tetragrammatron validates.
Metatron scribes.
Receipt records.

### Canonical source documents

The full derivation lives in omi-canon:
- `dev-docs/The 8-Tuple Basis of \`omi---imo?O_o\`.md`
- `dev-docs/OMI Gauge Table, F-Column Surface, and.md`
- `docs/The 8-Tuple Basis of omi---imo?O_o.md`
