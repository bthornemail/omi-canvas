# OMI Canvas Type Engine v0

`omi-canvas` is the Haskell type engine for discovering OMI declarations, reducing them through the canonical pipeline, witnessing memory reconciliation, and projecting accepted relations onto canvas surfaces.

This repository does not make Canvas authoritative. Canvas is a projection layer over the canonical construction pipeline:

```text
Declaration
  -> Citation
  -> Gauge
  -> WittgensteinOperator
  -> TruthGate
  -> DecisionTable
  -> KarnaughMap
  -> Combinator
  -> Delta
  -> Blackboard
  -> ProjectionFace
  -> Attestation
```

The milestone anchor is:

```text
a97d66f Add Haskell OMI canvas type engine
```

## Public API Boundary

The exported module chain is:

```text
OMI.Gauge
  -> OMI.Wittgenstein
  -> OMI.TruthGate
  -> OMI.DecisionTable
  -> OMI.Karnaugh
  -> OMI.Pipeline
  -> OMI.Canvas
```

Adapter and witness layers sit around that chain:

```text
OMI.Markdown -> OMI.Lisp declarations
OMI.Memory -> OMI.Reconcile -> OMI.VCS
OMI.Carrier -> OMI.Stream / OMI.Net.Frame / OMI.Gossip.Types / OMI.Runtime
OMI.Canvas.JSON <- OMI.Canvas / OMI.Pipeline output
```

The API enforces the doctrine:

- constructors are hidden at authority boundaries
- callers cannot forge `Attestation` or `ProjectionFace`
- `Canvas` projects relation structure but does not accept it
- `TruthGate`, `DecisionTable`, and `KarnaughMap` classify, declare, and reduce
- `OMI.Pipeline` is the public path to the attestation boundary

## Core Modules

- `OMI.Kernel` defines the minimal OMI kernel values: `Null`, `Bit`, `Pair`, `Nibble`, `Byte`, `Word16`, `Word32`, and `Relation`.
- `OMI.Core` defines canonical null values and relation wrappers.
- `OMI.Lisp` parses OMI-Lisp declarations and preserves the existing decision-table S-expression shape.
- `OMI.Gauge` validates the F* gauge family and canonical pre-header.
- `OMI.Wittgenstein` maps gauge low nibbles onto the 16 local truth operators.
- `OMI.TruthGate` classifies declarations against gate families.
- `OMI.DecisionTable` exposes typed decision-table extraction.
- `OMI.Karnaugh` reduces decision tables into typed Karnaugh regions.
- `OMI.Pipeline` composes the canonical stage functions.
- `OMI.Canvas` exposes typed projection nodes, edges, and faces.
- `OMI.Carrier` models the minimal unary carrier fragment: carrier prefix, CAR addressed frame, CDR addressed frame, and active unary register.
- `OMI.Markdown` extracts front matter, OMI-Lisp fences, decision-table fences, canvas fences, evidence spans, and FS/GS/RS/US scope metadata. It does not accept.
- `OMI.Canvas.JSON` encodes accepted projection faces into JSON Canvas-shaped output. It does not validate or receipt.
- `OMI.Scope` models FS/GS/RS/US scope as an OMI relation.
- `OMI.Memory` models bitboards, bit-blips, blackboard faces, carrier faces, reconcile state, and version witnesses.
- `OMI.Reconcile` builds the reconciliation path from bitboard/bit-blip witnesses to attestation.
- `OMI.VCS` records only accepted reconciliation values that already have a receipt boundary.
- `OMI.Stream`, `OMI.Net.Frame`, `OMI.Gossip.Types`, and `OMI.Runtime` are pure adapter shells for stream/runtime concepts (historical archive at `/home/main/omi/omi-types/archive/`). They do not import runtime IO, snapshot hash authority, WAL authority, or storage authority.

## Verification

Run the package path first:

```sh
cabal build all
cabal test all
```

Keep the direct GHC regression check green:

```sh
ghc -isrc -fforce-recomp -fno-code app/Main.hs
```

## Repository Boundaries

- Runtime C, firmware, and ISA behavior remain in `/home/main/omi/omi-isa`.
- Coq proofs remain in `/home/main/omi/omi-axioms`.
- This repo mirrors the proven concepts as Haskell types and total stage functions.
- JSON Canvas is adapter output only; it is not an authority surface.
- Hashes/checksums used by adapters are evidence metadata only. OMI identity remains the addressed place-value relation, not a digest.
- There is no heavy `OmiFragment` schema. Carrier exchange is prefix + CAR frame + CDR frame + unary register; composition carries meaning and receipt accepts.

## Polytope Toolbox

The polytope registry (moved to `/home/main/omi/omi-types/archive/polytope/`) provides 160 polytope templates and 4 OMI configuration witnesses as projection data. The registry is not authority — rendering is projection only.

- Haskell types and JSON loaders: `omi-types/archive/polytope/OmiCanvasPolytopeRegistry.hs`
- Obsidian/Canvas toolbox: `omi-types/archive/polytope/omi-polytope-toolbox.canvas`
- Normalized template JSON: `omi-types/archive/polytope/omi-polytope-toolbox.normalized.json`

Usage flow:

```text
registry JSON
  → Haskell Registry type (omi-types/archive/polytope/)
  → filter/group by dimension/category
  → render as toolbox cards
  → clone/drag into canvas as snap-to-grid templates
  → attach accepted OMI relation metadata only after validation
```


