# OMI Polyform Carrier Layer

40-bit codepoint transport, barcode witnesses, bitboards, bit-blits, and bitmap tiles.

This layer turns a bounded 40-bit carrier handle into deterministic visual and bitmap witness surfaces. It supports barcode-like packet surfaces, bitmap tiles, bitboards, bit-blits, polyform SVGs, and optical transport experiments.

## Authority Boundary

```
This layer does not define OMI identity.
The 40-bit codepoint is a projection handle.
The rendered symbol is a carrier witness.
Validation remains external to rendering.
```

## Frozen 40-bit Layout

```
bits 39..35  basis      (32 values)
bits 34..30  rank       (32 values)
bits 29..25  group      (32 values)
bits 24..20  degree     (32 values)
bits 19..00  path/witness  (1,048,576 selectors)
```

## Project Layout

```
include/
  carrier40.h                 — 40-bit codepoint type, pack/unpack/format/parse
  polyform.h                  — polyform derivation (basis, rank, group, degree, cells)
  polytope_codepoint_bridge.h — bridge from registry metadata to codepoint
  svg.h                       — SVG emitters
src/
  carrier40.c
  polyform.c
  polytope_codepoint_bridge.c
  svg.c
  main.c                      — CLI dispatcher
Makefile
README.md
```

## Build

```sh
make
```

## Generate sample SVGs

```sh
make examples
```

Outputs go to `out/`.

## Toolbox SVGs

```sh
make toolbox-svgs
```

Generates witness surfaces for canonical polytope templates:
- simplex-0d, cube-3d, cross-4d, exceptional-24cell.

## CLI

```sh
./polyform-cli inspect <40-bit-hex>
./polyform-cli polyform-svg <40-bit-hex> <output.svg>
./polyform-cli beetag-svg <identity> <output.svg>
./polyform-cli aztec-svg <40-bit-hex> <output.svg>
./polyform-cli maxicode-svg <40-bit-hex> <output.svg>
./polyform-cli smith-svg <output.svg>
./polyform-cli rods-svg <divisor> <output.svg>
./polyform-cli guess-svg <lo> <hi> <output.svg>
```

## Bridge to Polytope Registry

The polytope registry (JSON) provides template metadata:

```
registry JSON
  → Haskell Registry type (omi-types/archive/polytope/OmiCanvasPolytopeRegistry.hs)
  → derive category, dimension, group, degree
  → polytope_to_codepoint() → 40-bit carrier handle
  → C polyform derivation
  → SVG / barcode / bitmap witness
  → Canvas node template
```

The Haskell type and JSON registry live in `/home/main/omi/omi-types/archive/polytope/`. The `.canvas` toolbox file is at `omi-types/archive/polytope/omi-polytope-toolbox.canvas`.

## Relationship to OMI Proof Suite

The proof suite (omi-axioms) defines admissible structure.
This carrier layer projects witness surfaces from that structure.
Projection is not validation.

```
Proof canonizes admissible structure.
Codepoint is a projection handle, not identity.
Rendering is projection.
Validation determines.
Omi-Attestation witnesses.
Accepted Omi-State may be recorded.
Projection only displays accepted relation state.
```
