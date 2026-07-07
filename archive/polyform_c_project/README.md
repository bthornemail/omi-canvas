# Polyform Barcode Trinity in C

This is a C starter project that turns your notes into a runnable library/CLI for:

- a 40-bit virtual codepoint space
- polyform derivation from that codepoint
- BEEtag-style 5x5 packet rendering
- Aztec-like spiral witness rendering
- MaxiCode-like bullseye + hex-grid witness rendering
- SVG emitters for Smith charts, Genaille-style rods, and a binary-guess SMIL demo

## What is implemented

### 1. Codepoint core

A 40-bit codepoint is represented as 8 groups of 5 bits. That follows the `Codepoint40` idea in your notes and gives a stable canonical handle for projection and witness generation. The project exposes conversion both to/from a 64-bit integer and to a 5-byte layout.

### 2. Polyform derivation

A `pf_polyform_t` carries:

- `basis`
- `rank`
- `group`
- `degree`
- explicit cell coordinates

The derivation is deterministic from the 40-bit codepoint. The current mapping is a practical starter, not a final constitutional mapping.

### 3. BEEtag packet surface

The BEEtag paper describes a 25-bit tag matrix with a 15-bit identity and a 10-bit error check, surrounded by white and black borders. It also constrains usable tags by orientation uniqueness and Hamming distance. This project implements the 15-bit identity + 10-bit parity-style construction and renders the 5x5 code matrix as SVG. See the paper sections on tag design and error checking for the original structure. fileciteturn1file3

### 4. Aztec-like witness surface

The uploaded Aztec material describes the core ideas of an Aztec encoder: a central bullseye, mode message around the core, bit stuffing, padding to codeword boundaries, Reed-Solomon check words, and spiral message placement around the center. This starter project only implements the core visual idea and a spiral placement for a 40-bit payload. It does **not** yet implement the full standard encoder, Reed-Solomon, or mode-word logic. The intended next step is to replace the simplified spiral with a standards-accurate layout. The notes in `Barcode Trinity.txt` also emphasize the 40-bit codepoint as the fixed state projected into the Aztec witness. fileciteturn1file8 fileciteturn1file9

### 5. MaxiCode-like witness surface

The MaxiCode patent material describes a central acquisition target plus a hexagonal cell field on three equally spaced axes, with geometry recoverable from the transform domain and a preference for central placement of high-priority data near the acquisition target. This project renders a simplified bullseye plus hex-grid witness inspired by that geometry. It is a witness surface, not a standards-accurate UPS MaxiCode encoder. fileciteturn1file11 fileciteturn1file12

### 6. SWAR / Omicron bridge

Your Haskell file defines a 16-register 64-bit SWAR machine, with byte-lane operations and polyomino-oriented instructions like `GNOMON`, `CHIRAL`, and `TILE`. This C project is structured so you can later add a direct SWAR runtime under the same codepoint/polyform interface. fileciteturn1file15 fileciteturn1file17

## Build

```sh
make
```

## Generate sample SVGs

```sh
make examples
```

Outputs go to `out/`.

## CLI

```sh
./polyform-cli inspect 123456789a
./polyform-cli polyform-svg 123456789a out/polyform.svg
./polyform-cli beetag-svg 1234 out/beetag.svg
./polyform-cli aztec-svg 123456789a out/aztec_like.svg
./polyform-cli maxicode-svg 123456789a out/maxicode_like.svg
./polyform-cli smith-svg out/smith_chart.svg
./polyform-cli rods-svg 7 out/genaille_division_rods.svg
./polyform-cli guess-svg 1 127 out/binary_guess_number_trick_SMIL.svg
```

## Project layout

```text
include/
  polyform.h
  svg.h
src/
  main.c
  polyform.c
  svg.c
Makefile
README.md
```

## What is still missing

- full Aztec standards encoder/decoder
- Reed-Solomon across GF(16)/GF(64)/GF(256)/GF(1024)/GF(4096)
- full MaxiCode message packing and ECC
- scanning/decoder side for BEEtag/Aztec/MaxiCode
- exact polyform algebra for all requested bases and ranks
- IPC/FIFO runtime bridge
- deterministic ND replay / canonical receipts

## Suggested next step

The cleanest next increment is to freeze one constitutional 40-bit layout:

- 5 bits basis
- 5 bits rank
- 5 bits group
- 5 bits degree
- 20 bits path / growth / witness selector

Then the same payload can drive:

- BEEtag packetization
- Aztec witness
- MaxiCode witness
- 2D SVG
- 2.5D extrusion
- 3D voxel export

That would give you a stable authority boundary before adding standard-compliant ECC and scanners.
