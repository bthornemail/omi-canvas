# OMI Polytope Toolbox Outputs

Generated from `omi_regular_polytope_registry.json`.

## Files

- `OmiCanvasPolytopeRegistry.hs` — Haskell types and JSON loaders for `omi-canvas`.
- `omi-polytope-toolbox.canvas` — Obsidian/Canvas toolbox with 160 polytope cards and 4 OMI configuration witness cards.
- `omi-polytope-toolbox.normalized.json` — normalized app-facing template list.

## Registry Stats

- Polytopes: 160
- OMI configuration witnesses: 4
- Source schema: `omi.polytope.registry.v0`
- Invariant: This registry is data only. Rendering is projection; receipt/validation remains authority.

## Suggested Use

Load the JSON registry in Haskell, classify templates by dimension/category, and render cards into an omi-canvas toolbox lane.
The `.canvas` file is projection-only: clone/drag cards as scaffolds, then attach accepted OMI relation metadata after validation.
