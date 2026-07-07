# JSON Canvas & Desktop Tooling

**Origin**: Independent sub-project(s). An Embedded Domain-Specific Language
(EDSL) for the JSON Canvas spec 1.0, plus directory-tree visualization and
a Markdown extraction/verification toolchain.

## Architecture

```
Desktop/
  ├── CanvasEDSL.hs        — JSON Canvas types (Canvas, Node, Edge, Color)
  │                           + NDJSON event streaming (CanvasEvent)
  ├── TreeCanvas.hs         — Directory tree → JSON Canvas converter
  │                           (vertical/horizontal/radial/indented layouts)
  ├── MdExtract.hs          — Scan .md files → extract fenced NDJSON/JSON
  │                           blocks → per-file + aggregate NDJSON
  ├── MdManifest.hs         — Build manifest (ulp.manifest.v0.2) for
  │                           extraction runs (SHA-256, file stats, git HEAD)
  └── MdVerifyEvidence.hs   — Verify extracted NDJSON records against
                              original Markdown byte spans
```

## Key Concepts

- **JSON Canvas**: Spec 1.0 format for visual diagrams (nodes with x/y/w/h,
  edges with from/to sides and end shapes)
- **CanvasEvent**: NDJSON streamable events (EvAddNode, EvRemoveNode,
  EvAddEdge, EvSnapshot, etc.) for incremental canvas construction
- **TreeCanvas**: 4 layout modes (vertical, horizontal, radial, indented),
  coloring by size/type/depth/age, watch mode for live updates
- **MdExtract**: Extracts fenced code blocks from Markdown, tags them with
  source evidence (byte offsets, line numbers), supports strict/loose parsing
- **MdManifest**: Produces a deterministic build manifest with root hash
- **MdVerifyEvidence**: Re-reads source Markdown to verify extracted records

## Key Files

| File | Lines | Role |
|------|-------|------|
| `src/Desktop/CanvasEDSL.hs` | ~400 | Full JSON Canvas spec 1.0 EDSL + NDJSON streaming |
| `src/Desktop/TreeCanvas.hs` | ~200 | Directory → Canvas converter with 4 layouts |
| `src/Desktop/MdExtract.hs` | ~500 | Markdown fenced-block extraction engine |
| `src/Desktop/MdManifest.hs` | ~250 | Build manifest generation |
| `src/Desktop/MdVerifyEvidence.hs` | ~200 | Post-extraction evidence verification |

## Canvas Event Schema

Events use `ulp.canvas.event.v0.1` schema:
```json
{"schema":"ulp.canvas.event.v0.1","op":"addNode","node":{...}}
{"schema":"ulp.canvas.event.v0.1","op":"addEdge","edge":{...}}
```
