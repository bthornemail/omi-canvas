# Mnemonic Manifold

**Origin**: Independent sub-project. A cryptographic encoding system using
Fano-plane projective geometry to map triples (subject-predicate-object)
onto points and lines of the Fano plane — related to the "mnemonic" encoding
scheme referenced in the omi ecosystem.

## Architecture

```
MnemonicManifold/
  ├── SHA256.hs     — Pure Haskell SHA-256 (full digest + Word64 BE)
  ├── Spec.hs       — Fano plane spec: 7 points (3-bit), 7 lines (XOR=0),
  │                    hashS/hashP/hashO (domain-separated hashing),
  │                    closure metrics (sabbath, satisfied lines)
  ├── Canon.hs      — CanonTriple decoder from JSON lines (strict/lenient)
  ├── Brackets.hs   — Balanced bracket depth counting/stripping
  ├── Ids.hs        — shortHashHex16: first 8 bytes of SHA-256 as hex
  ├── Emit.hs       — Canvas event emitter for Fano-plane visualization
  └── JsonText.hs   — Pure-text JSON construction (no aeson dependency)
```

## Key Concepts

- **Fano Plane**: 7 points (001-111), 7 lines of 3 points each where XOR=0
- **Domain Separation**: hashS/hashP/hashO prefix hashing for triple terms
- **Canon Triple**: Parsed JSON line → (subject, predicate, object) with
  source evidence metadata and bracket-reference depths
- **Closure**: A measure of how many Fano lines are "satisfied" by a set of
  triples; sabbath = all 7 lines satisfied
- **Canvas Visualization**: Emit.hs generates NDJSON CanvasEvents showing
  the Fano structure with dynamic per-clause nodes and edges

## Key Files

| File | Lines | Role |
|------|-------|------|
| `src/MnemonicManifold/SHA256.hs` | ~120 | Pure SHA-256: padding, schedule, compression, 64 round constants |
| `src/MnemonicManifold/Spec.hs` | ~100 | Fano plane math: Point, Line, closure, sabbath |
| `src/MnemonicManifold/Canon.hs` | ~180 | Canon triple parser (strict + lenient modes) |
| `src/MnemonicManifold/Emit.hs` | ~300 | Canvas event generation for Fano visualization |
| `src/MnemonicManifold/Ids.hs` | ~30 | Short hash from SHA-256 |
| `src/MnemonicManifold/Brackets.hs` | ~30 | Bracket depth utilities |
| `src/MnemonicManifold/JsonText.hs` | ~60 | Pure-text JSON builder |
