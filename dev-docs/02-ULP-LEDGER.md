# Universal Ledger Protocol (ULP)

**Origin**: Independent sub-project (possibly "ulp-runtime"). A commit-based
ledger protocol with Merkle tree integrity, HMAC signing, logical clocks,
and DAG-based merge semantics.

## Architecture

```
Runtime (initRuntime, appendRuntimeCommit, validateLog)
  ├── Types        — CommitEvent, ChannelState, CommitType, VertexIdentity,
  │                  EdgeState, FaceInvariant, CentroidState, Merkle, etc.
  ├── Canonical    — stableJson canonical sort, sha256Hex, canonicalPayload
  ├── Merkle       — 6-section Merkle tree (identities, vertex, edges,
  │                  faces, centroid, meta), pairHash, getSigningMessage
  ├── Validate     — sequential commit validation (lc, genesis, self_hash,
  │                  merkle, signature, invariant checker)
  ├── Merge        — DAG merge (dedup, rank computation, sort, chooseTip)
  ├── Storage      — filesystem-backed log.ndjson (load, append, writeLog)
  └── NDJSON       — NDJSON I/O: encodeLine, decodeFile, appendLine
```

## Key Concepts

- **CommitEvent**: Full commit with lc, prev_hash, self_hash, merkle, sig,
  plus typed sections (identities, vertex, edges, faces, centroid, meta)
- **Logical Clock**: Monotonically increasing integer per-node
- **Merkle Tree**: 6 section leaves → binary pair hashing → single root
- **Signing Message**: Merkle root if present, else self_hash
- **Validation**: 6 sequential checks accumulating errors
- **Merge**: DAG-based with rank depth, dedup by hash, sort by (rank, lc, timestamp, cid)

## Key Files

| File | Lines | Role |
|------|-------|------|
| `src/ULP/Types.hs` | ~200 | All ULP data types with JSON instances |
| `src/ULP/Canonical.hs` | ~50 | Canonical JSON serialization, SHA-256 hex |
| `src/ULP/Merkle.hs` | ~60 | Merkle tree computation and validation |
| `src/ULP/Validate.hs` | ~80 | Sequential commit validation pipeline |
| `src/ULP/Merge.hs` | ~70 | DAG merge, dedup, chooseTip |
| `src/ULP/Storage.hs` | ~60 | Filesystem NDJSON log |
| `src/ULP/NDJSON.hs` | ~40 | NDJSON encode/decode |
| `src/ULP/Runtime.hs` | ~60 | High-level runtime: init, append, validate |
