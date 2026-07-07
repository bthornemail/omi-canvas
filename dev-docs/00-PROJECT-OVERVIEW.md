# omi-canvas — Project Overview

This repository is a **conglomeration of files from several distinct projects**,
assembled into a single source tree. The common thread is that all projects
relate to the **omi ecosystem** — a distributed snapshot-synchronization
protocol with a canvas-based visualization layer.

## Origins

The codebase merges the following independent efforts:

| # | Project | Paths | Language | Description |
|---|---------|-------|----------|-------------|
| 1 | **Canonical Snapshot Protocol** | `src/Snapshot/` | Haskell | Binary snapshot format (CSNP/CSPT) with encode/decode, a work scheduler, shard-based routing, section reconciliation, and the Universe VM instruction set |
| 2 | **Universal Ledger Protocol (ULP)** | `src/ULP/` | Haskell | Commit-based ledger with Merkle trees, HMAC signing, NDJSON storage, DAG merge logic, and validation |
| 3 | **Runtime Network Node** | `src/Runtime/` | Haskell | P2P gossip networking over TCP, Unix domain socket control, WAL-based persistent store, config management |
| 4 | **Mnemonic Manifold** | `src/MnemonicManifold/` | Haskell | Fano-plane-based cryptographic encoding: canon triples, pure SHA256, short IDs, JSON text builder |
| 5 | **JSON Canvas EDSL & Desktop** | `src/Desktop/` | Haskell | Embedded DSL for JSON Canvas spec 1.0, directory-tree-to-canvas visualization, Markdown extraction/manifest/verification |
| 6 | **Algorithmic Clock** | `src/AlgorithmicClock.hs` | Haskell | Pure bitwise reference clock implementation |
| 7 | **Bitwise Oracle** | `src/oracle.hs` | Haskell | Binary pattern analysis tool |
| 8 | **Semantic Basis Protocol Stream** | `src/Stream.hs` | Haskell | Chunked message streaming types |
| 9 | **CLI Entry Points** | `app/`, `app2/`, `app3/` | Haskell | Three separate Main.hs files — snapshot CLI, ULP runtime CLI, and cryptographic CLI |
| 10 | **Test Suite** | `test/` | Haskell / JS / Shell | Golden tests, property tests, fuzz tests, cross-runtime JS↔Haskell interop tests |
| 11 | **CI & Scripts** | `scripts/` | Shell | Build/test/fuzz orchestration for CI pipelines |

## Directory Layout

```
omi-canvas/
  app/Main.hs              — snapshot/canvas/mnemonic CLI (1500+ lines)
  app2/Main.hs             — ULP runtime CLI (config, gossip, server)
  app3/Main.hs             — cryptography CLI (sign, hash, Fano, clock)
  src/
    AlgorithmicClock.hs    — Pure bitwise clock
    Stream.hs              — Semantic Basis Protocol streaming
    oracle.hs              — Bitwise oracle
    Desktop/               — Canvas EDSL, TreeCanvas, MD toolchain
    MnemonicManifold/      — Fano-plane encoding, SHA256, canon triples
    Runtime/               — P2P node, gossip, store, config, server
    Snapshot/              — Core snapshot protocol (full sub-tree)
    ULP/                   — Universal Ledger Protocol
  test/                    — 62 files: specs, golden data, shell/JS interop
  scripts/                 — CI build/test/fuzz
```

## Build System

No `.cabal` or `stack.yaml` file exists in-tree. CI scripts reference a
`port-matroid/` subdirectory that is not present in this checkout. The
project targets the Haskell ecosystem (GHC) and would require a Cabal or
Stack project file to build. Some test scripts also depend on Node.js for
cross-runtime validation (`test/*.mjs`).

## Cross-Project Dependencies

```
MnemonicManifold/SHA256.hs      ← used by: MnemonicManifold/*, Desktop/MdManifest, app/Main
MnemonicManifold/Spec.hs        ← used by: MnemonicManifold/Canon, MnemonicManifold/Emit
MnemonicManifold/Ids.hs         ← used by: MnemonicManifold/Emit
MnemonicManifold/JsonText.hs    ← used by: MnemonicManifold/Emit, Desktop/MdManifest
Desktop/CanvasEDSL.hs           ← used by: Desktop/TreeCanvas, app/Main
Snapshot/Types.hs               ← used by: Snapshot/*, Runtime/Node, app/Main
Snapshot/Universe/Core.hs       ← used by: Snapshot/Scheduler/*, Runtime/Node
ULP/Types.hs                    ← used by: ULP/*
```
