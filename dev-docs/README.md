# omi-canvas — Development Documentation

This directory documents the `omi-canvas` source tree, which is a
**conglomeration of files from multiple independent projects** combined
into a single Haskell codebase.

## Documents

| File | Covers |
|------|--------|
| `00-PROJECT-OVERVIEW.md` | High-level overview, origin table, directory layout |
| `01-SNAPSHOT-PROTOCOL.md` | Canonical Snapshot Protocol (CSNP/CSPT, scheduler, routing, reconcile, Universe VM) |
| `02-ULP-LEDGER.md` | Universal Ledger Protocol (commits, Merkle, validation, merge) |
| `03-MNEMONIC-MANIFOLD.md` | Mnemonic Manifold (Fano plane, SHA-256, canon triples, canvas emit) |
| `04-RUNTIME-NETWORK.md` | Runtime Network Node (gossip, WAL, TCP, control socket) |
| `05-CANVAS-DESKTOP.md` | JSON Canvas EDSL, TreeCanvas, Markdown toolchain |
| `06-APP-ENTRYPOINTS.md` | The 3 CLI entry points and their commands |
| `07-ALGORITHMIC-CLOCK-AND-ORACLE.md` | Algorithmic Clock, bitwise oracle, stream types |
| `08-TEST-SUITE.md` | Test organization: golden, property, fuzz, interop |
| `09-FILE-INDEX.md` | Complete file listing with origin attribution |
| `10-DEPENDENCY-MAP.md` | Module dependency graph, shared utilities, build clues |

## Quick Origin Reference

- **`src/Snapshot/`** — Canonical Snapshot Protocol (largest component, ~27 files)
- **`src/ULP/` + `src/Runtime/`** — ULP ledger + P2P runtime node (~17 files)
- **`src/Desktop/`** — JSON Canvas CLI tooling (~5 files)
- **`src/MnemonicManifold/`** — Fano-plane cryptographic encoding (~7 files)
- **`src/AlgorithmicClock.hs`**, **`src/oracle.hs`**, **`src/Stream.hs`** — Standalone utilities
- **`app/Main.hs`** — Snapshot + Canvas + Mnemonic CLI (1500+ lines)
- **`app2/Main.hs`** — ULP Runtime CLI
- **`app3/Main.hs`** — Cryptographic CLI
- **`test/`** — 62 files across Haskell, JS, and Shell
- **`scripts/ci/`** — 6 CI scripts referencing `port-matroid/` subdirectory
