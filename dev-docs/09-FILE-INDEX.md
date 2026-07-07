# Complete File Index

Every non-git file in the repository, attributed to its origin project.

## Root

| File | Origin Project | Description |
|------|---------------|-------------|
| `.gitignore` | Repo config | Ignores dist-newstyle/, dev-docs/temp.txt |
| `User_Guide.md` | JSON Canvas CLI | User guide for canvas CLI commands |
| `Enhanced CLI with Tree Command Integration.md` | TreeCanvas | Spec for from-tree/watch-tree CLI commands |

## app/ — CLI Entry Points

| File | Origin | Description |
|------|--------|-------------|
| `app/Main.hs` | JSON Canvas CLI + Mnemonic Manifold + Snapshot | Main CLI: canvas ops, mnemonic emit, md extract, snapshot encode/decode, manifest |
| `app2/Main.hs` | ULP Runtime | ULP CLI: init, commit, validate, gossip, server, control |
| `app3/Main.hs` | Mnemonic Manifold + crypto | Cryptographic CLI: mnemonic, sign, hash, fano, clock |

## src/ — Core Library

### Algorithmic Clock (standalone)
| File | Description |
|------|-------------|
| `src/AlgorithmicClock.hs` | Pure bitwise clock implementation |

### Bitwise Oracle (standalone)
| File | Description |
|------|-------------|
| `src/oracle.hs` | Binary pattern analysis tool |

### Semantic Basis Protocol (standalone)
| File | Description |
|------|-------------|
| `src/Stream.hs` | Chunked message streaming types |

### Desktop — JSON Canvas & MD Tooling
| File | Description |
|------|-------------|
| `src/Desktop/CanvasEDSL.hs` | JSON Canvas spec 1.0 EDSL + NDJSON events |
| `src/Desktop/TreeCanvas.hs` | Directory tree → canvas converter |
| `src/Desktop/MdExtract.hs` | Markdown fenced-block NDJSON extraction |
| `src/Desktop/MdManifest.hs` | Build manifest generator |
| `src/Desktop/MdVerifyEvidence.hs` | Evidence verification against source MD |

### MnemonicManifold — Fano-Plane Crypto
| File | Description |
|------|-------------|
| `src/MnemonicManifold/SHA256.hs` | Pure Haskell SHA-256 |
| `src/MnemonicManifold/Spec.hs` | Fano plane mathematical spec |
| `src/MnemonicManifold/Canon.hs` | Canon triple parser |
| `src/MnemonicManifold/Emit.hs` | Fano-plane canvas event emitter |
| `src/MnemonicManifold/Ids.hs` | Short hash (SHA-256 → 16 hex chars) |
| `src/MnemonicManifold/Brackets.hs` | Bracket depth utilities |
| `src/MnemonicManifold/JsonText.hs` | Pure-text JSON construction |

### Runtime — P2P Network Node
| File | Description |
|------|-------------|
| `src/Runtime/Config.hs` | Configuration parsing |
| `src/Runtime/Control.hs` | Unix domain socket control |
| `src/Runtime/Log.hs` | Structured logging |
| `src/Runtime/Node.hs` | Core node state machine |
| `src/Runtime/Server.hs` | TCP server loop |
| `src/Runtime/Store.hs` | WAL + snapshot persistence |
| `src/Runtime/Net/Framing.hs` | Length-prefixed TCP framing |
| `src/Runtime/Net/Gossip.hs` | Gossip sync protocol |
| `src/Runtime/Net/Gossip/Types.hs` | Gossip message types |

### Snapshot — Canonical Snapshot Protocol
| File | Description |
|------|-------------|
| `src/Snapshot/Types.hs` | Core types (Snapshot, Entity, Value) |
| `src/Snapshot/Encode.hs` | Canonical binary encoder |
| `src/Snapshot/Decode.hs` | Strict binary decoder |
| `src/Snapshot/Limits.hs` | Encoding size limits |
| `src/Snapshot/Errors.hs` | Error types (40+ constructors) |
| `src/Snapshot/Routing/Types.hs` | Routing context types |
| `src/Snapshot/Routing/Core.hs` | Shard routing algorithm |
| `src/Snapshot/Routing/Encode.hs` | Routing context encoder |
| `src/Snapshot/Routing/Decode.hs` | Routing context decoder |
| `src/Snapshot/Scheduler/Types.hs` | Scheduler types (Cell, WorkItem, etc.) |
| `src/Snapshot/Scheduler/Core.hs` | scheduleStep algorithm |
| `src/Snapshot/Scheduler/Encode.hs` | Work set encoder |
| `src/Snapshot/Scheduler/Decode.hs` | Work set decoder |
| `src/Snapshot/Scheduler/Validate.hs` | Work set validation |
| `src/Snapshot/Scheduler/Union.hs` | Work set union |
| `src/Snapshot/Scheduler/Network/Types.hs` | Network message types |
| `src/Snapshot/Scheduler/Network/Encode.hs` | Network message encoder |
| `src/Snapshot/Scheduler/Network/Decode.hs` | Network message decoder |
| `src/Snapshot/Scheduler/Network/Digest.hs` | Work digest builder |
| `src/Snapshot/Scheduler/Network/Authority.hs` | Replica authority check |
| `src/Snapshot/Scheduler/Network/Epoch.hs` | Epoch management |
| `src/Snapshot/Scheduler/Network/State.hs` | Protocol state machine |
| `src/Snapshot/Scheduler/Network/Sim.hs` | Convergence simulator |
| `src/Snapshot/Reconcile/Types.hs` | Reconciliation types |
| `src/Snapshot/Reconcile/Core.hs` | Section reconciliation |
| `src/Snapshot/Universe/Types.hs` | Universe VM types |
| `src/Snapshot/Universe/Core.hs` | Universe VM instruction set |

### ULP — Universal Ledger Protocol
| File | Description |
|------|-------------|
| `src/ULP/Types.hs` | All ULP data types |
| `src/ULP/Canonical.hs` | Canonical JSON serialization |
| `src/ULP/Merkle.hs` | Merkle tree computation |
| `src/ULP/Validate.hs` | Commit validation pipeline |
| `src/ULP/Merge.hs` | DAG merge logic |
| `src/ULP/Storage.hs` | Filesystem NDJSON log |
| `src/ULP/NDJSON.hs` | NDJSON I/O |
| `src/ULP/Runtime.hs` | High-level runtime API |

## test/

### Test Data
| File | Description |
|------|-------------|
| `test/replay/basic.before.csnp` | Pre-replay snapshot fixture |
| `test/replay/basic.after.csnp` | Post-replay expected snapshot |
| `test/replay/basic.stream.instrstream` | Instruction stream for replay |
| `test/seam/automaton/events.ndjson` | 5 automaton trace events (advance_tick, create_entity, set_component, remove_component, delete_entity) |
| `test/seam/automaton/expected.hash` | Expected SHA-256 hash of automaton trace |

### Haskell Specs
| File | Category |
|------|----------|
| `test/GoldenSpec.hs` | Golden |
| `test/HaltGoldenSpec.hs` | Golden |
| `test/InstructionGoldenSpec.hs` | Golden |
| `test/InstructionStreamGoldenSpec.hs` | Golden |
| `test/SchedulerGoldenSpec.hs` | Golden |
| `test/SchedulerErrorGoldenSpec.hs` | Golden |
| `test/NetworkDigestGoldenSpec.hs` | Golden |
| `test/RoutingGoldenSpec.hs` | Golden |
| `test/RoutingErrorGoldenSpec.hs` | Golden |
| `test/ReconcileErrorGoldenSpec.hs` | Golden |
| `test/PropertySpec.hs` | Property |
| `test/SchedulerSpec.hs` | Property |
| `test/UnionLawSpec.hs` | Property |
| `test/ConvergenceSimSpec.hs` | Property |
| `test/StorageFuzzSpec.hs` | Fuzz |
| `test/NetworkFuzzSpec.hs` | Fuzz |
| `test/UniverseSpec.hs` | Unit |
| `test/NetworkStateSpec.hs` | Unit |
| `test/NetworkEpochSpec.hs` | Unit |
| `test/NetworkAuthoritySpec.hs` | Unit |
| `test/NetworkBadSpec.hs` | Unit |
| `test/RoutingBadSpec.hs` | Unit |
| `test/ReconcileSpec.hs` | Unit |
| `test/NormalizationSpec.hs` | Unit |
| `test/ConvergenceSpec.hs` | Integration |
| `test/ReplaySpec.hs` | Unit |
| `test/TestMdExtract.hs` | Unit |
| `test/TestMnemonicManifold.hs` | Unit |

### Shell Scripts
| File | Description |
|------|-------------|
| `test/cli-smoke.sh` | ULP CLI smoke test |
| `test/interop-suite.sh` | Run all interop checks |
| `test/vector-check.sh` | Check commit1.json against JS + HS |
| `test/reverse-vector-check.sh` | HS → JS validation |
| `test/replay-suite.sh` | Replay golden + tamper tests |
| `test/replay-compare.sh` | JS vs HS replay fingerprint comparison |
| `test/replay-hs.sh` | Haskell replay helper |
| `test/sig-suite.sh` | Full signature round-trip tests |
| `test/thin-slice-demo.sh` | Snapshot deterministic hash demo |

### JavaScript Modules
| File | Description |
|------|-------------|
| `test/vector-validate.mjs` | JS-side commit validation |
| `test/hs-to-js-validate.mjs` | Validate HS commit in JS |
| `test/replay-js.mjs` | JS-side replay |
| `test/sign-message.mjs` | HMAC test signer |
| `test/signature-js-golden.mjs` | JS golden signature generator |
| `test/verify-signature.mjs` | HMAC test verifier |

## scripts/

| File | Description |
|------|-------------|
| `scripts/ci/build.sh` | cabal build all --enable-tests |
| `scripts/ci/test.sh` | cabal test all |
| `scripts/ci/fuzz_network_fast.sh` | Quick network fuzz (CI) |
| `scripts/ci/fuzz_network_nightly.sh` | Extended network fuzz (nightly) |
| `scripts/ci/fuzz_storage_fast.sh` | Quick storage fuzz (CI) |
| `scripts/ci/fuzz_storage_nightly.sh` | Extended storage fuzz (nightly) |

## Cross-Reference: Which Files Share a Common Origin

| Origin Project | Files |
|----------------|-------|
| **JSON Canvas CLI** | `app/Main.hs`, `src/Desktop/CanvasEDSL.hs`, `User_Guide.md` |
| **TreeCanvas** | `src/Desktop/TreeCanvas.hs`, `Enhanced CLI with Tree Command Integration.md` |
| **Markdown Toolchain** | `src/Desktop/MdExtract.hs`, `src/Desktop/MdManifest.hs`, `src/Desktop/MdVerifyEvidence.hs`, `test/TestMdExtract.hs`, `test/vectors/md-*.md`, `test/vectors/md-*.golden.*` |
| **Mnemonic Manifold** | `app3/Main.hs`, `src/MnemonicManifold/*`, `test/TestMnemonicManifold.hs`, `test/vectors/mnemonic-*.golden.ndjson`, `test/vectors/spo-mini.*` |
| **Snapshot Protocol** | `app/Main.hs` (snapshot cmds), `src/Snapshot/*`, most of `test/*Spec.hs`, all `test/golden-*/`, `test/bad*/`, `test/convergence/`, `test/replay/` |
| **ULP Runtime** | `app2/Main.hs`, `src/ULP/*`, `src/Runtime/*`, `test/cli-smoke.sh`, `test/vector-check.sh`, `test/reverse-vector-check.sh`, `test/replay-suite.sh`, `test/replay-compare.sh`, `test/replay-hs.sh`, `test/sig-suite.sh`, `test/*.mjs`, `test/vectors/commit1.json`, `test/vectors/replay-golden.ndjson`, `test/vectors/signed-js.ndjson` |
| **Algorithmic Clock** | `src/AlgorithmicClock.hs`, part of `app3/Main.hs` |
| **Bitwise Oracle** | `src/oracle.hs` |
| **Semantic Basis Stream** | `src/Stream.hs`, part of `app/Main.hs` |
