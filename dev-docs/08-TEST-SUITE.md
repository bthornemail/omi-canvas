# Test Suite

**62 files** across Haskell (Hspec, QuickCheck), JavaScript (Node.js), and
Shell (Bash) — organized by testing strategy.

## Haskell Specs (27 files)

### Golden Tests
| Spec | Tests | Golden Data |
|------|-------|-------------|
| `GoldenSpec.hs` | Snapshot encode/decode round-trip | `test/golden/` (empty.csnp, one-entity.csnp, etc.) |
| `HaltGoldenSpec.hs` | All 12 HaltReason codes | `test/golden-halt/` |
| `InstructionGoldenSpec.hs` | Instruction encoding | `test/golden-instr/` |
| `InstructionStreamGoldenSpec.hs` | Stream encoding | `test/golden-instrstream/` |
| `SchedulerGoldenSpec.hs` | Full schedule → apply → match | `test/golden-schedule/` |
| `SchedulerErrorGoldenSpec.hs` | 6 ScheduleError codes | `test/golden-scheduler-errors/` |
| `NetworkDigestGoldenSpec.hs` | Work digest canonical build/decode | `test/golden-network/` |
| `RoutingGoldenSpec.hs` | Routing context round-trip | `test/golden-routing/` |
| `RoutingErrorGoldenSpec.hs` | Routing error codes | `test/golden-routing-errors/` |
| `ReconcileErrorGoldenSpec.hs` | Reconcile error codes | `test/golden-reconcile/` |

### Property-Based Tests
| Spec | Properties |
|------|------------|
| `PropertySpec.hs` | Snapshot invariants, encoding round-trips |
| `SchedulerSpec.hs` | Permutation determinism, budget monotonicity |
| `UnionLawSpec.hs` | Union commutativity, associativity, scheduling determinism |
| `ConvergenceSimSpec.hs` | Random SimNode clusters → all converge to expected union |
| `StorageFuzzSpec.hs` | Random byte inputs for storage encoding/decoding |

### Unit Tests
| Spec | Code Under Test |
|------|-----------------|
| `UniverseSpec.hs` | Universe VM: opcodes, entity lifecycle, authority, errors |
| `ReconcileSpec.hs` | Section merging, conflict resolution |
| `RoutingBadSpec.hs` | Malformed routing context rejection |
| `NetworkStateSpec.hs` | Node state machine (initState, stepMessage) |
| `NetworkEpochSpec.hs` | Epoch adoption/rejection |
| `NetworkAuthoritySpec.hs` | Replica authority verification |
| `NetworkBadSpec.hs` | Malformed network message rejection |
| `NetworkFuzzSpec.hs` | Random byte message parsing |
| `NormalizationSpec.hs` | Unicode NFC compliance |
| `ConvergenceSpec.hs` | Golden convergence integration test |
| `ReplaySpec.hs` | Instruction replay on snapshots |

### Standalone Haskell Tests
| File | Tests |
|------|-------|
| `TestMdExtract.hs` | NDJSON extraction from Markdown fences |
| `TestMnemonicManifold.hs` | Fano spec, brackets, canon triples, IDs |

## Shell Scripts (8)

| Script | Purpose |
|--------|---------|
| `cli-smoke.sh` | Smoke test: init → commit → validate → tip → replay → merge |
| `vector-check.sh` | Validate commit1.json against both JS and Haskell validators |
| `reverse-vector-check.sh` | Haskell produces commit → JS validates it |
| `replay-suite.sh` | Replay golden NDJSON, test reordering and tamper detection |
| `replay-compare.sh` | JS and Haskell replay must produce matching fingerprints |
| `replay-hs.sh` | Haskell-only replay helper |
| `interop-suite.sh` | Runs all interop checks (vector, sig, replay) |
| `sig-suite.sh` | Full signature round-trip: JS signs → HS validates, HS signs → JS validates, tamper tests |
| `thin-slice-demo.sh` | Snapshot validation + deterministic hash demo |

## JavaScript Modules (.mjs) — 7

| Module | Role |
|--------|------|
| `vector-validate.mjs` | Validates commit1.json against browser-v1 validator |
| `hs-to-js-validate.mjs` | Validates Haskell-produced commit in JS |
| `replay-js.mjs` | JS-side replay: validate, chooseTip, fingerprint |
| `sign-message.mjs` | HMAC-SHA256 test signer |
| `signature-js-golden.mjs` | JS signs fixture → writes signed-js.ndjson |
| `verify-signature.mjs` | HMAC-SHA256 test verifier |
| `verify-signature.mjs` | Verification script (reused by sig-suite) |

## Test Data Directories

| Directory | Contents |
|-----------|----------|
| `test/golden/` | empty.csnp, one-entity.csnp/.cspt, hash files |
| `test/golden-halt/` | malformed, overflow, unauthorized, unknown-opcode .halt |
| `test/golden-instr/` | advance-tick, create-entity, nop, set-component .instr + .hash |
| `test/golden-instrstream/` | basic.instrstream + .hash |
| `test/golden-schedule/` | basic.before/after.csnp, basic.batch.instrstream |
| `test/golden-scheduler-errors/` | 6 .err files |
| `test/golden-network/` | digest-basic, digest-reject-noncanonical, routing-epoch0/1 |
| `test/golden-routing/` | basic.ctx, basic.route |
| `test/golden-routing-errors/` | 5 .err files |
| `test/golden-reconcile/` | 5 .err files (incompatible-region, internal, etc.) |
| `test/convergence/` | before/after.csnp, peer-a/b.workset, collision, dupe-ok, multishard |
| `test/bad/` | 12 invalid .csnp/.cspt files (bad hash, bad magic, dup ID, etc.) |
| `test/bad-instr/` | 4 invalid .instr files |
| `test/bad-instrstream/` | 3 invalid .instrstream files |
| `test/bad-network/` | 3 invalid .msg files |
| `test/bad-routing/` | 7 invalid .ctx files |
| `test/replay/` | basic.before/after.csnp, basic.stream.instrstream |
| `test/seam/automaton/` | events.ndjson (5 automaton trace events) + expected.hash (SHA-256) |
| `test/vectors/` | 15 test vectors: commit1.json, replay-golden.ndjson, signed-js.ndjson, mnemonic-manifold golden, md-extract golden, etc. |

## CI Fuzz Scripts

| Script | Environment Variables | Purpose |
|--------|----------------------|---------|
| `scripts/ci/fuzz_network_fast.sh` | NETWORK_FUZZ_MAX=100, STEPS=50 | Quick CI network fuzz |
| `scripts/ci/fuzz_network_nightly.sh` | NETWORK_FUZZ_MAX=5000, STEPS=500 | Extended nightly network fuzz |
| `scripts/ci/fuzz_storage_fast.sh` | STORAGE_FUZZ_MAX=200, MAX_SMALL=100 | Quick CI storage fuzz |
| `scripts/ci/fuzz_storage_nightly.sh` | STORAGE_FUZZ_MAX=10000, MAX_SMALL=2000 | Extended nightly storage fuzz |
