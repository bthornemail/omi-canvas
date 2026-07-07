# Dependency & Cross-Reference Map

## Module Dependency Graph

```
app/Main.hs
  ├── src/Desktop/CanvasEDSL.hs     (canvas types, NDJSON events)
  ├── src/Desktop/TreeCanvas.hs      (from-tree/watch-tree commands)
  ├── src/Desktop/MdExtract.hs       (md extract command)
  ├── src/Desktop/MdManifest.hs      (manifest command)
  ├── src/MnemonicManifold/Canon.hs  (mnemonic-manifold emit)
  ├── src/MnemonicManifold/Emit.hs   (canvas event generation)
  ├── src/MnemonicManifold/Ids.hs    (short hashing)
  ├── src/MnemonicManifold/SHA256.hs (hashing)
  ├── src/MnemonicManifold/Spec.hs   (Fano plane)
  ├── src/Snapshot/*                 (snapshot encode/decode commands)
  └── src/Stream.hs                  (streaming operations)

app2/Main.hs
  ├── src/Runtime/Config.hs          (config management)
  ├── src/Runtime/Control.hs         (control socket)
  ├── src/Runtime/Log.hs             (logging)
  ├── src/Runtime/Node.hs            (node state)
  ├── src/Runtime/Server.hs          (TCP server)
  ├── src/Runtime/Store.hs           (WAL persistence)
  ├── src/Runtime/Net/Framing.hs     (TCP framing)
  ├── src/Runtime/Net/Gossip.hs      (gossip protocol)
  ├── src/Runtime/Net/Gossip/Types.hs
  ├── src/ULP/*                      (ULP commit operations)
  └── src/Snapshot/Scheduler/*       (work scheduling)

app3/Main.hs
  ├── src/MnemonicManifold/SHA256.hs (hashing)
  ├── src/MnemonicManifold/Spec.hs   (Fano plane)
  ├── src/MnemonicManifold/Canon.hs  (canon triple)
  ├── src/MnemonicManifold/JsonText.hs
  └── src/AlgorithmicClock.hs        (clock operations)

src/Snapshot/Universe/Core.hs
  └── src/Snapshot/Universe/Types.hs

src/Snapshot/Routing/Core.hs
  ├── src/Snapshot/Routing/Types.hs
  └── src/MnemonicManifold/SHA256.hs  (hashing for shard scoring)

src/Snapshot/Scheduler/Core.hs
  ├── src/Snapshot/Scheduler/Types.hs
  └── src/Snapshot/Universe/Core.hs   (instruction stream decoding)

src/Snapshot/Scheduler/Network/State.hs
  ├── src/Snapshot/Scheduler/Network/Types.hs
  ├── src/Snapshot/Scheduler/Network/Epoch.hs
  └── src/Snapshot/Routing/Core.hs    (authority verification)

src/Snapshot/Reconcile/Core.hs
  ├── src/Snapshot/Reconcile/Types.hs
  └── src/Snapshot/Encode.hs / Decode.hs  (round-trip canonicalization)

src/Runtime/Node.hs
  ├── src/Snapshot/*                   (types, scheduler, universe)
  ├── src/Runtime/Store.hs
  └── src/Runtime/Log.hs

src/Runtime/Net/Gossip.hs
  ├── src/Runtime/Net/Gossip/Types.hs
  ├── src/Runtime/Net/Framing.hs
  └── src/Runtime/Store.hs
```

## Shared Utilities

| Utility | Used By |
|---------|---------|
| `MnemonicManifold/SHA256.hs` | MnemonicManifold/*, app/Main, app3/Main, Snapshot/Routing, Desktop/MdManifest |
| `MnemonicManifold/JsonText.hs` | MnemonicManifold/Emit, Desktop/MdManifest |
| `Desktop/CanvasEDSL.hs` | Desktop/TreeCanvas, app/Main, test/TestMdExtract |

## Key Type Dependencies

```
Snapshot.Types.Snapshot
  → Snapshot.Encode (CSNP bytes)
  → Snapshot.Decode (CSNP bytes)
  → Snapshot.Scheduler.Core (scheduleStep input)
  → Snapshot.Reconcile.Core (reconciliation target)
  → Runtime.Node (tickOnce mutation target)
  → Runtime.Store (persistence)

ULP.Types.CommitEvent
  → ULP.Canonical (canonicalPayload)
  → ULP.Merkle (computeCommitMerkle)
  → ULP.Validate (validateCommit)
  → ULP.Merge (mergeCommits)
  → ULP.Storage (persistence)
  → ULP.NDJSON (I/O)
  → ULP.Runtime (high-level API)

Desktop.CanvasEDSL.Canvas
  → app/Main (all canvas commands)
  → Desktop.TreeCanvas (tree-to-canvas output)
```

## Build System Clues

- CI scripts reference `port-matroid/` subdirectory (not in tree)
- CI uses `cabal build all --enable-tests` and `cabal test all`
- No `.cabal`, `stack.yaml`, or `cabal.project` at root
- No `package.yaml` (hpack)
- Test scripts build with `cabal build exe:ulp-runtime`
- JS tests import from `../../browser-v1/` (not in tree) — likely a
  sibling directory `browser-v1/` with the JS reference implementation
