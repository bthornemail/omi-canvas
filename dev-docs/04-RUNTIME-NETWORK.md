# Runtime Network Node

**Origin**: Independent sub-project. A peer-to-peer runtime node with
gossip-based state synchronization, TCP networking, WAL persistence, and
Unix domain socket control.

## Architecture

```
Runtime/
  ├── Config.hs       — Config type, defaultConfig, loadConfig (key=value)
  ├── Control.hs      — Unix domain socket: status, dump-snapshot commands
  ├── Log.hs          — Structured logging (text/JSON, level filtering)
  ├── Node.hs         — NodeState, handleMessage, tickOnce (schedule → apply → WAL)
  ├── Server.hs       — TCP server: accept loop, connection limit, message dispatch
  ├── Store.hs        — Persistent storage: snapshot, WAL (PMWAL format with CRC32),
  │                      manifest, rotate, replay
  └── Net/
        ├── Framing.hs    — Length-prefixed TCP framing (4-byte LE header)
        ├── Gossip.hs     — Gossip protocol: mkSummary, decidePull, handlePullReq,
        │                    applyPullSnap, applyPullWal
        └── Gossip/Types.hs — NodeId, Summary, Msg (MHello, MPullReq, etc.),
                               NackCode, encodeMsg/decodeMsg
```

## Key Concepts

- **WAL (Write-Ahead Log)**: `PMWAL` + version header, length-prefixed CRC32
  entries, generation-based rotation every 1000 entries
- **Gossip**: Pull-based state sync — nodes exchange summaries (snapshot
  hash, WAL size, generation, epoch), pull snapshots or WAL chunks as needed
- **Control Socket**: Unix socket for live administration (status queries,
  snapshot dumps)
- **Node Tick**: `tickOnce` schedules work from the work set, applies
  instructions to the snapshot, appends to WAL, rotates periodically

## Key Files

| File | Lines | Role |
|------|-------|------|
| `src/Runtime/Config.hs` | ~90 | Configuration parsing |
| `src/Runtime/Control.hs` | ~70 | Unix control socket commands |
| `src/Runtime/Log.hs` | ~30 | Logging infrastructure |
| `src/Runtime/Node.hs` | ~90 | Core node state machine |
| `src/Runtime/Server.hs` | ~60 | TCP server loop |
| `src/Runtime/Store.hs` | ~380 | WAL/snapshot persistence layer |
| `src/Runtime/Net/Framing.hs` | ~40 | TCP framing protocol |
| `src/Runtime/Net/Gossip.hs` | ~280 | Gossip sync protocol |
| `src/Runtime/Net/Gossip/Types.hs` | ~70 | Gossip message types |
