# CLI Entry Points

Three separate `Main.hs` files provide command-line interfaces, each
serving a different sub-project.

## app/Main.hs — Snapshot & Canvas CLI (1500+ lines)

**Origin**: JSON Canvas CLI + Mnemonic Manifold CLI + Snapshot tools.

Commands:
- **create** — Create JSON Canvas from CLI arguments
- **view** — Display canvas (text/json/ndjson/dot/svg/png)
- **list** — List/filter/sort nodes and edges
- **export** — Export to markdown/org/csv/html
- **import** — Import from markdown/org/csv/dot
- **stream** — Apply NDJSON event stream to canvas
- **validate** — Validate canvas files
- **query** — Query canvas nodes/edges
- **transform** — Move/resize/recolor/layout nodes
- **stats** — Canvas statistics
- **from-tree** — Directory tree → canvas (from TreeCanvas)
- **watch-tree** — Watch directory for changes, auto-update canvas
- **mnemonic-manifold emit** — Emit Fano-plane canvas events
- **md extract** — Extract NDJSON from Markdown fences
- **snapshot encode/decode** — CSNP binary operations
- **manifest** — Generate build manifest

## app2/Main.hs — ULP Runtime CLI

**Origin**: ULP runtime node (ulp-runtime).

Commands:
- **init** — Initialize ULP storage
- **commit** — Create a new commit
- **validate** — Validate commit log
- **tip** — Show tip commit
- **replay** — Replay/fingerprint commit log
- **merge** — Merge remote commits
- **fingerprint** — Compute deterministic fingerprint
- **gossip** — Gossip network control (join/leave/list peers)
- **config** — View/set configuration
- **server** — Start TCP server
- **control** — Send control commands via Unix socket

## app3/Main.hs — Cryptographic CLI

**Origin**: Mnemonic Manifold + general crypto tools.

Commands:
- **mnemonic generate** — Generate mnemonic phrase
- **mnemonic decode** — Decode mnemonic to bytes
- **sign** — Sign a message
- **verify** — Verify a signature
- **hash** — Compute content hash
- **fano encode/decode** — Fano plane encoding/decoding
- **clock** — Algorithmic clock operations
