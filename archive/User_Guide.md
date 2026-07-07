# JSON Canvas CLI Tool User Guide

A command-line interface for working with JSON Canvas files (spec 1.0).

## Installation

```bash
git clone https://github.com/yourusername/json-canvas-cli
cd json-canvas-cli
cabal install
```

## Quick Start

Create a simple canvas:
```bash
json-canvas create \
  --node-id "node1" --node-type text --node-x 0 --node-y 0 --node-content "Hello" \
  --node-id "node2" --node-type text --node-x 320 --node-y 0 --node-content "World" \
  --edge-id "edge1" --from node1 --to node2 --from-side right --to-side left \
  --output mycanvas.json
```

View the canvas:
```bash
json-canvas view mycanvas.json
```

## Commands

### `create` - Create a new canvas

Create a canvas from scratch with nodes and edges.

**Options:**
- `-o, --output FILE` - Output file (default: canvas.json)
- `--node-id ID` - Node identifier
- `--node-type TYPE` - Node type: text, file, link, group
- `--node-x X` - X position
- `--node-y Y` - Y position
- `--node-width W` - Width (default: 240)
- `--node-height H` - Height (default: 240)
- `--node-content TEXT` - Content (text/url/file path)
- `--node-color COLOR` - Color (hex #RRGGBB or preset 1-6)
- `--node-label TEXT` - Label (for group nodes)
- `--edge-id ID` - Edge identifier
- `--from NODE` - Source node
- `--to NODE` - Target node
- `--from-side SIDE` - Source side: top, right, bottom, left
- `--to-side SIDE` - Target side
- `--edge-label TEXT` - Edge label
- `--edge-color COLOR` - Edge color
- `--bidirectional` - Use bidirectional arrows

**Examples:**

Create a simple diagram:
```bash
json-canvas create \
  --node-id "idea" --node-type text --node-x 100 --node-y 100 --node-content "Main Idea" \
  --node-id "detail" --node-type text --node-x 500 --node-y 100 --node-content "Details" \
  --edge-id "flow" --from idea --to detail --from-side right --to-side left \
  --output diagram.json
```

Create a group with colored nodes:
```bash
json-canvas create \
  --node-id "group1" --node-type group --node-x 0 --node-y 0 --node-label "Project A" \
  --node-id "task1" --node-type text --node-x 20 --node-y 30 --node-content "Task 1" --node-color "#FF0000" \
  --node-id "task2" --node-type text --node-x 20 --node-y 200 --node-content "Task 2" --node-color "2" \
  --output project.json
```

### `view` - View a canvas

Display a canvas in various formats.

**Options:**
- `FILE` - Input canvas file
- `-f, --format FMT` - Output format: text, json, ndjson, dot, svg, png (default: text)
- `-o, --output FILE` - Output file (default: input filename + extension)

**Examples:**

View as text:
```bash
json-canvas view diagram.json
```

Export to Graphviz DOT format:
```bash
json-canvas view diagram.json -f dot -o diagram.dot
```

Generate SVG:
```bash
json-canvas view diagram.json -f svg -o diagram.svg
```

### `list` - List nodes and edges

List the contents of a canvas with filtering and sorting.

**Options:**
- `FILE` - Input canvas file
- `--type TYPE` - Filter by type: nodes, edges, text, file, link, group
- `--sort FIELD` - Sort by: id, type, x, y, size
- `--filter EXPR` - Filter expression (e.g., "x>100")

**Examples:**

List all nodes:
```bash
json-canvas list diagram.json --type nodes
```

List only text nodes sorted by position:
```bash
json-canvas list diagram.json --type text --sort x
```

### `mnemonic-manifold emit` - Emit Canvas events from NDJSON

Generate an NDJSON stream of `ulp.canvas.event.v0.1` events that includes static Fano point/line nodes plus per-record clause nodes and edges.

```bash
json-canvas mnemonic-manifold emit \
  --in dev-docs/Artifacts/Canon/canon-core-octad.ndjson \
  --out /tmp/mnemonic.canvas.ndjson \
  --emit-static \
  --strict
```

Flags:
- `--emit-static` / `--no-emit-static`: include static Fano scaffolding (default: true)
- `--strict` / `--no-strict`: fail on unknown/invalid input lines (default: true)
- `--centroid`: emit an observer node with derived closure fields

### `md extract` - Extract NDJSON/JSON fences from Markdown

Scan a directory for `.md` files and extract fenced blocks into **pure NDJSON** files under `build/extract/ndjson/` (plus an optional aggregate `all.ndjson`).

```bash
json-canvas md extract \
  --root docs \
  --out build/extract \
  --strict \
  --langs ndjson,jsonl,jsonlines,json,hash
```

Helpful flags:
- `--loose-ndjson`: also parse standalone JSON object lines outside fences (heuristic)
- `--canon-filter`: only emit records that the mnemonic-manifold canon decoder can consume (useful for `--strict` pipelines)

Find large nodes:
```bash
json-canvas list diagram.json --filter "width*height>10000"
```

### `export` - Export to other formats

Export canvas to various document formats.

**Options:**
- `FILE` - Input canvas file
- `-f, --format FMT` - Export format: png, svg, pdf, md, org, html
- `-o, --output FILE` - Output file

**Examples:**

Export to Markdown:
```bash
json-canvas export diagram.json -f md -o diagram.md
```

Export to HTML:
```bash
json-canvas export diagram.json -f html -o diagram.html
```

### `import` - Import from other formats

Import diagrams from other formats.

**Options:**
- `FILE` - Input file
- `-f, --format FMT` - Import format: md, org, csv, dot
- `-o, --output FILE` - Output canvas file

**Examples:**

Import from CSV:
```bash
json-canvas import nodes.csv -f csv -o canvas.json
```

Import from Graphviz DOT:
```bash
json-canvas import diagram.dot -f dot -o canvas.json
```

### `stream` - Process NDJSON event streams

Apply a stream of events to a canvas (useful for incremental updates).

**Options:**
- `-i, --input FILE` - Initial canvas file (optional)
- `-o, --output FILE` - Output canvas file
- `--events FILE` - NDJSON events file
- `--watch` - Watch for file changes and apply continuously

**Examples:**

Apply events to empty canvas:
```bash
json-canvas stream --events updates.ndjson -o result.json
```

Apply events to existing canvas:
```bash
json-canvas stream -i base.json --events changes.ndjson -o updated.json
```

### `validate` - Validate a canvas

Check if a canvas file conforms to the spec.

**Options:**
- `FILE` - Input canvas file
- `--strict` - Strict validation (check for duplicate IDs)

**Examples:**

```bash
json-canvas validate diagram.json
```

### `query` - Query a canvas

Run queries against a canvas.

**Options:**
- `FILE` - Input canvas file
- `-q, --query EXPR` - Query expression
- `-f, --format FMT` - Output format: text, json

**Query expressions:**
- `nodes where type = text` - Find all text nodes
- `nodes with color` - Find colored nodes
- `nodes larger than 10000` - Find nodes with area > 10000
- `edges from node1` - Find edges from node1
- `edges to node2` - Find edges to node2

**Examples:**

```bash
json-canvas query diagram.json -q "nodes where type = text"
json-canvas query diagram.json -q "edges from main" -f json
```

### `transform` - Transform a canvas

Apply transformations to a canvas.

**Options:**
- `FILE` - Input canvas file
- `-o, --output FILE` - Output file

**Transform operations:**

Move nodes:
```bash
json-canvas transform diagram.json -o moved.json move --id node1 --x 100 --y 200
```

Resize nodes:
```bash
json-canvas transform diagram.json -o resized.json resize --id node1 --width 300 --height 200
```

Recolor nodes:
```bash
json-canvas transform diagram.json -o colored.json recolor --id node1 --color "#00FF00"
```

Add edges:
```bash
json-canvas transform diagram.json -o with-edges.json add-edge \
  --edge-id new --from node1 --to node2 --from-side right --to-side left
```

Auto-layout vertically:
```bash
json-canvas transform diagram.json -o layout.json layout vertical --spacing 80 --start-y 0
```

Auto-layout in grid:
```bash
json-canvas transform diagram.json -o layout.json layout grid --cols 3 --cell-size 240
```

Remove nodes and their connected edges:
```bash
json-canvas transform diagram.json -o clean.json remove-nodes node1 node2
```

### `stats` - Show canvas statistics

Display statistics about a canvas.

**Options:**
- `FILE` - Input canvas file
- `--detailed` - Show detailed statistics

**Examples:**

```bash
json-canvas stats diagram.json
json-canvas stats diagram.json --detailed
```

## Examples

### Creating a Project Timeline

```bash
json-canvas create \
  --node-id "start" --node-type text --node-x 100 --node-y 100 --node-content "Start" \
  --node-id "phase1" --node-type group --node-x 300 --node-y 50 --node-label "Phase 1" \
  --node-id "task1" --node-type text --node-x 350 --node-y 80 --node-content "Task 1.1" \
  --node-id "task2" --node-type text --node-x 350 --node-y 200 --node-content "Task 1.2" \
  --node-id "phase2" --node-type group --node-x 600 --node-y 50 --node-label "Phase 2" \
  --node-id "task3" --node-type text --node-x 650 --node-y 80 --node-content "Task 2.1" \
  --edge-id "e1" --from start --to phase1 --from-side right --to-side left \
  --edge-id "e2" --from phase1 --to phase2 --from-side right --to-side left \
  --output timeline.json
```

### Creating a Network Diagram

```bash
json-canvas create \
  --node-id "router" --node-type file --node-x 400 --node-y 200 --node-content "router.png" \
  --node-id "pc1" --node-type text --node-x 100 --node-y 100 --node-content "PC 1" \
  --node-id "pc2" --node-type text --node-x 100 --node-y 300 --node-content "PC 2" \
  --node-id "server" --node-type text --node-x 700 --node-y 200 --node-content "Server" \
  --edge-id "conn1" --from pc1 --to router --from-side right --to-side left \
  --edge-id "conn2" --from pc2 --to router --from-side right --to-side left \
  --edge-id "conn3" --from router --to server --from-side right --to-side left \
  --output network.json
```

### Processing an Event Stream

Create an events file `events.ndjson`:
```json
{"schema":"ulp.canvas.event.v0.1","op":"addNode","node":{"id":"node1","type":"text","x":0,"y":0,"width":240,"height":240,"text":"Hello"}}
{"schema":"ulp.canvas.event.v0.1","op":"addNode","node":{"id":"node2","type":"text","x":320,"y":0,"width":240,"height":240,"text":"World"}}
{"schema":"ulp.canvas.event.v0.1","op":"addEdge","edge":{"id":"edge1","fromNode":"node1","toNode":"node2","fromSide":"right","toSide":"left","toEnd":"arrow"}}
```

Apply the events:
```bash
json-canvas stream --events events.ndjson -o result.json
```

## Tips and Tricks

1. **Batch processing with scripts**: Combine commands with shell scripts for batch processing multiple canvases.

2. **Pipeline integration**: Use with `jq` for advanced JSON processing:
   ```bash
   json-canvas view diagram.json -f json | jq '.nodes[] | select(.type=="text")'
   ```

3. **Version control**: Store canvases in Git and use the CLI to generate human-readable diffs:
   ```bash
   json-canvas view old.json > old.txt
   json-canvas view new.json > new.txt
   diff old.txt new.txt
   ```

4. **Continuous integration**: Validate canvases in CI pipelines:
   ```bash
   json-canvas validate diagram.json --strict
   ```

5. **Template system**: Create base canvases and transform them for different outputs:
   ```bash
   json-canvas transform base.json -o variant1.json move --id logo --x 100 --y 200
   ```

## Troubleshooting

**"Invalid JSON Canvas file"** - The file is not valid JSON or doesn't conform to the canvas spec. Use `validate` to get detailed errors.

**Missing dependencies for PNG export** - Install graphviz and cairo: `brew install graphviz cairo` (macOS) or `apt-get install graphviz libcairo2-dev` (Linux)

**Large files** - For very large canvases, use NDJSON streaming for incremental processing rather than loading the entire file.

## License

MIT License - See LICENSE file for details.
```

This comprehensive CLI tool and user guide provides a full-featured interface for working with JSON Canvas files, including creation, viewing, transformation, import/export, streaming, and validation capabilities.
