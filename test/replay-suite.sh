#!/usr/bin/env bash
set -euo pipefail

ROOT="${1:-/tmp/ulp-hs-replay-suite}"
FIXTURE_DIR="$(cd "$(dirname "$0")" && pwd)"
GOLDEN="$FIXTURE_DIR/vectors/replay-golden.ndjson"

rm -rf "$ROOT"
mkdir -p "$ROOT"

# Golden
"$FIXTURE_DIR/replay-compare.sh" "$ROOT/golden"

# R-N1 reorder lines (non-causal reorder should diverge or invalidate)
REORDERED="$ROOT/reordered.ndjson"
node -e "const fs=require('fs');const lines=fs.readFileSync(process.argv[1],'utf8').trim().split(/\\r?\\n/); if(lines.length>1){[lines[0],lines[1]]=[lines[1],lines[0]];} fs.writeFileSync(process.argv[2],lines.join('\\n')+'\\n');" "$GOLDEN" "$REORDERED"
node "$FIXTURE_DIR/replay-js.mjs" "$REORDERED" "$ROOT/reordered-js.json"
"$FIXTURE_DIR/replay-hs.sh" "$REORDERED" "$ROOT/reordered-hs.json" "$ROOT/reordered-hs"
RJ="$(node -e "const fs=require('fs');const o=JSON.parse(fs.readFileSync(process.argv[1],'utf8'));process.stdout.write(String(o.invalid_count))" "$ROOT/reordered-js.json")"
RH="$(node -e "const fs=require('fs');const o=JSON.parse(fs.readFileSync(process.argv[1],'utf8'));process.stdout.write(String(o.invalid_count))" "$ROOT/reordered-hs.json")"
[[ "$RJ" != "0" || "$RH" != "0" ]]

# R-N2 tamper field
TAMPERED="$ROOT/tampered.ndjson"
node -e "const fs=require('fs');const lines=fs.readFileSync(process.argv[1],'utf8').trim().split(/\\r?\\n/); const o=JSON.parse(lines[0]); if(o.faces&&o.faces[0]) o.faces[0].status=(o.faces[0].status==='pass'?'fail':'pass'); lines[0]=JSON.stringify(o); fs.writeFileSync(process.argv[2],lines.join('\\n')+'\\n');" "$GOLDEN" "$TAMPERED"
node "$FIXTURE_DIR/replay-js.mjs" "$TAMPERED" "$ROOT/tampered-js.json"
"$FIXTURE_DIR/replay-hs.sh" "$TAMPERED" "$ROOT/tampered-hs.json" "$ROOT/tampered-hs"
TJ="$(node -e "const fs=require('fs');const o=JSON.parse(fs.readFileSync(process.argv[1],'utf8'));process.stdout.write(String(o.invalid_count))" "$ROOT/tampered-js.json")"
TH="$(node -e "const fs=require('fs');const o=JSON.parse(fs.readFileSync(process.argv[1],'utf8'));process.stdout.write(String(o.invalid_count))" "$ROOT/tampered-hs.json")"
[[ "$TJ" != "0" && "$TH" != "0" ]]

echo "replay-suite:ok"
