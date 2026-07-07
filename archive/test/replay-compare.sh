#!/usr/bin/env bash
set -euo pipefail

ROOT="${1:-/tmp/ulp-hs-replay-compare}"
FIXTURE_DIR="$(cd "$(dirname "$0")" && pwd)"
IN="$FIXTURE_DIR/vectors/replay-golden.ndjson"
OUT_JS="$ROOT/js-fingerprint.json"
OUT_HS="$ROOT/hs-fingerprint.json"

rm -rf "$ROOT"
mkdir -p "$ROOT"

node "$FIXTURE_DIR/replay-js.mjs" "$IN" "$OUT_JS"
"$FIXTURE_DIR/replay-hs.sh" "$IN" "$OUT_HS" "$ROOT/hs"

HASH_JS="$(node -e "const fs=require('fs');const o=JSON.parse(fs.readFileSync(process.argv[1],'utf8'));process.stdout.write(o.fingerprint_hash)" "$OUT_JS")"
HASH_HS="$(node -e "const fs=require('fs');const o=JSON.parse(fs.readFileSync(process.argv[1],'utf8'));process.stdout.write(o.fingerprint_hash)" "$OUT_HS")"

echo "js_hash=$HASH_JS"
echo "hs_hash=$HASH_HS"

[[ "$HASH_JS" == "$HASH_HS" ]]

echo "replay-compare:ok"
