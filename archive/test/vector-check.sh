#!/usr/bin/env bash
set -euo pipefail

ROOT="${1:-/tmp/ulp-hs-vector}"
FIXTURE_DIR="$(cd "$(dirname "$0")" && pwd)"
FIXTURE_JSON="$FIXTURE_DIR/vectors/commit1.json"
FIXTURE_NDJSON="$ROOT/log.ndjson"

rm -rf "$ROOT"
mkdir -p "$ROOT"

# JS-side validation against browser-v1 canonical rules.
node "$FIXTURE_DIR/vector-validate.mjs"

# Haskell-side validation through CLI.
node -e "const fs=require('fs'); const o=JSON.parse(fs.readFileSync('$FIXTURE_JSON','utf8')); fs.writeFileSync('$FIXTURE_NDJSON', JSON.stringify(o)+'\n');"
cd "$FIXTURE_DIR/.."

cabal build exe:ulp-runtime >/dev/null
BIN="$(cabal list-bin exe:ulp-runtime)"
VALIDATE_OUT="$("$BIN" validate --root "$ROOT")"
printf '%s\n' "$VALIDATE_OUT"

grep -q 'validated records: 1' <<<"$VALIDATE_OUT"
grep -q 'valid count: 1' <<<"$VALIDATE_OUT"

echo "vector-check:ok"
