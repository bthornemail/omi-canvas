#!/usr/bin/env bash
set -euo pipefail

ROOT="${1:-/tmp/ulp-hs-reverse-vector}"

rm -rf "$ROOT"
mkdir -p "$ROOT"

cd "$(dirname "$0")/.."

cabal build exe:ulp-runtime >/dev/null
BIN="$(cabal list-bin exe:ulp-runtime)"

"$BIN" init --root "$ROOT"
"$BIN" commit --root "$ROOT" --type commit >/dev/null

node "./test/hs-to-js-validate.mjs" "$ROOT/log.ndjson"

echo "reverse-vector-check:ok"
