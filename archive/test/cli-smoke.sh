#!/usr/bin/env bash
set -euo pipefail

ROOT="${1:-/tmp/ulp-hs-smoke}"
REMOTE="${ROOT}-remote.ndjson"

rm -rf "$ROOT" "$REMOTE"

cabal build exe:ulp-runtime
BIN="$(cabal list-bin exe:ulp-runtime)"

"$BIN" init --root "$ROOT"
"$BIN" commit --root "$ROOT" --type commit
"$BIN" validate --root "$ROOT"
"$BIN" tip --root "$ROOT"
"$BIN" replay --root "$ROOT"

cp "$ROOT/log.ndjson" "$REMOTE"
"$BIN" merge --root "$ROOT" --from "$REMOTE"

echo "cli-smoke:ok"
