#!/usr/bin/env bash
set -euo pipefail

IN="${1:?input ndjson path required}"
OUT="${2:?output json path required}"
ROOT="${3:-/tmp/ulp-hs-replay}"

cd "$(dirname "$0")/.."
cabal build exe:ulp-runtime >/dev/null
BIN="$(cabal list-bin exe:ulp-runtime)"

"$BIN" init --root "$ROOT" >/dev/null
"$BIN" fingerprint --root "$ROOT" --log "$IN" > "$OUT"

echo "replay-hs:ok"
