#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"

"$SCRIPT_DIR/vector-check.sh"
"$SCRIPT_DIR/reverse-vector-check.sh"
"$SCRIPT_DIR/sig-suite.sh"
"$SCRIPT_DIR/replay-suite.sh"

echo "interop-suite:ok"
