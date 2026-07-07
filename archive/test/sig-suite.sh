#!/usr/bin/env bash
set -euo pipefail

ROOT="${1:-/tmp/ulp-hs-sig}"
FIXTURE_DIR="$(cd "$(dirname "$0")" && pwd)"

if [[ -z "${ULP_TEST_PRIVKEY:-}" ]]; then
  echo "ULP_TEST_PRIVKEY is required" >&2
  exit 1
fi

mkdir -p "$ROOT"
cd "$FIXTURE_DIR/.."

cabal build exe:ulp-runtime >/dev/null
BIN="$(cabal list-bin exe:ulp-runtime)"

# Golden: JS signs fixture; Haskell verifies through delegated verifier hook.
JS_OUT="$(node "$FIXTURE_DIR/signature-js-golden.mjs")"
EXPECTED_ADDR="$(node -e "const j=JSON.parse(process.argv[1]); process.stdout.write(j.expected_address)" "$JS_OUT")"
cp "$FIXTURE_DIR/vectors/signed-js.ndjson" "$ROOT/log.ndjson"

VAL_OUT="$(ULP_TEST_VERIFIER_JS="$FIXTURE_DIR/verify-signature.mjs" ULP_TEST_EXPECTED_ADDRESS="$EXPECTED_ADDR" "$BIN" validate --root "$ROOT")"
printf '%s\n' "$VAL_OUT"
grep -q 'validated records: 1' <<<"$VAL_OUT"
grep -q 'valid count: 1' <<<"$VAL_OUT"

# N1 signature tamper
node -e "const fs=require('fs');const p='$ROOT/log.ndjson';const o=JSON.parse(fs.readFileSync(p,'utf8').trim());o.sig=o.sig.slice(0,-1)+'0';fs.writeFileSync(p,JSON.stringify(o)+'\\n');"
VAL_TAMPER_SIG="$(ULP_TEST_VERIFIER_JS="$FIXTURE_DIR/verify-signature.mjs" ULP_TEST_EXPECTED_ADDRESS="$EXPECTED_ADDR" "$BIN" validate --root "$ROOT")"
printf '%s\n' "$VAL_TAMPER_SIG"
grep -q 'valid count: 0' <<<"$VAL_TAMPER_SIG"
grep -q 'invalid:sig' <<<"$VAL_TAMPER_SIG"

# restore golden signed fixture
cp "$FIXTURE_DIR/vectors/signed-js.ndjson" "$ROOT/log.ndjson"

# N2 message tamper (merkle/self_hash break)
node -e "const fs=require('fs');const p='$ROOT/log.ndjson';const o=JSON.parse(fs.readFileSync(p,'utf8').trim());o.faces[0].status=o.faces[0].status==='pass'?'fail':'pass';fs.writeFileSync(p,JSON.stringify(o)+'\\n');"
VAL_TAMPER_MSG="$(ULP_TEST_VERIFIER_JS="$FIXTURE_DIR/verify-signature.mjs" ULP_TEST_EXPECTED_ADDRESS="$EXPECTED_ADDR" "$BIN" validate --root "$ROOT")"
printf '%s\n' "$VAL_TAMPER_MSG"
grep -q 'valid count: 0' <<<"$VAL_TAMPER_MSG"
(grep -q 'invalid:merkle' <<<"$VAL_TAMPER_MSG" || grep -q 'invalid:self_hash' <<<"$VAL_TAMPER_MSG")

# N3 wrong expected signer
cp "$FIXTURE_DIR/vectors/signed-js.ndjson" "$ROOT/log.ndjson"
VAL_WRONG_ADDR="$(ULP_TEST_VERIFIER_JS="$FIXTURE_DIR/verify-signature.mjs" ULP_TEST_EXPECTED_ADDRESS="0x2222222222222222222222222222222222222222" "$BIN" validate --root "$ROOT")"
printf '%s\n' "$VAL_WRONG_ADDR"
grep -q 'valid count: 0' <<<"$VAL_WRONG_ADDR"
grep -q 'invalid:sig' <<<"$VAL_WRONG_ADDR"

# N4 missing sig
node -e "const fs=require('fs');const p='$ROOT/log.ndjson';const o=JSON.parse(fs.readFileSync(p,'utf8').trim());o.sig='';fs.writeFileSync(p,JSON.stringify(o)+'\\n');"
VAL_MISSING_SIG="$(ULP_TEST_VERIFIER_JS="$FIXTURE_DIR/verify-signature.mjs" ULP_TEST_EXPECTED_ADDRESS="$EXPECTED_ADDR" "$BIN" validate --root "$ROOT")"
printf '%s\n' "$VAL_MISSING_SIG"
grep -q 'valid count: 0' <<<"$VAL_MISSING_SIG"
grep -q 'invalid:sig' <<<"$VAL_MISSING_SIG"

# Golden reverse direction: Haskell signs produced commit, JS verifies.
rm -rf "$ROOT/hs"
mkdir -p "$ROOT/hs"
ULP_TEST_SIGNER_JS="$FIXTURE_DIR/sign-message.mjs" "$BIN" init --root "$ROOT/hs" >/dev/null
ULP_TEST_SIGNER_JS="$FIXTURE_DIR/sign-message.mjs" "$BIN" commit --root "$ROOT/hs" --type commit >/dev/null
node --input-type=module <<'NODE'
import fs from 'node:fs/promises';
import { validateCommit } from '../../runtime/browser-v1/commit.js';
import crypto from 'node:crypto';
const pk = process.env.ULP_TEST_PRIVKEY;
const txt = await fs.readFile('/tmp/ulp-hs-sig/hs/log.ndjson','utf8');
const c = JSON.parse(txt.trim().split(/\r?\n/).find(Boolean));
const res = await validateCommit(c, null, { signatureVerifier: async (x, msg) => x.sig === ('hmac:' + crypto.createHmac('sha256', pk).update(msg).digest('hex')) });
if (!res.valid) throw new Error('hs->js signature golden failed: '+JSON.stringify(res.errors));
console.log('sig-golden-hs-to-js:ok');
NODE

echo "sig-suite:ok"
