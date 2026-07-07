import crypto from 'node:crypto';

const msg = process.argv[2];
const sig = process.argv[3];
const expected = process.argv[4] || '';
if (!msg || !sig) {
  process.stdout.write('false');
  process.exit(0);
}

// Deterministic test-only verifier matching sign-message.mjs.
const expectedSig = 'hmac:' + crypto.createHmac('sha256', expected).update(msg).digest('hex');
process.stdout.write(sig === expectedSig ? 'true' : 'false');
