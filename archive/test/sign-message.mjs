import crypto from 'node:crypto';

const msg = process.argv[2];
const key = process.env.ULP_TEST_PRIVKEY;
if (!msg) throw new Error('missing message argument');
if (!key) throw new Error('missing ULP_TEST_PRIVKEY');

// Deterministic test-only signer: HMAC-SHA256 over signing message.
const sig = 'hmac:' + crypto.createHmac('sha256', key).update(msg).digest('hex');
process.stdout.write(sig);
