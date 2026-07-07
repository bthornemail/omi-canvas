import fs from 'node:fs/promises';
import crypto from 'node:crypto';

const fixturePath = new URL('./vectors/commit1.json', import.meta.url);
const outPath = new URL('./vectors/signed-js.ndjson', import.meta.url);

const key = process.env.ULP_TEST_PRIVKEY;
if (!key) throw new Error('missing ULP_TEST_PRIVKEY');

const commit = JSON.parse(await fs.readFile(fixturePath, 'utf8'));
const msg = commit?.merkle?.root || commit?.self_hash;
if (!msg) throw new Error('fixture missing signing message');

commit.sig = 'hmac:' + crypto.createHmac('sha256', key).update(msg).digest('hex');
await fs.writeFile(outPath, JSON.stringify(commit) + '\n');

console.log(JSON.stringify({ expected_address: key, out: outPath.pathname }));
