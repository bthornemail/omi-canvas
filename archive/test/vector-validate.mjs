import fs from 'node:fs/promises';
import { validateCommit } from '../../browser-v1/commit.js';

const fixturePath = new URL('./vectors/commit1.json', import.meta.url);
const commit = JSON.parse(await fs.readFile(fixturePath, 'utf8'));

const EXPECTED_SELF_HASH = '0x65e7b0c21a089bd11b24ed96350df3be4c620fde94a77d04a8a9235aa952c56f';
const EXPECTED_MERKLE_ROOT = '0xea4bfe2d80688bd4ef047d881af7b2a48c468bb6d98acc4ef4d5634013dd1eed';

if (commit.self_hash !== EXPECTED_SELF_HASH) {
  throw new Error(`fixture self_hash mismatch: ${commit.self_hash}`);
}
if (!commit.merkle || commit.merkle.root !== EXPECTED_MERKLE_ROOT) {
  throw new Error(`fixture merkle.root mismatch: ${commit.merkle && commit.merkle.root}`);
}

const result = await validateCommit(commit, null, {
  signatureVerifier: async (c) => c.sig === 'vector-sig'
});

if (!result.valid) {
  throw new Error(`browser-v1 validator rejected fixture: ${JSON.stringify(result.errors)}`);
}

console.log('vector-js:ok', {
  self_hash: commit.self_hash,
  merkle_root: commit.merkle.root
});
