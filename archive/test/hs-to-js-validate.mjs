import fs from 'node:fs/promises';
import { validateCommit } from '../../browser-v1/commit.js';

const ndjsonPath = process.argv[2];
if (!ndjsonPath) {
  throw new Error('usage: node hs-to-js-validate.mjs <path-to-log.ndjson>');
}

const text = await fs.readFile(ndjsonPath, 'utf8');
const line = text
  .split(/\r?\n/)
  .map((s) => s.trim())
  .find(Boolean);

if (!line) {
  throw new Error('no commit lines found in NDJSON file');
}

const commit = JSON.parse(line);

const result = await validateCommit(commit, null, {
  signatureVerifier: async (c) => typeof c.sig === 'string' && c.sig.length > 0
});

if (!result.valid) {
  throw new Error(`browser validator rejected Haskell-produced commit: ${JSON.stringify(result.errors)}`);
}

if (!commit.self_hash || !commit.merkle || !commit.merkle.root) {
  throw new Error('expected self_hash and merkle.root in Haskell-produced commit');
}

console.log('hs-to-js:ok', {
  id: commit.id,
  self_hash: commit.self_hash,
  merkle_root: commit.merkle.root
});
