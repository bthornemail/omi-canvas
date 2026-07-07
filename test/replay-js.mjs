import fs from 'node:fs/promises';
import { parseNdjson } from '../../browser-v1/ndjson.js';
import { validateCommit } from '../../browser-v1/commit.js';
import { chooseTip } from '../../browser-v1/merge.js';
import { stableStringify, sha256Hex } from '../../browser-v1/canonicalize.js';

const inPath = process.argv[2];
const outPath = process.argv[3];
if (!inPath || !outPath) throw new Error('usage: node replay-js.mjs <in.ndjson> <out.json>');

const txt = await fs.readFile(inPath, 'utf8');
const commits = parseNdjson(txt);

let prevValid = null;
const validCommits = [];
let invalidCount = 0;

for (const c of commits) {
  const vr = await validateCommit(c, prevValid, {
    signatureVerifier: async (x) => typeof x.sig === 'string' && x.sig.length > 0,
  });
  if (vr.valid) {
    validCommits.push(c);
    prevValid = c;
  } else {
    invalidCount += 1;
  }
}

const tip = chooseTip(validCommits);
const fp = {
  valid_count: validCommits.length,
  invalid_count: invalidCount,
  tip_self_hash: tip ? tip.self_hash : null,
  tip_merkle_root: tip && tip.merkle ? tip.merkle.root : null,
  stop_metric: tip ? tip.centroid?.stop_metric ?? null : null,
  closure_ratio: tip ? tip.centroid?.closure_ratio ?? null : null,
  sabbath: tip ? tip.centroid?.sabbath ?? null : null,
  faces_status: tip ? (tip.faces || []).map((f) => f.status) : [],
};
fp.fingerprint_hash = await sha256Hex(stableStringify(fp));

await fs.writeFile(outPath, JSON.stringify(fp, null, 2) + '\n');
console.log('replay-js:ok', { fingerprint_hash: fp.fingerprint_hash });
