# ZK System Setup Guide

## Installation

### 1. Install o1js

```bash
npm install o1js
```

**Current version**: `2.10.0`

**Future Enhancement**: For provable DAG operations using `IndexedMerkleMap`, upgrade to o1js v2.12+:

```bash
npm install o1js@latest
```

See `DAG-FUTURE.md` for what this unlocks.

### 2. TypeScript Configuration

If you encounter type errors, add to your `tsconfig.json`:

```json
{
  "compilerOptions": {
    "skipLibCheck": true,
    "moduleResolution": "node",
    "allowSyntheticDefaultImports": true,
    "esModuleInterop": true
  }
}
```

### 3. Browser Configuration

o1js requires specific headers for `SharedArrayBuffer`:

```typescript
// In your server config (e.g., vite.config.ts, next.config.js)
headers: [
  {
    key: 'Cross-Origin-Opener-Policy',
    value: 'same-origin'
  },
  {
    key: 'Cross-Origin-Embedder-Policy',
    value: 'require-corp'
  }
]
```

## First-Time Setup

### 1. Initialize ZK System

```typescript
import { initializeZkSystem } from '$lib/commons/compute/zk';

// Call once at app startup
// WARNING: This takes 3-5 minutes!
console.log('Initializing ZK system...');
await initializeZkSystem();
console.log('ZK system ready!');
```

### 2. Cache Compilation Keys (Recommended)

To avoid recompiling on every app start:

```typescript
import { compileZkPrograms } from '$lib/commons/compute/zk';
import fs from 'fs/promises';

// Compile once
const compiled = await compileZkPrograms();

// Save to disk
await fs.writeFile(
  './zk-keys.json',
  JSON.stringify({
    eventIntegrity: compiled.eventIntegrity.verificationKey,
    allocation: compiled.allocation.verificationKey,
    // ... other keys
  })
);

// Later: Load from disk (instant startup)
const keys = JSON.parse(await fs.readFile('./zk-keys.json', 'utf-8'));
```

## Troubleshooting

### Error: "Cannot find module 'o1js'"

**Solution:** Run `npm install o1js` and restart your dev server.

### Error: "SharedArrayBuffer is not defined"

**Solution:** Add the CORS headers shown above to your server config.

### Error: "WebAssembly compilation failed"

**Solution:** Update your browser. o1js requires modern browsers with WebAssembly support.

### Compilation is Very Slow

**Solution:** This is normal for first run. Use key caching (see above) to avoid recompiling.

### Type Errors with o1js Struct

**Solution:** o1js uses dynamic property creation. Add `skipLibCheck: true` to `tsconfig.json`.

## Verification

After setup, verify the system works:

```typescript
import { isZkSystemReady, getZkSystemStatus } from '$lib/commons/compute/zk';

// Check status
console.log('ZK Ready?', isZkSystemReady());
console.log('ZK Status:', getZkSystemStatus());

// Test proof generation
import { Field } from 'o1js';
import { ProvenanceEventZK } from '$lib/commons/compute/zk';

// Should not throw
const testEvent = new ProvenanceEventZK({
  id: new EventHash({ value: Field(123) }),
  author: Field(456),
  // ... other fields
});

console.log('✅ ZK system verified');
```

## Production Deployment

### Pre-compile Keys

Don't compile in production! Pre-compile keys and include them:

```bash
# In development
npm run build:zk-keys

# Deploy with pre-compiled keys
npm run deploy
```

### Environment Variables

```bash
# .env
ZK_ENABLED=true
ZK_KEYS_PATH=./dist/zk-keys.json
```

### CDN for o1js

For faster loading, use o1js from CDN:

```html
<script src="https://unpkg.com/o1js@latest/dist/web/index.js"></script>
```

## Performance Tuning

### Worker Threads (Node.js)

```typescript
import { Worker } from 'worker_threads';

// Generate proofs in worker thread
const worker = new Worker('./zk-worker.js');
worker.postMessage({ event, type: 'prove' });
```

### Web Workers (Browser)

```typescript
// zk-worker.ts
import { generateAllocationProof } from '$lib/commons/compute/zk';

self.onmessage = async (e) => {
  const proof = await generateAllocationProof(e.data);
  self.postMessage({ proof });
};

// main.ts
const worker = new Worker('./zk-worker.js');
worker.postMessage(allocationClaim);
worker.onmessage = (e) => {
  console.log('Proof ready:', e.data.proof);
};
```

## Next Steps

1. Read [README.md](./README.md) for usage guide
2. See [API Reference](./index.ts) for complete API
3. Check [examples/](../examples/) for code samples
4. Review [zk-types.ts](./zk-types.ts) for available circuits

## Support

- [o1js Docs](https://docs.o1labs.org/o1js)
- [Mina Discord](https://discord.gg/minaprotocol)
- Issues: GitHub repo

