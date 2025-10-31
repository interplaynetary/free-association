# Fast JSON Parsing Utilities

Efficient JSON parsing utilities using the `fast-json` library for performance-critical operations.

## Quick Start

```typescript
import { fastExtractTimestamp, fastParse } from '$lib/utils/fastJsonParser';

// Extract timestamp without full parsing
const timestamp = await fastExtractTimestamp(jsonString, '_updatedAt');

// Use in your update logic
if (timestamp > lastSeen) {
  const data = fastParse(jsonString);
  processUpdate(data);
}
```

## Why Fast JSON?

Traditional `JSON.parse()` parses the entire JSON structure before you can access any field. For large objects, this is wasteful if you only need specific fields.

**Example Scenario:**
```typescript
// You have a 100KB JSON with 1000 fields
// You only need to check the timestamp (_updatedAt)

// ❌ Slow: Parse entire 100KB
const data = JSON.parse(json); // ~5ms
if (data._updatedAt > lastSeen) {
  processData(data);
}

// ✅ Fast: Extract only timestamp
const timestamp = await fastExtractTimestamp(json); // ~0.5ms
if (timestamp > lastSeen) {
  const data = JSON.parse(json); // Only parse if needed
  processData(data);
}
```

**Performance Gain**: 10x faster when you can skip full parsing!

## Core Functions

### `fastExtractTimestamp(jsonString, field?)`

Extract a timestamp field efficiently.

```typescript
const timestamp = await fastExtractTimestamp(jsonString, '_updatedAt');
// Default field is '_updatedAt'
const timestamp2 = await fastExtractTimestamp(jsonString);
```

**Use Case**: Check data freshness before expensive operations

### `fastParseSelective(jsonString, paths)`

Extract multiple specific fields without full parsing.

```typescript
const metadata = {};
await fastParseSelective(jsonString, {
  'id': (v) => metadata.id = v,
  '_updatedAt': (v) => metadata.timestamp = v,
  'user.name': (v) => metadata.userName = v,
  'slots[0].type': (v) => metadata.firstSlotType = v
});
```

**Use Case**: Extract metadata for filtering/routing decisions

### `fastParseArrayFind(jsonString, arrayPath, predicate)`

Find item in array with early termination.

```typescript
const found = await fastParseArrayFind(
  jsonString,
  'need_slots[*].id',
  (id) => id === targetId
);
// Stops parsing as soon as target is found!
```

**Use Case**: Search large arrays without parsing everything

### `fastParseNetworkMessage(jsonString)`

Extract tracking ID from network messages efficiently.

```typescript
const { trackId, message } = await fastParseNetworkMessage(jsonString);

if (isDuplicate(trackId)) {
  return; // Skip full processing
}

processMessage(message);
```

**Use Case**: Duplicate detection in network protocols

### `fastParseSafe(jsonString, maxSize?)`

Parse with size limit protection.

```typescript
const result = await fastParseSafe(jsonString, 1024 * 1024); // 1MB max

if (result.success) {
  processData(result.data);
} else {
  console.error('Parse error:', result.error);
}
```

**Use Case**: Protect against oversized payloads

### `fastValidateStructure(jsonString, options)`

Check for required fields before full parsing.

```typescript
const { valid, missing } = await fastValidateStructure(jsonString, {
  requiredFields: ['id', 'type', '_updatedAt'],
  optionalFields: ['metadata']
});

if (!valid) {
  console.error('Missing fields:', missing);
  return;
}

const data = JSON.parse(jsonString); // Safe to parse
```

**Use Case**: Schema validation before expensive parsing

## Where It's Used

### 1. **Store Persistence** (`src/lib/commons/utils/store.svelte.ts`)

Optimizes network update processing:

```typescript
// Extract timestamp first
const timestamp = await fastExtractTimestamp(data, '_updatedAt');

// Skip full parse if data is stale
if (timestamp <= lastNetworkTimestamp) {
  return; // Saved ~5ms by not parsing!
}

// Only parse if data is fresh
const parsedData = fastParse(data);
// ...validate and process
```

### 2. **Network Message Queueing**

Decides whether to queue messages without full parsing:

```typescript
if (isPersisting) {
  fastExtractTimestamp(data, '_updatedAt')
    .then((networkTimestamp) => {
      if (networkTimestamp !== lastTimestamp) {
        queuedNetworkUpdate = data;
      }
    });
}
```

## Performance Characteristics

| Operation | Standard Parse | Fast JSON | Speedup |
|-----------|---------------|-----------|---------|
| Extract timestamp (100KB) | ~5ms | ~0.5ms | **10x** |
| Find in array (100 items, target at position 10) | ~5ms | ~1ms | **5x** |
| Find in array (100 items, target at position 50) | ~5ms | ~2.5ms | **2x** |
| Extract 3 metadata fields | ~5ms | ~1ms | **5x** |
| Validate 4 required fields | ~5ms | ~0.8ms | **6x** |

**Key Insight**: The earlier your target field appears in the JSON, the bigger the performance gain!

## When to Use

### ✅ Good Use Cases

1. **Timestamp/Version Checks**
   - Check if data is stale before processing
   - Example: Network update filtering

2. **Metadata Extraction**
   - Extract IDs, types, versions for routing
   - Example: Message type detection

3. **Array Searches**
   - Find specific items in large arrays
   - Example: Finding slots by ID

4. **Size Validation**
   - Check JSON size before parsing
   - Example: Reject oversized payloads

5. **Structure Validation**
   - Verify required fields exist
   - Example: Pre-parse validation

### ❌ Don't Use When

1. **Small Objects** (< 1KB)
   - Native `JSON.parse()` is faster

2. **Need All Fields**
   - No benefit if you need the entire object anyway

3. **Synchronous Requirements**
   - Fast-json is async (Promise-based)

4. **Simple Objects** (< 10 fields)
   - Overhead not worth it

## Best Practices

### Pattern 1: Timestamp Gate

```typescript
async function processUpdate(jsonString: string) {
  // Step 1: Quick check
  const timestamp = await fastExtractTimestamp(jsonString);
  if (timestamp <= lastSeen) return; // Early exit!
  
  // Step 2: Full parse only if needed
  const data = JSON.parse(jsonString);
  
  // Step 3: Process
  processData(data);
}
```

### Pattern 2: Metadata-Based Routing

```typescript
async function routeMessage(jsonString: string) {
  // Extract just the type field
  let type: string;
  await fastParseSelective(jsonString, {
    'type': (v) => type = v
  });
  
  // Route based on type (without full parse)
  if (type === 'heartbeat') {
    return; // Skip expensive parsing for heartbeats
  }
  
  // Full parse for important messages
  const message = JSON.parse(jsonString);
  handleMessage(message);
}
```

### Pattern 3: Batch Filtering

```typescript
async function filterBatch(messages: string[]) {
  // Extract all timestamps quickly
  const timestamps = await Promise.all(
    messages.map(msg => fastExtractTimestamp(msg))
  );
  
  // Filter to only new messages
  const newMessages = messages.filter((msg, i) => 
    timestamps[i] > lastSeen
  );
  
  // Parse only the new ones
  return newMessages.map(msg => JSON.parse(msg));
}
```

## Benchmark Your Code

Run the included benchmarks:

```bash
bun run benchmark:json
```

Or test with your own data:

```typescript
import { fastExtractTimestamp } from '$lib/utils/fastJsonParser';

const myLargeJson = JSON.stringify({ /* your data */ });

// Benchmark standard parse
console.time('Standard');
const data1 = JSON.parse(myLargeJson);
const ts1 = data1._updatedAt;
console.timeEnd('Standard');

// Benchmark fast extraction
console.time('Fast');
const ts2 = await fastExtractTimestamp(myLargeJson);
console.timeEnd('Fast');
```

## Common Pitfalls

### ❌ Using for Small Objects

```typescript
// Don't do this for small objects
const tiny = '{"id": "123", "name": "test"}';
await fastExtractTimestamp(tiny); // Slower than JSON.parse!
```

### ❌ Extracting Many Fields from Small Object

```typescript
// If you need many fields, just parse it
await fastParseSelective(smallJson, {
  'field1': ...,
  'field2': ...,
  'field3': ...,
  // 20 more fields...
});
// Just use JSON.parse(smallJson) instead!
```

### ✅ Correct Usage

```typescript
// Good: Large object, only need timestamp
const large = /* 100KB JSON */;
const timestamp = await fastExtractTimestamp(large);

// Good: Find in large array
const found = await fastParseArrayFind(
  largeArrayJson,
  'items[*].id',
  id => id === target
);
```

## API Reference

See [`fastJsonParser.ts`](./fastJsonParser.ts) for complete API documentation.

## Further Reading

- [Full Optimization Guide](../../../docs/FAST_JSON_OPTIMIZATION.md)
- [Usage Examples](./fastJsonParser.example.ts)
- [fast-json on npm](https://www.npmjs.com/package/fast-json)

## Summary

Fast JSON parsing provides significant performance improvements when:

1. ✅ Objects are large (> 10KB)
2. ✅ You need specific fields only
3. ✅ You can skip processing based on metadata
4. ✅ You're searching arrays with early termination

Use it wisely in performance-critical paths like network synchronization, caching decisions, and message routing!

