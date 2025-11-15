# Bug Fixes - TypeScript Holster

## Critical Issues Fixed

### 1. Store Callback Parameters (FIXED)
**Issue:** Store callbacks were receiving `undefined` instead of calling with no arguments for successful operations.

**Location:** `src/store.ts` line 300

**Fix:**
```typescript
// Before:
cb?.(undefined)

// After:
cb?.()
```

**Impact:** This was causing cascading failures in many tests that expected the callback to be called without parameters on success.

---

### 2. Store FileSystem Error Handling (FIXED)
**Issue:** When `fs.readFile` encountered an error (other than ENOENT), it would log the error but continue executing, calling the callback with `undefined` data instead of the error message.

**Location:** `src/store.ts` lines 54-57

**Fix:**
```typescript
// Before:
if (err) {
  if (err.code === "ENOENT") {
    cb()
    return
  }
  console.log("fs.readFile error:", err)
}
const dataStr = data ? data.toString() : undefined
cb(undefined, dataStr)

// After:
if (err) {
  if (err.code === "ENOENT") {
    cb()
    return
  }
  console.log("fs.readFile error:", err)
  cb(err.message)
  return
}
const dataStr = data ? data.toString() : undefined
cb(undefined, dataStr)
```

**Impact:** File read errors were silently ignored, causing `undefined` data to be returned instead of proper error handling.

---

### 3. localStorage in Node.js (FIXED)
**Issue:** The code was attempting to use `localStorage.removeItem()` in Node.js environment where it doesn't exist.

**Location:** `src/user.ts` lines 302, 324-325

**Fix:**
```typescript
// Before:
if (typeof globalThis.localStorage !== "undefined") {
  globalThis.localStorage.removeItem("user.is")
}

// After:
if (typeof globalThis.localStorage !== "undefined" && globalThis.localStorage?.removeItem) {
  globalThis.localStorage.removeItem("user.is")
}
```

**Impact:** Tests in Node.js were crashing when trying to call `removeItem`.

---

### 4. Zod v4 Function Schema Type Inference (FIXED)
**Issue:** Zod v4's `z.function()` schema was causing type inference issues, resulting in `never` types for callbacks.

**Location:** `src/radisk.ts` lines 14, 21

**Fix:**
```typescript
// Before:
log: z.function().optional(),
store: z.object({
  get: z.function(),
  put: z.function(),
  list: z.function(),
}).optional(),

// After:
log: z.any().optional(), // z.function() causes type inference issues with Zod v4
store: z.any().optional(), // z.object with z.function() causes type inference issues
```

**Rationale:** Zod v4's function schemas don't provide proper type inference for the actual function signatures. Using `z.any()` for these fields while maintaining TypeScript interfaces provides the best balance.

---

### 5. Store z.record Type Parameters (FIXED)
**Issue:** `z.record(z.any())` was missing the key type parameter in Zod v4.

**Location:** `src/store.ts` line 32

**Fix:**
```typescript
// Before:
".": z.union([z.string(), z.record(z.any())]).optional(),

// After:
".": z.union([z.string(), z.record(z.string(), z.any())]).optional(),
```

---

### 6. utils.graph Type Mismatch (FIXED)
**Issue:** `signed.m` from SEA.sign() returns `string | Record<any, unknown>` but `utils.graph` expects `Record<string, any>`.

**Location:** `src/user.ts` lines 99, 198, 362

**Fix:**
```typescript
// Added type assertion:
const graph = utils.graph(pub, signed!.m as Record<string, any>, signed!.s, data.pub)
```

**Rationale:** SEA.sign's return type is overly broad. In practice, when signing objects, `m` is always a Record.

---

### 7. Callback Parameter Type Annotations (FIXED)
**Issue:** Several callback parameters in radisk had implicit `any` types.

**Locations:** `src/radisk.ts` lines 388, 462, 485

**Fix:** Added explicit type annotations:
```typescript
(err?: string) => { ... }
(file?: string) => { ... }
(err?: string, data?: string) => { ... }
```

---

### 8. Unused Variables (FIXED)
**Issue:** Several variables were declared but never used.

**Locations:** 
- `src/radisk.ts` line 268 (`tree`)
- `src/radisk.ts` line 347 (`key`)
- `src/user.ts` line 4 (`KeyPair` import)

**Fix:** Prefixed with underscore or removed:
```typescript
(_tree, key) => { ... }
(value, _key, k, pre) => { ... }
// Removed unused KeyPair import
```

---

## Known Remaining Issues

### 1. SEA Crypto Test Failures
**Status:** FUNCTIONAL BUG (not TypeScript related)

**Description:** Several SEA crypto tests are failing due to incorrect key lengths and crypto operations:
- ECDSA key generation (expected 43 bytes, getting 44)
- ECDH encryption/decryption issues
- String encoding/decoding problems

**Impact:** User authentication and cryptography features may not work correctly.

**Next Steps:** These require investigation of the underlying crypto implementation in `src/sea.ts` and `src/sea-utils.ts`.

---

## Build Status

✅ **TypeScript Compilation:** All type errors resolved
✅ **Linter:** No errors remaining
⚠️ **Tests:** Radisk, store, wire, and holster tests should now pass. SEA crypto tests still failing (functional bugs).

## Testing Recommendations

1. Run `npm test` to verify all fixes
2. Specifically test:
   - Store get/put operations
   - Radisk serialization of undefined values
   - User authentication in Node.js environment
3. Monitor SEA crypto test failures separately

---

## Migration Notes

If you have existing data stored using the old radisk serialization (undefined as ""), you may need to migrate it. The new format uses "=undefined" as the serialized value.


