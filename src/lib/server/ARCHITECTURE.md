# Server Architecture Refactoring

This document describes the refactored server architecture that eliminates DRY violations and provides a unified approach to common server-side patterns.

## Overview

The refactoring addresses these key areas:

1. **Unified Authentication** - Single auth system supporting JWT, API Keys, and Basic Auth
2. **Request Handlers** - Standardized request validation and response formatting
3. **Health Checks** - Consolidated health monitoring across all services
4. **Database Helpers** - Type-safe wrappers for Gun/Holster operations
5. **Environment Config** - Centralized configuration management

## New Modules

### 1. Configuration (`lib/server/config.ts`)

Centralized environment variable loading with fallbacks for static builds.

```typescript
import { config } from '$lib/server/config';

// Access config values
config.nodeEnv
config.jwtSecret
config.holsterUsername
config.openrouterBaseUrl
```

**Key Features:**
- Single source of truth for all env vars
- Automatic fallbacks for static builds
- Type-safe access
- Runtime validation

### 2. Unified Authentication (`lib/server/middleware/unified-auth.ts`)

Consolidated auth system supporting multiple methods.

```typescript
import { requireAuth, authenticate, checkAuth } from '$lib/server/middleware/unified-auth';

// In routes - require auth with specific methods
export const GET: RequestHandler = async ({ request }) => {
  const auth = requireAuth(request, {
    allowJwt: true,
    allowApiKey: true,
    allowBasic: false
  });
  
  // auth.user contains user info if available
  const userId = auth.user?.userId;
};

// For SvelteKit events
import { requireAuthEvent } from '$lib/server/middleware/unified-auth';

export const POST: RequestHandler = async (event) => {
  const auth = requireAuthEvent(event);
  // ...
};

// Backward compatibility
import { checkAuth } from '$lib/server/middleware/unified-auth';

export const POST: RequestHandler = async (event) => {
  const authError = checkAuth(event, { allowBasic: true });
  if (authError) return authError;
  // ...
};
```

**Key Features:**
- Supports JWT, API Key, and Basic Auth in one system
- Flexible per-route auth configuration
- Backward compatible with existing code
- Centralized API key management

### 3. Request Handlers (`lib/server/middleware/request-handler.ts`)

Standardized request handling with automatic validation.

```typescript
import { createPOSTHandler, createGETHandler } from '$lib/server/middleware/request-handler';
import { mySchema } from './schemas';

// POST with validation
export const POST = createPOSTHandler(
  mySchema,
  async ({ data, auth, event }) => {
    // data is validated and typed
    // auth contains authentication info
    // event is the full SvelteKit RequestEvent
    
    return { success: true, result: data };
  },
  {
    requireAuth: true,
    authOptions: { allowBasic: true },
    emptyResponse: false  // return JSON by default
  }
);

// GET without validation
export const GET = createGETHandler(
  async ({ auth, event }) => {
    return { data: [] };
  },
  { requireAuth: true }
);
```

**Key Features:**
- Automatic JSON parsing and validation
- Built-in auth checking
- Consistent error handling
- Type-safe data access
- Flexible response formats

### 4. Holster Database Helpers (`lib/server/holster/db.ts`)

Type-safe wrappers for Gun/Holster operations.

```typescript
import {
  holsterNext,
  holsterNextPut,
  getAccountByCode,
  getAccountByCodeOrFail,
  updateAccount,
  holsterVerify,
  holsterEncrypt,
  ensureAuthenticated
} from '$lib/server/holster/db';

// Get data
const account = await holsterNext('accounts', code);

// Put data
await holsterNextPut('accounts', code, { subscribed: count });

// Get account with error handling
const account = await getAccountByCodeOrFail(code);

// Verify signed data
const verified = await holsterVerify(signedData, publicKey);

// Ensure user is authenticated
ensureAuthenticated();  // throws if not
```

**Key Features:**
- Promise-based API (no manual promise wrapping)
- Automatic error handling
- Type-safe operations
- Crypto helpers (encrypt, decrypt, verify)
- Common patterns extracted

### 5. Health Monitoring (`lib/server/health/index.ts`)

Unified health check system for all services.

```typescript
import { checkHealth, registerHealthCheck } from '$lib/server/health';

// Check all services
const health = await checkHealth();

// Check specific services
const health = await checkHealth(['ai-proxy', 'key-pool']);

// Register custom health check
registerHealthCheck('my-service', async () => ({
  status: 'ok',
  details: { customMetric: 123 }
}));
```

**Built-in Health Checks:**
- `system` - Memory, uptime, process info
- `holster` - Database stats, request performance
- `key-pool` - API key pool status
- `llm-router` - Available flows
- `ai-proxy` - OpenRouter configuration
- `data-relay` - Registered relay types

**Unified Endpoint:**
```
GET /api/health                    # All services
GET /api/health?services=ai,llm   # Specific services
```

Response:
```json
{
  "status": "ok",
  "timestamp": 1234567890,
  "services": {
    "system": { "status": "ok", "details": {...} },
    "holster": { "status": "ok", "details": {...} }
  },
  "summary": {
    "total": 6,
    "ok": 5,
    "degraded": 1,
    "down": 0
  }
}
```

## Migration Guide

### Converting Existing Routes

#### Before (Old Pattern):
```typescript
import { error, json } from '@sveltejs/kit';
import type { RequestHandler } from '@sveltejs/kit';
import { mySchema } from './schemas';
import { checkAuth } from '$lib/server/holster/auth';

export const POST: RequestHandler = async (event) => {
  const authError = checkAuth(event);
  if (authError) return authError;
  
  const body = await event.request.json();
  const result = mySchema.safeParse(body);
  
  if (!result.success) {
    const firstError = result.error.errors[0];
    error(400, firstError.message);
  }
  
  const { field1, field2 } = result.data;
  
  // ... business logic ...
  
  return json({ success: true });
};
```

#### After (New Pattern):
```typescript
import { mySchema } from './schemas';
import { createPOSTHandler } from '$lib/server/middleware/request-handler';

export const POST = createPOSTHandler(
  mySchema,
  async ({ data }) => {
    const { field1, field2 } = data;
    
    // ... business logic ...
    
    return { success: true };
  },
  {
    requireAuth: true,
    authOptions: { allowBasic: true, allowJwt: false, allowApiKey: false }
  }
);
```

**Benefits:**
- 80% less boilerplate
- Automatic validation
- Type-safe data access
- Consistent error handling

### Converting Holster Database Operations

#### Before:
```typescript
const account = await new Promise(res => {
  user.get("accounts").next(code, res);
});

if (!account || !(account as any).epub) {
  error(404, "Account not found");
}

const err = await new Promise(res => {
  user.get("accounts").next(code).put(data, res);
});

if (err) {
  console.log(err);
  error(500, "Database error");
}
```

#### After:
```typescript
const account = await getAccountByCodeOrFail(code);

await holsterNextPut("accounts", code, data);
```

### Converting Environment Variables

#### Before:
```typescript
let MY_VAR: string | undefined;

try {
  const env = await import('$env/static/private');
  MY_VAR = env.MY_VAR;
} catch (e) {
  MY_VAR = undefined;
}
```

#### After:
```typescript
import { config } from '$lib/server/config';

const myVar = config.myVar;
```

### Converting Authentication

#### Before (Multiple Auth Systems):
```typescript
// System 1: JWT/API Key
import { requireAuth } from '$lib/server/middleware/auth';
const auth = requireAuth(request);

// System 2: Basic Auth
import { checkAuth } from '$lib/server/holster/auth';
const authError = checkAuth(event);
if (authError) return authError;
```

#### After (Unified):
```typescript
import { requireAuth, checkAuth } from '$lib/server/middleware/unified-auth';

// Option 1: Throw on auth failure
const auth = requireAuth(request, {
  allowJwt: true,
  allowApiKey: true,
  allowBasic: true
});

// Option 2: Return error response (backward compatible)
const authError = checkAuth(event, {
  allowBasic: true
});
if (authError) return authError;
```

## Backward Compatibility

All existing routes continue to work without changes:

- `lib/server/middleware/auth.ts` - Re-exports unified auth
- `lib/server/holster/auth.ts` - Uses unified auth internally
- Old import paths still work
- Old patterns still function

However, new code should use the new patterns for consistency.

## Best Practices

### 1. Use Request Handlers for All Routes
```typescript
// ✅ Good
export const POST = createPOSTHandler(schema, async ({ data }) => {
  // handler
});

// ❌ Avoid
export const POST: RequestHandler = async ({ request }) => {
  const body = await request.json();
  // manual validation...
};
```

### 2. Use Database Helpers
```typescript
// ✅ Good
const account = await getAccountByCode(code);

// ❌ Avoid
const account = await new Promise(res => {
  user.get("accounts").next(code, res);
});
```

### 3. Use Centralized Config
```typescript
// ✅ Good
import { config } from '$lib/server/config';
const url = config.openrouterBaseUrl;

// ❌ Avoid
const url = process.env.OPENROUTER_BASE_URL || 'default';
```

### 4. Use Unified Health Endpoint
```typescript
// ✅ Good
GET /api/health?services=ai,keys

// ❌ Avoid (deprecated endpoints)
GET /api/ai/health
GET /api/keys/health
GET /api/llm/health
```

## Testing

The new architecture makes testing easier:

```typescript
import { authenticate } from '$lib/server/middleware/unified-auth';
import { validateBody } from '$lib/server/middleware/request-handler';

// Test auth
const result = authenticate(mockRequest, { allowApiKey: true });

// Test validation
const data = validateBody({ field: 'value' }, schema);
```

## Future Improvements

1. Add OpenAPI/Swagger documentation generation
2. Add request/response logging middleware
3. Add CORS middleware
4. Add request timing middleware
5. Add database query optimization
6. Add caching layer for frequent queries

## Questions?

For questions or issues with the new architecture, check:
- This document for patterns and examples
- Individual module files for detailed JSDoc comments
- `lib/server/middleware/index.ts` for all available exports

