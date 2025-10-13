# OpenRouter-First Simplification ✅

## Summary

The backend has been simplified to **OpenRouter-only** with **shared Zod v4 schemas** across all services.

## What Changed

### 1. OpenRouter-Only Architecture

**Before:** Multi-provider support (OpenAI, Anthropic, Mistral, OpenRouter)
```typescript
const PROVIDER_ENDPOINTS = {
  'openrouter': { ... },
  'openai': { ... },
  'anthropic': { ... },
  'mistral': { ... }
};
```

**After:** OpenRouter-only
```typescript
// Single provider: OpenRouter
// Access all models through one unified API
const OPENROUTER_ENDPOINT = 'https://openrouter.ai/api/v1/chat/completions';
```

### 2. Shared Schemas Package

Created `server/shared-schemas/` with:
- `completion.ts` - Request/response schemas
- `routing.ts` - Routing and health schemas
- `index.ts` - Unified exports

All services now import from the same schema definitions.

### 3. Simplified Key Pooling

**Before:** Separate pools per model
```
GPT_4_KEYS=...
CLAUDE_3_OPUS_KEYS=...
MISTRAL_LARGE_KEYS=...
```

**After:** Single OpenRouter pool
```
OPENROUTER_KEYS=key1,key2,key3
```

## Benefits

### Simplicity
- ✅ **One API** to integrate with
- ✅ **No provider-specific logic**
- ✅ **Unified error handling**
- ✅ **Simpler configuration**

### Key Pooling
- ✅ **Donated keys work for all models**
- ✅ **No model-specific key management**
- ✅ **Easier for donors** (one key = access to everything)
- ✅ **Better load distribution**

### Schema Consistency
- ✅ **No schema drift** between services
- ✅ **Type safety across boundaries**
- ✅ **Single source of truth**
- ✅ **Easier to maintain**

### Cost & Flexibility
- ✅ **OpenRouter handles routing** (no custom logic needed)
- ✅ **Access to 100+ models** with one integration
- ✅ **Built-in failover** (OpenRouter handles it)
- ✅ **Cost tracking** included

## Shared Schemas Structure

```
server/shared-schemas/
├── package.json
├── tsconfig.json
├── completion.ts          # Request/response schemas
├── routing.ts             # Routing & health schemas
├── index.ts               # Unified exports
└── README.md
```

### Usage

Services import schemas directly:

```typescript
// In ai-proxy/server.ts
import {
  BaseCompletionRequestSchema,
  RoutingResponseSchema
} from '../shared-schemas/completion';

// In llm-router/router.ts
import {
  ExtendedCompletionRequestSchema,
  RoutingResponseSchema
} from '../shared-schemas/completion';

// In key-pool/server.ts
import {
  HealthReportSchema,
  KeyHealthStatus
} from '../shared-schemas/routing';
```

## OpenRouter Model Format

Models are specified as `provider/model-name`:

```typescript
// Examples
'anthropic/claude-3-opus'
'openai/gpt-4-turbo'
'google/gemini-pro'
'meta-llama/llama-3-70b'
```

## Configuration Changes

### Environment Variables

**Removed:**
```bash
# No longer needed
OPENAI_BASE_URL=...
ANTHROPIC_BASE_URL=...
MISTRAL_BASE_URL=...
GPT_4_KEYS=...
CLAUDE_3_OPUS_KEYS=...
MISTRAL_LARGE_KEYS=...
```

**Added:**
```bash
# Single configuration
OPENROUTER_BASE_URL=https://openrouter.ai/api/v1
OPENROUTER_KEYS=sk-or-v1-key1,sk-or-v1-key2,sk-or-v1-key3
```

### Docker Compose

Simplified environment configuration:

```yaml
key-pool:
  environment:
    - OPENROUTER_KEYS=${OPENROUTER_KEYS}  # Single pool

ai-proxy:
  environment:
    - OPENROUTER_BASE_URL=${OPENROUTER_BASE_URL:-https://openrouter.ai/api/v1}
```

## Request Flow

```
Client Request
    ↓
AI Proxy (validates with shared schema)
    ↓
LLM Router (routes using shared schema)
    ↓
Key Pool (returns OpenRouter key)
    ↓
OpenRouter API (handles provider routing)
    ↓
Response (validated with shared schema)
```

## Migration Path

### For Existing Keys

If you have provider-specific keys, you can:

1. **Get OpenRouter keys** instead (recommended)
2. **Add keys to OpenRouter** account
3. **Use one unified pool**

### For Existing Code

No breaking changes to API contracts:
- ✅ Same endpoints
- ✅ Same request format
- ✅ Same response format
- ✅ Just simpler internally

## Example Usage

### Request to AI Proxy

```bash
curl -X POST http://localhost:8767/api/ai/completion \
  -H "Content-Type: application/json" \
  -H "X-API-Key: your-key" \
  -d '{
    "messages": [
      {"role": "user", "content": "Hello!"}
    ],
    "model": "anthropic/claude-3-opus",  # OpenRouter format
    "maxTokens": 1000
  }'
```

### Typed Flow Request

```bash
curl -X POST http://localhost:8767/api/ai/completion \
  -H "Content-Type: application/json" \
  -H "X-API-Key: your-key" \
  -d '{
    "requestType": "recognition-analysis",
    "players": [...],
    "analysisType": "mutual-recognition"
  }'
```

### Response

```json
{
  "choices": [...],
  "usage": {...},
  "_routing": {
    "model": "anthropic/claude-3-opus",
    "provider": "openrouter",
    "flow": "Recognition Analysis",
    "responseTimeMs": 1234
  }
}
```

## Files Created

- `server/shared-schemas/package.json`
- `server/shared-schemas/tsconfig.json`
- `server/shared-schemas/completion.ts`
- `server/shared-schemas/routing.ts`
- `server/shared-schemas/index.ts`
- `server/shared-schemas/README.md`

## Next Steps

### To Complete Migration

1. ✅ Update AI Proxy to use shared schemas
2. ✅ Update LLM Router to use shared schemas
3. ✅ Update Key Pool to OpenRouter-only
4. ✅ Simplify provider logic
5. ✅ Update documentation

### Testing

```bash
# Test with OpenRouter key
export OPENROUTER_KEYS=sk-or-v1-your-key

# Start services
cd server
docker-compose up
```

## Why OpenRouter?

### For Donors
- **One key** gives access to all models
- **Simpler** to contribute
- **Transparent** pricing
- **Usage tracking** built-in

### For the System
- **Less complexity** (one provider vs many)
- **Better failover** (handled by OpenRouter)
- **More models** (100+ models available)
- **Easier scaling**

### For Users
- **More choice** in models
- **Better availability** (OpenRouter handles outages)
- **Consistent experience**
- **Lower latency** (OpenRouter optimizes routing)

## Summary

✅ **OpenRouter-only architecture**  
✅ **Shared Zod v4 schemas**  
✅ **Simplified key pooling**  
✅ **Type-safe across services**  
✅ **Easier to maintain**  
✅ **Better for donors**  
✅ **Access to 100+ models**

The system is now simpler, more maintainable, and easier to extend! 🎉

