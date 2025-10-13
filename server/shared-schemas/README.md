# Shared Schemas

Shared Zod v4 schemas for Free Association backend services.

## Architecture: OpenRouter-First

All services now use **OpenRouter exclusively** for LLM access:
- Single API key per donor
- Unified model access across providers
- Simplified routing and key management
- No provider-specific logic needed

## Schemas

### Completion Schemas (`completion.ts`)

- `MessageSchema` - OpenAI-compatible message format
- `BaseCompletionRequestSchema` - Standard completion request
- `ExtendedCompletionRequestSchema` - With domain-specific fields
- `TokenRequestSchema` - Authentication token request

### Routing Schemas (`routing.ts`)

- `RoutingResponseSchema` - Router response (OpenRouter-only)
- `HealthReportSchema` - Key health reporting
- `KeyResponseSchema` - Key pool response
- `KeyHealthStatus` - Health status enum

## Usage

### In AI Proxy

```typescript
import {
  BaseCompletionRequestSchema,
  RoutingResponseSchema,
  HealthReportSchema
} from '@free-association/shared-schemas';

// Validate request
const parsed = BaseCompletionRequestSchema.safeParse(req.body);

// Validate routing response
const routing = RoutingResponseSchema.parse(routingData);

// Create health report
const report = HealthReportSchema.parse({ key, status, error, cost });
```

### In LLM Router

```typescript
import {
  ExtendedCompletionRequestSchema,
  RoutingResponseSchema,
  FlowMetadataSchema
} from '@free-association/shared-schemas';

// Validate incoming request (supports typed flows)
const request = ExtendedCompletionRequestSchema.parse(req.body);

// Return routing decision
return RoutingResponseSchema.parse({
  success: true,
  model: 'anthropic/claude-3-opus',
  provider: 'openrouter',
  key: openRouterKey,
  flow: { name, requestType, description }
});
```

### In Key Pool

```typescript
import {
  HealthReportSchema,
  KeyResponseSchema,
  KeyHealthStatus
} from '@free-association/shared-schemas';

// Validate health report
const report = HealthReportSchema.parse(req.body);

// Return key
return KeyResponseSchema.parse({
  success: true,
  model: 'openrouter',
  key: selectedKey,
  pool: { totalKeys, healthyKeys }
});
```

## Benefits

### Type Safety Across Services
- ✅ Compile-time type checking
- ✅ No schema drift between services
- ✅ Single source of truth

### OpenRouter Simplification
- ✅ One provider to handle
- ✅ No provider-specific logic
- ✅ Unified key pooling
- ✅ Simplified routing

### Maintainability
- ✅ Update schemas in one place
- ✅ All services automatically sync
- ✅ Easier refactoring

## Installation

From any service directory:

```bash
bun link ../shared-schemas
```

Or add to package.json:

```json
{
  "dependencies": {
    "@free-association/shared-schemas": "link:../shared-schemas"
  }
}
```

## Development

```bash
cd server/shared-schemas
bun install

# Type check
bun tsc --noEmit
```

## Version

Current: 1.0.0 (OpenRouter-first architecture)

