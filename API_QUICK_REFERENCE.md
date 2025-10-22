# API Quick Reference

## Base URL
```
http://localhost:5173/api
```

## Authentication

### Get Token
```bash
POST /api/ai/token
Content-Type: application/json

{
  "apiKey": "your-master-api-key",
  "userId": "optional-user-id"
}
```

### Use Token
```bash
Authorization: Bearer <token>
```

### Or Use API Key
```bash
X-API-Key: your-master-api-key
```

## AI Completion

### Simple Request
```bash
POST /api/ai/completion
Authorization: Bearer <token>
Content-Type: application/json

{
  "prompt": "Your prompt here",
  "maxTokens": 100
}
```

### Chat Format
```bash
POST /api/ai/completion
Authorization: Bearer <token>
Content-Type: application/json

{
  "messages": [
    {"role": "user", "content": "Hello!"}
  ],
  "maxTokens": 100,
  "temperature": 0.7
}
```

### With Flow Type
```bash
POST /api/ai/completion
Authorization: Bearer <token>
Content-Type: application/json

{
  "requestType": "recognition-analysis",
  "analysisType": "mutual-recognition",
  "players": [...],
  "maxTokens": 500
}
```

## Health Checks

```bash
GET /api/ai/health          # AI Proxy health
GET /api/llm/health         # LLM Router health
GET /api/keys/health        # Key Pool health
```

## Key Management

```bash
GET  /api/keys/status                    # Pool statistics
GET  /api/keys/openrouter                # Get a healthy key
POST /api/keys/openrouter                # Add a key
  {"key": "sk-or-v1-...", "donorName": "..."}
DELETE /api/keys/openrouter              # Remove a key
  {"key": "sk-or-v1-..."}
```

## Flow Management

```bash
GET /api/llm/flows                       # List all flows
POST /api/llm/route                      # Get routing decision
  {same body as completion request}
```

## Available Flow Types

- `recognition-analysis` - Analyze mutual recognition networks
- `capacity-recommendation` - Recommend capacity distribution
- `code-generation` - Generate code
- `data-analysis` - Analyze data
- `chat` - General conversation (default)

## Rate Limits

- General API: 100 req/15min
- AI Endpoints: 20 req/15min
- Auth Endpoints: 5 req/15min
- Token Usage: 10,000 tokens/15min

## Response Format

Success:
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

Error:
```json
{
  "message": "Error description",
  "detail": "More details"
}
```

## Environment Setup

Create `.env`:
```env
JWT_SECRET=generate-with-openssl-rand-base64-32
JWT_EXPIRY=24h
MASTER_API_KEY=generate-with-openssl-rand-hex-32
OPENROUTER_KEYS=sk-or-v1-key1,sk-or-v1-key2
OPENROUTER_BASE_URL=https://openrouter.ai/api/v1
APP_URL=http://localhost:5173
NODE_ENV=development
```

## Start Server

```bash
bun install
bun run dev
```

Server starts at: http://localhost:5173

