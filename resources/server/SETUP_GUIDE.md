# Setup Guide - After Server Migration

## Quick Start

1. **Install dependencies**:
   ```bash
   bun install
   ```

2. **Create environment file**:
   ```bash
   cp .env.example .env
   ```

3. **Edit `.env` file** with your configuration:
   ```env
   # Authentication (REQUIRED)
   JWT_SECRET=your-super-secret-jwt-key-change-me
   JWT_EXPIRY=24h
   MASTER_API_KEY=your-master-api-key-change-me
   
   # OpenRouter Configuration (REQUIRED for AI features)
   OPENROUTER_KEYS=sk-or-v1-your-key-1,sk-or-v1-your-key-2
   OPENROUTER_BASE_URL=https://openrouter.ai/api/v1
   
   # Application Configuration
   APP_URL=http://localhost:5173
   NODE_ENV=development
   ```

4. **Generate secure keys**:
   ```bash
   # Generate JWT secret
   openssl rand -base64 32
   
   # Generate API key
   openssl rand -hex 32
   ```

5. **Start the development server**:
   ```bash
   bun run dev
   ```

6. **Test the APIs**:
   ```bash
   # Check health
   curl http://localhost:5173/api/ai/health
   
   # Get key pool status
   curl http://localhost:5173/api/keys/status
   
   # List available flows
   curl http://localhost:5173/api/llm/flows
   ```

## Directory Structure

```
src/
├── lib/
│   └── server/                    # Server-side code
│       ├── schemas/               # Zod validation schemas
│       │   ├── completion.ts
│       │   ├── routing.ts
│       │   └── index.ts
│       ├── llm/                   # LLM routing logic
│       │   ├── flows.ts          # Flow definitions
│       │   └── router.ts         # Routing logic
│       ├── key-pool/              # API key management
│       │   └── manager.ts
│       └── middleware/            # Middleware utilities
│           ├── auth.ts           # Authentication
│           └── rate-limit.ts     # Rate limiting
└── routes/
    └── api/                       # API endpoints
        ├── ai/                    # AI Proxy
        │   ├── +server.ts        # API info
        │   ├── health/           # Health check
        │   ├── token/            # Token generation
        │   └── completion/       # Main AI endpoint
        ├── llm/                   # LLM Router
        │   ├── health/
        │   ├── flows/
        │   └── route/
        └── keys/                  # Key Pool
            ├── health/
            ├── status/
            ├── [model]/
            └── health/[model]/
```

## API Endpoints

### AI Proxy (`/api/ai`)

- `GET /api/ai` - API documentation
- `GET /api/ai/health` - Health check
- `POST /api/ai/token` - Generate JWT token
- `POST /api/ai/completion` - AI completions (main endpoint)

### LLM Router (`/api/llm`)

- `GET /api/llm/health` - Health check
- `GET /api/llm/flows` - List available flows
- `POST /api/llm/route` - Get routing decision

### Key Pool (`/api/keys`)

- `GET /api/keys/health` - Service health
- `GET /api/keys/status` - Pool status
- `GET /api/keys/:model` - Get a healthy key
- `POST /api/keys/:model` - Add a key
- `DELETE /api/keys/:model` - Remove a key
- `POST /api/keys/health/:model` - Report key health

## Usage Examples

### 1. Generate a Token

```bash
curl -X POST http://localhost:5173/api/ai/token \
  -H "Content-Type: application/json" \
  -d '{
    "apiKey": "your-master-api-key",
    "userId": "user123"
  }'
```

Response:
```json
{
  "token": "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9...",
  "expiresIn": "24h",
  "type": "Bearer"
}
```

### 2. Make an AI Completion Request

```bash
curl -X POST http://localhost:5173/api/ai/completion \
  -H "Content-Type: application/json" \
  -H "Authorization: Bearer YOUR_JWT_TOKEN" \
  -d '{
    "messages": [
      {"role": "user", "content": "What is free association?"}
    ],
    "maxTokens": 200,
    "temperature": 0.7
  }'
```

### 3. Use with API Key (alternative to JWT)

```bash
curl -X POST http://localhost:5173/api/ai/completion \
  -H "Content-Type: application/json" \
  -H "X-API-Key: your-master-api-key" \
  -d '{
    "prompt": "Explain mutual recognition",
    "maxTokens": 150
  }'
```

### 4. Use Typed Flows

```bash
curl -X POST http://localhost:5173/api/ai/completion \
  -H "Content-Type: application/json" \
  -H "Authorization: Bearer YOUR_TOKEN" \
  -d '{
    "requestType": "recognition-analysis",
    "analysisType": "mutual-recognition",
    "players": [
      {
        "id": "alice",
        "name": "Alice",
        "recognitions": {"bob": 30, "charlie": 20}
      },
      {
        "id": "bob",
        "name": "Bob",
        "recognitions": {"alice": 40, "charlie": 10}
      }
    ],
    "maxTokens": 500
  }'
```

### 5. Check Key Pool Status

```bash
curl http://localhost:5173/api/keys/status
```

Response:
```json
{
  "status": "ok",
  "timestamp": 1234567890,
  "pool": {
    "name": "openrouter",
    "totalKeys": 2,
    "health": {
      "healthy": 2,
      "degraded": 0,
      "failed": 0,
      "rate_limited": 0,
      "depleted": 0
    },
    "stats": {
      "totalRequests": 150,
      "successRate": "98.67%",
      "totalCostToday": "$0.0234"
    }
  }
}
```

## Authentication Methods

### Method 1: JWT Token (Recommended)

1. Generate a token using `/api/ai/token`
2. Use in `Authorization: Bearer <token>` header

**Pros:**
- Scoped to specific users
- Can include custom claims
- Time-limited

### Method 2: API Key

Use directly in `X-API-Key` header

**Pros:**
- Simpler for server-to-server
- No expiration
- One step authentication

## Rate Limits

### Default Limits:

- **General API**: 100 requests per 15 minutes per user/IP
- **AI Endpoints**: 20 requests per 15 minutes per user/IP
- **Auth Endpoints**: 5 attempts per 15 minutes per IP
- **Token Usage**: 10,000 tokens per 15 minutes per user/IP

### Rate Limit Headers:

Responses include:
- `X-RateLimit-Limit`: Maximum requests allowed
- `X-RateLimit-Remaining`: Requests remaining
- `X-RateLimit-Reset`: Time when limit resets

### Exceeding Limits:

Returns `429 Too Many Requests`:
```json
{
  "message": "Too many AI requests. Limit: 20 AI requests per 15 minutes.",
  "retryAfter": "450 seconds"
}
```

## Flow Types

Available request types for intelligent routing:

1. **`recognition-analysis`**: Analyzes mutual recognition networks
2. **`capacity-recommendation`**: Recommends capacity distribution
3. **`code-generation`**: Generates code with best practices
4. **`data-analysis`**: Analyzes data and provides insights
5. **`chat`**: General conversational AI (default)

Each flow selects optimal models and constructs appropriate prompts.

## Environment Variables Reference

| Variable | Required | Default | Description |
|----------|----------|---------|-------------|
| `JWT_SECRET` | Yes | - | Secret for signing JWT tokens |
| `JWT_EXPIRY` | No | `24h` | Token expiration time |
| `MASTER_API_KEY` | Yes | - | Master API key for authentication |
| `OPENROUTER_KEYS` | Yes | - | Comma-separated OpenRouter API keys |
| `OPENROUTER_BASE_URL` | No | `https://openrouter.ai/api/v1` | OpenRouter API base URL |
| `APP_URL` | No | `http://localhost:5173` | Application URL for OpenRouter |
| `NODE_ENV` | No | `development` | Environment mode |

## Troubleshooting

### "No OpenRouter keys available"

**Problem**: No API keys configured or all keys are unhealthy.

**Solution**:
1. Add keys to `.env`: `OPENROUTER_KEYS=sk-or-v1-...`
2. Check key health: `curl http://localhost:5173/api/keys/status`
3. Verify keys are valid on OpenRouter dashboard

### "JWT_SECRET not configured"

**Problem**: Missing JWT_SECRET environment variable.

**Solution**:
1. Generate a secret: `openssl rand -base64 32`
2. Add to `.env`: `JWT_SECRET=your-generated-secret`
3. Restart the server

### "Invalid API key"

**Problem**: Wrong API key provided.

**Solution**:
1. Check `MASTER_API_KEY` in `.env`
2. Use the correct key in `X-API-Key` header
3. Or generate a JWT token instead

### Rate Limit Errors

**Problem**: Exceeding rate limits.

**Solution**:
1. Wait for the retry period (shown in error)
2. Implement exponential backoff in your client
3. Use multiple API keys for higher throughput

## Production Deployment

### Security Checklist:

- [ ] Generate strong `JWT_SECRET` (32+ characters)
- [ ] Use complex `MASTER_API_KEY`
- [ ] Set `NODE_ENV=production`
- [ ] Configure multiple OpenRouter keys
- [ ] Set up proper CORS origins
- [ ] Enable HTTPS
- [ ] Set secure `APP_URL`
- [ ] Monitor rate limits
- [ ] Set up logging

### Deployment Steps:

1. Build the application:
   ```bash
   npm run build
   ```

2. Set production environment variables

3. Deploy to your hosting platform (Vercel, Netlify, etc.)

4. Test all endpoints in production

5. Monitor key pool health and costs

## Next Steps

1. ✅ Configure environment variables
2. ✅ Test all API endpoints
3. ⏹️ Update client code to use new API URLs
4. ⏹️ Decide on Gun/Holster relay strategy (see `GUN_HOLSTER_OPTIONS.md`)
5. ⏹️ Deploy to production
6. ⏹️ Set up monitoring and logging

## Support

For more information:
- **Server Migration**: See `SERVER_MIGRATION.md`
- **Gun/Holster Options**: See `GUN_HOLSTER_OPTIONS.md`
- **Architecture**: See `server/ARCHITECTURE.md` (archived)

