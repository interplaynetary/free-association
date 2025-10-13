# Key Pool Manager

API key pool management service with health monitoring, round-robin rotation, and automatic failover for multiple LLM providers.

## Features

- 🔑 **Multi-Provider Support** - Manage keys for OpenAI, Anthropic, Mistral, Google, Meta
- 🔄 **Round-Robin Rotation** - Distribute load evenly across available keys
- 💚 **Health Monitoring** - Track key status (healthy/degraded/failed/rate-limited)
- 🔁 **Auto-Recovery** - Automatically recover degraded or rate-limited keys
- 📊 **Usage Statistics** - Track requests, success rates, and costs per key
- ➕ **Dynamic Management** - Add/remove keys via REST API without restart

## Configuration

Environment variables:
- `KEY_POOL_HOST` - Bind host (default: 0.0.0.0)
- `KEY_POOL_PORT` - Port (default: 8769)
- `HEALTH_CHECK_INTERVAL` - Health check interval in ms (default: 30000)

### Adding Keys

Keys are configured via environment variables in the format:
```bash
MODEL_NAME_KEYS=key1,key2,key3
```

Example:
```bash
GPT_4_KEYS=sk-proj-xxx,sk-proj-yyy
CLAUDE_3_OPUS_KEYS=sk-ant-xxx,sk-ant-yyy
MISTRAL_LARGE_KEYS=xxx,yyy
```

## Endpoints

### GET /keys/:model
Get a healthy key for a specific model.

**Response:**
```json
{
  "success": true,
  "model": "gpt-4",
  "key": "sk-proj-...",
  "pool": {
    "totalKeys": 3,
    "healthyKeys": 2
  }
}
```

### POST /health/:model
Report key health status after usage.

**Request:**
```json
{
  "key": "sk-proj-...",
  "status": "healthy",
  "error": null
}
```

Valid statuses: `healthy`, `degraded`, `failed`, `rate_limited`

### GET /status
Get status of all key pools.

**Response:**
```json
{
  "status": "ok",
  "timestamp": 1234567890,
  "models": {
    "gpt-4": {
      "totalKeys": 3,
      "health": {
        "healthy": 2,
        "degraded": 1,
        "failed": 0,
        "rate_limited": 0
      },
      "currentIndex": 1,
      "stats": {
        "totalRequests": 150,
        "successRate": "96.67%"
      }
    }
  }
}
```

### GET /health
Service health check.

### POST /keys/:model
Add a new key to a model pool (admin endpoint).

**Request:**
```json
{
  "key": "sk-proj-new-key"
}
```

### DELETE /keys/:model
Remove a key from a model pool (admin endpoint).

**Request:**
```json
{
  "key": "sk-proj-old-key"
}
```

## Key Health States

- **healthy** - Key is working normally
- **degraded** - Key experienced temporary issues, auto-recovers after 5 minutes
- **failed** - Key has critical issues, requires manual intervention
- **rate_limited** - Key hit rate limits, auto-recovers after 15 minutes

## Health Check Monitor

Background process runs every 30 seconds to:
- Auto-recover degraded keys after cooldown period
- Clear rate-limit status after window expires
- Update usage statistics
- Alert on persistent failures

## Running Locally

```bash
# Set API keys
export GPT_4_KEYS=sk-proj-xxx,sk-proj-yyy
export CLAUDE_3_OPUS_KEYS=sk-ant-xxx

bun install
bun start
```

## Running with Docker

```bash
docker build -t key-pool .
docker run -p 8769:8769 \
  -e GPT_4_KEYS=sk-proj-xxx,sk-proj-yyy \
  -e CLAUDE_3_OPUS_KEYS=sk-ant-xxx \
  key-pool
```

## Supported Models

- gpt-4, gpt-4-turbo, gpt-3.5-turbo
- claude-3-opus, claude-3-sonnet, claude-3-haiku
- mistral-large, mistral-medium, mistral-small
- gemini-pro, gemini-ultra
- llama-3-70b, llama-3-8b

