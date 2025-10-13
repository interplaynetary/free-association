# OpenRouter Key Pooling Setup Guide

## Overview

This system supports **community-funded AI access** through key pooling. Contributors can donate their OpenRouter API keys (with credits) to a shared pool that serves all users.

## Why OpenRouter?

- ✅ **One key = 400+ models** - Contributors donate one OpenRouter key, not separate provider keys
- ✅ **Pay-as-you-go** - Add $10-$100 in credits per key
- ✅ **Automatic depletion detection** - System marks keys as "depleted" when credits run out
- ✅ **Round-robin rotation** - Load distributed evenly across all healthy keys
- ✅ **Usage tracking** - See exactly how much each key has spent

## How It Works

### Example: Community Key Pool

```
Alice donates: OpenRouter key with $50 credit
Bob donates: OpenRouter key with $100 credit  
Charlie donates: OpenRouter key with $25 credit

System behavior:
1. Requests rotate through all 3 keys (Alice → Bob → Charlie → Alice...)
2. Each request reports cost back to key pool
3. When Alice's $50 runs out → key marked as "depleted"
4. System continues with Bob and Charlie's keys
5. Alice can re-fund her key and mark it as "healthy" again
```

## Setup Instructions

### 1. Get an OpenRouter API Key

1. Go to [OpenRouter.ai](https://openrouter.ai)
2. Sign up for an account
3. Add credits ($10-$100 recommended)
4. Generate an API key (starts with `sk-or-v1-...`)

### 2. Add Key to Pool

**Option A: Via Environment Variable**

Edit `server/.env`:
```bash
# Add your key to the comma-separated list
OPENROUTER_KEYS=sk-or-v1-alice-key,sk-or-v1-bob-key,sk-or-v1-charlie-key
```

**Option B: Via REST API (Dynamic)**

```bash
curl -X POST http://localhost:8769/keys/openrouter \
  -H "Content-Type: application/json" \
  -d '{"key": "sk-or-v1-your-key-here"}'
```

### 3. Monitor Key Health

Check pool status:
```bash
curl http://localhost:8769/status
```

Response shows:
```json
{
  "openrouter": {
    "totalKeys": 3,
    "health": {
      "healthy": 2,
      "depleted": 1,
      "degraded": 0,
      "failed": 0,
      "rate_limited": 0
    },
    "stats": {
      "totalRequests": 450,
      "successRate": "98.67%",
      "totalCostToday": "$2.4567"
    }
  }
}
```

## Key Health States

| State | Description | Auto-Recovery |
|-------|-------------|---------------|
| **healthy** | Key working normally | N/A |
| **degraded** | Temporary issues | After 5 minutes |
| **rate_limited** | Hit rate limit | After 15 minutes |
| **depleted** | No credits remaining | **Manual only** |
| **failed** | Critical error | Manual only |

## Re-Funding a Depleted Key

When a key runs out of credits:

1. **Add credits** to your OpenRouter account
2. **Mark key as healthy** via API:

```bash
curl -X POST http://localhost:8769/health/openrouter \
  -H "Content-Type: application/json" \
  -d '{
    "key": "sk-or-v1-your-key",
    "status": "healthy"
  }'
```

## Removing a Key from Pool

```bash
curl -X DELETE http://localhost:8769/keys/openrouter \
  -H "Content-Type: application/json" \
  -d '{"key": "sk-or-v1-your-key"}'
```

## Cost Tracking

Each request reports cost back to the pool. View per-key spending:

```bash
curl http://localhost:8769/status
```

This shows `totalCostToday` for the pool and tracks individual key usage.

## Supported Models (via OpenRouter)

All models use the same key pool:

- **OpenAI**: `openai/gpt-4`, `openai/gpt-4-turbo`, `openai/gpt-3.5-turbo`
- **Anthropic**: `anthropic/claude-3-opus`, `anthropic/claude-3-sonnet`, `anthropic/claude-3-haiku`
- **Mistral**: `mistralai/mistral-large`, `mistralai/mistral-medium`, `mistralai/mistral-small`
- **+400 more models** - See [OpenRouter Models](https://openrouter.ai/models)

## Testing the System

1. **Start services**:
```bash
cd server
docker-compose up -d
```

2. **Add your OpenRouter key** to `.env`

3. **Get auth token**:
```bash
curl -X POST http://localhost:8767/auth/token \
  -H "Content-Type: application/json" \
  -d '{"apiKey": "dev-key-12345-change-in-production"}'
```

4. **Make a request**:
```bash
curl -X POST http://localhost:8767/api/ai/completion \
  -H "Authorization: Bearer YOUR_TOKEN" \
  -H "Content-Type: application/json" \
  -d '{
    "messages": [{"role": "user", "content": "Hello!"}],
    "maxTokens": 50
  }'
```

5. **Check routing decision** in response:
```json
{
  "choices": [...],
  "_routing": {
    "model": "openai/gpt-3.5-turbo",
    "provider": "openrouter",
    "category": "conversational",
    "reason": "Best match for conversational task",
    "responseTimeMs": 1234
  }
}
```

## FAQ

**Q: Can I use direct OpenAI/Anthropic keys instead?**  
A: Yes! The system supports both. OpenRouter keys are recommended for simplicity, but you can add direct provider keys to the pool as well.

**Q: What happens when all keys are depleted?**  
A: The system returns a 503 error. Contributors need to re-fund their keys or add new ones.

**Q: How do I see which key is being used?**  
A: Keys are rotated automatically. Check logs or pool status to see current rotation index.

**Q: Can I set a spending limit per key?**  
A: Not yet implemented. For now, add the credit amount you're comfortable donating to your OpenRouter account.

**Q: Does the system support weighted distribution?**  
A: Not yet. All healthy keys are rotated equally. This could be added based on remaining credits.

## Contributing Keys

To contribute to the Free Association AI pool:

1. Get an OpenRouter key with credits
2. Share your key with the pool administrator
3. Monitor your key's usage via the status endpoint
4. Re-fund when depleted if you'd like to continue contributing

Thank you for supporting community-funded AI access! 🌱

