# AI Proxy Gateway

Production-ready API gateway for securely proxying requests to an AI (LLM) backend such as OpenAI, local LLM, or other service.

## Overview

- Secure, authenticated REST API forwarding requests to the configured AI backend.
- Designed to restrict/monitor AI usage by API key and/or JWT.
- Rate limiting, CORS, security headers (Helmet), robust error handling.

## Features

- 🔐 API Key or JWT authentication for all endpoints
- ⏳ General and stricter AI endpoint rate limits (100/15min and 20/15min)
- 🦾 POST `/api/ai/completion`: Proxy to LLM/completion endpoint (configurable URL)
- ⚠️ CORS and security headers (Helmet)
- 🩺 `/health` endpoint
- 🧾 `/` API self-documentation
- 📗 Easily extensible for other AI functions (chat, images, etc)

## Quick Start

```bash
bun install
bun run server.js
```

## Configuration

Environment variables:
- `AI_PROXY_HOST` - Bind host (default: 0.0.0.0)
- `AI_PROXY_PORT` - Port (default: 8767)
- `MASTER_API_KEY` - Required strong API key
- `JWT_SECRET` - JWT signing secret
- `AI_API_URL` - Your backend AI completion endpoint (e.g., https://api.openai.com/v1/completions or local Ollama endpoint)
- `AI_API_KEY` - (Optional) Secret to authenticate to backend (sent as Bearer)
- `ALLOWED_ORIGINS` - For CORS
- `NODE_ENV` - Should be set to production in prod

## Endpoints

- `GET /` - API self-documentation
- `GET /health` - Service/health check
- `POST /auth/token` - Issue JWT tokens (send `{apiKey}` in body)
- `POST /api/ai/completion` - Proxy completions (send OpenAI-compatible JSON)

## Example Usage

Authenticate and query completions:

```bash
# Get a JWT
curl -X POST http://localhost:8767/auth/token \
  -H "Content-Type: application/json" \
  -d '{"apiKey": "your-master-key"}'

# Use JWT or API key for AI completion
curl -X POST http://localhost:8767/api/ai/completion \
  -H "Authorization: Bearer <token>" \
  -H "Content-Type: application/json" \
  -d '{ "model": "gpt-3.5-turbo", "prompt": "Once upon a time...", "max_tokens": 100 }'

# Or with API key:
curl -X POST http://localhost:8767/api/ai/completion \
  -H "X-API-Key: your-master-key" \
  -H "Content-Type: application/json" \
  -d '{ "model": "gpt-3.5-turbo", "prompt": "Tell me a joke", "max_tokens": 32 }'
```

## Security Best Practices
- Use strong, unguessable `MASTER_API_KEY` (openssl rand -hex 32)
- Use a unique, long `JWT_SECRET`
- Restrict CORS origins appropriately
- Monitor logs and rate limits

## For Local/Private LLMs
If targeting a local endpoint (e.g., Ollama, vLLM), set `AI_API_URL` in `.env` to your AI backend.

## Project Structure

```
ai-proxy/
├── server.js              # Main server
├── middleware/            # Auth, rate-limiting, security
├── utils/                 # (If used)
├── package.json
├── Dockerfile (uses Bun)
```

For configuration or bug reports, see this repo's main tracker.
