# Quick Start Guide

Get the Free Association secure infrastructure running in minutes.

## What You're Starting

3 containerized services:
1. **gun-relay** (port 8765) - Gun database relay
2. **holster-relay** (port 8766) - Holster database relay
3. **data-api** (port 8767) - Secure AI Proxy Gateway (LLM/AI completions only)

## Prerequisites

- Docker 20.10+
- Docker Compose 1.29+
- [Bun](https://bun.sh/) if running locally outside Docker

## Start Services

```bash
# Clone/navigate to project
cd free-association

# Start all 3 services
cd server && docker-compose up -d

# Check they're running
cd server && docker-compose ps
```

Expected output:
```
NAME            STATUS     PORTS
gun-relay       Up         0.0.0.0:8765->8765/tcp
holster-relay   Up         0.0.0.0:8766->8766/tcp
data-api        Up         0.0.0.0:8767->8767/tcp
```

## Test the Services

### Health Checks

```bash
# Gun relay
curl http://localhost:8765/gun

# Holster relay
curl http://localhost:8766/health

# Data API (AI Proxy)
curl http://localhost:8767/health
```

### View API Documentation

```bash
# Open in browser or:
curl http://localhost:8767/
```

## Authentication

All `/api/ai/*` endpoints require authentication via **either**:
- **API Key**: `X-API-Key: your-key`
- **JWT Token**: `Authorization: Bearer token`

### Development Credentials

```
API Key: dev-key-12345-change-in-production
```

⚠️ **CHANGE THESE IN PRODUCTION!**

### Get a JWT token (for Authorization: Bearer ...)
```bash
curl -X POST http://localhost:8767/auth/token \
  -H "Content-Type: application/json" \
  -d '{"apiKey": "dev-key-12345-change-in-production", "userId": "test-user"}'
```

## AI Completion Example

```bash
curl -X POST http://localhost:8767/api/ai/completion \
  -H "Authorization: Bearer <token>" \
  -H "Content-Type: application/json" \
  -d '{ "model": "gpt-3.5-turbo", "prompt": "Hello, AI!", "max_tokens": 32 }'
```
Or:
```bash
curl -X POST http://localhost:8767/api/ai/completion \
  -H "X-API-Key: dev-key-12345-change-in-production" \
  -H "Content-Type: application/json" \
  -d '{ "model": "gpt-3.5-turbo", "prompt": "Tell me a joke", "max_tokens": 32 }'
```

## View Logs

```bash
# All services
cd server && docker-compose logs -f

# Specific service
cd server && docker-compose logs -f gun-relay
cd server && docker-compose logs -f holster-relay
cd server && docker-compose logs -f data-api
```

## Stop Services

```bash
# Stop all
cd server && docker-compose down

# Stop and remove volumes (⚠️ deletes data)
cd server && docker-compose down -v
```

## Common Issues

### Port already in use
```bash
lsof -i :8767
# Or change ports in server/docker-compose.yml
```

### Authentication fails
- Verify API key matches: `dev-key-12345-change-in-production`
- Check header format: `X-API-Key: key` (no quotes)
- For JWT: token expires in 24h, get a new one

### Services won't start
```bash
# Check logs
cd server && docker-compose logs

# Rebuild
cd server && docker-compose down
cd server && docker-compose build --no-cache
cd server && docker-compose up -d
```

### Cannot connect to Gun/Holster
- Verify services are healthy: `cd server && docker-compose ps`
- Check connectivity: `curl http://localhost:8765/gun`
- Review logs: `cd server && docker-compose logs gun-relay`

## Rate Limits

Default limits (per source):
- General endpoints: 100 requests / 15 minutes
- AI endpoints: 20 requests / 15 minutes
- Auth endpoint: 5 requests / 15 minutes
- AI completions: 10,000 tokens / 15 minutes

If you hit rate limits, wait 15 minutes or adjust in `data-api/middleware/security.js`.

## API Endpoints Reference

### Public (No Auth)
- `GET /` - API documentation
- `GET /health` - Health check
- `POST /auth/token` - Generate JWT

### AI Services (Auth Required)
- `POST /api/ai/completion` - Completion/LLM proxy endpoint (OpenAI-compatible)

## Next Steps

1. **Explore the API** - Try /api/ai/completion
2. **Read the docs** - See [data-api/README.md](data-api/README.md)
3. **Production setup** - Follow [SECURITY.md](SECURITY.md)
4. **Deploy** - See [DEPLOYMENT.md](DEPLOYMENT.md)

## Security Warning

⚠️ **Default credentials are for DEVELOPMENT only!**

Before production:
1. Generate strong API keys: `openssl rand -hex 32`
2. Generate strong JWT secret: `openssl rand -base64 32`
3. Update `.env` or environment variables
4. Set up HTTPS/WSS
5. Review [SECURITY.md](SECURITY.md)

## Need Help?

- Deployment guide: [DEPLOYMENT.md](DEPLOYMENT.md)
- Security setup: [SECURITY.md](SECURITY.md)
- API documentation: [data-api/README.md](data-api/README.md)
- Docker reference: [DOCKER_QUICKSTART.md](DOCKER_QUICKSTART.md)
- Architecture: [ARCHITECTURE.md](ARCHITECTURE.md)

## Summary

You now have:
- ✅ Gun relay running on port 8765
- ✅ Holster relay running on port 8766
- ✅ AI proxy gateway on port 8767
- ✅ Authentication (API key + JWT)
- ✅ Rate limiting & security
- ✅ AI completion endpoint ready for integration

**All three services are isolated, scalable, and production-ready!**
