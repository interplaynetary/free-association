# Quick Start Guide

Get the Free Association secure infrastructure running in 5 minutes.

## What You're Starting

3 containerized services:
1. **gun-relay** (port 8765) - Gun database relay
2. **holster-relay** (port 8766) - Holster database relay
3. **data-api** (port 8767) - Secure API Gateway with Gun, Holster, and AI endpoints

## Prerequisites

- Docker 20.10+
- Docker Compose 1.29+

## Start Services

```bash
# Clone/navigate to project
cd free-association

# Start all 3 services
docker-compose up -d

# Check they're running
docker-compose ps
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

# Data API
curl http://localhost:8767/health
```

### View API Documentation

```bash
# Open in browser or:
curl http://localhost:8767/
```

## Authentication

All `/api/*` endpoints require authentication via **either**:
- **API Key**: `X-API-Key: your-key`
- **JWT Token**: `Authorization: Bearer token`

### Default Development Credentials

```
API Key: dev-key-12345-change-in-production
```

⚠️ **CHANGE THESE IN PRODUCTION!** See [SECURITY.md](SECURITY.md)

### Method 1: Using API Key

```bash
curl -H "X-API-Key: dev-key-12345-change-in-production" \
  http://localhost:8767/api/gun/get?path=users
```

### Method 2: Using JWT Token

**Step 1: Get JWT token**
```bash
curl -X POST http://localhost:8767/auth/token \
  -H "Content-Type: application/json" \
  -d '{"apiKey": "dev-key-12345-change-in-production", "userId": "test-user"}'
```

Response:
```json
{
  "token": "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9...",
  "expiresIn": "24h",
  "type": "Bearer"
}
```

**Step 2: Use token**
```bash
curl -H "Authorization: Bearer eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9..." \
  http://localhost:8767/api/gun/get?path=users
```

## Example Requests

### Gun Database

**Write data:**
```bash
curl -X POST http://localhost:8767/api/gun/put \
  -H "X-API-Key: dev-key-12345-change-in-production" \
  -H "Content-Type: application/json" \
  -d '{
    "path": "users/alice",
    "data": {"name": "Alice", "age": 30, "lastSeen": 1234567890}
  }'
```

**Read data:**
```bash
curl -H "X-API-Key: dev-key-12345-change-in-production" \
  "http://localhost:8767/api/gun/get?path=users/alice"
```

**Seed with sample data:**
```bash
curl -X POST http://localhost:8767/api/gun/seed \
  -H "X-API-Key: dev-key-12345-change-in-production"
```

### Holster Database

**Write data:**
```bash
curl -X POST http://localhost:8767/api/holster/put \
  -H "X-API-Key: dev-key-12345-change-in-production" \
  -H "Content-Type: application/json" \
  -d '{
    "path": "users/bob",
    "data": {"name": "Bob", "email": "bob@example.com"}
  }'
```

**Read data:**
```bash
curl -H "X-API-Key: dev-key-12345-change-in-production" \
  "http://localhost:8767/api/holster/get?path=users/bob"
```

**Seed with sample data:**
```bash
curl -X POST http://localhost:8767/api/holster/seed \
  -H "X-API-Key: dev-key-12345-change-in-production"
```

### AI Services (Mock)

**Chat completions:**
```bash
curl -X POST http://localhost:8767/api/ai/chat/completions \
  -H "X-API-Key: dev-key-12345-change-in-production" \
  -H "Content-Type: application/json" \
  -d '{
    "model": "mock-gpt-4",
    "messages": [
      {"role": "system", "content": "You are a helpful assistant."},
      {"role": "user", "content": "What is free association?"}
    ]
  }'
```

**Generate embeddings:**
```bash
curl -X POST http://localhost:8767/api/ai/embeddings \
  -H "X-API-Key: dev-key-12345-change-in-production" \
  -H "Content-Type: application/json" \
  -d '{
    "input": "Free association is a system of mutual recognition.",
    "model": "mock-text-embedding-ada-002"
  }'
```

**Analyze free-association data:**
```bash
curl -X POST http://localhost:8767/api/ai/analyze \
  -H "X-API-Key: dev-key-12345-change-in-production" \
  -H "Content-Type: application/json" \
  -d '{
    "type": "recognition",
    "data": {
      "players": ["alice", "bob", "charlie"]
    }
  }'
```

**List available models:**
```bash
curl -H "X-API-Key: dev-key-12345-change-in-production" \
  http://localhost:8767/api/ai/models
```

## Frontend Development

**Start Docker services:**
```bash
docker-compose up -d
```

**Copy environment config:**
```bash
cp .env.development .env.local
```

**Run frontend:**
```bash
bun install
bun run dev
```

Frontend connects to:
- Gun: `http://localhost:8765/gun`
- Holster: `ws://localhost:8766/holster`
- API: `http://localhost:8767`

## View Logs

```bash
# All services
docker-compose logs -f

# Specific service
docker-compose logs -f gun-relay
docker-compose logs -f holster-relay
docker-compose logs -f data-api
```

## Stop Services

```bash
# Stop all
docker-compose down

# Stop and remove volumes (⚠️ deletes data)
docker-compose down -v
```

## Common Issues

### Port already in use
```bash
# Check what's using the port
lsof -i :8767

# Or change ports in docker-compose.yml
```

### Authentication fails
- Verify API key matches: `dev-key-12345-change-in-production`
- Check header format: `X-API-Key: key` (no quotes)
- For JWT: token expires in 24h, get a new one

### Services won't start
```bash
# Check logs
docker-compose logs

# Rebuild
docker-compose down
docker-compose build --no-cache
docker-compose up -d
```

### Cannot connect to Gun/Holster
- Verify services are healthy: `docker-compose ps`
- Check connectivity: `curl http://localhost:8765/gun`
- Review logs: `docker-compose logs gun-relay`

## Rate Limits

Default limits (per IP):
- General endpoints: 100 requests / 15 minutes
- AI endpoints: 20 requests / 15 minutes
- Auth endpoint: 5 requests / 15 minutes

If you hit rate limits, wait 15 minutes or adjust in `data-api/middleware/security.js`.

## API Endpoints Reference

### Public (No Auth)
- `GET /` - API documentation
- `GET /health` - Health check
- `POST /auth/token` - Generate JWT

### Gun Database (Auth Required)
- `POST /api/gun/put` - Write data
- `GET /api/gun/get?path=...` - Read data
- `POST /api/gun/seed` - Seed database

### Holster Database (Auth Required)
- `POST /api/holster/put` - Write data
- `GET /api/holster/get?path=...` - Read data
- `POST /api/holster/seed` - Seed database

### AI Services (Auth Required, Mock)
- `POST /api/ai/chat/completions` - Chat with AI
- `POST /api/ai/embeddings` - Generate embeddings
- `POST /api/ai/analyze` - Analyze data
- `GET /api/ai/models` - List models

## Next Steps

1. **Explore the API** - Try different endpoints
2. **Read the docs** - See [data-api/README.md](data-api/README.md)
3. **Integrate real AI** - Replace mock with OpenAI/Claude
4. **Production setup** - Follow [SECURITY.md](SECURITY.md)
5. **Deploy** - See [DEPLOYMENT.md](DEPLOYMENT.md)

## Security Warning

⚠️ **Default credentials are for DEVELOPMENT only!**

Before production:
1. Generate strong API keys: `openssl rand -hex 32`
2. Generate strong JWT secret: `openssl rand -base64 32`
3. Update `.env` or environment variables
4. Set up HTTPS/WSS
5. Review [SECURITY.md](SECURITY.md)

## Need Help?

- Full deployment guide: [DEPLOYMENT.md](DEPLOYMENT.md)
- Security setup: [SECURITY.md](SECURITY.md)
- API documentation: [data-api/README.md](data-api/README.md)
- Docker reference: [DOCKER_QUICKSTART.md](DOCKER_QUICKSTART.md)
- Architecture: [ARCHITECTURE.md](ARCHITECTURE.md)

## Summary

You now have:
- ✅ Gun relay running on port 8765
- ✅ Holster relay running on port 8766
- ✅ Secure API gateway on port 8767
- ✅ Authentication (API key + JWT)
- ✅ Rate limiting & security
- ✅ Mock AI endpoints ready for integration

**All three services are isolated, scalable, and production-ready!**
