# Secure API Gateway

**Critical Infrastructure** - Production-ready API gateway for Gun and Holster databases.

## Overview

Secure access to:
1. **Gun Database** - Decentralized graph database
2. **Holster Database** - Alternative decentralized database

## Security Features

✅ API Key + JWT authentication
✅ Rate limiting (100 requests / 15 minutes)
✅ Helmet security headers
✅ CORS protection
✅ Error handling & logging

## Quick Start

```bash
npm install
cp .env.example .env
# Edit .env - CHANGE SECURITY CREDENTIALS!
npm start
```

## Authentication

All `/api/*` endpoints require **either**:
- API Key: `X-API-Key: your-key`
- JWT: `Authorization: Bearer token`

Get JWT:
```bash
curl -X POST http://localhost:8767/auth/token \
  -H "Content-Type: application/json" \
  -d '{"apiKey": "dev-key-12345-change-in-production"}'
```

## Endpoints

### Gun Database
- `POST /api/gun/put` - Write data
- `GET /api/gun/get?path=...` - Read data
- `POST /api/gun/seed` - Seed with sample data

### Holster Database
- `POST /api/holster/put` - Write data
- `GET /api/holster/get?path=...` - Read data
- `POST /api/holster/seed` - Seed with sample data

### Public
- `GET /` - API documentation
- `GET /health` - Health check
- `POST /auth/token` - Generate JWT

## Example Usage

```bash
# Write to Gun
curl -X POST http://localhost:8767/api/gun/put \
  -H "X-API-Key: dev-key-12345-change-in-production" \
  -H "Content-Type: application/json" \
  -d '{
    "path": "users/alice",
    "data": {"name": "Alice", "age": 30}
  }'

# Read from Gun
curl -H "X-API-Key: dev-key-12345-change-in-production" \
  "http://localhost:8767/api/gun/get?path=users/alice"

# Write to Holster
curl -X POST http://localhost:8767/api/holster/put \
  -H "X-API-Key: dev-key-12345-change-in-production" \
  -H "Content-Type: application/json" \
  -d '{
    "path": "users/bob",
    "data": {"name": "Bob", "email": "bob@example.com"}
  }'
```

## Production Setup

### 1. Generate Secure Credentials

```bash
# API Key
openssl rand -hex 32

# JWT Secret
openssl rand -base64 32
```

### 2. Update .env

```env
NODE_ENV=production
MASTER_API_KEY=your-secure-key-here
JWT_SECRET=your-jwt-secret-minimum-32-chars
GUN_PEER_URL=https://gun.yourdomain.com/gun
HOLSTER_PEER_URL=wss://holster.yourdomain.com/holster
ALLOWED_ORIGINS=https://yourdomain.com
```

### 3. Deploy Behind HTTPS

Use Nginx reverse proxy:
```nginx
server {
    listen 443 ssl http2;
    server_name api.yourdomain.com;

    ssl_certificate /path/to/cert.pem;
    ssl_certificate_key /path/to/key.pem;

    location / {
        proxy_pass http://localhost:8767;
        proxy_set_header Host $host;
        proxy_set_header X-Real-IP $remote_addr;
    }
}
```

## Troubleshooting

**Auth fails**: Check API key matches `MASTER_API_KEY` in .env

**Rate limit**: Wait 15 minutes or increase limits in `middleware/security.js`

**CORS**: Add origin to `ALLOWED_ORIGINS` in .env

**Cannot connect**: Verify Gun/Holster relays are running

## Security Best Practices

- ✅ Use HTTPS in production
- ✅ Rotate API keys regularly
- ✅ Use strong, random JWT secrets (32+ chars)
- ✅ Never commit .env files
- ✅ Monitor rate limits and logs
- ✅ Use different keys for dev/staging/prod

## Project Structure

```
data-api/
├── server.js              # Main server
├── middleware/
│   ├── auth.js           # API key + JWT auth
│   └── security.js       # Rate limiting, helmet, CORS
└── routes/
    ├── gun.js            # Gun database endpoints
    └── holster.js        # Holster database endpoints
```

For detailed API documentation, start the server and visit `http://localhost:8767/`
