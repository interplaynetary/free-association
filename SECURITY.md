# Security Guide - Free Association Infrastructure

**This is critical infrastructure.** Follow these security guidelines carefully.

## Overview

The Free Association infrastructure consists of 3 services:
1. **gun-relay** - Gun database relay
2. **holster-relay** - Holster database relay
3. **data-api** - Secure API gateway with AI proxy

## Security Architecture

```
                  Internet
                     │
                     │ HTTPS/WSS
                     ▼
            ┌────────────────┐
            │  Reverse Proxy │ (Nginx/Cloudflare)
            │  - SSL/TLS     │
            │  - Rate Limit  │
            │  - DDoS Protect│
            └────────────────┘
                     │
        ┌────────────┼────────────┐
        │            │            │
        ▼            ▼            ▼
   ┌────────┐  ┌─────────┐  ┌──────────┐
   │  Gun   │  │ Holster │  │ Data API │
   │ :8765  │  │  :8766  │  │  :8767   │
   │        │  │         │  │ + Auth   │
   │        │  │         │  │ + Rate   │
   └────────┘  └─────────┘  └──────────┘
        │            │            │
        └────────────┴────────────┘
           Docker Network
```

## Critical Security Setup

### 1. API Gateway Security (data-api)

#### Generate Strong Credentials

**API Key (32+ characters):**
```bash
# Linux/Mac
openssl rand -hex 32

# Or using Node.js
node -e "console.log(require('crypto').randomBytes(32).toString('hex'))"
```

**JWT Secret (32+ characters):**
```bash
openssl rand -base64 32
```

#### Configure Environment

**Development (.env):**
```env
MASTER_API_KEY=dev-key-for-testing-only
JWT_SECRET=dev-secret-for-testing-only
NODE_ENV=development
DEBUG=true
```

**Production (.env or secrets manager):**
```env
MASTER_API_KEY=<your-secure-64-char-random-key>
JWT_SECRET=<your-secure-64-char-random-secret>
NODE_ENV=production
DEBUG=false

# Restrict CORS to your domains only
ALLOWED_ORIGINS=https://app.yourdomain.com,https://yourdomain.com
```

### 2. Authentication Methods

The data-api supports two authentication methods:

#### API Key Authentication
```bash
curl -H "X-API-Key: your-api-key" \
  http://localhost:8767/api/gun/get?path=users
```

**Best for:**
- Server-to-server communication
- Backend services
- Automated scripts

#### JWT Token Authentication
```bash
# 1. Get JWT token
curl -X POST http://localhost:8767/auth/token \
  -H "Content-Type: application/json" \
  -d '{"apiKey": "your-master-key", "userId": "user123"}'

# 2. Use token
curl -H "Authorization: Bearer eyJhbGc..." \
  http://localhost:8767/api/gun/get?path=users
```

**Best for:**
- Frontend applications
- Mobile apps
- User-specific access

### 3. Rate Limiting

Built-in rate limits (per IP address):

| Endpoint Type | Limit | Window |
|--------------|-------|---------|
| General API | 100 requests | 15 minutes |
| AI Endpoints | 20 requests | 15 minutes |
| Auth Endpoint | 5 requests | 15 minutes |

**Adjust for production:**

Edit `data-api/middleware/security.js`:
```javascript
export const generalLimiter = rateLimit({
  windowMs: 15 * 60 * 1000,
  max: 1000  // Increase for production
});
```

### 4. CORS Configuration

**Development:**
```env
ALLOWED_ORIGINS=http://localhost:5173,http://localhost:5174
```

**Production:**
```env
# Only allow your production domains
ALLOWED_ORIGINS=https://app.yourdomain.com,https://yourdomain.com
```

**Additional CORS hardening:**

Edit `data-api/middleware/security.js`:
```javascript
export function configureCors() {
  return {
    origin: (origin, callback) => {
      const allowedOrigins = process.env.ALLOWED_ORIGINS.split(',');

      // Reject if origin not in whitelist
      if (!origin || allowedOrigins.includes(origin)) {
        callback(null, true);
      } else {
        callback(new Error('Not allowed by CORS'));
      }
    },
    credentials: true,
    methods: ['GET', 'POST'], // Restrict methods
    allowedHeaders: ['Content-Type', 'Authorization', 'X-API-Key']
  };
}
```

### 5. HTTPS/TLS Setup

**Never run in production without HTTPS!**

#### Option A: Reverse Proxy (Recommended)

**Nginx configuration:**
```nginx
# /etc/nginx/sites-available/free-association

upstream gun_relay {
    server localhost:8765;
}

upstream holster_relay {
    server localhost:8766;
}

upstream data_api {
    server localhost:8767;
}

# Gun relay
server {
    listen 443 ssl http2;
    server_name gun.yourdomain.com;

    ssl_certificate /etc/letsencrypt/live/yourdomain.com/fullchain.pem;
    ssl_certificate_key /etc/letsencrypt/live/yourdomain.com/privkey.pem;
    ssl_protocols TLSv1.2 TLSv1.3;
    ssl_ciphers HIGH:!aNULL:!MD5;

    location / {
        proxy_pass http://gun_relay;
        proxy_set_header Host $host;
        proxy_set_header X-Real-IP $remote_addr;
        proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
        proxy_set_header X-Forwarded-Proto $scheme;
    }
}

# Holster relay (WebSocket)
server {
    listen 443 ssl http2;
    server_name holster.yourdomain.com;

    ssl_certificate /etc/letsencrypt/live/yourdomain.com/fullchain.pem;
    ssl_certificate_key /etc/letsencrypt/live/yourdomain.com/privkey.pem;

    location / {
        proxy_pass http://holster_relay;
        proxy_http_version 1.1;
        proxy_set_header Upgrade $http_upgrade;
        proxy_set_header Connection "upgrade";
        proxy_set_header Host $host;
    }
}

# Data API
server {
    listen 443 ssl http2;
    server_name api.yourdomain.com;

    ssl_certificate /etc/letsencrypt/live/yourdomain.com/fullchain.pem;
    ssl_certificate_key /etc/letsencrypt/live/yourdomain.com/privkey.pem;

    # Additional security headers
    add_header Strict-Transport-Security "max-age=31536000; includeSubDomains" always;
    add_header X-Frame-Options "DENY" always;
    add_header X-Content-Type-Options "nosniff" always;
    add_header X-XSS-Protection "1; mode=block" always;

    location / {
        proxy_pass http://data_api;
        proxy_set_header Host $host;
        proxy_set_header X-Real-IP $remote_addr;
        proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
        proxy_set_header X-Forwarded-Proto $scheme;
    }
}
```

**Get SSL certificates:**
```bash
# Install Certbot
sudo apt install certbot python3-certbot-nginx

# Get certificates
sudo certbot --nginx -d gun.yourdomain.com \
  -d holster.yourdomain.com \
  -d api.yourdomain.com
```

#### Option B: Cloudflare (Easiest)

1. Add your domain to Cloudflare
2. Point DNS records:
   - `gun.yourdomain.com` → your server IP
   - `holster.yourdomain.com` → your server IP
   - `api.yourdomain.com` → your server IP
3. Enable "Full (strict)" SSL mode
4. Enable "Always Use HTTPS"
5. Configure firewall rules to restrict access

### 6. Network Security

#### Firewall Rules (ufw example)

```bash
# Allow SSH
sudo ufw allow 22/tcp

# Allow HTTP/HTTPS (for reverse proxy)
sudo ufw allow 80/tcp
sudo ufw allow 443/tcp

# Block direct access to service ports
sudo ufw deny 8765/tcp
sudo ufw deny 8766/tcp
sudo ufw deny 8767/tcp

# Enable firewall
sudo ufw enable
```

#### Docker Network Isolation

Services are isolated in Docker network `free-association-network`:
- Services can communicate internally
- Only exposed ports accessible from host
- Add additional networks for further isolation

```yaml
# docker-compose.yml
networks:
  public-network:
    driver: bridge
  internal-network:
    driver: bridge
    internal: true  # No external access

services:
  data-api:
    networks:
      - public-network
      - internal-network

  gun-relay:
    networks:
      - internal-network  # Only internal
```

### 7. Secrets Management

**Never commit secrets to Git!**

#### Option A: Environment Variables

```bash
# Load from file
export $(cat .env | xargs)
docker-compose up -d
```

#### Option B: Docker Secrets

```yaml
# docker-compose.yml
secrets:
  api_key:
    external: true
  jwt_secret:
    external: true

services:
  data-api:
    secrets:
      - api_key
      - jwt_secret
```

```bash
# Create secrets
echo "my-secure-api-key" | docker secret create api_key -
echo "my-jwt-secret" | docker secret create jwt_secret -
```

#### Option C: Secrets Manager (AWS/GCP/Azure)

**AWS Secrets Manager:**
```javascript
// data-api/server.js
import { SecretsManagerClient, GetSecretValueCommand } from "@aws-sdk/client-secrets-manager";

const client = new SecretsManagerClient({ region: "us-east-1" });
const secret = await client.send(
  new GetSecretValueCommand({ SecretId: "free-association/api-key" })
);
process.env.MASTER_API_KEY = JSON.parse(secret.SecretString).apiKey;
```

### 8. Logging & Monitoring

#### Enable Logging

```env
DEBUG=true  # Development only!
NODE_ENV=production
```

#### Structured Logging

Add Winston or Pino:

```bash
npm install winston
```

```javascript
// data-api/utils/logger.js
import winston from 'winston';

export const logger = winston.createLogger({
  level: 'info',
  format: winston.format.json(),
  transports: [
    new winston.transports.File({ filename: 'error.log', level: 'error' }),
    new winston.transports.File({ filename: 'combined.log' })
  ]
});

if (process.env.NODE_ENV !== 'production') {
  logger.add(new winston.transports.Console({
    format: winston.format.simple()
  }));
}
```

#### Monitor Rate Limits

```bash
# Watch for rate limit hits
docker logs data-api -f | grep "Too many requests"

# Set up alerts (example with Datadog)
```

### 9. API Key Rotation

**Rotate keys every 90 days:**

```bash
# 1. Generate new key
NEW_KEY=$(openssl rand -hex 32)

# 2. Add new key to system (supports multiple keys)
# Update data-api/middleware/auth.js to load from database

# 3. Update clients with new key

# 4. Remove old key after grace period
```

### 10. Security Checklist

#### Before Production Deployment

- [ ] Changed `MASTER_API_KEY` from default
- [ ] Changed `JWT_SECRET` from default (32+ chars)
- [ ] Set `NODE_ENV=production`
- [ ] Set `DEBUG=false`
- [ ] Configured `ALLOWED_ORIGINS` to production domains only
- [ ] Enabled HTTPS/WSS (via reverse proxy or certificates)
- [ ] Configured firewall rules
- [ ] Set up monitoring and alerts
- [ ] Tested rate limiting
- [ ] Reviewed CORS configuration
- [ ] Secrets stored securely (not in git)
- [ ] Different credentials for dev/staging/prod
- [ ] Regular backup schedule configured
- [ ] Incident response plan documented

#### Ongoing Security

- [ ] Rotate API keys every 90 days
- [ ] Rotate JWT secrets every 180 days
- [ ] Update dependencies monthly (`npm audit`)
- [ ] Review access logs weekly
- [ ] Monitor rate limit hits
- [ ] Test security annually (penetration testing)
- [ ] Review and update CORS whitelist
- [ ] Check for unauthorized access attempts

## Security Incident Response

### If API Key is Compromised:

1. **Immediately rotate the key:**
   ```bash
   # Generate new key
   openssl rand -hex 32

   # Update environment variables
   # Restart services
   docker-compose restart data-api
   ```

2. **Review access logs:**
   ```bash
   docker logs data-api --since 24h | grep "401\|403"
   ```

3. **Notify users/services** using the old key

4. **Investigate breach** - check for unauthorized data access

### If JWT Secret is Compromised:

1. **Rotate JWT secret immediately**
2. **All existing tokens become invalid** (users must re-authenticate)
3. **Review recent token issuances**
4. **Check for suspicious activity**

### If Rate Limit is Bypassed:

1. **Check for distributed attacks** (multiple IPs)
2. **Implement IP-based blocking:**
   ```nginx
   # Nginx
   deny 1.2.3.4;
   deny 5.6.7.0/24;
   ```
3. **Consider Cloudflare or DDoS protection**
4. **Temporarily increase rate limits if legitimate traffic**

## Security Resources

- OWASP Top 10: https://owasp.org/www-project-top-ten/
- Node.js Security Best Practices: https://nodejs.org/en/docs/guides/security/
- Docker Security: https://docs.docker.com/engine/security/
- Let's Encrypt: https://letsencrypt.org/

## Reporting Security Issues

**Do not file public GitHub issues for security vulnerabilities.**

Email security concerns to: [your-security-email]

Include:
- Description of vulnerability
- Steps to reproduce
- Potential impact
- Suggested fix (if any)

## Questions?

For security questions or concerns, contact the infrastructure team before deploying to production.

**Remember: This is critical infrastructure. When in doubt, over-secure rather than under-secure.**
