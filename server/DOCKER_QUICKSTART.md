# Docker Quick Start Guide

Quick reference for running the Free Association containerized services.

## ⚠️ CRITICAL: Security Setup First

**Before running any services**, you MUST configure credentials:

```bash
# 1. Copy environment template
cp .env.example .env

# 2. Generate secure credentials
openssl rand -hex 32      # Copy output for MASTER_API_KEY
openssl rand -base64 48   # Copy output for JWT_SECRET

# 3. Edit .env file and paste the generated values
nano .env  # or your preferred editor

# Required variables:
# MASTER_API_KEY=<paste first generated value>
# JWT_SECRET=<paste second generated value>
# ALLOWED_ORIGINS=http://localhost:5173,http://localhost:5174  # For production: your domains
```

**Production Note:** In production, you MUST also set `ALLOWED_ORIGINS` to your actual domain(s), otherwise the server will refuse to start.

## TL;DR

```bash
# Setup credentials (see above) then:
# Start all services
cd server && docker-compose up -d

# Check status
cd server && docker-compose ps

# View logs
cd server && docker-compose logs -f

# Stop all services
cd server && docker-compose down
```

## The Three Services

1. **gun-relay** (port 8765) - Gun database relay
2. **holster-relay** (port 8766) - Holster database relay
3. **data-api** (port 8767) - REST API for database population

## Common Commands

### Start/Stop

```bash
# Start all services in background
cd server && docker-compose up -d

# Start and view logs
cd server && docker-compose up

# Stop all services
cd server && docker-compose down

# Stop and remove volumes (WARNING: deletes data)
cd server && docker-compose down -v

# Restart a specific service
docker-compose restart gun-relay
```

### Monitoring

```bash
# Check service health
cd server && docker-compose ps

# View all logs
cd server && docker-compose logs -f

# View specific service logs
cd server && docker-compose logs -f gun-relay

# Check resource usage
docker stats
```

### Rebuilding

```bash
# Rebuild all services
cd server && docker-compose build

# Rebuild without cache
cd server && docker-compose build --no-cache

# Rebuild and restart
cd server && docker-compose up -d --build
```

## Testing the Services

```bash
# Test Gun relay
curl http://localhost:8765/gun

# Test Holster relay
curl http://localhost:8766/health

# Test Data API
curl http://localhost:8767/health

# Seed databases
curl -X POST http://localhost:8767/gun/seed
curl -X POST http://localhost:8767/holster/seed
```

## Frontend Development

```bash
# 1. Start Docker services
cd server && docker-compose up -d

# 2. Copy environment config
cp .env.development .env.local

# 3. Run frontend
bun install
bun run dev
```

Frontend will connect to:
- Gun: `http://localhost:8765/gun`
- Holster: `ws://localhost:8766/holster`
- API: `http://localhost:8767`

## Troubleshooting

### Services won't start
```bash
cd server && docker-compose logs
cd server && docker-compose down
cd server && docker-compose up -d
```

### Port already in use
Edit `server/docker-compose.yml` and change port mappings:
```yaml
ports:
  - "9765:8765"  # Use port 9765 instead
```

### Need clean slate
```bash
cd server && docker-compose down -v
docker system prune -a
cd server && docker-compose up -d
```

### Check what's running
```bash
docker ps
cd server && docker-compose ps
```

## Data Persistence

Data is stored in Docker volumes:
- `gun-data` - Gun database
- `holster-data` - Holster database

To backup:
```bash
docker run --rm -v free-association_gun-data:/data -v $(pwd):/backup \
  alpine tar czf /backup/gun-backup.tar.gz -C /data .
```

## Individual Services

Run just one service:
```bash
cd server && docker-compose up -d gun-relay
cd server && docker-compose up -d holster-relay
cd server && docker-compose up -d data-api
```

## Production Notes

For production deployment, see `DEPLOYMENT.md` for:
- Environment configuration
- SSL/TLS setup
- Scaling strategies
- Cloud deployment options
- Security hardening

## Need Help?

- Full deployment guide: `DEPLOYMENT.md`
- Service-specific docs: Check each service's `README.md`
- Issues: https://github.com/playnet-org/free-association/issues
