# Free Association - Deployment Guide

This guide explains how to deploy the Free Association project with its three containerized services.

## Architecture Overview

The project has been split into 3 independent containerized services:

1. **gun-relay** - Gun database relay server (port 8765)
2. **holster-relay** - Holster database relay server (port 8766)
3. **data-api** - REST API for database population (port 8767)

Each service is independently deployable and can be scaled separately. The services communicate over a Docker network.

## Project Structure

```
free-association/
├── gun-relay/           # Gun relay server
│   ├── Dockerfile
│   ├── server.js
│   ├── package.json
│   └── README.md
├── holster-relay/       # Holster relay server
│   ├── Dockerfile
│   ├── server.js
│   ├── package.json
│   └── README.md
├── data-api/            # Data API server
│   ├── Dockerfile
│   ├── server.js
│   ├── package.json
│   └── README.md
├── docker-compose.yml   # Orchestrates all services
├── .env.docker          # Docker environment template
└── src/                 # Frontend application
    └── lib/
        ├── config.ts    # Configuration module
        └── state/
            ├── gun.svelte.ts      # Now uses config
            └── holster.svelte.ts  # Now uses config
```

## Quick Start (Docker Compose)

### Prerequisites

- Docker 20.10+
- Docker Compose 1.29+

### Steps

0. **CRITICAL: Configure security credentials first:**

```bash
# Copy environment template
cp .env.example .env

# Generate secure credentials
openssl rand -hex 32      # For MASTER_API_KEY
openssl rand -base64 48   # For JWT_SECRET

# Edit .env file and set:
# - MASTER_API_KEY (required, 32+ characters)
# - JWT_SECRET (required, 32+ characters)
# - ALLOWED_ORIGINS (required in production)
# - NODE_ENV=production (for production deployments)
```

**⚠️ Production Requirements:**
- `MASTER_API_KEY`: Must be 32+ characters, high entropy
- `JWT_SECRET`: Must be 32+ characters, no repeating patterns
- `ALLOWED_ORIGINS`: MUST be set to your domain(s), comma-separated
- `NODE_ENV`: Set to `production` to enable fail-fast validation

The server will refuse to start in production without proper credentials.

1. **Build and start all services:**

```bash
docker-compose up -d
```

2. **Check service health:**

```bash
docker-compose ps
```

You should see all three services running:
- gun-relay (healthy) on port 8765
- holster-relay (healthy) on port 8766
- data-api (healthy) on port 8767

3. **View logs:**

```bash
# All services
docker-compose logs -f

# Specific service
docker-compose logs -f gun-relay
docker-compose logs -f holster-relay
docker-compose logs -f data-api
```

4. **Test the services:**

```bash
# Test Gun relay
curl http://localhost:8765/gun

# Test Holster relay health
curl http://localhost:8766/health

# Test Data API
curl http://localhost:8767/health
```

5. **Stop services:**

```bash
docker-compose down
```

6. **Stop and remove volumes (wipes data):**

```bash
docker-compose down -v
```

## Service Details

### Gun Relay (Port 8765)

Gun database relay with persistent storage.

**Endpoints:**
- `http://localhost:8765/gun` - Gun database endpoint
- Gun WebRTC for peer-to-peer connections

**Data persistence:** Stored in Docker volume `gun-data`

**Configuration:**
```bash
GUN_RELAY_HOST=0.0.0.0
GUN_RELAY_PORT=8765
GUN_RELAY_STORE=true
GUN_RELAY_PATH=public
GUN_RELAY_SHOW_QR=false
```

### Holster Relay (Port 8766)

Holster database relay with WebSocket support.

**Endpoints:**
- `ws://localhost:8766/holster` - Holster WebSocket endpoint
- `http://localhost:8766/health` - Health check

**Data persistence:** Stored in Docker volume `holster-data`

**Configuration:**
```bash
HOLSTER_RELAY_HOST=0.0.0.0
HOLSTER_RELAY_PORT=8766
HOLSTER_RELAY_STORAGE=true
HOLSTER_RELAY_STORAGE_PATH=./holster-data
```

### Data API (Port 8767)

REST API for populating and querying both Gun and Holster databases.

**Endpoints:**

Health:
- `GET /health` - Service status

Gun operations:
- `POST /gun/put` - Write to Gun
- `GET /gun/get?path=...` - Read from Gun
- `POST /gun/seed` - Seed Gun with sample data

Holster operations:
- `POST /holster/put` - Write to Holster
- `GET /holster/get?path=...` - Read from Holster
- `POST /holster/seed` - Seed Holster with sample data

**Configuration:**
```bash
DATA_API_HOST=0.0.0.0
DATA_API_PORT=8767
GUN_PEER_URL=http://gun-relay:8765/gun
HOLSTER_PEER_URL=ws://holster-relay:8766/holster
```

## Frontend Configuration

The frontend now uses environment variables to connect to the relay servers.

### Development Setup

1. **Copy the development environment file:**

```bash
cp .env.development .env.local
```

2. **Start the Docker services:**

```bash
docker-compose up -d
```

3. **Run the frontend:**

```bash
bun install
bun run dev
```

The frontend will connect to:
- Gun relay: `http://localhost:8765/gun`
- Holster relay: `ws://localhost:8766/holster`
- Data API: `http://localhost:8767`

### Production Setup

1. **Update `.env.production` with your production URLs:**

```bash
VITE_GUN_PEER_URL=https://gun.yourdomain.com/gun
VITE_HOLSTER_PEER_URL=wss://holster.yourdomain.com/holster
VITE_DATA_API_URL=https://api.yourdomain.com
```

2. **Build the frontend:**

```bash
bun run build
```

## Individual Service Deployment

Each service can be deployed independently.

### Deploy Gun Relay Only

```bash
cd gun-relay
docker build -t gun-relay .
docker run -d \
  -p 8765:8765 \
  -v gun-data:/app/store \
  --name gun-relay \
  gun-relay
```

### Deploy Holster Relay Only

```bash
cd holster-relay
docker build -t holster-relay .
docker run -d \
  -p 8766:8766 \
  -v holster-data:/app/holster-data \
  --name holster-relay \
  holster-relay
```

### Deploy Data API Only

```bash
cd data-api
docker build -t data-api .
docker run -d \
  -p 8767:8767 \
  -e GUN_PEER_URL=http://gun-relay:8765/gun \
  -e HOLSTER_PEER_URL=ws://holster-relay:8766/holster \
  --name data-api \
  data-api
```

## Production Deployment

### Using Docker Swarm

1. **Initialize swarm:**

```bash
docker swarm init
```

2. **Deploy stack:**

```bash
docker stack deploy -c docker-compose.yml free-association
```

3. **Check services:**

```bash
docker stack services free-association
```

### Using Kubernetes

1. **Convert docker-compose to Kubernetes manifests using kompose:**

```bash
kompose convert -f docker-compose.yml
```

2. **Apply to cluster:**

```bash
kubectl apply -f .
```

### Using Cloud Providers

#### AWS ECS

Use AWS ECS with the Docker images:
1. Push images to ECR
2. Create ECS task definitions
3. Deploy as ECS services

#### Google Cloud Run

```bash
# Build and push
docker build -t gcr.io/PROJECT_ID/gun-relay ./gun-relay
docker push gcr.io/PROJECT_ID/gun-relay

# Deploy
gcloud run deploy gun-relay \
  --image gcr.io/PROJECT_ID/gun-relay \
  --port 8765
```

#### DigitalOcean App Platform

Create `app.yaml` and use:
```bash
doctl apps create --spec app.yaml
```

## Database Seeding

Use the Data API to populate databases:

```bash
# Seed Gun database
curl -X POST http://localhost:8767/gun/seed

# Seed Holster database
curl -X POST http://localhost:8767/holster/seed

# Custom data
curl -X POST http://localhost:8767/gun/put \
  -H "Content-Type: application/json" \
  -d '{
    "path": "users/alice",
    "data": {"name": "Alice", "age": 30}
  }'
```

## Monitoring

### Health Checks

All services have built-in health checks:

```bash
# Check all services
curl http://localhost:8765/gun        # Gun (returns Gun data)
curl http://localhost:8766/health     # Holster
curl http://localhost:8767/health     # Data API
```

### Logs

```bash
# View logs
docker-compose logs -f [service-name]

# Export logs
docker-compose logs > logs.txt
```

### Metrics

Add Prometheus/Grafana for metrics:

```yaml
# Add to docker-compose.yml
prometheus:
  image: prom/prometheus
  ports:
    - "9090:9090"
  volumes:
    - ./prometheus.yml:/etc/prometheus/prometheus.yml

grafana:
  image: grafana/grafana
  ports:
    - "3000:3000"
```

## Troubleshooting

### Services won't start

```bash
# Check logs
docker-compose logs

# Rebuild images
docker-compose build --no-cache
docker-compose up -d
```

### Port conflicts

Change ports in `docker-compose.yml` or stop conflicting services.

### Data not persisting

Check volume mounts:
```bash
docker volume ls
docker volume inspect free-association_gun-data
```

### Cannot connect from frontend

1. Check service health: `docker-compose ps`
2. Verify environment variables in `.env.local`
3. Check CORS if deployed remotely
4. Ensure ports are exposed: `docker-compose port gun-relay 8765`

## Backup and Restore

### Backup data volumes

```bash
# Backup Gun data
docker run --rm \
  -v free-association_gun-data:/data \
  -v $(pwd):/backup \
  alpine tar czf /backup/gun-backup.tar.gz -C /data .

# Backup Holster data
docker run --rm \
  -v free-association_holster-data:/data \
  -v $(pwd):/backup \
  alpine tar czf /backup/holster-backup.tar.gz -C /data .
```

### Restore data volumes

```bash
# Restore Gun data
docker run --rm \
  -v free-association_gun-data:/data \
  -v $(pwd):/backup \
  alpine sh -c "cd /data && tar xzf /backup/gun-backup.tar.gz"

# Restore Holster data
docker run --rm \
  -v free-association_holster-data:/data \
  -v $(pwd):/backup \
  alpine sh -c "cd /data && tar xzf /backup/holster-backup.tar.gz"
```

## Security Considerations

1. **Change default ports** in production
2. **Use HTTPS/WSS** for all external connections
3. **Enable authentication** on relay servers if needed
4. **Restrict CORS** in Data API for production
5. **Use secrets management** for sensitive configs
6. **Enable firewall rules** to restrict access
7. **Regular backups** of data volumes

## Support

For issues or questions:
- Check individual service READMEs in each service directory
- Review service logs: `docker-compose logs [service-name]`
- File issues at: https://github.com/playnet-org/free-association/issues
