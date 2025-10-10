# Free Association - Architecture Documentation

## Overview

The Free Association project has been refactored from a monolithic structure into a microservices architecture with three independent, containerized services.

## Before: Monolithic Architecture

```
┌─────────────────────────────────────┐
│      Single Server Process          │
│  (start.cjs on port 8765)           │
│                                     │
│  ┌──────────────────────────────┐  │
│  │   Gun Relay                  │  │
│  │   (embedded in server)       │  │
│  └──────────────────────────────┘  │
│                                     │
│  Frontend connects to external      │
│  Holster relay: holster.haza.website│
└─────────────────────────────────────┘
           │
           │
      ┌────▼────┐
      │ Browser │
      │ Client  │
      └─────────┘
```

**Problems:**
- Single point of failure
- Hard to scale individual components
- Gun and Holster coupled to main application
- No easy way to populate databases externally
- Dependent on external Holster relay

## After: Microservices Architecture

```
┌──────────────────────────────────────────────────────────┐
│              Docker Network (bridge)                      │
│                                                           │
│  ┌─────────────────┐  ┌─────────────────┐  ┌──────────┐ │
│  │  gun-relay      │  │ holster-relay   │  │ data-api │ │
│  │  :8765          │  │  :8766          │  │  :8767   │ │
│  │                 │  │                 │  │          │ │
│  │  Gun Protocol   │  │  Holster/WS     │  │  REST    │ │
│  │  Storage: Gun   │  │  Storage: Holst.│  │  HTTP    │ │
│  └────────┬────────┘  └────────┬────────┘  └────┬─────┘ │
│           │                    │                  │       │
│           │                    │        ┌─────────┴─────┐ │
│           │                    │        │  Talks to     │ │
│           │                    │        │  both relays  │ │
│           │                    │        └───────────────┘ │
└───────────┼────────────────────┼──────────────────────────┘
            │                    │
            │                    │
     ┌──────▼────────────────────▼──────┐
     │      Browser Client (SvelteKit)  │
     │                                   │
     │  - Gun client → gun-relay:8765   │
     │  - Holster client → holster:8766 │
     │  - Optional API calls → api:8767 │
     └───────────────────────────────────┘
```

**Benefits:**
- **Independent scaling** - Scale Gun, Holster, or API separately
- **Isolation** - Failure in one service doesn't crash others
- **Flexibility** - Replace/upgrade services independently
- **Testability** - Each service can be tested in isolation
- **Data control** - REST API allows external data population
- **Self-hosted** - No dependency on external Holster relay

## Service Architecture

### 1. Gun Relay (`gun-relay`)

**Purpose:** Decentralized graph database relay for Gun protocol

**Technology Stack:**
- Node.js 20
- Express.js
- @gun-vue/relay
- Gun database

**Responsibilities:**
- Accept Gun protocol connections
- Store and sync Gun data
- Provide WebRTC peer discovery
- Persist data to disk (radisk)

**Storage:**
- Docker volume: `gun-data`
- Path: `/app/store`
- Format: Gun radisk format

**Endpoints:**
- `http://localhost:8765/gun` - Gun database endpoint
- WebRTC connections for P2P

**Configuration:**
```env
GUN_RELAY_HOST=0.0.0.0
GUN_RELAY_PORT=8765
GUN_RELAY_STORE=true
GUN_RELAY_PATH=public
GUN_RELAY_SHOW_QR=false
```

### 2. Holster Relay (`holster-relay`)

**Purpose:** Alternative decentralized database relay using Holster protocol

**Technology Stack:**
- Node.js 20
- Express.js
- WebSocket (ws)
- @mblaney/holster

**Responsibilities:**
- Accept WebSocket connections for Holster protocol
- Store and sync Holster data
- Handle authentication (if enabled)
- Persist data to filesystem

**Storage:**
- Docker volume: `holster-data`
- Path: `/app/holster-data`
- Format: Holster native format

**Endpoints:**
- `ws://localhost:8766/holster` - WebSocket endpoint
- `http://localhost:8766/health` - Health check

**Configuration:**
```env
HOLSTER_RELAY_HOST=0.0.0.0
HOLSTER_RELAY_PORT=8766
HOLSTER_RELAY_STORAGE=true
HOLSTER_RELAY_STORAGE_PATH=./holster-data
```

### 3. Data API (`data-api`)

**Purpose:** REST API for database population and external integrations

**Technology Stack:**
- Node.js 20
- Express.js
- Gun client
- Holster client
- CORS enabled

**Responsibilities:**
- Provide HTTP REST endpoints
- Connect to both Gun and Holster relays as a client
- Write data to both databases
- Read data from both databases
- Seed databases with test data

**Endpoints:**

Health:
- `GET /health` - Service status

Gun operations:
- `POST /gun/put` - Write data to Gun
- `GET /gun/get?path=...` - Read data from Gun
- `POST /gun/seed` - Seed Gun with sample data

Holster operations:
- `POST /holster/put` - Write data to Holster
- `GET /holster/get?path=...` - Read from Holster
- `POST /holster/seed` - Seed Holster with sample data

Documentation:
- `GET /` - API documentation

**Configuration:**
```env
DATA_API_HOST=0.0.0.0
DATA_API_PORT=8767
GUN_PEER_URL=http://gun-relay:8765/gun
HOLSTER_PEER_URL=ws://holster-relay:8766/holster
```

## Frontend Integration

### Configuration System

New centralized configuration at `src/lib/config.ts`:

```typescript
export const config = {
  gun: {
    peers: [
      import.meta.env.VITE_GUN_PEER_URL || 'http://localhost:8765/gun',
      // Fallback to external peers
      'https://104.248.129.153/gun',
      // ...
    ],
    localStorage: false,
    radisk: true
  },
  holster: {
    peers: [
      import.meta.env.VITE_HOLSTER_PEER_URL || 'ws://localhost:8766/holster',
      'wss://holster.haza.website' // Fallback
    ],
    indexedDB: true,
    secure: true
  }
};
```

### State Management Updates

**`src/lib/state/gun.svelte.ts`:**
```typescript
import { config } from '../config';

export const gun = new Gun({
  peers: config.gun.peers,
  localStorage: config.gun.localStorage,
  radisk: config.gun.radisk
});
```

**`src/lib/state/holster.svelte.ts`:**
```typescript
import { config } from '../config';

export const holster = Holster({
  peers: config.holster.peers,
  indexedDB: config.holster.indexedDB,
  secure: config.holster.secure
});
```

### Environment Files

**Development (`.env.development`):**
```env
VITE_GUN_PEER_URL=http://localhost:8765/gun
VITE_HOLSTER_PEER_URL=ws://localhost:8766/holster
VITE_DATA_API_URL=http://localhost:8767
```

**Production (`.env.production`):**
```env
VITE_GUN_PEER_URL=https://gun.yourdomain.com/gun
VITE_HOLSTER_PEER_URL=wss://holster.yourdomain.com/holster
VITE_DATA_API_URL=https://api.yourdomain.com
```

## Data Flow

### Write Path

```
User Action (Browser)
    │
    ├─→ Gun Data
    │       │
    │       └─→ Gun Client (gun.svelte.ts)
    │               │
    │               └─→ Gun Relay (:8765)
    │                       │
    │                       └─→ Persists to gun-data volume
    │
    └─→ Holster Data
            │
            └─→ Holster Client (holster.svelte.ts)
                    │
                    └─→ Holster Relay (:8766)
                            │
                            └─→ Persists to holster-data volume
```

### Read Path

```
Browser loads page
    │
    ├─→ Gun Client subscribes to data
    │       │
    │       └─→ Gun Relay (:8765)
    │               │
    │               └─→ Streams data to browser
    │
    └─→ Holster Client subscribes to data
            │
            └─→ Holster Relay (:8766)
                    │
                    └─→ Streams data to browser
```

### API-based Population (External)

```
External Script/Tool
    │
    └─→ HTTP POST to Data API (:8767)
            │
            ├─→ /gun/put
            │       │
            │       └─→ Gun Client → Gun Relay (:8765)
            │
            └─→ /holster/put
                    │
                    └─→ Holster Client → Holster Relay (:8766)
```

## Network Architecture

### Docker Network

All three services communicate over a bridge network:

```yaml
networks:
  free-association-network:
    driver: bridge
```

**Internal DNS:**
- `gun-relay:8765` - Gun relay hostname
- `holster-relay:8766` - Holster relay hostname
- `data-api:8767` - Data API hostname

**External Access:**
Services expose ports to host:
- `localhost:8765` → `gun-relay:8765`
- `localhost:8766` → `holster-relay:8766`
- `localhost:8767` → `data-api:8767`

### Service Dependencies

```
data-api depends on:
  - gun-relay (healthy)
  - holster-relay (healthy)

gun-relay: independent
holster-relay: independent
```

## Storage Architecture

### Volumes

**gun-data:**
- Type: Named volume
- Mount: `/app/store` in gun-relay
- Format: Gun radisk
- Persistence: Survives container restarts

**holster-data:**
- Type: Named volume
- Mount: `/app/holster-data` in holster-relay
- Format: Holster filesystem
- Persistence: Survives container restarts

### Backup Strategy

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

## Deployment Models

### Development

```
docker-compose up -d
└─→ All three services on localhost
    └─→ Frontend connects to localhost:8765 & localhost:8766
```

### Production - Single Server

```
Cloud VM
├─→ Docker Compose
│   ├─→ gun-relay (behind Nginx)
│   ├─→ holster-relay (behind Nginx)
│   └─→ data-api (behind Nginx)
└─→ Nginx reverse proxy
    ├─→ gun.domain.com → gun-relay:8765
    ├─→ holster.domain.com → holster-relay:8766
    └─→ api.domain.com → data-api:8767
```

### Production - Distributed

```
Gun Relay Servers (Cluster)
├─→ gun-relay-1.domain.com
├─→ gun-relay-2.domain.com
└─→ gun-relay-3.domain.com
    └─→ Load Balanced

Holster Relay Servers (Cluster)
├─→ holster-relay-1.domain.com
├─→ holster-relay-2.domain.com
└─→ holster-relay-3.domain.com
    └─→ Load Balanced

Data API (Single or Scaled)
└─→ api.domain.com
```

## Security Considerations

### Network Security
- Services isolated in Docker network
- Only necessary ports exposed to host
- Can add firewall rules per service

### Authentication
- Gun SEA authentication (already implemented)
- Holster secure mode (enabled)
- Data API can add JWT/API keys

### Data Security
- Gun data encrypted if SEA is used
- Holster secure updates enabled
- Volumes can be encrypted at rest

### Transport Security
- Use HTTPS for Gun relay (production)
- Use WSS for Holster relay (production)
- Use HTTPS for Data API (production)

## Monitoring & Observability

### Health Checks

Built into each service:
- Gun: HTTP check on `/gun`
- Holster: HTTP check on `/health`
- Data API: HTTP check on `/health`

### Logging

Each service logs to stdout:
```bash
docker-compose logs -f [service-name]
```

### Metrics

Can integrate:
- Prometheus for metrics collection
- Grafana for visualization
- AlertManager for alerting

## Scaling Strategy

### Horizontal Scaling

**Gun Relay:**
```bash
docker-compose up -d --scale gun-relay=3
```

**Holster Relay:**
```bash
docker-compose up -d --scale holster-relay=3
```

**Data API:**
```bash
docker-compose up -d --scale data-api=3
```

### Vertical Scaling

Adjust resource limits in `docker-compose.yml`:
```yaml
services:
  gun-relay:
    deploy:
      resources:
        limits:
          cpus: '2'
          memory: 4G
```

## Migration Path

### From Old Architecture

1. Keep existing `start.cjs` running
2. Start Docker services
3. Update frontend to point to new relays
4. Test thoroughly
5. Migrate data if needed
6. Shutdown old `start.cjs`

### Data Migration

Gun data should sync automatically as Gun is P2P.
Holster may require manual data export/import.

## Future Enhancements

- **Service Mesh:** Istio/Linkerd for advanced networking
- **Message Queue:** Add Redis/RabbitMQ for async operations
- **Caching Layer:** Redis for frequently accessed data
- **CDN Integration:** Serve static content from CDN
- **Auto-scaling:** Kubernetes HPA or Docker Swarm autoscaling
- **Multi-region:** Deploy relays in multiple geographic regions

## Conclusion

The refactored architecture provides:
- ✅ Independent, scalable services
- ✅ Isolated failure domains
- ✅ Flexible deployment options
- ✅ Easy database population via API
- ✅ Self-hosted infrastructure
- ✅ Better observability
- ✅ Production-ready foundation
