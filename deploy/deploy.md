---
description: Deploy frontend to App Platform and backend to Droplet
---

# Free Association Deployment Workflow

This project uses a **split deployment architecture**:
- **Frontend (Static)**: Deployed to Digital Ocean App Platform → `free.playnet.lol`
- **Backend (Node.js)**: Deployed to Digital Ocean Droplet → Holster relay + API routes

## Architecture Overview

```
┌─────────────────────────────────────────────────────────────┐
│                    free.playnet.lol                         │
│              (Digital Ocean App Platform)                   │
│                                                             │
│  ┌─────────────────────────────────────────────────────┐   │
│  │  Static SvelteKit Frontend (adapter-static)         │   │
│  │  - PWA with service worker                          │   │
│  │  - Client-side routing                              │   │
│  │  - Connects to backend APIs                         │   │
│  └─────────────────────────────────────────────────────┘   │
└─────────────────────────────────────────────────────────────┘
                           │
                           │ HTTPS/WSS
                           ▼
┌─────────────────────────────────────────────────────────────┐
│              api.free.playnet.lol (or similar)              │
│                  (Digital Ocean Droplet)                    │
│                                                             │
│  ┌─────────────────────────────────────────────────────┐   │
│  │  SvelteKit Backend (adapter-node)                   │   │
│  │  - Port 3000: API routes (/api/*)                   │   │
│  │  - Port 8766: Holster relay (WebSocket)             │   │
│  │  - Persistent storage: /var/www/free-association    │   │
│  └─────────────────────────────────────────────────────┘   │
│                                                             │
│  ┌─────────────────────────────────────────────────────┐   │
│  │  Nginx Reverse Proxy                                │   │
│  │  - SSL termination (Let's Encrypt)                  │   │
│  │  - Rate limiting                                    │   │
│  │  - WebSocket proxying                               │   │
│  └─────────────────────────────────────────────────────┘   │
└─────────────────────────────────────────────────────────────┘
```

## Prerequisites

### For App Platform (Frontend)
- Digital Ocean account with App Platform enabled
- Connected to your GitHub repository

### For Droplet (Backend)
- Digital Ocean Droplet (Ubuntu 22.04+)
- SSH access configured

## Path-Based API Routing

This deployment uses **path-based routing** for simplicity - no DNS configuration needed!

```
https://free.playnet.lol/          → Static frontend (App Platform)
https://free.playnet.lol/api/*     → Backend API (proxied to Droplet)
https://free.playnet.lol/holster   → WebSocket (proxied to Droplet)
```

**Benefits:**
- ✅ No DNS changes required
- ✅ No CORS issues (same origin)
- ✅ No environment variables needed
- ✅ Simpler configuration

**How it works:**
1. App Platform serves your static frontend
2. App Platform proxies `/api/*` requests to your droplet
3. App Platform proxies `/holster` WebSocket to your droplet

**Configuration:** See [deploy/app-platform-proxy.md](file:///home/ruzgar/Programs/playnet/free-association/deploy/app-platform-proxy.md) for App Platform proxy setup.

## Initial Setup

### 1. Setup Droplet (One-time)

SSH into your droplet and run the setup script:

```bash
# Clone repository
git clone https://github.com/interplaynetary/free-association.git
cd free-association

# Run initial setup
chmod +x deploy/setup-server.sh
./deploy/setup-server.sh
```

This script will:
- Install Bun runtime
- Create application directories
- Setup Nginx reverse proxy
- Configure systemd service
- Setup firewall rules
- Configure automatic backups

### 2. Configure Environment Variables

Edit the `.env` file on your droplet:

```bash
nano /var/www/free-association/.env
```

Update these critical values:
- `APP_URL=https://free.playnet.lol`
- `JWT_SECRET` (generate with: `openssl rand -base64 32`)
- `MASTER_API_KEY` (generate with: `openssl rand -hex 32`)
- `ALLOWED_ORIGINS=https://free.playnet.lol`
- Add your `OPENROUTER_KEYS` if using LLM features

### 3. Update Nginx Configuration

```bash
sudo nano /etc/nginx/sites-available/free-association
```

Change `server_name` to your actual domain (e.g., `api.free.playnet.lol`)

### 4. Setup SSL Certificate

```bash
sudo certbot --nginx -d api.free.playnet.lol
```

### 5. Install and Start Service

```bash
# Copy systemd service file
sudo cp deploy/free-association.service /etc/systemd/system/

# Enable and start
sudo systemctl enable free-association
sudo systemctl start free-association

# Check status
sudo systemctl status free-association
```

## Deployment Process

### Frontend Deployment (App Platform)

// turbo-all

The frontend is automatically deployed via Digital Ocean App Platform when you push to your main branch.

**Manual deployment via App Platform:**

1. Go to Digital Ocean App Platform dashboard
2. Select your app
3. Click "Deploy" or configure auto-deploy from GitHub

**Build settings:**
- Build Command: `npm run build`
- Output Directory: `build`

**IMPORTANT: Configure API Proxy**

You MUST configure App Platform to proxy `/api/*` and `/holster` requests to your droplet.

See detailed instructions: [deploy/app-platform-proxy.md](file:///home/ruzgar/Programs/playnet/free-association/deploy/app-platform-proxy.md)

**Quick setup:**
- Add route: `/api` → proxy to `http://YOUR_DROPLET_IP:3000/api`
- Add route: `/holster` → proxy to `http://YOUR_DROPLET_IP:8766/holster` (WebSocket enabled)

### Backend Deployment (Droplet)

Deploy the backend to your droplet:

```bash
# 1. Build the server locally (or on droplet)
BUILD_TARGET=server npm run build

# 2. Sync to droplet (from your local machine)
rsync -avz --delete \
  --exclude 'node_modules' \
  --exclude '.git' \
  --exclude 'holster-data' \
  ./ user@your-droplet-ip:/var/www/free-association/

# 3. SSH into droplet and install dependencies
ssh user@your-droplet-ip
cd /var/www/free-association
bun install --production

# 4. Restart the service
sudo systemctl restart free-association

# 5. Check logs
sudo journalctl -u free-association -f
```

**Alternative: Build on droplet directly**

```bash
# SSH into droplet
ssh user@your-droplet-ip
cd /var/www/free-association

# Pull latest changes
git pull origin main

# Install dependencies and build
bun install
BUILD_TARGET=server bun run build

# Restart service
sudo systemctl restart free-association
```

## Verification

### Check Frontend
```bash
curl https://free.playnet.lol
```

### Check Backend API
```bash
curl https://api.free.playnet.lol/api/health
```

### Check Holster Relay
```bash
# Should show WebSocket upgrade
curl -i https://api.free.playnet.lol/holster
```

### Check Service Status
```bash
sudo systemctl status free-association
sudo journalctl -u free-association -n 50
```

### Check Nginx Logs
```bash
sudo tail -f /var/log/nginx/free-association-access.log
sudo tail -f /var/log/nginx/free-association-error.log
```

## Troubleshooting

### Frontend not loading
- Check App Platform build logs
- Verify `BASE_PATH` environment variable
- Check browser console for API connection errors

### Backend API not responding
- Check service status: `sudo systemctl status free-association`
- Check logs: `sudo journalctl -u free-association -f`
- Verify Nginx config: `sudo nginx -t`
- Check firewall: `sudo ufw status`

### Holster WebSocket not connecting
- Verify Nginx WebSocket proxy configuration
- Check if port 8766 is accessible
- Review browser WebSocket connection errors
- Ensure SSL certificate covers the domain

### Data not persisting
- Check directory permissions: `ls -la /var/www/free-association/holster-data`
- Verify systemd `ReadWritePaths` includes data directories
- Check disk space: `df -h`

## Backup and Recovery

### Manual Backup
```bash
# Run backup script
sudo /usr/local/bin/backup-free-association.sh

# List backups
ls -lh /var/backups/free-association-data/
```

### Restore from Backup
```bash
# Stop service
sudo systemctl stop free-association

# Restore data
cd /var/www/free-association
tar -xzf /var/backups/free-association-data/holster-data-YYYYMMDD_HHMMSS.tar.gz

# Start service
sudo systemctl start free-association
```

## Monitoring

### Resource Usage
```bash
# Check memory and CPU
htop

# Check disk usage
df -h
du -sh /var/www/free-association/*
```

### Service Health
```bash
# Application health endpoint
curl https://api.free.playnet.lol/api/health

# System health
sudo systemctl status free-association
```

## Scaling Considerations

### Current Architecture
- **Frontend**: Scales automatically via App Platform CDN
- **Backend**: Single droplet (vertical scaling only)

### Future Improvements
1. **Load Balancing**: Add multiple droplets behind a load balancer
2. **Database**: Move to managed database (currently using file-based Holster)
3. **Caching**: Add Redis for session/rate limiting
4. **Monitoring**: Add Prometheus + Grafana
5. **CI/CD**: Automate deployment with GitHub Actions

## Standard Practice Comparison

**Your approach (Split deployment)** ✅
- Frontend on CDN/App Platform
- Backend on dedicated server
- **Pros**: Better performance, easier scaling, cost-effective
- **Cons**: More complex deployment

**Alternative (Unified deployment)**
- Everything on one droplet or App Platform
- **Pros**: Simpler deployment
- **Cons**: Less flexible, harder to scale

**Your architecture is correct for applications with:**
- WebSocket requirements (Holster relay)
- Persistent data storage needs
- Separate frontend/backend scaling requirements

## Notes

- Digital Ocean App Platform handles backups for the frontend automatically
- Droplet backups are configured via cron (daily at 2 AM)
- SSL certificates auto-renew via certbot
- Holster data is stored persistently on the droplet filesystem
