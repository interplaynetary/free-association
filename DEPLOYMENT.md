# Deployment Guide - Hybrid Architecture

## Architecture Overview

Your app uses a **hybrid deployment**:

```
┌─────────────────────┐
│   GitHub Pages      │  ← Static PWA/SPA
│   (Frontend)        │     yourname.github.io
└─────────┬───────────┘
          │
          │ API calls
          ↓
┌─────────────────────┐
│  DigitalOcean       │  ← Backend APIs + Relays
│  (Backend)          │     api.your-domain.com
│  • SvelteKit API    │
│  • Gun Relay        │
│  • Holster Relay    │
└─────────────────────┘
```

## Initial Setup (One-Time)

### 1. Prepare Your DigitalOcean Droplet

SSH into your droplet and run the setup script:

```bash
# Download the setup script
curl -O https://raw.githubusercontent.com/YOUR_USERNAME/free-association/main/deploy/setup-server.sh

# Run it
bash setup-server.sh
```

This script will:
- ✅ Install Bun, Nginx, and required packages
- ✅ Create application directories
- ✅ Set up data persistence directories
- ✅ Configure Nginx reverse proxy
- ✅ Set up firewall rules
- ✅ Configure automatic daily backups

### 2. Configure Environment Variables

Edit `/var/www/free-association/.env` on your server:

```bash
ssh your-user@your-droplet
nano /var/www/free-association/.env
```

Generate secrets:
```bash
# JWT Secret
openssl rand -base64 32

# Master API Key
openssl rand -hex 32
```

Update:
```env
JWT_SECRET=your-generated-jwt-secret
MASTER_API_KEY=your-generated-api-key
OPENROUTER_KEYS=sk-or-v1-your-key-1,sk-or-v1-your-key-2
APP_URL=https://api.your-domain.com
ALLOWED_ORIGINS=https://yourname.github.io,https://your-custom-domain.com
```

### 3. Update Systemd Service

Edit `deploy/free-association.service` and update:
- `User=` to your SSH username
- `/home/YOUR_USER/.bun/bin/bun` to your actual Bun path

Then install:
```bash
sudo cp deploy/free-association.service /etc/systemd/system/
sudo systemctl daemon-reload
sudo systemctl enable free-association
```

### 4. Update Nginx Configuration

Edit `/etc/nginx/sites-available/free-association`:
- Replace `api.your-domain.com` with your actual domain
- Update CORS origins for your GitHub Pages URL

Reload Nginx:
```bash
sudo nginx -t
sudo systemctl reload nginx
```

### 5. Set Up SSL (HTTPS)

```bash
sudo certbot --nginx -d api.your-domain.com
```

### 6. Configure GitHub Secrets

In your GitHub repository, go to **Settings > Secrets and variables > Actions**:

Add these secrets:
- `DROPLET_HOST`: Your server IP or domain (e.g., `123.45.67.89` or `api.your-domain.com`)
- `DROPLET_USER`: Your SSH username (e.g., `root` or your user)
- `DROPLET_SSH_KEY`: Your **private** SSH key (entire content)

To get your SSH private key:
```bash
cat ~/.ssh/id_rsa
# or
cat ~/.ssh/id_ed25519
```

## Auto-Deployment Workflow

Once configured, deployment is automatic:

### What Triggers Deployment

Push to `main` branch when changes affect:
- `src/routes/api/**` - API routes
- `src/lib/server/**` - Server libraries
- `server/**` - Relay servers
- `package.json` or `bun.lockb` - Dependencies

Or manually trigger via **Actions** tab in GitHub.

### What Happens During Deployment

1. ✅ Builds SvelteKit with Node.js adapter
2. ✅ Creates deployment archive
3. ✅ **Backs up data directories** (gun-data, holster-data)
4. ✅ Uploads new code
5. ✅ **Restores data directories**
6. ✅ Installs production dependencies
7. ✅ Restarts systemd service
8. ✅ Runs health checks

### Data Persistence Strategy

Your data is **preserved** during every deployment:

```bash
# Before deployment
gun-data/     ──► Backup to /var/backups/
holster-data/ ──► Backup to /var/backups/

# Deploy new code...

# After deployment
/var/backups/ ──► Restore to gun-data/
/var/backups/ ──► Restore to holster-data/
```

Additionally:
- ✅ Daily automatic backups at 2 AM
- ✅ Backups retained for 7 days
- ✅ Located in `/var/backups/free-association-data/`

## Manual Deployment

If you need to deploy manually:

### Build Locally
```bash
# Use server config
cp svelte.config.server.js svelte.config.js
bun run build

# This creates build-server/ directory
```

### Upload to Server
```bash
rsync -avz --exclude 'gun-data' --exclude 'holster-data' \
  build-server/ package.json bun.lockb server/ \
  your-user@your-droplet:/var/www/free-association/
```

### Restart Service
```bash
ssh your-user@your-droplet
cd /var/www/free-association
bun install --production
sudo systemctl restart free-association
```

## Frontend Deployment (GitHub Pages)

Your static frontend continues to deploy to GitHub Pages as before:

```bash
# Build for GitHub Pages
bun run build

# The existing GitHub Pages workflow deploys the static site
```

Update your frontend API configuration to point to your DigitalOcean backend:

```typescript
// src/lib/config.ts
export const API_BASE_URL = 'https://api.your-domain.com';
```

## Monitoring & Logs

### View Logs
```bash
# Service logs
sudo journalctl -u free-association -f

# Or check log files
tail -f /var/log/free-association/access.log
tail -f /var/log/free-association/error.log
```

### Check Status
```bash
# Service status
sudo systemctl status free-association

# Health checks
curl https://api.your-domain.com/api/ai/health
curl https://api.your-domain.com/api/llm/health
curl https://api.your-domain.com/api/keys/health
```

### Manual Backups
```bash
# Run backup script manually
sudo /usr/local/bin/backup-free-association.sh

# List backups
ls -lh /var/backups/free-association-data/
```

### Restore from Backup
```bash
# Stop service
sudo systemctl stop free-association

# Restore Gun data
cd /var/www/free-association
tar -xzf /var/backups/free-association-data/gun-data-YYYYMMDD_HHMMSS.tar.gz

# Restore Holster data
tar -xzf /var/backups/free-association-data/holster-data-YYYYMMDD_HHMMSS.tar.gz

# Start service
sudo systemctl start free-association
```

## Troubleshooting

### Deployment Fails
```bash
# Check GitHub Actions logs in the Actions tab

# SSH to server and check
ssh your-user@your-droplet
sudo systemctl status free-association
sudo journalctl -u free-association -n 50
```

### Service Won't Start
```bash
# Check configuration
sudo systemctl status free-association

# Check environment variables
cat /var/www/free-association/.env

# Check permissions
ls -la /var/www/free-association/
```

### Data Loss
```bash
# Automatic backups are in:
ls /var/backups/free-association-data/

# Restore from latest backup (see above)
```

### CORS Issues
```bash
# Update Nginx config
sudo nano /etc/nginx/sites-available/free-association

# Update the add_header Access-Control-Allow-Origin line
# Then reload
sudo nginx -t
sudo systemctl reload nginx
```

## Scaling Considerations

### Multiple Instances
Use PM2 or Docker Swarm to run multiple instances:
```bash
bun add -g pm2
pm2 start build-server/index.js -i max
```

### Load Balancer
Add Nginx upstream for load balancing multiple droplets.

### Database Migration
Consider moving Gun/Holster data to:
- Block storage volume
- External database service
- S3-compatible storage

## Security Checklist

- ✅ Firewall configured (UFW)
- ✅ SSL/TLS enabled (Certbot)
- ✅ Environment variables secured
- ✅ Regular backups enabled
- ✅ CORS properly configured
- ✅ Rate limiting in Nginx
- ✅ Systemd security hardening
- ⚠️ Keep dependencies updated: `bun update`

## Quick Reference

```bash
# Start service
sudo systemctl start free-association

# Stop service
sudo systemctl stop free-association

# Restart service
sudo systemctl restart free-association

# View logs
sudo journalctl -u free-association -f

# Health check
curl https://api.your-domain.com/api/ai/health

# Manual backup
sudo /usr/local/bin/backup-free-association.sh

# Update and redeploy (auto via GitHub)
git add .
git commit -m "Update server"
git push origin main
```

## Cost Optimization

- Start with a **$6/month Basic Droplet** (1GB RAM, 1 CPU)
- Monitor with: `htop`, `df -h`, `free -h`
- Upgrade if needed based on usage

---

**Questions?** Check logs, review GitHub Actions, or consult DigitalOcean's monitoring dashboard.

