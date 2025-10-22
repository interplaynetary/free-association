# Quick Start: Auto-Deploy to DigitalOcean

## TL;DR

1. **Install adapter**: `bun add -D @sveltejs/adapter-node`
2. **Run setup on your droplet**: `bash deploy/setup-server.sh`
3. **Add GitHub secrets**: `DROPLET_HOST`, `DROPLET_USER`, `DROPLET_SSH_KEY`
4. **Push to main**: Automatic deployment! 🚀

## What You Get

✅ **Auto-deployment** on every push to `main`  
✅ **Data persistence** - gun-data & holster-data preserved  
✅ **Daily backups** at 2 AM  
✅ **Health checks** after deployment  
✅ **Zero downtime** with systemd  
✅ **SSL/HTTPS** ready (Certbot)  

## Architecture

```
Frontend (GitHub Pages)
    ↓ API calls
Backend (DigitalOcean)
    ├── SvelteKit APIs (port 3000)
    ├── Gun Relay (port 8765)
    └── Holster Relay (port 8766)
```

## Setup Steps

### 1. On Your DigitalOcean Droplet

```bash
# SSH into your droplet
ssh root@your-droplet-ip

# Download and run setup
curl -O https://raw.githubusercontent.com/YOUR_USER/free-association/main/deploy/setup-server.sh
bash setup-server.sh

# Edit environment variables
nano /var/www/free-association/.env

# Update with your secrets:
# - JWT_SECRET (run: openssl rand -base64 32)
# - MASTER_API_KEY (run: openssl rand -hex 32)
# - OPENROUTER_KEYS
# - APP_URL and ALLOWED_ORIGINS
```

### 2. Update Systemd Service

```bash
# Edit the service file locally
nano deploy/free-association.service

# Change:
# - User=YOUR_USERNAME
# - ExecStart path to your Bun location

# Then on server:
sudo cp deploy/free-association.service /etc/systemd/system/
sudo systemctl daemon-reload
sudo systemctl enable free-association
```

### 3. Configure DNS & SSL

```bash
# Point your domain to your droplet
# A record: api.your-domain.com → your-droplet-ip

# Update Nginx config
sudo nano /etc/nginx/sites-available/free-association
# Change: api.your-domain.com to your actual domain

# Enable SSL
sudo certbot --nginx -d api.your-domain.com
```

### 4. GitHub Secrets

In your GitHub repo: **Settings → Secrets and variables → Actions**

Add:
- `DROPLET_HOST`: `123.45.67.89` or `api.your-domain.com`
- `DROPLET_USER`: Your SSH username
- `DROPLET_SSH_KEY`: Your private key (entire file)

Get your private key:
```bash
cat ~/.ssh/id_rsa  # or id_ed25519
```

### 5. Deploy!

```bash
# Install Node adapter
bun add -D @sveltejs/adapter-node

# Push to GitHub
git add .
git commit -m "Add auto-deployment"
git push origin main

# Watch it deploy in GitHub Actions tab! 🎉
```

## Data Safety

Your data is **protected** during deployments:

**Before deployment:**
```bash
gun-data/     → Backed up
holster-data/ → Backed up
```

**After deployment:**
```bash
New code deployed
↓
Data restored ✅
```

**Daily automatic backups** to `/var/backups/free-association-data/`

## Test Your Deployment

```bash
# Health checks
curl https://api.your-domain.com/api/ai/health
curl https://api.your-domain.com/api/llm/health
curl https://api.your-domain.com/api/keys/health

# WebSocket endpoints
# Gun: wss://api.your-domain.com/gun
# Holster: wss://api.your-domain.com/holster
```

## Update Frontend Config

Point your frontend to the backend:

```typescript
// src/lib/config.ts or similar
export const API_BASE_URL = 'https://api.your-domain.com';

export const config = {
  gun: {
    peers: [
      'https://api.your-domain.com/gun',  // Your DigitalOcean server
      'https://104.248.129.153/gun'       // Fallback
    ]
  },
  holster: {
    peers: [
      'wss://api.your-domain.com/holster' // Your DigitalOcean server
    ]
  }
};
```

## Monitoring

```bash
# View logs
ssh your-user@your-droplet
sudo journalctl -u free-association -f

# Or check files
tail -f /var/log/free-association/access.log

# Service status
sudo systemctl status free-association
```

## Common Issues

### 1. Deployment fails
- Check GitHub Actions logs
- Verify SSH key is correct (no extra newlines!)
- Test SSH manually: `ssh -i ~/.ssh/id_rsa user@host`

### 2. Service won't start
```bash
sudo systemctl status free-association
sudo journalctl -u free-association -n 50
```

### 3. CORS errors
Update Nginx config at `/etc/nginx/sites-available/free-association`:
```nginx
add_header Access-Control-Allow-Origin https://yourname.github.io always;
```
Then: `sudo systemctl reload nginx`

### 4. Data not persisting
Check permissions:
```bash
ls -la /var/www/free-association/gun-data
ls -la /var/www/free-association/holster-data
```

## Manual Deployment

If needed:
```bash
# Build locally
bun run build:server

# Upload
rsync -avz build-server/ package.json bun.lockb \
  your-user@your-droplet:/var/www/free-association/

# Restart
ssh your-user@your-droplet "cd /var/www/free-association && bun install --production && sudo systemctl restart free-association"
```

## Next Steps

- [ ] Set up monitoring (UptimeRobot, Datadog, etc.)
- [ ] Configure log rotation
- [ ] Set up alerts for service failures
- [ ] Consider load balancing for scaling
- [ ] Add staging environment

## Files Created

```
.github/workflows/deploy-server.yml  ← GitHub Actions workflow
deploy/
  ├── setup-server.sh                ← Server setup script
  └── free-association.service       ← Systemd service
svelte.config.server.js              ← Node adapter config
DEPLOYMENT.md                        ← Full documentation
DEPLOYMENT_QUICK_START.md           ← This file!
```

## Cost

- **DigitalOcean Droplet**: Starting at $6/month (1GB RAM)
- **Domain**: ~$12/year
- **SSL**: Free (Let's Encrypt)

**Total**: ~$7-10/month for full production deployment! 💰

---

**Need help?** Check `DEPLOYMENT.md` for detailed docs or GitHub Actions logs for deployment issues.

