# App Platform Configuration for Path-Based API Routing

This document explains how to configure Digital Ocean App Platform to proxy API requests to your droplet.

## Architecture

```
Browser Request: https://free.playnet.lol/api/health
       ↓
App Platform (free.playnet.lol)
       ↓
   Routes /api/* → Droplet (YOUR_DROPLET_IP:3000)
       ↓
Backend responds
```

## Option 1: App Platform HTTP Routes (Recommended)

Digital Ocean App Platform supports HTTP routes that can proxy to external services.

### Configuration

In your App Platform dashboard:

1. Go to your app → **Settings** → **Routes**
2. Add a new route:
   - **Path:** `/api`
   - **Type:** `Proxy`
   - **Destination:** `http://YOUR_DROPLET_IP:3000/api`
   - **Preserve Path Prefix:** Yes

3. Add another route for WebSocket:
   - **Path:** `/mesh`
   - **Type:** `Proxy`
   - **Destination:** `http://YOUR_DROPLET_IP:8766/mesh`
   - **Preserve Path Prefix:** Yes
   - **Enable WebSocket:** Yes

### App Spec YAML

Alternatively, you can configure this in your `app.yaml` or App Spec:

```yaml
name: free-association
services:
  - name: frontend
    github:
      repo: interplaynetary/free-association
      branch: main
    build_command: npm run build
    output_dir: build
    routes:
      - path: /
        preserve_path_prefix: false
    http_port: 3000
    
  # Proxy routes
  - name: api-proxy
    type: worker
    routes:
      - path: /api
        preserve_path_prefix: true
    http_port: 3000
    run_command: |
      # This is a proxy configuration
      # App Platform will handle the proxying
    
domains:
  - domain: free.playnet.lol
    type: PRIMARY
```

## Option 2: Custom Nginx Configuration

If App Platform doesn't support HTTP routes directly, you can use a custom Nginx configuration.

### 1. Create `nginx.conf`

Create a file in your repo: `nginx.conf`

```nginx
server {
    listen 8080;
    server_name _;
    
    # Serve static frontend
    root /workspace/build;
    index index.html;
    
    # API proxy
    location /api/ {
        proxy_pass http://YOUR_DROPLET_IP:3000;
        proxy_http_version 1.1;
        proxy_set_header Host $host;
        proxy_set_header X-Real-IP $remote_addr;
        proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
        proxy_set_header X-Forwarded-Proto $scheme;
    }
    
    # Mesh WebSocket proxy
    location /mesh {
        proxy_pass http://YOUR_DROPLET_IP:8766;
        proxy_http_version 1.1;
        proxy_set_header Upgrade $http_upgrade;
        proxy_set_header Connection "Upgrade";
        proxy_set_header Host $host;
        proxy_read_timeout 86400;
    }
    
    # SPA fallback
    location / {
        try_files $uri $uri/ /index.html;
    }
}
```

### 2. Update App Spec

```yaml
name: free-association
static_sites:
  - name: frontend
    github:
      repo: interplaynetary/free-association
      branch: main
    build_command: npm run build
    output_dir: build
    # Use custom Nginx config
    nginx:
      config_file_path: nginx.conf
```

## Option 3: Cloudflare Workers (Advanced)

If you're using Cloudflare for DNS, you can use a Worker to proxy API requests:

```javascript
addEventListener('fetch', event => {
  event.respondWith(handleRequest(event.request))
})

async function handleRequest(request) {
  const url = new URL(request.url)
  
  // Proxy /api/* to droplet
  if (url.pathname.startsWith('/api/')) {
    const dropletUrl = `http://YOUR_DROPLET_IP:3000${url.pathname}${url.search}`
    return fetch(dropletUrl, {
      method: request.method,
      headers: request.headers,
      body: request.body
    })
  }
  
  // Proxy /mesh to WebSocket
  if (url.pathname === '/mesh') {
    const wsUrl = `ws://YOUR_DROPLET_IP:8766/mesh`
    // WebSocket upgrade logic
  }
  
  // Serve static frontend
  return fetch(request)
}
```

## Recommended Approach

**For Digital Ocean App Platform:** Use **Option 1** (HTTP Routes) if available, otherwise **Option 2** (Custom Nginx).

## Security Considerations

### 1. Use HTTPS for Droplet Communication

Instead of `http://YOUR_DROPLET_IP:3000`, use HTTPS:

```nginx
location /api/ {
    proxy_pass https://YOUR_DROPLET_IP:3000;
    # ... rest of config
}
```

Make sure your droplet has SSL configured (via Nginx + Let's Encrypt).

### 2. Restrict Droplet Access

Configure your droplet firewall to only accept connections from App Platform:

```bash
# Get App Platform IP ranges from Digital Ocean docs
# Add firewall rules
sudo ufw allow from APP_PLATFORM_IP_RANGE to any port 3000
sudo ufw allow from APP_PLATFORM_IP_RANGE to any port 8766
```

### 3. Use Internal Networking (Best)

If your droplet and App Platform are in the same region, use Digital Ocean's private networking:

```nginx
location /api/ {
    # Use private IP instead of public
    proxy_pass http://PRIVATE_DROPLET_IP:3000;
}
```

## Testing

### 1. Test Locally

```bash
# Start frontend
npm run dev

# API calls should work with relative paths
curl http://localhost:5173/api/health
```

### 2. Test Production

```bash
# After deploying
curl https://free.playnet.lol/api/health

# Should return response from droplet
```

### 3. Test WebSocket

```javascript
// In browser console at https://free.playnet.lol
const ws = new WebSocket('wss://free.playnet.lol/mesh');
ws.onopen = () => console.log('Connected!');
```

## Troubleshooting

### Issue: 404 on /api/* routes

**Cause:** App Platform not proxying correctly

**Solution:**
- Check route configuration in App Platform
- Verify droplet is accessible from App Platform
- Check droplet logs: `sudo journalctl -u free-association -f`

### Issue: CORS errors

**Cause:** Droplet rejecting requests from App Platform

**Solution:**
Update droplet `.env`:
```bash
ALLOWED_ORIGINS=https://free.playnet.lol
```

### Issue: WebSocket connection fails

**Cause:** WebSocket upgrade not configured

**Solution:**
- Ensure `Upgrade` and `Connection` headers are set
- Check if App Platform supports WebSocket proxying
- May need to use direct connection to droplet for WebSocket

## Alternative: Direct WebSocket Connection

If App Platform doesn't support WebSocket proxying well, you can:

1. Keep API proxied through App Platform
2. Connect WebSocket directly to droplet

```typescript
// In your code
const wsUrl = import.meta.env.DEV 
  ? 'ws://localhost:8766/mesh'
  : 'wss://YOUR_DROPLET_DOMAIN:8766/mesh';
```

This requires:
- DNS record for droplet
- SSL on droplet
- Open port 8766 on droplet firewall
