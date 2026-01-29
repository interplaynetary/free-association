# Managing App Platform Configuration from Repo

You can manage your App Platform deployment configuration directly from this repository using the `.do/app.yaml` file.

## Current Configuration

The [.do/app.yaml](file:///home/ruzgar/Programs/playnet/free-association/.do/app.yaml) file defines:
- Static site deployment
- Build commands
- Domain configuration
- (Optional) Proxy service configuration

## Two Approaches for Proxy Routing

### Approach 1: App Platform UI (Free Tier) ✅ **Recommended**

**Pros:**
- Works on free tier
- No additional resources needed
- Simple configuration

**Cons:**
- Must configure via UI (not in code)

**Setup:**
1. Deploy your app with current `app.yaml`
2. Go to App Platform dashboard → Settings → Routes
3. Add custom routes:
   - `/api` → `http://YOUR_DROPLET_IP:3000/api`
   - `/holster` → `http://YOUR_DROPLET_IP:8766/holster`

**Note:** Route configuration via UI is persistent - you only need to set it once.

---

### Approach 2: Proxy Service in app.yaml (Paid Plan)

**Pros:**
- Fully managed in code (infrastructure as code)
- Version controlled
- Automatic deployment

**Cons:**
- Requires paid App Platform plan
- Uses additional resources ($$$)

**Setup:**

1. **Uncomment the `services` section in `.do/app.yaml`**

2. **Replace `YOUR_DROPLET_IP` with your actual droplet IP**

3. **Push to GitHub:**
   ```bash
   git add .do/app.yaml deploy/proxy-server.js
   git commit -m "Add proxy service configuration"
   git push origin main
   ```

4. **App Platform will automatically:**
   - Deploy the static frontend
   - Deploy the proxy service
   - Configure routes

## How app.yaml Works

### Automatic Detection

When you push to GitHub, App Platform:
1. Detects `.do/app.yaml` in your repo
2. Reads the configuration
3. Deploys according to the spec

### Manual Updates

To update configuration:
```bash
# Edit .do/app.yaml
vim .do/app.yaml

# Commit and push
git add .do/app.yaml
git commit -m "Update app configuration"
git push origin main

# App Platform auto-deploys with new config
```

### Configuration Options

The `app.yaml` supports:

```yaml
# Static sites
static_sites:
  - name: frontend
    build_command: npm run build
    output_dir: build
    catchall_document: index.html  # SPA routing

# Services (Node.js, Python, etc.)
services:
  - name: api-proxy
    environment_slug: node-js
    run_command: node server.js
    routes:
      - path: /api

# Workers (background jobs)
workers:
  - name: background-job
    run_command: node worker.js

# Databases
databases:
  - name: db
    engine: PG
    version: "12"

# Domains
domains:
  - domain: free.playnet.lol
    type: PRIMARY
```

## Recommended Setup

For your use case (free tier), I recommend:

1. **Use current `app.yaml` for static site** ✅ (already configured)
2. **Configure proxy routes via UI** (one-time setup)
3. **Keep `app.yaml` simple** (just static site config)

This gives you:
- ✅ Infrastructure as code for frontend
- ✅ Free tier compatibility
- ✅ Simple maintenance

## Alternative: Full Infrastructure as Code

If you want **everything** in code and don't mind the cost:

1. Uncomment the `services` section in `app.yaml`
2. Add droplet IP as environment variable
3. Push to GitHub
4. App Platform deploys both frontend and proxy service

**Cost:** ~$5-10/month for the proxy service

## Verification

After deploying with `app.yaml`:

```bash
# Check deployment
doctl apps list

# Get app details
doctl apps get <app-id>

# View app spec
doctl apps spec get <app-id>

# Update app spec
doctl apps update <app-id> --spec .do/app.yaml
```

## Summary

**Current state:**
- ✅ `app.yaml` configured for static site
- ⚠️ Proxy routes need manual UI configuration (one-time)

**To make it fully code-managed:**
- Uncomment `services` section in `app.yaml`
- Add `deploy/proxy-server.js` dependencies
- Requires paid plan

**Recommendation:** Keep it simple with UI-based proxy configuration unless you need full IaC.
