# iOS PWA GitHub Pages Fix

## The Problem

iOS PWAs were getting 404 errors when installed from GitHub Pages because the PWA manifest wasn't using the correct base path (`/free-association/`).

## Root Cause

1. **Static Files Not Processed**: SvelteKit doesn't process static files (like `manifest.json`) during build
2. **iOS Strict URL Matching**: iOS requires exact URL matching between manifest and deployment path
3. **Service Worker Scope Mismatch**: Service worker scope needs to match manifest scope exactly

## The Solution

### 1. Dynamic Manifest Generation (`scripts/generate-manifest.js`)

- Automatically generates correct manifest based on `BASE_PATH` environment variable
- Handles both development (`/`) and production (`/free-association/`) paths
- Ensures all URLs (start_url, scope, icons) use correct base path

### 2. Updated Build Scripts (`package.json`)

```json
{
	"dev": "node scripts/generate-manifest.js --dev && vite dev",
	"build": "node scripts/generate-manifest.js && vite build"
}
```

### 3. Service Worker Base Path Handling (`src/service-worker.ts`)

- Added base path normalization in fetch handler
- Fixed notification click handler to use correct URL
- Ensures cached assets are served correctly regardless of base path

### 4. Enhanced PWA Meta Tags (`src/app.html`)

- Added iOS-specific PWA meta tags
- Used `%sveltekit.assets%` for proper asset path resolution
- Added apple-touch-startup-image for better iOS integration

## Key Changes

### Manifest Generation

```javascript
const manifest = {
	start_url: isDev ? '/' : `${basePath}/`,
	scope: isDev ? '/' : `${basePath}/`,
	icons: [
		{
			src: isDev ? '/favicon.png' : `${basePath}/favicon.png`
		}
	]
};
```

### Service Worker Fetch Handler

```javascript
// Handle base path for GitHub Pages deployment
let pathname = url.pathname;
const basePath = '/free-association';
if (pathname.startsWith(basePath)) {
	pathname = pathname.substring(basePath.length) || '/';
}
```

## Testing

1. Build locally: `BASE_PATH="/free-association" npm run build`
2. Deploy to GitHub Pages
3. Test on iOS Safari - PWA should install correctly
4. Verify start URL points to correct path

## Why This Works

- **Absolute URLs**: iOS requires absolute URLs in manifest for subdirectory deployments
- **Scope Matching**: Service worker scope exactly matches manifest scope
- **Build-Time Generation**: Manifest is generated with correct paths before build
- **Environment Awareness**: Different paths for dev vs production

This solution ensures PWA works correctly on both Android and iOS when deployed to GitHub Pages subdirectories.
