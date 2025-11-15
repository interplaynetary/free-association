# PWA Setup with Workbox in SvelteKit

This document describes the Progressive Web App (PWA) setup implemented in this SvelteKit application using Workbox.

## Overview

The application has been configured as a Progressive Web App with the following features:

- **Service Worker with Workbox**: Manages caching strategies and offline functionality
- **Precaching**: Static assets are cached for offline access
- **Runtime Caching**: Dynamic content is cached with appropriate strategies
- **Offline Fallback**: Dedicated offline page for network failures
- **Update Notifications**: Users are prompted when a new version is available
- **PWA Manifest**: Proper metadata for installation prompts

## Installation

Install the required dependencies:

```bash
# Using npm
npm install -D vite-plugin-pwa workbox-cacheable-response workbox-core workbox-expiration workbox-precaching workbox-routing workbox-strategies workbox-window

# Using bun
bun add -D vite-plugin-pwa workbox-cacheable-response workbox-core workbox-expiration workbox-precaching workbox-routing workbox-strategies workbox-window
```

## Configuration

### 1. Vite Configuration (`vite.config.ts`)

The Vite config includes the `vite-plugin-pwa` plugin with Workbox configuration:

- **Strategy**: `injectManifest` - allows custom service worker with Workbox integration
- **Auto-reload**: Service worker updates are handled gracefully
- **Dev mode**: Service worker is enabled in development for testing

### 2. SvelteKit Configuration (`svelte.config.js`)

The built-in SvelteKit service worker is disabled since we're using vite-plugin-pwa:

```javascript
serviceWorker: {
  register: false // Using vite-plugin-pwa instead
}
```

### 3. Service Worker (`src/service-worker.ts`)

The service worker implements:

#### Caching Strategies:

- **Static Assets** (CSS, JS, Fonts): `CacheFirst` - serve from cache, fallback to network
- **Images**: `CacheFirst` with 60-day expiration and max 100 entries
- **API Calls**: `NetworkFirst` - try network first, fallback to cache (5-minute expiration)
- **Dynamic Content**: `StaleWhileRevalidate` - serve cached version while updating in background
- **WebSockets**: `NetworkOnly` - always use network

#### Custom Features:

- **Notification Management**: Custom notification system with queueing
- **Clean Architecture**: Focused service worker for PWA functionality

### 4. PWA Manifest (`static/manifest.json`)

Enhanced manifest with:

- App name, description, and branding
- Multiple icon sizes for different devices
- Display modes (standalone, window-controls-overlay)
- Shortcuts for quick actions
- Share target configuration
- Categories for app store listings

**Note**: The service worker is now clean and focused solely on PWA functionality, without additional integrations that could complicate maintenance.

## Features

### Offline Support

The app works offline with the following capabilities:

1. **Precached Assets**: All static assets (HTML, CSS, JS, images) are cached on installation
2. **Runtime Caching**: Pages visited while online are cached for offline access
3. **Offline Page**: A dedicated `/offline` route shows when content is unavailable

### Update Management

Users are automatically notified when a new version is available:

1. Service worker detects new version
2. Toast notification appears with "Update Now" and "Later" options
3. Clicking "Update Now" activates the new service worker and reloads the page
4. Updates check automatically every hour

### Installation

The app can be installed on supported devices:

- **Desktop**: Install button in browser (Chrome, Edge, etc.)
- **Mobile**: "Add to Home Screen" prompt
- **Standalone Mode**: App runs in its own window without browser UI

## Usage

### Development

```bash
npm run dev
```

The service worker is enabled in development mode for testing.

### Production Build

```bash
npm run build
```

The service worker is generated during build with all static assets precached.

### Preview

```bash
npm run preview
```

Test the production build locally, including PWA functionality.

## Testing

### Chrome DevTools

1. Open DevTools → Application tab
2. Check **Service Workers** section:
   - Verify service worker is registered
   - Test "Update on reload"
   - Test "Skip waiting"
3. Check **Cache Storage**:
   - Verify precached assets
   - Check runtime caches (images-v1, api-cache-v1, etc.)
4. Test offline mode:
   - Enable "Offline" checkbox
   - Navigate to different pages
   - Verify cached content loads

### Lighthouse

Run Lighthouse audit for PWA score:

```bash
# In Chrome DevTools → Lighthouse
# Select "Progressive Web App" category
# Run audit
```

Target score: 90+ for all PWA criteria

### Mobile Testing

1. **Installation**:
   - Visit app in mobile browser
   - Look for "Add to Home Screen" prompt
   - Install and verify standalone mode

2. **Offline**:
   - Enable airplane mode
   - Open installed app
   - Verify offline functionality

## File Structure

```
├── src/
│   ├── service-worker.ts              # Workbox service worker
│   ├── lib/
│   │   ├── utils/
│   │   │   └── pwa.ts                 # PWA registration and update logic
│   │   └── notification-manager.ts    # Custom notification system
│   └── routes/
│       ├── +layout.svelte             # Service worker registration
│       └── offline/
│           └── +page.svelte           # Offline fallback page
├── static/
│   └── manifest.json                  # PWA manifest
├── vite.config.ts                     # Vite + PWA plugin config
└── svelte.config.js                   # SvelteKit config
```

## Caching Details

### Cache Names and Strategies

| Cache Name | Strategy | Max Entries | Max Age | Purpose |
|------------|----------|-------------|---------|---------|
| `workbox-precache-v{version}` | Precache | N/A | N/A | Static app assets |
| `static-assets-v1` | CacheFirst | 60 | 30 days | CSS, JS, Fonts |
| `images-v1` | CacheFirst | 100 | 60 days | Images |
| `api-cache-v1` | NetworkFirst | 50 | 5 minutes | API responses |
| `dynamic-content-v1` | StaleWhileRevalidate | 50 | 24 hours | HTML pages |

### Cache Cleanup

Old caches are automatically cleaned up when:
- A new service worker version is activated
- Cache entries exceed `maxEntries`
- Cache entries exceed `maxAgeSeconds`

## Troubleshooting

### Service Worker Not Updating

1. Check Chrome DevTools → Application → Service Workers
2. Click "Update" to force check for new version
3. Enable "Update on reload" during development
4. Clear cache storage if needed

### Manifest Not Loading

1. Verify manifest link in `app.html`: `<link rel="manifest" href="/manifest.json">`
2. Check Network tab for 404 errors
3. Ensure manifest is in `static/` directory
4. Validate manifest JSON syntax

### Offline Page Not Showing

1. Verify offline route exists: `src/routes/offline/+page.svelte`
2. Check service worker cache includes offline page
3. Test with Chrome DevTools offline mode
4. Check browser console for errors

### Icons Not Displaying

1. Ensure icon files exist in `static/` directory
2. Verify icon sizes match manifest specifications
3. Use proper icon format (PNG recommended)
4. Check icon paths are absolute (start with `/`)

## Best Practices

1. **Cache Versioning**: Update cache names when making breaking changes
2. **Cache Size**: Monitor cache storage usage, set appropriate limits
3. **Update Strategy**: Balance update frequency with user experience
4. **Offline UX**: Provide clear feedback when offline
5. **Testing**: Test on real devices and various network conditions
6. **Performance**: Monitor Lighthouse PWA score regularly

## Resources

- [Workbox Documentation](https://developers.google.com/web/tools/workbox)
- [vite-plugin-pwa Documentation](https://vite-pwa-org.netlify.app/)
- [SvelteKit PWA Guide](https://kit.svelte.dev/docs/service-workers)
- [Web App Manifest](https://developer.mozilla.org/en-US/docs/Web/Manifest)
- [Service Worker API](https://developer.mozilla.org/en-US/docs/Web/API/Service_Worker_API)

