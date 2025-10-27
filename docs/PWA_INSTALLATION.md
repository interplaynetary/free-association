# PWA Installation Guide

This guide covers how to install the necessary dependencies and verify that the PWA setup is working correctly.

## Prerequisites

- Node.js (v18 or higher) or Bun runtime
- A package manager (npm, yarn, pnpm, or bun)

## Step 1: Install Dependencies

The PWA dependencies have been added to `package.json`. Install them using your preferred package manager:

### Using npm
```bash
npm install
```

### Using yarn
```bash
yarn install
```

### Using pnpm
```bash
pnpm install
```

### Using bun
```bash
bun install
```

## Step 2: Verify Installation

After installation, verify that the following packages are installed:

**Development Dependencies:**
- `vite-plugin-pwa` - Vite plugin for PWA support
- `workbox-cacheable-response` - Workbox plugin for caching responses
- `workbox-core` - Core Workbox functionality
- `workbox-expiration` - Workbox plugin for cache expiration
- `workbox-precaching` - Workbox precaching functionality
- `workbox-routing` - Workbox routing functionality
- `workbox-strategies` - Workbox caching strategies
- `workbox-window` - Workbox window integration

You can verify by checking your `node_modules` directory or by running:

```bash
npm list vite-plugin-pwa workbox-window
```

## Step 3: Development Testing

Start the development server to test the PWA:

```bash
npm run dev
```

The service worker is enabled in development mode, so you can test PWA functionality immediately.

### Testing in Chrome DevTools

1. Open Chrome DevTools (F12)
2. Go to the **Application** tab
3. Check the **Service Workers** section
4. You should see the service worker registered

### Testing Offline Mode

1. In Chrome DevTools, go to **Application** → **Service Workers**
2. Check the "Offline" checkbox
3. Refresh the page
4. The app should still load from cache

## Step 4: Build for Production

Build the application for production:

```bash
npm run build
```

This will:
- Generate optimized static assets
- Create the service worker with Workbox
- Precache all static assets
- Generate the PWA manifest

## Step 5: Preview Production Build

Test the production build locally:

```bash
npm run preview
```

Then open your browser and test:
1. Navigate to the preview URL (usually `http://localhost:4173`)
2. Open DevTools and verify service worker registration
3. Test offline functionality
4. Test PWA installation prompt

## Step 6: PWA Installation Testing

### Desktop (Chrome/Edge)

1. Navigate to your app in Chrome or Edge
2. Look for the install icon in the address bar (⊕ or computer icon)
3. Click the icon and select "Install"
4. The app should open in a standalone window

### Mobile (iOS Safari)

1. Open the app in Safari
2. Tap the Share button
3. Tap "Add to Home Screen"
4. The app should appear as an icon on your home screen

### Mobile (Android Chrome)

1. Open the app in Chrome
2. Tap the menu (⋮)
3. Tap "Install app" or "Add to Home Screen"
4. The app should appear as an icon on your home screen

## Troubleshooting

### Service Worker Not Registering

**Problem**: Service worker doesn't appear in DevTools

**Solutions**:
1. Ensure you're using HTTPS or localhost (service workers require secure contexts)
2. Check browser console for errors
3. Verify `vite-plugin-pwa` is installed correctly
4. Clear browser cache and reload

### Module Not Found Errors

**Problem**: TypeScript errors about missing modules

**Solutions**:
1. Run `npm install` to ensure all dependencies are installed
2. Restart your TypeScript language server
3. Restart your IDE/editor
4. Run `npm run prepare` to sync SvelteKit types

### Cache Not Working

**Problem**: Pages not loading offline

**Solutions**:
1. Visit pages while online first (so they get cached)
2. Check DevTools → Application → Cache Storage
3. Verify service worker is in "activated" state
4. Check console for cache errors

### Installation Prompt Not Showing

**Problem**: Install button doesn't appear

**Solutions**:
1. Verify manifest.json is accessible at `/manifest.json`
2. Check manifest validation in DevTools → Application → Manifest
3. Ensure icons are properly configured and accessible
4. Try clearing site data and revisiting
5. On iOS, you won't see an automatic prompt (must use Share → Add to Home Screen)

### Type Errors After Installation

**Problem**: TypeScript shows errors for Workbox types

**Solutions**:
1. Ensure all Workbox packages are installed
2. Check that versions match (all should be 7.x)
3. Restart TypeScript server
4. Add `/// <reference lib="webworker" />` to service-worker.ts

## Next Steps

Once installation is complete and verified:

1. **Customize Caching Strategies**: Edit `src/service-worker.ts` to adjust cache strategies for your needs
2. **Add Icons**: Replace `/static/favicon.png` with proper PWA icons (192x192, 512x512)
3. **Configure Manifest**: Update `/static/manifest.json` with your app details
4. **Test Performance**: Run Lighthouse audit to verify PWA score
5. **Deploy**: Deploy your app to production (must use HTTPS)

## Verification Checklist

Before considering the PWA setup complete, verify:

- [ ] All dependencies installed without errors
- [ ] Development server runs successfully
- [ ] Service worker appears in DevTools
- [ ] Offline mode works (cached pages load)
- [ ] Production build completes successfully
- [ ] Manifest is valid (check DevTools)
- [ ] Installation prompt appears (or manual installation works)
- [ ] App runs in standalone mode after installation
- [ ] Update notifications work (test by changing version)
- [ ] Lighthouse PWA score is 90+

## Additional Resources

- [Workbox Documentation](https://developers.google.com/web/tools/workbox)
- [vite-plugin-pwa Documentation](https://vite-pwa-org.netlify.app/)
- [Chrome DevTools Service Worker Debugging](https://developer.chrome.com/docs/devtools/progressive-web-apps/)
- [PWA Checklist](https://web.dev/pwa-checklist/)

## Getting Help

If you encounter issues:

1. Check the browser console for errors
2. Review the PWA_SETUP.md documentation
3. Check Workbox/vite-plugin-pwa GitHub issues
4. Verify your environment meets prerequisites

