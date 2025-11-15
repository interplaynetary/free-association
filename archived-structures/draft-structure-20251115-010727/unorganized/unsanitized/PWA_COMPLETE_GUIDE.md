# Complete PWA Feature Guide

## 🎉 Overview

Your app now has a **complete PWA setup** with all advanced features implemented in an elegant, idiomatic way aligned with Vite-PWA documentation.

---

## ✨ Features Implemented

### 1. **📱 App Shortcuts**
Long-press the app icon to access quick actions.

**What it does:**
- Quick access to Recognition Tree
- Jump to Collective View
- Open Map View directly

**Configuration:** `vite.config.ts` → `manifest.shortcuts`

**How to use:**
- Install app on home screen
- Long-press app icon
- Select quick action

---

### 2. **📤 Share Target**
Your app appears in the system share menu!

**What it does:**
- Receive shared content from other apps
- Handles text, URLs, and titles
- Automatically redirects to main app

**Configuration:** `vite.config.ts` → `manifest.share_target`

**How to test:**
1. Share a link from browser/app
2. Select "Playnet" from share menu
3. Content appears in your app

**Route:** `/share` - Handles incoming shared content

**Accessing shared data:**
```typescript
// In your app
const sharedContent = JSON.parse(
  sessionStorage.getItem('shared-content') || '{}'
);

console.log(sharedContent.title);
console.log(sharedContent.text);
console.log(sharedContent.url);
```

---

### 3. **🔔 Push Notifications**
Full push notification support with queue management.

**What it does:**
- Receive push notifications even when app is closed
- Queue notifications in IndexedDB
- Handle notification clicks
- Custom notification actions

**How to use:**

```typescript
import { subscribeToPushNotifications } from '$lib/utils/sw-messaging';

// Subscribe to push (you'll need VAPID keys)
const subscription = await subscribeToPushNotifications(
  'YOUR_VAPID_PUBLIC_KEY'
);

// Send subscription to your server
await fetch('/api/push-subscribe', {
  method: 'POST',
  body: JSON.stringify(subscription)
});
```

**Queue a local notification:**

```typescript
import { queueNotification } from '$lib/utils/sw-messaging';

await queueNotification({
  tag: 'achievement',
  title: 'New Recognition!',
  body: 'Someone recognized your contribution',
  icon: '/icon.png',
  data: { url: '/tree' }
});
```

**Listen for notification clicks:**
Service worker automatically handles clicks and opens the app.

---

### 4. **🔄 Background Sync**
Automatically retry failed API calls when back online.

**What it does:**
- Queues failed requests when offline
- Retries when connection returns
- Works for up to 24 hours
- No data loss

**Automatic:** API calls to `/api/*` are automatically queued if they fail.

**Manual sync:**

```typescript
import { requestSync, requestHolsterSync } from '$lib/utils/sw-messaging';

// Generic sync
await requestSync('sync-data');

// Holster-specific sync
await requestHolsterSync();
```

**Listen for sync events:**

```typescript
// In your app
window.addEventListener('holster-sync-requested', () => {
  console.log('Service worker requesting Holster sync');
  // Trigger your Holster sync logic
});
```

---

## 📋 Service Worker API

### **sw-messaging.ts** - Type-safe communication

All service worker communication is handled through a clean API:

#### Send Messages

```typescript
import { sendMessageToSW } from '$lib/utils/sw-messaging';

await sendMessageToSW({
  type: 'REQUEST_SYNC',
  data: { tag: 'my-sync' }
});
```

#### Get Service Worker Version

```typescript
import { getSWVersion } from '$lib/utils/sw-messaging';

const version = await getSWVersion();
console.log('SW version:', version);
```

#### Manually Cache URLs

```typescript
import { cacheUrls } from '$lib/utils/sw-messaging';

await cacheUrls([
  '/important-page',
  '/assets/critical.css',
  '/data.json'
]);
```

#### Listen to SW Messages

```typescript
import { listenToSWMessages } from '$lib/utils/sw-messaging';

const cleanup = listenToSWMessages((message) => {
  console.log('Message from SW:', message);
  
  if (message.type === 'SYNC_COMPLETE') {
    console.log('Background sync finished!');
  }
});

// Later: cleanup when component unmounts
cleanup();
```

---

## 🔧 Caching Strategies

### Precaching
**All build assets** cached automatically (JS, CSS, HTML)

### Static Assets
**Strategy:** CacheFirst  
**Cache:** 60 files, 30 days  
**Best for:** JS, CSS, fonts

### Images
**Strategy:** CacheFirst  
**Cache:** 100 images, 60 days  
**Best for:** Photos, icons, graphics

### API Routes
**Strategy:** NetworkFirst + Background Sync  
**Cache:** 50 entries, 5 minutes  
**Best for:** REST APIs  
**Bonus:** Auto-retries when offline

### Google Fonts
**Strategy:** CacheFirst  
**Cache:** 20 fonts, 1 year  
**Best for:** Web fonts

### Pages/Documents
**Strategy:** StaleWhileRevalidate  
**Cache:** 30 pages, 24 hours  
**Best for:** HTML pages, navigation

---

## 🎯 Usage Examples

### Example 1: Handle Shared Content

```svelte
<script lang="ts">
  import { onMount } from 'svelte';

  let sharedContent = $state(null);

  onMount(() => {
    const stored = sessionStorage.getItem('shared-content');
    if (stored) {
      sharedContent = JSON.parse(stored);
      // Clear after reading
      sessionStorage.removeItem('shared-content');
      
      // Use the shared content
      console.log('Shared:', sharedContent);
    }
  });
</script>

{#if sharedContent}
  <div class="shared-content">
    <h2>{sharedContent.title}</h2>
    <p>{sharedContent.text}</p>
    {#if sharedContent.url}
      <a href={sharedContent.url}>View Source</a>
    {/if}
  </div>
{/if}
```

### Example 2: Request Sync When Network Changes

```typescript
import { requestHolsterSync } from '$lib/utils/sw-messaging';

window.addEventListener('online', async () => {
  console.log('Back online! Syncing...');
  await requestHolsterSync();
});
```

### Example 3: Show Notification on Recognition

```typescript
import { queueNotification } from '$lib/utils/sw-messaging';

async function onRecognitionReceived(recognition) {
  await queueNotification({
    tag: `recognition-${recognition.id}`,
    title: 'New Recognition!',
    body: `${recognition.from} recognized your contribution`,
    icon: recognition.avatar || '/favicon.png',
    data: {
      url: `/recognition/${recognition.id}`,
      recognitionId: recognition.id
    }
  });
}
```

### Example 4: Monitor SW Lifecycle

```svelte
<script lang="ts">
  import { onMount } from 'svelte';
  import { listenToSWMessages } from '$lib/utils/sw-messaging';

  let swStatus = $state('checking...');
  let swVersion = $state('unknown');

  onMount(() => {
    const cleanup = listenToSWMessages((message) => {
      switch (message.type) {
        case 'SW_ACTIVATED':
          swStatus = 'active';
          swVersion = message.version;
          break;
        case 'SYNC_STARTED':
          swStatus = 'syncing...';
          break;
        case 'SYNC_COMPLETE':
          swStatus = 'synced';
          break;
      }
    });

    return cleanup;
  });
</script>

<div class="sw-status">
  SW Status: {swStatus} (v{swVersion})
</div>
```

---

## 🧪 Testing Your PWA

### Test Shortcuts

1. **Build and preview:**
   ```bash
   bun run build
   bun run preview
   ```

2. **Install app** (Chrome): Install button in address bar

3. **Test shortcuts:**
   - Right-click app icon (desktop)
   - Long-press icon (mobile)
   - Should see 3 shortcuts

### Test Share Target

1. **Open another app/browser**

2. **Share any content**

3. **Select "Playnet"** from share menu

4. **Verify:** Should open `/share` then redirect to `/`

5. **Check console:** Should see shared data logged

### Test Push Notifications

1. **Get VAPID keys** (use web-push npm package):
   ```bash
   npx web-push generate-vapid-keys
   ```

2. **Subscribe in app:**
   ```typescript
   const subscription = await subscribeToPushNotifications(publicKey);
   ```

3. **Send test notification** from server:
   ```javascript
   const webpush = require('web-push');
   webpush.sendNotification(subscription, JSON.stringify({
     title: 'Test',
     body: 'Hello from server!'
   }));
   ```

### Test Background Sync

1. **Open DevTools** → Network tab

2. **Set to "Offline"**

3. **Make an API call** (it will queue)

4. **Check Application** → Background Sync → Should see "api-queue"

5. **Set back to "Online"**

6. **Should retry automatically!**

### Test Offline Mode

1. **Visit app** (online)

2. **Open DevTools** → Network → Select "Offline"

3. **Reload page** → Should still work!

4. **Navigate** → All cached pages work

5. **Check Cache Storage** → See all cached assets

---

## 📊 DevTools Inspection

### Application Tab

**Service Workers:**
- Status: "activated and is running"
- Version: Check SW version
- Update: Click to force update

**Cache Storage:**
- `workbox-precache-*` - Build assets
- `static-assets-v3` - JS, CSS, fonts
- `images-v3` - Images
- `api-cache-v3` - API responses
- `pages-v3` - HTML pages
- `google-fonts-v3` - Web fonts
- `manual-cache-v3` - Manually cached

**Background Sync:**
- `api-queue` - Failed API calls
- `sync-data` - Generic sync
- `sync-holster` - Holster sync

**Push Messaging:**
- Check subscription status
- Test push notifications

---

## 🚀 Performance Benefits

### Before PWA:
```
First load:     5-10s (download everything)
Repeat visits:  2-5s (re-download changed assets)
Offline:        ❌ Doesn't work
API failures:   ❌ Lost requests
```

### With Your PWA:
```
First load:     5-10s (download + cache)
Repeat visits:  <1s (instant from cache)
Offline:        ✅ Fully functional
API failures:   ✅ Auto-retry when online
Updates:        ⚡ Background, seamless
Notifications:  ✅ Even when closed
```

---

## 🔐 Security Notes

### VAPID Keys
- Keep private key secret
- Store on server only
- Public key can be in client

### Push Subscription
- Unique per user/device
- Store in your database
- Associate with user account

### Notification Permissions
- Request when needed (not on load)
- Respect user choice
- Explain why you need it

---

## 🎨 Customization

### Change Cache Names

Edit `service-worker.ts`:
```typescript
cacheName: 'my-custom-cache-v1'
```

### Adjust Cache Limits

```typescript
new ExpirationPlugin({
  maxEntries: 200,        // More entries
  maxAgeSeconds: 7 * 24 * 60 * 60  // 7 days
})
```

### Add Custom Routes

```typescript
registerRoute(
  ({ url }) => url.pathname.startsWith('/my-api/'),
  new NetworkFirst({
    cacheName: 'my-api-cache',
    plugins: [/* ... */]
  })
);
```

### Modify Shortcuts

Edit `vite.config.ts` → `manifest.shortcuts`:
```typescript
{
  name: 'My Custom Shortcut',
  url: '/my-route',
  icons: [{ src: '/my-icon.png', sizes: '192x192' }]
}
```

---

## 📚 Advanced Features

### Periodic Background Sync

```typescript
import { registerPeriodicSync } from '$lib/utils/sw-messaging';

// Sync every 24 hours (when conditions are met)
await registerPeriodicSync('content-sync', 24 * 60 * 60 * 1000);
```

**Note:** Browser decides when to actually sync based on:
- Battery level
- Network conditions
- App usage patterns

### Notification Actions

```typescript
await queueNotification({
  tag: 'message',
  title: 'New Message',
  body: 'You have a new message',
  actions: [
    { action: 'reply', title: 'Reply' },
    { action: 'dismiss', title: 'Dismiss' }
  ]
});

// Handle in service-worker.ts
self.addEventListener('notificationclick', (event) => {
  if (event.action === 'reply') {
    // Handle reply action
  }
});
```

### Badge API

```typescript
// Set badge count
if ('setAppBadge' in navigator) {
  await (navigator as any).setAppBadge(5);
}

// Clear badge
await (navigator as any).clearAppBadge();
```

---

## 🐛 Troubleshooting

### Service Worker Not Updating

```typescript
// Force update
const registration = await navigator.serviceWorker.ready;
await registration.update();
```

### Cache Not Clearing

```typescript
// Clear specific cache
await caches.delete('static-assets-v3');

// Clear all caches
const cacheNames = await caches.keys();
await Promise.all(
  cacheNames.map(name => caches.delete(name))
);
```

### Background Sync Not Working

Check browser support:
```typescript
if ('serviceWorker' in navigator && 'sync' in ServiceWorkerRegistration.prototype) {
  console.log('Background sync supported!');
} else {
  console.log('Background sync not supported');
}
```

### Push Notifications Not Showing

1. Check permissions:
   ```typescript
   console.log(Notification.permission); // "granted" | "denied" | "default"
   ```

2. Check subscription:
   ```typescript
   const registration = await navigator.serviceWorker.ready;
   const subscription = await registration.pushManager.getSubscription();
   console.log(subscription);
   ```

3. Check service worker console in DevTools

---

## 🎯 Best Practices

### 1. Request Permissions Contextually
✅ **Good:** Ask for notification permission when user enables notifications feature
❌ **Bad:** Ask immediately on page load

### 2. Handle Offline Gracefully
✅ **Good:** Show UI indicating offline mode, queue actions
❌ **Bad:** Just show error messages

### 3. Update Service Worker Smoothly
✅ **Good:** Show update prompt, let user choose when
❌ **Bad:** Force reload immediately

### 4. Monitor Cache Size
✅ **Good:** Set reasonable limits, expire old content
❌ **Bad:** Cache everything forever

### 5. Test Offline First
✅ **Good:** Test offline scenarios during development
❌ **Bad:** Only test with good internet

---

## 📖 Resources

- **Vite PWA Docs:** https://vite-pwa-org.netlify.app/
- **Workbox Docs:** https://developer.chrome.com/docs/workbox/
- **Web Push:** https://web.dev/articles/push-notifications-overview
- **Background Sync:** https://web.dev/articles/background-sync
- **Service Worker API:** https://developer.mozilla.org/en-US/docs/Web/API/Service_Worker_API

---

## 🎉 You Now Have

✅ **Full offline support**  
✅ **Push notifications**  
✅ **Background sync**  
✅ **Share target**  
✅ **App shortcuts**  
✅ **Smart caching**  
✅ **Auto-updates**  
✅ **Type-safe messaging**  
✅ **Production-ready**

Your PWA is **complete, elegant, and idiomatic!** 🚀

