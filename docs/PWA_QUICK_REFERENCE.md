# PWA Quick Reference Card

## 🚀 Common Operations

### Queue a Notification
```typescript
import { queueNotification } from '$lib/utils/sw-messaging';

await queueNotification({
  tag: 'unique-id',
  title: 'Title',
  body: 'Message',
  icon: '/icon.png'
});
```

### Request Background Sync
```typescript
import { requestSync, requestHolsterSync } from '$lib/utils/sw-messaging';

await requestSync('sync-data');      // Generic
await requestHolsterSync();          // Holster-specific
```

### Listen to SW Messages
```typescript
import { listenToSWMessages } from '$lib/utils/sw-messaging';

const cleanup = listenToSWMessages((msg) => {
  console.log(msg.type, msg.data);
});

// Later: cleanup();
```

### Cache URLs Manually
```typescript
import { cacheUrls } from '$lib/utils/sw-messaging';

await cacheUrls(['/page1', '/page2']);
```

### Get SW Version
```typescript
import { getSWVersion } from '$lib/utils/sw-messaging';

const version = await getSWVersion();
```

### Subscribe to Push
```typescript
import { subscribeToPushNotifications } from '$lib/utils/sw-messaging';

const sub = await subscribeToPushNotifications('VAPID_PUBLIC_KEY');
```

---

## 📱 PWA Features

| Feature | Location | Notes |
|---------|----------|-------|
| **Shortcuts** | Long-press icon | 3 quick actions |
| **Share Target** | System share menu | Receives shared content |
| **Push Notifications** | Background | Even when app closed |
| **Background Sync** | Auto | Retries failed API calls |
| **Offline Mode** | Auto | All cached content works |

---

## 🗂️ Cache Names

| Cache | Strategy | Max Entries | Max Age |
|-------|----------|-------------|---------|
| `static-assets-v3` | CacheFirst | 60 | 30 days |
| `images-v3` | CacheFirst | 100 | 60 days |
| `api-cache-v3` | NetworkFirst | 50 | 5 min |
| `google-fonts-v3` | CacheFirst | 20 | 1 year |
| `pages-v3` | StaleWhileRevalidate | 30 | 24 hours |

---

## 🔧 Message Types

### To Service Worker
- `SKIP_WAITING` - Activate new SW
- `QUEUE_NOTIFICATION` - Queue notification
- `CLEAR_NOTIFICATION` - Clear notification
- `CLEAR_ALL_NOTIFICATIONS` - Clear all
- `REQUEST_SYNC` - Trigger sync
- `GET_VERSION` - Get SW version
- `CACHE_URLS` - Cache URLs manually

### From Service Worker
- `SW_ACTIVATED` - SW activated
- `SYNC_STARTED` - Sync starting
- `SYNC_COMPLETE` - Sync finished
- `HOLSTER_SYNC_REQUESTED` - Holster sync needed

---

## 🧪 Testing Commands

```bash
# Build for production
bun run build

# Preview production build
bun run preview

# Check what's cached
# DevTools > Application > Cache Storage

# Test offline
# DevTools > Network > Offline

# Force SW update
# DevTools > Application > Service Workers > Update
```

---

## 🐛 Debug Checklist

- [ ] Service Worker registered? (check console)
- [ ] Cache Storage populated? (check DevTools)
- [ ] Notifications permitted? (`Notification.permission`)
- [ ] Push subscription active? (check Application tab)
- [ ] Background sync registered? (check Application tab)
- [ ] Offline mode works? (test with Network offline)

---

## 📂 Key Files

| File | Purpose |
|------|---------|
| `src/service-worker.ts` | Main SW logic |
| `src/lib/utils/pwa.ts` | SW registration |
| `src/lib/utils/sw-messaging.ts` | SW communication API |
| `src/routes/share/+page.svelte` | Share target handler |
| `vite.config.ts` | PWA configuration |

---

## 🔗 External Docs

- [Vite PWA](https://vite-pwa-org.netlify.app/)
- [Workbox](https://developer.chrome.com/docs/workbox/)
- [Web Push](https://web.dev/articles/push-notifications-overview)

---

**Full guide:** `docs/PWA_COMPLETE_GUIDE.md`

