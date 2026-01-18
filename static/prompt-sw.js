// This service worker's only job is to kill itself and the old caches
// It must reside at the same path as the old "zombie" service worker (likely /prompt-sw.js)

self.addEventListener('install', () => {
    // Take over immediately
    self.skipWaiting();
});

self.addEventListener('activate', (event) => {
    event.waitUntil(
        (async () => {
            // 1. Claim all clients so we can control them immediately
            await self.clients.claim();

            // 2. Clear ALL caches (nuclear option to ensure no stale assets remain)
            const keys = await caches.keys();
            await Promise.all(keys.map((key) => caches.delete(key)));
            console.log('[Kill-Switch] All caches cleared.');

            // 3. Unregister THIS service worker
            // This leaves the browser with "no service worker", so the next reload
            // will fetch the app fresh and (hopefully) install the NEW SvelteKit worker.
            await self.registration.unregister();
            console.log('[Kill-Switch] Service worker unregistered.');

            // 4. Force reload all clients to reset the state
            const clients = await self.clients.matchAll({ type: 'window' });
            clients.forEach((client) => client.navigate(client.url));
        })()
    );
});
