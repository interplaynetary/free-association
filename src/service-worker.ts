/// <reference lib="webworker" />

import { cleanupOutdatedCaches, precacheAndRoute } from 'workbox-precaching';
import { registerRoute } from 'workbox-routing';
import { CacheFirst, NetworkFirst, StaleWhileRevalidate } from 'workbox-strategies';
import { ExpirationPlugin } from 'workbox-expiration';
import { CacheableResponsePlugin } from 'workbox-cacheable-response';
import { BackgroundSyncPlugin } from 'workbox-background-sync';
import { NotificationManager } from './lib/notification-manager';

// This is injected by vite-plugin-pwa
declare let self: ServiceWorkerGlobalScope & {
	__WB_MANIFEST: Array<{ url: string; revision: string | null }>;
};

const SW_VERSION = '1.0.0';
const notificationManager = new NotificationManager();

console.log(`[Service Worker] v${SW_VERSION} initializing...`);

// ============================================================================
// PRECACHING - Build assets via Workbox
// ============================================================================

cleanupOutdatedCaches();

// self.__WB_MANIFEST is injected by vite-plugin-pwa with injectManifest
precacheAndRoute(self.__WB_MANIFEST);

// ============================================================================
// RUNTIME CACHING STRATEGIES
// ============================================================================

// Static assets (CSS, JS, Fonts) - CacheFirst with expiration
registerRoute(
	({ request, url }: { request: Request; url: URL }) => {
		// Only cache production assets, not dev resources
		const isDev = url.pathname.includes('/@vite/') || 
		              url.pathname.includes('/@fs/') ||
		              url.pathname.includes('/.svelte-kit/generated/') ||
		              url.search.includes('?v=') ||
		              url.search.includes('?t=');
		
		const isStaticAsset = request.destination === 'style' ||
		                      request.destination === 'script' ||
		                      request.destination === 'font';
		
		return isStaticAsset && !isDev;
	},
	new CacheFirst({
		cacheName: 'static-assets-v3',
		plugins: [
			new CacheableResponsePlugin({ statuses: [0, 200] }),
			new ExpirationPlugin({
				maxEntries: 60,
				maxAgeSeconds: 30 * 24 * 60 * 60 // 30 days
			})
		]
	})
);

// Images - CacheFirst with expiration
registerRoute(
	({ request }: { request: Request }) => request.destination === 'image',
	new CacheFirst({
		cacheName: 'images-v3',
		plugins: [
			new CacheableResponsePlugin({ statuses: [0, 200] }),
			new ExpirationPlugin({
				maxEntries: 100,
				maxAgeSeconds: 60 * 24 * 60 * 60 // 60 days
			})
		]
	})
);

// API routes - NetworkFirst with background sync fallback
const apiSyncPlugin = new BackgroundSyncPlugin('api-queue', {
	maxRetentionTime: 24 * 60 // Retry for up to 24 hours
});

registerRoute(
	({ url }: { url: URL }) => url.pathname.startsWith('/api/'),
	new NetworkFirst({
		cacheName: 'api-cache-v3',
		plugins: [
			new CacheableResponsePlugin({ statuses: [0, 200] }),
			new ExpirationPlugin({
				maxEntries: 50,
				maxAgeSeconds: 5 * 60 // 5 minutes
			}),
			apiSyncPlugin
		]
	})
);

// Google Fonts - CacheFirst, long-term
registerRoute(
	({ url }: { url: URL }) => 
		url.origin === 'https://fonts.googleapis.com' ||
		url.origin === 'https://fonts.gstatic.com',
	new CacheFirst({
		cacheName: 'google-fonts-v3',
		plugins: [
			new CacheableResponsePlugin({ statuses: [0, 200] }),
			new ExpirationPlugin({
				maxEntries: 20,
				maxAgeSeconds: 365 * 24 * 60 * 60 // 1 year
			})
		]
	})
);

// Documents/Pages - StaleWhileRevalidate for fast loads + fresh content
registerRoute(
	({ request, url }: { request: Request; url: URL }) => {
		const isDocument = request.destination === 'document' ||
		                   (request.mode === 'navigate' && request.method === 'GET');
		const isSameOrigin = url.origin === self.location.origin;
		const notApi = !url.pathname.startsWith('/api/');
		
		return isDocument && isSameOrigin && notApi;
	},
	new StaleWhileRevalidate({
		cacheName: 'pages-v3',
		plugins: [
			new CacheableResponsePlugin({ statuses: [0, 200] }),
			new ExpirationPlugin({
				maxEntries: 30,
				maxAgeSeconds: 24 * 60 * 60 // 24 hours
			})
		]
	})
);

// ============================================================================
// BACKGROUND SYNC
// ============================================================================

// Generic background sync handler
self.addEventListener('sync', (event) => {
	console.log('[Service Worker] Background sync:', event.tag);
	
	if (event.tag === 'sync-data') {
		event.waitUntil(syncData());
	} else if (event.tag === 'sync-holster') {
		event.waitUntil(syncHolsterData());
	}
});

async function syncData(): Promise<void> {
	console.log('[Service Worker] Syncing data...');
	try {
		// Notify main app that sync is starting
		const clients = await self.clients.matchAll();
		for (const client of clients) {
			client.postMessage({
				type: 'SYNC_STARTED',
				timestamp: Date.now()
			});
		}
		
		// Background sync logic would go here
		// For now, just notify success
		
		for (const client of clients) {
			client.postMessage({
				type: 'SYNC_COMPLETE',
				timestamp: Date.now()
			});
		}
		
		console.log('[Service Worker] Sync complete');
	} catch (error) {
		console.error('[Service Worker] Sync failed:', error);
		throw error; // Let workbox retry
	}
}

async function syncHolsterData(): Promise<void> {
	console.log('[Service Worker] Syncing Holster data...');
	try {
		const clients = await self.clients.matchAll();
		for (const client of clients) {
			client.postMessage({
				type: 'HOLSTER_SYNC_REQUESTED',
				timestamp: Date.now()
			});
		}
	} catch (error) {
		console.error('[Service Worker] Holster sync failed:', error);
		throw error;
	}
}

// ============================================================================
// PUSH NOTIFICATIONS
// ============================================================================

self.addEventListener('push', async (event) => {
	console.log('[Service Worker] Push notification received');
	
	if (!event.data) {
		console.log('[Service Worker] Push event has no data');
		return;
	}
	
	try {
		const data = event.data.json();
		console.log('[Service Worker] Push data:', data);
		
		event.waitUntil(
			notificationManager.queueNotification({
				id: data.id || `push-${Date.now()}`,
				tag: data.tag || 'default',
				title: data.title || 'Notification',
				body: data.body || '',
				source: data.source || 'push',
				icon: data.icon || '/favicon.png',
				badge: data.badge || '/favicon.png',
				data: data.data || {},
				timestamp: Date.now(),
				processed: false
			})
		);
	} catch (error) {
		console.error('[Service Worker] Error processing push:', error);
	}
});

// ============================================================================
// NOTIFICATION INTERACTIONS
// ============================================================================

self.addEventListener('notificationclick', (event) => {
	console.log('[Service Worker] Notification clicked:', event.action);
	
	event.notification.close();
	
	const notificationData = event.notification.data || {};
	const url = notificationData.url || '/';
	
	event.waitUntil(
		self.clients.matchAll({ type: 'window', includeUncontrolled: true }).then((clientList) => {
			// Try to focus existing window
			for (const client of clientList) {
				if (client.url === url && 'focus' in client) {
					return client.focus();
				}
			}
			// Open new window
			if (self.clients.openWindow) {
				return self.clients.openWindow(url);
			}
		})
	);
});

self.addEventListener('notificationclose', (event) => {
	console.log('[Service Worker] Notification closed:', event.notification.tag);
	// Track dismissals if needed
});

// ============================================================================
// MESSAGE HANDLING (App ↔ Service Worker communication)
// ============================================================================

self.addEventListener('message', async (event) => {
	const { type, data } = event.data || {};
	
	console.log('[Service Worker] Message received:', type);
	
	switch (type) {
		case 'SKIP_WAITING':
			// Force activate new service worker
			self.skipWaiting();
			break;
			
		case 'QUEUE_NOTIFICATION':
			// Queue a notification via NotificationManager
			await notificationManager.queueNotification(data);
			break;
			
		case 'CLEAR_NOTIFICATION':
			// Clear a specific notification
			await notificationManager.clearNotification(data.tag);
			break;
			
		case 'CLEAR_ALL_NOTIFICATIONS':
			// Clear all notifications
			await notificationManager.clearAllNotifications();
			break;
			
		case 'REQUEST_SYNC':
			// Manually trigger background sync
			if ('sync' in self.registration) {
				await self.registration.sync.register(data.tag || 'sync-data');
			}
			break;
			
		case 'GET_VERSION':
			// Return service worker version
			event.ports[0]?.postMessage({ version: SW_VERSION });
			break;
			
		case 'CACHE_URLS':
			// Manually cache specific URLs
			if (data.urls && Array.isArray(data.urls)) {
				await cacheUrls(data.urls);
			}
			break;
			
		default:
			console.log('[Service Worker] Unknown message type:', type);
	}
});

async function cacheUrls(urls: string[]): Promise<void> {
	const cache = await caches.open('manual-cache-v3');
	await cache.addAll(urls);
	console.log('[Service Worker] Cached URLs:', urls);
}

// ============================================================================
// LIFECYCLE EVENTS
// ============================================================================

self.addEventListener('install', (event) => {
	console.log(`[Service Worker] v${SW_VERSION} installing...`);
	// Force immediate activation
	self.skipWaiting();
});

self.addEventListener('activate', (event) => {
	console.log(`[Service Worker] v${SW_VERSION} activating...`);
	
	event.waitUntil(
		(async () => {
			// Take control of all clients immediately
			await self.clients.claim();
			
			// Notify all clients that new service worker is active
			const clients = await self.clients.matchAll();
			for (const client of clients) {
				client.postMessage({
					type: 'SW_ACTIVATED',
					version: SW_VERSION,
					timestamp: Date.now()
				});
			}
			
			console.log(`[Service Worker] v${SW_VERSION} activated and controlling clients`);
		})()
	);
});

// ============================================================================
// PERIODIC BACKGROUND SYNC (if supported)
// ============================================================================

self.addEventListener('periodicsync', (event: any) => {
	console.log('[Service Worker] Periodic sync:', event.tag);
	
	if (event.tag === 'content-sync') {
		event.waitUntil(syncData());
	}
});

console.log(`[Service Worker] v${SW_VERSION} initialized ✓`);

