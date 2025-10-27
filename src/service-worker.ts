/// <reference types="@sveltejs/kit" />
/// <reference lib="webworker" />

import { build, files, prerendered, version } from '$service-worker';
import { NotificationManager } from './lib/notification-manager';
import {
	cleanupOutdatedCaches,
	createHandlerBoundToURL,
	precacheAndRoute
} from 'workbox-precaching';
import { NavigationRoute, registerRoute } from 'workbox-routing';
import {
	CacheFirst,
	NetworkFirst,
	StaleWhileRevalidate,
	NetworkOnly
} from 'workbox-strategies';
import { ExpirationPlugin } from 'workbox-expiration';
import { CacheableResponsePlugin } from 'workbox-cacheable-response';

declare const self: ServiceWorkerGlobalScope;

// Initialize notification manager
const notificationManager = new NotificationManager();

// Workbox: Clean up outdated caches
cleanupOutdatedCaches();

// Workbox: Precache all static assets
const manifest = [
	...build.map((file: string) => ({ url: file, revision: version })),
	...files.map((file: string) => ({ url: file, revision: version })),
	...prerendered.map((file: string) => ({ url: file, revision: version }))
];

precacheAndRoute(manifest);

// Workbox: Cache strategy for navigation requests (app shell)
const handler = createHandlerBoundToURL('/index.html');
const navigationRoute = new NavigationRoute(handler, {
	denylist: [/^\/api\//, /\.json$/, /\.xml$/]
});
registerRoute(navigationRoute);

// Workbox: Cache strategy for static assets (CSS, JS, Fonts)
registerRoute(
	({ request }: { request: Request }) =>
		request.destination === 'style' ||
		request.destination === 'script' ||
		request.destination === 'font',
	new CacheFirst({
		cacheName: 'static-assets-v1',
		plugins: [
			new CacheableResponsePlugin({
				statuses: [0, 200]
			}),
			new ExpirationPlugin({
				maxEntries: 60,
				maxAgeSeconds: 30 * 24 * 60 * 60 // 30 days
			})
		]
	})
);

// Workbox: Cache strategy for images
registerRoute(
	({ request }: { request: Request }) => request.destination === 'image',
	new CacheFirst({
		cacheName: 'images-v1',
		plugins: [
			new CacheableResponsePlugin({
				statuses: [0, 200]
			}),
			new ExpirationPlugin({
				maxEntries: 100,
				maxAgeSeconds: 60 * 24 * 60 * 60 // 60 days
			})
		]
	})
);

// Workbox: Cache strategy for API calls (Network First with fallback)
registerRoute(
	({ url }: { url: URL }) => url.pathname.startsWith('/api/'),
	new NetworkFirst({
		cacheName: 'api-cache-v1',
		plugins: [
			new CacheableResponsePlugin({
				statuses: [0, 200]
			}),
			new ExpirationPlugin({
				maxEntries: 50,
				maxAgeSeconds: 5 * 60 // 5 minutes
			})
		]
	})
);

// Workbox: Stale-while-revalidate for dynamic content
registerRoute(
	({ url }: { url: URL }) => url.origin === self.location.origin && !url.pathname.startsWith('/api/'),
	new StaleWhileRevalidate({
		cacheName: 'dynamic-content-v1',
		plugins: [
			new CacheableResponsePlugin({
				statuses: [0, 200]
			}),
			new ExpirationPlugin({
				maxEntries: 50,
				maxAgeSeconds: 24 * 60 * 60 // 24 hours
			})
		]
	})
);

// Workbox: Network-only for websocket connections and external resources
registerRoute(
	({ url }: { url: URL }) => url.protocol === 'wss:' || url.protocol === 'ws:',
	new NetworkOnly()
);

// Service worker lifecycle events
self.addEventListener('install', (event) => {
	console.log('[Service Worker] Installing...');
	self.skipWaiting();
});

self.addEventListener('activate', (event) => {
	console.log('[Service Worker] Activating...');
	event.waitUntil(self.clients.claim());
});

// Notification click handler
self.addEventListener('notificationclick', (event) => {
	event.notification.close();

	event.waitUntil(
		self.clients.matchAll({ type: 'window' }).then((clientList) => {
			// Check if there's already a window open
			for (const client of clientList) {
				if ('focus' in client) {
					return client.focus();
				}
			}
			// If no window is open, open a new one
			if (self.clients.openWindow) {
				const basePath = self.registration.scope.replace(self.location.origin, '').replace(/\/$/, '');
				const targetUrl = basePath || '/';
				return self.clients.openWindow(targetUrl);
			}
		})
	);
});

// Message handler for notifications and other commands
self.addEventListener('message', async (event) => {
	const { type, data } = event.data;

	switch (type) {
		case 'SKIP_WAITING':
			self.skipWaiting();
			break;
		case 'QUEUE_NOTIFICATION':
			await notificationManager.queueNotification(data);
			break;
		case 'CLEAR_NOTIFICATION':
			await notificationManager.clearNotification(data.tag);
			break;
		case 'CLEAR_ALL_NOTIFICATIONS':
			await notificationManager.clearAllNotifications();
			break;
		default:
			console.log('Unknown message type:', type);
	}
});

console.log('[Service Worker] Initialized with Workbox');
