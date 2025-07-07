/// <reference types="@sveltejs/kit" />
import { build, files, version } from '$service-worker';

// Polyfill window object for Gun in service worker context
if (typeof window === 'undefined') {
	// @ts-ignore
	globalThis.window = {
		crypto: self.crypto,
		TextEncoder: self.TextEncoder,
		TextDecoder: self.TextDecoder,
		WebSocket: self.WebSocket,
		location: self.location
	};
}

import Gun from 'gun';
import 'gun/lib/rindexed';
import 'gun/lib/store'; // Bridges GUN storage adapter logic
import 'gun/lib/webrtc.js';
import { NotificationManager } from './lib/notification-manager';

// Create Gun instance
const gun = new Gun({
	peers: [
		'https://gun-manhattan.herokuapp.com/gun',
		'https://peer.wallie.io/gun',
		'https://gun.defucc.me/gun'
	],
	localStorage: false,
	store: window.RindexedDB,
	radisk: false
});

// Function to check connected peers
function getConnectedPeers() {
	try {
		// @ts-ignore - Gun's internal structure
		const peers = gun._.opt.peers;
		if (!peers) return [];

		return Object.values(peers).filter((peer: any) => {
			return peer && peer.wire && peer.wire.readyState === 1;
		});
	} catch (error) {
		console.error('Error checking connected peers:', error);
		return [];
	}
}

// Initialize notification manager
const notificationManager = new NotificationManager();

// Create unique cache name for this deployment
const CACHE = `cache-${version}`;
const ASSETS = [
	...build, // the app itself
	...files // everything in `static`
];

// Service worker event handlers
self.addEventListener('install', (event) => {
	// Create a new cache and add all files to it
	async function addFilesToCache() {
		const cache = await caches.open(CACHE);
		await cache.addAll(ASSETS);
	}

	event.waitUntil(addFilesToCache());
});

self.addEventListener('activate', (event) => {
	// Remove previous cached data from disk
	async function deleteOldCaches() {
		for (const key of await caches.keys()) {
			if (key !== CACHE) await caches.delete(key);
		}
	}

	event.waitUntil(deleteOldCaches());
});

self.addEventListener('fetch', (event) => {
	// Ignore POST requests etc
	if (event.request.method !== 'GET') return;

	async function respond() {
		const url = new URL(event.request.url);
		const cache = await caches.open(CACHE);

		// `build`/`files` can always be served from the cache
		if (ASSETS.includes(url.pathname)) {
			const response = await cache.match(url.pathname);
			if (response) {
				return response;
			}
		}

		// For everything else, try the network first, but
		// fall back to the cache if we're offline
		try {
			const response = await fetch(event.request);

			// If we're offline, fetch can return a value that is not a Response
			// instead of throwing - and we can't pass this non-Response to respondWith
			if (!(response instanceof Response)) {
				throw new Error('invalid response from fetch');
			}

			if (response.status === 200) {
				cache.put(event.request, response.clone());
			}

			return response;
		} catch (err) {
			const response = await cache.match(event.request);

			if (response) {
				return response;
			}

			// If there's no cache, then just error out
			// as there is nothing we can do to respond to this request
			throw err;
		}
	}

	event.respondWith(respond());
});

self.addEventListener('notificationclick', (event) => {
	event.notification.close();

	// @ts-ignore
	event.waitUntil(
		// @ts-ignore
		clients.matchAll().then((clientList) => {
			if (clientList.length > 0) {
				return clientList[0].focus();
			}
			// @ts-ignore
			const basePath = self.registration.scope.replace(self.location.origin, '').replace(/\/$/, '');
			// @ts-ignore
			return clients.openWindow(basePath || '/');
		})
	);
});

self.addEventListener('message', async (event) => {
	const { type, data } = event.data;

	switch (type) {
		case 'QUEUE_NOTIFICATION':
			await notificationManager.queueNotification(data);
			break;
		case 'CLEAR_NOTIFICATION':
			await notificationManager.clearNotification(data.tag);
			break;
		case 'CLEAR_ALL_NOTIFICATIONS':
			await notificationManager.clearAllNotifications();
			break;
		case 'GET_PEER_COUNT':
			const peerCount = getConnectedPeers().length;
			console.log('Connected peers:', peerCount);
			break;
		default:
			console.log('Unknown message type:', type);
	}
});

// Set up Gun subscription for notifications
gun
	.get('test')
	.get('paste')
	.on((data: any) => {
		if (data && typeof data === 'string') {
			notificationManager.queueNotification({
				title: 'New Paste Available',
				body: data.length > 100 ? `${data.substring(0, 100)}...` : data,
				tag: 'paste-notification',
				source: 'gun-paste',
				timestamp: Date.now(),
				data: { content: data }
			});
		}
	});

console.log('getConnectedPeers', getConnectedPeers())