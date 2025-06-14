import { precacheAndRoute, cleanupOutdatedCaches } from 'workbox-precaching';

declare const self: ServiceWorkerGlobalScope;

// Precache app shell
precacheAndRoute(self.__WB_MANIFEST);
cleanupOutdatedCaches();

// Handle push notifications
self.addEventListener('push', (event: any) => {
	const options = {
		body: event.data?.text() || 'You have a new notification',
		icon: '/favicon.png',
		badge: '/favicon.png',
		vibrate: [100, 50, 100],
		data: {
			dateOfArrival: Date.now(),
			primaryKey: 1
		},
		actions: [
			{
				action: 'explore',
				title: 'View',
				icon: '/favicon.png'
			},
			{
				action: 'close',
				title: 'Close',
				icon: '/favicon.png'
			}
		]
	};

	event.waitUntil(self.registration.showNotification('Free Association', options));
});

// Handle notification clicks
self.addEventListener('notificationclick', (event: any) => {
	event.notification.close();

	if (event.action === 'explore') {
		event.waitUntil(self.clients.openWindow('/'));
	}
});

// Handle background sync for notifications
self.addEventListener('sync', (event: any) => {
	if (event.tag === 'notification-sync') {
		event.waitUntil(syncNotifications());
	}
});

async function syncNotifications() {
	// This would sync with your notification queue
	// For now, just log that sync happened
	console.log('Background sync for notifications');
}
