/**
 * Service Worker messaging utilities
 * Provides typed communication between app and service worker
 */

export interface SWMessage {
	type: string;
	data?: any;
}

export interface SWResponse {
	type: string;
	data?: any;
	version?: string;
	timestamp?: number;
}

/**
 * Send a message to the service worker
 */
export async function sendMessageToSW(message: SWMessage): Promise<void> {
	if (!('serviceWorker' in navigator)) {
		console.warn('[SW] Service Worker not supported');
		return;
	}

	const registration = await navigator.serviceWorker.ready;
	if (registration.active) {
		registration.active.postMessage(message);
	}
}

/**
 * Send a message and wait for response
 */
export async function sendMessageToSWWithResponse(message: SWMessage): Promise<SWResponse> {
	if (!('serviceWorker' in navigator)) {
		throw new Error('Service Worker not supported');
	}

	const registration = await navigator.serviceWorker.ready;
	if (!registration.active) {
		throw new Error('No active service worker');
	}

	return new Promise((resolve, reject) => {
		const messageChannel = new MessageChannel();
		
		messageChannel.port1.onmessage = (event) => {
			resolve(event.data);
		};

		registration.active.postMessage(message, [messageChannel.port2]);
		
		// Timeout after 5 seconds
		setTimeout(() => reject(new Error('Service Worker response timeout')), 5000);
	});
}

/**
 * Queue a notification via service worker
 */
export async function queueNotification(notification: {
	id?: string;
	tag: string;
	title: string;
	body: string;
	icon?: string;
	badge?: string;
	data?: any;
}): Promise<void> {
	await sendMessageToSW({
		type: 'QUEUE_NOTIFICATION',
		data: {
			...notification,
			id: notification.id || `notif-${Date.now()}`,
			icon: notification.icon || '/favicon.png',
			badge: notification.badge || '/favicon.png',
			timestamp: Date.now(),
			processed: false
		}
	});
}

/**
 * Clear a specific notification
 */
export async function clearNotification(tag: string): Promise<void> {
	await sendMessageToSW({
		type: 'CLEAR_NOTIFICATION',
		data: { tag }
	});
}

/**
 * Clear all notifications
 */
export async function clearAllNotifications(): Promise<void> {
	await sendMessageToSW({
		type: 'CLEAR_ALL_NOTIFICATIONS'
	});
}

/**
 * Request background sync
 */
export async function requestSync(tag: string = 'sync-data'): Promise<void> {
	if (!('serviceWorker' in navigator)) {
		console.warn('[SW] Service Worker not supported');
		return;
	}

	try {
		const registration = await navigator.serviceWorker.ready;
		
		// Try native sync API first
		if ('sync' in registration) {
			await (registration as any).sync.register(tag);
			console.log('[SW] Background sync registered:', tag);
		} else {
			// Fallback: send message to SW
			await sendMessageToSW({
				type: 'REQUEST_SYNC',
				data: { tag }
			});
			console.log('[SW] Background sync requested via message:', tag);
		}
	} catch (error) {
		console.error('[SW] Background sync failed:', error);
	}
}

/**
 * Request Holster data sync
 */
export async function requestHolsterSync(): Promise<void> {
	await requestSync('sync-holster');
}

/**
 * Get service worker version
 */
export async function getSWVersion(): Promise<string> {
	try {
		const response = await sendMessageToSWWithResponse({
			type: 'GET_VERSION'
		});
		return response.version || 'unknown';
	} catch (error) {
		console.error('[SW] Failed to get version:', error);
		return 'unknown';
	}
}

/**
 * Manually cache specific URLs
 */
export async function cacheUrls(urls: string[]): Promise<void> {
	await sendMessageToSW({
		type: 'CACHE_URLS',
		data: { urls }
	});
}

/**
 * Subscribe to push notifications
 */
export async function subscribeToPushNotifications(
	vapidPublicKey: string
): Promise<PushSubscription | null> {
	if (!('serviceWorker' in navigator) || !('PushManager' in window)) {
		console.warn('[SW] Push notifications not supported');
		return null;
	}

	try {
		const registration = await navigator.serviceWorker.ready;
		
		// Check if already subscribed
		let subscription = await registration.pushManager.getSubscription();
		
		if (!subscription) {
			// Subscribe to push notifications
			subscription = await registration.pushManager.subscribe({
				userVisibleOnly: true,
				applicationServerKey: urlBase64ToUint8Array(vapidPublicKey)
			});
			
			console.log('[SW] Push subscription created:', subscription.endpoint);
		}
		
		return subscription;
	} catch (error) {
		console.error('[SW] Push subscription failed:', error);
		return null;
	}
}

/**
 * Unsubscribe from push notifications
 */
export async function unsubscribeFromPushNotifications(): Promise<boolean> {
	if (!('serviceWorker' in navigator)) {
		return false;
	}

	try {
		const registration = await navigator.serviceWorker.ready;
		const subscription = await registration.pushManager.getSubscription();
		
		if (subscription) {
			await subscription.unsubscribe();
			console.log('[SW] Push subscription removed');
			return true;
		}
		
		return false;
	} catch (error) {
		console.error('[SW] Unsubscribe failed:', error);
		return false;
	}
}

/**
 * Register periodic background sync (if supported)
 */
export async function registerPeriodicSync(
	tag: string,
	minInterval: number = 24 * 60 * 60 * 1000 // 24 hours default
): Promise<boolean> {
	if (!('serviceWorker' in navigator)) {
		return false;
	}

	try {
		const registration = await navigator.serviceWorker.ready;
		
		if ('periodicSync' in registration) {
			await (registration as any).periodicSync.register(tag, {
				minInterval
			});
			console.log('[SW] Periodic sync registered:', tag);
			return true;
		}
		
		console.warn('[SW] Periodic sync not supported');
		return false;
	} catch (error) {
		console.error('[SW] Periodic sync registration failed:', error);
		return false;
	}
}

/**
 * Listen to messages from service worker
 */
export function listenToSWMessages(
	callback: (message: SWResponse) => void
): () => void {
	if (!('serviceWorker' in navigator)) {
		return () => {};
	}

	const handler = (event: MessageEvent) => {
		callback(event.data);
	};

	navigator.serviceWorker.addEventListener('message', handler);

	// Return cleanup function
	return () => {
		navigator.serviceWorker.removeEventListener('message', handler);
	};
}

// ============================================================================
// HELPERS
// ============================================================================

function urlBase64ToUint8Array(base64String: string): Uint8Array {
	const padding = '='.repeat((4 - (base64String.length % 4)) % 4);
	const base64 = (base64String + padding).replace(/-/g, '+').replace(/_/g, '/');
	const rawData = window.atob(base64);
	const outputArray = new Uint8Array(rawData.length);

	for (let i = 0; i < rawData.length; ++i) {
		outputArray[i] = rawData.charCodeAt(i);
	}

	return outputArray;
}

