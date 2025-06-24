// Type definitions for Gun and service worker integration
interface GunInstance {
	get: (key: string) => any;
	user: () => any;
	_: any;
}

interface ServiceWorkerMessage {
	type: string;
	itemType?: string;
	key?: string;
	subscription?: string;
}

interface NotificationData {
	type: string;
	peerId?: string;
	peerName?: string;
	messageId?: string;
	timestamp?: string;
}

// Service Worker Subscription Configuration
export interface ServiceWorkerSubscriptionConfig {
	id: string;
	path: string;
	scope: 'gun' | 'user';
	notification: {
		title: string;
		body: string;
		icon?: string;
		badge?: string;
		tag?: string;
		requireInteraction?: boolean;
		vibrate?: number[];
		actions?: { action: string; title: string }[];
	};
	dataProcessor?: string;
}

// Default subscription configurations
const defaultSubscriptionConfigs: ServiceWorkerSubscriptionConfig[] = [
	{
		id: 'chat',
		path: 'chat',
		scope: 'gun',
		notification: {
			title: 'New Message',
			body: '{{senderAlias}}: {{decryptedMessage}}',
			icon: '/favicon.png',
			badge: '/favicon.png',
			requireInteraction: true,
			vibrate: [200, 100, 200],
			actions: [
				{ action: 'reply', title: 'Reply' },
				{ action: 'view', title: 'View Chat' }
			]
		},
		dataProcessor: 'decryptMessage'
	},
	{
		id: 'capacities',
		path: 'capacities',
		scope: 'gun',
		notification: {
			title: 'New Capacity Offered',
			body: '{{senderAlias}} is offering: {{capacity}}',
			icon: '/favicon.png',
			badge: '/favicon.png',
			requireInteraction: true,
			actions: [
				{ action: 'view', title: 'View Offer' },
				{ action: 'respond', title: 'Respond' }
			]
		},
		dataProcessor: 'processCapacity'
	},
	{
		id: 'rooms',
		path: 'rooms',
		scope: 'gun',
		notification: {
			title: 'Room Invitation',
			body: '{{senderAlias}} invited you to "{{room}}"',
			icon: '/favicon.png',
			badge: '/favicon.png',
			requireInteraction: true,
			actions: [
				{ action: 'join', title: 'Join Room' },
				{ action: 'view', title: 'View Details' }
			]
		}
	},
	{
		id: 'user-tree',
		path: 'tree',
		scope: 'user',
		notification: {
			title: 'Tree Updated',
			body: 'Your collaboration tree has been updated',
			icon: '/favicon.png'
		}
	},
	{
		id: 'user-capacities',
		path: 'capacities',
		scope: 'user',
		notification: {
			title: 'Capacities Updated',
			body: 'Your capacities have been updated',
			icon: '/favicon.png'
		}
	}
];

/**
 * Load subscription configuration from JSON file
 */
async function loadSubscriptionConfig(): Promise<ServiceWorkerSubscriptionConfig[]> {
	try {
		const response = await fetch('/src/lib/sw-config.json');
		if (!response.ok) {
			console.warn('[SW Config] Could not load sw-config.json, using defaults');
			return defaultSubscriptionConfigs;
		}
		const config = await response.json();
		return config.subscriptions || defaultSubscriptionConfigs;
	} catch (error) {
		console.warn('[SW Config] Error loading configuration, using defaults:', error);
		return defaultSubscriptionConfigs;
	}
}

/**
 * Initialize service worker with Gun subscriptions
 */
export async function initializeServiceWorker(
	customConfig?: ServiceWorkerSubscriptionConfig[]
): Promise<void> {
	if (typeof window === 'undefined' || !('serviceWorker' in navigator)) {
		return;
	}

	try {
		const registration = await navigator.serviceWorker.ready;

		// Use custom config, or load from JSON file, or fallback to defaults
		let config: ServiceWorkerSubscriptionConfig[];
		if (customConfig) {
			config = customConfig;
		} else {
			config = await loadSubscriptionConfig();
		}

		console.log('[SW Init] Setting up service worker with subscriptions:', config);

		// Send configuration to service worker
		registration.active?.postMessage({
			type: 'SETUP_SUBSCRIPTIONS',
			config
		});

		// Initialize Gun in service worker
		registration.active?.postMessage({
			type: 'INIT_GUN'
		});

		console.log('[SW Init] Service worker initialized with Gun subscriptions');
	} catch (error) {
		console.error('[SW Init] Error initializing service worker:', error);
	}
}

/**
 * Send authentication credentials to service worker
 */
export async function authenticateServiceWorker(alias: string, password: string): Promise<void> {
	if (typeof window === 'undefined' || !('serviceWorker' in navigator)) {
		return;
	}

	try {
		const registration = await navigator.serviceWorker.ready;
		registration.active?.postMessage({
			type: 'GUN_AUTH',
			credentials: { alias, password }
		});
		console.log('[SW Auth] Authentication credentials sent to service worker');
	} catch (error) {
		console.error('[SW Auth] Error sending auth to service worker:', error);
	}
}

/**
 * Send a message through the service worker
 */
export async function sendMessageThroughServiceWorker(
	text: string,
	chatId?: string
): Promise<void> {
	if (typeof window === 'undefined' || !('serviceWorker' in navigator)) {
		return;
	}

	try {
		const registration = await navigator.serviceWorker.ready;
		registration.active?.postMessage({
			type: 'SEND_MESSAGE',
			message: { text, chatId }
		});
		console.log('[SW Message] Message sent through service worker');
	} catch (error) {
		console.error('[SW Message] Error sending message through service worker:', error);
	}
}

/**
 * Create custom subscription configuration
 */
export function createSubscriptionConfig(
	id: string,
	path: string,
	scope: 'gun' | 'user',
	notificationConfig: Partial<ServiceWorkerSubscriptionConfig['notification']>,
	dataProcessor?: string
): ServiceWorkerSubscriptionConfig {
	return {
		id,
		path,
		scope,
		notification: {
			title: 'New Update',
			body: 'You have a new update',
			icon: '/favicon.png',
			badge: '/favicon.png',
			...notificationConfig
		},
		dataProcessor
	};
}

// Main application class for Gun PWA notifications
class GunPWANotifications {
	private gun: GunInstance | null;
	private serviceWorker: ServiceWorker | null;

	constructor() {
		// Only initialize in browser environment
		if (typeof window === 'undefined') {
			this.gun = null;
			this.serviceWorker = null;
			return;
		}

		// Type assertion for Gun import - you'll need to ensure Gun is available globally
		this.gun = (window as any).Gun
			? (window as any).Gun(['https://your-gun-relay.herokuapp.com/gun'])
			: null;
		this.serviceWorker = null;
		this.init();
	}

	async init(): Promise<void> {
		// Early return if not in browser
		if (typeof window === 'undefined') {
			return;
		}

		// Request notification permission
		if ('Notification' in window && Notification.permission === 'default') {
			await Notification.requestPermission();
		}

		// Register service worker
		if ('serviceWorker' in navigator) {
			try {
				const registration = await navigator.serviceWorker.register('/service-worker.js');
				console.log('Service Worker registered:', registration);

				// Get active service worker
				this.serviceWorker = registration.active || registration.waiting || registration.installing;

				// Listen for service worker messages
				navigator.serviceWorker.addEventListener(
					'message',
					this.handleServiceWorkerMessage.bind(this)
				);

				// Set up background sync (note: limited browser support)
				if ('sync' in registration) {
					(registration as any).sync.register('gun-sync');
				}
			} catch (error) {
				console.error('Service Worker registration failed:', error);
			}
		}

		// Set up visibility change handling
		if (typeof document !== 'undefined') {
			document.addEventListener('visibilitychange', this.handleVisibilityChange.bind(this));
		}

		// Set up periodic sync when app is active
		this.setupPeriodicSync();
	}

	// Subscribe to notifications for specific GUN paths
	subscribeToNotifications(subscriptionType: string): void {
		if (this.serviceWorker) {
			this.serviceWorker.postMessage({
				type: 'SUBSCRIBE',
				subscription: subscriptionType
			});
		}
	}

	unsubscribeFromNotifications(subscriptionType: string): void {
		if (this.serviceWorker) {
			this.serviceWorker.postMessage({
				type: 'UNSUBSCRIBE',
				subscription: subscriptionType
			});
		}
	}

	handleServiceWorkerMessage(event: MessageEvent<ServiceWorkerMessage>): void {
		const { type, itemType, key } = event.data;

		if (type === 'OPEN_ITEM') {
			// Handle notification click - navigate to specific content
			if (itemType === 'chat') {
				this.openChat(key!);
			} else if (itemType === 'mention') {
				this.openMention(key!);
			}
		}
	}

	handleVisibilityChange(): void {
		if (typeof document === 'undefined') return;

		if (document.visibilityState === 'visible') {
			// App became visible - sync immediately
			if (this.serviceWorker) {
				this.serviceWorker.postMessage({ type: 'SYNC_NOW' });
			}
		}
	}

	// Workaround: Periodic sync while app is open
	setupPeriodicSync(): void {
		if (typeof document === 'undefined') return;

		// Sync every 30 seconds when app is active
		setInterval(() => {
			if (document.visibilityState === 'visible') {
				this.syncData();
			}
		}, 30000);
	}

	syncData(): void {
		// Force sync with GUN peers
		// This doesn't solve the background problem but helps when app is active
		if (this.gun?._ && this.gun._.opt && this.gun._.opt.peers) {
			this.gun._.opt.peers = this.gun._.opt.peers;
		}
	}

	// Example usage methods
	startChatNotifications(): void {
		if (!this.gun) return;

		this.subscribeToNotifications('chat');

		// Also listen in main thread for immediate updates
		this.gun
			.get('chat')
			.map()
			.on((data: any, key: string) => {
				if (typeof document !== 'undefined' && document.visibilityState === 'visible') {
					// Handle real-time updates when app is open
					this.displayMessage(data);
				}
			});
	}

	startMentionNotifications(userId: string): void {
		if (!this.gun) return;

		this.subscribeToNotifications('mentions');

		this.gun
			.get('mentions')
			.get(userId)
			.map()
			.on((data: any, key: string) => {
				if (typeof document !== 'undefined' && document.visibilityState === 'visible') {
					this.displayMention(data);
				}
			});
	}

	openChat(key: string): void {
		if (typeof window === 'undefined') return;
		// Navigate to specific chat
		window.location.hash = `#chat/${key}`;
	}

	openMention(key: string): void {
		if (typeof window === 'undefined') return;
		// Navigate to specific mention
		window.location.hash = `#mention/${key}`;
	}

	displayMessage(data: any): void {
		// Update UI with new message
		console.log('New message:', data);
	}

	displayMention(data: any): void {
		// Update UI with new mention
		console.log('New mention:', data);
	}
}

// Initialize the app only in browser environment
let gunPWA: GunPWANotifications | null = null;

if (typeof window !== 'undefined') {
	gunPWA = new GunPWANotifications();

	// Example usage
	document.addEventListener('DOMContentLoaded', () => {
		if (gunPWA) {
			// Start listening for chat notifications
			gunPWA.startChatNotifications();

			// Start listening for mentions
			gunPWA.startMentionNotifications('current-user-id');
		}
	});

	// Handle URL parameters for notification clicks
	const urlParams = new URLSearchParams(window.location.search);
	if (urlParams.get('open')) {
		const type = urlParams.get('open');
		const key = urlParams.get('key');

		if (type === 'chat' && key && gunPWA) {
			gunPWA.openChat(key);
		} else if (type === 'mention' && key && gunPWA) {
			gunPWA.openMention(key);
		}
	}
}

// Export the class and instance
export { GunPWANotifications as P2PNotificationManager };
export { gunPWA };
