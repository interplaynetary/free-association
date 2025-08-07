import { openDB, type IDBPDatabase, type DBSchema } from 'idb';

// Database schema interface
interface NotificationDB extends DBSchema {
	queue: {
		key: number;
		value: NotificationData;
		indexes: {
			'by-timestamp': number;
			'by-tag': string;
		};
	};
}

// TypeScript interfaces
export interface NotificationData {
	id?: number;
	title: string;
	body: string;
	tag: string;
	source: string;
	timestamp: number;
	processed?: boolean;
	data?: Record<string, any>;
	icon?: string;
	badge?: string;
	requireInteraction?: boolean;
	silent?: boolean;
}

export interface NotificationConfig {
	dbName: string;
	dbVersion: number;
	defaultIcon: string;
	defaultBadge: string;
}

// Default configuration
export const defaultConfig: NotificationConfig = {
	dbName: 'playnet-notifications',
	dbVersion: 1,
	defaultIcon: './favicon.png',
	defaultBadge: './favicon.png'
};

// Generic NotificationManager class
export class NotificationManager {
	private db: IDBPDatabase<NotificationDB> | null = null;
	private config: NotificationConfig;

	constructor(config: NotificationConfig = defaultConfig) {
		this.config = config;
		this.init();
	}

	private async init(): Promise<void> {
		console.log('[SW NotificationManager] Initializing...');
		try {
			await this.setupDatabase();
			console.log('[SW NotificationManager] Database setup complete');
			console.log('[SW NotificationManager] Notification permission:', Notification.permission);

			// Check if we're in a service worker context
			if (typeof self !== 'undefined' && 'registration' in self) {
				console.log('[SW NotificationManager] Service worker registration available: true');
			} else {
				console.log('[SW NotificationManager] Service worker registration available: false');
			}

			console.log('[SW NotificationManager] Initialized successfully');
		} catch (error) {
			console.error('[SW NotificationManager] Initialization failed:', error);
		}
	}

	private async setupDatabase(): Promise<void> {
		console.log('[SW NotificationManager] Setting up database...');

		this.db = await openDB<NotificationDB>(this.config.dbName, this.config.dbVersion, {
			upgrade(db) {
				console.log('[SW NotificationManager] Database upgrade needed');

				if (!db.objectStoreNames.contains('queue')) {
					console.log('[SW NotificationManager] Creating queue object store');
					const store = db.createObjectStore('queue', {
						keyPath: 'id',
						autoIncrement: true
					});
					store.createIndex('by-timestamp', 'timestamp', { unique: false });
					store.createIndex('by-tag', 'tag', { unique: false });
					console.log('[SW NotificationManager] Queue object store created');
				}
			}
		});

		console.log('[SW NotificationManager] Database opened successfully');
	}

	// Public method to queue notifications from external sources
	async queueNotification(notification: NotificationData): Promise<void> {
		console.log('[SW NotificationManager] queueNotification called with:', notification);

		if (!this.db) {
			console.error('[SW NotificationManager] Database not initialized!');
			return;
		}

		try {
			const notificationWithProcessed = { ...notification, processed: false };
			console.log('[SW NotificationManager] Adding to IndexedDB:', notificationWithProcessed);

			await this.db.add('queue', notificationWithProcessed);
			console.log('[SW NotificationManager] Successfully added to queue');

			await this.processQueue();
		} catch (error) {
			console.error('[SW NotificationManager] Error queuing notification:', error);
		}
	}

	private async processQueue(): Promise<void> {
		console.log('[SW NotificationManager] Processing queue...');

		if (!this.db) {
			console.error('[SW NotificationManager] Database not initialized in processQueue!');
			return;
		}

		try {
			// Step 1: Read all notifications
			const allNotifications = await this.db.getAll('queue');
			console.log('[SW NotificationManager] All notifications in queue:', allNotifications.length);
			console.log(
				'[SW NotificationManager] Queue details:',
				allNotifications.map((n) => ({
					id: n.id,
					tag: n.tag,
					processed: n.processed,
					timestamp: n.timestamp
				}))
			);

			const unprocessed = allNotifications.filter((n) => !n.processed);
			console.log('[SW NotificationManager] Unprocessed notifications:', unprocessed.length);

			if (unprocessed.length === 0) {
				console.log('[SW NotificationManager] No unprocessed notifications');
				// Clean up old processed notifications (older than 1 hour)
				await this.cleanupOldNotifications();
				return;
			}

			// Step 2: Process notifications (async operations)
			for (const notification of unprocessed) {
				console.log('[SW NotificationManager] Processing notification:', notification.tag);
				await this.displayNotification(notification);

				// Step 3: Mark as processed and update
				notification.processed = true;
				await this.db.put('queue', notification);
				console.log('[SW NotificationManager] Marked notification as processed:', notification.tag);
			}

			console.log('[SW NotificationManager] All notifications processed successfully');

			// Clean up old processed notifications after processing
			await this.cleanupOldNotifications();
		} catch (error) {
			console.error('[SW NotificationManager] Error in processQueue:', error);
		}
	}

	private async displayNotification(data: NotificationData): Promise<void> {
		console.log('[SW NotificationManager] Displaying notification:', data);

		try {
			const options: NotificationOptions = {
				body: data.body,
				icon: data.icon || this.config.defaultIcon,
				badge: data.badge || this.config.defaultBadge,
				tag: data.tag,
				data: data.data,
				requireInteraction: data.requireInteraction || false,
				silent: data.silent || false
			};

			console.log('[SW NotificationManager] Notification options:', options);

			// Check if we're in a service worker context
			if (typeof self !== 'undefined' && 'registration' in self) {
				// Service worker context
				const registration = (self as any).registration as ServiceWorkerRegistration;
				await registration.showNotification(data.title, options);
				console.log('[SW NotificationManager] Notification shown via service worker');
			} else if (typeof window !== 'undefined' && 'Notification' in window) {
				// Main thread context
				new Notification(data.title, options);
				console.log('[SW NotificationManager] Notification shown via main thread');
			} else {
				console.error('[SW NotificationManager] No notification API available');
			}
		} catch (error) {
			console.error('[SW NotificationManager] Error displaying notification:', error);
		}
	}

	async clearNotification(tag: string): Promise<void> {
		console.log('[SW NotificationManager] Clearing notification with tag:', tag);
		try {
			// Check if we're in a service worker context
			if (typeof self !== 'undefined' && 'registration' in self) {
				const registration = (self as any).registration as ServiceWorkerRegistration;
				const notifications = await registration.getNotifications({ tag });
				notifications.forEach((notification: Notification) => notification.close());
				console.log('[SW NotificationManager] Cleared notifications with tag:', tag);
			}
		} catch (error) {
			console.error('[SW NotificationManager] Error clearing notification:', error);
		}
	}

	async clearAllNotifications(): Promise<void> {
		console.log('[SW NotificationManager] Clearing all notifications');
		try {
			// Check if we're in a service worker context
			if (typeof self !== 'undefined' && 'registration' in self) {
				const registration = (self as any).registration as ServiceWorkerRegistration;
				const notifications = await registration.getNotifications();
				notifications.forEach((notification: Notification) => notification.close());
				console.log('[SW NotificationManager] Cleared all notifications');
			}
		} catch (error) {
			console.error('[SW NotificationManager] Error clearing all notifications:', error);
		}
	}

	// Helper method to get notification count
	async getNotificationCount(): Promise<number> {
		if (!this.db) return 0;
		return await this.db.count('queue');
	}

	// Helper method to clear processed notifications from storage
	async clearProcessedNotifications(): Promise<void> {
		if (!this.db) return;

		try {
			const tx = this.db.transaction('queue', 'readwrite');
			const store = tx.objectStore('queue');

			// Use async iterator to find and delete processed notifications
			for await (const cursor of store) {
				if (cursor.value.processed) {
					await cursor.delete();
				}
			}

			console.log('[SW NotificationManager] Cleared processed notifications from storage');
		} catch (error) {
			console.error('[SW NotificationManager] Error clearing processed notifications:', error);
		}
	}

	// Helper method to clean up old processed notifications
	private async cleanupOldNotifications(): Promise<void> {
		if (!this.db) return;

		try {
			const oneHourAgo = Date.now() - 60 * 60 * 1000; // 1 hour ago
			const tx = this.db.transaction('queue', 'readwrite');
			const store = tx.objectStore('queue');

			let deletedCount = 0;
			// Use async iterator to find and delete old processed notifications
			for await (const cursor of store) {
				if (cursor.value.processed && cursor.value.timestamp < oneHourAgo) {
					await cursor.delete();
					deletedCount++;
				}
			}

			if (deletedCount > 0) {
				console.log(
					`[SW NotificationManager] Cleaned up ${deletedCount} old processed notifications`
				);
			}
		} catch (error) {
			console.error('[SW NotificationManager] Error cleaning up old notifications:', error);
		}
	}
}
