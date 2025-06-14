import { openDB, type DBSchema, type IDBPDatabase } from 'idb';
import Push from 'push.js';

interface NotificationData {
	id: string;
	title: string;
	body: string;
	scheduledAt: Date;
	delivered: number; // 0 = false, 1 = true
	recurring?: {
		interval: 'daily' | 'weekly' | 'monthly';
		days?: number[]; // 0-6, Sunday = 0
	};
	tags?: string[];
	createdAt: Date;
}

interface NotificationDB extends DBSchema {
	notifications: {
		key: string;
		value: NotificationData;
		indexes: { 'by-scheduled': string; 'by-delivered': number };
	};
}

class NotificationService {
	private db: IDBPDatabase<NotificationDB> | null = null;
	private checkInterval: number | null = null;

	async init() {
		this.db = await openDB<NotificationDB>('notifications-db', 1, {
			upgrade(db) {
				const store = db.createObjectStore('notifications', {
					keyPath: 'id'
				});
				store.createIndex('by-scheduled', 'scheduledAt');
				store.createIndex('by-delivered', 'delivered');
			}
		});

		// Request notification permission
		await this.requestPermission();

		// Start checking for due notifications
		this.startNotificationCheck();
	}

	async requestPermission(): Promise<boolean> {
		if (!('Notification' in window)) {
			console.warn('This browser does not support notifications');
			return false;
		}

		if (Notification.permission === 'granted') {
			return true;
		}

		if (Notification.permission === 'denied') {
			return false;
		}

		const permission = await Notification.requestPermission();
		return permission === 'granted';
	}

	async scheduleNotification(
		title: string,
		body: string,
		scheduledAt: Date,
		options?: {
			recurring?: NotificationData['recurring'];
			tags?: string[];
		}
	): Promise<string> {
		if (!this.db) throw new Error('Database not initialized');

		const id = crypto.randomUUID();
		const notification: NotificationData = {
			id,
			title,
			body,
			scheduledAt,
			delivered: 0,
			recurring: options?.recurring,
			tags: options?.tags || [],
			createdAt: new Date()
		};

		await this.db.add('notifications', notification);
		return id;
	}

	async getScheduledNotifications(): Promise<NotificationData[]> {
		if (!this.db) return [];
		return this.db.getAll('notifications');
	}

	async getUndeliveredNotifications(): Promise<NotificationData[]> {
		if (!this.db) return [];
		return this.db.getAllFromIndex('notifications', 'by-delivered', 0);
	}

	async deleteNotification(id: string): Promise<void> {
		if (!this.db) return;
		await this.db.delete('notifications', id);
	}

	async markAsDelivered(id: string): Promise<void> {
		if (!this.db) return;
		const notification = await this.db.get('notifications', id);
		if (notification) {
			notification.delivered = 1;
			await this.db.put('notifications', notification);
		}
	}

	private async checkDueNotifications() {
		const notifications = await this.getUndeliveredNotifications();
		const now = new Date();

		for (const notification of notifications) {
			if (notification.scheduledAt <= now) {
				await this.deliverNotification(notification);

				// Handle recurring notifications
				if (notification.recurring) {
					await this.scheduleRecurring(notification);
				}
			}
		}
	}

	private async deliverNotification(notification: NotificationData) {
		try {
			// Use push.js for cross-browser compatibility
			Push.create(notification.title, {
				body: notification.body,
				icon: '/favicon.png',
				timeout: 8000,
				onClick: function () {
					window.focus();
				}
			});

			await this.markAsDelivered(notification.id);
		} catch (error) {
			console.error('Failed to deliver notification:', error);
		}
	}

	private async scheduleRecurring(notification: NotificationData) {
		if (!notification.recurring) return;

		const nextDate = new Date(notification.scheduledAt);

		switch (notification.recurring.interval) {
			case 'daily':
				nextDate.setDate(nextDate.getDate() + 1);
				break;
			case 'weekly':
				nextDate.setDate(nextDate.getDate() + 7);
				break;
			case 'monthly':
				nextDate.setMonth(nextDate.getMonth() + 1);
				break;
		}

		await this.scheduleNotification(notification.title, notification.body, nextDate, {
			recurring: notification.recurring,
			tags: notification.tags
		});
	}

	private startNotificationCheck() {
		// Check every minute
		this.checkInterval = window.setInterval(() => {
			this.checkDueNotifications();
		}, 60000);
	}

	destroy() {
		if (this.checkInterval) {
			clearInterval(this.checkInterval);
			this.checkInterval = null;
		}
	}

	// Quick scheduling helpers
	async scheduleIn(title: string, body: string, minutes: number) {
		const scheduledAt = new Date();
		scheduledAt.setMinutes(scheduledAt.getMinutes() + minutes);
		return this.scheduleNotification(title, body, scheduledAt);
	}

	async scheduleDaily(title: string, body: string, hour: number, minute: number = 0) {
		const scheduledAt = new Date();
		scheduledAt.setHours(hour, minute, 0, 0);

		// If time has passed today, schedule for tomorrow
		if (scheduledAt <= new Date()) {
			scheduledAt.setDate(scheduledAt.getDate() + 1);
		}

		return this.scheduleNotification(title, body, scheduledAt, {
			recurring: { interval: 'daily' }
		});
	}
}

// Singleton instance
export const notificationService = new NotificationService();
