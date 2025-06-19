export class P2PNotificationManager {
	registration: ServiceWorkerRegistration | null = null;
	activeNotifications = new Map<string, { title: string; options: any }>();

	constructor(registration: ServiceWorkerRegistration | null = null) {
		this.registration = registration;
	}

	async showNotification(title: string, options: any = {}) {
		// Check if notifications are supported and permitted
		if (!('Notification' in window)) {
			console.warn('Notifications not supported');
			return;
		}

		if (Notification.permission !== 'granted') {
			console.warn('Notifications not permitted');
			return;
		}

		// Use browser notification API directly since zero-config doesn't support custom SW messages
		try {
			const notification = new Notification(title, {
				body: options.body || '',
				icon: options.icon || '/favicon.png',
				badge: options.badge || '/favicon.png',
				tag: options.tag || 'default',
				data: options.data || {},
				requireInteraction: options.requireInteraction || false,
				silent: options.silent || false,
			});

			// Handle notification click
			notification.onclick = () => {
				window.focus();

				const data = options.data || {};
				if (data.type === 'message' && data.peerId) {
					window.location.href = `/rooms?peerId=${data.peerId}`;
				} else if (data.type === 'capacity' && data.peerId) {
					window.location.href = `/rooms?peerId=${data.peerId}`;
				} else if (data.type === 'room-invite' && data.roomName) {
					window.location.href = `/rooms?room=${encodeURIComponent(data.roomName)}`;
				}

				notification.close();
			};

			// Track notification
			if (options.tag) {
				this.activeNotifications.set(options.tag, { title, options });
			}
		} catch (error) {
			console.error('Failed to show notification:', error);
		}
	}

	// P2P specific notification methods
	onPeerConnected(peerId: string, peerName: string | null = null) {
		this.showNotification(`Peer Connected`, {
			body: `${peerName || peerId} joined the session`,
			tag: `peer-connected-${peerId}`,
			icon: '/favicon.png',
			data: { type: 'peer-connected', peerId }
		});
	}

	onPeerMessage(
		message: { id: string; text: string; sender?: string },
		peerId: string,
		peerName: string | null = null
	) {
		this.showNotification(`New Message`, {
			body: `${peerName || peerId}: ${message.text}`,
			tag: `message-${peerId}`,
			icon: '/favicon.png',
			data: {
				type: 'message',
				peerId,
				messageId: message.id,
				peerName
			}
		});
	}

	onFileReceived(filename: string, peerId: string, peerName: string | null = null) {
		this.showNotification(`File Received`, {
			body: `${peerName || peerId} sent: ${filename}`,
			tag: `file-${peerId}-${Date.now()}`,
			icon: '/favicon.png',
			data: {
				type: 'file',
				peerId,
				filename,
				peerName
			}
		});
	}

	onConnectionLost(peerId: string, peerName: string | null = null) {
		this.showNotification(`Connection Lost`, {
			body: `Lost connection to ${peerName || peerId}`,
			tag: `connection-lost-${peerId}`,
			icon: '/favicon.png',
			data: { type: 'connection-lost', peerId }
		});
	}

	onCapacityOffered(capacity: string, peerId: string, peerName: string | null = null) {
		this.showNotification(`New Capacity Offered`, {
			body: `${peerName || peerId} is offering: ${capacity}`,
			tag: `capacity-${peerId}-${Date.now()}`,
			icon: '/favicon.png',
			data: {
				type: 'capacity',
				peerId,
				capacity,
				peerName
			}
		});
	}

	onRoomInvite(roomName: string, peerId: string, peerName: string | null = null) {
		this.showNotification(`Room Invitation`, {
			body: `${peerName || peerId} invited you to "${roomName}"`,
			tag: `room-invite-${peerId}-${Date.now()}`,
			icon: '/favicon.png',
			data: {
				type: 'room-invite',
				peerId,
				roomName,
				peerName
			}
		});
	}

	// Clear specific notification (limited with browser API)
	async clearNotification(tag: string) {
		// Browser API doesn't support clearing specific notifications by tag
		// This is a limitation of the zero-config approach
		this.activeNotifications.delete(tag);
	}

	// Clear all notifications (also limited)
	async clearAllNotifications() {
		// Browser API doesn't support clearing all notifications
		// This is a limitation of the zero-config approach
		this.activeNotifications.clear();
	}

	// Request notification permission
	async requestPermission(): Promise<NotificationPermission> {
		if (!('Notification' in window)) {
			console.warn('Notifications not supported');
			return 'denied';
		}

		if (Notification.permission === 'default') {
			return await Notification.requestPermission();
		}

		return Notification.permission;
	}
}
