<script lang="ts">
	import { onMount } from 'svelte';
	import { browser } from '$app/environment';
	import { pwaInfo } from 'virtual:pwa-info';

	let notificationPermission: NotificationPermission = $state('default');
	let serviceWorkerRegistration: ServiceWorkerRegistration | null = $state(null);
	let swReady = $state(false);

	// Request notification permission
	async function requestNotificationPermission() {
		if (!browser || !('Notification' in window)) {
			console.log('Notifications not supported');
			return false;
		}

		try {
			const permission = await Notification.requestPermission();
			notificationPermission = permission;

			if (permission === 'granted') {
				console.log('Notification permission granted');
				return true;
			} else {
				console.log('Notification permission denied');
				return false;
			}
		} catch (error) {
			console.error('Error requesting notification permission:', error);
			return false;
		}
	}

	// Check current notification permission status
	function checkNotificationPermission() {
		if (browser && 'Notification' in window) {
			notificationPermission = Notification.permission;
		}
	}

	// Test notification function (for development) - zero-config approach
	async function sendTestNotification() {
		if (!swReady || !serviceWorkerRegistration) {
			console.log('Service Worker not ready yet');
			return;
		}

		if (notificationPermission !== 'granted') {
			const granted = await requestNotificationPermission();
			if (!granted) return;
		}

		try {
			await serviceWorkerRegistration.showNotification('Test from Playnet', {
				body: 'This is a test notification from the zero-config PWA',
				icon: '/favicon.png',
				badge: '/favicon.png',
				tag: 'test',
				data: {
					type: 'test',
					timestamp: Date.now()
				}
			});
			console.log('Test notification sent');
		} catch (error) {
			console.error('Failed to send test notification:', error);
		}
	}

	// Initialize on mount - zero-config approach
	onMount(async () => {
		checkNotificationPermission();

		// Only proceed if PWA is available
		if (!browser || !pwaInfo) {
			console.log('PWA not available');
			return;
		}

		// Wait for service worker to be ready
		setTimeout(async () => {
			try {
				if ('serviceWorker' in navigator) {
					const registration = await navigator.serviceWorker.ready;
					serviceWorkerRegistration = registration;
					swReady = true;
					console.log('Service Worker ready for notifications');

					// Auto-request notification permission if not already set
					if (notificationPermission === 'default') {
						await requestNotificationPermission();
					}
				}
			} catch (error) {
				console.error('Service Worker setup failed:', error);
			}
		}, 2000);
	});
</script>

<!-- Only show UI in development or if permission needs to be granted -->
{#if notificationPermission === 'default'}
	<div class="notification-prompt">
		<p>Enable notifications to get periodic reminders</p>
		<button onclick={requestNotificationPermission} class="permission-button">
			Enable Notifications
		</button>
	</div>
{:else if notificationPermission === 'granted'}
	<div class="notification-status">
		<p class="success">✓ Notifications enabled</p>
		<!-- Test button for development -->
		{#if import.meta.env.DEV}
			<button onclick={sendTestNotification} class="test-button" disabled={!swReady}>
				{swReady ? 'Send Test Notification' : 'SW Loading...'}
			</button>
			<p class="debug">SW Ready: {swReady}</p>
		{/if}
	</div>
{:else}
	<div class="notification-status">
		<p class="error">
			Notifications blocked. Enable in browser settings to receive periodic reminders.
		</p>
	</div>
{/if}

<style>
	.notification-prompt {
		background: #f3f4f6;
		border: 1px solid #d1d5db;
		border-radius: 8px;
		padding: 1rem;
		margin: 1rem 0;
	}

	.notification-status {
		padding: 0.5rem 0;
	}

	.permission-button {
		background: #3b82f6;
		color: white;
		border: none;
		border-radius: 6px;
		padding: 0.5rem 1rem;
		cursor: pointer;
		font-size: 0.875rem;
		transition: background-color 0.2s;
	}

	.permission-button:hover {
		background: #2563eb;
	}

	.test-button {
		background: #10b981;
		color: white;
		border: none;
		border-radius: 4px;
		padding: 0.25rem 0.75rem;
		cursor: pointer;
		font-size: 0.75rem;
		margin-left: 0.5rem;
		transition: background-color 0.2s;
	}

	.test-button:hover:not(:disabled) {
		background: #059669;
	}

	.test-button:disabled {
		background: #6b7280;
		cursor: not-allowed;
	}

	.success {
		color: #10b981;
		font-size: 0.875rem;
		margin: 0;
	}

	.error {
		color: #ef4444;
		font-size: 0.875rem;
		margin: 0;
	}

	.debug {
		font-size: 0.75rem;
		color: #6b7280;
		margin: 0.25rem 0 0 0;
	}
</style>
 