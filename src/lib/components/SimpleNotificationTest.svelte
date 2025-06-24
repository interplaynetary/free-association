<script lang="ts">
	import { onMount } from 'svelte';
	import { browser } from '$app/environment';
	import { base } from '$app/paths';

	let status = $state('Checking...');
	let swRegistration: ServiceWorkerRegistration | null = $state(null);

	async function testNotification() {
		status = 'Testing service worker notification...';

		try {
			// Request permission first
			if (Notification.permission !== 'granted') {
				const permission = await Notification.requestPermission();
				if (permission !== 'granted') {
					status = 'Permission denied';
					return;
				}
			}

			// Get service worker registration the proper way
			if (!swRegistration && 'serviceWorker' in navigator) {
				swRegistration = await navigator.serviceWorker.ready;
			}

			if (swRegistration) {
				await swRegistration.showNotification('Service Worker Test from Playnet', {
					body: 'This is a SvelteKit service worker notification',
					icon: `${base}/favicon.png`,
					tag: `sw-test-${Date.now()}` // Unique tag for each notification
				});
				status = 'Service worker notification sent!';
			} else {
				status = 'No service worker found';
			}
		} catch (error) {
			status = `SW Error: ${error}`;
			console.error('Service worker notification test failed:', error);
		}
	}

	async function testDirectNotification() {
		status = 'Testing direct notification...';

		try {
			// Request permission first
			if (Notification.permission !== 'granted') {
				const permission = await Notification.requestPermission();
				if (permission !== 'granted') {
					status = 'Permission denied';
					return;
				}
			}

			// Try direct notification (no service worker needed)
			new Notification('Direct Test from Playnet', {
				body: 'This is a direct notification without service worker',
				icon: `${base}/favicon.png`,
				tag: `direct-test-${Date.now()}` // Unique tag for each notification
			});

			status = 'Direct notification sent! (No service worker needed)';
		} catch (error) {
			status = `Direct error: ${error}`;
			console.error('Direct notification failed:', error);
		}
	}

	onMount(async () => {
		if (!browser) {
			status = 'Not in browser';
			return;
		}

		status = 'Checking SvelteKit service worker...';

		// Give time for SW to register (SvelteKit handles this automatically)
		setTimeout(async () => {
			try {
				if ('serviceWorker' in navigator) {
					const registration = await navigator.serviceWorker.ready;
					swRegistration = registration;
					status = 'SvelteKit service worker ready for notifications!';
				} else {
					status = 'Service Worker not supported';
				}
			} catch (error) {
				status = `SW Error: ${error}`;
			}
		}, 1000);
	});
</script>

{#if import.meta.env.DEV}
	<div class="test-container">
		<h3>SvelteKit Service Worker Debug</h3>
		<p>Status: {status}</p>
		<p class="debug">
			Service Worker supported: {'serviceWorker' in navigator ? 'Yes' : 'No'}<br />
			Notifications supported: {'Notification' in window ? 'Yes' : 'No'}<br />
			Current permission: {browser && 'Notification' in window
				? Notification.permission
				: 'Unknown'}<br />
			SW Registration: {swRegistration ? 'Ready' : 'Not ready'}
		</p>
		<div class="buttons">
			<button onclick={testNotification}>Test SW Notification</button>
			<button onclick={testDirectNotification}>Test Direct Notification</button>
		</div>
	</div>
{/if}

<style>
	.test-container {
		border: 1px solid #ccc;
		padding: 1rem;
		margin: 1rem 0;
		border-radius: 4px;
		background: #f9f9f9;
	}

	h3 {
		margin: 0 0 0.5rem 0;
		color: #333;
	}

	.buttons {
		margin-top: 1rem;
	}

	button {
		background: #007bff;
		color: white;
		border: none;
		padding: 0.5rem 1rem;
		border-radius: 4px;
		cursor: pointer;
		margin: 0.25rem 0.5rem 0.25rem 0;
	}

	button:hover {
		background: #0056b3;
	}

	.debug {
		font-size: 0.8em;
		color: #666;
		margin-top: 1rem;
		border-top: 1px solid #ddd;
		padding-top: 0.5rem;
	}
</style>
