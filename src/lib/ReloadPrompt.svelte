<script lang="ts">
	import { onMount } from 'svelte';
	import { writable } from 'svelte/store';
	
	const needRefresh = writable(false);
	const offlineReady = writable(false);
	let registration: ServiceWorkerRegistration | undefined;

	onMount(() => {
		console.log('[PWA] Service worker registration disabled');
		// Service worker registration disabled
		// if ('serviceWorker' in navigator) {
		// 	navigator.serviceWorker.register('/service-worker.js').then((reg) => {
		// 		registration = reg;
		// 		console.log('[PWA] SW Registered:', reg);
		// 		
		// 		// Check for updates
		// 		reg.addEventListener('updatefound', () => {
		// 			const newWorker = reg.installing;
		// 			if (newWorker) {
		// 				newWorker.addEventListener('statechange', () => {
		// 					if (newWorker.state === 'installed' && navigator.serviceWorker.controller) {
		// 						needRefresh.set(true);
		// 					}
		// 				});
		// 			}
		// 		});
		// 		
		// 		// Show offline ready once
		// 		if (reg.active) {
		// 			setTimeout(() => offlineReady.set(true), 1000);
		// 		}
		// 	}).catch((error) => {
		// 		console.log('[PWA] SW registration error:', error);
		// 	});
		// }
	});

	const updateServiceWorker = async () => {
		if (registration && registration.waiting) {
			registration.waiting.postMessage({ type: 'SKIP_WAITING' });
			navigator.serviceWorker.addEventListener('controllerchange', () => {
				window.location.reload();
			});
		}
	};

	const close = () => {
		offlineReady.set(false);
		needRefresh.set(false);
	};

	$: toast = $offlineReady || $needRefresh;
</script>

{#if toast}
	<div class="pwa-toast" role="alert">
		<div class="message">
			{#if $offlineReady}
				<span>App ready to work offline</span>
			{:else}
				<span>New content available, click on reload button to update.</span>
			{/if}
		</div>
		{#if $needRefresh}
			<button on:click={updateServiceWorker}> Reload </button>
		{/if}
		<button on:click={close}> Close </button>
	</div>
{/if}

<style>
	.pwa-toast {
		position: fixed;
		right: 0;
		bottom: 0;
		margin: 16px;
		padding: 12px;
		border: 1px solid #8885;
		border-radius: 4px;
		z-index: 2000;
		text-align: left;
		box-shadow: 3px 4px 5px 0 #8885;
		background-color: white;
	}
	.pwa-toast .message {
		margin-bottom: 8px;
	}
	.pwa-toast button {
		border: 1px solid #8885;
		outline: none;
		margin-right: 5px;
		border-radius: 2px;
		padding: 3px 10px;
		cursor: pointer;
	}
	.pwa-toast button:hover {
		background-color: #f3f4f6;
	}
</style>

