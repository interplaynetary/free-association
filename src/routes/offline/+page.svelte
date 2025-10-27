<script lang="ts">
	import { onMount } from 'svelte';
	
	let isOnline = $state(false);
	
	onMount(() => {
		isOnline = navigator.onLine;
		
		const handleOnline = () => {
			isOnline = true;
			// Reload the page when back online
			setTimeout(() => {
				window.location.reload();
			}, 1000);
		};
		
		const handleOffline = () => {
			isOnline = false;
		};
		
		window.addEventListener('online', handleOnline);
		window.addEventListener('offline', handleOffline);
		
		return () => {
			window.removeEventListener('online', handleOnline);
			window.removeEventListener('offline', handleOffline);
		};
	});
</script>

<svelte:head>
	<title>Offline - Playnet</title>
</svelte:head>

<div class="flex min-h-screen items-center justify-center bg-gray-50 px-4">
	<div class="max-w-md text-center">
		{#if isOnline}
			<div class="mb-6">
				<svg class="mx-auto h-16 w-16 text-green-500" fill="none" viewBox="0 0 24 24" stroke="currentColor">
					<path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M9 12l2 2 4-4m6 2a9 9 0 11-18 0 9 9 0 0118 0z" />
				</svg>
			</div>
			<h1 class="mb-4 text-3xl font-bold text-gray-900">Back Online!</h1>
			<p class="mb-6 text-gray-600">Your connection has been restored. Reloading...</p>
		{:else}
			<div class="mb-6">
				<svg class="mx-auto h-16 w-16 text-gray-400" fill="none" viewBox="0 0 24 24" stroke="currentColor">
					<path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M18.364 5.636a9 9 0 010 12.728m0 0l-2.829-2.829m2.829 2.829L21 21M15.536 8.464a5 5 0 010 7.072m0 0l-2.829-2.829m-4.243 2.829a4.978 4.978 0 01-1.414-2.83m-1.414 5.658a9 9 0 01-2.167-9.238m7.824 2.167a1 1 0 111.414 1.414m-1.414-1.414L3 3m8.293 8.293l1.414 1.414" />
				</svg>
			</div>
			<h1 class="mb-4 text-3xl font-bold text-gray-900">You're Offline</h1>
			<p class="mb-6 text-gray-600">
				It looks like you've lost your internet connection. Some features may be limited until you're back online.
			</p>
			<div class="rounded-lg bg-blue-50 p-4">
				<p class="text-sm text-blue-800">
					<strong>Tip:</strong> You can still access previously visited pages and cached content.
				</p>
			</div>
			<button
				onclick={() => window.location.reload()}
				class="mt-6 rounded-lg bg-gray-900 px-6 py-3 text-white hover:bg-gray-800 focus:outline-none focus:ring-2 focus:ring-gray-900 focus:ring-offset-2"
			>
				Try Again
			</button>
		{/if}
	</div>
</div>

