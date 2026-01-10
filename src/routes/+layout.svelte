<script lang="ts">
	import Header from '$lib/components/Header.svelte';
	import ToolBar from '$lib/components/ToolBar.svelte';
	import DraggedNode from '$lib/components/DraggedNode.svelte';
	import { Toaster } from 'svelte-french-toast';
	import '../app.css';
	import type { LayoutProps } from './$types';
	import { globalState, initializeGlobalState } from '$lib/global.svelte';
	import { onMount, onDestroy } from 'svelte';
	import { browser } from '$app/environment';
	import { initializeProtocol, cleanupProtocol } from '$lib/protocol/startup';
	import { pwaInfo } from 'virtual:pwa-info';
	// V5: Store initialization and auto-composition happen in holster.svelte.ts after authentication

	// Layout props
	let { children }: LayoutProps = $props();

	// PWA web manifest link
	let webManifestLink = $derived(pwaInfo ? pwaInfo.webManifest.linkTag : '');

    console.log('[TRACE] src/routes/+layout.svelte: <module scope>');

	// Initialize services dynamically on mount to avoid initialization order issues
	onMount(async () => {
        console.log('[TRACE] src/routes/+layout.svelte: onMount');
		// Dynamically import and initialize services after component mounts
		// This ensures all dependencies (like globalState) are fully initialized
		if (browser) {
			await import('$lib/services');
			
			// Initialize protocol layer (Auth, Stores, Capacity)
			initializeProtocol();

			// Initialize global state subscriptions after SvelteKit routing context is ready
			// This prevents TDZ errors on iOS Safari when accessing the page store
			initializeGlobalState();
		}

		// Register PWA service worker with auto-update
		if (browser && pwaInfo) {
			const { registerSW } = await import('virtual:pwa-register');
			registerSW({
				immediate: true,
				onRegistered(r: ServiceWorkerRegistration | undefined) {
					console.log('SW Registered:', r);
				},
				onRegisterError(error: Error) {
					console.log('SW registration error', error);
				}
			});
		}

		// Request notification permission
		if (browser && 'Notification' in window && Notification.permission === 'default') {
			Notification.requestPermission().then((permission) => {
				console.log('Notification permission:', permission);
			});
		}
		console.log('[TRACE] [EXIT] src/routes/+layout.svelte: onMount');
	});

	onDestroy(() => {
		if (browser) {
			cleanupProtocol();
		}
	});
</script>

<svelte:head>
	{@html webManifestLink}
</svelte:head>

<main>
	<div class="app-header">
		<Header />
	</div>
	<div class="app-content">
		{@render children()}
	</div>
	<div class="app-footer">
		<ToolBar />
	</div>
</main>

<!-- Toast notification component - positioned at top center -->
<Toaster />

<!-- DraggedNode component that appears on top of everything -->
<DraggedNode
	show={globalState.isDragging}
	nodeName={globalState.draggedNodeName}
	nodeColor={globalState.draggedNodeColor}
	x={globalState.dragX}
	y={globalState.dragY}
/>

<style>
	main {
		display: flex;
		flex-direction: column;
		/* Enhanced fallback chain for maximum browser support */
		height: 100vh; /* Standard fallback */
		height: -webkit-fill-available; /* iOS Safari fallback */
		height: 100dvh; /* Modern dynamic viewport height */
		width: 100vw; /* Standard fallback */
		width: 100dvw; /* Modern dynamic viewport width */

		/* Enhanced safe area support with vendor prefixes */
		padding-top: constant(safe-area-inset-top); /* iOS 11.0-11.2 */
		padding-top: env(safe-area-inset-top); /* iOS 11.2+ */
		padding-bottom: constant(safe-area-inset-bottom);
		padding-bottom: env(safe-area-inset-bottom);
		padding-left: constant(safe-area-inset-left);
		padding-left: env(safe-area-inset-left);
		padding-right: constant(safe-area-inset-right);
		padding-right: env(safe-area-inset-right);

		/* Enhanced box-sizing support */
		-webkit-box-sizing: border-box;
		-moz-box-sizing: border-box;
		box-sizing: border-box;

		/* Allow normal overflow - individual pages control their scrolling */
		overflow: visible;

		/* Enhanced mobile support */
		-webkit-overflow-scrolling: touch; /* Smooth scrolling on iOS */
	}

	.app-header {
		position: relative;
		width: 100%;
		z-index: 100;
		background: white;
		min-height: 60px;
		display: flex;
		flex-direction: column;
		flex-shrink: 0;
	}

	.app-content {
		flex: 1;
		width: 100%;
		overflow-y: auto;
		/* overflow-x: hidden; */ /* Removed - let pages control their own overflow */
		position: relative;
		z-index: 1;
		/* Remove fixed height calculation, let flexbox handle it */
		min-height: 0; /* Important for flexbox overflow */
		/* Enhanced mobile scrolling */
		-webkit-overflow-scrolling: touch;
		transition: padding-top 0.3s ease-out;
	}

	.app-footer {
		position: relative;
		width: 100%;
		z-index: 100;
		background: white;
		flex-shrink: 0;
	}


</style>
