<script lang="ts" module>
	// Module-level imports - executed once
	let pwaInfoPromise: Promise<any>;
	try {
		pwaInfoPromise = import('virtual:pwa-info')
			.then(m => {
				console.log('[LAYOUT] PWA info loaded successfully');
				return m.pwaInfo;
			})
			.catch((err) => {
				console.log('[LAYOUT] PWA info not available (expected in production):', err.message);
				return undefined;
			});
	} catch (err) {
		console.log('[LAYOUT] PWA module import failed:', err);
		pwaInfoPromise = Promise.resolve(undefined);
	}
</script>

<script lang="ts">
	import Header from '$lib/components/Header.svelte';
	import ToolBar from '$lib/components/ToolBar.svelte';
	import DraggedNode from '$lib/components/DraggedNode.svelte';
	import { Toaster } from 'svelte-french-toast';
	import '../app.css';
	import type { LayoutProps } from './$types';
	import { globalState, initializeGlobalState } from '$lib/global.svelte';
	import { onMount } from 'svelte';
	import { browser } from '$app/environment';
	import { loading } from '$lib/translations';
	// V5: Store initialization and auto-composition happen in holster.svelte.ts after authentication
	
	// Get PWA info from module promise
	let pwaInfo: any = $state(undefined);
	pwaInfoPromise.then(info => pwaInfo = info);

	// Layout props
	let { children }: LayoutProps = $props();

	// PWA manifest link tag (injected dynamically by @vite-pwa/sveltekit)
	// See: https://vite-pwa-org.netlify.app/frameworks/sveltekit.html
	const webManifestLink = $derived(pwaInfo ? pwaInfo.webManifest.linkTag : '');

	// Request notification permission on mount
	// Note: SW registration is handled by ReloadPrompt component
	onMount(() => {
		if (!browser) return;
		
		// CRITICAL: Initialize globalState FIRST before anything else (fixes iOS 500 error)
		// Must happen before services and before any $derived reactives fire
		console.log('[LAYOUT] Initializing globalState...');
		initializeGlobalState();
		
		// Initialize global services after DOM is ready
		console.log('[LAYOUT] Initializing services...');
		import('$lib/services').then(() => {
			console.log('[LAYOUT] Services initialized successfully');
		}).catch((err) => {
			console.error('[LAYOUT] Failed to initialize services:', err);
		});
		
		// Request notification permission
		if ('Notification' in window && Notification.permission === 'default') {
			Notification.requestPermission().then((permission) => {
				console.log('Notification permission:', permission);
			});
		}
	});
</script>

<svelte:head>
	{@html webManifestLink}
</svelte:head>

<main>
	{#if $loading}
		<div class="loading-translations">
			<p>Loading translations...</p>
		</div>
	{:else}
		<div class="app-header">
			<Header />
		</div>
		<div class="app-content">
			{@render children()}
		</div>
		<div class="app-footer">
			<ToolBar />
		</div>
	{/if}
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

<!-- PWA Reload Prompt - dynamically imported only when PWA is active -->
{#if browser && pwaInfo}
	{#await import('$lib/ReloadPrompt.svelte') then { default: ReloadPrompt }}
		<ReloadPrompt />
	{/await}
{/if}

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
		overflow-x: hidden; /* Prevent horizontal scrolling */
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

	.loading-translations {
		display: flex;
		align-items: center;
		justify-content: center;
		width: 100%;
		height: 100%;
		color: #666;
	}

	/* Fullscreen styling is now handled by FullScreenControl */
</style>
