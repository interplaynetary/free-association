<script lang="ts">
	import Header from '$lib/components/Header.svelte';
	import ToolBar from '$lib/components/ToolBar.svelte';
	import DraggedNode from '$lib/components/DraggedNode.svelte';
	import { Toaster } from 'svelte-french-toast';
	import '../app.css';
	import type { LayoutProps } from './$types';
	import { globalState } from '$lib/global.svelte';
	import { onMount } from 'svelte';
	import { browser } from '$app/environment';
	import { base } from '$app/paths';
	import { loading } from '$lib/translations';

	// Initialize global services (auto-initializes viewport and navigation handling)
	import '$lib/services';

	// Layout props
	let { children }: LayoutProps = $props();

	// Handle notification permission request (layout-appropriate functionality)
	onMount(() => {
		// Request notification permission if supported
		if (browser && 'Notification' in window && Notification.permission === 'default') {
			Notification.requestPermission().then((permission) => {
				console.log('Notification permission:', permission);
			});
		}
	});
</script>

<svelte:head>
	<link rel="manifest" href="{base}/manifest.json" />
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
