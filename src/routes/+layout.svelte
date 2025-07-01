<script lang="ts">
	import Header from '$lib/components/Header.svelte';
	import SimpleNotificationTest from '$lib/components/SimpleNotificationTest.svelte';
	import DraggedNode from '$lib/components/DraggedNode.svelte';
	import '../app.css';
	import type { LayoutProps } from './$types';
	import { globalState } from '$lib/global.svelte';
	import { onMount } from 'svelte';
	import { browser } from '$app/environment';
	import { base } from '$app/paths';
	import { Toaster } from 'svelte-french-toast';

	// Layout props
	let { children }: LayoutProps = $props();

	// Set up global keyboard event listener and reliable viewport handling
	onMount(() => {
		// SvelteKit will automatically register the service worker
		// We just need to check if it's working
		if (browser && 'serviceWorker' in navigator) {
			navigator.serviceWorker.ready
				.then((registration) => {
					console.log('Service worker registered successfully:', registration);

					// Request notification permission if not already granted
					if ('Notification' in window && Notification.permission === 'default') {
						return Notification.requestPermission();
					}
				})
				.catch((error) => {
					console.error('Service worker registration failed:', error);
				});
		}

		function handleGlobalKeydown(event: KeyboardEvent) {
			// Handle escape key for zoom out navigation or exit edit mode
			if (event.key === 'Escape') {
				// If we're in edit mode, let the edit mode handle the escape
				if (globalState.editMode) {
					// The Child component will handle exiting edit mode
					// Don't prevent default here to allow the input blur to work
					return;
				}

				// Check if we're currently editing (input fields, etc.) - fallback check
				const activeElement = document.activeElement;
				const isEditing =
					activeElement &&
					(activeElement.tagName === 'INPUT' ||
						activeElement.tagName === 'TEXTAREA' ||
						(activeElement as HTMLElement).isContentEditable ||
						activeElement.closest('.node-edit-input'));

				// Only trigger navigation if we're not currently editing
				if (!isEditing) {
					event.preventDefault();
					globalState.zoomOut();
				}
			}
		}

		// Enhanced Visual Viewport API with better feature detection
		function handleViewportChange() {
			// Multiple levels of feature detection for better compatibility
			if (!window.visualViewport && !window.innerHeight) return;

			let keyboardHeight = 0;

			// Primary method: Visual Viewport API (best support)
			if (window.visualViewport) {
				keyboardHeight = window.innerHeight - window.visualViewport.height;
			}
			// Fallback method: Screen height comparison
			else if (window.screen && window.screen.height) {
				keyboardHeight = Math.max(0, window.screen.height - window.innerHeight - 100);
			}

			// Apply dynamic adjustment for virtual keyboard with vendor prefixes
			if (keyboardHeight > 0) {
				document.documentElement.style.setProperty('--keyboard-height', `${keyboardHeight}px`);
				// Also set with vendor prefixes for better compatibility
				document.documentElement.style.setProperty(
					'-webkit-keyboard-height',
					`${keyboardHeight}px`
				);
				document.documentElement.style.setProperty('-moz-keyboard-height', `${keyboardHeight}px`);
			} else {
				document.documentElement.style.setProperty('--keyboard-height', '0px');
				document.documentElement.style.setProperty('-webkit-keyboard-height', '0px');
				document.documentElement.style.setProperty('-moz-keyboard-height', '0px');
			}
		}

		// Enhanced browser support detection and setup
		const hasVisualViewport = 'visualViewport' in window;

		// Store the scroll handler reference for cleanup
		const scrollHandler = (event: Event) => {
			// Only handle if viewport height is significantly smaller (keyboard is open)
			const keyboardHeight = window.innerHeight - (window?.visualViewport?.height || 0);
			if (keyboardHeight > 100) {
				// Only if keyboard is likely open
				handleViewportChange();
			}
		};

		// Set up Visual Viewport API listener with enhanced compatibility
		if (hasVisualViewport && window.visualViewport) {
			window.visualViewport.addEventListener('resize', handleViewportChange);
			// Only listen to scroll events when virtual keyboard is active
			// This prevents interference with normal page scrolling
			window.visualViewport.addEventListener('scroll', scrollHandler);
		}
		// Fallback to window resize for older browsers
		else {
			window.addEventListener('resize', handleViewportChange);
			// Additional orientation change listener for mobile
			window.addEventListener('orientationchange', () => {
				// Delay to ensure dimensions are updated
				setTimeout(handleViewportChange, 100);
			});
		}

		// Initial calculation
		handleViewportChange();

		document.addEventListener('keydown', handleGlobalKeydown);

		return () => {
			document.removeEventListener('keydown', handleGlobalKeydown);
			if (hasVisualViewport && window.visualViewport) {
				window.visualViewport.removeEventListener('resize', handleViewportChange);
				window.visualViewport.removeEventListener('scroll', scrollHandler);
			}
		};
	});
</script>

<svelte:head>
	<link rel="manifest" href="{base}/manifest.json" />
</svelte:head>

<main>
	<div class="app-header">
		<Header />
		<!-- Temporary debug component
		<SimpleNotificationTest /> -->
	</div>
	<div class="app-content">
		{@render children()}
	</div>
</main>

<!-- Toaster component for svelte-french-toast - positioned at the bottom -->
<Toaster position="bottom-center" />

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
		padding: 16px;
		position: relative;
		z-index: 1;
		/* Remove fixed height calculation, let flexbox handle it */
		min-height: 0; /* Important for flexbox overflow */
		/* Enhanced mobile scrolling */
		-webkit-overflow-scrolling: touch;
	}
</style>
