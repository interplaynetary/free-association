<script lang="ts">
	import Header from '$lib/components/Header.svelte';
	import '../app.css';
	import type { LayoutProps } from './$types';
	import { globalState } from '$lib/global.svelte';
	import { onMount } from 'svelte';

	// Layout props
	let { children }: LayoutProps = $props();

	// Derived toast state
	let toast = $derived(globalState.toast);

	// Set up global keyboard event listener and reliable viewport handling
	onMount(() => {
		function handleGlobalKeydown(event: KeyboardEvent) {
			// Handle escape key for zoom out navigation
			if (event.key === 'Escape') {
				// Check if we're currently editing (input fields, etc.)
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

		// Set up Visual Viewport API listener with enhanced compatibility
		if (hasVisualViewport && window.visualViewport) {
			window.visualViewport.addEventListener('resize', handleViewportChange);
			// Also listen to scroll events for better mobile support
			window.visualViewport.addEventListener('scroll', handleViewportChange);
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

		// PWA registration - SvelteKit PWA auto-registers the service worker
		if ('serviceWorker' in navigator) {
			console.log('Service Worker supported');
		}

		return () => {
			document.removeEventListener('keydown', handleGlobalKeydown);
			if (hasVisualViewport && window.visualViewport) {
				window.visualViewport.removeEventListener('resize', handleViewportChange);
				window.visualViewport.removeEventListener('scroll', handleViewportChange);
			}
		};
	});
</script>

<svelte:head>
	<meta name="theme-color" content="#ffffff" />
</svelte:head>

<main>
	<div class="app-header">
		<Header />
	</div>
	<div class="app-content">
		{@render children()}
	</div>
</main>

<!-- Toast notification - moved outside main to avoid layout conflicts -->
{#if toast.visible}
	<div class="toast-container">
		<div class="toast toast-{toast.type}">
			{toast.message}
		</div>
	</div>
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
		padding: 16px;
		position: relative;
		z-index: 1;
		/* Remove fixed height calculation, let flexbox handle it */
		min-height: 0; /* Important for flexbox overflow */
		/* Enhanced mobile scrolling */
		-webkit-overflow-scrolling: touch;
	}

	.toast-container {
		position: fixed;
		/* Enhanced safe area and keyboard height support with fallbacks */
		bottom: 20px; /* Base fallback */
		bottom: calc(20px + constant(safe-area-inset-bottom)); /* iOS 11.0-11.2 */
		bottom: calc(20px + env(safe-area-inset-bottom)); /* iOS 11.2+ */
		bottom: calc(
			20px + env(safe-area-inset-bottom) + var(--keyboard-height, 0px)
		); /* Full support */

		right: 20px; /* Base fallback */
		right: calc(20px + constant(safe-area-inset-right)); /* iOS 11.0-11.2 */
		right: calc(20px + env(safe-area-inset-right)); /* iOS 11.2+ */

		z-index: 100000; /* Ensure toasts appear above all dropdowns (99999) */
		/* Enhanced pointer events with vendor prefixes */
		pointer-events: none;
		-webkit-pointer-events: none;
		-moz-pointer-events: none;
	}

	.toast {
		padding: 12px 20px;
		border-radius: 8px;
		/* Enhanced border-radius with vendor prefixes for older browsers */
		-webkit-border-radius: 8px;
		-moz-border-radius: 8px;
		margin-top: 10px;
		font-size: 14px;
		/* Enhanced box-shadow with vendor prefixes */
		box-shadow: 0 4px 8px rgba(0, 0, 0, 0.1);
		-webkit-box-shadow: 0 4px 8px rgba(0, 0, 0, 0.1);
		-moz-box-shadow: 0 4px 8px rgba(0, 0, 0, 0.1);
		/* Enhanced transitions with vendor prefixes */
		transition: all 0.3s ease;
		-webkit-transition: all 0.3s ease;
		-moz-transition: all 0.3s ease;
		-o-transition: all 0.3s ease;
		/* Enhanced animations with vendor prefixes */
		animation: slide-in 0.3s ease forwards;
		-webkit-animation: slide-in 0.3s ease forwards;
		-moz-animation: slide-in 0.3s ease forwards;
		/* Re-enable pointer events for the toast itself */
		pointer-events: auto;
		-webkit-pointer-events: auto;
		-moz-pointer-events: auto;
	}

	@keyframes slide-in {
		from {
			transform: translateX(100%);
			opacity: 0;
		}
		to {
			transform: translateX(0);
			opacity: 1;
		}
	}

	/* Enhanced webkit animations for Safari and older browsers */
	@-webkit-keyframes slide-in {
		from {
			-webkit-transform: translateX(100%);
			opacity: 0;
		}
		to {
			-webkit-transform: translateX(0);
			opacity: 1;
		}
	}

	/* Enhanced moz animations for Firefox */
	@-moz-keyframes slide-in {
		from {
			-moz-transform: translateX(100%);
			opacity: 0;
		}
		to {
			-moz-transform: translateX(0);
			opacity: 1;
		}
	}

	.toast-success {
		background-color: #4caf50;
		color: white;
	}

	.toast-info {
		background-color: #2196f3;
		color: white;
	}

	.toast-warning {
		background-color: #ff9800;
		color: white;
	}

	.toast-error {
		background-color: #f44336;
		color: white;
	}
</style>
