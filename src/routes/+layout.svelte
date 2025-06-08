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

	// Set up global keyboard event listener
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

		// Mobile keyboard handling for viewport stability
		function handleViewportChange() {
			// Force a layout recalculation to ensure proper viewport reset
			// This is particularly important for mobile browsers when keyboard closes
			const main = document.querySelector('main');
			if (main) {
				// Temporarily force a style recalculation
				main.style.height = '100dvh';

				// Use requestAnimationFrame to ensure the browser processes the change
				requestAnimationFrame(() => {
					main.style.height = '100dvh';
				});
			}
		}

		// Enhanced keyboard detection and layout correction
		function setupMobileKeyboardHandling() {
			let isKeyboardOpen = false;
			let initialViewportHeight = window.visualViewport?.height || window.innerHeight;

			function detectKeyboardState() {
				const currentHeight = window.visualViewport?.height || window.innerHeight;
				const heightDifference = initialViewportHeight - currentHeight;

				// Keyboard is considered open if viewport height decreased significantly (more than 150px)
				const keyboardNowOpen = heightDifference > 150;

				if (isKeyboardOpen !== keyboardNowOpen) {
					isKeyboardOpen = keyboardNowOpen;

					if (!isKeyboardOpen) {
						// Keyboard just closed - ensure layout resets properly
						setTimeout(() => {
							handleViewportChange();
							// Double-check after a delay for stubborn browsers
							setTimeout(handleViewportChange, 300);
						}, 100);
					}
				}
			}

			// Update initial height on orientation change
			function updateInitialHeight() {
				setTimeout(() => {
					initialViewportHeight = window.visualViewport?.height || window.innerHeight;
				}, 500);
			}

			// Listen to various events that indicate viewport changes
			if (window.visualViewport) {
				window.visualViewport.addEventListener('resize', detectKeyboardState);
				window.visualViewport.addEventListener('scroll', detectKeyboardState);
			}

			window.addEventListener('resize', detectKeyboardState);
			window.addEventListener('orientationchange', updateInitialHeight);

			// Also listen for focus/blur events on inputs as additional indicators
			document.addEventListener('focusout', (event) => {
				const target = event.target as Element;
				if (target && (target.tagName === 'INPUT' || target.tagName === 'TEXTAREA')) {
					// Input lost focus - keyboard might be closing
					setTimeout(detectKeyboardState, 300);
				}
			});

			// Cleanup function
			return () => {
				if (window.visualViewport) {
					window.visualViewport.removeEventListener('resize', detectKeyboardState);
					window.visualViewport.removeEventListener('scroll', detectKeyboardState);
				}
				window.removeEventListener('resize', detectKeyboardState);
				window.removeEventListener('orientationchange', updateInitialHeight);
			};
		}

		document.addEventListener('keydown', handleGlobalKeydown);
		const cleanupKeyboard = setupMobileKeyboardHandling();

		return () => {
			document.removeEventListener('keydown', handleGlobalKeydown);
			cleanupKeyboard?.();
		};
	});
</script>

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
		/* Use dynamic viewport height with proper fallbacks */
		height: 100vh; /* Fallback for older browsers */
		height: 100dvh; /* Modern dynamic viewport height */
		width: 100vw;
		/* Use padding for safe areas (recommended approach) */
		padding-top: env(safe-area-inset-top);
		padding-bottom: env(safe-area-inset-bottom);
		padding-left: env(safe-area-inset-left);
		padding-right: env(safe-area-inset-right);
		/* Ensure total dimensions don't exceed viewport */
		box-sizing: border-box;
		/* Prevent any overflow that could cause scrolling */
		overflow: hidden;
	}

	.app-header {
		position: relative;
		width: 100%;
		z-index: 100;
		background: white;
		padding-bottom: 8px;
		min-height: 60px;
		display: flex;
		flex-direction: column;
		flex-shrink: 0;
	}

	.app-content {
		flex: 1;
		width: 100%;
		overflow-y: auto;
		padding: 16px;
		position: relative;
		z-index: 1;
		/* Remove fixed height calculation, let flexbox handle it */
		min-height: 0; /* Important for flexbox overflow */
	}

	.toast-container {
		position: fixed;
		bottom: calc(20px + env(safe-area-inset-bottom));
		right: calc(20px + env(safe-area-inset-right));
		z-index: 1000;
		/* Ensure toast doesn't interfere with document flow */
		pointer-events: none;
	}

	.toast {
		padding: 12px 20px;
		border-radius: 8px;
		margin-top: 10px;
		font-size: 14px;
		box-shadow: 0 4px 8px rgba(0, 0, 0, 0.1);
		transition: all 0.3s ease;
		animation: slide-in 0.3s ease forwards;
		/* Re-enable pointer events for the toast itself */
		pointer-events: auto;
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
