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

	// Set up global keyboard event listener and virtual keyboard handling
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

		// Modern VirtualKeyboard API for Chromium browsers
		if ('virtualKeyboard' in navigator) {
			try {
				// @ts-ignore - VirtualKeyboard API is experimental
				navigator.virtualKeyboard.overlaysContent = true;
				console.log('VirtualKeyboard API enabled');
			} catch (error) {
				console.warn('VirtualKeyboard API failed to initialize:', error);
			}
		}

		// Fallback: iOS Safari visual viewport handling
		function handleViewportChange() {
			const visualViewport = window.visualViewport;
			if (visualViewport) {
				const viewportHeight = visualViewport.height;
				const windowHeight = window.innerHeight;
				const keyboardHeight = Math.max(0, windowHeight - viewportHeight);

				// Update CSS custom property for keyboard height
				document.documentElement.style.setProperty('--keyboard-height', `${keyboardHeight}px`);

				// Prevent the interface from being stuck when keyboard dismisses
				if (keyboardHeight === 0) {
					// Small delay to ensure proper reset
					setTimeout(() => {
						window.scrollTo(0, 0);
					}, 100);
				}
			}
		}

		// Set up visual viewport listeners for fallback
		if (window.visualViewport) {
			window.visualViewport.addEventListener('resize', handleViewportChange);
			// Initial call
			handleViewportChange();
		}

		document.addEventListener('keydown', handleGlobalKeydown);

		return () => {
			document.removeEventListener('keydown', handleGlobalKeydown);
			if (window.visualViewport) {
				window.visualViewport.removeEventListener('resize', handleViewportChange);
			}
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
		/* Handle virtual keyboard appearance */
		padding-bottom: max(env(safe-area-inset-bottom), env(keyboard-inset-height, 0px));
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
		/* Adjust padding bottom for keyboard (fallback) */
		padding-bottom: calc(16px + var(--keyboard-height, 0px));
	}

	.toast-container {
		position: fixed;
		bottom: calc(20px + env(safe-area-inset-bottom) + env(keyboard-inset-height, 0px));
		right: calc(20px + env(safe-area-inset-right));
		z-index: 100000; /* Ensure toasts appear above all dropdowns (99999) */
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

	/* Additional CSS for older browsers and edge cases */
	@supports not (padding: env(keyboard-inset-height)) {
		.app-content {
			/* Fallback behavior for browsers without keyboard-inset support */
			padding-bottom: calc(16px + var(--keyboard-height, 0px));
		}
	}
</style>
