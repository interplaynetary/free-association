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

	// Set up global keyboard event listener and mobile viewport handling
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

		// Mobile viewport height handling
		function setViewportHeight() {
			// Get the actual viewport height
			const vh = window.innerHeight * 0.01;
			document.documentElement.style.setProperty('--vh', `${vh}px`);

			// For iOS Safari specifically - force height update
			if (/iPad|iPhone|iPod/.test(navigator.userAgent)) {
				document.body.style.height = `${window.innerHeight}px`;
			}

			// Also update the root font size for consistent scaling
			const vw = window.innerWidth * 0.01;
			document.documentElement.style.setProperty('--vw', `${vw}px`);
		}

		// Set initial viewport height
		setViewportHeight();

		// Update on resize/orientation change
		window.addEventListener('resize', setViewportHeight);
		window.addEventListener('orientationchange', () => {
			// Delay for orientation change to complete
			setTimeout(setViewportHeight, 100);
		});

		// Handle mobile browser toolbar show/hide
		let ticking = false;
		function handleScroll() {
			if (!ticking) {
				requestAnimationFrame(() => {
					setViewportHeight();
					ticking = false;
				});
				ticking = true;
			}
		}

		window.addEventListener('scroll', handleScroll, { passive: true });
		document.addEventListener('keydown', handleGlobalKeydown);

		return () => {
			window.removeEventListener('resize', setViewportHeight);
			window.removeEventListener('orientationchange', setViewportHeight);
			window.removeEventListener('scroll', handleScroll);
			document.removeEventListener('keydown', handleGlobalKeydown);
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

	<!-- Toast notification -->
	{#if toast.visible}
		<div class="toast-container">
			<div class="toast toast-{toast.type}">
				{toast.message}
			</div>
		</div>
	{/if}
</main>

<style>
	main {
		display: flex;
		flex-direction: column;
		/* Use custom CSS variable for reliable mobile viewport */
		height: calc(var(--vh, 1vh) * 100);
		min-height: calc(var(--vh, 1vh) * 100);
		/* Fallback for browsers without custom properties */
		height: 100vh;
		min-height: 100vh;
		/* Add safe area insets for mobile devices */
		padding-top: env(safe-area-inset-top);
		padding-bottom: env(safe-area-inset-bottom);
		padding-left: env(safe-area-inset-left);
		padding-right: env(safe-area-inset-right);
		overflow: hidden;
		position: relative;
		width: 100%;
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
		/* Additional mobile support */
		-webkit-overflow-scrolling: touch; /* Smooth scrolling on iOS */
		transform: translateZ(0); /* Force hardware acceleration */
	}

	.toast-container {
		position: fixed;
		bottom: calc(20px + env(safe-area-inset-bottom));
		right: calc(20px + env(safe-area-inset-right));
		z-index: 1000;
	}

	.toast {
		padding: 12px 20px;
		border-radius: 8px;
		margin-top: 10px;
		font-size: 14px;
		box-shadow: 0 4px 8px rgba(0, 0, 0, 0.1);
		transition: all 0.3s ease;
		animation: slide-in 0.3s ease forwards;
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
