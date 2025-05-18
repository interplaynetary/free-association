<script lang="ts">
	import Header from '$lib/components/Header.svelte';
	import '../app.css';
	import type { LayoutProps } from './$types';
	import { globalState } from '$lib/global.svelte';
	import { currentUser } from '$lib/global.svelte';
	import { page } from '$app/stores';
	import { username, userpub } from '$lib/stores';
	import { browser } from '$app/environment';
	import type { UserData } from '$lib/global.svelte';

	// Layout data from +layout.ts using Svelte 5 runes
	let { children }: LayoutProps = $props();

	// Derived values using Svelte 5 syntax
	let toast = $derived(globalState.toast);
	let sessionData = $derived($page.data.session);

	// State with safe default values
	let viewportWidth = $state(0);
	let viewportHeight = $state(0);

	// Handle window resize
	function handleResize() {
		viewportWidth = window.innerWidth;
		viewportHeight = window.innerHeight;
	}

	// Initialize client-side only code
	$effect.root(() => {
		if (browser) {
			// Initialize viewport dimensions
			viewportWidth = window.innerWidth;
			viewportHeight = window.innerHeight;

			// Initialize global state
			globalState.initialize();
		}
	});

	// Update user data when session changes
	$effect(() => {
		if (sessionData?.user) {
			username.set(sessionData.user.name || '');
			userpub.set(sessionData.user.id || '');

			// Pass user data to global state via store
			const userData: UserData = {
				id: sessionData.user.id || '',
				name: sessionData.user.name || null,
				email: sessionData.user.email || null,
				image: sessionData.user.image || null
			};

			// Update currentUser via the store API
			currentUser.set(userData);
		} else {
			username.set('');
			userpub.set('');
			currentUser.set(null);
		}
	});
</script>

<svelte:window on:resize={handleResize} />

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
		min-height: 100vh;
	}

	.app-header {
		position: relative;
		width: 100%;
		z-index: 100;
		background: white;
	}

	.app-content {
		flex: 1;
		width: 100%;
		overflow-y: auto;
		padding: 16px;
		position: relative;
		z-index: 1;
	}

	.toast-container {
		position: fixed;
		bottom: 20px;
		right: 20px;
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
