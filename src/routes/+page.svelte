<script lang="ts">
	import { page } from '$app/stores';
	import { goto } from '$app/navigation';
	import { browser } from '$app/environment';
	import { globalState } from '$lib/global.svelte';

	let isLoading = $state(false);
	let userId = $derived($page.data.session?.user?.id);

	// Only initialize when in browser and user is logged in
	$effect.root(() => {
		if (!browser || !userId) return;

		isLoading = true;

		// Using top-level async IIFE to handle the async initialization
		(async () => {
			// Make sure global state is initialized
			if (!globalState.tree) {
				await globalState.initialize();
			}

			console.log('Going to user root node');
			// Now redirect to user's root node
			goto(`/${userId}`);
		})();

		// Cleanup function that returns nothing (void)
		return () => {};
	});
</script>

{#if isLoading}
	<div class="loading-container">
		<p>Loading your data...</p>
	</div>
{:else}
	<div class="home-container">
		<div class="welcome-card">
			<h1>Welcome to Free Association</h1>
			<p>Connect, share, and fulfill needs in your network</p>

			<div class="actions">
				<a href="/auth/login" class="btn btn-primary">Sign In</a>
				<a href="/auth/register" class="btn btn-outline">Create Account</a>
			</div>
		</div>
	</div>
{/if}

<style>
	.loading-container {
		display: flex;
		justify-content: center;
		align-items: center;
		min-height: calc(100vh - 120px);
		font-size: 1.2rem;
		color: #666;
	}

	.home-container {
		display: flex;
		justify-content: center;
		align-items: center;
		min-height: calc(100vh - 120px);
		padding: 1rem;
	}

	.welcome-card {
		background: white;
		border-radius: 8px;
		box-shadow: 0 4px 12px rgba(0, 0, 0, 0.1);
		padding: 2rem;
		text-align: center;
		max-width: 500px;
		width: 100%;
	}

	h1 {
		color: #2196f3;
		margin-bottom: 1rem;
	}

	p {
		color: #666;
		margin-bottom: 2rem;
		font-size: 1.1rem;
	}

	.actions {
		display: flex;
		gap: 1rem;
		justify-content: center;
	}

	.btn {
		display: inline-block;
		padding: 0.75rem 1.5rem;
		border-radius: 4px;
		font-weight: 500;
		text-decoration: none;
		font-size: 1rem;
		transition: all 0.2s ease;
	}

	.btn-primary {
		background-color: #2196f3;
		color: white;
	}

	.btn-primary:hover {
		background-color: #1976d2;
	}

	.btn-outline {
		border: 1px solid #2196f3;
		color: #2196f3;
		background: transparent;
	}

	.btn-outline:hover {
		background-color: #f0f8ff;
	}
</style>
