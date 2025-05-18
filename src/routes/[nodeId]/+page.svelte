<script lang="ts">
	import Parent from '$lib/components/Parent.svelte';
	import { globalState } from '$lib/global.svelte';
	import { page } from '$app/stores';
	import { browser } from '$app/environment';
	import { userTree } from '$lib/global.svelte';

	// Get node ID from the route parameter
	let nodeId = $derived($page.params.nodeId);

	// Add loading state
	let isLoading = $state(true);
	let treeLoaded = $state(false);

	// Initialize tree and set current path
	$effect.root(() => {
		if (!browser) return;

		// Using top-level async IIFE
		(async () => {
			// If tree is not loaded, initialize it
			if (!globalState.tree) {
				await globalState.initialize();
			}

			// Set the current path to the node ID from the URL
			if (nodeId) {
				globalState.navigateToPath([nodeId]);
			}

			isLoading = false;
		})();

		// Return cleanup function
		return () => {};
	});

	// Track tree loading state with derived state
	let isTreeLoaded = $derived(!!userTree);

	$effect(() => {
		if (isTreeLoaded) {
			console.log('tree loaded', userTree);
			treeLoaded = true;
		}
	});
</script>

{#if isLoading}
	<div class="loading-container">
		<p>Loading your tree...</p>
	</div>
{:else if treeLoaded}
	<Parent />
{:else}
	<div class="loading-container">
		<p>Unable to load tree data. Please try again.</p>
	</div>
{/if}

<style>
	.loading-container {
		display: flex;
		justify-content: center;
		align-items: center;
		height: 80vh;
		font-size: 1.2rem;
		color: #666;
	}
</style>
