<script lang="ts">
	import Parent from '$lib/components/Parent.svelte';
	import { currentPath, userTree } from '$lib/simpleglobal.svelte';
	import { page } from '$app/stores';
	import { browser } from '$app/environment';
	import { get } from 'svelte/store';

	// Get node ID from the route parameter
	let nodeId = $derived($page.params.nodeId);

	// Loading states
	let isLoading = $state(true);

	// Set up navigation when the component loads
	function setupNavigation() {
		if (!browser) return;

		try {
			// Set the current path to the node ID from the URL
			if (nodeId) {
				currentPath.set([nodeId]);
			}
		} finally {
			isLoading = false;
		}
	}

	// Call setup right away
	setupNavigation();
</script>

{#if isLoading}
	<div class="loading-container">
		<p>Loading your tree...</p>
	</div>
{:else if get(userTree)}
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
