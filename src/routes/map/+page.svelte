<script lang="ts">
	import { onMount } from 'svelte';
	import { globalState } from '$lib/global.svelte';
	import { goto } from '$app/navigation';
	import { base } from '$app/paths';

	// Set the view to 'map' when this route is accessed
	onMount(() => {
		globalState.setView('map');
	});

	// Redirect to home page - the view is already set to 'map'
	// This allows the toolbar and all other components to work normally
	onMount(() => {
		goto(base + '/', { replaceState: true });
	});
</script>

<!-- This page immediately redirects to home with map view active -->
