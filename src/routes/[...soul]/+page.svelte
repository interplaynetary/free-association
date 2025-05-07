<script lang="ts">
	import Parent from '$lib/components/Parent.svelte';
	import type { PageData } from './$types';
	import { globalState } from '$lib/global.svelte';
	import { goto } from '$app/navigation';

	// Get the soul from the URL without reloading the page, just update the components internally
	let { data }: { data: PageData } = $props();

	//render the soul ids for the paths (and then we can use the names simply for aesthetic purposes)
	// similar to how stack overflow does root/internal-id-of-post/name-of-post

	// Get current node ID and name from global state
	let currentNodeId = $derived(globalState.currentZipper?.zipperCurrent.nodeId || 'root');

	let currentNodeName = $derived(globalState.currentZipper?.zipperCurrent.nodeName || 'unnamed');

	// Update URL when the current node changes
	$effect(() => {
		if (currentNodeId) {
			const formattedName = currentNodeName?.replace(/\s+/g, '-').toLowerCase() || '';
			goto(`/${currentNodeId}${formattedName ? ':' + formattedName : ''}`);
		}
	});
</script>

<!-- For node page, use Parent with its own header -->
<Parent />
