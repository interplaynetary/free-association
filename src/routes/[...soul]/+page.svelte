<script lang="ts">
	import Parent from '$lib/components/Parent.svelte';
	import type { PageData } from './$types';
	import { globalState } from '$lib/global.svelte';
	import { goto } from '$app/navigation';
	import { get } from 'svelte/store';
	// Get the soul from the URL without reloading the page, just update the components internally
	let { data }: { data: PageData } = $props();

	//render the soul ids for the paths (and then we can use the names simply for aesthetic purposes)
	// similar to how stack overflow does root/internal-id-of-post/name-of-post

	// this lays the foundation for us to reactively load the correct page based on the soul id and name
	// we will do this with a sort of zoom in functionality from the parent component
	$effect(() => {
		// $inspect(globalState.recStore.nameStore);
		const name = get(globalState.recStore.nameStore);
		const id = globalState.recStore.id; // temporary fix as any
		//console.log('data soul', id);
		if (name) {
			goto(`/${id}:${name}`);
		} else {
			goto(`/${id}`);
		}
	});
</script>

<!-- For node page, use Parent with its own header -->
<Parent />
