<script lang="ts">
	import { onMount, getContext } from 'svelte';
	import type { RecognitionStore } from '../../types/types';
	
	let InventoryComponent: any;
	
	// Get data from props
	let { data } = $props<{
		data: Record<string, any>;
	}>();
	
	// Get store from context
	const recStore = getContext<RecognitionStore>('recStore');
	
	// Debugging to help diagnose issues
	console.log('Inventory page - recStore from context:', recStore);

	onMount(async () => {
		try {
			console.log('Loading Inventory component...');
			const module = await import('$lib/components/Inventory.svelte');
			InventoryComponent = module.default;
			console.log('Inventory component loaded');
		} catch (error) {
			console.error('Error loading Inventory component:', error);
		}
	});
</script>

<div class="inventory-wrapper panel-wrapper">
	{#if InventoryComponent && recStore}
		<svelte:component this={InventoryComponent} store={recStore} />
	{:else}
		<div class="loading">
			Loading inventory...
			{#if !recStore}
				<div class="error mt-10">(Store is not available)</div>
			{/if}
		</div>
	{/if}
</div>

<!-- CSS moved to global app.css --> 