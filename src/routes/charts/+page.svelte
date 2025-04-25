<script lang="ts">
	import { onMount, getContext } from 'svelte';
	import type { RecognitionStore } from '../../types/types';
	
	let ChartsComponent: any;
	
	// Get data from props
	let { data } = $props<{
		data: Record<string, any>;
	}>();
	
	// Get store from context
	const recStore = getContext<RecognitionStore>('recStore');
	
	// Debugging to help diagnose issues
	console.log('Charts page - recStore from context:', recStore);

	onMount(async () => {
		try {
			console.log('Loading Charts component...');
			const module = await import('$lib/components/StackedBar.svelte');
			ChartsComponent = module.default;
			console.log('Charts component loaded');
		} catch (error) {
			console.error('Error loading Charts component:', error);
		}
	});
	
	// Sample data for charts
	const chartData = [
		{ category: "Recognition", peer: "Self", value: 25 },
		{ category: "Recognition", peer: "Peer", value: 15 },
		{ category: "Contributions", peer: "Self", value: 20 },
		{ category: "Contributions", peer: "Peer", value: 10 },
		{ category: "Innovation", peer: "Self", value: 15 },
		{ category: "Innovation", peer: "Peer", value: 30 },
	];
</script>

<div class="charts-wrapper panel-wrapper">
	{#if ChartsComponent}
		<svelte:component 
			this={ChartsComponent} 
			data={chartData}
			width={800}
		/>
	{:else}
		<div class="loading">
			Loading charts...
			{#if !recStore}
				<div class="error mt-10">(Store is not available)</div>
			{/if}
		</div>
	{/if}
</div>

<!-- CSS moved to global app.css --> 