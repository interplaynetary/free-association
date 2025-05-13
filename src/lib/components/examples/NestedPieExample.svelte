<script lang="ts">
	/**
	 * NestedPieExample.svelte - Example usage of the NestedPie component
	 * Now using real data from the centralized system instead of example data
	 */
	import NestedPie from '../NestedPie.svelte';
	import { onMount } from 'svelte';
	import { globalState } from '$lib/global.svelte';
	import { providerShares } from '$lib/centralized/calculations';
	import { getNodeName } from '$lib/centralized/forestUtils';
	import type { PieSlice, PieChartData } from '../NestedPie.svelte';
	import type { ShareMap, Forest, TreeZipper } from '$lib/centralized/types';

	// State for pie chart data
	let layeredData = $state<Array<PieChartData>>([]);
	let maxLayers = 4; // Maximum number of layers to calculate share depths for
	let showDebugInfo = $state(false);

	// Initialize with example data immediately to ensure the chart renders
	layeredData = createExampleData();

	// Watch for changes in the current user, forest or path
	$effect(() => {
		const currentUser = globalState.currentUser;
		const currentForest = globalState.currentForest;
		const currentPath = globalState.currentPath; // Adding this to trigger reactive updates when modifying the tree

		if (currentUser && currentForest) {
			// Update shares whenever forest or user changes
			updateProviderShares();
		} else {
			// If no user is logged in, use example data
			layeredData = createExampleData();
		}
	});

	// Function to update provider shares based on the current user
	function updateProviderShares() {
		const currentUser = globalState.currentUser;
		if (!currentUser || !globalState.currentForest) return;

		// Get the username for the current user
		const username = currentUser.username?.toLowerCase() || currentUser.pub.toLowerCase();

		// Get the zipper for the current user from the forest
		const userZipper = globalState.currentForest.get(username);
		if (!userZipper) {
			console.log(`User zipper not found for username: ${username}`);
			layeredData = createExampleData();
			return;
		}

		// Generate pie chart data for different depths
		const newLayers: Array<PieChartData> = [];

		// Calculate shares at each depth (1 through maxLayers)
		for (let depth = 1; depth <= maxLayers; depth++) {
			try {
				// Get provider shares for the current user at this depth
				const shares = providerShares(globalState.currentForest, userZipper, depth);

				// Convert shares to pie slices
				const slices = shareMapToPieSlices(shares);

				// Only add to chart if we have actual shares
				if (slices.length > 0) {
					newLayers.push({
						name: `Depth ${depth}`,
						slices: slices
					});
				}
			} catch (err) {
				console.error(`Error calculating shares at depth ${depth}:`, err);
			}
		}

		// Update layered data - if we have data, use it, otherwise use example data
		if (newLayers.length > 0) {
			layeredData = newLayers;
		} else {
			// If there are no valid shares found, use example data
			layeredData = createExampleData();
		}
	}

	// Convert a share map to pie slices
	function shareMapToPieSlices(shares: ShareMap): Array<PieSlice> {
		// If empty, return empty array
		if (shares.size === 0) return [];

		const slices = Array.from(shares.entries())
			.filter(([_, share]) => share > 0)
			.map(([id, share]) => ({
				name: getNodeName(globalState.currentForest, id),
				value: Math.round(share * 100) // Convert to percentage value
			}))
			.sort((a, b) => b.value - a.value); // Sort by value descending

		return slices;
	}

	// Create example data if no user is logged in or no shares are found
	function createExampleData(): Array<PieChartData> {
		return [
			{
				name: 'Direct Contributors',
				slices: [
					{ name: 'Alice', value: 35 },
					{ name: 'Bob', value: 25 },
					{ name: 'Charlie', value: 20 },
					{ name: 'David', value: 15 },
					{ name: 'Eve', value: 5 }
				]
			},
			{
				name: 'Secondary Contributors',
				slices: [
					{ name: 'Alice', value: 30 },
					{ name: 'Bob', value: 25 },
					{ name: 'Charlie', value: 20 },
					{ name: 'David', value: 15 },
					{ name: 'Eve', value: 10 }
				]
			},
			{
				name: 'Tertiary Contributors',
				slices: [
					{ name: 'Alice', value: 25 },
					{ name: 'Bob', value: 25 },
					{ name: 'Charlie', value: 20 },
					{ name: 'David', value: 15 },
					{ name: 'Eve', value: 15 }
				]
			}
		];
	}

	// Handle pie click events
	function handlePieClick(event: CustomEvent<{ data: PieSlice; layer: number }>): void {
		console.log('Pie segment clicked:', event.detail);
		// Show toast with the clicked segment info
		globalState.showToast(`${event.detail.data.name}: ${event.detail.data.value}%`, 'info');
	}

	function toggleDebugInfo() {
		showDebugInfo = !showDebugInfo;
	}

	// Initialize on mount
	onMount(() => {
		if (globalState.currentUser && globalState.currentForest) {
			updateProviderShares();
		} else {
			layeredData = createExampleData();
		}
	});
</script>

<div class="container">
	<div class="pie-container">
		<!-- 
		{#if globalState.currentUser}
			<h3 class="subtitle">
				Contribution Shares for {globalState.currentUser.alias}
			</h3>
		{:else}
			<h3 class="subtitle">Example Contribution Shares (Log in to see your data)</h3>
		{/if}

		Debug button for toggling debug info
		<div class="debug-buttons">
			<button class="debug-button" on:click={toggleDebugInfo}>
				{showDebugInfo ? 'Hide Debug Info' : 'Show Debug Info'}
			</button>
		</div>  -->

		<!-- Nested Pie Chart Component -->
		<div class="chart-container" id="pie-chart-container">
			{#if layeredData && layeredData.length > 0}
				<NestedPie
					layers={[...layeredData].reverse()}
					width={300}
					height={300}
					centerRadius={30}
					ringThickness={40}
					padAngle={0.02}
					cornerRadius={2}
					on:pieClick={handlePieClick}
				/>
			{:else}
				<div class="no-data">No chart data available</div>
			{/if}
		</div>
	</div>

	<!-- Debug information panel (only shown when toggled) -->
	{#if showDebugInfo}
		<div class="debug-panel">
			<div class="debug-info">
				<strong>Current Data:</strong>
				<pre>{JSON.stringify(layeredData, null, 2)}</pre>

				<strong>User:</strong>
				{globalState.currentUser?.alias || 'Not logged in'}<br />
				<strong>Path:</strong>
				{globalState.currentPath.join(' > ')}

				<div class="share-details">
					<strong>Current Share Details:</strong>
					{#each layeredData as layer, i}
						<div class="layer-details">
							<div class="layer-title">{layer.name}:</div>
							<ul>
								{#each layer.slices as slice}
									<li><strong>{slice.name}:</strong> {slice.value}%</li>
								{/each}
							</ul>
						</div>
					{/each}
				</div>
			</div>
		</div>
	{/if}
</div>

<style>
	.container {
		display: flex;
		flex-direction: column;
		align-items: center;
		width: 100%;
	}

	.pie-container {
		display: flex;
		flex-direction: column;
		align-items: center;
		margin-bottom: 1rem;
	}

	.chart-container {
		width: 320px;
		height: 320px;
		margin: 10px auto;
		border: 1px dashed rgba(0, 0, 0, 0.1);
		border-radius: 50%;
		display: flex;
		justify-content: center;
		align-items: center;
		overflow: visible; /* Allow hover effects to spill over */
	}

	.no-data {
		color: #666;
		font-style: italic;
	}

	.debug-panel {
		max-width: 500px;
		margin-top: 20px;
		padding: 10px;
		background-color: #f8f8f8;
		border: 1px solid #ddd;
		border-radius: 4px;
	}

	.debug-info {
		font-size: 0.8rem;
		background-color: #f5f5f5;
		border: 1px solid #ddd;
		border-radius: 4px;
		padding: 0.5rem 1rem;
		margin-bottom: 1rem;
		text-align: left;
	}

	.debug-info pre {
		max-height: 200px;
		overflow: auto;
		background-color: #f0f0f0;
		padding: 0.5rem;
		border-radius: 3px;
		font-size: 0.7rem;
	}

	.layer-details {
		margin-top: 0.5rem;
		border-left: 2px solid #ddd;
		padding-left: 0.5rem;
	}

	.layer-title {
		font-weight: bold;
		margin-bottom: 0.2rem;
	}

	.share-details {
		margin-top: 1rem;
	}

	.share-details ul {
		margin: 0.25rem 0;
		padding-left: 1.5rem;
	}
</style>
