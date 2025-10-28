<script lang="ts">
	/**
	 * NestedPieExample.svelte - Example usage of the NestedPie component
	 * Shows two rings: SOGF (direct contributions) and Provider Shares (mutual recognition)
	 */
	import NestedPie from './NestedPie.svelte';
	import { onMount } from 'svelte';
	import { globalState, currentPath } from '$lib/global.svelte';
	import { userAlias, userPub } from '$lib/state/auth.svelte';
	import { userTree, userSogf, generalShares } from '$lib/state/core.svelte';
	import type { PieSlice, PieChartData } from './NestedPie.svelte';
	import type { Node, RootNode } from '$lib/commons/v5/schemas';
	import { normalizeShareMap } from '$lib/commons/v5/protocol';
	import { get } from 'svelte/store';

	// State for pie chart data
	let layeredData = $state<Array<PieChartData>>([]);
	let showDebugInfo = $state(false);

	// Initialize with example data immediately to ensure the chart renders
	layeredData = createExampleData();

	// Watch for changes in the current user, path, SOGF, or providerShares
	$effect(() => {
		const tree = get(userTree);
		const sogf = get(userSogf);
		const shares = get(generalShares);
		const path = get(currentPath);

		if ($userAlias && $userPub && tree) {
			// Update shares whenever user data changes
			updateChartData();
		} else {
			// If no user is logged in, use example data
			layeredData = createExampleData();
		}
	});

	// Function to update chart data based on SOGF and providerShares
	function updateChartData() {
		const tree = get(userTree) as RootNode;
		const sogf = get(userSogf);
		const mutualRecognition = get(generalShares);

		if (!$userAlias || !$userPub || !tree) return;

		// Generate pie chart data for the two layers
		const newLayers: Array<PieChartData> = [];

		// Create nodes map for faster lookups
		const nodesMap: Record<string, Node> = {};

		// Populate nodes map (helper function to traverse tree)
		function populateNodesMap(node: Node) {
			nodesMap[node.id] = node;
			for (const child of node.children) {
				populateNodesMap(child);
			}
		}

		// Start with the root node
		populateNodesMap(tree);

		// Add SOGF layer (direct contributions to me)
		if (sogf && Object.keys(sogf).length > 0) {
			try {
				// Convert SOGF to pie slices
				const sogfSlices = shareMapToPieSlices(sogf, nodesMap);

				// Add SOGF layer
				if (sogfSlices.length > 0) {
					newLayers.push({
						name: 'Direct Contributions (SOGF)',
						slices: sogfSlices
					});
				}
			} catch (err) {
				console.error('Error creating SOGF layer:', err);
			}
		}

		// Add Provider Shares layer (mutual recognition)
		if (mutualRecognition && Object.keys(mutualRecognition).length > 0) {
			try {
				// Convert provider shares to pie slices
				const providerSlices = shareMapToPieSlices(mutualRecognition, nodesMap);

				// Add provider shares layer
				if (providerSlices.length > 0) {
					newLayers.push({
						name: 'Mutual Recognition',
						slices: providerSlices
					});
				}
			} catch (err) {
				console.error('Error creating Provider Shares layer:', err);
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
	function shareMapToPieSlices(
		shares: Record<string, number>,
		nodesMap: Record<string, Node>
	): Array<PieSlice> {
		// If empty, return empty array
		if (Object.keys(shares).length === 0) return [];

		const slices: PieSlice[] = [];

		// Process each share entry
		for (const [id, share] of Object.entries(shares)) {
			if (share > 0) {
				// Get node name from nodes map
				const node = nodesMap[id];
				if (node) {
					slices.push({
						name: node.name,
						value: Math.round(share * 100) // Convert to percentage value
					});
				}
			}
		}

		// Sort by value descending
		return slices.sort((a, b) => b.value - a.value);
	}

	// Create example data if no user is logged in or no shares are found
	function createExampleData(): Array<PieChartData> {
		return [
			{
				name: 'Direct Contributions (SOGF)',
				slices: [
					{ name: 'Alice', value: 35 },
					{ name: 'Bob', value: 25 },
					{ name: 'Charlie', value: 20 },
					{ name: 'David', value: 15 },
					{ name: 'Eve', value: 5 }
				]
			},
			{
				name: 'Mutual Recognition',
				slices: [
					{ name: 'Alice', value: 30 },
					{ name: 'Bob', value: 25 },
					{ name: 'Charlie', value: 20 },
					{ name: 'David', value: 15 },
					{ name: 'Eve', value: 10 }
				]
			}
		];
	}

	// Handle pie click events
	function handlePieClick(event: CustomEvent<{ data: PieSlice; layer: number }>): void {
		// Show toast with the clicked segment info
		globalState.showToast(`${event.detail.data.name}: ${event.detail.data.value}%`, 'info');
	}

	function toggleDebugInfo() {
		showDebugInfo = !showDebugInfo;
	}

	// Initialize on mount
	onMount(() => {
		if ($userAlias && $userPub && get(userTree)) {
			updateChartData();
		} else {
			layeredData = createExampleData();
		}
	});
</script>

<div class="container">
	<div class="pie-container">
		<div class="debug-buttons">
			<button class="debug-button" onclick={toggleDebugInfo}>
				{showDebugInfo ? 'Hide Debug Info' : 'Show Debug Info'}
			</button>
		</div>

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
				{$userAlias || 'Not logged in'}<br />
				<strong>Path:</strong>
				{get(currentPath).join(' > ')}

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

	.debug-buttons {
		margin-bottom: 10px;
	}

	.debug-button {
		padding: 5px 10px;
		background-color: #f0f0f0;
		border: 1px solid #ddd;
		border-radius: 4px;
		cursor: pointer;
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
