<script lang="ts">

// update this to work with gun and be contributor based.

	// Define types for our data structure
	interface RecognitionDataPoint {
		category: string;
		peer: string;
		value: number;
	}

	// Record type definitions
	type GroupedData = Record<string, { [peer: string]: number; total: number }>;
	type PercentageData = Record<string, { [peer: string]: number }>;
	type ColorMap = Record<string, string>;

	// Component props using Svelte 5 runes
	let { data = [], colorScheme = ['#3498db', '#e74c3c', '#2ecc71', '#f39c12', '#9b59b6'] } =
		$props<{
			data: RecognitionDataPoint[];
			colorScheme?: string[];
		}>();

	// State variables
	let hoveredBar = $state<{
		category: string;
		peer: string;
		value: number;
	} | null>(null);

	let uniquePeers = $state<string[]>([]);
	let peerColors = $state<ColorMap>({});
	let groupedData = $state<GroupedData>({});
	let sortedCategories = $state<string[]>([]);
	let percentages = $state<PercentageData>({});

	// Process data whenever it changes
	$effect(() => {
		processData(data);
	});

	// Process the data
	function processData(inputData: RecognitionDataPoint[]): void {
		// Get unique categories and peers
		const uniqueCategories = new Set<string>();
		const uniquePeerSet = new Set<string>();

		inputData.forEach((item) => {
			uniqueCategories.add(item.category);
			uniquePeerSet.add(item.peer);
		});

		// Update peers array
		uniquePeers = Array.from(uniquePeerSet);

		// Assign colors to peers
		const colors: ColorMap = {};
		uniquePeers.forEach((peer, i) => {
			colors[peer] = colorScheme[i % colorScheme.length];
		});
		peerColors = colors;

		// Process data into grouped structure
		const result: GroupedData = {};

		// Initialize structure
		uniqueCategories.forEach((category) => {
			result[category] = { total: 0 };
			uniquePeers.forEach((peer) => {
				result[category][peer] = 0;
			});
		});

		// Fill with actual values
		inputData.forEach((item) => {
			result[item.category][item.peer] = item.value;
			result[item.category].total += item.value;
		});

		// Update grouped data
		groupedData = result;

		// Sort categories by total
		sortedCategories = Object.entries(result)
			.sort((a, b) => b[1].total - a[1].total)
			.map(([category]) => category);

		// Calculate percentages
		const percentageResult: PercentageData = {};

		Object.keys(result).forEach((category) => {
			percentageResult[category] = {};
			const total = result[category].total;

			uniquePeers.forEach((peer) => {
				if (total > 0) {
					percentageResult[category][peer] = result[category][peer] / total;
				} else {
					percentageResult[category][peer] = 0;
				}
			});
		});

		percentages = percentageResult;
	}

	// Hover state management
	function setHoveredBar(category: string, peer: string, value: number): void {
		hoveredBar = { category, peer, value };
	}

	function clearHoveredBar(): void {
		hoveredBar = null;
	}
</script>

<div class="chart-container">
	{#each sortedCategories as category}
		<div class="bar-row">
			{#each uniquePeers as peer}
				{#if groupedData[category]?.[peer] > 0}
					<div
						class="bar-segment"
						style="
              width: {(percentages[category][peer] * 100).toFixed(1)}%; 
              background-color: {peerColors[peer]};
            "
						onclick={() => setHoveredBar(category, peer, groupedData[category][peer])}
						title="{category}: {peer}"
					></div>
				{/if}
			{/each}
		</div>
	{/each}

	{#if hoveredBar}
		<div class="tooltip">
			<div class="tooltip-content">
				<strong>{hoveredBar.category}: {hoveredBar.peer}</strong>
				<div>
					{hoveredBar.value} ({(percentages[hoveredBar.category][hoveredBar.peer] * 100).toFixed(
						1
					)}%)
				</div>
			</div>
		</div>
		<div class="tooltip-overlay" onclick={clearHoveredBar}></div>
	{/if}
</div>

<style>
	.chart-container {
		width: 100%;
		height: 100%;
		display: flex;
		flex-direction: column;
		position: relative;
	}

	.bar-row {
		flex: 1;
		display: flex;
		overflow: hidden;
		margin-bottom: 1px;
	}

	.bar-segment {
		height: 100%;
		transition: all 0.2s ease;
		cursor: pointer;
	}

	.bar-segment:hover {
		opacity: 0.8;
		filter: brightness(1.1);
	}

	.tooltip-overlay {
		position: fixed;
		top: 0;
		left: 0;
		right: 0;
		bottom: 0;
		z-index: 1;
	}

	.tooltip {
		position: absolute;
		top: 50%;
		left: 50%;
		transform: translate(-50%, -50%);
		background-color: rgba(0, 0, 0, 0.85);
		color: white;
		padding: 8px 12px;
		border-radius: 4px;
		font-size: 0.9rem;
		z-index: 2;
		box-shadow: 0 3px 10px rgba(0, 0, 0, 0.2);
		min-width: 120px;
		text-align: center;
		line-height: 1.4;
	}
</style>
