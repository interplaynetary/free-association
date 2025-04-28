<script lang="ts">
	/**
	 * NestedPieExample.svelte - Example usage of the NestedPie component
	 */
	import NestedPie from '../NestedPie.svelte';
	import * as d3 from 'd3';

	// Define types for our component
	interface PieSlice {
		name: string;
		value: number;
		color?: string;
	}

	interface PieChartData {
		name: string;
		slices: Array<PieSlice>;
	}

	// Create distinct pie charts for each layer
	const layeredData: Array<PieChartData> = [
		// Layer 0 (innermost)
		{
			name: 'Alex',
			slices: [
				{ name: 'Ethan', value: 35, color: '#3498db' },
				{ name: 'Liam', value: 25, color: '#e74c3c' },
				{ name: 'Noah', value: 20, color: '#2ecc71' },
				{ name: 'Oliver', value: 15, color: '#9b59b6' },
				{ name: 'Benjamin', value: 5, color: '#f1c40f' }
			]
		},

		// Layer 1
		{
			name: 'Logan',
			slices: [
				{ name: 'William', value: 22, color: '#2980b9' },
				{ name: 'Lucas', value: 13, color: '#21618c' }
			]
		},

		// Layer 2
		{
			name: 'Mason',
			slices: [
				{ name: 'Elijah', value: 12, color: '#1f618d' },
				{ name: 'James', value: 6, color: '#2471a3' },
				{ name: 'Gabriel', value: 4, color: '#2980b9' }
			]
		},

		// Layer 3
		{
			name: 'Michael',
			slices: [
				{ name: 'Alexander', value: 7, color: '#154360' },
				{ name: 'Anthony', value: 3, color: '#1a5276' },
				{ name: 'Christopher', value: 2, color: '#1e5d8c' }
			]
		},

		// Layer 4
		{
			name: 'Daniel',
			slices: [
				{ name: 'Matthew', value: 3, color: '#0e2b3a' },
				{ name: 'Joshua', value: 2.5, color: '#0e2b3a' },
				{ name: 'Nicholas', value: 1.5, color: '#103649' }
			]
		},

		// Layer 5 (outermost)
		{
			name: 'Andrew',
			slices: [
				{ name: 'Joseph', value: 2, color: '#091b24' },
				{ name: 'Samuel', value: 1, color: '#0c2c3b' }
			]
		}
	];

	// Handle pie click events
	function handlePieClick(event: CustomEvent<{ data: PieSlice; layer: number }>): void {
		console.log('Pie segment clicked:', event.detail);
		alert(`Clicked on: ${event.detail.data.name} at layer ${event.detail.layer}`);
	}

	// Example of custom color schemes for specific layers
	const customColorSchemes = [
		// Layer 0 - Industry Sectors (use default colors from data)
		(d: PieSlice, i: number) => d.color || d3.schemeCategory10[i % 10],

		// Layer 1 - Technology Sectors (blues)
		(d: PieSlice, i: number) => d.color || d3.interpolateBlues(0.3 + i * 0.3),

		// Layer 2 - Software Segments (purples)
		(d: PieSlice, i: number) => d.color || d3.interpolatePurples(0.4 + i * 0.2)
	];
</script>

<NestedPie
	layers={layeredData}
	width={600}
	height={600}
	colorSchemes={customColorSchemes}
	centerRadius={60}
	ringThickness={40}
	padAngle={0.01}
	cornerRadius={3}
	on:pieClick={handlePieClick}
/>
