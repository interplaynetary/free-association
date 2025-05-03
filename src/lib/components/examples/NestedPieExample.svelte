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

	// Create distinct pie charts for each layer with increasing number of slices
	const layeredData: Array<PieChartData> = [
		// Layer 0 (innermost): Primary Industry Sectors
		{
			name: 'Alice',
			slices: [
				{ name: 'Bryce', value: 2 },
				{ name: 'Cameron', value: 1 },
				{ name: 'Diana', value: 0.5 },
				{ name: 'Ethan', value: 0.5 },
				{ name: 'Victor', value: 3 },
				{ name: 'Wendy', value: 2.5 },
				{ name: 'Xavier', value: 1.5 },
				{ name: 'Yvonne', value: 1 },
				{ name: 'Zoe', value: 0.5 },
				{ name: 'Quincy', value: 7 },
				{ name: 'Rachel', value: 3 },
				{ name: 'Sophia', value: 2 },
				{ name: 'Tessa', value: 1 },
				{ name: 'Lily', value: 12 },
				{ name: 'Mike', value: 6 },
				{ name: 'Nina', value: 4 },
				{ name: 'Olivia', value: 3 },
				{ name: 'Helen', value: 22 },
				{ name: 'Ian', value: 13 },
				{ name: 'Julia', value: 10 },
				{ name: 'Bob', value: 35 },
				{ name: 'Charlie', value: 25 },
				{ name: 'David', value: 20 },
				{ name: 'Eve', value: 15 },
				{ name: 'Frank', value: 5 }
			]
		},

		// Layer 1: Technology Breakdown
		{
			name: 'George',
			slices: [
				{ name: 'Victor', value: 3 },
				{ name: 'Wendy', value: 2.5 },
				{ name: 'Xavier', value: 1.5 },
				{ name: 'Yvonne', value: 1 },
				{ name: 'Zoe', value: 0.5 },
				{ name: 'Quincy', value: 7 },
				{ name: 'Rachel', value: 3 },
				{ name: 'Sophia', value: 2 },
				{ name: 'Tessa', value: 1 },
				{ name: 'Lily', value: 12 },
				{ name: 'Mike', value: 6 },
				{ name: 'Nina', value: 4 },
				{ name: 'Olivia', value: 3 },
				{ name: 'Helen', value: 22 },
				{ name: 'Ian', value: 13 },
				{ name: 'Julia', value: 10 },
				{ name: 'Bob', value: 35 },
				{ name: 'Charlie', value: 25 },
				{ name: 'David', value: 20 },
				{ name: 'Eve', value: 15 },
				{ name: 'Frank', value: 5 }
			]
		},

		// Layer 2: Software Breakdown
		{
			name: 'Kevin',
			slices: [
				{ name: 'Quincy', value: 7 },
				{ name: 'Rachel', value: 3 },
				{ name: 'Sophia', value: 2 },
				{ name: 'Tessa', value: 1 },
				{ name: 'Lily', value: 12 },
				{ name: 'Mike', value: 6 },
				{ name: 'Nina', value: 4 },
				{ name: 'Olivia', value: 3 },
				{ name: 'Helen', value: 22 },
				{ name: 'Ian', value: 13 },
				{ name: 'Julia', value: 10 },
				{ name: 'Bob', value: 35 },
				{ name: 'Charlie', value: 25 },
				{ name: 'David', value: 20 },
				{ name: 'Eve', value: 15 },
				{ name: 'Frank', value: 5 }
			]
		},

		// Layer 3: Cloud Services
		{
			name: 'Patrick',
			slices: [
				{ name: 'Lily', value: 12 },
				{ name: 'Mike', value: 6 },
				{ name: 'Nina', value: 4 },
				{ name: 'Olivia', value: 3 },
				{ name: 'Helen', value: 22 },
				{ name: 'Ian', value: 13 },
				{ name: 'Julia', value: 10 },
				{ name: 'Bob', value: 35 },
				{ name: 'Charlie', value: 25 },
				{ name: 'David', value: 20 },
				{ name: 'Eve', value: 15 },
				{ name: 'Frank', value: 5 }
			]
		},

		// Layer 4: AWS Services
		{
			name: 'Uma',
			slices: [
				{ name: 'Helen', value: 22 },
				{ name: 'Ian', value: 13 },
				{ name: 'Julia', value: 10 },
				{ name: 'Bob', value: 35 },
				{ name: 'Charlie', value: 25 },
				{ name: 'David', value: 20 },
				{ name: 'Eve', value: 15 },
				{ name: 'Frank', value: 5 }
			]
		},

		// Layer 5 (outermost): EC2 Instance Types
		{
			name: 'Ava',
			slices: [
				{ name: 'Bob', value: 35 },
				{ name: 'Charlie', value: 25 },
				{ name: 'David', value: 20 },
				{ name: 'Eve', value: 15 },
				{ name: 'Frank', value: 5 }
			]
		}
	];

	// Handle pie click events
	function handlePieClick(event: CustomEvent<{ data: PieSlice; layer: number }>): void {
		console.log('Pie segment clicked:', event.detail);
		alert(`Clicked on: ${event.detail.data.name} at layer ${event.detail.layer}`);
	}
</script>

<NestedPie
	layers={layeredData}
	width={300}
	height={300}
	centerRadius={40}
	ringThickness={100}
	padAngle={0.01}
	cornerRadius={3}
	on:pieClick={handlePieClick}
/>
