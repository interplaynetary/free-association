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
		// Layer 0 (innermost): Primary Industry Sectors
		{
			name: 'Industry Sectors',
			slices: [
				{ name: 'Technology', value: 35 },
				{ name: 'Finance', value: 25 },
				{ name: 'Healthcare', value: 20 },
				{ name: 'Retail', value: 15 },
				{ name: 'Energy', value: 5 }
			]
		},

		// Layer 1: Technology Breakdown
		{
			name: 'Technology Sectors',
			slices: [
				{ name: 'Software', value: 22 },
				{ name: 'Hardware', value: 13 }
			]
		},

		// Layer 2: Software Breakdown
		{
			name: 'Software Segments',
			slices: [
				{ name: 'Cloud Services', value: 12 },
				{ name: 'Mobile Apps', value: 6 },
				{ name: 'Web Development', value: 4 }
			]
		},

		// Layer 3: Cloud Services
		{
			name: 'Cloud Providers',
			slices: [
				{ name: 'AWS', value: 7 },
				{ name: 'Azure', value: 3 },
				{ name: 'Google Cloud', value: 2 }
			]
		},

		// Layer 4: AWS Services
		{
			name: 'AWS Services',
			slices: [
				{ name: 'EC2', value: 3 },
				{ name: 'S3', value: 2.5 },
				{ name: 'Lambda', value: 1.5 }
			]
		},

		// Layer 5 (outermost): EC2 Instance Types
		{
			name: 'EC2 Platforms',
			slices: [
				{ name: 'Linux', value: 2 },
				{ name: 'Windows', value: 1 }
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
