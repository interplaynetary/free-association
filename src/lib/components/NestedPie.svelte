<script lang="ts">
	/**
	 * NestedPie.svelte - A layered pie chart component with up to 6 distinct pie charts
	 * Built using D3's pie generator
	 */
	import * as d3 from 'd3';
	import { onMount, createEventDispatcher } from 'svelte';

	// Props
	export let layers: Array<PieChartData> = []; // Array of pie chart data sets to render as layers
	export let width: number = 800;
	export let height: number = 800;
	export let margin: number = 20;
	export let maxLayers: number = 6; // Maximum number of layers to display
	export let centerRadius: number = 50; // Radius of the innermost pie chart
	export let ringThickness: number = 50; // Thickness of each ring
	export let padAngle: number = 0.01; // Padding between pie segments
	export let cornerRadius: number = 0; // Corner radius for pie segments
	export let labels: boolean = true; // Whether to show labels
	export let colorSchemes: Array<(d: PieSlice, i: number) => string> = []; // Optional color schemes for each layer

	// Interfaces
	interface PieSlice {
		name: string;
		value: number;
		color?: string;
	}

	interface PieChartData {
		name: string;
		slices: Array<PieSlice>;
	}

	// Event dispatcher for interactions
	const dispatch = createEventDispatcher<{
		pieClick: { data: PieSlice; layer: number };
	}>();

	// Local state
	let chartContainer: HTMLDivElement;
	let svg: d3.Selection<SVGSVGElement, unknown, null, undefined>;
	let tooltip: d3.Selection<HTMLDivElement, unknown, HTMLElement, any>;

	// Process data on changes
	$: if (layers && chartContainer) {
		renderChart();
	}

	$: maxRadius = Math.min(width, height) / 2 - margin;
	$: layerRadii = Array.from(
		{ length: Math.min(layers.length, maxLayers) },
		(_, i) => centerRadius + i * ringThickness
	);

	// Get default color scheme for a layer
	function getDefaultColorScheme(layerIndex: number): (d: PieSlice, i: number) => string {
		if (colorSchemes[layerIndex]) {
			return colorSchemes[layerIndex];
		}

		// Create color schemes with different hue ranges for each layer
		const baseHue = (layerIndex * 60) % 360;
		return (d: PieSlice, i: number) => {
			if (d.color) return d.color;
			const hue = (baseHue + i * 30) % 360;
			return d3.hsl(hue, 0.7, 0.5).toString();
		};
	}

	// Function to hash a string to a color
	function hashStringToColor(str: string): string {
		let hash = 0;
		for (let i = 0; i < str.length; i++) {
			hash = str.charCodeAt(i) + ((hash << 5) - hash);
		}
		const color = (hash & 0x00ffffff).toString(16).padStart(6, '0').toUpperCase();
		return `#${color}`;
	}

	onMount(() => {
		if (layers.length && chartContainer) {
			renderChart();
		}

		// Create tooltip
		tooltip = d3
			.select('body')
			.append('div')
			.attr('class', 'nested-pie-tooltip')
			.style('opacity', 0)
			.style('position', 'absolute')
			.style('background', 'rgba(0,0,0,0.7)')
			.style('color', 'white')
			.style('padding', '8px')
			.style('border-radius', '4px')
			.style('pointer-events', 'none')
			.style('z-index', 100)
			.style('font-size', '12px');

		return () => {
			// Clean up tooltip on component destroy
			if (tooltip) tooltip.remove();
		};
	});

	// Main chart rendering function
	function renderChart() {
		// Clean up any existing chart
		if (chartContainer) {
			chartContainer.innerHTML = '';
		}

		// Create SVG
		svg = d3
			.select(chartContainer)
			.append('svg')
			.attr('width', width)
			.attr('height', height)
			.attr('viewBox', [-width / 2, -height / 2, width, height])
			.attr('style', 'max-width: 100%; height: auto;');

		// Create a group for the pie chart, centered in the SVG
		const chartGroup = svg.append('g');

		// Render each layer (from largest to smallest)
		// Render in reverse order so smaller pies appear on top
		for (let i = Math.min(layers.length, maxLayers) - 1; i >= 0; i--) {
			renderPieLayer(layers[i], i);
		}
	}

	// Render a specific layer of the pie chart
	function renderPieLayer(layerData: PieChartData, layerIndex: number) {
		// Create pie generator
		const pie = d3
			.pie<PieSlice>()
			.value((d) => d.value)
			.sort(null)
			.padAngle(padAngle);

		// Calculate inner and outer radius based on layer
		const innerRadius = layerIndex === 0 ? 0 : centerRadius + (layerIndex - 1) * ringThickness;
		const outerRadius = centerRadius + layerIndex * ringThickness;

		// Create arc generator
		const arc = d3
			.arc<d3.PieArcDatum<PieSlice>>()
			.innerRadius(innerRadius)
			.outerRadius(outerRadius)
			.cornerRadius(cornerRadius);

		// Get pie layout data
		const arcs = pie(layerData.slices);

		// Get color scheme for this layer
		const colorScale = getDefaultColorScheme(layerIndex);

		// Create pie segments
		const segments = svg
			.select('g')
			.selectAll(`.pie-segment-layer-${layerIndex}`)
			.data(arcs)
			.join('path')
			.attr('class', `pie-segment-layer-${layerIndex}`)
			.attr('d', arc)
			.attr('fill', (d) => hashStringToColor(d.data.name))
			.attr('stroke', 'white')
			.attr('stroke-width', 1)
			.style('cursor', 'pointer');

		// Add hover interactions
		segments
			.on('mouseover', (event, d) => {
				d3.select(event.currentTarget)
					.transition()
					.duration(200)
					.attr('transform', getSegmentExpansion(d, layerIndex));

				tooltip
					.style('opacity', 1)
					.html(
						`
            <div>
              <strong>${layerData.name}: ${d.data.name}</strong>
              <div>Value: ${d.data.value}</div>
              <div>Percentage: ${(((d.endAngle - d.startAngle) / (2 * Math.PI)) * 100).toFixed(1)}%</div>
            </div>
          `
					)
					.style('left', event.pageX + 10 + 'px')
					.style('top', event.pageY - 28 + 'px');
			})
			.on('mouseout', (event) => {
				d3.select(event.currentTarget)
					.transition()
					.duration(200)
					.attr('transform', 'translate(0, 0)');

				tooltip.style('opacity', 0);
			})
			.on('click', (event, d) => {
				dispatch('pieClick', {
					data: d.data,
					layer: layerIndex
				});
			});
	}

	// Helper function to create expansion effect on hover
	function getSegmentExpansion(d: d3.PieArcDatum<any>, layerIndex: number): string {
		const angle = (d.startAngle + d.endAngle) / 2;
		const x = Math.sin(angle) * 5;
		const y = -Math.cos(angle) * 5;
		return `translate(${x}, ${y})`;
	}
</script>

<!-- Layered pie chart with up to 6 distinct layers -->
<div bind:this={chartContainer}></div>
