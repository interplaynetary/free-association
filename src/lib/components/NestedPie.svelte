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
	export let centerRadius: number = 50; // Initial radius for calculations
	export let ringThickness: number = 50; // Base thickness used for calculations
	export let padAngle: number = 0.01; // Padding between pie segments
	export let cornerRadius: number = 0; // Corner radius for pie segments
	export let labels: boolean = false; // Whether to show labels (default to false)
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

	// Golden ratio for Fibonacci-inspired thickness
	const PHI = 0.618;

	// Process data on changes
	$: if (layers && chartContainer) {
		renderChart();
	}

	$: maxRadius = Math.min(width, height) / 2 - margin;

	// Calculate layer radii with Fibonacci-inspired progression
	$: layerRadii = calculateLayerRadii(
		Math.min(layers.length, maxLayers),
		centerRadius,
		ringThickness
	);

	// Calculate layer radii using the golden ratio
	function calculateLayerRadii(
		numLayers: number,
		baseRadius: number,
		baseThickness: number
	): number[] {
		// Start with the maximum available radius
		const maxAvailableRadius = maxRadius;

		// Calculate how much space we need for all layers
		let totalThickness = 0;
		const thicknessFactors = [];

		// Generate thickness factors using golden ratio in reverse
		// (largest at center, diminishing outward)
		for (let i = 0; i < numLayers; i++) {
			const factor = Math.pow(PHI, i);
			thicknessFactors.push(factor);
			totalThickness += factor * baseThickness;
		}

		// Scale all thicknesses to fit within the maximum radius
		const scaleFactor = maxAvailableRadius / totalThickness;

		// Calculate cumulative radii
		const radii = [];
		let currentRadius = 0;

		for (let i = 0; i < numLayers; i++) {
			const thickness = thicknessFactors[i] * baseThickness * scaleFactor;
			currentRadius += thickness;
			radii.push(currentRadius);
		}

		return radii.reverse(); // Reverse to have largest radius first (outermost layer)
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

		// Get the inner and outer radius for this layer
		const outerRadius = layerRadii[layerIndex];
		const innerRadius = layerIndex === layerRadii.length - 1 ? 0 : layerRadii[layerIndex + 1];

		// Create arc generator
		const arc = d3
			.arc<d3.PieArcDatum<PieSlice>>()
			.innerRadius(innerRadius)
			.outerRadius(outerRadius)
			.cornerRadius(cornerRadius);

		// Get pie layout data
		const arcs = pie(layerData.slices);

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

		// Only add text labels if enabled
		if (labels) {
			// Add text labels
			const labelRadius = (innerRadius + outerRadius) / 2;

			svg
				.select('g')
				.selectAll(`.pie-label-layer-${layerIndex}`)
				.data(arcs)
				.join('text')
				.attr('class', `pie-label-layer-${layerIndex}`)
				.attr('transform', (d) => {
					const [x, y] = arc.centroid(d);
					return `translate(${x}, ${y})`;
				})
				.attr('text-anchor', 'middle')
				.attr('pointer-events', 'none')
				.attr('fill', 'white')
				.style('font-size', `${Math.max(8, (outerRadius - innerRadius) * 0.3)}px`)
				.style('text-shadow', '0px 0px 2px rgba(0,0,0,0.8)')
				.text((d) => {
					// Only show label if the arc is large enough
					const arcLength = (d.endAngle - d.startAngle) * labelRadius;
					const textLength =
						d.data.name.length * (Math.max(8, (outerRadius - innerRadius) * 0.3) * 0.6);

					if (arcLength > textLength) {
						return d.data.name;
					}
					return '';
				});
		}
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
<div bind:this={chartContainer} class="layered-pie-chart-container"></div>

<style>
	.layered-pie-chart-container {
		position: relative;
		display: flex;
		justify-content: center;
		align-items: center;
	}
</style>
