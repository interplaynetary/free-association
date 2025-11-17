<script lang="ts">
	import { onMount } from 'svelte';
	import * as d3 from 'd3';

	let svgElement: SVGSVGElement;

	onMount(() => {
		// Load LOT font
		const link = document.createElement('link');
		link.href = 'https://fonts.cdnfonts.com/css/lot';
		link.rel = 'stylesheet';
		document.head.appendChild(link);

		const width = 600;
		const height = 600;
		const centerX = width / 2;
		const centerY = height / 2;

		// Clear any existing SVG content
		d3.select(svgElement).selectAll('*').remove();

		const svg = d3.select(svgElement).attr('width', width).attr('height', height);

		// SDG-inspired colors separated by temperature for better contrast
		const warmColors = [
			'#E5243B', // red
			'#C5192D', // dark red
			'#DD1367', // pink
			'#A21942', // dark magenta
			'#FF3A21', // orange-red
			'#FD6925', // orange
			'#FD9D24', // light orange
			'#DDA63A', // gold
			'#FCC30B', // yellow
			'#BF8B2E'  // brownish gold
		];

		const coolColors = [
			'#26BDE2', // cyan
			'#0A97D9', // blue
			'#00689D', // dark blue
			'#19486A', // navy
			'#4C9F38', // green
			'#3F7E44', // dark green
			'#56C02B'  // lime green
		];

		// Inner layer: scattered mix of warm and cool (alternating for diversity)
		const innerLayerColors = [
			warmColors[0],  // red
			coolColors[0],  // cyan
			warmColors[4],  // orange-red
			coolColors[4],  // green
			warmColors[2],  // pink
			coolColors[1],  // blue
			warmColors[8],  // yellow
			coolColors[5]   // dark green
		];

		// Outer layer: also scattered, but offset to contrast with inner layer
		// Since outer has 12 segments and inner has 8, they align at different angles
		// Start with cool where inner starts with warm to ensure juxtaposition
		const outerLayerColors = [
			coolColors[2],  // dark blue (contrasts with red above)
			coolColors[6],  // lime green
			warmColors[5],  // orange
			coolColors[3],  // navy
			warmColors[1],  // dark red
			warmColors[7],  // gold
			coolColors[0],  // cyan (contrasts with pink above)
			coolColors[4],  // green
			warmColors[6],  // light orange
			warmColors[3],  // dark magenta
			coolColors[1],  // blue
			warmColors[9]   // brownish gold
		];

		// Create data for TWO concentric layers with diverse yet juxtaposed colors
		const layers = [
			{
				innerRadius: 150,
				outerRadius: 190,
				data: d3.range(8).map((i) => ({
					value: 1,
					color: innerLayerColors[i]
				}))
			},
			{
				innerRadius: 200,
				outerRadius: 240,
				data: d3.range(12).map((i) => ({
					value: 1,
					color: outerLayerColors[i]
				}))
			}
		];

		const g = svg.append('g').attr('transform', `translate(${centerX},${centerY})`);

		// Add "FA" text in the center
		g.append('text')
			.attr('x', 0)
			.attr('y', 0)
			.attr('text-anchor', 'middle')
			.attr('dominant-baseline', 'middle')
			.attr('font-family', 'LOT, sans-serif')
			.attr('font-size', '120px')
			.attr('font-weight', 'bold')
			.attr('fill', '#5B92E5') // United Nations blue
			.style('opacity', 0)
			.text('FA')
			.transition()
			.duration(1000)
			.delay(800)
			.style('opacity', 1);

		// Create each concentric layer
		layers.forEach((layer, layerIndex) => {
			const pie = d3.pie<{ value: number; color: string }>()
				.value((d) => d.value)
				.padAngle(0.02)
				.sort(null);

			const arc = d3.arc()
				.innerRadius(layer.innerRadius)
				.outerRadius(layer.outerRadius)
				.cornerRadius(8); // Rounded edges

			const arcs = g
				.selectAll(`.arc-layer-${layerIndex}`)
				.data(pie(layer.data))
				.enter()
				.append('g')
				.attr('class', `arc-layer-${layerIndex}`);

			arcs
				.append('path')
				.attr('d', arc as any)
				.attr('fill', (d) => d.data.color)
				.attr('stroke', '#fff')
				.attr('stroke-width', 2)
				.style('opacity', 0)
				.transition()
				.duration(800)
				.delay((d, i) => i * 50 + layerIndex * 200)
				.style('opacity', 1);

			// Add hover effect
			arcs.selectAll('path')
				.on('mouseenter', function () {
					d3.select(this)
						.transition()
						.duration(200)
						.style('opacity', 0.8)
						.attr('transform', 'scale(1.05)');
				})
				.on('mouseleave', function () {
					d3.select(this)
						.transition()
						.duration(200)
						.style('opacity', 1)
						.attr('transform', 'scale(1)');
				});
		});
	});
</script>

<div class="flex flex-col items-center justify-center min-h-screen bg-gray-50 p-8">
	<div class="bg-white rounded-lg shadow-lg p-8">

		<svg bind:this={svgElement} class="mx-auto"></svg>
	</div>
</div>

