<script lang="ts">
	import { onMount, onDestroy, afterUpdate } from 'svelte';
	import * as d3 from 'd3';

	// Define interfaces for our data structures
	interface DegreeInfo {
		level: number;
		title: string;
		description: string;
		color: string;
		nodeCount: number;
	}

	interface Node extends d3.SimulationNodeDatum {
		id: string;
		level: number;
		radius: number;
		color: string;
		label: string | null;
		x?: number;
		y?: number;
	}

	interface Link extends d3.SimulationLinkDatum<Node> {
		source: string | Node;
		target: string | Node;
		level: number;
		sourceLevel: number;
		targetLevel: number;
		weight: number;
		strength: number;
	}

	let svgRef: SVGSVGElement | null = null;
	let activeLevel = 0;
	let windowDimensions = { width: 800, height: 600 };
	let isTransitioning = false;

	const degreeInfo: DegreeInfo[] = [
		{
			level: 0,
			title: 'You - The Center',
			description: 'You are the center of your network of relationships',
			color: '#ff5252',
			nodeCount: 1
		},
		{
			level: 1,
			title: 'First Degree Connections',
			description: 'Direct contributors - friends, colleagues, local community',
			color: '#7986cb',
			nodeCount: 6
		},
		{
			level: 2,
			title: 'Second Degree Connections',
			description: 'Contributors to your contributors - expanding network',
			color: '#4db6ac',
			nodeCount: 12
		},
		{
			level: 3,
			title: 'Third Degree: Communities',
			description: 'Entire communities and networks - hundreds of connections',
			color: '#aed581',
			nodeCount: 24
		},
		{
			level: 4,
			title: 'Fourth Degree: Regional',
			description: 'Regional networks - thousands of potential connections',
			color: '#ffd54f',
			nodeCount: 48
		},
		{
			level: 5,
			title: 'Fifth Degree: National',
			description: 'National/continental networks - millions of connections',
			color: '#ff8a65',
			nodeCount: 96
		},
		{
			level: 6,
			title: 'Sixth Degree: Global',
			description: 'Global reach - access to worldwide surplus capacity',
			color: '#ba68c8',
			nodeCount: 192
		}
	];

	function generateNetworkData(level: number): { nodes: Node[]; links: Link[] } {
		const nodes: Node[] = [];
		const links: Link[] = [];
		nodes.push({
			id: 'You',
			level: 0,
			radius: 16,
			color: degreeInfo[0].color,
			label: 'You'
		});
		if (level === 0) return { nodes, links };
		for (let l = 1; l <= level; l++) {
			const count = degreeInfo[l].nodeCount;
			const baseColor = degreeInfo[l].color;
			for (let i = 0; i < count; i++) {
				const isLabelNode = i === 0;
				nodes.push({
					id: `L${l}-${i}`,
					level: l,
					radius: isLabelNode ? Math.max(6, 14 - l * 1.4) : Math.max(4, 12 - l * 1.5),
					color: baseColor,
					label: isLabelNode ? `Degree ${l}` : null
				});
			}
		}
		for (let l = 1; l <= level; l++) {
			const currentLevelNodes = nodes.filter((n) => n.level === l);
			const prevLevelNodes = nodes.filter((n) => n.level === l - 1);
			currentLevelNodes.forEach((node) => {
				const connectionCount = Math.min(
					prevLevelNodes.length,
					1 + Math.floor(Math.random() * Math.min(3, prevLevelNodes.length))
				);
				const shuffled = [...prevLevelNodes].sort(() => 0.5 - Math.random());
				const selectedNodes = shuffled.slice(0, connectionCount);
				const rawWeights = selectedNodes.map(() => Math.random() + 0.3);
				const totalWeight = rawWeights.reduce((a, b) => a + b, 0);
				selectedNodes.forEach((target, idx) => {
					const normalizedWeight = rawWeights[idx] / totalWeight;
					links.push({
						source: node.id,
						target: target.id,
						level: l,
						sourceLevel: l,
						targetLevel: target.level,
						weight: normalizedWeight,
						strength: 0.2 / l
					});
				});
			});
		}
		return { nodes, links };
	}

	function updateDimensions() {
		const containerWidth = Math.min(window.innerWidth - 40, 900);
		const containerHeight = Math.min(window.innerHeight - 200, 600);
		windowDimensions = { width: containerWidth, height: containerHeight };
	}

	let resizeHandler: (() => void) | null = null;

	onMount(() => {
		updateDimensions();
		resizeHandler = () => updateDimensions();
		window.addEventListener('resize', resizeHandler);
		return () => {
			if (resizeHandler) window.removeEventListener('resize', resizeHandler);
		};
	});

	let transitionTimer: ReturnType<typeof setTimeout> | null = null;

	$: if (activeLevel !== undefined) {
		isTransitioning = true;
		if (transitionTimer) clearTimeout(transitionTimer);
		transitionTimer = setTimeout(() => {
			isTransitioning = false;
		}, 700);
	}

	afterUpdate(() => {
		if (!svgRef) return;
		const { width, height } = windowDimensions;
		const margin = { top: 60, right: 20, bottom: 90, left: 20 };
		const graphWidth = width - margin.left - margin.right;
		const graphHeight = height - margin.top - margin.bottom;
		const centerX = graphWidth / 2;
		const centerY = graphHeight / 2;
		d3.select(svgRef).selectAll('*').remove();
		const svg = d3
			.select(svgRef)
			.attr('width', width)
			.attr('height', height)
			.append('g')
			.attr('transform', `translate(${margin.left},${margin.top})`);
		const gridSize = 20;
		const defs = svg.append('defs');
		const pattern = defs
			.append('pattern')
			.attr('id', 'grid-pattern')
			.attr('width', gridSize)
			.attr('height', gridSize)
			.attr('patternUnits', 'userSpaceOnUse');
		pattern
			.append('path')
			.attr('d', `M ${gridSize} 0 L 0 0 0 ${gridSize}`)
			.attr('fill', 'none')
			.attr('stroke', '#f0f0f0')
			.attr('stroke-width', 1);
		svg
			.append('rect')
			.attr('width', graphWidth)
			.attr('height', graphHeight)
			.attr('fill', 'white')
			.attr('fill-opacity', 0.5)
			.attr('stroke', '#e2e8f0')
			.attr('stroke-width', 1)
			.attr('rx', 8)
			.attr('ry', 8);
		svg
			.append('text')
			.attr('x', centerX)
			.attr('y', -30)
			.attr('text-anchor', 'middle')
			.attr('class', 'text-xl font-bold text-gray-800')
			.text("The Power of Six Degrees: Sharing Humanity's Cooperative Wealth");
		const data = generateNetworkData(activeLevel);
		const linksBySource: Record<string, Link[]> = {};
		data.links.forEach((link) => {
			const sourceId = typeof link.source === 'string' ? link.source : link.source.id;
			if (!linksBySource[sourceId]) linksBySource[sourceId] = [];
			linksBySource[sourceId].push(link);
		});
		for (let i = 1; i <= 6; i++) {
			const sourceColor = degreeInfo[i].color;
			const targetColor = degreeInfo[i - 1].color;
			const gradient = defs
				.append('linearGradient')
				.attr('id', `link-gradient-${i}`)
				.attr('gradientUnits', 'userSpaceOnUse');
			gradient.append('stop').attr('offset', '0%').attr('stop-color', sourceColor);
			gradient.append('stop').attr('offset', '100%').attr('stop-color', targetColor);
		}
		const simulation = d3
			.forceSimulation<Node, Link>(data.nodes)
			.force(
				'link',
				d3
					.forceLink<Node, Link>(data.links)
					.id((d) => d.id)
					.distance((d) => 40 + (d.source as Node).level * 20)
					.strength((d) => d.weight * d.strength * 2)
			)
			.force(
				'charge',
				d3.forceManyBody<Node>().strength((d) => -150 - (6 - d.level) * 30)
			)
			.force('center', d3.forceCenter<Node>(centerX, centerY))
			.force(
				'collide',
				d3.forceCollide<Node>().radius((d) => d.radius + 4)
			)
			.force('x', d3.forceX<Node>(centerX).strength(0.05))
			.force('y', d3.forceY<Node>(centerY).strength(0.05));
		const link = svg
			.append('g')
			.selectAll<SVGLineElement, Link>('line')
			.data(data.links)
			.enter()
			.append('line')
			.attr('stroke', (d) => `url(#link-gradient-${d.sourceLevel})`)
			.attr('stroke-opacity', 0.7)
			.attr('stroke-width', (d) => Math.max(1, d.weight * 10));
		const nodeGroups = svg
			.append('g')
			.selectAll<SVGGElement, Node>('g')
			.data(data.nodes)
			.enter()
			.append('g')
			.attr('class', (d) => `node-group level-${d.level}`)
			.call(
				d3
					.drag<SVGGElement, Node>()
					.on('start', dragstarted)
					.on('drag', dragged)
					.on('end', dragended) as any
			);
		nodeGroups
			.append('circle')
			.attr('r', (d) => d.radius)
			.attr('fill', (d) => d.color)
			.attr('stroke', '#fff')
			.attr('stroke-width', 1.5);
		nodeGroups
			.filter((d) => !!d.label)
			.append('text')
			.attr('dx', (d) => d.radius + 5)
			.attr('dy', '.35em')
			.attr('class', 'text-sm font-medium')
			.text((d) => d.label || '');
		const infoPanel = svg
			.append('g')
			.attr('transform', `translate(${centerX}, ${graphHeight + 35})`);
		defs
			.append('filter')
			.attr('id', 'drop-shadow')
			.append('feDropShadow')
			.attr('dx', '0')
			.attr('dy', '1')
			.attr('stdDeviation', '2')
			.attr('flood-opacity', '0.2');
		infoPanel
			.append('rect')
			.attr('x', -220)
			.attr('y', -25)
			.attr('width', 440)
			.attr('height', 50)
			.attr('rx', 8)
			.attr('ry', 8)
			.attr('fill', 'white')
			.attr('filter', 'url(#drop-shadow)')
			.attr('stroke', '#e2e8f0')
			.attr('stroke-width', 1);
		infoPanel
			.append('text')
			.attr('text-anchor', 'middle')
			.attr('y', -5)
			.attr('class', 'text-base font-semibold text-gray-800')
			.text(degreeInfo[activeLevel].title);
		infoPanel
			.append('text')
			.attr('text-anchor', 'middle')
			.attr('y', 20)
			.attr('class', 'text-sm text-gray-600')
			.text(degreeInfo[activeLevel].description);
		if (activeLevel > 0) {
			const legend = svg.append('g').attr('transform', `translate(${graphWidth - 160}, 20)`);
			legend
				.append('rect')
				.attr('width', 150)
				.attr('height', Math.min(activeLevel + 1, 7) * 25 + 60)
				.attr('rx', 8)
				.attr('ry', 8)
				.attr('fill', 'white')
				.attr('filter', 'url(#drop-shadow)')
				.attr('stroke', '#e2e8f0')
				.attr('stroke-width', 1);
			legend
				.append('text')
				.attr('x', 75)
				.attr('y', 20)
				.attr('text-anchor', 'middle')
				.attr('class', 'text-xs font-semibold')
				.text('Network Legend');
			for (let i = 0; i <= activeLevel; i++) {
				const legendItem = legend.append('g').attr('transform', `translate(15, ${i * 25 + 35})`);
				legendItem.append('circle').attr('r', 6).attr('fill', degreeInfo[i].color);
				legendItem
					.append('text')
					.attr('x', 15)
					.attr('y', 4)
					.attr('class', 'text-xs font-medium')
					.text(i === 0 ? 'You' : `Degree ${i}`);
			}
			if (activeLevel > 0) {
				const weightLegendY = Math.min(activeLevel + 1, 7) * 25 + 35;
				legend
					.append('text')
					.attr('x', 15)
					.attr('y', weightLegendY)
					.attr('class', 'text-xs font-medium')
					.text('Connection Strength:');
				const weights = [0.25, 0.5, 1];
				weights.forEach((weight, i) => {
					const y = weightLegendY + (i + 1) * 15;
					legend
						.append('line')
						.attr('x1', 15)
						.attr('y1', y)
						.attr('x2', 55)
						.attr('y2', y)
						.attr('stroke', '#666')
						.attr('stroke-width', weight * 8);
					legend
						.append('text')
						.attr('x', 65)
						.attr('y', y + 4)
						.attr('class', 'text-xs')
						.text(weight === 1 ? 'Strong' : weight === 0.5 ? 'Medium' : 'Weak');
				});
			}
		}
		const totalNodes = data.nodes.length;
		const totalConnections = data.links.length;
		const avgConnectionsPerNode = totalNodes > 1 ? (totalConnections / totalNodes).toFixed(1) : '0';
		const statsBox = svg.append('g').attr('transform', `translate(20, 20)`);
		statsBox
			.append('rect')
			.attr('width', 160)
			.attr('height', 90)
			.attr('rx', 8)
			.attr('ry', 8)
			.attr('fill', 'white')
			.attr('filter', 'url(#drop-shadow)')
			.attr('stroke', '#e2e8f0')
			.attr('stroke-width', 1);
		const statsContent = statsBox.append('g').attr('transform', 'translate(15, 20)');
		statsContent
			.append('text')
			.attr('class', 'text-xs font-medium')
			.text(`Degree Level: ${activeLevel}`);
		statsContent
			.append('text')
			.attr('y', 20)
			.attr('class', 'text-xs font-medium')
			.text(`Nodes: ${totalNodes}`);
		statsContent
			.append('text')
			.attr('y', 40)
			.attr('class', 'text-xs font-medium')
			.text(`Connections: ${totalConnections}`);
		statsContent
			.append('text')
			.attr('y', 60)
			.attr('class', 'text-xs font-medium')
			.text(`Avg. Connections: ${avgConnectionsPerNode}`);
		simulation.on('tick', () => {
			link
				.attr('x1', (d) => (d.source as Node).x || 0)
				.attr('y1', (d) => (d.source as Node).y || 0)
				.attr('x2', (d) => (d.target as Node).x || 0)
				.attr('y2', (d) => (d.target as Node).y || 0);
			nodeGroups.attr('transform', (d) => {
				const x = Math.max(d.radius, Math.min(graphWidth - d.radius, d.x || 0));
				const y = Math.max(d.radius, Math.min(graphHeight - d.radius, d.y || 0));
				return `translate(${x}, ${y})`;
			});
		});
		function dragstarted(event: d3.D3DragEvent<SVGGElement, Node, Node>) {
			if (!event.active) simulation.alphaTarget(0.3).restart();
			event.subject.fx = event.subject.x;
			event.subject.fy = event.subject.y;
		}
		function dragged(event: d3.D3DragEvent<SVGGElement, Node, Node>) {
			event.subject.fx = event.x;
			event.subject.fy = event.y;
		}
		function dragended(event: d3.D3DragEvent<SVGGElement, Node, Node>) {
			if (!event.active) simulation.alphaTarget(0);
			event.subject.fx = null;
			event.subject.fy = null;
		}
	});

	function handleSliderChange(e: Event) {
		const target = e.target as HTMLInputElement;
		activeLevel = parseInt(target.value);
	}
</script>

<div class="visualization-container">
	<div class="transition-opacity {isTransitioning ? 'opacity-70' : 'opacity-100'}">
		<svg bind:this={svgRef} class="visualization-svg"></svg>
	</div>
	<div class="controls-container">
		<div class="degree-indicator">
			<div class="degree-label">
				<div class="degree-color" style="background-color: {degreeInfo[activeLevel].color}"></div>
				<span class="degree-title"
					>Current: {activeLevel === 0 ? 'You (Center)' : `Degree ${activeLevel}`}</span
				>
			</div>
			<span class="network-depth-label">Network Depth</span>
		</div>
		<div class="slider-container">
			<input
				type="range"
				min="0"
				max="6"
				bind:value={activeLevel}
				on:input={handleSliderChange}
				class="slider-input"
			/>
			<div class="slider-ticks">
				{#each [0, 1, 2, 3, 4, 5, 6] as level}
					<div class="tick-mark">
						<div class="tick {level === activeLevel ? 'tick-active' : 'tick-inactive'}"></div>
						<span
							class="tick-label {level === activeLevel
								? 'tick-label-active'
								: 'tick-label-inactive'}">{level}</span
						>
					</div>
				{/each}
			</div>
		</div>
	</div>
	<div class="description">
		<p class="description-text">
			This visualization demonstrates how connections multiply across six degrees of separation,
			with weighted links showing the varying strength of relationships in the network. Each node's
			outgoing connections are normalized to sum to 1, representing the distribution of mutual
			recognition.
		</p>
	</div>
</div>

<style>
	.visualization-container {
		display: flex;
		flex-direction: column;
		align-items: center;
		background-color: #f9fafb;
		padding: 1.5rem;
		border-radius: 0.75rem;
		box-shadow:
			0 4px 6px -1px rgba(0, 0, 0, 0.1),
			0 2px 4px -1px rgba(0, 0, 0, 0.06);
	}

	.visualization-svg {
		width: 100%;
		border-radius: 0.5rem;
		box-shadow:
			0 1px 3px 0 rgba(0, 0, 0, 0.1),
			0 1px 2px 0 rgba(0, 0, 0, 0.06);
	}

	.transition-opacity {
		transition: opacity 700ms;
	}

	.opacity-70 {
		opacity: 0.7;
	}

	.opacity-100 {
		opacity: 1;
	}

	.controls-container {
		margin-top: 2rem;
		width: 100%;
		max-width: 32rem;
		background-color: #ffffff;
		padding: 1rem;
		border-radius: 0.5rem;
		box-shadow:
			0 1px 3px 0 rgba(0, 0, 0, 0.1),
			0 1px 2px 0 rgba(0, 0, 0, 0.06);
	}

	.degree-indicator {
		display: flex;
		justify-content: space-between;
		align-items: center;
		margin-bottom: 0.75rem;
	}

	.degree-color {
		width: 1rem;
		height: 1rem;
		border-radius: 9999px;
	}

	.degree-label {
		display: flex;
		align-items: center;
		gap: 0.5rem;
	}

	.degree-title {
		font-weight: 600;
		color: #1f2937;
	}

	.network-depth-label {
		font-size: 0.875rem;
		font-weight: 500;
		color: #4b5563;
	}

	.slider-container {
		position: relative;
	}

	.slider-input {
		width: 100%;
		height: 0.5rem;
		background-color: #e5e7eb;
		border-radius: 0.375rem;
		-webkit-appearance: none;
		appearance: none;
		cursor: pointer;
	}

	.slider-input::-webkit-slider-thumb {
		-webkit-appearance: none;
		appearance: none;
		width: 1rem;
		height: 1rem;
		border-radius: 50%;
		background: #3b82f6;
		cursor: pointer;
	}

	.slider-ticks {
		display: flex;
		justify-content: space-between;
		margin-top: 0.5rem;
		padding: 0 0.25rem;
	}

	.tick-mark {
		display: flex;
		flex-direction: column;
		align-items: center;
	}

	.tick {
		width: 0.25rem;
		height: 0.75rem;
	}

	.tick-active {
		background-color: #3b82f6;
	}

	.tick-inactive {
		background-color: #9ca3af;
	}

	.tick-label {
		font-size: 0.75rem;
		margin-top: 0.25rem;
	}

	.tick-label-active {
		font-weight: 700;
		color: #3b82f6;
	}

	.tick-label-inactive {
		color: #4b5563;
	}

	.description {
		margin-top: 1rem;
		text-align: center;
		max-width: 42rem;
	}

	.description-text {
		font-size: 0.875rem;
		color: #4b5563;
	}
</style>
