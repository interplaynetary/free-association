<script lang="ts">
	import { getColorForNameHash, getColorForUserId } from '$lib/utils/colorUtils';
	import { pie, arc } from 'd3-shape';

	// Collective node data interface
	interface CollectiveNodeData {
		id: string;
		name: string;
		weight: number;
		contributors: string[];
		contributorWeights: Record<string, number>;
		hasChildren: boolean;
	}

	// Node dimensions interface
	interface Dimensions {
		x0: number;
		y0: number;
		x1: number;
		y1: number;
	}

	// Component props
	let { node, dimensions } = $props<{
		node: CollectiveNodeData;
		dimensions: Dimensions;
	}>();

	// Calculate node size for styling
	const nodeWidth = $derived(dimensions.x1 - dimensions.x0);
	const nodeHeight = $derived(dimensions.y1 - dimensions.y0);
	const nodeSizeRatio = $derived(Math.min(nodeWidth, nodeHeight) * 100);

	// Split node name for display
	const segments = $derived(node.name.split(/(?=[A-Z][^A-Z])/g));

	// Calculate optimal font size
	const longestSegment = $derived(
		segments.reduce(
			(longest: string, segment: string) => (segment.length > longest.length ? segment : longest),
			''
		)
	);

	const fontSize = $derived(() => {
		const availableWidth = nodeWidth * 0.9 * 400;
		const availableHeight = nodeHeight * 0.9 * 400;
		const charWidth = 0.55;
		const lineHeight = 1.15;

		const maxFontSizeForWidth = (availableWidth * 0.95) / (longestSegment.length * charWidth * 16);
		const maxFontSizeForHeight = (availableHeight * 0.95) / (segments.length * lineHeight * 16);

		const calculatedSize = Math.min(maxFontSizeForWidth, maxFontSizeForHeight);
		return Math.max(0.4, Math.min(3.5, calculatedSize * 0.95));
	});

	// Check if this is the only child (full container)
	const isOnlyChild = $derived(nodeWidth >= 0.999 && nodeHeight >= 0.999);

	// Responsive padding
	const adaptivePadding = $derived(Math.max(1, Math.min(8, nodeSizeRatio * 0.1)));

	// Visibility factor for small nodes
	const visibilityFactor = $derived(Math.min(1, Math.max(0, (nodeSizeRatio - 5) / 7)));

	// Check if node has contributors
	const hasContributors = $derived(node.contributors.length > 0);

	// Create pie chart data for contributors
	const pieData = $derived(() => {
		if (!hasContributors) return [];

		const data = node.contributors.map((id: string) => ({
			id,
			value: node.contributorWeights[id] || 1 / node.contributors.length
		}));

		const pieGenerator = pie<any>()
			.value((d: any) => d.value)
			.sort(null);

		return pieGenerator(data);
	});

	// Arc generator for pie chart
	const arcPath = $derived(() => {
		const radius = 18;
		return arc<any>().innerRadius(0).outerRadius(radius);
	});

	// Calculate button positioning for pie chart
	const titleWidthPercent = $derived(
		((longestSegment.length * fontSize() * 0.6) / (nodeWidth * 100)) * 100
	);
	const titleEndPercent = $derived(50 + titleWidthPercent / 2);
	const buttonCenterPercent = $derived(titleEndPercent + (100 - titleEndPercent) / 2);
	const availableSpacePercent = $derived(100 - titleEndPercent);
	const buttonSizePercent = $derived(Math.min(availableSpacePercent * 0.6, 15));
</script>

<div
	class="collective-node"
	style="
    background-color: {getColorForNameHash(node.name)};
    border: {isOnlyChild ? 'none' : '1px solid #fff'};
    width: 100%;
    height: 100%;
    overflow: hidden;
    box-sizing: border-box;
  "
>
	<div
		class="collective-node-content"
		style="
      width: 100%;
      height: 100%;
      display: flex;
      align-items: center;
      justify-content: center;
      padding: {adaptivePadding}px;
      box-sizing: border-box;
      position: relative;
    "
	>
		<!-- Static Node Title -->
		<div
			class="node-title-area"
			style="
        display: flex;
        align-items: center;
        justify-content: center;
        position: absolute;
        top: 50%;
        left: 50%;
        transform: translate(-50%, -50%);
        max-width: 90%;
        z-index: 2;
      "
		>
			<div class="node-title" style="font-size: {fontSize()}rem;" title={node.name}>
				{#each segments as segment}
					<span class="title-segment">{segment}</span>
				{/each}
			</div>
		</div>

		<!-- Static Collective Recognition Info -->
		{#if visibilityFactor > 0.1 && hasContributors}
			<div
				class="collective-info"
				style="
					position: absolute;
					top: 8px;
					left: 8px;
					background: rgba(0, 0, 0, 0.6);
					color: white;
					padding: 4px 8px;
					border-radius: 4px;
					font-size: 10px;
					z-index: 3;
					opacity: {visibilityFactor * 0.8};
				"
			>
				Weight: {(node.weight * 100).toFixed(1)}%
			</div>

			<!-- Static Contributor Pie Chart -->
			<svg
				class="contributor-pie-chart"
				style="
            position: absolute;
            opacity: {visibilityFactor};
            top: 50%; 
            left: {buttonCenterPercent}%;
            transform: translate(-50%, -50%);
            width: {buttonSizePercent}%;
            height: {buttonSizePercent}%;
				"
				viewBox="-22 -22 44 44"
			>
				{#each pieData() as segment}
					<path d={arcPath()(segment)} fill={getColorForUserId(segment.data.id)} />
				{/each}

				<!-- Center indicator -->
				<circle
					cx={0}
					cy={0}
					r={4}
					fill="rgba(255, 255, 255, 0.9)"
					stroke="rgba(0, 0, 0, 0.3)"
					stroke-width="1"
				/>
			</svg>
		{/if}

		<!-- Static Children Indicator -->
		{#if node.hasChildren}
			<div
				class="children-indicator"
				style="
							position: absolute;
					bottom: 8px;
					right: 8px;
					background: rgba(0, 0, 0, 0.6);
					color: white;
					padding: 2px 6px;
					border-radius: 2px;
					font-size: 10px;
					z-index: 3;
					opacity: {visibilityFactor * 0.8};
				"
			>
				📁
			</div>
		{/if}
	</div>
</div>

<style>
	.collective-node {
		border-radius: 2px;
		position: relative;
	}

	.collective-node-content {
		text-align: center;
	}

	.node-title-area {
		user-select: none;
		pointer-events: none;
	}

	.node-title {
		color: rgba(0, 0, 0, 0.8);
		text-shadow:
			0px 0px 3px rgba(255, 255, 255, 0.8),
			0px 0px 2px rgba(255, 255, 255, 0.6);
		font-weight: 500;
		word-break: break-word;
		overflow-wrap: break-word;
		width: 100%;
		max-width: 100%;
		overflow: hidden;
		text-align: center;
		line-height: 1.1;
	}

	.title-segment {
		line-height: 1.1;
		display: block;
	}

	.contributor-pie-chart {
		border-radius: 4px;
	}

	.collective-info {
		font-family: monospace;
		font-weight: 500;
		letter-spacing: 0.5px;
	}

	.children-indicator {
		font-family: monospace;
		font-weight: 500;
	}
</style>
