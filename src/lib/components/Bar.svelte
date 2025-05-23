<script context="module">
	// Helper function to get a color based on index

	// Helper function to calculate border radius for each segment
	function getSegmentBorderRadius(
		index: number,
		total: number,
		isVertical: boolean = false
	): string {
		if (total === 1) return 'inherit';

		if (isVertical) {
			// Vertical layout: stack top to bottom
			if (index === 0) {
				return '999px 999px 0 0'; // Top rounded
			} else if (index === total - 1) {
				return '0 0 999px 999px'; // Bottom rounded
			}
			return '0';
		} else {
			// Horizontal layout: stack left to right
			if (index === 0) {
				return '999px 0 0 999px'; // Left side rounded
			} else if (index === total - 1) {
				return '0 999px 999px 0'; // Right side rounded
			}
			return '0';
		}
	}
</script>

<script lang="ts">
	import { nodesMap } from '$lib/state.svelte';
	import { getColorForUserId } from '$lib/utils/colorUtils';

	// Define the interface for bar segments
	interface BarSegment {
		id: string;
		value: number; // Value as percentage (0-100)
	}

	// Define component props using Svelte 5 $props
	let {
		segments = [],
		height = '100%',
		width = '100%',
		vertical = false,
		rounded = false,
		showLabels = false,
		showValues = false,
		backgroundColor = '#e0e0e0'
	} = $props<{
		segments: BarSegment[];
		height?: string;
		width?: string;
		vertical?: boolean;
		rounded?: boolean;
		showLabels?: boolean;
		showValues?: boolean;
		backgroundColor?: string;
	}>();

	// Calculate total of all segment values
	const totalValue = $derived(
		segments.reduce((sum: number, segment: BarSegment) => sum + segment.value, 0)
	);

	// Normalize segment values and add names/colors
	const normalizedSegments = $derived(
		segments.map((segment: BarSegment) => {
			const node = $nodesMap[segment.id];
			const name = node ? node.name : segment.id.substring(0, 6);
			return {
				...segment,
				normalizedValue: totalValue ? (segment.value / totalValue) * 100 : 0,
				label: name,
				color: getColorForUserId(segment.id)
			};
		})
	);

	// Get CSS border-radius value based on rounded prop and height
	const borderRadius = $derived(rounded ? height : '0');
</script>

<div
	class="stacked-bar"
	class:vertical
	style:height
	style:width
	style:background-color={backgroundColor}
	style:border-radius={borderRadius}
>
	{#each normalizedSegments as segment, i}
		{#if segment.normalizedValue > 0}
			<div
				class="bar-segment"
				style:width={vertical ? '100%' : `${segment.normalizedValue}%`}
				style:height={vertical ? `${segment.normalizedValue}%` : '100%'}
				style:background-color={segment.color}
				style:border-radius={getSegmentBorderRadius(i, normalizedSegments.length, vertical)}
				data-id={segment.id}
				data-value={segment.value}
			>
				{#if showLabels && segment.label}
					<span class="segment-label" class:small={segment.normalizedValue < 10}>
						{segment.label}
					</span>
				{/if}
				{#if showValues}
					<span class="segment-value" class:small={segment.normalizedValue < 10}>
						{segment.value.toFixed(1)}%
					</span>
				{/if}
			</div>
		{/if}
	{/each}
</div>

<style>
	.stacked-bar {
		display: flex;
		overflow: hidden;
		width: 100%;
		box-sizing: border-box;
	}

	.stacked-bar.vertical {
		flex-direction: column;
		height: 100%;
	}

	.bar-segment {
		height: 100%;
		transition: width 0.3s ease;
		position: relative;
		display: flex;
		align-items: center;
		justify-content: center;
		overflow: hidden;
		min-width: 1px;
	}

	.segment-label,
	.segment-value {
		color: #000;
		font-size: 0.8rem;
		text-shadow: 0 1px 0 rgba(255, 255, 255, 0.4);
		white-space: nowrap;
		padding: 0 4px;
		overflow: hidden;
		text-overflow: ellipsis;
		font-weight: 500;
	}

	.segment-label.small,
	.segment-value.small {
		transform: scale(0.8);
		opacity: 0.9;
	}
</style>
