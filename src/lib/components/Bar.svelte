<script context="module">
	// Helper function to get a color based on index

	// Helper function to calculate border radius for each segment
	function getSegmentBorderRadius(
		index: number,
		total: number,
		isVertical = false,
		rounded = false
	) {
		if (!rounded || total === 1) return rounded ? 'inherit' : '0';

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
	import { userNamesCache, getUserName } from '$lib/state.svelte';
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
		showLabelsOnSelect = false,
		showLabelsAboveOnSelect = false,
		showValues = false,
		backgroundColor = '#e0e0e0'
	} = $props<{
		segments: BarSegment[];
		height?: string;
		width?: string;
		vertical?: boolean;
		rounded?: boolean;
		showLabels?: boolean;
		showLabelsOnSelect?: boolean;
		showLabelsAboveOnSelect?: boolean;
		showValues?: boolean;
		backgroundColor?: string;
	}>();

	// State for tracking selected/hovered segment
	let selectedSegmentId = $state<string | null>(null);
	// State for popup positioning
	let popupPosition = $state<{ x: number; y: number; show: boolean }>({ x: 0, y: 0, show: false });
	let barContainer: HTMLDivElement | undefined = $state();

	// Calculate total of all segment values
	const totalValue = $derived(
		segments.reduce((sum: number, segment: BarSegment) => sum + segment.value, 0)
	);

	// Normalize segment values and add names/colors
	const normalizedSegments = $derived(
		segments.map((segment: BarSegment) => {
			// Get name from reactive cache only
			let displayName = $userNamesCache[segment.id] || segment.id.substring(0, 8) + '...';

			return {
				...segment,
				normalizedValue: totalValue ? (segment.value / totalValue) * 100 : 0,
				label: displayName,
				color: getColorForUserId(segment.id)
			};
		})
	);

	// Trigger name lookups for uncached segments
	$effect(() => {
		segments.forEach((segment: BarSegment) => {
			if (!$userNamesCache[segment.id]) {
				getUserName(segment.id);
			}
		});
	});

	// Get CSS border-radius value based on rounded prop and height
	const borderRadius = $derived(rounded ? height : '0');

	// Handler functions for segment interaction
	function handleSegmentEnter(segmentId: string, event?: MouseEvent) {
		if (showLabelsOnSelect) {
			selectedSegmentId = segmentId;
		}

		if (showLabelsAboveOnSelect && event) {
			selectedSegmentId = segmentId;
			updatePopupPosition(event);
		}
	}

	function handleSegmentLeave() {
		if (showLabelsOnSelect || showLabelsAboveOnSelect) {
			selectedSegmentId = null;
			if (showLabelsAboveOnSelect) {
				popupPosition = { x: 0, y: 0, show: false };
			}
		}
	}

	function handleSegmentClick(segmentId: string, event?: MouseEvent) {
		if (showLabelsOnSelect) {
			selectedSegmentId = selectedSegmentId === segmentId ? null : segmentId;
		}

		if (showLabelsAboveOnSelect && event) {
			selectedSegmentId = selectedSegmentId === segmentId ? null : segmentId;
			if (selectedSegmentId) {
				updatePopupPosition(event);
			} else {
				popupPosition = { x: 0, y: 0, show: false };
			}
		}
	}

	function handleSegmentTouch(segmentId: string, event: TouchEvent) {
		if (showLabelsAboveOnSelect) {
			selectedSegmentId = segmentId;
			// Use the first touch point for positioning
			const touch = event.touches[0];
			if (touch) {
				const syntheticEvent = {
					clientX: touch.clientX,
					clientY: touch.clientY,
					target: event.target
				} as MouseEvent;
				updatePopupPosition(syntheticEvent);
			}
		}
	}

	// Update popup position based on mouse/touch event
	function updatePopupPosition(event: MouseEvent) {
		const segmentRect = (event.target as HTMLElement).getBoundingClientRect();

		// Use fixed positioning to escape container bounds
		const centerX = segmentRect.left + segmentRect.width / 2;
		const topY = segmentRect.top - 40; // Position above the segment with gap

		popupPosition = {
			x: centerX,
			y: topY,
			show: true
		};
	}

	// Function to determine if labels should be shown for a segment
	function shouldShowLabel(segmentId: string): boolean {
		if (showLabels) return true;
		if (showLabelsOnSelect && !showLabelsAboveOnSelect) return selectedSegmentId === segmentId;
		return false;
	}

	// Get the label for popup display - simple reactive variable
	let popupLabel = $state('');

	// Update popup label when relevant state changes
	$effect(() => {
		if (!selectedSegmentId || !showLabelsAboveOnSelect || !popupPosition.show) {
			popupLabel = '';
			return;
		}

		// Get the name directly from cache or fallback
		const cachedName = $userNamesCache[selectedSegmentId];
		popupLabel = cachedName || selectedSegmentId.substring(0, 8) + '...';
	});
</script>

<div
	bind:this={barContainer}
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
				class:interactive={showLabelsOnSelect || showLabelsAboveOnSelect}
				class:selected={(showLabelsOnSelect || showLabelsAboveOnSelect) &&
					selectedSegmentId === segment.id}
				style:width={vertical ? '100%' : `${segment.normalizedValue}%`}
				style:height={vertical ? `${segment.normalizedValue}%` : '100%'}
				style:background-color={segment.color}
				style:border-radius={getSegmentBorderRadius(
					i,
					normalizedSegments.length,
					vertical,
					rounded
				)}
				data-id={segment.id}
				data-value={segment.value}
				on:mouseenter={(event) => handleSegmentEnter(segment.id, event)}
				on:mouseleave={handleSegmentLeave}
				on:click={(event) => handleSegmentClick(segment.id, event)}
				on:touchstart={(event) => handleSegmentTouch(segment.id, event)}
				role={showLabelsOnSelect || showLabelsAboveOnSelect ? 'button' : undefined}
				tabindex={showLabelsOnSelect || showLabelsAboveOnSelect ? 0 : undefined}
			>
				{#if shouldShowLabel(segment.id) && segment.label}
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

<!-- Popup for showLabelsAboveOnSelect - rendered outside bar container -->
{#if showLabelsAboveOnSelect && popupPosition.show && popupLabel}
	<div class="segment-popup" style:left="{popupPosition.x}px" style:top="{popupPosition.y}px">
		<div class="popup-content">
			{popupLabel}
		</div>
		<div class="popup-arrow"></div>
	</div>
{/if}

<style>
	.stacked-bar {
		display: flex;
		overflow: hidden;
		width: 100%;
		box-sizing: border-box;
		position: relative; /* Enable absolute positioning for popup */
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
		min-height: 25px;
	}

	.bar-segment.interactive {
		cursor: pointer;
		transition: all 0.2s ease;
	}

	.bar-segment.interactive:hover {
		opacity: 0.8;
		transform: scale(1.02);
		box-shadow: 0 2px 4px rgba(0, 0, 0, 0.2);
		z-index: 1;
	}

	.bar-segment.selected {
		opacity: 0.9;
		transform: scale(1.01);
		box-shadow: 0 1px 3px rgba(0, 0, 0, 0.15);
		z-index: 1;
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

	/* Popup styles */
	.segment-popup {
		position: fixed;
		z-index: 10000;
		pointer-events: none;
		transform: translateX(-50%);
		animation: popupFadeIn 0.15s ease-out;
	}

	.popup-content {
		background: rgba(255, 255, 255, 0.95);
		color: #1f2937;
		padding: 6px 10px;
		border-radius: 6px;
		font-size: 0.75rem;
		font-weight: 500;
		white-space: nowrap;
		box-shadow: 0 4px 12px rgba(0, 0, 0, 0.15);
		backdrop-filter: blur(8px);
		border: 1px solid rgba(0, 0, 0, 0.1);
		max-width: 200px;
		overflow: hidden;
		text-overflow: ellipsis;
	}

	.popup-arrow {
		position: absolute;
		top: 100%;
		left: 50%;
		transform: translateX(-50%);
		width: 0;
		height: 0;
		border-left: 5px solid transparent;
		border-right: 5px solid transparent;
		border-top: 5px solid rgba(255, 255, 255, 0.95);
	}

	@keyframes popupFadeIn {
		from {
			opacity: 0;
			transform: translateX(-50%) translateY(-4px);
		}
		to {
			opacity: 1;
			transform: translateX(-50%) translateY(0);
		}
	}
</style>
