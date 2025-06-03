<script lang="ts">
	import Parent from '$lib/components/Parent.svelte';
	import Bar from '$lib/components/Bar.svelte';
	import { userSogf, userTree, recalculateFromTree, providerShares } from '$lib/state.svelte';
	import { derived } from 'svelte/store';
	import { onMount } from 'svelte';

	// Reactive variable for mobile detection
	let isMobile = $state(false);
	let mediaQuery: MediaQueryList;

	// Create reactive derived store from userSogf
	const barSegments = derived(userSogf, ($sogf) => {
		if (!$sogf || Object.keys($sogf).length === 0) {
			return [];
		}

		// Transform SOGF data into segments for Bar
		return Object.entries($sogf)
			.filter(([_, value]) => value > 0) // Only include non-zero values
			.map(([id, value]) => ({
				id,
				value: value * 100 // Convert from decimal to percentage
			}))
			.sort((a, b) => b.value - a.value); // Sort by value descending
	});

	// Create reactive derived store from providerShares
	const providerSegments = derived(providerShares, ($providerShares) => {
		console.log('[UI] providerShares changed:', $providerShares);

		if (!$providerShares || Object.keys($providerShares).length === 0) {
			console.log('[UI] No provider shares data for segments');
			return [];
		}

		// Transform provider shares data into segments for Bar
		const segments = Object.entries($providerShares)
			.filter(([_, value]) => value > 0) // Only include non-zero values
			.map(([id, value]) => ({
				id,
				value: value * 100 // Convert from decimal to percentage
			}))
			.sort((a, b) => b.value - a.value); // Sort by value descending

		console.log('[UI] Generated provider segments:', segments);
		return segments;
	});

	// Ensure SOGF is calculated if we have a tree but no SOGF
	onMount(() => {
		const tree = $userTree;
		const sogf = $userSogf;

		if (tree && (!sogf || Object.keys(sogf).length === 0)) {
			recalculateFromTree();
		}

		// Set up media query listener for responsive behavior
		mediaQuery = window.matchMedia('(max-width: 768px)');

		// Function to handle media query changes
		function handleMediaChange(e: MediaQueryListEvent | MediaQueryList) {
			isMobile = e.matches;
		}

		// Set initial value
		handleMediaChange(mediaQuery);

		// Listen for changes
		mediaQuery.addEventListener('change', handleMediaChange);

		// Cleanup listener on unmount
		return () => {
			mediaQuery.removeEventListener('change', handleMediaChange);
		};
	});
</script>

<div class="layout">
	<div class="parent">
		<Parent />
	</div>
	<div class="bars">
		<div class="bar-group" class:vertical={!isMobile}>
			<div class="bar-label">
				{#if isMobile}
					your<br />recognition
				{:else}
					YR
				{/if}
			</div>
			<div class="bar-area">
				{#if $barSegments.length > 0}
					<Bar
						segments={$barSegments}
						width="100%"
						height="100%"
						vertical={!isMobile}
						showLabels={false}
						showValues={false}
						rounded={false}
					/>
				{:else}
					<div class="placeholder">
						<p>
							You have not yet recognized any contributors! Adding contributors to a node makes it a
							contribution!
						</p>
					</div>
				{/if}
			</div>
		</div>
		<div class="bar-group" class:vertical={!isMobile}>
			<div class="bar-label">
				{#if isMobile}
					mutual<br />recognition
				{:else}
					MR
				{/if}
			</div>
			<div class="bar-area">
				{#if $providerSegments.length > 0}
					<Bar
						segments={$providerSegments}
						width="100%"
						height="100%"
						vertical={!isMobile}
						showLabels={false}
						showValues={false}
						rounded={false}
					/>
				{:else}
					<div class="placeholder">
						<p>You don't have any mutual contributors yet!</p>
					</div>
				{/if}
			</div>
		</div>
	</div>
</div>

<style>
	:global(body) {
		margin: 0;
		padding: 0;
		height: 100%;
		overflow: hidden;
	}

	.layout {
		display: grid;
		grid-template-columns: 9fr 1fr;
		gap: 0.5rem;
		width: 100%;
		height: 100%;
		max-height: 100%;
		overflow: hidden;
	}

	.parent,
	.bars {
		width: 100%;
		height: 100%;
		overflow: auto;
	}

	.bars {
		display: flex;
		flex-direction: row;
		gap: 0.5rem;
	}

	.bar-group {
		flex: 1;
		display: grid;
		min-height: 0;
	}

	/* Horizontal layout */
	.bar-group {
		grid-template-columns: auto 1fr;
		gap: 0.75rem;
		align-items: center;
		height: 2rem;
	}

	/* Vertical layout */
	.bar-group.vertical {
		grid-template-columns: 1fr;
		grid-template-rows: auto 1fr;
		gap: 0.25rem;
		height: 100%;
		min-height: 0;
		max-height: 100%;
		overflow: hidden;
		display: flex;
		flex-direction: column;
		width: 2rem;
	}

	.bar-group.vertical .bar-area {
		flex: 1;
		order: 1;
		display: flex;
		align-items: flex-end;
		width: 100%;
	}

	.bar-group.vertical .bar-label {
		order: 2;
		font-size: min(0.5em, 1vw);
		padding: 0 0.25rem;
		max-width: 100%;
		text-align: center;
	}

	.bar-area {
		height: 100%;
		min-width: 0;
		min-height: 0;
	}

	.bar-label {
		font-size: min(0.6em, 1.2vh);
		color: #666;
		text-transform: uppercase;
		letter-spacing: 0.05em;
		font-weight: 500;
		line-height: 1.1;
		white-space: nowrap;
		overflow: hidden;
		text-overflow: ellipsis;
	}

	.placeholder {
		height: 100%;
		display: flex;
		align-items: center;
		justify-content: center;
		text-align: center;
		color: #666;
		font-size: 0.9em;
		padding: 1rem;
		background: #f5f5f5;
		border-radius: 4px;
	}

	/* Responsive layout for mobile */
	@media (max-width: 768px) {
		.layout {
			grid-template-columns: 1fr;
			grid-template-rows: 1fr auto;
		}

		.bars {
			flex-direction: column;
			height: auto;
			gap: 1rem;
		}

		.placeholder {
			padding: 0.5rem;
			font-size: 0.8em;
		}
	}
</style>
