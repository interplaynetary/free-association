<script lang="ts">
	import Parent from '$lib/components/Parent.svelte';
	import Bar from '$lib/components/Bar.svelte';
	import Map from '$lib/components/Map.svelte';
	import Capacities from '$lib/components/Capacities.svelte';
	import Shares from '$lib/components/Shares.svelte';
	import { userSogf, userTree, generalShares } from '$lib/state/core.svelte';
	import { recalculateFromTree } from '$lib/state/calculations.svelte';
	import { globalState } from '$lib/global.svelte';
	import { derived } from 'svelte/store';
	import { onMount } from 'svelte';
	import { t, loading } from '$lib/translations';

	// Reactive view state
	const currentView = $derived(globalState.currentView);

	// Reactive variable for Bar component orientation
	let isBarVertical = $state(false);

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

	// Create reactive derived store from generalShares
	const providerSegments = derived(generalShares, ($generalShares) => {
		console.log('[UI] generalShares changed:', $generalShares);

		if (!$generalShares || Object.keys($generalShares).length === 0) {
			console.log('[UI] No provider shares data for segments');
			return [];
		}

		// Transform provider shares data into segments for Bar
		const segments = Object.entries($generalShares)
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

		// Set up media query for Bar component orientation
		const mediaQuery = window.matchMedia('(min-width: 769px)');
		
		function handleMediaChange(e: MediaQueryListEvent | MediaQueryList) {
			isBarVertical = e.matches; // true for desktop (vertical bars), false for mobile (horizontal bars)
			console.log('[Layout] Media query changed - isBarVertical:', isBarVertical, 'viewport width:', window.innerWidth);
		}

		handleMediaChange(mediaQuery);
		mediaQuery.addEventListener('change', handleMediaChange);

		return () => {
			mediaQuery.removeEventListener('change', handleMediaChange);
		};
	});
</script>

<div class="layout root-page" class:full-width={currentView !== 'tree'}>
	<div class="view-content">
		{#if currentView === 'tree'}
			<Parent />
		{:else if currentView === 'map'}
			{#key currentView}
				<Map fullHeight={true} />
			{/key}
		{:else if currentView === 'inventory'}
			<div class="inventory-view">
				<h2 class="text-center text-2xl font-bold">{$t('home.capacities')}</h2>
				<Capacities />

				<h2 class="text-center text-2xl font-bold">{$t('home.shares')}</h2>
				<Shares />
			</div>
		{/if}
	</div>

	{#if currentView === 'tree'}
	<div class="bars">
		{#key $loading}
		<div class="bar-group">
			<div
				class="bar-label"
				title={$t('home.your_recognition_description')}
			>
				{#if isBarVertical}
					{$t('home.your_recognition_abbr')}
				{:else}
					{@html $t('home.your_recognition').toLowerCase().replace(' ', '<br />')}
				{/if}
			</div>
			<div class="bar-area">
				{#if $barSegments.length > 0}
					<Bar
						segments={$barSegments}
						width="100%"
						height="100%"
						vertical={isBarVertical}
						showLabelsOnSelect={true}
						showValues={false}
						rounded={false}
					/>
				{:else}
					<div class="placeholder">
						<p>
							{$t('home.no_contributors')}
						</p>
					</div>
				{/if}
			</div>
		</div>
		<div class="bar-group">
			<div
				class="bar-label"
				title={$t('home.mutual_recognition_description')}
			>
				{#if isBarVertical}
					{$t('home.mutual_recognition_abbr')}
				{:else}
					{@html $t('home.mutual_recognition').toLowerCase().replace(' ', '<br />')}
				{/if}
			</div>
			<div class="bar-area">
				{#if $providerSegments.length > 0}
					<Bar
						segments={$providerSegments}
						width="100%"
						height="100%"
						vertical={isBarVertical}
						showLabelsOnSelect={true}
						showValues={false}
						rounded={false}
					/>
				{:else}
					<div class="placeholder">
						<p>{$t('home.no_mutual_contributors')}</p>
					</div>
				{/if}
			</div>
		</div>
		{/key}
	</div>
	{/if}
</div>

<style>
	/* Removed conflicting :global(body) styles - handled by layout */

	.layout {
		display: grid;
		grid-template-columns: 9fr 1fr;
		width: 100%;
		height: 100%;
		max-height: 100%;
		overflow: hidden;
		user-select: none;
	}

	/* Full-width layout when bars are hidden */
	.layout.full-width {
		grid-template-columns: 1fr;
	}

	/* Root page specific: ensure it doesn't scroll */
	.layout.root-page {
		overflow: hidden;
		height: 100%;
		max-height: 100%;
		position: relative;
	}

	.view-content,
	.bars {
		width: 100%;
		height: 100%;
		overflow: auto;
	}

	.bars {
		display: flex;
		gap: 0.5rem;
		padding: 0.5rem;
		width: 100%;
		height: 100%;
	}

	/* Mobile/Horizontal mode: bars stack vertically, each bar-group is horizontal */
	@media (max-width: 768px) {
		.bars {
			flex-direction: column;
			height: auto;
		}

		.bar-group {
			display: grid;
			grid-template-columns: auto 1fr;
			gap: 0.75rem;
			align-items: center;
			height: 2rem;
			width: 100%;
		}

		.bar-label {
			white-space: normal;
		}

		.bar-area {
			width: 100%;
		}
	}

	/* Desktop/Vertical mode: bars side by side, each bar-group is vertical */
	@media (min-width: 769px) {
		.bars {
			flex-direction: row;
		}

		.bar-group {
			display: flex;
			flex-direction: column;
			gap: 0.25rem;
			height: 100%;
			width: 2rem;
			min-height: 0;
			max-height: 100%;
			overflow: hidden;
		}

		.bar-area {
			flex: 1;
			order: 1;
			display: flex;
			align-items: flex-end;
			width: 100%;
		}

		.bar-label {
			order: 2;
			font-size: min(0.5em, 1vw);
			padding: 0 0.25rem;
			max-width: 100%;
			text-align: center;
		}
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
		overflow: hidden;
		text-overflow: ellipsis;
	}

	.placeholder {
		height: 100%;
		width: 100%;
		display: flex;
		align-items: center;
		justify-content: center;
		text-align: center;
		color: #666;
		background: #f5f5f5;
		border-radius: 4px;
		overflow: hidden;
	}

	.placeholder p {
		margin: 0;
		padding: 0.25rem;
		line-height: 1.2;
		word-break: break-word;
		hyphens: auto;
	}

	/* Horizontal placeholder (mobile) */
	@media (max-width: 768px) {
		.placeholder {
			padding: 0.5rem;
		}

		.placeholder p {
			font-size: clamp(0.5rem, 1.5vw, 0.75rem);
		}
	}

	/* Vertical placeholder (desktop) */
	@media (min-width: 769px) {
		.placeholder {
			writing-mode: vertical-rl;
			text-orientation: mixed;
			padding: 0.5rem 0.25rem;
		}

		.placeholder p {
			font-size: clamp(0.35rem, 0.8vw, 0.5rem);
			max-width: 100%;
		}
	}

	.inventory-view {
		padding: 1rem;
		overflow-y: auto;
		height: 100%;
	}

	.inventory-view h2 {
		margin: 1.5rem 0 1rem 0;
	}

	.inventory-view h2:first-child {
		margin-top: 0;
	}

	/* Additional mobile adjustments */
	@media (max-width: 768px) {
		.layout {
			grid-template-columns: 1fr;
			grid-template-rows: 1fr auto;
		}
	}
</style>
