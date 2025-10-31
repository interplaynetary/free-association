<script lang="ts">
	import Parent from '$lib/components/Parent.svelte';
	import Bar from '$lib/components/Bar.svelte';
	import Map from '$lib/components/Map.svelte';
	import Capacities from '$lib/components/Capacities.svelte';
	import Shares from '$lib/components/Shares.svelte';
	// V5: Import from v5 stores - fully reactive, no manual recalculation needed!
	import { myRecognitionTreeStore, myRecognitionWeights, myMutualRecognition } from '$lib/commons/v5/stores.svelte';
	import { globalState } from '$lib/global.svelte';
	import { derived } from 'svelte/store';
	import { t, loading } from '$lib/translations';

	// Reactive view state
	const currentView = $derived(globalState.currentView);

	// V5: Create reactive derived store from myRecognitionWeights (replaces userSogf)
	// Recognition weights are automatically computed from the tree in v5!
	const barSegments = derived(myRecognitionWeights, ($weights) => {
		console.log('[📊 UI-YR] Recognition weights changed - generating segments for bar...');
		
		if (!$weights || Object.keys($weights).length === 0) {
			console.log('[📊 UI-YR] ❌ No recognition weights available');
			return [];
		}

		const totalEntries = Object.keys($weights).length;
		const nonZeroEntries = Object.values($weights).filter(v => v > 0).length;
		console.log(`[📊 UI-YR] Recognition weights has ${totalEntries} entries (${nonZeroEntries} non-zero)`);

		// Transform recognition weights into segments for Bar
		const segments = Object.entries($weights)
			.filter(([_, value]) => value > 0) // Only include non-zero values
			.map(([id, value]) => ({
				id,
				value: value * 100 // Convert from decimal to percentage
			}))
			.sort((a, b) => b.value - a.value); // Sort by value descending
		
		console.log(`[📊 UI-YR] ✅ Generated ${segments.length} segments for recognition bar:`);
		segments.forEach(seg => {
			console.log(`  • ${seg.id.slice(0, 20)}... → ${seg.value.toFixed(2)}%`);
		});
		
		return segments;
	});

	// V5: Create reactive derived store from myMutualRecognition (replaces generalShares)
	// Mutual recognition is automatically computed from recognition weights + network data in v5!
	const providerSegments = derived(myMutualRecognition, ($mutualRec) => {
		console.log('[📊 UI-MR] Mutual recognition changed - generating segments for bar...');

		if (!$mutualRec || Object.keys($mutualRec).length === 0) {
			console.log('[📊 UI-MR] ❌ No mutual recognition data available');
			return [];
		}

		const totalEntries = Object.keys($mutualRec).length;
		const nonZeroEntries = Object.values($mutualRec).filter(v => v > 0).length;
		console.log(`[📊 UI-MR] Mutual recognition has ${totalEntries} entries (${nonZeroEntries} non-zero)`);

		// Transform mutual recognition data into segments for Bar
		const segments = Object.entries($mutualRec)
			.filter(([_, value]) => value > 0) // Only include non-zero values
			.map(([id, value]) => ({
				id,
				value: value * 100 // Convert from decimal to percentage
			}))
			.sort((a, b) => b.value - a.value); // Sort by value descending

		console.log(`[📊 UI-MR] ✅ Generated ${segments.length} segments for mutual recognition bar:`);
		segments.forEach(seg => {
			console.log(`  • ${seg.id.slice(0, 20)}... → ${seg.value.toFixed(2)}%`);
		});
		
		return segments;
	});

	// V5: No manual recalculation needed! Everything is reactive 🎉
	// Recognition weights auto-update when tree changes
	// Mutual recognition auto-updates when recognition weights or network data changes
</script>

<div class="layout root-page" class:full-width={currentView !== 'tree'}>
	<div class="view-content">
		{#if currentView === 'tree'}
			<Parent />
		{:else if currentView === 'map'}
			<Map fullHeight={true} />
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
		{#key $loading}
		<div class="bars">
		<div class="bar-group">
			<div
				class="bar-label bar-label-yr"
				title={$t('home.your_recognition_description')}
			>
				<span class="label-mobile">{@html $t('home.your_recognition').toLowerCase().replace(' ', '<br />')}</span>
				<span class="label-desktop">{$t('home.your_recognition_abbr')}</span>
			</div>
			<div class="bar-area">
				{#if $barSegments.length > 0}
					<Bar
						segments={$barSegments}
						width="100%"
						height="100%"
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
				class="bar-label bar-label-mr"
				title={$t('home.mutual_recognition_description')}
			>
				<span class="label-mobile">{@html $t('home.mutual_recognition').toLowerCase().replace(' ', '<br />')}</span>
				<span class="label-desktop">{$t('home.mutual_recognition_abbr')}</span>
			</div>
			<div class="bar-area">
				{#if $providerSegments.length > 0}
					<Bar
						segments={$providerSegments}
						width="100%"
						height="100%"
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
		</div>
		{/key}
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
	}

	/* Mobile: Horizontal bars stacked vertically */
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

		.bar-area {
			height: 100%;
			width: 100%;
		}

		.bar-label {
			white-space: normal;
		}

		.label-mobile {
			display: inline;
		}

		.label-desktop {
			display: none;
		}
	}

	/* Desktop: Vertical bars side by side */
	@media (min-width: 769px) {
		.bars {
			flex-direction: row;
			height: 100%;
		}

		.bar-group {
			display: flex;
			flex-direction: column;
			width: 2rem;
			height: 100%;
			min-height: 0;
			max-height: 100%;
			gap: 0.25rem;
			overflow: hidden;
		}

		.bar-area {
			flex: 1;
			order: 1;
			display: flex;
			align-items: flex-end;
			width: 100%;
			min-height: 0;
		}

		.bar-label {
			order: 2;
			font-size: min(0.5em, 1vw);
			padding: 0 0.25rem;
			max-width: 100%;
			text-align: center;
		}

		.label-mobile {
			display: none;
		}

		.label-desktop {
			display: inline;
		}
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

	/* Responsive layout for mobile */
	@media (max-width: 768px) {
		.layout {
			grid-template-columns: 1fr;
			grid-template-rows: 1fr auto;
		}

		.bars {
			flex-direction: column;
			height: auto;
			gap: 0.5rem;
		}

		.placeholder {
			padding: 0.5rem;
			font-size: 0.8em;
		}
	}
</style>
