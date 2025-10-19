<script lang="ts">
	import Parent from '$lib/components/collective/Parent.svelte';
	import Bar from '$lib/components/Bar.svelte';
	import { userSogf, userTree, generalShares } from '$lib/state/core.svelte';
	import { recalculateFromTree } from '$lib/state/calculations.svelte';
	import { derived } from 'svelte/store';
	import { onMount } from 'svelte';
	import {
		collectiveMembers,
		collectiveForest,
		mergeContributorTrees,
		createSimpleMergeConfig
	} from '$lib/collective-tree.svelte';
	import type { CollectiveTree, Node } from '$lib/schema';

	// Reactive variable for mobile detection
	let isMobile = $state(false);
	let mediaQuery: MediaQueryList;

	// Create collective tree from collective members and their trees
	const collectiveTree = derived(
		[collectiveMembers, collectiveForest, userTree],
		([$collectiveMembers, $collectiveForest, $userTree]) => {
			// If we don't have any collective members, show empty state
			if (!$collectiveMembers || $collectiveMembers.length === 0) {
				console.log('[COLLECTIVE] No collective members defined');
				return null;
			}

			// Check if we have trees for collective members
			const contributorTrees: Record<string, Node> = {};

			// Add trees from collective forest for each member
			for (const member of $collectiveMembers) {
				const memberId = typeof member === 'string' ? member : member.id;
				const memberTree = $collectiveForest.get(memberId);

				if (memberTree) {
					contributorTrees[memberId] = memberTree;
					console.log(`[COLLECTIVE] Added tree for member: ${memberId}`);
				} else {
					console.log(`[COLLECTIVE] Waiting for tree from member: ${memberId}`);
				}
			}

			// Only create collective tree if we have at least one tree from collective members
			if (Object.keys(contributorTrees).length === 0) {
				console.log('[COLLECTIVE] No trees available yet from collective members, waiting...');
				return null;
			}

			try {
				const config = createSimpleMergeConfig(contributorTrees);
				const result = mergeContributorTrees(config);

				console.log(
					`[COLLECTIVE] Created collective tree with ${Object.keys(contributorTrees).length} of ${$collectiveMembers.length} member trees`
				);
				console.log('[COLLECTIVE] Collective tree stats:', result.merge_stats);

				return result.collective_tree;
			} catch (error) {
				console.error('[COLLECTIVE] Error creating collective tree:', error);
				return null;
			}
		}
	);

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

<div class="layout root-page">
	<div class="parent">
		{#if $collectiveTree}
			<Parent collectiveTree={$collectiveTree} />
		{:else}
			<div class="empty-state">
				<p>No collective members added yet.</p>
				<p class="hint">Add members to the collective to see their merged tree visualization.</p>
			</div>
		{/if}
	</div>
	<div class="bars">
		<div class="bar-group" class:vertical={!isMobile}>
			<div
				class="bar-label"
				title="Your-Recognition: your acknowledgment of contributions towards the realization of your priorities"
			>
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
						showLabelsOnSelect={true}
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
			<div
				class="bar-label"
				title="Mutual-Recognition: Your mutual-recognition with another is the minimum of your recognition of each other. This displays your mutual-recognition with each as a % of your total-mutual-recognition with all!"
			>
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
						showLabelsOnSelect={true}
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
	/* Removed conflicting :global(body) styles - handled by layout */

	.layout {
		display: grid;
		grid-template-columns: 9fr 1fr;
		gap: 0.5rem;
		width: 100%;
		height: 100%;
		max-height: 100%;
		overflow: hidden;
	}

	/* Root page specific: ensure it doesn't scroll */
	.layout.root-page {
		overflow: hidden;
		height: 100%;
		max-height: 100%;
		position: relative;
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

	.empty-state {
		height: 100%;
		display: flex;
		flex-direction: column;
		align-items: center;
		justify-content: center;
		text-align: center;
		color: #666;
		font-size: 1em;
		padding: 2rem;
	}

	.empty-state .hint {
		font-size: 0.9em;
		color: #999;
		margin-top: 0.5rem;
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
