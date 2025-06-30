<script lang="ts">
	import { userNetworkCapacitiesWithShares } from '$lib/state/core.svelte';
	import { getUserName, user } from '$lib/state/gun.svelte';
	import Share from './Share.svelte';
	import type {
		Node,
		RootNode,
		NonRootNode,
		Capacity,
		CapacitiesCollection,
		ShareMap,
		ProviderCapacity,
		RecipientCapacity,
		BaseCapacity
	} from '$lib/schema';

	async function handleProviderClick(provider: string) {
		// This function will be implemented later to navigate to the provider
		console.log(`Navigating to provider: ${provider}`);
	}

	let shares = $state<RecipientCapacity[]>([]);
	let expandedShares = $state<Set<string>>(new Set());

	function toggleShare(shareId: string) {
		const newExpanded = new Set(expandedShares);
		if (newExpanded.has(shareId)) {
			newExpanded.delete(shareId);
		} else {
			newExpanded.add(shareId);
		}
		expandedShares = newExpanded;
	}

	$effect(() => {
		void (async () => {
			if (!$userNetworkCapacitiesWithShares) {
				shares = [];
				return;
			}

			const allShares = Object.entries($userNetworkCapacitiesWithShares).map(
				([capacityId, capacity]) =>
					({
						...capacity,
						id: capacityId
					}) as RecipientCapacity
			);

			// Filter out shares with no name or zero/no quantity
			shares = allShares.filter(
				(share) =>
					share.name &&
					share.name.trim() !== '' &&
					share.computed_quantity &&
					share.computed_quantity > 0
			);
		})();
	});
</script>

<div class="shares-list space-y-3 p-2">
	{#each shares as share (share.id)}
		<Share
			{share}
			expanded={expandedShares.has(share.id)}
			onToggle={() => toggleShare(share.id)}
			onProviderClick={handleProviderClick}
		/>
	{/each}
</div>

<style>
	:global(body) {
		font-family: 'Inter', system-ui, sans-serif;
		background: #f7fafc;
	}

	.shares-list {
		display: flex;
		flex-direction: column;
	}
</style>
