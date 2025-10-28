<script lang="ts">
	// V5: Import commitments from v5 stores
	import { getAllCommitmentsRecord } from '$lib/commons/v5/stores.svelte';
	import { getUserName } from '$lib/state/users.svelte';
	import { globalState } from '$lib/global.svelte';
	import Share from '$lib/components/Share.svelte';
	import { getAllocatedSlotCount, getTotalSlotCount } from '$lib/commons/v5/protocol';
	import { t } from '$lib/translations';
	import type {
		Node,
		RootNode,
		NonRootNode,
		ShareMap,
		Commitment,
		AvailabilitySlot,
		NeedSlot
	} from '$lib/commons/v5/schemas';
	
	// V5: Use Commitment types directly (already have capacity_slots and need_slots)
	type Capacity = Commitment;
	type ProviderCapacity = Commitment;  // V5: Already has capacity_slots
	type RecipientCapacity = Commitment;  // V5: Already has need_slots
	type BaseCapacity = Commitment;
	type CapacitiesCollection = Record<string, Commitment>;

	async function handleProviderClick(provider: string) {
		// This function will be implemented later to navigate to the provider
		console.log(`Navigating to provider: ${provider}`);
	}

	// All filter state comes from globalState
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

	// Direct use of protocol functions - no wrappers needed!

	// Base shares data - ALL available network capacities (for discovery and desire expression)
	let allShares = $derived(() => {
		// V5: Get all commitments
		const allCommitments = getAllCommitmentsRecord();
		if (!allCommitments) {
			return [];
		}

		const sharesList = Object.entries(allCommitments).map(
			([capacityId, capacity]) =>
				({
					...capacity,
					id: capacityId
				}) as any // Use any since we're now working with inventory data, not RecipientCapacity
		);

		// Show ALL capacities with names - don't filter by allocation status
		// This allows users to discover and express desire for unallocated capacities
		return sharesList.filter((share) => share.name && share.name.trim() !== '');
	});

	// Filtered and sorted shares
	let filteredShares = $derived(() => {
		let filtered = allShares();

		// Apply search filter from global state
		if (globalState.inventorySearchQuery.trim()) {
			const query = globalState.inventorySearchQuery.toLowerCase().trim();
			filtered = filtered.filter(
				(share) =>
					share.name.toLowerCase().includes(query) ||
					(share.unit && share.unit.toLowerCase().includes(query)) ||
					(share.emoji && share.emoji.includes(query))
			);
		}

		// Apply provider filter
		if (globalState.inventorySelectedProvider !== 'all') {
			filtered = filtered.filter((share) => share.provider_id === globalState.inventorySelectedProvider);
		}

		// Apply sorting
		filtered.sort((a, b) => {
			let comparison = 0;

			switch (globalState.inventorySortBy) {
				case 'name':
					comparison = a.name.localeCompare(b.name);
					break;
				case 'allocated_slots':
					comparison = getAllocatedSlotCount(a) - getAllocatedSlotCount(b);
					break;
				case 'total_slots':
					comparison = getTotalSlotCount(a) - getTotalSlotCount(b);
					break;
				case 'provider':
					comparison = (a.provider_id || '').localeCompare(b.provider_id || '');
					break;
			}

			return globalState.inventorySortDirection === 'asc' ? comparison : -comparison;
		});

		return filtered;
	});

	// Quick stats for display
	let stats = $derived(() => ({
		total: allShares().length,
		filtered: filteredShares().length,
	}));

	function clearFilters() {
		globalState.inventorySearchQuery = '';
		globalState.inventorySelectedProvider = 'all';
		globalState.inventorySortBy = 'name';
		globalState.inventorySortDirection = 'asc';
	}
</script>

<div class="shares-container">
	<!-- Results Summary -->
	{#if stats().filtered !== stats().total}
		<div class="results-summary">
			Showing {stats().filtered} of {stats().total} shares
		</div>
	{/if}

	<!-- Shares List -->
	<div class="shares-list">
		{#if filteredShares().length > 0}
			{#each filteredShares() as share (share.id)}
				<Share
					{share}
					expanded={expandedShares.has(share.id)}
					onToggle={() => toggleShare(share.id)}
					onProviderClick={handleProviderClick}
				/>
			{/each}
	{:else}
		<div class="empty-state">
			{#if globalState.inventorySearchQuery || globalState.inventorySelectedProvider !== 'all'}
				<p>{$t('inventory.no_shares_match_filters')}</p>
				<button class="clear-btn" onclick={clearFilters}>{$t('inventory.clear_filters')}</button>
			{:else}
				<p>{$t('inventory.no_shares_available')}</p>
			{/if}
		</div>
	{/if}
	</div>
</div>

<style>
	:global(body) {
		font-family: 'Inter', system-ui, sans-serif;
		background: #f7fafc;
	}

	.shares-container {
		display: flex;
		flex-direction: column;
		gap: 16px;
		padding: 12px;
		max-width: 100%;
		overflow: hidden;
	}

	.clear-btn {
		padding: 6px 12px;
		border: 1px solid #fca5a5;
		border-radius: 6px;
		background: #fef2f2;
		color: #dc2626;
		cursor: pointer;
		font-size: 13px;
		font-weight: 500;
		transition: all 0.2s ease;
		flex-shrink: 0;
		white-space: nowrap;
	}

	.clear-btn:hover {
		background: #fee2e2;
		border-color: #f87171;
	}

	/* Results Summary */
	.results-summary {
		padding: 8px 16px;
		background: #f0f9ff;
		border: 1px solid #bae6fd;
		border-radius: 6px;
		font-size: 13px;
		color: #0369a1;
		text-align: center;
	}

	/* Shares List */
	.shares-list {
		display: flex;
		flex-direction: column;
		gap: 12px;
	}

	/* Empty State */
	.empty-state {
		text-align: center;
		padding: 48px 24px;
		background: white;
		border-radius: 8px;
		border: 1px solid #e5e7eb;
		color: #6b7280;
	}

	.empty-state p {
		margin: 0 0 16px 0;
		font-size: 14px;
	}

	/* Responsive Design */
	@media (max-width: 480px) {
		.shares-container {
			padding: 8px;
		}
	}
</style>
