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

	// Search and filter state
	let searchQuery = $state('');
	let selectedProvider = $state('all');
	let sortBy = $state<'name' | 'quantity' | 'percentage' | 'provider'>('name');
	let sortDirection = $state<'asc' | 'desc'>('asc');

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

	// Base shares data - all valid shares
	let allShares = $derived(() => {
		if (!$userNetworkCapacitiesWithShares) {
			return [];
		}

		const sharesList = Object.entries($userNetworkCapacitiesWithShares).map(
			([capacityId, capacity]) =>
				({
					...capacity,
					id: capacityId
				}) as RecipientCapacity
		);

		// Filter out shares with no name or zero/no quantity
		return sharesList.filter(
			(share) =>
				share.name &&
				share.name.trim() !== '' &&
				share.computed_quantity &&
				share.computed_quantity > 0
		);
	});

	// Cache for provider names
	let providerNames = $state<Record<string, string>>({});

	// Load provider names asynchronously
	$effect(() => {
		void (async () => {
			const uniqueProviders = [...new Set(allShares().map((share) => share.provider_id))];

			for (const providerId of uniqueProviders) {
				if (providerId && !providerNames[providerId]) {
					try {
						const name = await getUserName(providerId);
						if (name) {
							providerNames = {
								...providerNames,
								[providerId]: name.length > 20 ? name.substring(0, 20) + '...' : name
							};
						}
					} catch (error) {
						console.warn('Failed to get provider name:', providerId, error);
					}
				}
			}
		})();
	});

	// Get providers with resolved names
	let providersWithNames = $derived(() => {
		const providerMap = new Map<string, string>();

		allShares().forEach((share) => {
			if (share.provider_id && !providerMap.has(share.provider_id)) {
				const displayName = providerNames[share.provider_id] || share.provider_id;
				providerMap.set(share.provider_id, displayName);
			}
		});

		return Array.from(providerMap.entries())
			.map(([id, name]) => ({ id, name }))
			.sort((a, b) => a.name.localeCompare(b.name));
	});

	// Filtered and sorted shares
	let filteredShares = $derived(() => {
		let filtered = allShares();

		// Apply search filter
		if (searchQuery.trim()) {
			const query = searchQuery.toLowerCase().trim();
			filtered = filtered.filter(
				(share) =>
					share.name.toLowerCase().includes(query) ||
					(share.unit && share.unit.toLowerCase().includes(query)) ||
					(share.emoji && share.emoji.includes(query))
			);
		}

		// Apply provider filter
		if (selectedProvider !== 'all') {
			filtered = filtered.filter((share) => share.provider_id === selectedProvider);
		}

		// Apply sorting
		filtered.sort((a, b) => {
			let comparison = 0;

			switch (sortBy) {
				case 'name':
					comparison = a.name.localeCompare(b.name);
					break;
				case 'quantity':
					comparison = (a.computed_quantity || 0) - (b.computed_quantity || 0);
					break;
				case 'percentage':
					comparison = a.share_percentage - b.share_percentage;
					break;
				case 'provider':
					comparison = (a.provider_id || '').localeCompare(b.provider_id || '');
					break;
			}

			return sortDirection === 'asc' ? comparison : -comparison;
		});

		return filtered;
	});

	// Quick stats for display
	let stats = $derived(() => ({
		total: allShares().length,
		filtered: filteredShares().length,
		providers: providersWithNames().length
	}));

	function clearFilters() {
		searchQuery = '';
		selectedProvider = 'all';
		sortBy = 'name';
		sortDirection = 'asc';
	}
</script>

<div class="shares-container">
	<!-- Search and Filter Controls -->
	<div class="filter-bar">
		<div class="search-section">
			<input
				type="text"
				class="search-input"
				placeholder="Search shares..."
				bind:value={searchQuery}
			/>
		</div>

		<div class="filter-controls">
			<select class="filter-select" bind:value={selectedProvider}>
				<option value="all">All providers ({stats().providers})</option>
				{#each providersWithNames() as provider}
					<option value={provider.id}>{provider.name}</option>
				{/each}
			</select>

			<div class="sort-controls">
				<select class="sort-select" bind:value={sortBy}>
					<option value="name">Sort by name</option>
					<option value="quantity">Sort by quantity</option>
					<option value="percentage">Sort by percentage</option>
					<option value="provider">Sort by provider</option>
				</select>

				<button
					class="sort-direction-btn"
					onclick={() => (sortDirection = sortDirection === 'asc' ? 'desc' : 'asc')}
					title="Toggle sort direction"
				>
					{sortDirection === 'asc' ? '↑' : '↓'}
				</button>
			</div>

			{#if searchQuery || selectedProvider !== 'all' || sortBy !== 'name' || sortDirection !== 'asc'}
				<button class="clear-btn" onclick={clearFilters}>Clear</button>
			{/if}
		</div>
	</div>

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
				{#if searchQuery || selectedProvider !== 'all'}
					<p>No shares match your filters.</p>
					<button class="clear-btn" onclick={clearFilters}>Clear filters</button>
				{:else}
					<p>No shares available.</p>
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

	/* Filter Bar */
	.filter-bar {
		background: white;
		border-radius: 8px;
		padding: 16px;
		box-shadow: 0 1px 3px rgba(0, 0, 0, 0.05);
		border: 1px solid #e5e7eb;
		width: 100%;
		box-sizing: border-box;
	}

	.search-section {
		width: 100%;
		margin-bottom: 12px;
	}

	.search-input {
		width: 100%;
		padding: 10px 14px;
		border: 1px solid #d1d5db;
		border-radius: 6px;
		font-size: 14px;
		background: #f9fafb;
		transition: all 0.2s ease;
		box-sizing: border-box;
	}

	.search-input:focus {
		outline: none;
		border-color: #3b82f6;
		background: white;
		box-shadow: 0 0 0 3px rgba(59, 130, 246, 0.1);
	}

	.search-input::placeholder {
		color: #9ca3af;
	}

	.filter-controls {
		display: grid;
		grid-template-columns: 1fr auto auto;
		gap: 8px;
		align-items: center;
		width: 100%;
	}

	.filter-select {
		padding: 8px 12px;
		border: 1px solid #d1d5db;
		border-radius: 6px;
		font-size: 13px;
		background: white;
		color: #374151;
		cursor: pointer;
		transition: border-color 0.2s ease;
		width: 100%;
		box-sizing: border-box;
		min-width: 0;
	}

	.sort-controls {
		display: flex;
		gap: 4px;
		align-items: center;
		flex-shrink: 0;
	}

	.sort-select {
		padding: 8px 10px;
		border: 1px solid #d1d5db;
		border-radius: 6px;
		font-size: 13px;
		background: white;
		color: #374151;
		cursor: pointer;
		transition: border-color 0.2s ease;
		box-sizing: border-box;
		width: 130px;
	}

	.filter-select:focus,
	.sort-select:focus {
		outline: none;
		border-color: #3b82f6;
	}

	.sort-direction-btn {
		padding: 6px 8px;
		border: 1px solid #d1d5db;
		border-radius: 6px;
		background: white;
		color: #6b7280;
		cursor: pointer;
		font-size: 14px;
		font-weight: 600;
		transition: all 0.2s ease;
		display: flex;
		align-items: center;
		justify-content: center;
		width: 32px;
		height: 32px;
		flex-shrink: 0;
	}

	.sort-direction-btn:hover {
		background: #f3f4f6;
		border-color: #9ca3af;
		color: #374151;
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
		grid-column: 1 / -1;
		justify-self: center;
		margin-top: 8px;
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

		.filter-bar {
			padding: 12px;
		}

		.filter-controls {
			grid-template-columns: 1fr;
			gap: 12px;
		}

		.sort-controls {
			justify-self: stretch;
			justify-content: space-between;
		}

		.sort-select {
			flex: 1;
			width: auto;
		}

		.clear-btn {
			margin-top: 4px;
		}
	}
</style>
