<script lang="ts">
	import { goto } from '$app/navigation';
	import type { PageData } from './$types';
	import { publicTreesArray, publishMyTree, unpublishMyTree } from '$lib/network/public-trees.svelte';
	import { holsterUserPub } from '$lib/network/holster.svelte';
	import { getUserName } from '$lib/network/users.svelte';

	let { data }: { data: PageData } = $props();

	// View toggle state
	let currentView = $state<'organizations' | 'individuals'>('organizations');

	// Search and filter state
	let searchQuery = $state('');
	let selectedCategory = $state<string>('all');

	// Category definitions (matching our org structure)
	const categories = {
		all: 'All Organizations',
		un: 'UN Agencies',
		science: 'Climate Science',
		conservation: 'Conservation & NGOs',
		finance: 'Climate Finance',
		foundations: 'Philanthropic Foundations',
		regional: 'Regional & Political',
		coalitions: 'Climate Coalitions'
	};

	// Categorize organizations by slug patterns
	function getCategoryForOrg(slug: string): string {
		const unAgencies = ['unep', 'undp', 'unicef', 'unhcr', 'wfp', 'who', 'fao', 'unhabitat'];
		const science = ['ipcc'];
		const conservation = [
			'greenpeace',
			'wwf',
			'natureconservancy',
			'conservationinternational',
			'oxfam',
			'redcross',
			'threefiveozero'
		];
		const finance = ['greenclimatefund', 'worldbank', 'imf', 'asiandevbank', 'africandevbank'];
		const foundations = [
			'bezosearthfund',
			'gatesfoundation',
			'rockefellerfoundation',
			'bloombergphilanthropies',
			'fordfoundation',
			'climateworks'
		];
		const regional = ['europeanunion', 'africanunion', 'aosis', 'c40cities'];
		const coalitions = ['climateactionnetwork', 'wemeanbus'];

		if (unAgencies.includes(slug)) return 'un';
		if (science.includes(slug)) return 'science';
		if (conservation.includes(slug)) return 'conservation';
		if (finance.includes(slug)) return 'finance';
		if (foundations.includes(slug)) return 'foundations';
		if (regional.includes(slug)) return 'regional';
		if (coalitions.includes(slug)) return 'coalitions';
		return 'all';
	}

	// Format budget for display
	function formatBudget(budget: number): string {
		if (budget >= 1_000_000_000) {
			return `$${(budget / 1_000_000_000).toFixed(2)}B`;
		} else if (budget >= 1_000_000) {
			return `$${(budget / 1_000_000).toFixed(0)}M`;
		} else {
			return `$${(budget / 1_000).toFixed(0)}K`;
		}
	}

	// Filter organizations based on search and category
	const filteredOrganizations = $derived(() => {
		let filtered = data.organizations;

		// Filter by category
		if (selectedCategory !== 'all') {
			filtered = filtered.filter((org) => getCategoryForOrg(org.slug) === selectedCategory);
		}

		// Filter by search query
		if (searchQuery.trim()) {
			const query = searchQuery.toLowerCase();
			filtered = filtered.filter(
				(org) =>
					org.name.toLowerCase().includes(query) ||
					org.description?.toLowerCase().includes(query) ||
					org.slug.toLowerCase().includes(query)
			);
		}

		return filtered;
	});

	// Group filtered organizations by category
	const groupedOrganizations = $derived(() => {
		const groups: Record<string, typeof data.organizations> = {};

		for (const org of filteredOrganizations()) {
			const cat = getCategoryForOrg(org.slug);
			if (!groups[cat]) groups[cat] = [];
			groups[cat].push(org);
		}

		return groups;
	});

	// Filter public trees based on search
	const filteredPublicTrees = $derived(() => {
		let filtered = $publicTreesArray;

		// Filter by search query
		if (searchQuery.trim()) {
			const query = searchQuery.toLowerCase();
			filtered = filtered.filter(
				(tree) =>
					tree.alias.toLowerCase().includes(query) ||
					tree.pubkey.toLowerCase().includes(query)
			);
		}

		return filtered;
	});

	// Check if my tree is published
	const isMyTreePublished = $derived(() => {
		if (!$holsterUserPub) return false;
		return $publicTreesArray.some((tree) => tree.pubkey === $holsterUserPub);
	});

	// Format timestamp for display
	function formatLastSeen(timestamp: number): string {
		const now = Date.now();
		const diff = now - timestamp;
		const minutes = Math.floor(diff / 60000);
		const hours = Math.floor(diff / 3600000);
		const days = Math.floor(diff / 86400000);

		if (minutes < 1) return 'Just now';
		if (minutes < 60) return `${minutes}m ago`;
		if (hours < 24) return `${hours}h ago`;
		return `${days}d ago`;
	}

	// Handle publish/unpublish
	async function handlePublishToggle() {
		if (!$holsterUserPub) return;
		
		try {
			const alias = await getUserName($holsterUserPub);
			if (isMyTreePublished()) {
				await unpublishMyTree($holsterUserPub);
			} else {
				await publishMyTree(alias, $holsterUserPub);
			}
		} catch (error) {
			console.error('[ORG-PAGE] Failed to toggle publish:', error);
			alert('Failed to update tree publication status');
		}
	}

</script>

<svelte:head>
	<title>Climate Organizations Directory - COP30 Demo</title>
	<meta
		name="description"
		content="Explore 33 climate organizations in the COP30 demo ecosystem with recognition trees and mutual recognition networks."
	/>
</svelte:head>

<div
	class="min-h-screen bg-gradient-to-br from-green-50 via-blue-50 to-purple-50 dark:from-gray-900 dark:via-gray-800 dark:to-gray-900"
>
	<div class="container mx-auto max-w-7xl px-4 py-8">
		<!-- Header -->
		<div class="mb-12 text-center">
			<div class="mb-6 flex justify-center">
				<img
					src="/logo.png"
					alt="Free Association Logo"
					class="h-32 w-auto drop-shadow-2xl md:h-40 lg:h-48"
				/>
			</div>
			<h1 class="mb-4 text-5xl font-bold">
				<span class="bg-gradient-to-r from-green-600 to-blue-600 bg-clip-text text-transparent"
					>Climate Organizations</span
				>
			</h1>
			<button
				onclick={() => (currentView = 'organizations')}
				class="rounded-lg px-6 py-3 font-semibold transition-all {currentView === 'organizations'
					? 'bg-blue-600 text-white shadow-lg'
					: 'bg-white text-gray-700 hover:bg-gray-100 dark:bg-gray-800 dark:text-gray-300 dark:hover:bg-gray-700'}"
			>
				🏢 Organizations ({data.organizations.length})
			</button>
{#if $publicTreesArray.length > 0}
			<button
				onclick={() => (currentView = 'individuals')}
				class="rounded-lg px-6 py-3 font-semibold transition-all {currentView === 'individuals'
					? 'bg-blue-600 text-white shadow-lg'
					: 'bg-white text-gray-700 hover:bg-gray-100 dark:bg-gray-800 dark:text-gray-300 dark:hover:bg-gray-700'}"
			>
				👤 Individuals ({$publicTreesArray.length})
			</button>
			{/if}
		</div>

		<!-- Search and Filter Controls -->
		<div class="mb-8 rounded-lg bg-white p-6 shadow-lg dark:bg-gray-800">
			<div class="flex flex-col gap-4 md:flex-row">
				<!-- Search Input -->
				<div class="flex-1">
					<label
						for="search"
						class="mb-2 block text-sm font-medium text-gray-700 dark:text-gray-300"
					>
						Search {currentView === 'organizations' ? 'Organizations' : 'Individuals'}
					</label>
					<input
						id="search"
						type="text"
						bind:value={searchQuery}
						placeholder={currentView === 'organizations'
							? 'Search by name, description, or slug...'
							: 'Search by alias or public key...'}
						class="w-full rounded-lg border border-gray-300 bg-white px-4 py-2 text-gray-900 focus:border-transparent focus:ring-2 focus:ring-blue-500 dark:border-gray-600 dark:bg-gray-700 dark:text-white"
					/>
				</div>

				<!-- Category Filter (only for organizations) -->
				{#if currentView === 'organizations'}
					<div class="md:w-64">
						<label
							for="category"
							class="mb-2 block text-sm font-medium text-gray-700 dark:text-gray-300"
						>
							Filter by Category
						</label>
						<select
							id="category"
							bind:value={selectedCategory}
							class="w-full rounded-lg border border-gray-300 bg-white px-4 py-2 text-gray-900 focus:border-transparent focus:ring-2 focus:ring-blue-500 dark:border-gray-600 dark:bg-gray-700 dark:text-white"
						>
							{#each Object.entries(categories) as [key, label]}
								<option value={key}>{label}</option>
							{/each}
						</select>
					</div>
				{/if}
			</div>

			<!-- Results Count -->
		<div class="mt-4 text-sm text-gray-600 dark:text-gray-400">
			{#if currentView === 'organizations'}
				Showing {filteredOrganizations().length} of {data.organizations.length} organizations
			{:else}
				Showing {filteredPublicTrees().length} of {$publicTreesArray.length} public trees
			{/if}
		</div>
	</div>

	<!-- Organizations View -->
	{#if currentView === 'organizations'}
		<!-- Organization Cards by Category -->
		{#each Object.entries(categories) as [catKey, catLabel]}
			{#if groupedOrganizations()[catKey] && groupedOrganizations()[catKey].length > 0}
				<div class="mb-12">
					<h2 class="mb-6 flex items-center gap-3 text-3xl font-bold text-gray-800 dark:text-white">
						<span>{catLabel}</span>
						<span class="text-lg font-normal text-gray-500 dark:text-gray-400">
							({groupedOrganizations()[catKey].length})
						</span>
					</h2>

					<div class="grid grid-cols-1 gap-6 md:grid-cols-2 lg:grid-cols-3">
						{#each groupedOrganizations()[catKey] as org}
							<button
								onclick={() => goto(`/org/${org.slug}`)}
								class="group transform rounded-lg border-2 border-transparent bg-white p-6 text-left shadow-md transition-all duration-300 hover:scale-105 hover:border-blue-500 hover:shadow-xl dark:bg-gray-800"
							>
								<!-- Organization Header -->
								<div class="mb-4 flex items-start gap-4">
									<div class="text-5xl">{org.emoji}</div>
									<div class="min-w-0 flex-1">
										<h3
											class="mb-1 text-xl font-bold text-gray-900 transition-colors group-hover:text-blue-600 dark:text-white dark:group-hover:text-blue-400"
										>
											{org.name}
										</h3>
										<p class="line-clamp-2 text-sm text-gray-600 dark:text-gray-400">
											{org.description}
										</p>
									</div>
								</div>

								<!-- Organization Stats -->
								<div
									class="mt-4 grid grid-cols-3 gap-3 border-t border-gray-200 pt-4 dark:border-gray-700"
								>
									<div class="text-center">
										<div class="text-lg font-bold text-blue-600 dark:text-blue-400">
											{formatBudget(org.monthlyBudget)}
										</div>
										<div class="text-xs text-gray-500 dark:text-gray-400">Budget/mo</div>
									</div>
									<div class="text-center">
										<div class="text-lg font-bold text-green-600 dark:text-green-400">
											{org.recognizes}
										</div>
										<div class="text-xs text-gray-500 dark:text-gray-400">Partners</div>
									</div>
									<div class="text-center">
										<div class="text-lg font-bold text-purple-600 dark:text-purple-400">
											{org.priorities}
										</div>
										<div class="text-xs text-gray-500 dark:text-gray-400">Priorities</div>
									</div>
								</div>

								<!-- Navigate Arrow -->
								<div
									class="mt-4 flex items-center justify-end text-sm text-blue-600 transition-transform group-hover:translate-x-1 dark:text-blue-400"
								>
									<span class="font-medium">Explore</span>
									<svg class="ml-1 h-4 w-4" fill="none" stroke="currentColor" viewBox="0 0 24 24">
										<path
											stroke-linecap="round"
											stroke-linejoin="round"
											stroke-width="2"
											d="M9 5l7 7-7 7"
										/>
									</svg>
								</div>
							</button>
						{/each}
					</div>
				</div>
			{/if}
		{/each}

		<!-- No Results Message -->
		{#if filteredOrganizations().length === 0}
			<div class="py-16 text-center">
				<div class="mb-4 text-6xl">🔍</div>
				<h3 class="mb-2 text-2xl font-bold text-gray-700 dark:text-gray-300">
					No organizations found
				</h3>
				<p class="mb-6 text-gray-500 dark:text-gray-400">
					Try adjusting your search or filter criteria
				</p>
				<button
					onclick={() => {
						searchQuery = '';
						selectedCategory = 'all';
					}}
					class="rounded-lg bg-blue-600 px-6 py-2 text-white transition-colors hover:bg-blue-700"
				>
					Clear Filters
				</button>
			</div>
		{/if}
	{:else}
		<!-- Individuals View -->
		<div class="mb-8">
			<!-- Publish Button -->
			{#if $holsterUserPub}
				<div class="mb-8 flex justify-center">
					<button
						onclick={handlePublishToggle}
						class="rounded-lg px-8 py-3 font-semibold shadow-lg transition-all {isMyTreePublished()
							? 'bg-red-600 text-white hover:bg-red-700'
							: 'bg-green-600 text-white hover:bg-green-700'}"
					>
						{isMyTreePublished() ? '🚫 Unpublish My Tree' : '📢 Publish My Tree'}
					</button>
				</div>
			{/if}

			<!-- Public Trees Grid -->
			{#if filteredPublicTrees().length > 0}
				<div class="grid grid-cols-1 gap-6 md:grid-cols-2 lg:grid-cols-3">
					{#each filteredPublicTrees() as tree}
						<button
							onclick={() => goto(`/org/${tree.pubkey}`)}
							class="group transform rounded-lg border-2 border-transparent bg-white p-6 text-left shadow-md transition-all duration-300 hover:scale-105 hover:border-purple-500 hover:shadow-xl dark:bg-gray-800"
						>
							<!-- Tree Header -->
							<div class="mb-4 flex items-start gap-4">
								<div class="text-5xl">🌳</div>
								<div class="min-w-0 flex-1">
									<h3
										class="mb-1 text-xl font-bold text-gray-900 transition-colors group-hover:text-purple-600 dark:text-white dark:group-hover:text-purple-400"
									>
										{tree.alias}
									</h3>
									<p class="truncate text-sm text-gray-600 dark:text-gray-400">
										{tree.pubkey.slice(0, 20)}...
									</p>
								</div>
							</div>

							<!-- Tree Stats -->
							<div class="mt-4 border-t border-gray-200 pt-4 dark:border-gray-700">
								<div class="text-center">
									<div class="text-sm text-gray-500 dark:text-gray-400">Last Seen</div>
									<div class="text-lg font-bold text-purple-600 dark:text-purple-400">
										{formatLastSeen(tree.lastSeen)}
									</div>
								</div>
							</div>

							<!-- Navigate Arrow -->
							<div
								class="mt-4 flex items-center justify-end text-sm text-purple-600 transition-transform group-hover:translate-x-1 dark:text-purple-400"
							>
								<span class="font-medium">View Tree</span>
								<svg class="ml-1 h-4 w-4" fill="none" stroke="currentColor" viewBox="0 0 24 24">
									<path
										stroke-linecap="round"
										stroke-linejoin="round"
										stroke-width="2"
										d="M9 5l7 7-7 7"
									/>
								</svg>
							</div>
						</button>
					{/each}
				</div>
			{:else}
				<!-- No Trees Message -->
				<div class="py-16 text-center">
					<div class="mb-4 text-6xl">🌳</div>
					<h3 class="mb-2 text-2xl font-bold text-gray-700 dark:text-gray-300">
						No public trees found
					</h3>
					<p class="mb-6 text-gray-500 dark:text-gray-400">
						{searchQuery.trim()
							? 'Try adjusting your search criteria'
							: 'Be the first to publish your tree!'}
					</p>
					{#if searchQuery.trim()}
						<button
							onclick={() => (searchQuery = '')}
							class="rounded-lg bg-purple-600 px-6 py-2 text-white transition-colors hover:bg-purple-700"
						>
							Clear Search
						</button>
					{/if}
				</div>
			{/if}
		</div>
	{/if}

	<!-- Footer Info -->
	<div class="mt-16 text-center text-sm text-gray-500 dark:text-gray-400">
		<p>
			{currentView === 'organizations'
				? 'Click any organization to explore their recognition tree and mutual recognition network'
				: 'Click any individual to view their public recognition tree'}
		</p>
	</div>
	</div>
</div>
