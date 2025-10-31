<script lang="ts">
	import { onMount } from 'svelte';
	import Quest from '$lib/components/Quest.svelte';
	import { 
		myMainQuestsStore,
		mySideQuestsStore,
		myArchivedQuestsStore,
		myActiveQuests,
		questSharingSettingsStore,
		networkQuestsStore,
		initializeQuestSharing,
		updateSharedQuests
	} from '$lib/commons/v5/quest-stores.svelte';
	import { generateQuests } from '$lib/services/quest-service';
	import type { Quest as QuestType, QuestType as QuestTypeEnum } from '$lib/commons/v5/quest-schemas';
	
	// State
	let generating = false;
	let error: string | null = null;
	let successMessage: string | null = null;
	let filterType: 'all' | 'main' | 'side' | 'archived' = 'all';
	let filterScale: 'all' | 'local' | 'community' | 'regional' | 'global' = 'all';
	let showNetworkQuests = false;
	
	// Stores
	$: mainQuests = $myMainQuestsStore || [];
	$: sideQuests = $mySideQuestsStore || [];
	$: archivedQuests = $myArchivedQuestsStore || [];
	$: sharingSettings = $questSharingSettingsStore || { enabled: false, shareWith: 'mutual-contributors' };
	$: networkQuests = $networkQuestsStore || {};
	
	// Filtered quests
	$: filteredQuests = (() => {
		let quests: QuestType[] = [];
		
		// Select based on type filter
		if (filterType === 'all') {
			quests = [...mainQuests, ...sideQuests];
		} else if (filterType === 'main') {
			quests = mainQuests;
		} else if (filterType === 'side') {
			quests = sideQuests;
		} else if (filterType === 'archived') {
			quests = archivedQuests;
		}
		
		// Filter by scale
		if (filterScale !== 'all') {
			quests = quests.filter(q => q.scale === filterScale);
		}
		
		// Sort by type (main first) then by creation date
		return quests.sort((a, b) => {
			if (a.type === 'main' && b.type !== 'main') return -1;
			if (a.type !== 'main' && b.type === 'main') return 1;
			return (b._updatedAt || 0) - (a._updatedAt || 0);
		});
	})();
	
	// Network quests list
	$: networkQuestsList = (() => {
		return Object.entries(networkQuests).flatMap(([pub, quests]) => 
			quests.map(q => ({ ...q, fromPub: pub }))
		);
	})();
	
	// Generate quests
	async function handleGenerateQuests() {
		generating = true;
		error = null;
		successMessage = null;
		
		try {
			const result = await generateQuests({
				maxQuests: 5,
				includeGeolocation: true,
				includePeerQuests: sharingSettings.enabled
			});
			
			if (result.success) {
				successMessage = `✨ Generated ${result.quests?.length || 0} new quests!`;
				setTimeout(() => successMessage = null, 5000);
			} else {
				error = result.error || 'Failed to generate quests';
			}
		} catch (err: any) {
			error = err.message || 'Quest generation failed';
		} finally {
			generating = false;
		}
	}
	
	// Toggle sharing
	async function handleToggleSharing() {
		const newSettings = {
			...sharingSettings,
			enabled: !sharingSettings.enabled,
			_updatedAt: Date.now()
		};
		
		questSharingSettingsStore.set(newSettings);
		
		if (newSettings.enabled) {
			initializeQuestSharing();
			updateSharedQuests();
		}
	}
	
	// Initialize on mount
	onMount(() => {
		if (sharingSettings.enabled) {
			initializeQuestSharing();
		}
	});
</script>

<svelte:head>
	<title>Quests - Free Association</title>
</svelte:head>

<div class="container mx-auto px-4 py-8 max-w-6xl">
	<!-- Header -->
	<div class="mb-8">
		<h1 class="text-3xl font-bold text-gray-900 mb-2">🎯 Your Quests</h1>
		<p class="text-gray-600">AI-generated actions aligned with your values and capacities</p>
	</div>
	
	<!-- Status Messages -->
	{#if successMessage}
		<div class="mb-6 p-4 bg-green-50 border border-green-200 rounded-lg text-green-800">
			{successMessage}
		</div>
	{/if}
	
	{#if error}
		<div class="mb-6 p-4 bg-red-50 border border-red-200 rounded-lg text-red-800">
			{error}
		</div>
	{/if}
	
	<!-- Action Bar -->
	<div class="mb-6 bg-white rounded-lg shadow-sm p-4 border border-gray-200">
		<div class="flex flex-wrap gap-4 items-center justify-between">
			<div class="flex gap-3">
				<button
					on:click={handleGenerateQuests}
					disabled={generating}
					class="px-6 py-3 bg-gradient-to-r from-purple-600 to-blue-600 text-white font-medium rounded-lg hover:from-purple-700 hover:to-blue-700 disabled:opacity-50 disabled:cursor-not-allowed transition-all shadow-md hover:shadow-lg"
				>
					{generating ? '🔄 Generating...' : '✨ Generate Quests'}
				</button>
				
				<button
					on:click={handleToggleSharing}
					class="px-4 py-3 {sharingSettings.enabled ? 'bg-green-100 text-green-700 border-green-300' : 'bg-gray-100 text-gray-700 border-gray-300'} border font-medium rounded-lg hover:opacity-80 transition-all"
				>
					{sharingSettings.enabled ? '🌐 Sharing Enabled' : '🔒 Sharing Disabled'}
				</button>
			</div>
			
			<div class="text-sm text-gray-600">
				{mainQuests.length} main · {sideQuests.length} side · {archivedQuests.length} archived
			</div>
		</div>
	</div>
	
	<!-- Filters -->
	<div class="mb-6 flex flex-wrap gap-3">
		<!-- Type Filter -->
		<div class="flex gap-2">
			<button
				on:click={() => filterType = 'all'}
				class="px-4 py-2 rounded-lg font-medium text-sm transition-colors {filterType === 'all' ? 'bg-blue-600 text-white' : 'bg-gray-100 text-gray-700 hover:bg-gray-200'}"
			>
				All
			</button>
			<button
				on:click={() => filterType = 'main'}
				class="px-4 py-2 rounded-lg font-medium text-sm transition-colors {filterType === 'main' ? 'bg-purple-600 text-white' : 'bg-gray-100 text-gray-700 hover:bg-gray-200'}"
			>
				⭐ Main
			</button>
			<button
				on:click={() => filterType = 'side'}
				class="px-4 py-2 rounded-lg font-medium text-sm transition-colors {filterType === 'side' ? 'bg-blue-600 text-white' : 'bg-gray-100 text-gray-700 hover:bg-gray-200'}"
			>
				📌 Side
			</button>
			<button
				on:click={() => filterType = 'archived'}
				class="px-4 py-2 rounded-lg font-medium text-sm transition-colors {filterType === 'archived' ? 'bg-gray-600 text-white' : 'bg-gray-100 text-gray-700 hover:bg-gray-200'}"
			>
				📦 Archived
			</button>
		</div>
		
		<!-- Scale Filter -->
		<div class="flex gap-2 ml-4">
			<button
				on:click={() => filterScale = 'all'}
				class="px-3 py-2 rounded-lg text-sm {filterScale === 'all' ? 'bg-gray-600 text-white' : 'bg-gray-100 text-gray-600 hover:bg-gray-200'}"
			>
				All Scales
			</button>
			<button
				on:click={() => filterScale = 'local'}
				class="px-3 py-2 rounded-lg text-sm {filterScale === 'local' ? 'bg-blue-600 text-white' : 'bg-gray-100 text-gray-600 hover:bg-gray-200'}"
			>
				Local
			</button>
			<button
				on:click={() => filterScale = 'community'}
				class="px-3 py-2 rounded-lg text-sm {filterScale === 'community' ? 'bg-green-600 text-white' : 'bg-gray-100 text-gray-600 hover:bg-gray-200'}"
			>
				Community
			</button>
			<button
				on:click={() => filterScale = 'regional'}
				class="px-3 py-2 rounded-lg text-sm {filterScale === 'regional' ? 'bg-purple-600 text-white' : 'bg-gray-100 text-gray-600 hover:bg-gray-200'}"
			>
				Regional
			</button>
			<button
				on:click={() => filterScale = 'global'}
				class="px-3 py-2 rounded-lg text-sm {filterScale === 'global' ? 'bg-orange-600 text-white' : 'bg-gray-100 text-gray-600 hover:bg-gray-200'}"
			>
				Global
			</button>
		</div>
	</div>
	
	<!-- Quest List -->
	<div class="mb-8">
		{#if filteredQuests.length === 0}
			<div class="text-center py-12 bg-gray-50 rounded-lg border-2 border-dashed border-gray-300">
				<p class="text-gray-500 text-lg mb-4">No quests yet!</p>
				<p class="text-gray-400 mb-6">Click "Generate Quests" to get personalized actions based on your values.</p>
				<button
					on:click={handleGenerateQuests}
					disabled={generating}
					class="px-6 py-3 bg-purple-600 text-white font-medium rounded-lg hover:bg-purple-700 disabled:opacity-50"
				>
					{generating ? 'Generating...' : '✨ Generate Your First Quests'}
				</button>
			</div>
		{:else}
			<div class="grid gap-4 md:grid-cols-2 lg:grid-cols-2">
				{#each filteredQuests as quest (quest.id)}
					<Quest {quest} />
				{/each}
			</div>
		{/if}
	</div>
	
	<!-- Network Quests Section -->
	{#if sharingSettings.enabled && networkQuestsList.length > 0}
		<div class="mb-8">
			<div class="flex items-center justify-between mb-4">
				<h2 class="text-2xl font-bold text-gray-900">🌐 Network Quests</h2>
				<button
					on:click={() => showNetworkQuests = !showNetworkQuests}
					class="text-sm text-gray-600 hover:text-gray-900"
				>
					{showNetworkQuests ? 'Hide' : 'Show'} ({networkQuestsList.length})
				</button>
			</div>
			
			{#if showNetworkQuests}
				<div class="grid gap-4 md:grid-cols-2 lg:grid-cols-2">
					{#each networkQuestsList as quest (quest.id)}
						<Quest quest={quest} showActions={false} />
					{/each}
				</div>
			{/if}
		</div>
	{/if}
</div>

