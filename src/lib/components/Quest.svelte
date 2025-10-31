<script lang="ts">
	import type { Quest } from '$lib/modules/quests/quest-schemas';
	import { completeQuest, archiveQuest, deleteQuest, updateQuestProgress } from '$lib/services/quest-service';
	import { get } from 'svelte/store';
	
	export let quest: Quest;
	export let showActions: boolean = true;
	export let compact: boolean = false;
	
	// Scale colors
	const scaleColors: Record<string, string> = {
		local: 'bg-blue-100 text-blue-800',
		community: 'bg-green-100 text-green-800',
		regional: 'bg-purple-100 text-purple-800',
		global: 'bg-orange-100 text-orange-800'
	};
	
	// Difficulty colors
	const difficultyColors: Record<string, string> = {
		easy: 'bg-gray-100 text-gray-800',
		medium: 'bg-yellow-100 text-yellow-800',
		hard: 'bg-red-100 text-red-800',
		epic: 'bg-pink-100 text-pink-800'
	};
	
	// Get origin display text
	function getOriginText(quest: Quest): string {
		if (quest.origin.type === 'ai') {
			const modelName = quest.origin.model.split('/')[1] || quest.origin.model;
			return `AI: ${modelName}`;
		} else if (quest.origin.type === 'peer') {
			return `From: ${quest.origin.alias || quest.origin.pub.slice(0, 8)}`;
		} else {
			return 'Manual';
		}
	}
	
	// Action handlers
	function handleComplete() {
		if (confirm('Mark this quest as completed?')) {
			completeQuest(quest.id);
		}
	}
	
	function handleArchive() {
		if (confirm('Archive this quest?')) {
			archiveQuest(quest.id);
		}
	}
	
	function handleDelete() {
		if (confirm('Permanently delete this quest?')) {
			deleteQuest(quest.id);
		}
	}
	
	function handleProgressUpdate(event: Event) {
		const target = event.target as HTMLInputElement;
		const progress = parseFloat(target.value) / 100;
		updateQuestProgress(quest.id, progress);
	}
	
	// Format location
	function formatLocation(quest: Quest): string {
		if (!quest.location) return '';
		if (quest.location.online) return '🌐 Online';
		
		const parts = [
			quest.location.city,
			quest.location.state_province,
			quest.location.country
		].filter(Boolean);
		
		return parts.length > 0 ? `📍 ${parts.join(', ')}` : '';
	}
</script>

<div class="quest-card border rounded-lg p-4 {compact ? 'p-3' : 'p-4'} shadow-sm hover:shadow-md transition-shadow {quest.completion?.completed ? 'bg-gray-50 opacity-75' : 'bg-white'}">
	<!-- Header -->
	<div class="flex items-start justify-between mb-2">
		<div class="flex-1">
			<h3 class="{compact ? 'text-lg' : 'text-xl'} font-semibold {quest.completion?.completed ? 'line-through text-gray-500' : 'text-gray-900'}">
				{quest.title}
			</h3>
		</div>
		
		{#if quest.completion?.completed}
			<span class="text-green-600 font-bold ml-2">✓</span>
		{/if}
	</div>
	
	<!-- Badges -->
	<div class="flex flex-wrap gap-2 mb-3">
		<!-- Type Badge -->
		<span class="text-xs px-2 py-1 rounded-full font-medium {quest.type === 'main' ? 'bg-purple-100 text-purple-800' : 'bg-blue-100 text-blue-800'}">
			{quest.type === 'main' ? '⭐ Main Quest' : '📌 Side Quest'}
		</span>
		
		<!-- Scale Badge -->
		<span class="text-xs px-2 py-1 rounded-full font-medium {scaleColors[quest.scale]}">
			{quest.scale}
		</span>
		
		<!-- Difficulty Badge -->
		<span class="text-xs px-2 py-1 rounded-full font-medium {difficultyColors[quest.difficulty]}">
			{quest.difficulty}
		</span>
		
		<!-- Origin Badge -->
		<span class="text-xs px-2 py-1 rounded-full font-medium bg-indigo-100 text-indigo-800">
			{getOriginText(quest)}
		</span>
	</div>
	
	<!-- Description -->
	{#if !compact}
		<p class="text-gray-700 mb-3 text-sm leading-relaxed">{quest.description}</p>
	{/if}
	
	<!-- Location -->
	{#if quest.location && formatLocation(quest)}
		<p class="text-sm text-gray-600 mb-2">{formatLocation(quest)}</p>
	{/if}
	
	<!-- Rewards -->
	{#if !compact && quest.rewards && quest.rewards.length > 0}
		<div class="mb-3">
			<h4 class="text-sm font-semibold text-gray-700 mb-1">Rewards:</h4>
			<ul class="text-sm text-gray-600 space-y-1">
				{#each quest.rewards as reward}
					<li class="flex items-start">
						<span class="mr-2">🎁</span>
						<span>{reward.description}</span>
					</li>
				{/each}
			</ul>
		</div>
	{/if}
	
	<!-- Progress Bar -->
	{#if !quest.completion?.completed && quest.completion?.progress !== undefined}
		<div class="mb-3">
			<div class="flex justify-between text-sm mb-1">
				<span class="text-gray-600">Progress</span>
				<span class="text-gray-700 font-medium">{Math.round((quest.completion.progress || 0) * 100)}%</span>
			</div>
			<input
				type="range"
				min="0"
				max="100"
				value={(quest.completion.progress || 0) * 100}
				on:change={handleProgressUpdate}
				class="w-full h-2 bg-gray-200 rounded-lg appearance-none cursor-pointer"
			/>
		</div>
	{/if}
	
	<!-- Tags -->
	{#if !compact && quest.tags && quest.tags.length > 0}
		<div class="flex flex-wrap gap-1 mb-3">
			{#each quest.tags as tag}
				<span class="text-xs px-2 py-0.5 bg-gray-100 text-gray-600 rounded">#{tag}</span>
			{/each}
		</div>
	{/if}
	
	<!-- Actions -->
	{#if showActions && !quest.completion?.completed}
		<div class="flex gap-2 mt-3 pt-3 border-t border-gray-200">
			<button
				on:click={handleComplete}
				class="flex-1 px-3 py-2 text-sm font-medium text-white bg-green-600 hover:bg-green-700 rounded-md transition-colors"
			>
				Complete
			</button>
			<button
				on:click={handleArchive}
				class="px-3 py-2 text-sm font-medium text-gray-700 bg-gray-100 hover:bg-gray-200 rounded-md transition-colors"
			>
				Archive
			</button>
			<button
				on:click={handleDelete}
				class="px-3 py-2 text-sm font-medium text-red-600 hover:text-red-700 hover:bg-red-50 rounded-md transition-colors"
			>
				Delete
			</button>
		</div>
	{:else if quest.completion?.completed && quest.completion.completedAt}
		<div class="text-sm text-gray-500 mt-2 pt-2 border-t border-gray-200">
			Completed: {new Date(quest.completion.completedAt).toLocaleDateString()}
		</div>
	{/if}
</div>

<style>
	.quest-card {
		transition: all 0.2s ease-in-out;
	}
	
	.quest-card:hover {
		transform: translateY(-2px);
	}
</style>