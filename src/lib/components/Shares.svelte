<script lang="ts">
	import { userNetworkCapacitiesWithShares } from '$lib/state/core.svelte';
	import { getUserName, user } from '$lib/state/gun.svelte';
	import Chat from './Chat.svelte';
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

	interface ShareWithProvider extends RecipientCapacity {
		provider_name: string;
	}

	// Green color scale for share percentage
	const colors = ['#dcfce7', '#86efac', '#22c55e'];

	function getShareColor(percentage: number): string {
		if (percentage <= 0) return '#f3f4f6';
		const index = Math.floor(percentage * colors.length);
		return colors[Math.min(index, colors.length - 1)];
	}

	async function handleProviderClick(provider: string) {
		// This function will be implemented later to navigate to the provider
		console.log(`Navigating to provider: ${provider}`);
	}

	let shares = $state<ShareWithProvider[]>([]);
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

	function getChatId(share: ShareWithProvider): string {
		// Use capacity ID as the chat ID for all conversations about this capacity
		return share.id;
	}

	$effect(() => {
		void (async () => {
			if (!$userNetworkCapacitiesWithShares) {
				shares = [];
				return;
			}

			shares = await Promise.all(
				Object.entries($userNetworkCapacitiesWithShares).map(async ([capacityId, capacity]) => {
					const providerName = await getUserName((capacity as RecipientCapacity).provider_id);
					return {
						...capacity,
						id: capacityId,
						provider_name: providerName || ''
					} as RecipientCapacity & { provider_name: string };
				})
			);
		})();
	});
</script>

<div class="shares-list space-y-3 p-2">
	{#each shares as share (share.id)}
		<div class="capacity-share-container">
			<div
				class="capacity-share flex cursor-pointer items-center justify-between rounded p-2 shadow-sm"
				style="background-color: {getShareColor(
					share.share_percentage
				)}; border: 1px solid #e5e7eb;"
				onclick={() => toggleShare(share.id)}
			>
				<div class="flex min-w-0 flex-1 flex-col pr-2">
					<span
						class="share-value name overflow-hidden font-medium text-ellipsis whitespace-nowrap"
					>
						<span class="capacity-emoji">{share.emoji || '📦'}</span>
						{share.name}
					</span>
					<span class="share-value qty text-sm">
						{Number.isInteger(share.computed_quantity)
							? share.computed_quantity
							: share.computed_quantity.toFixed(2)}
						{share.unit}
						<span class="text-xs text-gray-600">({(share.share_percentage * 100).toFixed(1)}%)</span
						>
					</span>
				</div>
				<div class="flex items-center gap-2">
					<button
						type="button"
						class="provider-btn rounded-md px-2 py-1 text-xs whitespace-nowrap"
						onclick={(e) => {
							e.stopPropagation();
							handleProviderClick(share.provider_name);
						}}
					>
						{share.provider_name}
					</button>
					<span class="expand-icon text-sm">
						{expandedShares.has(share.id) ? '▼' : '▶'}
					</span>
				</div>
			</div>

			{#if expandedShares.has(share.id)}
				<div class="chat-container mt-2 rounded border border-gray-200 bg-gray-50 p-3">
					<div class="chat-header mb-2">
						<h4 class="text-sm font-medium text-gray-700">
							💬 Chat about {share.name} with {share.provider_name}
						</h4>
					</div>
					<Chat
						chatId={getChatId(share)}
						placeholder={`Message ${share.provider_name} about ${share.name}...`}
						maxLength={200}
					/>
				</div>
			{/if}
		</div>
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

	.capacity-share-container {
		width: 100%;
	}

	.capacity-share {
		transition: all 0.2s ease;
	}

	.capacity-share:hover {
		box-shadow: 0 2px 8px rgba(0, 0, 0, 0.1);
		transform: translateY(-1px);
	}

	.share-value {
		min-width: 0;
		line-height: 1.2;
	}

	.capacity-emoji {
		margin-right: 0.5rem;
		font-size: 1.1em;
		display: inline-block;
	}

	.provider-btn {
		background: rgba(255, 255, 255, 0.5);
		color: #4b5563;
		border: none;
		transition:
			background 0.2s,
			color 0.2s;
		cursor: pointer;
		flex-shrink: 0;
	}

	.provider-btn:hover {
		background: rgba(255, 255, 255, 0.8);
		color: #1f2937;
	}

	.expand-icon {
		color: #6b7280;
		font-weight: bold;
		min-width: 12px;
		text-align: center;
	}

	.chat-container {
		animation: slideDown 0.2s ease-out;
	}

	.chat-header h4 {
		margin: 0;
	}

	@keyframes slideDown {
		from {
			opacity: 0;
			max-height: 0;
			padding-top: 0;
			padding-bottom: 0;
		}
		to {
			opacity: 1;
			max-height: 500px;
			padding-top: 0.75rem;
			padding-bottom: 0.75rem;
		}
	}
</style>
