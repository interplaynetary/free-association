<script lang="ts">
	// @ts-ignore
	import { ourRecipientShares } from '$lib/state/core.svelte';
	import { getUserName } from '$lib/state/gun.svelte';

	interface CapacityShare {
		id: string;
		name: string;
		quantity: number;
		unit: string;
		provider: string;
		share_percentage: number;
	}

	let capacityShares = $state<CapacityShare[]>([]);

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

	// Watch ourRecipientShares and update the display
	$effect(() => {
		const updateShares = async () => {
			const shares: CapacityShare[] = [];

			for (const [capacityId, capacity] of Object.entries($ourRecipientShares)) {
				// Get the provider's name
				const providerName = await getUserName(capacity.owner_id);

				shares.push({
					id: capacityId,
					name: capacity.name,
					quantity: capacity.quantity * (capacity.recipient_shares?.[capacity.owner_id] || 0),
					unit: capacity.unit,
					provider: providerName,
					share_percentage: capacity.recipient_shares?.[capacity.owner_id] || 0
				});
			}

			capacityShares = shares;
		};

		updateShares();
	});
</script>

<div class="shares-list grid grid-cols-1 gap-3 p-2 sm:grid-cols-2 md:grid-cols-3 lg:grid-cols-4">
	{#each capacityShares as share (share.id)}
		<div
			class="capacity-share flex items-center justify-between rounded p-2 shadow-sm"
			style="background-color: {getShareColor(share.share_percentage)}; border: 1px solid #e5e7eb;"
		>
			<div class="flex min-w-0 flex-1 flex-col pr-2">
				<span class="share-value name overflow-hidden font-medium text-ellipsis whitespace-nowrap"
					>{share.name}</span
				>
				<span class="share-value qty text-sm">
					{share.quantity.toFixed(2)}
					{share.unit}
					<span class="text-xs text-gray-600">({(share.share_percentage * 100).toFixed(1)}%)</span>
				</span>
			</div>
			<button
				type="button"
				class="provider-btn rounded-md px-2 py-1 text-xs whitespace-nowrap"
				onclick={() => handleProviderClick(share.provider)}
			>
				{share.provider}
			</button>
		</div>
	{/each}
</div>

<style>
	:global(body) {
		font-family: 'Inter', system-ui, sans-serif;
		background: #f7fafc;
	}

	.share-value {
		min-width: 0;
		line-height: 1.2;
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
</style>
