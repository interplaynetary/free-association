<script lang="ts">
	/**
	 * SlotAllocationBar Component
	 * 
	 * Visualizes allocation satisfaction using Bar component:
	 * - For need slots: Shows satisfaction % from each provider (colored) + unsatisfied % (grey)
	 * - For capacity slots: Shows distribution % to each recipient (colored) + unused % (grey)
	 * 
	 * Features:
	 * - Uses getColorForUserId for consistent provider/recipient colors
	 * - Interactive hover/click labels from Bar component
	 * - Real-time reactivity from network commitments
	 */
	
	import type { SlotAllocationRecord, NeedSlot, AvailabilitySlot } from '@playnet/free-association/schemas';
	import { networkAllocations, myCommitmentStore } from '$lib/protocol/stores/stores.svelte';
	import { getColorForUserId } from '$lib/utils/ui/colorUtils';
	import Bar from '$lib/components/Bar.svelte';
	
	interface Props {
		slot: NeedSlot | AvailabilitySlot;
		isCapacity: boolean;
		myPubKey: string;
	}
	
	let { slot, isCapacity, myPubKey }: Props = $props();
	
	// Compute allocations for this slot
	let allocations = $derived.by(() => {
		const allocs: Array<{ 
			pubKey: string; 
			quantity: number; 
			tier: string;
			needSlotId?: string;
		}> = [];
		
		if (isCapacity) {
			// OUTGOING: Who am I allocating to from this capacity?
			const myCommitment = $myCommitmentStore;
			if (myCommitment?.slot_allocations) {
				for (const alloc of myCommitment.slot_allocations) {
					if (alloc.availability_slot_id === slot.id) {
						allocs.push({
							pubKey: alloc.recipient_pubkey,
							quantity: alloc.quantity,
							tier: alloc.tier,
							needSlotId: alloc.recipient_need_slot_id
						});
					}
				}
			}
		} else {
			// INCOMING: Who is allocating to this need?
			// Check network allocations
			for (const [pubKey, networkAllocs] of $networkAllocations.entries()) {
				if (!networkAllocs || networkAllocs.length === 0) continue;
				
				for (const alloc of networkAllocs) {
					if (alloc.recipient_pubkey === myPubKey && 
					    alloc.recipient_need_slot_id === slot.id) {
						allocs.push({
							pubKey,
							quantity: alloc.quantity,
							tier: alloc.tier
						});
					}
				}
			}
			
			// ✅ Also check self-allocations from my own commitment
			const myCommitment = $myCommitmentStore;
			if (myCommitment?.slot_allocations) {
				for (const alloc of myCommitment.slot_allocations) {
					if (alloc.recipient_pubkey === myPubKey && 
					    alloc.recipient_need_slot_id === slot.id) {
						allocs.push({
							pubKey: myPubKey,
							quantity: alloc.quantity,
							tier: alloc.tier
						});
					}
				}
			}
		}
		
		return allocs;
	});
	
	let totalAllocated = $derived(
		allocations.reduce((sum, a) => sum + a.quantity, 0)
	);
	
	let allocationPercentage = $derived(
		slot.quantity > 0 ? (totalAllocated / slot.quantity) * 100 : 0
	);
	
	// Build segments for Bar component
	let barSegments = $derived.by(() => {
		const segments: Array<{ id: string; value: number }> = [];
		
		// Add segments for each provider/recipient (using their actual percentage of total slot)
		for (const alloc of allocations) {
			const percentage = slot.quantity > 0 ? (alloc.quantity / slot.quantity) * 100 : 0;
			segments.push({
				id: alloc.pubKey,
				value: percentage
			});
		}
		
		// Add grey segment for unsatisfied/unused portion
		const unsatisfiedPercentage = 100 - allocationPercentage;
		if (unsatisfiedPercentage > 0) {
			segments.push({
				id: '__unsatisfied__',
				value: unsatisfiedPercentage
			});
		}
		
		return segments;
	});
	
	let mutualCount = $derived(
		allocations.filter(a => a.tier === 'mutual').length
	);
	
	let nonMutualCount = $derived(
		allocations.filter(a => a.tier === 'non-mutual').length
	);
	
	// Status text
	let statusText = $derived(() => {
		if (isCapacity) {
			if (allocations.length === 0) {
				return 'No allocations yet';
			}
			const unused = slot.quantity - totalAllocated;
			return `${totalAllocated.toFixed(1)} / ${slot.quantity} allocated${unused > 0 ? ` (${unused.toFixed(1)} unused)` : ''}`;
		} else {
			if (allocations.length === 0) {
				return 'No providers yet';
			}
			const unmet = slot.quantity - totalAllocated;
			if (unmet <= 0) {
				return `✅ Fully satisfied (${totalAllocated.toFixed(1)} / ${slot.quantity})`;
			}
			return `${totalAllocated.toFixed(1)} / ${slot.quantity} satisfied (${unmet.toFixed(1)} unmet)`;
		}
	});
</script>

<div class="slot-allocation-bar">
	<!-- Thin satisfaction bar at top -->
	<div class="bar-container">
		<Bar
			segments={barSegments}
			height="8px"
			width="100%"
			rounded={true}
			showLabelsAboveOnSelect={true}
			backgroundColor="#e5e5e5"
			forceHorizontal={true}
		/>
	</div>
	
	<!-- Status text below bar -->
	<div class="status-text" class:satisfied={!isCapacity && allocationPercentage >= 100}>
		<span class="emoji">{isCapacity ? '🎁' : allocationPercentage >= 100 ? '✅' : '🙏'}</span>
		<span class="text">{statusText()}</span>
		{#if allocations.length > 0}
			<span class="count">
				({allocations.length} {allocations.length === 1 ? (isCapacity ? 'recipient' : 'provider') : (isCapacity ? 'recipients' : 'providers')})
			</span>
		{/if}
	</div>
	
	<!-- Tier badges -->
	{#if allocations.length > 0}
		<div class="tier-badges">
			{#if mutualCount > 0}
				<span class="badge mutual">🤝 {mutualCount}</span>
			{/if}
			{#if nonMutualCount > 0}
				<span class="badge generous">💝 {nonMutualCount}</span>
			{/if}
		</div>
	{/if}
</div>

<style>
	.slot-allocation-bar {
		display: flex;
		flex-direction: column;
		gap: 0.4rem;
		margin-top: 0.5rem;
	}
	
	.bar-container {
		width: 100%;
		height: 8px;
		border-radius: 4px;
		overflow: hidden;
	}
	
	.status-text {
		display: flex;
		align-items: center;
		gap: 0.4rem;
		font-size: 0.85rem;
		color: #666;
	}
	
	.status-text.satisfied {
		color: #16a34a;
		font-weight: 500;
	}
	
	.status-text .emoji {
		font-size: 1rem;
	}
	
	.status-text .text {
		flex: 1;
	}
	
	.status-text .count {
		font-size: 0.8rem;
		opacity: 0.8;
	}
	
	.tier-badges {
		display: flex;
		gap: 0.4rem;
		flex-wrap: wrap;
	}
	
	.badge {
		padding: 0.15rem 0.4rem;
		border-radius: 4px;
		font-size: 0.75rem;
		font-weight: 500;
	}
	
	.badge.mutual {
		background: #e0f2fe;
		color: #0369a1;
	}
	
	.badge.generous {
		background: #fef3c7;
		color: #ca8a04;
	}
	
	/* Dark mode */
	@media (prefers-color-scheme: dark) {
		.status-text {
			color: #a0a0a0;
		}
		
		.status-text.satisfied {
			color: #22c55e;
		}
	}
</style>

