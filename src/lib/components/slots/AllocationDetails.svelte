<script lang="ts">
	/**
	 * AllocationDetails Component
	 * 
	 * Displays allocation details for a specific slot:
	 * - For capacity slots: Shows who we're allocating to (outgoing)
	 * - For need slots: Shows who's allocating to us (incoming)
	 * 
	 * Features:
	 * - Real-time updates from network commitments
	 * - Tier display (mutual vs non-mutual)
	 * - Progress indicators (allocated / total)
	 * - Transparency and trust building
	 */
	
	import type { SlotAllocationRecord, NeedSlot, AvailabilitySlot } from '$lib/protocol/schemas';
	import { networkAllocations, myCommitmentStore } from '$lib/protocol/stores.svelte';
	
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
			// ✅ Uses networkAllocations field store (fine-grained reactivity!)
			// Only updates when allocations change, not on every commitment field change
			for (const [pubKey, allocations] of $networkAllocations.entries()) {
				if (!allocations || allocations.length === 0) continue;
				
				for (const alloc of allocations) {
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
		}
		
		return allocs;
	});
	
	let totalAllocated = $derived(
		allocations.reduce((sum, a) => sum + a.quantity, 0)
	);
	
	let allocationPercentage = $derived(
		slot.quantity > 0 ? (totalAllocated / slot.quantity) * 100 : 0
	);
	
	let mutualCount = $derived(
		allocations.filter(a => a.tier === 'mutual').length
	);
	
	let nonMutualCount = $derived(
		allocations.filter(a => a.tier === 'non-mutual').length
	);
</script>

<div class="allocation-details">
	{#if allocations.length > 0}
		<div class="allocations-summary">
			<h4 class="header">
				{isCapacity ? '🎁 Allocating to' : '📥 Receiving from'}:
			</h4>
			
			<div class="stats">
				<span class="total">{totalAllocated.toFixed(2)} / {slot.quantity}</span>
				<span class="percentage" class:full={allocationPercentage >= 100}>
					({allocationPercentage.toFixed(0)}%)
				</span>
			</div>
			
			<div class="tier-stats">
				{#if mutualCount > 0}
					<span class="tier mutual">🤝 {mutualCount} mutual</span>
				{/if}
				{#if nonMutualCount > 0}
					<span class="tier non-mutual">💝 {nonMutualCount} generous</span>
				{/if}
			</div>
		</div>
		
		<ul class="allocation-list">
			{#each allocations as alloc}
				<li class="allocation-item {alloc.tier}">
					<span class="pubkey" title={alloc.pubKey}>
						{alloc.pubKey.slice(0, 12)}...{alloc.pubKey.slice(-4)}
					</span>
					<span class="quantity">{alloc.quantity.toFixed(2)}</span>
					<span class="tier-badge {alloc.tier}">
						{alloc.tier === 'mutual' ? '🤝' : '💝'}
					</span>
				</li>
			{/each}
		</ul>
		
		<div class="progress-bar">
			<div 
				class="progress-fill" 
				class:full={allocationPercentage >= 100}
				style="width: {Math.min(allocationPercentage, 100)}%"
			></div>
		</div>
	{:else}
		<div class="no-allocations">
			<span class="icon">{isCapacity ? '⏳' : '⌛'}</span>
			<span class="message">
				{isCapacity ? 'Not allocated yet' : 'No incoming allocations'}
			</span>
		</div>
	{/if}
</div>

<style>
	.allocation-details {
		margin-top: 0.75rem;
		padding: 0.75rem;
		background: var(--surface-2, #f5f5f5);
		border-radius: 8px;
		font-size: 0.9rem;
	}
	
	.allocations-summary {
		margin-bottom: 0.75rem;
	}
	
	.header {
		font-size: 0.95rem;
		font-weight: 600;
		margin: 0 0 0.5rem 0;
		color: var(--text-1, #333);
	}
	
	.stats {
		display: flex;
		align-items: center;
		gap: 0.5rem;
		margin-bottom: 0.5rem;
	}
	
	.total {
		font-weight: 600;
		color: var(--text-1, #333);
	}
	
	.percentage {
		color: var(--text-2, #666);
		font-size: 0.85rem;
	}
	
	.percentage.full {
		color: var(--success, #22c55e);
		font-weight: 600;
	}
	
	.tier-stats {
		display: flex;
		gap: 0.5rem;
		flex-wrap: wrap;
	}
	
	.tier {
		padding: 0.2rem 0.5rem;
		border-radius: 4px;
		font-size: 0.8rem;
		font-weight: 500;
	}
	
	.tier.mutual {
		background: var(--mutual-bg, #e0f2fe);
		color: var(--mutual-text, #0369a1);
	}
	
	.tier.non-mutual {
		background: var(--generous-bg, #fef3c7);
		color: var(--generous-text, #ca8a04);
	}
	
	.allocation-list {
		list-style: none;
		padding: 0;
		margin: 0;
		display: flex;
		flex-direction: column;
		gap: 0.4rem;
	}
	
	.allocation-item {
		display: flex;
		align-items: center;
		gap: 0.5rem;
		padding: 0.4rem 0.6rem;
		background: var(--surface-1, #fff);
		border-radius: 6px;
		border-left: 3px solid transparent;
	}
	
	.allocation-item.mutual {
		border-left-color: var(--mutual-border, #0ea5e9);
	}
	
	.allocation-item.non-mutual {
		border-left-color: var(--generous-border, #eab308);
	}
	
	.pubkey {
		flex: 1;
		font-family: 'Monaco', 'Menlo', 'Courier New', monospace;
		font-size: 0.8rem;
		color: var(--text-2, #666);
		overflow: hidden;
		text-overflow: ellipsis;
	}
	
	.quantity {
		font-weight: 600;
		color: var(--text-1, #333);
		min-width: 3rem;
		text-align: right;
	}
	
	.tier-badge {
		font-size: 1rem;
	}
	
	.progress-bar {
		margin-top: 0.75rem;
		height: 6px;
		background: var(--surface-3, #e5e5e5);
		border-radius: 3px;
		overflow: hidden;
	}
	
	.progress-fill {
		height: 100%;
		background: linear-gradient(90deg, #3b82f6, #0ea5e9);
		transition: width 0.3s ease;
	}
	
	.progress-fill.full {
		background: linear-gradient(90deg, #22c55e, #16a34a);
	}
	
	.no-allocations {
		display: flex;
		align-items: center;
		gap: 0.5rem;
		padding: 0.75rem;
		color: var(--text-3, #999);
		font-style: italic;
	}
	
	.no-allocations .icon {
		font-size: 1.2rem;
	}
	
	.no-allocations .message {
		font-size: 0.9rem;
	}
	
	/* Dark mode support */
	@media (prefers-color-scheme: dark) {
		.allocation-details {
			background: var(--surface-2, #2a2a2a);
		}
		
		.header {
			color: var(--text-1, #e5e5e5);
		}
		
		.total {
			color: var(--text-1, #e5e5e5);
		}
		
		.percentage {
			color: var(--text-2, #a0a0a0);
		}
		
		.allocation-item {
			background: var(--surface-1, #1f1f1f);
		}
		
		.pubkey {
			color: var(--text-2, #a0a0a0);
		}
		
		.quantity {
			color: var(--text-1, #e5e5e5);
		}
		
		.progress-bar {
			background: var(--surface-3, #3a3a3a);
		}
	}
</style>

