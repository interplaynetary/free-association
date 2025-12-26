<script lang="ts">
	/**
	 * SlotAllocationDetails Component
	 * 
	 * Expandable allocation details for a slot:
	 * - Compact view: Shows satisfaction bar + summary
	 * - Expanded view: Shows detailed list of providers/recipients
	 * 
	 * Combines SlotAllocationBar (compact visualization) with
	 * detailed allocation list (expandable)
	 */
	
	import type { SlotAllocationRecord, NeedSlot, AvailabilitySlot } from '$lib/protocol/schemas';
	import { networkAllocations, myCommitmentStore } from '$lib/protocol/stores/stores.svelte';
	import { getUserName, userNamesOrAliasesCache } from '$lib/network/users.svelte';
	import SlotAllocationBar from './SlotAllocationBar.svelte';
	
	interface Props {
		slot: NeedSlot | AvailabilitySlot;
		isCapacity: boolean;
		myPubKey: string;
	}
	
	let { slot, isCapacity, myPubKey }: Props = $props();
	
	let expanded = $state(false);
	
	// Compute allocations for this slot
	let allocations = $derived.by(() => {
		const allocs: Array<{ 
			pubKey: string; 
			quantity: number; 
			tier: string;
			needSlotId?: string;
			capacitySlotId?: string;
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
							needSlotId: alloc.recipient_need_slot_id,
							capacitySlotId: alloc.availability_slot_id
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
							tier: alloc.tier,
							needSlotId: alloc.recipient_need_slot_id,
							capacitySlotId: alloc.availability_slot_id
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
							tier: alloc.tier,
							needSlotId: alloc.recipient_need_slot_id,
							capacitySlotId: alloc.availability_slot_id
						});
					}
				}
			}
		}
		
		// Sort by quantity descending
		return allocs.sort((a, b) => b.quantity - a.quantity);
	});
	
	// Trigger name lookups
	$effect(() => {
		allocations.forEach(alloc => {
			getUserName(alloc.pubKey);
		});
	});
	
	let totalAllocated = $derived(
		allocations.reduce((sum, a) => sum + a.quantity, 0)
	);
	
	let allocationPercentage = $derived(
		slot.quantity > 0 ? (totalAllocated / slot.quantity) * 100 : 0
	);
</script>

<div class="slot-allocation-details">
	<!-- Always show the bar visualization -->
	<SlotAllocationBar {slot} {isCapacity} {myPubKey} />
	
	<!-- Expandable detailed list -->
	{#if allocations.length > 0}
		<button 
			class="expand-toggle"
			onclick={() => expanded = !expanded}
		>
			<span class="icon">{expanded ? '▼' : '▶'}</span>
			<span class="text">
				{expanded ? 'Hide' : 'Show'} {isCapacity ? 'recipients' : 'providers'}
			</span>
		</button>
		
		{#if expanded}
			<div class="details-list">
				<table class="allocation-table">
					<thead>
						<tr>
							<th class="pubkey-col">{isCapacity ? 'Recipient' : 'Provider'}</th>
							<th class="quantity-col">Amount</th>
							<th class="percent-col">%</th>
							<th class="tier-col">Tier</th>
						</tr>
					</thead>
					<tbody>
						{#each allocations as alloc}
							{@const percentage = slot.quantity > 0 ? (alloc.quantity / slot.quantity) * 100 : 0}
							{@const displayName = $userNamesOrAliasesCache[alloc.pubKey] || alloc.pubKey.slice(0, 12) + '...'}
							<tr class="allocation-row {alloc.tier}">
								<td class="pubkey-col" title={alloc.pubKey}>
									{#if alloc.pubKey === myPubKey}
										<span class="self-tag">You</span>
									{:else}
										<span class="name">{displayName}</span>
									{/if}
								</td>
								<td class="quantity-col">
									{alloc.quantity.toFixed(2)}
								</td>
								<td class="percent-col">
									{percentage.toFixed(1)}%
								</td>
								<td class="tier-col">
									<span class="tier-badge {alloc.tier}">
										{alloc.tier === 'mutual' ? '🤝' : '💝'}
									</span>
								</td>
							</tr>
						{/each}
					</tbody>
					<tfoot>
						<tr class="total-row">
							<td><strong>Total</strong></td>
							<td><strong>{totalAllocated.toFixed(2)}</strong></td>
							<td><strong>{allocationPercentage.toFixed(1)}%</strong></td>
							<td></td>
						</tr>
					</tfoot>
				</table>
			</div>
		{/if}
	{/if}
</div>

<style>
	.slot-allocation-details {
		display: flex;
		flex-direction: column;
		gap: 0.5rem;
		margin-top: 0.5rem;
		padding: 0;
		background: transparent;
		border-radius: 0;
		border: none;
	}
	
	.expand-toggle {
		display: flex;
		align-items: center;
		gap: 0.4rem;
		padding: 0.4rem 0.6rem;
		background: white;
		border: 1px solid #e5e7eb;
		border-radius: 6px;
		cursor: pointer;
		font-size: 0.85rem;
		color: #4b5563;
		transition: all 0.2s ease;
	}
	
	.expand-toggle:hover {
		background: #f3f4f6;
		border-color: #d1d5db;
	}
	
	.expand-toggle .icon {
		font-size: 0.7rem;
		transition: transform 0.2s ease;
	}
	
	.expand-toggle .text {
		font-weight: 500;
	}
	
	.details-list {
		margin-top: 0.5rem;
		overflow-x: auto;
	}
	
	.allocation-table {
		width: 100%;
		border-collapse: collapse;
		font-size: 0.85rem;
	}
	
	.allocation-table thead {
		background: #f3f4f6;
	}
	
	.allocation-table th {
		padding: 0.5rem 0.6rem;
		text-align: left;
		font-weight: 600;
		color: #6b7280;
		font-size: 0.75rem;
		text-transform: uppercase;
		letter-spacing: 0.05em;
	}
	
	.allocation-table td {
		padding: 0.5rem 0.6rem;
		border-top: 1px solid #e5e7eb;
	}
	
	.allocation-row {
		transition: background 0.15s ease;
	}
	
	.allocation-row:hover {
		background: #f9fafb;
	}
	
	.allocation-row.mutual {
		border-left: 3px solid #0ea5e9;
	}
	
	.allocation-row.non-mutual {
		border-left: 3px solid #eab308;
	}
	
	.pubkey-col {
		min-width: 120px;
	}
	
	.pubkey-col .self-tag {
		font-weight: 600;
		color: #7c3aed;
		background: #ede9fe;
		padding: 0.15rem 0.4rem;
		border-radius: 4px;
		font-size: 0.8rem;
	}
	
	.pubkey-col .name {
		color: #374151;
	}
	
	.quantity-col {
		text-align: right;
		font-weight: 600;
		color: #1f2937;
		min-width: 60px;
	}
	
	.percent-col {
		text-align: right;
		color: #6b7280;
		min-width: 50px;
	}
	
	.tier-col {
		text-align: center;
		min-width: 40px;
	}
	
	.tier-badge {
		font-size: 1rem;
	}
	
	.total-row {
		background: #f3f4f6;
		font-weight: 600;
	}
	
	.total-row td {
		padding: 0.6rem;
		border-top: 2px solid #d1d5db;
	}
	
	/* Dark mode - minimal changes since parent handles background */
	@media (prefers-color-scheme: dark) {
		.expand-toggle {
			background: #374151;
			border-color: #4b5563;
			color: #e5e7eb;
		}
		
		.expand-toggle:hover {
			background: #4b5563;
			border-color: #6b7280;
		}
		
		.allocation-table thead {
			background: #374151;
		}
		
		.allocation-table th {
			color: #9ca3af;
		}
		
		.allocation-table td {
			border-top-color: #4b5563;
		}
		
		.allocation-row:hover {
			background: #374151;
		}
		
		.pubkey-col .name {
			color: #e5e7eb;
		}
		
		.quantity-col {
			color: #f3f4f6;
		}
		
		.percent-col {
			color: #9ca3af;
		}
		
		.total-row {
			background: #374151;
		}
		
		.total-row td {
			border-top-color: #6b7280;
		}
	}
</style>

