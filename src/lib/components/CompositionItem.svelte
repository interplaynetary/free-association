<script lang="ts">
	import { get } from 'svelte/store';
	import {
		userDesiredComposeFrom,
		userDesiredComposeInto,
		feasibleComposeFrom,
		feasibleComposeInto,
		mutualFeasibleOurCapacities,
		mutualFeasibleTheirCapacities
	} from '$lib/state/protocol/compose.svelte';
	import Chat from './Chat.svelte';

	interface CompositionCapacity {
		id: string;
		name: string;
		emoji?: string;
		unit: string;
		quantity: number;
		computed_quantity?: number;
		provider_id?: string;
		provider_name?: string;
	}

	interface Props {
		ourCapacity: CompositionCapacity;
		theirCapacity: CompositionCapacity;
		direction: 'from' | 'into';
		expanded?: boolean;
		onToggle?: () => void;
	}

	let { ourCapacity, theirCapacity, direction, expanded = false, onToggle }: Props = $props();

	// Reactive state for desired quantity input
	let desiredQuantityInput = $state(0);

	// Get current stores
	let currentDesiredFrom = $derived(get(userDesiredComposeFrom));
	let currentDesiredInto = $derived(get(userDesiredComposeInto));
	let currentFeasibleFrom = $derived(get(feasibleComposeFrom));
	let currentFeasibleInto = $derived(get(feasibleComposeInto));
	let currentMutualFeasibleOurs = $derived(get(mutualFeasibleOurCapacities));
	let currentMutualFeasibleTheirs = $derived(get(mutualFeasibleTheirCapacities));

	// Update input when store changes
	$effect(() => {
		if (direction === 'from') {
			desiredQuantityInput = currentDesiredFrom[ourCapacity.id]?.[theirCapacity.id] || 0;
		} else {
			desiredQuantityInput = currentDesiredInto[ourCapacity.id]?.[theirCapacity.id] || 0;
		}
	});

	// Get feasible quantity
	let feasibleQuantity = $derived(() => {
		if (direction === 'from') {
			return currentFeasibleFrom[ourCapacity.id]?.[theirCapacity.id] || 0;
		} else {
			return currentFeasibleInto[ourCapacity.id]?.[theirCapacity.id] || 0;
		}
	});

	// Get mutual feasible information
	let mutualFeasibleInfo = $derived(() => {
		const compositionKey = `${ourCapacity.id}:${theirCapacity.id}:${theirCapacity.provider_id}`;

		if (direction === 'from') {
			return currentMutualFeasibleOurs[compositionKey];
		} else {
			return currentMutualFeasibleTheirs[compositionKey];
		}
	});

	// Get available quantity (what we can compose from/into)
	let availableQuantity = $derived(() => {
		if (direction === 'from') {
			// For compose-from, available is their computed_quantity (our share-adjusted amount)
			return theirCapacity.computed_quantity || 0;
		} else {
			// For compose-into, available is our capacity quantity
			return ourCapacity.quantity || 0;
		}
	});

	// Get unit for composition
	let compositionUnit = $derived(() => {
		return direction === 'from' ? theirCapacity.unit : ourCapacity.unit;
	});

	// Handle quantity changes
	function handleQuantityChange() {
		const store = direction === 'from' ? userDesiredComposeFrom : userDesiredComposeInto;
		const current = get(store);

		const updated = {
			...current,
			[ourCapacity.id]: {
				...current[ourCapacity.id],
				[theirCapacity.id]: desiredQuantityInput
			}
		};

		store.set(updated);
	}

	// Get color based on feasibility
	function getColor(desired: number, feasible: number): string {
		if (desired === 0) return 'transparent';
		if (feasible >= desired * 0.9) return '#dcfce7';
		if (feasible >= desired * 0.5) return '#fef3c7';
		return '#fee2e2';
	}
</script>

<div class="item" style="background: {getColor(desiredQuantityInput, feasibleQuantity())}">
	<div class="main" onclick={() => onToggle?.()}>
		<span class="name">{theirCapacity.emoji || '📦'} {theirCapacity.name}</span>

		<div class="input-group">
			<input
				type="number"
				min="0"
				step="0.1"
				bind:value={desiredQuantityInput}
				onchange={handleQuantityChange}
				onclick={(e) => e.stopPropagation()}
				placeholder="0"
			/>
			<span class="unit">{compositionUnit()}</span>
		</div>

		<div class="metrics">
			<span class="feasible">{feasibleQuantity().toFixed(1)}</span>
			{#if mutualFeasibleInfo()}
				<span class="mutual">{mutualFeasibleInfo().ourFeasibleAmount.toFixed(1)}</span>
			{/if}
		</div>
	</div>

	{#if expanded}
		<div class="details">
			<div class="analysis">
				<div>Desired: {desiredQuantityInput} {compositionUnit()}</div>
				<div>Feasible: {feasibleQuantity().toFixed(1)} {compositionUnit()}</div>
				<div>Available: {availableQuantity().toFixed(1)} {compositionUnit()}</div>
				{#if desiredQuantityInput > 0}
					<div>
						Achievability: {((feasibleQuantity() / desiredQuantityInput) * 100).toFixed(1)}%
					</div>
				{/if}
				{#if mutualFeasibleInfo()}
					<div class="mutual-info">
						<div>
							Their desired: {mutualFeasibleInfo().theirDesiredAmount.toFixed(1)}
							{compositionUnit()}
						</div>
						<div>
							Mutual feasible: {mutualFeasibleInfo().ourFeasibleAmount.toFixed(1)}
							{compositionUnit()}
						</div>
						<div>Alignment: {(mutualFeasibleInfo().desireViability * 100).toFixed(1)}%</div>
					</div>
				{/if}
			</div>

			<Chat
				chatId="composition-{ourCapacity.id}-{theirCapacity.id}-{direction}"
				placeholder="Discuss..."
				maxLength={200}
			/>
		</div>
	{/if}
</div>

<style>
	.item {
		border: 1px solid #e5e7eb;
		border-radius: 4px;
		overflow: hidden;
	}

	.main {
		display: flex;
		align-items: center;
		gap: 6px;
		padding: 6px;
		cursor: pointer;
		min-width: 0;
	}

	.main:hover {
		background: rgba(0, 0, 0, 0.02);
	}

	.name {
		flex: 1;
		font-size: 13px;
		font-weight: 500;
		min-width: 0;
		overflow: hidden;
		text-overflow: ellipsis;
		white-space: nowrap;
	}

	.input-group {
		display: flex;
		align-items: center;
		gap: 2px;
		flex-shrink: 0;
	}

	input {
		width: 50px;
		padding: 1px 3px;
		border: 1px solid #d1d5db;
		border-radius: 2px;
		text-align: right;
		font-size: 12px;
	}

	input:focus {
		outline: none;
		border-color: #3b82f6;
	}

	.unit {
		font-size: 10px;
		color: #9ca3af;
		width: 15px;
		flex-shrink: 0;
	}

	.metrics {
		display: flex;
		flex-direction: column;
		align-items: flex-end;
		font-size: 10px;
		flex-shrink: 0;
		min-width: 35px;
	}

	.feasible {
		color: #059669;
		font-weight: 500;
	}

	.mutual {
		color: #7c3aed;
		font-weight: 500;
	}

	.details {
		border-top: 1px solid #e5e7eb;
		background: #f9fafb;
		padding: 6px;
	}

	.analysis {
		font-size: 11px;
		color: #6b7280;
		margin-bottom: 6px;
	}

	.analysis > div {
		margin-bottom: 1px;
	}

	.mutual-info {
		margin-top: 3px;
		padding-top: 3px;
		border-top: 1px solid #e5e7eb;
	}

	.mutual-info > div {
		color: #7c3aed;
	}
</style>
