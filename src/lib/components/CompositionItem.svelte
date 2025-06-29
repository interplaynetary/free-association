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
	import {
		userNetworkCapacitiesWithShares,
		networkCapacities,
		contributorCapacityShares
	} from '$lib/state/core.svelte';
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

	// Determine provider ID using the same logic as compose.svelte.ts
	let providerId = $derived(() => {
		if (direction === 'from') {
			// For compose-from: get provider from userNetworkCapacitiesWithShares
			const networkCapacity = $userNetworkCapacitiesWithShares[theirCapacity.id];
			return (networkCapacity as any)?.provider_id;
		} else {
			// For compose-into: find who owns the target capacity in networkCapacities
			return Object.keys($networkCapacities).find(
				(id) => $networkCapacities[id] && $networkCapacities[id][theirCapacity.id]
			);
		}
	});

	// Update input when store changes
	$effect(() => {
		if (direction === 'from') {
			desiredQuantityInput = $userDesiredComposeFrom[ourCapacity.id]?.[theirCapacity.id] || 0;
		} else {
			desiredQuantityInput = $userDesiredComposeInto[ourCapacity.id]?.[theirCapacity.id] || 0;
		}
	});

	// Get feasible quantity
	let feasibleQuantity = $derived(() => {
		if (direction === 'from') {
			return $feasibleComposeFrom[ourCapacity.id]?.[theirCapacity.id] || 0;
		} else {
			return $feasibleComposeInto[ourCapacity.id]?.[theirCapacity.id] || 0;
		}
	});

	// Get mutual feasible information using proper provider ID
	let mutualFeasibleInfo = $derived(() => {
		const actualProviderId = providerId();
		if (!actualProviderId) return null;

		const compositionKey = `${ourCapacity.id}:${theirCapacity.id}:${actualProviderId}`;

		if (direction === 'from') {
			return $mutualFeasibleOurCapacities[compositionKey] || null;
		} else {
			return $mutualFeasibleTheirCapacities[compositionKey] || null;
		}
	});

	// Get available quantity using compose.svelte.ts logic
	let availableQuantity = $derived(() => {
		if (direction === 'from') {
			// For compose-from: available is their computed_quantity (our share-adjusted amount)
			const networkCapacity = $userNetworkCapacitiesWithShares[theirCapacity.id];
			return (networkCapacity as any)?.computed_quantity || 0;
		} else {
			// For compose-into: available is constrained by their share in our capacity
			const actualProviderId = providerId();
			if (!actualProviderId) return 0;

			const theirShareInOurCapacity =
				$contributorCapacityShares[actualProviderId]?.[ourCapacity.id] || 0;
			const ourCapacityQuantity = ourCapacity.quantity || 0;
			return ourCapacityQuantity * theirShareInOurCapacity;
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

	// Get constraint explanation
	let constraintExplanation = $derived(() => {
		if (direction === 'from') {
			const networkCapacity = $userNetworkCapacitiesWithShares[theirCapacity.id];
			const sharePercentage = (networkCapacity as any)?.share_percentage || 0;
			return `Limited by your ${(sharePercentage * 100).toFixed(1)}% share`;
		} else {
			const actualProviderId = providerId();
			if (!actualProviderId) return 'Provider not found';

			const theirShareInOurCapacity =
				$contributorCapacityShares[actualProviderId]?.[ourCapacity.id] || 0;
			return `Limited by their ${(theirShareInOurCapacity * 100).toFixed(1)}% share in your capacity`;
		}
	});
</script>

<div class="item" style="background: {getColor(desiredQuantityInput, feasibleQuantity())}">
	<div class="main" onclick={() => onToggle?.()}>
		<!-- Line 1: Emoji and Name -->
		<div class="header-line">
			<span class="name">{theirCapacity.emoji || '📦'} {theirCapacity.name}</span>
		</div>

		<!-- Line 2: Our Desire and Their Desire -->
		<div class="desire-line">
			<div class="our-desire">
				<label class="desire-label">Our desire:</label>
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
			</div>
			<div class="their-desire">
				<label class="desire-label">Their desire:</label>
				<span class="their-value">
					{#if mutualFeasibleInfo()}
						{mutualFeasibleInfo()?.theirDesiredAmount.toFixed(1)} {compositionUnit()}
					{:else}
						— {compositionUnit()}
					{/if}
				</span>
			</div>
		</div>

		<!-- Line 3: Mutual Feasible -->
		<div class="mutual-line">
			{#if mutualFeasibleInfo()}
				<span class="mutual-label">Mutual feasible:</span>
				<span class="mutual-value"
					>{mutualFeasibleInfo()?.ourFeasibleAmount.toFixed(1)} {compositionUnit()}</span
				>
			{:else}
				<span class="no-mutual">No mutual interest yet</span>
			{/if}
		</div>
	</div>

	{#if expanded}
		<div class="details">
			<div class="analysis">
				<div><strong>Desired:</strong> {desiredQuantityInput} {compositionUnit()}</div>
				<div><strong>Feasible:</strong> {feasibleQuantity().toFixed(1)} {compositionUnit()}</div>
				<div><strong>Available:</strong> {availableQuantity().toFixed(1)} {compositionUnit()}</div>
				<div class="constraint-info" title={constraintExplanation()}>
					<strong>Constraint:</strong>
					{constraintExplanation()}
				</div>
				{#if desiredQuantityInput > 0}
					<div>
						<strong>Achievability:</strong>
						{((feasibleQuantity() / desiredQuantityInput) * 100).toFixed(1)}%
					</div>
				{/if}
				{#if mutualFeasibleInfo()}
					{@const info = mutualFeasibleInfo()}
					<div class="mutual-info">
						<div>
							<strong>Their desired:</strong>
							{info?.theirDesiredAmount.toFixed(1)}
							{compositionUnit()}
						</div>
						<div>
							<strong>Mutual feasible:</strong>
							{info?.ourFeasibleAmount.toFixed(1)}
							{compositionUnit()}
						</div>
						<div>
							<strong>Desire alignment:</strong>
							{(info?.desireViability || 0 * 100).toFixed(1)}%
						</div>
						<div>
							<strong>Feasible alignment:</strong>
							{(info?.feasibleViability || 0 * 100).toFixed(1)}%
						</div>
						<div>
							<strong>Constraint ratio:</strong>
							{(info?.constraintRatio || 0 * 100).toFixed(1)}%
						</div>
					</div>
				{/if}
				{#if !providerId()}
					<div class="warning">⚠️ Provider not found - composition may not work</div>
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
		flex-direction: column;
		gap: 8px;
		padding: 8px;
		cursor: pointer;
		min-width: 0;
	}

	.main:hover {
		background: rgba(0, 0, 0, 0.02);
	}

	.header-line {
		display: flex;
		align-items: center;
	}

	.name {
		font-size: 14px;
		font-weight: 500;
		color: #374151;
		min-width: 0;
		overflow: hidden;
		text-overflow: ellipsis;
		white-space: nowrap;
	}

	.desire-line {
		display: flex;
		align-items: center;
		gap: 16px;
		flex-wrap: wrap;
	}

	.our-desire,
	.their-desire {
		display: flex;
		align-items: center;
		gap: 6px;
	}

	.desire-label {
		font-size: 11px;
		color: #6b7280;
		font-weight: 500;
		min-width: fit-content;
	}

	.input-group {
		display: flex;
		align-items: center;
		gap: 2px;
	}

	input {
		width: 50px;
		padding: 2px 4px;
		border: 1px solid #d1d5db;
		border-radius: 3px;
		text-align: right;
		font-size: 12px;
		background: white;
	}

	input:focus {
		outline: none;
		border-color: #3b82f6;
		box-shadow: 0 0 0 1px rgba(59, 130, 246, 0.2);
	}

	.unit {
		font-size: 10px;
		color: #9ca3af;
		min-width: 20px;
	}

	.their-value {
		font-size: 12px;
		color: #374151;
		font-weight: 500;
	}

	.mutual-line {
		display: flex;
		align-items: center;
		gap: 6px;
	}

	.mutual-label {
		font-size: 11px;
		color: #7c3aed;
		font-weight: 500;
	}

	.mutual-value {
		font-size: 12px;
		color: #7c3aed;
		font-weight: 600;
	}

	.no-mutual {
		font-size: 11px;
		color: #9ca3af;
		font-style: italic;
	}

	.details {
		border-top: 1px solid #e5e7eb;
		background: #f9fafb;
		padding: 8px;
	}

	.analysis {
		font-size: 11px;
		color: #6b7280;
		margin-bottom: 8px;
	}

	.analysis > div {
		margin-bottom: 2px;
	}

	.mutual-info {
		margin-top: 4px;
		padding-top: 4px;
		border-top: 1px solid #e5e7eb;
	}

	.mutual-info > div {
		color: #7c3aed;
	}

	.constraint-info {
		color: #7c3aed;
		font-weight: 500;
		font-size: 10px;
	}

	.warning {
		color: #dc2626;
		font-weight: 500;
		font-size: 10px;
		background: #fef2f2;
		padding: 3px 6px;
		border-radius: 3px;
		border: 1px solid #fecaca;
		margin-top: 4px;
	}
</style>
