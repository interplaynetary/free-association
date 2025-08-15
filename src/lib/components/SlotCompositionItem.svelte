<script lang="ts">
	import { get } from 'svelte/store';
	import {
		userDesiredSlotComposeFrom,
		userDesiredSlotComposeInto,
		networkDesiredSlotComposeFrom,
		networkDesiredSlotComposeInto
	} from '$lib/state/core.svelte';
	import {
		userNetworkCapacitiesWithSlotQuantities,
		networkCapacities,
		userCapacities
	} from '$lib/state/core.svelte';
	import Chat from '$lib/components/Chat.svelte';

	interface Props {
		sourceCapacityId: string;
		sourceSlotId: string;
		targetCapacityId: string;
		targetSlotId: string;
		direction: 'from' | 'into';
		currentCapacityId: string;
		currentSlotId: string;
		expanded?: boolean;
		onToggle?: () => void;
	}

	let {
		sourceCapacityId,
		sourceSlotId,
		targetCapacityId,
		targetSlotId,
		direction,
		currentCapacityId,
		currentSlotId,
		expanded = false,
		onToggle
	}: Props = $props();

	// Reactive state for desired quantity input
	let desiredQuantityInput = $state(0);

	// Track original values for change detection
	let originalValues = $state<Record<string, any>>({});

	// Helper to track original value on focus
	function handleFocus(fieldName: string, currentValue: any) {
		originalValues[fieldName] = currentValue;
	}

	// Helper to save only if value changed on blur
	function handleBlurIfChanged(fieldName: string, currentValue: any) {
		if (originalValues[fieldName] !== currentValue) {
			handleQuantityChange();
		}
	}

	// Get source and target capacity/slot info
	let sourceCapacity = $derived(() => {
		return (
			$userCapacities?.[sourceCapacityId] ||
			$userNetworkCapacitiesWithSlotQuantities[sourceCapacityId]
		);
	});

	let targetCapacity = $derived(() => {
		return (
			$userCapacities?.[targetCapacityId] ||
			$userNetworkCapacitiesWithSlotQuantities[targetCapacityId]
		);
	});

	let sourceSlot = $derived(() => {
		return sourceCapacity()?.availability_slots?.find((s: any) => s.id === sourceSlotId);
	});

	let targetSlot = $derived(() => {
		return targetCapacity()?.availability_slots?.find((s: any) => s.id === targetSlotId);
	});

	// Format slot display info
	function formatSlotInfo(slot: any): string {
		if (!slot) return 'Slot not found';
		const parts = [];
		if (slot.start_date) {
			parts.push(new Date(slot.start_date).toLocaleDateString());
		}
		if (!slot.all_day && slot.start_time) {
			parts.push(slot.start_time);
		}
		if (slot.all_day) {
			parts.push('All day');
		}
		return parts.length > 0 ? parts.join(' ') : 'No time set';
	}

	// Determine provider ID
	let providerId = $derived(() => {
		if (direction === 'from') {
			// For FROM: the source is from another provider, target is ours
			const sourceCap = $userNetworkCapacitiesWithSlotQuantities[sourceCapacityId];
			return (sourceCap as any)?.provider_id;
		} else {
			// For INTO: the target is another provider, source is ours
			return Object.keys($networkCapacities).find(
				(id) => $networkCapacities[id] && $networkCapacities[id][targetCapacityId]
			);
		}
	});

	// Update input when store changes - use correct store based on direction
	$effect(() => {
		if (direction === 'from') {
			// FROM: We want to compose FROM sourceSlot INTO targetSlot
			desiredQuantityInput =
				$userDesiredSlotComposeFrom[sourceCapacityId]?.[sourceSlotId]?.[targetCapacityId]?.[
					targetSlotId
				] || 0;
		} else {
			// INTO: We want to compose FROM sourceSlot INTO targetSlot (sourceSlot is ours)
			desiredQuantityInput =
				$userDesiredSlotComposeInto[sourceCapacityId]?.[sourceSlotId]?.[targetCapacityId]?.[
					targetSlotId
				] || 0;
		}
	});

	// Get their desire amount directly from network desires
	let theirDesireAmount = $derived(() => {
		const actualProviderId = providerId();
		if (!actualProviderId) return 0;

		if (direction === 'from') {
			// FROM: We want to compose FROM their sourceSlot INTO our targetSlot
			// Their perspective: They want to compose FROM their sourceSlot INTO our targetSlot
			// So we look at their ComposeInto desires (they want to compose INTO our slot)
			return (
				$networkDesiredSlotComposeInto[actualProviderId]?.[sourceCapacityId]?.[sourceSlotId]?.[
					targetCapacityId
				]?.[targetSlotId] || 0
			);
		} else {
			// INTO: We want to compose FROM our sourceSlot INTO their targetSlot
			// Their perspective: They want to compose FROM our sourceSlot INTO their targetSlot
			// So we look at their ComposeFrom desires (they want to compose FROM our slot)
			return (
				$networkDesiredSlotComposeFrom[actualProviderId]?.[sourceCapacityId]?.[sourceSlotId]?.[
					targetCapacityId
				]?.[targetSlotId] || 0
			);
		}
	});

	// Get unit for composition
	let compositionUnit = $derived(() => {
		return sourceCapacity()?.unit || 'units';
	});

	// Handle quantity changes
	function handleQuantityChange() {
		// Use the correct store based on direction
		const store = direction === 'from' ? userDesiredSlotComposeFrom : userDesiredSlotComposeInto;
		const current = get(store);

		// Convert empty/null/undefined to 0
		const desiredAmount =
			desiredQuantityInput === null ||
			desiredQuantityInput === undefined ||
			isNaN(desiredQuantityInput)
				? 0
				: desiredQuantityInput;

		// Initialize nested structure
		const updated = { ...current };
		if (!updated[sourceCapacityId]) updated[sourceCapacityId] = {};
		if (!updated[sourceCapacityId][sourceSlotId]) updated[sourceCapacityId][sourceSlotId] = {};
		if (!updated[sourceCapacityId][sourceSlotId][targetCapacityId])
			updated[sourceCapacityId][sourceSlotId][targetCapacityId] = {};

		updated[sourceCapacityId][sourceSlotId][targetCapacityId][targetSlotId] = desiredAmount;

		store.set(updated);
	}

	// Get color based on desire
	function getColor(desired: number): string {
		if (desired === 0) return 'transparent';
		return '#dcfce7'; // Light green for now
	}
</script>

<div class="slot-item" style="background: {getColor(desiredQuantityInput)}">
	<div
		class="main"
		role="button"
		tabindex="0"
		onclick={() => onToggle?.()}
		onkeydown={(e) => {
			if (e.key === 'Enter' || e.key === ' ') {
				e.preventDefault();
				onToggle?.();
			}
		}}
	>
		<!-- Line 1: Source and Target Slot Info -->
		<div class="header-line">
			<span class="slot-name">
				{sourceCapacity()?.emoji || '📦'}
				{sourceCapacity()?.name || 'Unknown'}
				<span class="slot-info">({formatSlotInfo(sourceSlot())})</span>
			</span>
			<span class="direction-arrow">→</span>
			<span class="slot-name">
				{targetCapacity()?.emoji || '📦'}
				{targetCapacity()?.name || 'Unknown'}
				<span class="slot-info">({formatSlotInfo(targetSlot())})</span>
			</span>
		</div>

		<!-- Line 2: Our Desire and Their Desire -->
		<div class="desire-line">
			<div class="our-desire">
				<label for="our-desire-input" class="desire-label">Our desire:</label>
				<div class="input-group">
					<input
						id="our-desire-input"
						type="number"
						min="0"
						step="0.1"
						bind:value={desiredQuantityInput}
						onfocus={() => handleFocus('quantity', desiredQuantityInput)}
						onblur={() => handleBlurIfChanged('quantity', desiredQuantityInput)}
						onclick={(e) => e.stopPropagation()}
						placeholder="0"
					/>
					<span class="unit">{compositionUnit()}</span>
				</div>
			</div>
			<div class="their-desire">
				<span class="desire-label">Their desire:</span>
				<span class="their-value">
					{#if theirDesireAmount() > 0}
						{theirDesireAmount().toFixed(1)} {compositionUnit()}
					{:else}
						— {compositionUnit()}
					{/if}
				</span>
			</div>
		</div>

		<!-- Line 3: Status -->
		<div class="status-line">
			{#if theirDesireAmount() > 0 && desiredQuantityInput > 0}
				<span class="status-good">✅ Mutual interest</span>
			{:else if desiredQuantityInput > 0}
				<span class="status-waiting">⏳ Waiting for their interest</span>
			{:else}
				<span class="status-none">💭 Express desire to activate</span>
			{/if}
		</div>

		<!-- Line 4: Provider warning if needed -->
		{#if !providerId()}
			<div class="warning-line">
				<span class="warning">⚠️ Provider not found - slot composition may not work</span>
			</div>
		{/if}
	</div>

	{#if expanded}
		<div class="details">
			<div class="analysis">
				<div>
					<strong>Source:</strong>
					{sourceCapacity()?.name} - {formatSlotInfo(sourceSlot())}
				</div>
				<div>
					<strong>Target:</strong>
					{targetCapacity()?.name} - {formatSlotInfo(targetSlot())}
				</div>
				<div><strong>Our desire:</strong> {desiredQuantityInput} {compositionUnit()}</div>
				<div>
					<strong>Their desire:</strong>
					{theirDesireAmount().toFixed(1)}
					{compositionUnit()}
				</div>
				<div>
					<strong>Direction:</strong>
					{direction === 'from' ? 'FROM other slot' : 'INTO other slot'}
				</div>
				{#if !providerId()}
					<div class="warning">⚠️ Provider not found - slot composition may not work</div>
				{/if}
			</div>

			<Chat
				chatId="slot-composition-{sourceCapacityId}-{sourceSlotId}-{targetCapacityId}-{targetSlotId}-{direction}"
				placeholder="Discuss slot composition..."
				maxLength={200}
			/>
		</div>
	{/if}
</div>

<style>
	.slot-item {
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
		gap: 8px;
		flex-wrap: wrap;
	}

	.slot-name {
		font-size: 14px;
		font-weight: 500;
		color: #374151;
		min-width: 0;
		overflow: hidden;
		text-overflow: ellipsis;
		white-space: nowrap;
		flex: 1;
	}

	.slot-info {
		font-size: 12px;
		font-weight: 400;
		color: #6b7280;
	}

	.direction-arrow {
		font-size: 16px;
		color: #7c3aed;
		font-weight: 700;
		flex-shrink: 0;
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

	.status-line {
		display: flex;
		align-items: center;
		gap: 6px;
	}

	.status-good {
		font-size: 11px;
		color: #10b981;
		font-weight: 500;
	}

	.status-waiting {
		font-size: 11px;
		color: #f59e0b;
		font-weight: 500;
	}

	.status-none {
		font-size: 11px;
		color: #6b7280;
		font-weight: 500;
	}

	.warning-line {
		display: flex;
		align-items: center;
	}

	.warning {
		color: #dc2626;
		font-weight: 500;
		font-size: 10px;
		background: #fef2f2;
		padding: 3px 6px;
		border-radius: 3px;
		border: 1px solid #fecaca;
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
</style>
