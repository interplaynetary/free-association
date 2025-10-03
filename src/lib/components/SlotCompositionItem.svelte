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
	import { userAliasesCache } from '$lib/state/users.svelte';
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

	// Determine target type (capacity, pubkey, or collective)
	let targetType = $derived(() => {
		// Check if target is a pubkey (64 hex characters)
		if (/^[0-9a-fA-F]{64}$/.test(targetCapacityId)) {
			return 'pubkey';
		}
		// Check if target is a collective
		if (targetCapacityId.startsWith('collective:')) {
			return 'collective';
		}
		// Default to capacity
		return 'capacity';
	});

	// Get source and target capacity/slot info
	let sourceCapacity = $derived(() => {
		return (
			$userCapacities?.[sourceCapacityId] ||
			$userNetworkCapacitiesWithSlotQuantities[sourceCapacityId]
		);
	});

	let targetCapacity = $derived(() => {
		// Only lookup capacity if target is actually a capacity ID
		if (targetType() === 'capacity') {
			return (
				$userCapacities?.[targetCapacityId] ||
				$userNetworkCapacitiesWithSlotQuantities[targetCapacityId]
			);
		}
		return null;
	});

	let sourceSlot = $derived(() => {
		return sourceCapacity()?.availability_slots?.find((s: any) => s.id === sourceSlotId);
	});

	let targetSlot = $derived(() => {
		// Only lookup slot if target is a capacity
		if (targetType() === 'capacity' && targetCapacity()) {
			return targetCapacity()?.availability_slots?.find((s: any) => s.id === targetSlotId);
		}
		return null;
	});

	// Format slot display info
	function formatSlotInfo(slot: any): string {
		if (!slot) return 'Slot not found';
		const parts = [];

		// Handle start_date - this could be a string date or an ISO string
		if (slot.start_date) {
			try {
				const date = new Date(slot.start_date);
				parts.push(date.toLocaleDateString());
			} catch (e) {
				// If parsing fails, use the raw string
				parts.push(String(slot.start_date));
			}
		}

		// Handle start_time if not all day
		if (!slot.all_day && slot.start_time) {
			// Handle different time formats
			if (typeof slot.start_time === 'string') {
				if (slot.start_time.includes('T')) {
					// Handle ISO format dates
					try {
						const date = new Date(slot.start_time);
						parts.push(date.toLocaleTimeString([], { hour: '2-digit', minute: '2-digit' }));
					} catch (e) {
						// If parsing fails, use the raw string but truncated
						const timeStr = slot.start_time;
						parts.push(timeStr.split('T')[1]?.substring(0, 5) || timeStr);
					}
				} else if (/^\d{2}:\d{2}(:\d{2})?$/.test(slot.start_time)) {
					// Already in HH:MM or HH:MM:SS format
					parts.push(slot.start_time.substring(0, 5)); // Take just HH:MM
				} else {
					// Unknown format, use as is
					parts.push(slot.start_time);
				}
			}
		}

		// Handle all_day flag
		if (slot.all_day) {
			parts.push('All day');
		}

		// Clean up any duplicate information
		// If we see the full ISO string in the output, remove it
		const result = parts.join(' ');
		if (slot.start_date && typeof slot.start_date === 'string' && slot.start_date.includes('T')) {
			// If the raw ISO string appears in the result, remove it
			return result.replace(slot.start_date, '').trim();
		}

		return result || 'No time set';
	}

	// Format target display based on target type
	function formatTargetDisplay(): string {
		const type = targetType();

		if (type === 'pubkey') {
			// For pubkey targets, show user-friendly name
			const userName =
				$userAliasesCache[targetCapacityId] || `${targetCapacityId.substring(0, 8)}...`;
			return `👤 ${userName}`;
		} else if (type === 'collective') {
			// For collective targets, show member count
			const members = targetCapacityId.slice(11).split(','); // Remove "collective:" prefix
			return `👥 Collective (${members.length} members)`;
		} else {
			// For capacity targets, show capacity name and slot info
			const capacity = targetCapacity();
			const slot = targetSlot();
			if (capacity && slot) {
				return `${capacity.emoji || '🎁'} ${capacity.name} (${formatSlotInfo(slot)})`;
			} else if (capacity) {
				return `${capacity.emoji || '🎁'} ${capacity.name}`;
			} else {
				return `🎁 ${targetCapacityId} (capacity not found)`;
			}
		}
	}

	// Determine provider ID
	let providerId = $derived(() => {
		if (direction === 'from') {
			// For FROM: the source is from another provider, target is ours
			const sourceCap = $userNetworkCapacitiesWithSlotQuantities[sourceCapacityId];
			return (sourceCap as any)?.provider_id;
		} else {
			// For INTO: determine provider based on target type
			const type = targetType();

			if (type === 'pubkey') {
				// For pubkey targets, the target IS the provider
				return targetCapacityId;
			} else if (type === 'collective') {
				// For collective targets, we need to handle multiple recipients
				// For now, return the collective identifier
				return targetCapacityId;
			} else {
				// For capacity targets, find the provider who owns this capacity
				return Object.keys($networkCapacities).find(
					(id) => $networkCapacities[id] && $networkCapacities[id][targetCapacityId]
				);
			}
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
		const type = targetType();

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
			// INTO: We want to compose FROM our sourceSlot INTO their target
			if (type === 'pubkey') {
				// For pubkey targets, they express desire via Share.svelte (compose-from pattern)
				// Look for their desire: FROM our slot TO themselves
				// The targetSlotId for pubkey targets should be the same as sourceSlotId for self-consumption
				return (
					$networkDesiredSlotComposeFrom[actualProviderId]?.[sourceCapacityId]?.[sourceSlotId]?.[
						actualProviderId
					]?.[sourceSlotId] || 0
				);
			} else {
				// For capacity/collective targets, use the normal pattern
				// Their perspective: They want to compose FROM our sourceSlot INTO their targetSlot
				// So we look at their ComposeFrom desires (they want to compose FROM our slot)
				return (
					$networkDesiredSlotComposeFrom[actualProviderId]?.[sourceCapacityId]?.[sourceSlotId]?.[
						targetCapacityId
					]?.[targetSlotId] || 0
				);
			}
		}
	});

	// Calculate mutual desire (minimum of both parties' desires)
	let mutualDesireAmount = $derived(() => {
		const ourDesire = desiredQuantityInput || 0;
		const theirDesire = theirDesireAmount() || 0;
		return Math.min(ourDesire, theirDesire);
	});

	// Calculate desire alignment (how well desires match)
	let desireAlignment = $derived(() => {
		const ourDesire = desiredQuantityInput || 0;
		const theirDesire = theirDesireAmount() || 0;
		if (ourDesire === 0 || theirDesire === 0) return 0;
		return Math.min(ourDesire, theirDesire) / Math.max(ourDesire, theirDesire);
	});

	// Get unit for composition
	let compositionUnit = $derived(() => {
		return sourceCapacity()?.unit || 'units';
	});

	// Handle quantity changes
	function handleQuantityChange() {
		console.log(`[COMPOSE] [UI] Handling quantity change for ${direction} composition...`);
		console.log(
			`[COMPOSE] [UI] Source: ${sourceCapacityId}:${sourceSlotId} -> Target: ${targetCapacityId}:${targetSlotId}`
		);
		console.log(`[COMPOSE] [UI] New desired quantity: ${desiredQuantityInput}`);

		// Get the correct stores based on direction
		const dataStore =
			direction === 'from' ? userDesiredSlotComposeFrom : userDesiredSlotComposeInto;
		const current = get(dataStore);

		console.log(`[COMPOSE] [UI] Current ${direction} store data:`, current);

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

		console.log(`[COMPOSE] [UI] Updated ${direction} data:`, updated);

		// Update store (Gun handles timestamps natively now)
		dataStore.set(updated);

		console.log(`[COMPOSE] [UI] ✅ Store updated for ${direction} composition`);
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
		<!-- Line 1: Source and Target Info -->
		<div class="header-line">
			{#if direction === 'from'}
				<!-- FROM: Show what we're getting FROM -->
				<span class="slot-name">
					{sourceCapacity()?.emoji || '🎁'}
					{sourceCapacity()?.name || 'Unknown'}
					<span class="slot-info">({formatSlotInfo(sourceSlot())})</span>
				</span>
			{:else}
				<!-- INTO: Show what we're giving INTO -->
				<span class="slot-name">
					{formatTargetDisplay()}
				</span>
			{/if}
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

		<!-- Line 3: Status with Mutual Desire -->
		<div class="status-line">
			{#if mutualDesireAmount() > 0}
				<span class="status-good">
					✅ Mutual desire: {mutualDesireAmount().toFixed(1)}
					{compositionUnit()}
					{#if desireAlignment() < 1}
						<span
							class="alignment-indicator"
							title="Desire alignment: {(desireAlignment() * 100).toFixed(0)}%"
						>
							({(desireAlignment() * 100).toFixed(0)}% aligned)
						</span>
					{/if}
				</span>
			{:else if desiredQuantityInput > 0 && theirDesireAmount() > 0}
				<span class="status-mismatch"
					>⚠️ No mutual desire (our: {desiredQuantityInput}, their: {theirDesireAmount().toFixed(
						1
					)})</span
				>
			{:else if desiredQuantityInput > 0}
				<span class="status-waiting"
					>⏳ Waiting for their interest ({desiredQuantityInput} {compositionUnit()} desired)</span
				>
			{:else if theirDesireAmount() > 0}
				<span class="status-available"
					>💡 They want {theirDesireAmount().toFixed(1)} {compositionUnit()}</span
				>
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
					{formatTargetDisplay()}
				</div>
				<div>
					<strong>Target Type:</strong>
					{targetType()}
				</div>
				<div><strong>Our desire:</strong> {desiredQuantityInput} {compositionUnit()}</div>
				<div>
					<strong>Their desire:</strong>
					{theirDesireAmount().toFixed(1)}
					{compositionUnit()}
				</div>
				<div>
					<strong>Mutual desire:</strong>
					{mutualDesireAmount().toFixed(1)}
					{compositionUnit()}
					{#if mutualDesireAmount() > 0}
						<span class="mutual-info">
							({(desireAlignment() * 100).toFixed(0)}% aligned)
						</span>
					{/if}
				</div>
				<div>
					<strong>Direction:</strong>
					{direction === 'from' ? 'FROM other slot/target' : 'INTO other slot/target'}
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

	.status-available {
		font-size: 11px;
		color: #3b82f6;
		font-weight: 500;
	}

	.status-mismatch {
		font-size: 11px;
		color: #ef4444;
		font-weight: 500;
	}

	.status-none {
		font-size: 11px;
		color: #6b7280;
		font-weight: 500;
	}

	.alignment-indicator {
		font-size: 10px;
		color: #6b7280;
		font-weight: 400;
		margin-left: 4px;
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

	.mutual-info {
		font-size: 10px;
		color: #6b7280;
		font-weight: 400;
		margin-left: 4px;
	}
</style>
