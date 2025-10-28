<script lang="ts">
	import { getUserName } from '$lib/state/users.svelte';
	import { getColorForUserId } from '$lib/utils/colorUtils';
	import Chat from '$lib/components/Chat.svelte';
	import { getReactiveUnreadCount } from '$lib/state/chat.svelte';
	import { handleAddressClick } from '$lib/utils/mapUtils';
	// V5: Import mutual recognition from v5 stores
	import { myMutualRecognition } from '$lib/commons/v5/stores.svelte';
	// V5: Composition feature not yet implemented
	// import { userDesiredSlotComposeFrom } from '$lib/state/core.svelte';
	import { userPub } from '$lib/state/auth.svelte';
	import { get } from 'svelte/store';
	// V5: Import slot utility functions from v5 protocol
	import {
		getSlotAllocatedQuantity,
		getSlotAvailableQuantity,
		getAllocatedSlotCount,
		getTotalSlotCount,
		getTotalAllocated,
		getTotalAvailable,
		isSlotRecurring,
		getRecurrenceDisplay,
		isSlotInPast,
		formatSlotTimeDisplay,
		formatSlotLocationDisplay,
		getSlotSortValue
	} from '$lib/commons/v5/protocol';

	interface Props {
		share: any; // Using any since we're now working with inventory data, not RecipientCapacity
		expanded?: boolean;
		onToggle?: () => void;
		onProviderClick?: (provider: string) => void;
	}

	let { share, expanded = false, onToggle, onProviderClick }: Props = $props();

	let providerName = $state<string>('');

	// UI state for expanded slots
	let slotsExpanded = $state(false);

	// Slot filtering and ordering state
	let slotFilterBy = $state<'all' | 'time' | 'location' | 'quantity'>('all');
	let slotSortBy = $state<'time' | 'location' | 'quantity'>('time');
	let slotSortDirection = $state<'asc' | 'desc'>('asc');

	// Section expansion state
	let pastSlotsExpanded = $state(false);
	let recurringSlotsExpanded = $state(true);
	let currentFutureSlotsExpanded = $state(true);

	// Get provider name asynchronously
	$effect(() => {
		void (async () => {
			if (share.provider_id) {
				const name = await getUserName(share.provider_id);
				providerName = name && name.length > 10 ? name.substring(0, 10) + '...' : name || '';
			}
		})();
	});

	// Get reactive unread message count for this share's chat
	let unreadCount = getReactiveUnreadCount(getChatId(share));

	// Green color scale for share percentage
	const colors = ['#dcfce7', '#86efac', '#22c55e'];

	function getShareColor(percentage: number): string {
		if (percentage <= 0) return '#f3f4f6';
		const index = Math.floor(percentage * colors.length);
		return colors[Math.min(index, colors.length - 1)];
	}

	function handleProviderClick(provider: string) {
		if (onProviderClick) {
			onProviderClick(provider);
		} else {
			console.log(`Navigating to provider: ${provider}`);
		}
	}

	// Calculate mutual recognition share for a slot: provider total quantity * user mutual-rec share
	function getSlotMutualRecognitionShare(share: any, slotId: string): number {
		// V5: Use capacity_slots instead of availability_slots
		const slot = share.capacity_slots?.find((s: any) => s.id === slotId);
		if (!slot) return 0;

		const providerId = share.provider_id;
		if (!providerId) return 0;

		// Get the user's mutual recognition share with this provider (v5)
		const userMutualRecShare = $myMutualRecognition[providerId] || 0;

		// Calculate: slot total quantity * mutual recognition share
		const totalQuantity = slot.quantity || 0;
		const mutualRecShare = totalQuantity * userMutualRecShare;

		return mutualRecShare;
	}

	// Toggle expanded state
	function toggleExpanded() {
		expanded = !expanded;
		// If we're expanding main content, close slots
		if (expanded) {
			slotsExpanded = false;
		}
	}

	// Toggle chat state
	function toggleChat() {
		expanded = !expanded;
		// If we're expanding chat, close slots
		if (expanded) {
			slotsExpanded = false;
		}
	}

	// Toggle slots expansion
	function toggleSlots() {
		slotsExpanded = !slotsExpanded;
		// If we're expanding slots, close the main expanded section
		if (slotsExpanded && expanded) {
			expanded = false;
		}
	}

	// Direct use of protocol functions - no wrappers needed!

	// Note: Time and location formatting functions moved to protocol.ts for reusability

	function getChatId(share: any): string {
		// Use capacity ID as the chat ID for all conversations about this capacity
		return share.id;
	}

	// Handle online link clicks
	function handleOnlineLinkClick(slot: any) {
		if (slot.online_link) {
			if (slot.online_link.startsWith('http')) {
				// It's a URL - open in new tab
				window.open(slot.online_link, '_blank', 'noopener,noreferrer');
			} else {
				// It's text - copy to clipboard
				navigator.clipboard
					.writeText(slot.online_link)
					.then(() => {
						// Could show a toast here if needed
						console.log('Online meeting details copied to clipboard');
					})
					.catch(() => {
						console.log('Failed to copy to clipboard');
					});
			}
		}
	}

	// Handle location click (combines address and online link handling)
	function handleLocationClick(slot: any) {
		if (slot.location_type === 'Online') {
			handleOnlineLinkClick(slot);
		} else {
			handleAddressClick(slot);
		}
	}

	// Note: Slot utility functions moved to protocol.ts for reusability

	// Categorize and sort slots
	let categorizedSlots = $derived(() => {
		// V5: Use capacity_slots instead of availability_slots
		if (!share.capacity_slots || !Array.isArray(share.capacity_slots)) {
			return { past: [], recurring: [], currentFuture: [] };
		}

		const past: any[] = [];
		const recurring: any[] = [];
		const currentFuture: any[] = [];

		// Categorize slots
		// V5: Use capacity_slots instead of availability_slots
		share.capacity_slots.forEach((slot: any) => {
			if (isSlotRecurring(slot)) {
				recurring.push(slot);
			} else if (isSlotInPast(slot)) {
				past.push(slot);
			} else {
				currentFuture.push(slot);
			}
		});

		// Sort each category
		const sortFn = (a: any, b: any) => {
			const aValue = getSlotSortValue(a, slotSortBy);
			const bValue = getSlotSortValue(b, slotSortBy);

			if (typeof aValue === 'string' && typeof bValue === 'string') {
				const comparison = aValue.localeCompare(bValue);
				return slotSortDirection === 'asc' ? comparison : -comparison;
			}

			const comparison = (aValue as number) - (bValue as number);
			return slotSortDirection === 'asc' ? comparison : -comparison;
		};

		past.sort(sortFn);
		recurring.sort(sortFn);
		currentFuture.sort(sortFn);

		return { past, recurring, currentFuture };
	});

	// Get total slot count for display
	let totalSlotCount = $derived(() => {
		const { past, recurring, currentFuture } = categorizedSlots();
		return past.length + recurring.length + currentFuture.length;
	});

	// V5: Composition features not yet implemented - commented out
	// TODO: Re-enable when v5 composition is implemented
	// function getCurrentDesiredAmount(capacityId: string, slotId: string): number {
	// 	return 0;
	// }

	// function handleDesireChange(capacityId: string, slotId: string, newAmount: number) {
	// 	console.log('[DESIRE] Composition not yet implemented in v5');
	// }
</script>

<div class="capacity-share-container">
	<div
		class="capacity-share flex cursor-pointer items-center justify-between rounded p-2 shadow-sm"
		style="background-color: {getShareColor(
			getTotalAllocated(share) / Math.max(getTotalAvailable(share), 1)
		)}; border: 1px solid #e5e7eb; border-left: 8px solid {getColorForUserId(share.provider_id)};"
		role="button"
		tabindex="0"
		onclick={toggleSlots}
		onkeydown={(e) => {
			if (e.key === 'Enter' || e.key === ' ') {
				e.preventDefault();
				toggleSlots();
			}
		}}
	>
		<div class="flex min-w-0 flex-1 flex-col pr-2">
			<div class="share-main-info flex items-center gap-2 overflow-hidden">
				<span class="share-value name flex-shrink-0 font-medium text-ellipsis whitespace-nowrap">
					<span class="capacity-emoji">{share.emoji || '🎁'}</span>
					{share.name}
				</span>
				<span class="share-value allocation-info flex-shrink-0 text-sm">
					{#if getTotalAllocated(share) > 0}
						<span class="ml-1 text-xs font-medium text-green-700">
							{getTotalAllocated(share)}{share.unit ? ' ' + share.unit : ''} allocated
						</span>
					{:else}
						<span class="ml-1 text-xs text-gray-600">
							{getTotalAvailable(share)}{share.unit ? ' ' + share.unit : ''} available
						</span>
					{/if}
				</span>
			</div>
			<!-- Show description preview if available -->
			{#if share.description}
				<div class="description-preview mt-1 text-xs text-gray-600">
					<span class="description-text">
						{share.description.length > 50
							? share.description.substring(0, 50) + '...'
							: share.description}
					</span>
				</div>
			{/if}
		</div>
		<div class="flex items-center gap-2">
			<!-- Chat button -->
			<button
				type="button"
				class="chat-btn relative"
				onclick={(e) => {
					e.stopPropagation();
					toggleChat();
				}}
				title="Chat about this capacity"
			>
				💬
				{#if $unreadCount > 0 && !expanded}
					<span class="unread-badge-btn">{$unreadCount > 99 ? '99+' : $unreadCount}</span>
				{/if}
			</button>
			<button
				type="button"
				class="provider-btn rounded-md text-xs font-medium whitespace-nowrap"
				style="background-color: {getColorForUserId(
					share.provider_id
				)}; color: white; border: none; padding: 6px 16px;"
				onclick={(e) => {
					e.stopPropagation();
					handleProviderClick(providerName);
				}}
			>
				{providerName}
			</button>
			<span class="expand-icon text-sm">
				{slotsExpanded ? '▼' : '▶'}
			</span>
		</div>
	</div>

	<!-- Expanded slots section -->
	{#if slotsExpanded}
		<div class="slots-section mt-2 rounded border border-gray-200 bg-purple-50 p-3">
			<div class="slots-header mb-3">
				<h4 class="text-sm font-medium text-gray-700">🕒 Available Capacity Slots</h4>
				<p class="mt-1 text-xs text-gray-500">
					{#if getTotalAllocated(share) > 0}
						Your current allocation: {getTotalAllocated(share)}{share.unit ? ' ' + share.unit : ''} of
						{getTotalAvailable(share)}{share.unit ? ' ' + share.unit : ''} total ({getTotalSlotCount(
							share
						)}
						slots)
					{:else}
						Available for allocation: {getTotalAvailable(share)}{share.unit ? ' ' + share.unit : ''}
						({getTotalSlotCount(share)}
						slots) - express desire to get some!
					{/if}
				</p>
			</div>

			<!-- Slot controls -->
			<div class="slots-controls mb-4 rounded border border-gray-200 bg-white p-3">
				<div class="flex flex-wrap items-center gap-2">
					<span class="text-xs font-medium text-gray-600">Sort by:</span>
					<select class="slot-control-select" bind:value={slotSortBy}>
						<option value="time">Time</option>
						<option value="location">Location</option>
						<option value="quantity">Quantity</option>
					</select>
					<button
						class="slot-control-btn"
						onclick={() => (slotSortDirection = slotSortDirection === 'asc' ? 'desc' : 'asc')}
						title="Toggle sort direction"
					>
						{slotSortDirection === 'asc' ? '↑' : '↓'}
					</button>
				</div>
			</div>

			<div class="slots-content space-y-4">
				{#if totalSlotCount() > 0}
					<!-- Recurring slots section -->
					{#if categorizedSlots().recurring.length > 0}
						<div class="slot-category">
							<button
								class="category-header"
								onclick={() => (recurringSlotsExpanded = !recurringSlotsExpanded)}
							>
								<span class="category-icon">{recurringSlotsExpanded ? '▼' : '▶'}</span>
								<span class="category-title"
									>🔄 Recurring ({categorizedSlots().recurring.length})</span
								>
							</button>
							{#if recurringSlotsExpanded}
								<div class="category-content">
									{#each categorizedSlots().recurring as slot (slot.id)}
										{@const allocatedQuantity = getSlotAllocatedQuantity(share, slot.id)}
										{@const availableQuantity = getSlotAvailableQuantity(share, slot.id)}
										{@const mutualRecShare = getSlotMutualRecognitionShare(share, slot.id)}
										<div class="slot-item rounded border border-gray-200 bg-white p-3 shadow-sm">
											<!-- Slot header row -->
											<div class="slot-header mb-2 flex flex-wrap items-center gap-2">
												{#if allocatedQuantity > 0}
													<!-- Your allocated quantity -->
													<span
														class="slot-quantity flex-shrink-0 rounded bg-green-100 px-2 py-1 text-sm font-medium text-green-800"
													>
														{Number.isInteger(allocatedQuantity)
															? allocatedQuantity
															: allocatedQuantity.toFixed(2)}
														{share.unit} allocated
													</span>
												{:else}
													<!-- Available for allocation -->
													<span
														class="slot-quantity flex-shrink-0 rounded bg-gray-100 px-2 py-1 text-sm font-medium text-gray-700"
													>
														{Number.isInteger(availableQuantity)
															? availableQuantity
															: availableQuantity.toFixed(2)}
														{share.unit} available
													</span>
												{/if}

												<!-- Total slot quantity for context -->
												<span class="slot-total flex-shrink-0 text-xs text-gray-500">
													of {availableQuantity} total
												</span>

												{#if mutualRecShare > 0}
													<!-- Mutual recognition share -->
													<span
														class="slot-share flex-shrink-0 rounded bg-purple-100 px-2 py-1 text-xs font-medium text-purple-800"
													>
														Share: {Number.isInteger(mutualRecShare)
															? mutualRecShare
															: mutualRecShare.toFixed(2)}
														{share.unit}
													</span>
												{/if}

												<!-- Recurrence indicator -->
												<span
													class="recurrence-badge flex-shrink-0 rounded bg-blue-100 px-2 py-1 text-xs text-blue-800"
												>
											{getRecurrenceDisplay(slot)}
										</span>

										<!-- V5: Composition/desire features not yet implemented -->
										<!-- <div class="desire-input-wrapper flex-shrink-0">...</div> -->
									</div>

									<!-- Slot summary row -->
											<div class="slot-summary mb-2 text-xs text-gray-500">
												<div class="flex flex-wrap gap-4">
													<span>⏰ {formatSlotTimeDisplay(slot)}</span>
													<button
														class="location-link"
														onclick={(e) => {
															e.stopPropagation();
															handleLocationClick(slot);
														}}
														title={slot.location_type === 'Online'
															? 'Click to open link or copy details'
															: 'Click to open in maps or copy address'}
													>
														📍 {formatSlotLocationDisplay(slot)}
													</button>
													{#if slot.mutual_agreement_required}
														<span>🤝 Mutual agreement required</span>
													{/if}
												</div>
											</div>

											<!-- Constraints info if present -->
											{#if slot.advance_notice_hours || slot.booking_window_hours}
												<div class="slot-constraints text-xs text-gray-500">
													{#if slot.advance_notice_hours}
														<span>⏳ {slot.advance_notice_hours}h advance notice required</span>
													{/if}
													{#if slot.booking_window_hours}
														<span>📅 Book within {slot.booking_window_hours}h window</span>
													{/if}
												</div>
											{/if}
										</div>
									{/each}
								</div>
							{/if}
						</div>
					{/if}

					<!-- Current/Future slots section -->
					{#if categorizedSlots().currentFuture.length > 0}
						<div class="slot-category">
							<button
								class="category-header"
								onclick={() => (currentFutureSlotsExpanded = !currentFutureSlotsExpanded)}
							>
								<span class="category-icon">{currentFutureSlotsExpanded ? '▼' : '▶'}</span>
								<span class="category-title"
									>📅 Current & Upcoming ({categorizedSlots().currentFuture.length})</span
								>
							</button>
							{#if currentFutureSlotsExpanded}
								<div class="category-content">
									{#each categorizedSlots().currentFuture as slot (slot.id)}
										{@const allocatedQuantity = getSlotAllocatedQuantity(share, slot.id)}
										{@const availableQuantity = getSlotAvailableQuantity(share, slot.id)}
										{@const mutualRecShare = getSlotMutualRecognitionShare(share, slot.id)}
										<div class="slot-item rounded border border-gray-200 bg-white p-3 shadow-sm">
											<!-- Slot header row -->
											<div class="slot-header mb-2 flex flex-wrap items-center gap-2">
												{#if allocatedQuantity > 0}
													<!-- Your allocated quantity -->
													<span
														class="slot-quantity flex-shrink-0 rounded bg-green-100 px-2 py-1 text-sm font-medium text-green-800"
													>
														{Number.isInteger(allocatedQuantity)
															? allocatedQuantity
															: allocatedQuantity.toFixed(2)}
														{share.unit} allocated
													</span>
												{:else}
													<!-- Available for allocation -->
													<span
														class="slot-quantity flex-shrink-0 rounded bg-blue-100 px-2 py-1 text-sm font-medium text-blue-800"
													>
														{Number.isInteger(availableQuantity)
															? availableQuantity
															: availableQuantity.toFixed(2)}
														{share.unit} available
													</span>
												{/if}

												<!-- Total slot quantity for context -->
												<span class="slot-total flex-shrink-0 text-xs text-gray-500">
													of {availableQuantity} total
												</span>

												{#if mutualRecShare > 0}
													<!-- Mutual recognition share -->
													<span
														class="slot-share flex-shrink-0 rounded bg-purple-100 px-2 py-1 text-xs font-medium text-purple-800"
													>
														Share: {Number.isInteger(mutualRecShare)
															? mutualRecShare
															: mutualRecShare.toFixed(2)}
												{share.unit}
												</span>
											{/if}

											<!-- V5: Composition/desire features not yet implemented -->
											<!-- <div class="desire-input-wrapper flex-shrink-0">...</div> -->
										</div>

										<!-- Slot summary row -->
											<div class="slot-summary mb-2 text-xs text-gray-500">
												<div class="flex flex-wrap gap-4">
													<span>⏰ {formatSlotTimeDisplay(slot)}</span>
													<button
														class="location-link"
														onclick={(e) => {
															e.stopPropagation();
															handleLocationClick(slot);
														}}
														title={slot.location_type === 'Online'
															? 'Click to open link or copy details'
															: 'Click to open in maps or copy address'}
													>
														📍 {formatSlotLocationDisplay(slot)}
													</button>
													{#if slot.mutual_agreement_required}
														<span>🤝 Mutual agreement required</span>
													{/if}
												</div>
											</div>

											<!-- Constraints info if present -->
											{#if slot.advance_notice_hours || slot.booking_window_hours}
												<div class="slot-constraints text-xs text-gray-500">
													{#if slot.advance_notice_hours}
														<span>⏳ {slot.advance_notice_hours}h advance notice required</span>
													{/if}
													{#if slot.booking_window_hours}
														<span>📅 Book within {slot.booking_window_hours}h window</span>
													{/if}
												</div>
											{/if}
										</div>
									{/each}
								</div>
							{/if}
						</div>
					{/if}

					<!-- Past slots section -->
					{#if categorizedSlots().past.length > 0}
						<div class="slot-category">
							<button
								class="category-header"
								onclick={() => (pastSlotsExpanded = !pastSlotsExpanded)}
							>
								<span class="category-icon">{pastSlotsExpanded ? '▼' : '▶'}</span>
								<span class="category-title">📜 Past ({categorizedSlots().past.length})</span>
							</button>
							{#if pastSlotsExpanded}
								<div class="category-content">
									{#each categorizedSlots().past as slot (slot.id)}
										{@const allocatedQuantity = getSlotAllocatedQuantity(share, slot.id)}
										{@const availableQuantity = getSlotAvailableQuantity(share, slot.id)}
										{@const mutualRecShare = getSlotMutualRecognitionShare(share, slot.id)}
										<div
											class="slot-item rounded border border-gray-200 bg-gray-50 p-3 opacity-75 shadow-sm"
										>
											<!-- Slot header row -->
											<div class="slot-header mb-2 flex flex-wrap items-center gap-2">
												{#if allocatedQuantity > 0}
													<!-- Your allocated quantity (past) -->
													<span
														class="slot-quantity flex-shrink-0 rounded bg-gray-200 px-2 py-1 text-sm font-medium text-gray-700"
													>
														{Number.isInteger(allocatedQuantity)
															? allocatedQuantity
															: allocatedQuantity.toFixed(2)}
														{share.unit} was allocated
													</span>
												{:else}
													<!-- Was available (past) -->
													<span
														class="slot-quantity flex-shrink-0 rounded bg-gray-100 px-2 py-1 text-sm font-medium text-gray-600"
													>
														{Number.isInteger(availableQuantity)
															? availableQuantity
															: availableQuantity.toFixed(2)}
														{share.unit} was available
													</span>
												{/if}

												<!-- Total slot quantity for context -->
												<span class="slot-total flex-shrink-0 text-xs text-gray-500">
													of {availableQuantity} total
												</span>

												{#if mutualRecShare > 0}
													<!-- Mutual recognition share -->
													<span
														class="slot-share flex-shrink-0 rounded bg-gray-200 px-2 py-1 text-xs font-medium text-gray-700"
													>
														Share: {Number.isInteger(mutualRecShare)
															? mutualRecShare
															: mutualRecShare.toFixed(2)}
														{share.unit}
													</span>
												{/if}

												<!-- Past slot - no desire input (disabled) -->
												<div class="desire-input-wrapper flex-shrink-0 opacity-50">
													<label
														for="past-slot-{share.id}-{slot.id}"
														class="desire-label text-xs text-gray-400">Past slot</label
													>
													<input
														id="past-slot-{share.id}-{slot.id}"
														type="number"
														class="desire-input"
														disabled
														value={0}
														placeholder="N/A"
													/>
												</div>
											</div>

											<!-- Slot summary row -->
											<div class="slot-summary mb-2 text-xs text-gray-500">
												<div class="flex flex-wrap gap-4">
													<span>⏰ {formatSlotTimeDisplay(slot)}</span>
													<button
														class="location-link"
														onclick={(e) => {
															e.stopPropagation();
															handleLocationClick(slot);
														}}
														title={slot.location_type === 'Online'
															? 'Click to open link or copy details'
															: 'Click to open in maps or copy address'}
													>
														📍 {formatSlotLocationDisplay(slot)}
													</button>
													{#if slot.mutual_agreement_required}
														<span>🤝 Mutual agreement required</span>
													{/if}
												</div>
											</div>

											<!-- Constraints info if present -->
											{#if slot.advance_notice_hours || slot.booking_window_hours}
												<div class="slot-constraints text-xs text-gray-500">
													{#if slot.advance_notice_hours}
														<span>⏳ {slot.advance_notice_hours}h advance notice required</span>
													{/if}
													{#if slot.booking_window_hours}
														<span>📅 Book within {slot.booking_window_hours}h window</span>
													{/if}
												</div>
											{/if}
										</div>
									{/each}
								</div>
							{/if}
						</div>
					{/if}
				{:else}
					<div class="empty-slots py-6 text-center text-xs text-gray-500 italic">
						No slots defined for this capacity.
					</div>
				{/if}
			</div>
		</div>
	{/if}

	{#if expanded}
		<div class="expanded-content mt-2 space-y-3">
			<!-- Description section -->
			{#if share.description}
				<div class="description-section rounded border border-gray-200 bg-gray-50 p-3">
					<div class="description-header mb-2">
						<h4 class="text-sm font-medium text-gray-700">📝 Description</h4>
					</div>
					<div class="description-content">
						<p class="text-sm leading-relaxed whitespace-pre-wrap text-gray-700">
							{share.description}
						</p>
					</div>
				</div>
			{/if}

			<!-- Chat section -->
			<div class="chat-container rounded border border-gray-200 bg-gray-50 p-3">
				<div class="chat-header mb-2">
					<h4 class="text-sm font-medium text-gray-700">
						💬 Chat about {share.name} with {providerName}
					</h4>
				</div>
				<Chat
					chatId={getChatId(share)}
					placeholder={`Message ${providerName} about ${share.name}...`}
					maxLength={200}
				/>
			</div>
		</div>
	{/if}
</div>

<style>
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

	.share-main-info {
		min-width: 0;
		flex: 1;
	}

	.share-main-info .name {
		overflow: hidden;
		min-width: 0;
	}

	.capacity-emoji {
		margin-right: 0.5rem;
		font-size: 1.1em;
		display: inline-block;
	}

	.chat-btn {
		background: none;
		border: 1px solid #e5e7eb;
		border-radius: 4px;
		padding: 2px 6px;
		color: #6b7280;
		font-size: 1em;
		cursor: pointer;
		transition: all 0.2s ease;
		width: 24px;
		height: 24px;
		display: flex;
		align-items: center;
		justify-content: center;
	}

	.chat-btn:hover {
		background: #f0f9ff;
		border-color: #3b82f6;
		color: #3b82f6;
		transform: scale(1.05);
	}

	.provider-btn {
		transition:
			opacity 0.2s,
			transform 0.2s;
		cursor: pointer;
		flex-shrink: 0;
		text-shadow: none;
		box-shadow: 0 1px 2px rgba(0, 0, 0, 0.1);
	}

	.provider-btn:hover {
		opacity: 0.9;
		transform: translateY(-1px);
		box-shadow: 0 2px 4px rgba(0, 0, 0, 0.15);
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

	.unread-badge-btn {
		position: absolute;
		top: -2px;
		right: -2px;
		background: #ef4444;
		color: white;
		font-size: 0.65rem;
		font-weight: 600;
		line-height: 1;
		min-width: 16px;
		height: 16px;
		border-radius: 8px;
		display: flex;
		align-items: center;
		justify-content: center;
		padding: 0 4px;
		box-shadow: 0 1px 3px rgba(0, 0, 0, 0.2);
		border: 1px solid white;
		z-index: 10;
	}

	.description-preview {
		line-height: 1.3;
	}

	.description-text {
		font-style: italic;
		color: #6b7280;
	}

	.description-section {
		animation: slideDown 0.2s ease-out;
	}

	.description-header h4 {
		margin: 0;
	}

	.description-content p {
		margin: 0;
		word-wrap: break-word;
	}

	.expanded-content {
		animation: slideDown 0.2s ease-out;
	}

	/* Slots section styling */
	.slots-section {
		animation: slideDown 0.2s ease-out;
	}

	.slots-header h4 {
		margin: 0;
	}

	.slots-header p {
		margin: 0;
	}

	.slot-item {
		animation: slideDown 0.2s ease-out;
	}

	.slot-quantity {
		font-family: 'SF Mono', 'Monaco', 'Inconsolata', 'Roboto Mono', monospace;
	}

	.slot-total {
		font-weight: 500;
	}

	.slot-summary span {
		display: inline-flex;
		align-items: center;
		gap: 2px;
	}

	.slot-constraints {
		display: flex;
		flex-wrap: wrap;
		gap: 1rem;
		margin-top: 0.5rem;
		padding-top: 0.5rem;
		border-top: 1px solid #f3f4f6;
	}

	.slot-constraints span {
		display: inline-flex;
		align-items: center;
		gap: 2px;
	}

	.empty-slots {
		background: rgba(243, 244, 246, 0.5);
		border-radius: 6px;
		border: 1px dashed #d1d5db;
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

	/* Slot controls styling */
	.slots-controls {
		animation: slideDown 0.2s ease-out;
	}

	.slot-control-select {
		padding: 4px 8px;
		border: 1px solid #d1d5db;
		border-radius: 4px;
		font-size: 0.75rem;
		background: white;
		color: #374151;
		cursor: pointer;
		transition: border-color 0.2s ease;
	}

	.slot-control-select:focus {
		outline: none;
		border-color: #3b82f6;
	}

	.slot-control-btn {
		padding: 4px 8px;
		border: 1px solid #d1d5db;
		border-radius: 4px;
		background: white;
		color: #6b7280;
		cursor: pointer;
		font-size: 0.75rem;
		font-weight: 600;
		transition: all 0.2s ease;
		display: flex;
		align-items: center;
		justify-content: center;
		width: 28px;
		height: 28px;
	}

	.slot-control-btn:hover {
		background: #f3f4f6;
		border-color: #9ca3af;
		color: #374151;
	}

	/* Slot category styling */
	.slot-category {
		border: 1px solid #e5e7eb;
		border-radius: 6px;
		background: white;
		overflow: hidden;
	}

	.category-header {
		width: 100%;
		display: flex;
		align-items: center;
		gap: 8px;
		padding: 12px 16px;
		background: #f9fafb;
		border: none;
		cursor: pointer;
		transition: all 0.2s ease;
		font-size: 0.875rem;
		font-weight: 500;
		text-align: left;
		border-bottom: 1px solid #e5e7eb;
	}

	.category-header:hover {
		background: #f3f4f6;
	}

	.category-icon {
		color: #6b7280;
		font-size: 0.75rem;
		transition: transform 0.2s ease;
	}

	.category-title {
		color: #374151;
		flex: 1;
	}

	.category-content {
		padding: 16px;
		background: white;
		animation: slideDown 0.2s ease-out;
	}

	.category-content .slot-item {
		margin-bottom: 12px;
	}

	.category-content .slot-item:last-child {
		margin-bottom: 0;
	}

	/* Badge styling */
	.recurrence-badge {
		font-family: 'SF Mono', 'Monaco', 'Inconsolata', 'Roboto Mono', monospace;
		font-size: 0.6rem;
		font-weight: 600;
		text-transform: uppercase;
		letter-spacing: 0.05em;
	}

	/* Location link styling */
	.location-link {
		background: none;
		border: none;
		color: inherit;
		font: inherit;
		cursor: pointer;
		padding: 2px 4px;
		border-radius: 4px;
		transition: all 0.2s ease;
		text-decoration: underline;
		text-decoration-color: transparent;
		display: inline-flex;
		align-items: center;
		gap: 2px;
	}

	.location-link:hover {
		background: rgba(59, 130, 246, 0.1);
		color: #3b82f6;
		text-decoration-color: #3b82f6;
		transform: translateY(-1px);
	}

	.location-link:active {
		transform: translateY(0);
		background: rgba(59, 130, 246, 0.15);
	}

	/* Desire input styling */
	.desire-input-wrapper {
		display: flex;
		align-items: center;
		gap: 4px;
		padding: 4px 8px;
		background: rgba(59, 130, 246, 0.05);
		border: 1px solid rgba(59, 130, 246, 0.2);
		border-radius: 6px;
		transition: all 0.2s ease;
	}

	.desire-input-wrapper:hover {
		background: rgba(59, 130, 246, 0.08);
		border-color: rgba(59, 130, 246, 0.3);
	}

	.desire-label {
		font-weight: 500;
		white-space: nowrap;
		color: #6b7280;
	}

	.desire-input {
		width: 60px;
		padding: 2px 6px;
		border: 1px solid rgba(59, 130, 246, 0.3);
		border-radius: 4px;
		background: white;
		font-size: 11px;
		font-weight: 600;
		text-align: center;
		color: #374151;
		transition: all 0.2s ease;
	}

	.desire-input:focus {
		outline: none;
		border-color: #3b82f6;
		box-shadow: 0 0 0 2px rgba(59, 130, 246, 0.2);
		background: #fefefe;
	}

	.desire-input:disabled {
		background: #f3f4f6;
		color: #9ca3af;
		border-color: #e5e7eb;
		cursor: not-allowed;
	}

	.desire-unit {
		font-weight: 500;
		color: #6b7280;
		white-space: nowrap;
	}

	/* Remove spinner arrows from number input */
	.desire-input::-webkit-outer-spin-button,
	.desire-input::-webkit-inner-spin-button {
		-webkit-appearance: none;
		appearance: none;
		margin: 0;
	}

	.desire-input[type='number'] {
		-moz-appearance: textfield;
		appearance: textfield;
	}
</style>
