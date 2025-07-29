<script lang="ts">
	import { getUserName } from '$lib/state/users.svelte';
	import { getColorForUserId } from '$lib/utils/colorUtils';
	import Chat from './Chat.svelte';
	import type { RecipientCapacity } from '$lib/schema';
	import { getReactiveUnreadCount } from '$lib/state/chat.svelte';

	interface Props {
		share: RecipientCapacity;
		expanded?: boolean;
		onToggle?: () => void;
		onProviderClick?: (provider: string) => void;
	}

	let { share, expanded = false, onToggle, onProviderClick }: Props = $props();

	let providerName = $state<string>('');

	// UI state for expanded slots
	let slotsExpanded = $state(false);

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

	// Get computed quantity for a specific slot
	function getSlotComputedQuantity(slotId: string): number {
		const slotQuantity = share.computed_quantities?.find((cq) => cq.slot_id === slotId);
		return slotQuantity?.quantity || 0;
	}

	// Get count of slots with non-zero quantities
	function getActiveSlotCount(): number {
		if (!share.computed_quantities || !Array.isArray(share.computed_quantities)) {
			return 0;
		}
		return share.computed_quantities.filter((slot) => slot.quantity > 0).length;
	}

	// Get total number of slots available
	function getTotalSlotCount(): number {
		return share.availability_slots?.length || 0;
	}

	// Format slot time display
	function formatSlotTimeDisplay(slot: any): string {
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

	// Format slot location display
	function formatSlotLocationDisplay(slot: any): string {
		if (slot.location_type === 'Specific') {
			if (slot.street_address) {
				return slot.street_address;
			}
			if (slot.latitude && slot.longitude) {
				return `${slot.latitude.toFixed(4)}, ${slot.longitude.toFixed(4)}`;
			}
		}

		return slot.location_type || 'No location';
	}

	function getChatId(share: RecipientCapacity): string {
		// Use capacity ID as the chat ID for all conversations about this capacity
		return share.id;
	}
</script>

<div class="capacity-share-container">
	<div
		class="capacity-share flex cursor-pointer items-center justify-between rounded p-2 shadow-sm"
		style="background-color: {getShareColor(
			share.share_percentage
		)}; border: 1px solid #e5e7eb; border-left: 8px solid {getColorForUserId(share.provider_id)};"
		onclick={toggleSlots}
	>
		<div class="flex min-w-0 flex-1 flex-col pr-2">
			<div class="share-main-info flex items-center gap-2 overflow-hidden">
				<span class="share-value name flex-shrink-0 font-medium text-ellipsis whitespace-nowrap">
					<span class="capacity-emoji">{share.emoji || '📦'}</span>
					{share.name}
				</span>
				<span class="share-value slot-count flex-shrink-0 text-sm">
					<span class="ml-1 text-xs text-gray-600"
						>({(share.share_percentage * 100).toFixed(1)}%)</span
					>
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
			{#if $unreadCount > 0}
				<span class="unread-badge">{$unreadCount > 99 ? '99+' : $unreadCount}</span>
			{/if}
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
			<!-- Slots button -->
			{#if share.availability_slots && share.availability_slots.length > 0}
				<button
					type="button"
					class="slots-btn"
					onclick={(e) => {
						e.stopPropagation();
						toggleSlots();
					}}
					title="View availability slots"
				>
					🕒
				</button>
			{/if}
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
				<h4 class="text-sm font-medium text-gray-700">🕒 Your Share Allocation</h4>
				<p class="mt-1 text-xs text-gray-500">
					Your {(share.share_percentage * 100).toFixed(1)}% share of each availability slot
				</p>
			</div>

			<div class="slots-content">
				{#if share.availability_slots && share.availability_slots.length > 0}
					<div class="slots-list space-y-3">
						{#each share.availability_slots as slot (slot.id)}
							{@const computedQuantity = getSlotComputedQuantity(slot.id)}
							<div class="slot-item rounded border border-gray-200 bg-white p-3 shadow-sm">
								<!-- Slot header row -->
								<div class="slot-header mb-2 flex flex-wrap items-center gap-2">
									<!-- Slot identifier -->
									<span class="slot-label flex-1 font-medium">
										Slot {slot.id.slice(-8)}
									</span>

									<!-- Your share quantity -->
									<span
										class="slot-quantity flex-shrink-0 rounded bg-green-100 px-2 py-1 text-sm font-medium text-green-800"
									>
										{Number.isInteger(computedQuantity)
											? computedQuantity
											: computedQuantity.toFixed(2)}
										{share.unit}
									</span>

									<!-- Total slot quantity for context -->
									<span class="slot-total flex-shrink-0 text-xs text-gray-500">
										of {slot.quantity} total
									</span>
								</div>

								<!-- Slot summary row -->
								<div class="slot-summary mb-2 text-xs text-gray-500">
									<div class="flex flex-wrap gap-4">
										<span>⏰ {formatSlotTimeDisplay(slot)}</span>
										<span>📍 {formatSlotLocationDisplay(slot)}</span>
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

	.slots-btn {
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

	.slots-btn:hover {
		background: #fdf4ff;
		border-color: #a855f7;
		color: #a855f7;
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

	/* Unread message badge */
	.unread-badge {
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
		flex-shrink: 0;
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

	.slots-preview {
		display: flex;
		align-items: center;
	}

	.slots-count {
		display: inline-flex;
		align-items: center;
		gap: 2px;
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

	.slots-list {
		max-height: 500px;
		overflow-y: auto;
	}

	.slot-item {
		animation: slideDown 0.2s ease-out;
	}

	.slot-label {
		font-weight: 500;
		color: #374151;
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
</style>
