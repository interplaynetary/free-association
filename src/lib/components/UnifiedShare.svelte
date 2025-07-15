<script lang="ts">
// We should use the state/inventory files to organize this
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

	// Helper functions for space-time data formatting
	function hasSpaceTimeData(share: RecipientCapacity): boolean {
		return Boolean(
			(share.location_type && share.location_type !== 'Undefined') ||
				share.start_date ||
				share.start_time ||
				share.end_date ||
				share.end_time ||
				(share.recurrence && share.recurrence !== 'Does not repeat')
		);
	}

	function formatLocation(share: RecipientCapacity): string {
		if (!share.location_type || share.location_type === 'Undefined') {
			return '';
		}

		if (share.location_type === 'LiveLocation') {
			return '📍 Live location';
		}

		if (share.location_type === 'Specific') {
			// Check if address is available first
			if (
				share.street_address ||
				share.city ||
				share.state_province ||
				share.postal_code ||
				share.country
			) {
				const addressParts = [
					share.street_address,
					share.city,
					share.state_province,
					share.postal_code,
					share.country
				].filter(Boolean);

				if (addressParts.length > 0) {
					return `📍 ${addressParts.join(', ')}`;
				}
			}

			// Fall back to coordinates if available
			if (share.latitude !== undefined && share.longitude !== undefined) {
				return `📍 ${share.latitude.toFixed(6)}, ${share.longitude.toFixed(6)}`;
			}

			return '📍 Specific location';
		}

		return '';
	}

	function formatTimeRange(share: RecipientCapacity): string {
		const parts: string[] = [];

		// Date range
		if (share.start_date || share.end_date) {
			const startDate = share.start_date ? new Date(share.start_date).toLocaleDateString() : '';
			const endDate = share.end_date ? new Date(share.end_date).toLocaleDateString() : '';

			if (startDate && endDate) {
				if (startDate === endDate) {
					parts.push(`📅 ${startDate}`);
				} else {
					parts.push(`📅 ${startDate} - ${endDate}`);
				}
			} else if (startDate) {
				parts.push(`📅 From ${startDate}`);
			} else if (endDate) {
				parts.push(`📅 Until ${endDate}`);
			}
		}

		// Time range (if not all day)
		if (!share.all_day && (share.start_time || share.end_time)) {
			const formatTime = (timeValue: string | null | undefined): string => {
				if (!timeValue) return '';

				// Handle different time formats
				let timeString = '';

				// If it's a full datetime string, extract the time part
				if (timeValue.includes('T')) {
					const date = new Date(timeValue);
					timeString = date.toTimeString().slice(0, 5); // Get HH:MM
				} else if (timeValue.includes(':')) {
					// If it's already in HH:MM or HH:MM:SS format, take first 5 characters
					timeString = timeValue.slice(0, 5);
				} else {
					timeString = timeValue;
				}

				return timeString;
			};

			const startTime = formatTime(share.start_time);
			const endTime = formatTime(share.end_time);

			if (startTime && endTime) {
				if (startTime === endTime) {
					parts.push(`🕐 ${startTime}`); // Show single time when start and end are the same
				} else {
					parts.push(`🕐 ${startTime} - ${endTime}`);
				}
			} else if (startTime) {
				parts.push(`🕐 From ${startTime}`);
			} else if (endTime) {
				parts.push(`🕐 Until ${endTime}`);
			}
		} else if (share.all_day) {
			parts.push('🕐 All day');
		}

		// Time zone
		if (share.time_zone) {
			parts.push(`🌍 ${share.time_zone}`);
		}

		return parts.join(' • ');
	}

	function formatRecurrence(share: RecipientCapacity): string {
		if (!share.recurrence || share.recurrence === 'Does not repeat') {
			return '';
		}

		if (share.recurrence === 'Custom...' && share.custom_recurrence_repeat_every) {
			let customText = `🔄 Every ${share.custom_recurrence_repeat_every} ${share.custom_recurrence_repeat_unit || 'days'}`;

			if (share.custom_recurrence_end_type === 'endsOn' && share.custom_recurrence_end_value) {
				const endDate = new Date(share.custom_recurrence_end_value).toLocaleDateString();
				customText += ` until ${endDate}`;
			} else if (
				share.custom_recurrence_end_type === 'endsAfter' &&
				share.custom_recurrence_end_value
			) {
				customText += ` for ${share.custom_recurrence_end_value} occurrences`;
			}

			return customText;
		}

		return `🔄 ${share.recurrence}`;
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
		onclick={() => onToggle?.()}
	>
		<div class="flex min-w-0 flex-1 flex-col pr-2">
			<div class="share-main-info flex items-center gap-2 overflow-hidden">
				<span class="share-value name flex-shrink-0 font-medium text-ellipsis whitespace-nowrap">
					<span class="capacity-emoji">{share.emoji || '📦'}</span>
					{share.name}
				</span>
				<span class="share-value qty flex-shrink-0 text-sm">
					{Number.isInteger(share.computed_quantity)
						? share.computed_quantity
						: share.computed_quantity.toFixed(2)}
					{share.unit}
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
			<!-- Show space-time preview inline if available -->
			{#if hasSpaceTimeData(share)}
				<div class="space-time-preview mt-1 text-xs text-gray-600">
					{#if formatLocation(share)}
						<span class="space-time-item">{formatLocation(share)}</span>
					{/if}
					{#if formatTimeRange(share)}
						<span class="space-time-item">{formatTimeRange(share)}</span>
					{/if}
					{#if formatRecurrence(share)}
						<span class="space-time-item">{formatRecurrence(share)}</span>
					{/if}
				</div>
			{/if}
		</div>
		<div class="flex items-center gap-2">
			{#if $unreadCount > 0}
				<span class="unread-badge">{$unreadCount > 99 ? '99+' : $unreadCount}</span>
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
				{expanded ? '▼' : '▶'}
			</span>
		</div>
	</div>

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
			<!-- Space-time coordinates section -->
			{#if hasSpaceTimeData(share)}
				<div class="space-time-coordinates rounded border border-gray-200 bg-blue-50 p-3">
					<div class="space-time-header mb-2">
						<h4 class="text-sm font-medium text-gray-700">📍 Space-time coordinates</h4>
					</div>
					<div class="space-time-info space-y-2">
						{#if formatLocation(share)}
							<div class="space-time-coordinate">
								<span class="coordinate-text text-sm text-gray-700">{formatLocation(share)}</span>
							</div>
						{/if}
						{#if formatTimeRange(share)}
							<div class="space-time-coordinate">
								<span class="coordinate-text text-sm text-gray-700">{formatTimeRange(share)}</span>
							</div>
						{/if}
						{#if formatRecurrence(share)}
							<div class="space-time-coordinate">
								<span class="coordinate-text text-sm text-gray-700">{formatRecurrence(share)}</span>
							</div>
						{/if}
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

	.space-time-preview {
		display: flex;
		flex-wrap: wrap;
		gap: 0.25rem;
		margin-top: 0.25rem;
		line-height: 1.3;
	}

	.space-time-item {
		display: inline-block;
	}

	.space-time-item:not(:last-child)::after {
		content: ' • ';
		color: #d1d5db;
		margin-left: 0.25rem;
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

	.space-time-coordinates {
		animation: slideDown 0.2s ease-out;
	}

	.space-time-header h4 {
		margin: 0;
	}

	.space-time-info {
		display: flex;
		flex-direction: column;
		gap: 0.5rem;
	}

	.space-time-coordinate {
		display: flex;
		align-items: center;
	}

	.coordinate-text {
		line-height: 1.4;
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
