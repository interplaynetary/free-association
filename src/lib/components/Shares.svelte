<script lang="ts">
	import { userNetworkCapacitiesWithShares } from '$lib/state/core.svelte';
	import { getUserName, user } from '$lib/state/gun.svelte';
	import Chat from './Chat.svelte';
	import type {
		Node,
		RootNode,
		NonRootNode,
		Capacity,
		CapacitiesCollection,
		ShareMap,
		ProviderCapacity,
		RecipientCapacity,
		BaseCapacity
	} from '$lib/schema';

	interface ShareWithProvider extends RecipientCapacity {
		provider_name: string;
	}

	// Green color scale for share percentage
	const colors = ['#dcfce7', '#86efac', '#22c55e'];

	function getShareColor(percentage: number): string {
		if (percentage <= 0) return '#f3f4f6';
		const index = Math.floor(percentage * colors.length);
		return colors[Math.min(index, colors.length - 1)];
	}

	async function handleProviderClick(provider: string) {
		// This function will be implemented later to navigate to the provider
		console.log(`Navigating to provider: ${provider}`);
	}

	// Helper functions for space-time data formatting
	function hasSpaceTimeData(share: ShareWithProvider): boolean {
		return Boolean(
			(share.location_type && share.location_type !== 'Undefined') ||
				share.start_date ||
				share.start_time ||
				share.end_date ||
				share.end_time ||
				(share.recurrence && share.recurrence !== 'Does not repeat')
		);
	}

	function formatLocation(share: ShareWithProvider): string {
		if (!share.location_type || share.location_type === 'Undefined') {
			return '';
		}

		if (share.location_type === 'LiveLocation') {
			return '📍 Live location';
		}

		if (share.location_type === 'Specific') {
			if (share.latitude !== undefined && share.longitude !== undefined) {
				return `📍 ${share.latitude.toFixed(6)}, ${share.longitude.toFixed(6)}`;
			}
			return '📍 Specific location';
		}

		return '';
	}

	function formatTimeRange(share: ShareWithProvider): string {
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

	function formatRecurrence(share: ShareWithProvider): string {
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

	let shares = $state<ShareWithProvider[]>([]);
	let expandedShares = $state<Set<string>>(new Set());

	function toggleShare(shareId: string) {
		const newExpanded = new Set(expandedShares);
		if (newExpanded.has(shareId)) {
			newExpanded.delete(shareId);
		} else {
			newExpanded.add(shareId);
		}
		expandedShares = newExpanded;
	}

	function getChatId(share: ShareWithProvider): string {
		// Use capacity ID as the chat ID for all conversations about this capacity
		return share.id;
	}

	$effect(() => {
		void (async () => {
			if (!$userNetworkCapacitiesWithShares) {
				shares = [];
				return;
			}

			const allShares = await Promise.all(
				Object.entries($userNetworkCapacitiesWithShares).map(async ([capacityId, capacity]) => {
					const providerName = (await getUserName(
						(capacity as RecipientCapacity).provider_id
					)) as string;
					const truncatedName =
						providerName && providerName.length > 10
							? providerName.substring(0, 10) + '...'
							: (providerName as string) || '';
					return {
						...capacity,
						id: capacityId,
						provider_name: truncatedName
					} as RecipientCapacity & { provider_name: string };
				})
			);

			// Filter out shares with no name or zero/no quantity
			shares = allShares.filter(
				(share) =>
					share.name &&
					share.name.trim() !== '' &&
					share.computed_quantity &&
					share.computed_quantity > 0
			);
		})();
	});
</script>

<div class="shares-list space-y-3 p-2">
	{#each shares as share (share.id)}
		<div class="capacity-share-container">
			<div
				class="capacity-share flex cursor-pointer items-center justify-between rounded p-2 shadow-sm"
				style="background-color: {getShareColor(
					share.share_percentage
				)}; border: 1px solid #e5e7eb;"
				onclick={() => toggleShare(share.id)}
			>
				<div class="flex min-w-0 flex-1 flex-col pr-2">
					<div class="share-main-info flex items-center gap-2 overflow-hidden">
						<span
							class="share-value name flex-shrink-0 font-medium text-ellipsis whitespace-nowrap"
						>
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
					<button
						type="button"
						class="provider-btn rounded-md px-2 py-1 text-xs whitespace-nowrap"
						onclick={(e) => {
							e.stopPropagation();
							handleProviderClick(share.provider_name);
						}}
					>
						{share.provider_name}
					</button>
					<span class="expand-icon text-sm">
						{expandedShares.has(share.id) ? '▼' : '▶'}
					</span>
				</div>
			</div>

			{#if expandedShares.has(share.id)}
				<div class="expanded-content mt-2 space-y-3">
					<!-- Space-time coordinates section -->
					{#if hasSpaceTimeData(share)}
						<div class="space-time-coordinates rounded border border-gray-200 bg-blue-50 p-3">
							<div class="space-time-header mb-2">
								<h4 class="text-sm font-medium text-gray-700">📍 Space-time coordinates</h4>
							</div>
							<div class="space-time-info space-y-2">
								{#if formatLocation(share)}
									<div class="space-time-coordinate">
										<span class="coordinate-text text-sm text-gray-700"
											>{formatLocation(share)}</span
										>
									</div>
								{/if}
								{#if formatTimeRange(share)}
									<div class="space-time-coordinate">
										<span class="coordinate-text text-sm text-gray-700"
											>{formatTimeRange(share)}</span
										>
									</div>
								{/if}
								{#if formatRecurrence(share)}
									<div class="space-time-coordinate">
										<span class="coordinate-text text-sm text-gray-700"
											>{formatRecurrence(share)}</span
										>
									</div>
								{/if}
							</div>
						</div>
					{/if}

					<!-- Chat section -->
					<div class="chat-container rounded border border-gray-200 bg-gray-50 p-3">
						<div class="chat-header mb-2">
							<h4 class="text-sm font-medium text-gray-700">
								💬 Chat about {share.name} with {share.provider_name}
							</h4>
						</div>
						<Chat
							chatId={getChatId(share)}
							placeholder={`Message ${share.provider_name} about ${share.name}...`}
							maxLength={200}
						/>
					</div>
				</div>
			{/if}
		</div>
	{/each}
</div>

<style>
	:global(body) {
		font-family: 'Inter', system-ui, sans-serif;
		background: #f7fafc;
	}

	.shares-list {
		display: flex;
		flex-direction: column;
	}

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
		background: rgba(255, 255, 255, 0.5);
		color: #4b5563;
		border: none;
		transition:
			background 0.2s,
			color 0.2s;
		cursor: pointer;
		flex-shrink: 0;
	}

	.provider-btn:hover {
		background: rgba(255, 255, 255, 0.8);
		color: #1f2937;
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
