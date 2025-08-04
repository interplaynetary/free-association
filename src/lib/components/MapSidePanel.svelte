<script lang="ts">
	import type { GroupedSlotMarkerData } from './Map.svelte';

	interface Props {
		markerData: GroupedSlotMarkerData | null;
		onClose: () => void;
	}

	let { markerData, onClose }: Props = $props();

	// Reactive visibility derived from markerData
	let isVisible = $derived(!!markerData);

	// Debug logging (better pattern than $effect)
	$inspect('MapSidePanel markerData:', markerData?.id);

	// Helper function to safely extract time from potentially malformed time strings
	function safeExtractTime(timeValue: string | null | undefined): string | undefined {
		if (!timeValue) return undefined;
		if (/^\d{2}:\d{2}$/.test(timeValue)) {
			return timeValue;
		}
		if (timeValue.includes('T')) {
			try {
				const date = new Date(timeValue);
				return date.toTimeString().substring(0, 5);
			} catch (e) {
				console.warn('Failed to parse time:', timeValue);
				return undefined;
			}
		}
		console.warn('Unknown time format:', timeValue);
		return undefined;
	}

	// Helper function to format time without leading zeros (08:30 → 8:30)
	function formatTimeClean(timeStr: string): string {
		if (!timeStr) return timeStr;
		const [hours, minutes] = timeStr.split(':');
		const cleanHours = parseInt(hours).toString();
		return `${cleanHours}:${minutes}`;
	}

	// Helper function to format date for display with smart labels
	function formatDateForDisplay(date: Date): string {
		const today = new Date();
		const tomorrow = new Date(today);
		tomorrow.setDate(tomorrow.getDate() + 1);
		if (date.toDateString() === today.toDateString()) {
			return 'Today';
		} else if (date.toDateString() === tomorrow.toDateString()) {
			return 'Tomorrow';
		} else {
			return date.toLocaleDateString('en-US', { month: 'short', day: 'numeric' });
		}
	}

	// Format slot time display - clean and comprehensive (matches Share.svelte)
	function formatSlotTimeDisplay(slot: any): string {
		const rawStartTime = safeExtractTime(slot.start_time);
		const rawEndTime = safeExtractTime(slot.end_time);
		const cleanStartTime = rawStartTime ? formatTimeClean(rawStartTime) : '';
		const cleanEndTime = rawEndTime ? formatTimeClean(rawEndTime) : '';

		if (slot.all_day) {
			const startDate = slot.start_date ? new Date(slot.start_date) : null;
			const endDate = slot.end_date ? new Date(slot.end_date) : null;
			if (startDate && endDate && startDate.getTime() !== endDate.getTime()) {
				const startStr = formatDateForDisplay(startDate);
				const endStr = formatDateForDisplay(endDate);
				return `${startStr} - ${endStr}, All day`;
			} else if (startDate) {
				const dateStr = formatDateForDisplay(startDate);
				return `${dateStr}, All day`;
			}
			return 'All day';
		}

		const startDate = slot.start_date ? new Date(slot.start_date) : null;
		const endDate = slot.end_date ? new Date(slot.end_date) : null;

		if (startDate) {
			const startDateStr = formatDateForDisplay(startDate);
			if (endDate && startDate.getTime() !== endDate.getTime()) {
				const endDateStr = formatDateForDisplay(endDate);
				const startTimeStr = cleanStartTime || '';
				const endTimeStr = cleanEndTime || '';
				if (startTimeStr && endTimeStr) {
					return `${startDateStr}, ${startTimeStr} - ${endDateStr}, ${endTimeStr}`;
				} else if (startTimeStr) {
					return `${startDateStr}, ${startTimeStr} - ${endDateStr}`;
				} else {
					return `${startDateStr} - ${endDateStr}`;
				}
			} else {
				if (cleanStartTime) {
					const timeRange = cleanEndTime ? `${cleanStartTime}-${cleanEndTime}` : cleanStartTime;
					return `${startDateStr}, ${timeRange}`;
				}
				return startDateStr;
			}
		}
		if (cleanStartTime) {
			return cleanEndTime ? `${cleanStartTime}-${cleanEndTime}` : cleanStartTime;
		}
		return 'No time set';
	}

	// Helper function to format slot location display
	function formatSlotLocationDisplay(slot: any): string {
		if (slot.location_type === 'Specific') {
			const addressParts = [];

			if (slot.street_address) addressParts.push(slot.street_address);
			if (slot.city) addressParts.push(slot.city);
			if (slot.state_province) addressParts.push(slot.state_province);
			if (slot.postal_code) addressParts.push(slot.postal_code);
			if (slot.country) addressParts.push(slot.country);

			if (addressParts.length > 0) {
				return addressParts.join(', ');
			}

			if (slot.latitude && slot.longitude) {
				return `${slot.latitude.toFixed(4)}, ${slot.longitude.toFixed(4)}`;
			}
		}

		return slot.location_type || 'No location';
	}

	// Helper function to check if a slot is recurring (matches Share.svelte)
	function isSlotRecurring(slot: any): boolean {
		return slot.recurrence && slot.recurrence !== 'Does not repeat';
	}

	// Helper function to check if a slot is in the past (matches Share.svelte)
	function isSlotInPast(slot: any): boolean {
		if (isSlotRecurring(slot)) return false;

		const now = new Date();
		let slotEndDate = slot.end_date ? new Date(slot.end_date) : null;
		let slotStartDate = slot.start_date ? new Date(slot.start_date) : null;

		// Use end date if available, otherwise use start date
		const relevantDate = slotEndDate || slotStartDate;
		if (!relevantDate) return false;

		// Set time to end of day for comparison
		const slotDate = new Date(relevantDate);
		slotDate.setHours(23, 59, 59, 999);

		return slotDate < now;
	}

	// Categorize slots like in Share.svelte
	function categorizeSlots(slots: any[]): {
		recurring: any[];
		currentFuture: any[];
		past: any[];
	} {
		const recurring: any[] = [];
		const currentFuture: any[] = [];
		const past: any[] = [];

		slots.forEach((slot) => {
			if (isSlotRecurring(slot)) {
				recurring.push(slot);
			} else if (isSlotInPast(slot)) {
				past.push(slot);
			} else {
				currentFuture.push(slot);
			}
		});

		return { recurring, currentFuture, past };
	}
</script>

{#if markerData}
	{@const { capacity, slots, lnglat, source, providerName } = markerData}
	{@const lngLatText = `${lnglat.lat.toFixed(6)}, ${lnglat.lng.toFixed(6)}`}
	{@const isGeocoded = source === 'geocoded'}
	{@const locationDisplay = formatSlotLocationDisplay(slots[0])}
	{@const categorizedSlots = categorizeSlots(slots)}
	{@const totalSlots = slots.length}

	<div class="side-panel">
		<!-- Header -->
		<div class="panel-header">
			<div class="header-content">
				<div class="capacity-info">
					<h2 class="capacity-title">
						<span class="capacity-emoji">{capacity.emoji || '🏠'}</span>
						{capacity.name}
					</h2>
					<div class="provider-info">
						<span class="provider-label">👤 {providerName}</span>
						{#if totalSlots > 1}
							<span class="slot-count-badge">{totalSlots} slots</span>
						{/if}
					</div>
				</div>
				<button class="close-btn" onclick={onClose} title="Close panel">
					<svg
						width="20"
						height="20"
						viewBox="0 0 24 24"
						fill="none"
						stroke="currentColor"
						stroke-width="2"
					>
						<line x1="18" y1="6" x2="6" y2="18"></line>
						<line x1="6" y1="6" x2="18" y2="18"></line>
					</svg>
				</button>
			</div>
		</div>

		<!-- Content -->
		<div class="panel-content">
			<!-- Location Info -->
			<div class="location-section">
				<h3 class="section-title">📍 Location</h3>
				{#if isGeocoded}
					<div class="location-notice geocoded-notice">
						<small>🗺️ Location from address (geocoded)</small>
					</div>
				{:else}
					<div class="location-notice coordinates-notice">
						<small>📍 Location from coordinates</small>
					</div>
				{/if}
				<div class="location-details">
					<div class="location-address">{locationDisplay}</div>
					<div class="location-coords">{lngLatText}</div>
				</div>
			</div>

			<!-- Slots Section -->
			<div class="slots-section">
				<h3 class="section-title">🕒 Available Slots</h3>

				{#if categorizedSlots.recurring.length > 0}
					<div class="slot-category">
						<h4 class="category-title">🔄 Recurring ({categorizedSlots.recurring.length})</h4>
						<div class="slot-list">
							{#each categorizedSlots.recurring as slot}
								<div class="slot-item">
									<div class="slot-main">
										<span class="slot-quantity">{slot.quantity || 0} {capacity.unit || ''}</span>
										<span class="slot-time">⏰ {formatSlotTimeDisplay(slot)}</span>
									</div>
									{#if slot.advance_notice_hours}
										<div class="slot-meta">
											<small class="notice-info">{slot.advance_notice_hours}h notice</small>
										</div>
									{/if}
								</div>
							{/each}
						</div>
					</div>
				{/if}

				{#if categorizedSlots.currentFuture.length > 0}
					<div class="slot-category">
						<h4 class="category-title">
							📅 Current & Upcoming ({categorizedSlots.currentFuture.length})
						</h4>
						<div class="slot-list">
							{#each categorizedSlots.currentFuture as slot}
								<div class="slot-item">
									<div class="slot-main">
										<span class="slot-quantity">{slot.quantity || 0} {capacity.unit || ''}</span>
										<span class="slot-time">⏰ {formatSlotTimeDisplay(slot)}</span>
									</div>
									{#if slot.advance_notice_hours}
										<div class="slot-meta">
											<small class="notice-info">{slot.advance_notice_hours}h notice</small>
										</div>
									{/if}
								</div>
							{/each}
						</div>
					</div>
				{/if}

				{#if categorizedSlots.past.length > 0}
					<div class="slot-category">
						<h4 class="category-title">📜 Past ({categorizedSlots.past.length})</h4>
						<div class="slot-list">
							{#each categorizedSlots.past as slot}
								<div class="slot-item past-slot">
									<div class="slot-main">
										<span class="slot-quantity">{slot.quantity || 0} {capacity.unit || ''}</span>
										<span class="slot-time">⏰ {formatSlotTimeDisplay(slot)}</span>
									</div>
								</div>
							{/each}
						</div>
					</div>
				{/if}
			</div>
		</div>
	</div>
{/if}

<style>
	.side-panel {
		position: absolute;
		top: 0;
		left: 0;
		width: 400px;
		height: 100%;
		background: white;
		box-shadow: 2px 0 8px rgba(0, 0, 0, 0.15);
		z-index: 1000;
		display: flex;
		flex-direction: column;
		animation: slideInFromLeft 0.3s ease-out;
	}

	@keyframes slideInFromLeft {
		from {
			transform: translateX(-100%);
		}
		to {
			transform: translateX(0);
		}
	}

	.panel-header {
		padding: 20px;
		border-bottom: 1px solid #e5e7eb;
		background: #f9fafb;
	}

	.header-content {
		display: flex;
		justify-content: space-between;
		align-items: flex-start;
	}

	.capacity-info {
		flex: 1;
	}

	.capacity-title {
		font-size: 18px;
		font-weight: 600;
		color: #111827;
		margin: 0 0 8px 0;
		display: flex;
		align-items: center;
		gap: 8px;
	}

	.capacity-emoji {
		font-size: 24px;
	}

	.provider-info {
		display: flex;
		align-items: center;
		gap: 12px;
		flex-wrap: wrap;
	}

	.provider-label {
		color: #6b7280;
		font-size: 14px;
		font-style: italic;
	}

	.slot-count-badge {
		background: #ef4444;
		color: white;
		padding: 2px 8px;
		border-radius: 12px;
		font-size: 12px;
		font-weight: 500;
	}

	.close-btn {
		background: none;
		border: none;
		cursor: pointer;
		color: #6b7280;
		padding: 4px;
		border-radius: 4px;
		transition: all 0.2s ease;
	}

	.close-btn:hover {
		background: #f3f4f6;
		color: #374151;
	}

	.panel-content {
		flex: 1;
		overflow-y: auto;
		padding: 20px;
	}

	.section-title {
		font-size: 16px;
		font-weight: 600;
		color: #374151;
		margin: 0 0 12px 0;
		display: flex;
		align-items: center;
		gap: 6px;
	}

	.location-section {
		margin-bottom: 24px;
	}

	.location-notice {
		padding: 6px 12px;
		border-radius: 6px;
		margin-bottom: 12px;
		text-align: center;
	}

	.geocoded-notice {
		background: #d1fae5;
		border: 1px solid #10b981;
		color: #047857;
	}

	.coordinates-notice {
		background: #e0e7ff;
		border: 1px solid #6366f1;
		color: #4338ca;
	}

	.location-details {
		background: #f9fafb;
		padding: 12px;
		border-radius: 8px;
		border: 1px solid #e5e7eb;
	}

	.location-address {
		font-size: 14px;
		color: #111827;
		margin-bottom: 4px;
	}

	.location-coords {
		font-size: 12px;
		color: #6b7280;
		font-family: monospace;
	}

	.slots-section {
		/* No specific styles needed here */
	}

	.slot-category {
		margin-bottom: 20px;
	}

	.category-title {
		font-size: 14px;
		font-weight: 600;
		color: #374151;
		margin: 0 0 8px 0;
		padding: 8px 0;
		border-bottom: 1px solid #f3f4f6;
	}

	.slot-list {
		display: flex;
		flex-direction: column;
		gap: 8px;
	}

	.slot-item {
		background: #f9fafb;
		border: 1px solid #e5e7eb;
		border-radius: 8px;
		padding: 12px;
		transition: all 0.2s ease;
	}

	.slot-item:hover {
		background: #f3f4f6;
		border-color: #d1d5db;
	}

	.slot-item.past-slot {
		opacity: 0.7;
		background: #f5f5f5;
	}

	.slot-main {
		display: flex;
		justify-content: space-between;
		align-items: center;
		margin-bottom: 4px;
	}

	.slot-quantity {
		font-weight: 600;
		color: #111827;
		font-size: 14px;
	}

	.slot-time {
		color: #6b7280;
		font-size: 13px;
	}

	.slot-meta {
		margin-top: 4px;
	}

	.notice-info {
		color: #9ca3af;
		font-size: 11px;
	}

	/* Responsive design - within map container */
	@media (max-width: 768px) {
		.side-panel {
			width: 320px; /* Smaller on mobile, but contained within map */
		}
	}

	@media (max-width: 500px) {
		.side-panel {
			width: 280px; /* Even smaller for very small screens */
		}
	}
</style>
