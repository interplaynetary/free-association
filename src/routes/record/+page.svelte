<script lang="ts">
	import { onMount } from 'svelte';
	import { derived } from 'svelte/store';
	import DropDown from '$lib/components/DropDown.svelte';
	import { t } from '$lib/translations';
	import { 
		initializeMyRecords,
		subscribeToParticipantRecords,
		unsubscribeFromParticipantRecords,
		getRecordsForParticipant,
		getRecordsByType,
		getRecordsByStatus,
		myRecords,
		networkRecords,
		recordStats,
		subscribedRecordParticipants,
		recordLoadingStates
	} from '$lib/network/records.svelte';
	import type { Record } from '$lib/modules/coalition/record';
	import { meshUser } from '$lib/network/mesh';
	import { userAliasesCache, userPubKeys } from '$lib/network/users.svelte';
	import { getColorForUserId } from '$lib/utils/ui/colorUtils';
	
	// Create data provider for dropdown
	const participantsDataProvider = derived(
		[userPubKeys, userAliasesCache],
		([$pubKeys, $aliases]) => {
			const result = $pubKeys.map(pubKey => ({
				id: pubKey,
				name: $aliases[pubKey] || pubKey.slice(0, 20) + '...',
				metadata: { gunAlias: $aliases[pubKey] }
			}));
			console.log('[RECORD] participantsDataProvider updated:', result.length, 'items');
			return result;
		}
	);

	// ═══════════════════════════════════════════════════════════════════
	// STATE
	// ═══════════════════════════════════════════════════════════════════

	let selectedParticipant = $state<string>('');
	let selectedParticipantName = $state<string>('');
	let filterType = $state<string>('all');
	let filterStatus = $state<'all' | 'pending' | 'adopted' | 'rejected'>('all');
	let selectedRecord = $state<Record | null>(null);
	let showDropdown = $state(false);
	let dropdownPosition = $state({ x: 0, y: 0 });

	// ═══════════════════════════════════════════════════════════════════
	// DERIVED DATA
	// ═══════════════════════════════════════════════════════════════════

	let participantRecordsStore = $derived(
		selectedParticipant ? getRecordsForParticipant(selectedParticipant) : null
	);

	let participantRecords = $state<Record[]>([]);
	
	$effect(() => {
		if (participantRecordsStore) {
			const unsubscribe = participantRecordsStore.subscribe(records => {
				participantRecords = records;
			});
			return unsubscribe;
		}
	});

	let filteredRecords = $derived.by(() => {
		let records = participantRecords;

		// Filter by type
		if (filterType !== 'all') {
			records = records.filter(r => r.type === filterType);
		}

		// Filter by status
		if (filterStatus !== 'all') {
			records = records.filter(r => r.status === filterStatus);
		}

		// Sort by timestamp (newest first)
		return records.sort((a, b) => 
			new Date(b.timestamp).getTime() - new Date(a.timestamp).getTime()
		);
	});

	let recordTypes = $derived.by(() => {
		const types = new Set<string>();
		participantRecords.forEach(r => types.add(r.type));
		return Array.from(types).sort();
	});

	let isLoading = $derived(
		selectedParticipant ? ($recordLoadingStates.get(selectedParticipant) || false) : false
	);

	// Debug: Watch showDropdown state
	$effect(() => {
		console.log('[RECORD] showDropdown changed to:', showDropdown);
	});

	// ═══════════════════════════════════════════════════════════════════
	// INITIALIZATION
	// ═══════════════════════════════════════════════════════════════════

	onMount(() => {
		// Initialize my records
		initializeMyRecords();

		// Set default to my records
		const authState = meshUser.is;
		if (authState) {
			selectedParticipant = authState.pub;
			selectedParticipantName = authState.username || 'Me';
		}
	});

	// ═══════════════════════════════════════════════════════════════════
	// PARTICIPANT SELECTION
	// ═══════════════════════════════════════════════════════════════════

	function handleShowParticipantDropdown(event: MouseEvent) {
		event.preventDefault();
		event.stopPropagation();
		const button = event.currentTarget as HTMLButtonElement;
		const rect = button.getBoundingClientRect();
		dropdownPosition = { x: rect.left, y: rect.bottom + 5 };
		console.log('[RECORD] Opening dropdown at position:', dropdownPosition, 'button rect:', rect);
		console.log('[RECORD] showDropdown before:', showDropdown);
		
		// Use setTimeout to ensure the click event finishes before we show the dropdown
		// This prevents the click-outside handler from immediately closing it
		setTimeout(() => {
			showDropdown = true;
			console.log('[RECORD] showDropdown after (delayed):', showDropdown);
		}, 0);
	}

	function handleSelectParticipant(detail: { id: string; name: string; metadata?: any }) {
		selectedParticipant = detail.id;
		selectedParticipantName = detail.name;
		selectedRecord = null; // Clear selection

		// Subscribe to their records if not already subscribed
		const authState = meshUser.is;
		if (detail.id !== authState?.pub) {
			subscribeToParticipantRecords(detail.id);
		}

		showDropdown = false;
	}

	// ═══════════════════════════════════════════════════════════════════
	// RECORD DISPLAY
	// ═══════════════════════════════════════════════════════════════════

	function formatTimestamp(isoString: string): string {
		const date = new Date(isoString);
		return date.toLocaleString('en-US', {
			year: 'numeric',
			month: 'long',
			day: 'numeric',
			hour: '2-digit',
			minute: '2-digit',
			timeZoneName: 'short'
		});
	}

	function getRecordTypeLabel(type: string): string {
		return type
			.split('_')
			.map(word => word.charAt(0).toUpperCase() + word.slice(1))
			.join(' ');
	}

	function getStatusClass(status: string): string {
		switch (status) {
			case 'adopted': return 'status-adopted';
			case 'rejected': return 'status-rejected';
			case 'pending': return 'status-pending';
			default: return '';
		}
	}

	function getStatusIcon(status: string): string {
		switch (status) {
			case 'adopted': return '✓';
			case 'rejected': return '✗';
			case 'pending': return '⧗';
			default: return '';
		}
	}

	function handleSelectRecord(record: Record) {
		selectedRecord = selectedRecord?.id === record.id ? null : record;
	}
</script>

<!-- Dropdown for participant selection (rendered at root for proper positioning) -->
<DropDown
	title="Select Participant"
	searchPlaceholder="Search participants..."
	position={dropdownPosition}
	width={320}
	maxHeight={400}
	dataProvider={participantsDataProvider}
	show={showDropdown}
	select={handleSelectParticipant}
	close={() => { 
		console.log('[RECORD] Dropdown close called'); 
		showDropdown = false; 
	}}
	updatePosition={(newPos) => { 
		console.log('[RECORD] Dropdown updatePosition called:', newPos);
		dropdownPosition = newPos; 
	}}
/>

<div class="record-page">
	<!-- Header -->
	<header class="un-header">
		<div class="header-content">
			<div class="un-logo">
				<div class="un-emblem">
					<img src="/logo.png" alt="Free Association Logo" width="60" height="60" />
				</div>
				<div class="header-text">
					<h1>Coalition Secretariat</h1>
					<p class="subtitle">Official Records System</p>
				</div>
			</div>

			<div class="stats-summary">
				<div class="stat-item">
					<span class="stat-label">My Records</span>
					<span class="stat-value">{$recordStats.myRecordsCount}</span>
				</div>
				<div class="stat-item">
					<span class="stat-label">Participants</span>
					<span class="stat-value">{$recordStats.networkParticipantsCount}</span>
				</div>
				<div class="stat-item">
					<span class="stat-label">Total Records</span>
					<span class="stat-value">{$recordStats.myRecordsCount + $recordStats.totalNetworkRecords}</span>
				</div>
			</div>
		</div>
	</header>

	<!-- Main Content -->
	<main class="record-main">
		<!-- Sidebar -->
		<aside class="sidebar">
			<section class="sidebar-section">
				<h2 class="sidebar-title">Participant</h2>
				<button 
					class="participant-selector"
					onclick={handleShowParticipantDropdown}
				>
					<div class="participant-info">
						<div 
							class="participant-dot" 
							style="background-color: {getColorForUserId(selectedParticipant)}"
						></div>
						<span class="participant-name">{selectedParticipantName || 'Select Participant'}</span>
					</div>
					<span class="dropdown-arrow">▼</span>
				</button>
			</section>

			<section class="sidebar-section">
				<h2 class="sidebar-title">Filters</h2>
				
				<div class="filter-group">
					<label class="filter-label" for="record-type-filter">Record Type</label>
					<select 
						id="record-type-filter"
						class="filter-select"
						bind:value={filterType}
					>
						<option value="all">All Types</option>
						{#each recordTypes as type}
							<option value={type}>{getRecordTypeLabel(type)}</option>
						{/each}
					</select>
				</div>

				<div class="filter-group">
					<label class="filter-label" for="status-filter">Status</label>
					<select 
						id="status-filter"
						class="filter-select"
						bind:value={filterStatus}
					>
						<option value="all">All Statuses</option>
						<option value="pending">Pending</option>
						<option value="adopted">Adopted</option>
						<option value="rejected">Rejected</option>
					</select>
				</div>
			</section>

			<section class="sidebar-section">
				<h2 class="sidebar-title">Statistics</h2>
				<div class="stats-list">
					<div class="stat-row">
						<span class="stat-label-small">Pending</span>
						<span class="stat-badge pending">{$recordStats.byStatus.pending}</span>
					</div>
					<div class="stat-row">
						<span class="stat-label-small">Adopted</span>
						<span class="stat-badge adopted">{$recordStats.byStatus.adopted}</span>
					</div>
					<div class="stat-row">
						<span class="stat-label-small">Rejected</span>
						<span class="stat-badge rejected">{$recordStats.byStatus.rejected}</span>
					</div>
				</div>
			</section>
		</aside>

		<!-- Content Area -->
		<div class="content-area">
			{#if !selectedParticipant}
				<div class="empty-state">
					<div class="empty-icon">📋</div>
					<h3>No Participant Selected</h3>
					<p>Please select a participant to view their official records.</p>
				</div>
			{:else if isLoading}
				<div class="loading-state">
					<div class="loading-spinner"></div>
					<p>Loading records...</p>
				</div>
			{:else if filteredRecords.length === 0}
				<div class="empty-state">
					<div class="empty-icon">📄</div>
					<h3>No Records Found</h3>
					<p>
						{#if filterType !== 'all' || filterStatus !== 'all'}
							No records match the current filters.
						{:else}
							This participant has not issued any records yet.
						{/if}
					</p>
				</div>
			{:else}
				<div class="records-container">
					<!-- Records List -->
					<div class="records-list">
						<div class="list-header">
							<h2 class="list-title">
								Official Records 
								<span class="record-count">({filteredRecords.length})</span>
							</h2>
						</div>

						{#each filteredRecords as record (record.id)}
							<div 
								class="record-card"
								class:selected={selectedRecord?.id === record.id}
								role="button"
								tabindex="0"
								onclick={() => handleSelectRecord(record)}
								onkeydown={(e) => {
									if (e.key === 'Enter' || e.key === ' ') {
										e.preventDefault();
										handleSelectRecord(record);
									}
								}}
							>
								<div class="record-header">
									<div class="record-type-badge">
										{getRecordTypeLabel(record.type)}
									</div>
									<div class="record-status {getStatusClass(record.status)}">
										<span class="status-icon">{getStatusIcon(record.status)}</span>
										<span class="status-text">{record.status.toUpperCase()}</span>
									</div>
								</div>

								<div class="record-meta">
									<div class="meta-item">
										<span class="meta-label">Record ID:</span>
										<span class="meta-value mono">{record.id.slice(0, 8)}...{record.id.slice(-8)}</span>
									</div>
									<div class="meta-item">
										<span class="meta-label">Issued:</span>
										<span class="meta-value">{formatTimestamp(record.timestamp)}</span>
									</div>
									{#if record.decision_timestamp}
										<div class="meta-item">
											<span class="meta-label">Decided:</span>
											<span class="meta-value">{formatTimestamp(record.decision_timestamp)}</span>
										</div>
									{/if}
								</div>

								{#if selectedRecord?.id === record.id}
									<div class="record-details">
										<h3 class="details-title">Record Data</h3>
										<pre class="details-content">{JSON.stringify(record.data, null, 2)}</pre>
									</div>
								{/if}
							</div>
						{/each}
					</div>
				</div>
			{/if}
		</div>
	</main>
</div>

<style>
	/* ═══════════════════════════════════════════════════════════════════ */
	/* UN COLOR PALETTE */
	/* ═══════════════════════════════════════════════════════════════════ */
	:root {
		--un-blue: #009edb;
		--un-blue-dark: #0077b3;
		--un-blue-light: #4fc3f7;
		--un-gold: #f4b942;
		--un-white: #ffffff;
		--un-gray-1: #f5f5f5;
		--un-gray-2: #e0e0e0;
		--un-gray-3: #9e9e9e;
		--un-gray-4: #616161;
		--un-gray-5: #333333;

		--status-pending: #ff9800;
		--status-adopted: #4caf50;
		--status-rejected: #f44336;
	}

	/* ═══════════════════════════════════════════════════════════════════ */
	/* LAYOUT */
	/* ═══════════════════════════════════════════════════════════════════ */
	.record-page {
		min-height: 100vh;
		background: var(--un-gray-1);
		display: flex;
		flex-direction: column;
	}

	/* ═══════════════════════════════════════════════════════════════════ */
	/* HEADER */
	/* ═══════════════════════════════════════════════════════════════════ */
	.un-header {
		background: linear-gradient(135deg, var(--un-blue-dark) 0%, var(--un-blue) 100%);
		color: var(--un-white);
		padding: 24px 32px;
		box-shadow: 0 4px 12px rgba(0, 0, 0, 0.15);
	}

	.header-content {
		max-width: 1400px;
		margin: 0 auto;
		display: flex;
		justify-content: space-between;
		align-items: center;
		flex-wrap: wrap;
		gap: 24px;
	}

	.un-logo {
		display: flex;
		align-items: center;
		gap: 16px;
	}

	.un-emblem {
		flex-shrink: 0;
		display: flex;
		align-items: center;
		justify-content: center;
	}

	.un-emblem img {
		object-fit: contain;
	}

	.header-text h1 {
		margin: 0;
		font-size: 28px;
		font-weight: 300;
		letter-spacing: 0.5px;
	}

	.subtitle {
		margin: 4px 0 0 0;
		font-size: 14px;
		opacity: 0.9;
		font-weight: 300;
	}

	.stats-summary {
		display: flex;
		gap: 32px;
	}

	.stat-item {
		display: flex;
		flex-direction: column;
		align-items: flex-end;
	}

	.stat-label {
		font-size: 11px;
		text-transform: uppercase;
		letter-spacing: 1px;
		opacity: 0.8;
		font-weight: 500;
	}

	.stat-value {
		font-size: 32px;
		font-weight: 300;
		line-height: 1;
		margin-top: 4px;
	}

	/* ═══════════════════════════════════════════════════════════════════ */
	/* MAIN CONTENT */
	/* ═══════════════════════════════════════════════════════════════════ */
	.record-main {
		flex: 1;
		display: flex;
		max-width: 1400px;
		margin: 0 auto;
		width: 100%;
		gap: 24px;
		padding: 24px 32px;
	}

	/* ═══════════════════════════════════════════════════════════════════ */
	/* SIDEBAR */
	/* ═══════════════════════════════════════════════════════════════════ */
	.sidebar {
		width: 280px;
		flex-shrink: 0;
		display: flex;
		flex-direction: column;
		gap: 16px;
	}

	.sidebar-section {
		background: var(--un-white);
		border-radius: 4px;
		padding: 20px;
		box-shadow: 0 2px 8px rgba(0, 0, 0, 0.08);
		border-left: 3px solid var(--un-blue);
	}

	.sidebar-title {
		margin: 0 0 16px 0;
		font-size: 13px;
		text-transform: uppercase;
		letter-spacing: 1.2px;
		color: var(--un-gray-4);
		font-weight: 600;
	}

	.participant-selector {
		width: 100%;
		padding: 12px 16px;
		background: var(--un-gray-1);
		border: 1px solid var(--un-gray-2);
		border-radius: 4px;
		cursor: pointer;
		display: flex;
		justify-content: space-between;
		align-items: center;
		transition: all 0.2s;
		font-size: 14px;
	}

	.participant-selector:hover {
		background: var(--un-white);
		border-color: var(--un-blue);
	}

	.participant-info {
		display: flex;
		align-items: center;
		gap: 10px;
	}

	.participant-dot {
		width: 12px;
		height: 12px;
		border-radius: 50%;
		flex-shrink: 0;
	}

	.participant-name {
		font-weight: 500;
		color: var(--un-gray-5);
	}

	.dropdown-arrow {
		color: var(--un-gray-3);
		font-size: 10px;
	}

	.filter-group {
		margin-bottom: 16px;
	}

	.filter-group:last-child {
		margin-bottom: 0;
	}

	.filter-label {
		display: block;
		font-size: 12px;
		font-weight: 500;
		color: var(--un-gray-4);
		margin-bottom: 6px;
		text-transform: uppercase;
		letter-spacing: 0.5px;
	}

	.filter-select {
		width: 100%;
		padding: 8px 12px;
		background: var(--un-white);
		border: 1px solid var(--un-gray-2);
		border-radius: 4px;
		font-size: 13px;
		color: var(--un-gray-5);
		cursor: pointer;
		transition: border-color 0.2s;
	}

	.filter-select:focus {
		outline: none;
		border-color: var(--un-blue);
	}

	.stats-list {
		display: flex;
		flex-direction: column;
		gap: 8px;
	}

	.stat-row {
		display: flex;
		justify-content: space-between;
		align-items: center;
		padding: 8px 0;
		border-bottom: 1px solid var(--un-gray-1);
	}

	.stat-row:last-child {
		border-bottom: none;
	}

	.stat-label-small {
		font-size: 13px;
		color: var(--un-gray-4);
		font-weight: 500;
	}

	.stat-badge {
		padding: 4px 10px;
		border-radius: 12px;
		font-size: 12px;
		font-weight: 600;
	}

	.stat-badge.pending {
		background: rgba(255, 152, 0, 0.1);
		color: var(--status-pending);
	}

	.stat-badge.adopted {
		background: rgba(76, 175, 80, 0.1);
		color: var(--status-adopted);
	}

	.stat-badge.rejected {
		background: rgba(244, 67, 54, 0.1);
		color: var(--status-rejected);
	}

	/* ═══════════════════════════════════════════════════════════════════ */
	/* CONTENT AREA */
	/* ═══════════════════════════════════════════════════════════════════ */
	.content-area {
		flex: 1;
		min-width: 0;
	}

	.empty-state,
	.loading-state {
		background: var(--un-white);
		border-radius: 4px;
		padding: 80px 40px;
		text-align: center;
		box-shadow: 0 2px 8px rgba(0, 0, 0, 0.08);
	}

	.empty-icon {
		font-size: 64px;
		margin-bottom: 24px;
		opacity: 0.3;
	}

	.empty-state h3 {
		margin: 0 0 12px 0;
		font-size: 24px;
		font-weight: 300;
		color: var(--un-gray-5);
	}

	.empty-state p {
		margin: 0;
		color: var(--un-gray-3);
		font-size: 14px;
	}

	.loading-spinner {
		width: 48px;
		height: 48px;
		border: 4px solid var(--un-gray-2);
		border-top-color: var(--un-blue);
		border-radius: 50%;
		animation: spin 1s linear infinite;
		margin: 0 auto 24px;
	}

	@keyframes spin {
		to {
			transform: rotate(360deg);
		}
	}

	.loading-state p {
		color: var(--un-gray-3);
		font-size: 14px;
	}

	/* ═══════════════════════════════════════════════════════════════════ */
	/* RECORDS LIST */
	/* ═══════════════════════════════════════════════════════════════════ */
	.records-container {
		background: var(--un-white);
		border-radius: 4px;
		box-shadow: 0 2px 8px rgba(0, 0, 0, 0.08);
	}

	.list-header {
		padding: 24px 32px;
		border-bottom: 2px solid var(--un-blue);
	}

	.list-title {
		margin: 0;
		font-size: 20px;
		font-weight: 300;
		color: var(--un-gray-5);
	}

	.record-count {
		color: var(--un-gray-3);
		font-size: 16px;
	}

	/* ═══════════════════════════════════════════════════════════════════ */
	/* RECORD CARDS */
	/* ═══════════════════════════════════════════════════════════════════ */
	.record-card {
		padding: 24px 32px;
		border-bottom: 1px solid var(--un-gray-1);
		cursor: pointer;
		transition: background-color 0.2s;
	}

	.record-card:hover {
		background: var(--un-gray-1);
	}

	.record-card.selected {
		background: rgba(0, 158, 219, 0.05);
		border-left: 4px solid var(--un-blue);
	}

	.record-card:last-child {
		border-bottom: none;
	}

	.record-header {
		display: flex;
		justify-content: space-between;
		align-items: center;
		margin-bottom: 16px;
		flex-wrap: wrap;
		gap: 12px;
	}

	.record-type-badge {
		padding: 6px 14px;
		background: var(--un-blue);
		color: var(--un-white);
		border-radius: 4px;
		font-size: 12px;
		font-weight: 600;
		text-transform: uppercase;
		letter-spacing: 0.8px;
	}

	.record-status {
		display: flex;
		align-items: center;
		gap: 6px;
		padding: 6px 12px;
		border-radius: 4px;
		font-size: 11px;
		font-weight: 600;
		letter-spacing: 0.8px;
	}

	.status-pending {
		background: rgba(255, 152, 0, 0.1);
		color: var(--status-pending);
		border: 1px solid var(--status-pending);
	}

	.status-adopted {
		background: rgba(76, 175, 80, 0.1);
		color: var(--status-adopted);
		border: 1px solid var(--status-adopted);
	}

	.status-rejected {
		background: rgba(244, 67, 54, 0.1);
		color: var(--status-rejected);
		border: 1px solid var(--status-rejected);
	}

	.status-icon {
		font-size: 14px;
	}

	.record-meta {
		display: grid;
		grid-template-columns: repeat(auto-fit, minmax(250px, 1fr));
		gap: 12px 24px;
	}

	.meta-item {
		display: flex;
		gap: 8px;
		font-size: 13px;
	}

	.meta-label {
		color: var(--un-gray-3);
		font-weight: 500;
	}

	.meta-value {
		color: var(--un-gray-5);
	}

	.mono {
		font-family: 'Courier New', monospace;
		font-size: 12px;
	}

	.record-details {
		margin-top: 20px;
		padding-top: 20px;
		border-top: 1px solid var(--un-gray-2);
	}

	.details-title {
		margin: 0 0 12px 0;
		font-size: 14px;
		font-weight: 600;
		text-transform: uppercase;
		letter-spacing: 0.8px;
		color: var(--un-gray-4);
	}

	.details-content {
		background: var(--un-gray-1);
		border: 1px solid var(--un-gray-2);
		border-radius: 4px;
		padding: 16px;
		font-family: 'Courier New', monospace;
		font-size: 12px;
		line-height: 1.6;
		color: var(--un-gray-5);
		overflow-x: auto;
		max-height: 400px;
		overflow-y: auto;
	}

	/* ═══════════════════════════════════════════════════════════════════ */
	/* RESPONSIVE */
	/* ═══════════════════════════════════════════════════════════════════ */
	@media (max-width: 1024px) {
		.record-main {
			flex-direction: column;
		}

		.sidebar {
			width: 100%;
		}

		.stats-summary {
			width: 100%;
			justify-content: space-around;
		}
	}

	@media (max-width: 768px) {
		.un-header {
			padding: 16px 20px;
		}

		.record-main {
			padding: 16px 20px;
		}

		.header-content {
			flex-direction: column;
			align-items: flex-start;
		}

		.stat-item {
			align-items: flex-start;
		}

		.list-header,
		.record-card {
			padding: 16px 20px;
		}
	}
</style>

