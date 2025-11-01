<script lang="ts">
	/**
	 * V5 Slots Test Page
	 * 
	 * Tests reactive CRUD operations on:
	 * - myNeedSlotsStore (DERIVED from commitment)
	 * - myCapacitySlotsStore (DERIVED from commitment)
	 * - myCommitmentStore (THE source of truth - contains slots!)
	 * - networkCommitments (sync verification)
	 * 
	 * ✅ ARCHITECTURAL SIMPLIFICATION:
	 * 
	 * OLD ARCHITECTURE (redundant):
	 * - myNeedSlotsStore → persisted separately
	 * - myCapacitySlotsStore → persisted separately  
	 * - myCommitmentStore → composed from above (slots duplicated!)
	 * 
	 * NEW ARCHITECTURE (single source of truth):
	 * - myCommitmentStore → THE ONLY persistent store (contains slots!)
	 *   ↓
	 * - myNeedSlotsStore → DERIVED (read-only)
	 * - myCapacitySlotsStore → DERIVED (read-only)
	 * 
	 * TO UPDATE SLOTS:
	 * - Use setMyNeedSlots(slots) helper
	 * - Use setMyCapacitySlots(slots) helper
	 * 
	 * REACTIVITY FLOW:
	 * 1. Call setMyNeedSlots() or setMyCapacitySlots()
	 * 2. Helper updates myCommitmentStore directly (single write!)
	 * 3. myCommitmentStore.set() → persists to Holster
	 * 4. Derived stores (myNeedSlotsStore, myCapacitySlotsStore) update automatically
	 * 5. Other participants receive via subscribeToCommitment()
	 * 6. networkCommitments.update() (versioned store with ITC)
	 * 7. Field stores update (networkNeedSlots, networkCapacitySlots)
	 * 8. Indexes rebuild (O(M) incremental)
	 * 9. UI updates everywhere! ✨
	 */
	
	import { onMount } from 'svelte';
	import { holsterUserPub } from '$lib/network/holster.svelte'
	import {
		myNeedSlotsStore,
		myCapacitySlotsStore,
		myCommitmentStore,
		myRecognitionWeights,
		myMutualRecognition,
		networkCommitments,
		networkNeedSlots,
		networkCapacitySlots,
		networkAllocations,    // ✅ NEW: Fine-grained allocations field store
		initializeAllocationStores,
		enableAutoCommitmentComposition,
		composeCommitmentFromSources,
		setMyNeedSlots,        // ✅ NEW: Helper to update need slots
		setMyCapacitySlots     // ✅ NEW: Helper to update capacity slots
	} from '$lib/protocol/stores.svelte';
	import { enableAutoAllocationPublishing } from '$lib/protocol/allocation.svelte';
	import type { NeedSlot, AvailabilitySlot, Commitment } from '$lib/protocol/schemas';
	
	// Reactive state (Svelte 5 runes)
	let needSlots = $state<NeedSlot[]>([]);
	let capacitySlots = $state<AvailabilitySlot[]>([]);
	let commitment = $state<Commitment | null>(null);
	let myPub = $state<string | null>(null);
	let networkCommitmentCount = $state(0);
	let networkNeedSlotCount = $state(0);
	let networkCapacitySlotCount = $state(0);
	let recognitionWeights = $state<Record<string, number>>({});
	let mutualRecognition = $state<Record<string, number>>({});
	
	// Form state for adding new slots
	let newNeedName = $state('');
	let newNeedType = $state('food');
	let newNeedQuantity = $state(10);
	
	let newCapacityName = $state('');
	let newCapacityType = $state('food');
	let newCapacityQuantity = $state(5);
	
	let autoCompose = $state(true);
	let showRawData = $state(false);
	
	// Cleanup functions
	let cleanupComposition: (() => void) | null = null;
	let cleanupAllocationPublishing: (() => void) | null = null;
	
	onMount(() => {
		console.log('[TEST-V5] Initializing stores...');
		
		// Initialize stores
		initializeAllocationStores();
		
		// Subscribe to stores (reactive)
		const unsubNeeds = myNeedSlotsStore.subscribe((slots) => {
			needSlots = slots || [];
			console.log('[TEST-V5] Need slots updated:', needSlots.length);
		});
		
		const unsubCapacity = myCapacitySlotsStore.subscribe((slots) => {
			capacitySlots = slots || [];
			console.log('[TEST-V5] Capacity slots updated:', capacitySlots.length);
		});
		
		const unsubCommitment = myCommitmentStore.subscribe((commit) => {
			commitment = commit;
			console.log('[TEST-V5] Commitment updated:', commit ? 'yes' : 'no');
		});
		
		const unsubPub = holsterUserPub.subscribe((pub) => {
			myPub = pub;
		});
		
		const unsubRecWeights = myRecognitionWeights.subscribe((weights) => {
			recognitionWeights = weights;
			console.log('[TEST-V5] Recognition weights:', Object.keys(weights).length);
		});
		
		const unsubMutualRec = myMutualRecognition.subscribe((mr) => {
			mutualRecognition = mr;
			console.log('[TEST-V5] Mutual recognition:', Object.keys(mr).length);
		});
		
		// Subscribe to network stores
		const unsubNetworkCommits = networkCommitments.subscribe((commitMap) => {
			networkCommitmentCount = commitMap.size;
		});
		
		const unsubNetworkNeeds = networkNeedSlots.subscribe((needMap) => {
			networkNeedSlotCount = Array.from(needMap.values())
				.reduce((sum, slots) => sum + (slots?.length || 0), 0);
		});
		
		const unsubNetworkCapacity = networkCapacitySlots.subscribe((capacityMap) => {
			networkCapacitySlotCount = Array.from(capacityMap.values())
				.reduce((sum, slots) => sum + (slots?.length || 0), 0);
		});
		
		// Enable auto-composition if checked
		if (autoCompose) {
			cleanupComposition = enableAutoCommitmentComposition();
		}
		
		// Enable auto-allocation publishing (always on for transparency)
		cleanupAllocationPublishing = enableAutoAllocationPublishing();
		
		console.log('[TEST-V5] ✅ Initialized and subscribed');
		
		return () => {
			unsubNeeds();
			unsubCapacity();
			unsubCommitment();
			unsubPub();
			unsubRecWeights();
			unsubMutualRec();
			unsubNetworkCommits();
			unsubNetworkNeeds();
			unsubNetworkCapacity();
			if (cleanupComposition) cleanupComposition();
			if (cleanupAllocationPublishing) cleanupAllocationPublishing();
		};
	});
	
	// CRUD Operations - Needs
	// ✅ Now using setMyNeedSlots() helper (single source of truth!)
	
	function addNeedSlot() {
		if (!newNeedName.trim()) return;
		
		const newSlot: NeedSlot = {
			id: `need_${Date.now()}_${Math.random()}`,
			name: newNeedName,
			need_type_id: newNeedType,
			quantity: newNeedQuantity,
			unit: 'units'
		};
		
		setMyNeedSlots([...needSlots, newSlot]); // ✅ NEW: Use helper instead of store.set()
		
		// Reset form
		newNeedName = '';
		newNeedQuantity = 10;
		
		console.log('[TEST-V5] ➕ Added need slot:', newSlot);
	}
	
	function removeNeedSlot(id: string) {
		setMyNeedSlots(needSlots.filter(s => s.id !== id)); // ✅ NEW: Use helper
		console.log('[TEST-V5] ➖ Removed need slot:', id);
	}
	
	function updateNeedQuantity(id: string, quantity: number) {
		const updated = needSlots.map(s =>
			s.id === id ? { ...s, quantity } : s
		);
		setMyNeedSlots(updated); // ✅ NEW: Use helper
		console.log('[TEST-V5] ✏️  Updated need quantity:', id, quantity);
	}
	
	// CRUD Operations - Capacity
	// ✅ Now using setMyCapacitySlots() helper (single source of truth!)
	
	function addCapacitySlot() {
		if (!newCapacityName.trim()) return;
		
		const newSlot: AvailabilitySlot = {
			id: `capacity_${Date.now()}_${Math.random()}`,
			name: newCapacityName,
			need_type_id: newCapacityType,
			quantity: newCapacityQuantity,
			unit: 'units'
		};
		
		setMyCapacitySlots([...capacitySlots, newSlot]); // ✅ NEW: Use helper instead of store.set()
		
		// Reset form
		newCapacityName = '';
		newCapacityQuantity = 5;
		
		console.log('[TEST-V5] ➕ Added capacity slot:', newSlot);
	}
	
	function removeCapacitySlot(id: string) {
		setMyCapacitySlots(capacitySlots.filter(s => s.id !== id)); // ✅ NEW: Use helper
		console.log('[TEST-V5] ➖ Removed capacity slot:', id);
	}
	
	function updateCapacityQuantity(id: string, quantity: number) {
		const updated = capacitySlots.map(s =>
			s.id === id ? { ...s, quantity } : s
		);
		setMyCapacitySlots(updated); // ✅ NEW: Use helper
		console.log('[TEST-V5] ✏️  Updated capacity quantity:', id, quantity);
	}
	
	// Manual composition (for testing)
	function manualCompose() {
		const composed = composeCommitmentFromSources();
		if (composed) {
			myCommitmentStore.set(composed);
			console.log('[TEST-V5] 🔧 Manual composition complete');
		}
	}
	
	// Toggle auto-composition
	function toggleAutoCompose() {
		if (autoCompose && !cleanupComposition) {
			cleanupComposition = enableAutoCommitmentComposition();
			console.log('[TEST-V5] 🔄 Auto-composition enabled');
		} else if (!autoCompose && cleanupComposition) {
			cleanupComposition();
			cleanupComposition = null;
			console.log('[TEST-V5] ⏸️  Auto-composition disabled');
		}
	}
	
	// Need type options
	const needTypes = [
		{ id: 'food', label: '🍎 Food' },
		{ id: 'housing', label: '🏠 Housing' },
		{ id: 'healthcare', label: '🏥 Healthcare' },
		{ id: 'education', label: '📚 Education' },
		{ id: 'transportation', label: '🚗 Transportation' },
		{ id: 'childcare', label: '👶 Childcare' }
	];
</script>

<div class="test-page">
	<header>
		<h1>V5 Slots Test Page</h1>
		<p class="subtitle">Reactive CRUD testing for allocation slots</p>
		
		<div class="status-bar">
			<div class="status-item">
				<strong>User:</strong> {myPub ? myPub.slice(0, 20) + '...' : 'Not authenticated'}
			</div>
			<div class="status-item">
				<strong>Network:</strong> {networkCommitmentCount} commitments, 
				{networkNeedSlotCount} needs, {networkCapacitySlotCount} capacity
			</div>
			<div class="status-item">
				<strong>Recognition:</strong> {Object.keys(recognitionWeights).length} people, 
				{Object.keys(mutualRecognition).length} mutual
			</div>
		</div>
		
		<div class="controls">
			<label>
				<input type="checkbox" bind:checked={autoCompose} onchange={toggleAutoCompose} />
				Auto-compose commitment
			</label>
			{#if !autoCompose}
				<button onclick={manualCompose} class="btn-secondary">
					🔧 Manual Compose
				</button>
			{/if}
			<label>
				<input type="checkbox" bind:checked={showRawData} />
				Show raw data
			</label>
		</div>
	</header>
	
	<div class="content">
		<!-- Need Slots Section -->
		<section class="slots-section needs">
			<h2>🙏 My Need Slots ({needSlots.length})</h2>
			
			<div class="add-form">
				<input
					type="text"
					bind:value={newNeedName}
					placeholder="Need name..."
					onkeydown={(e) => e.key === 'Enter' && addNeedSlot()}
				/>
				<select bind:value={newNeedType}>
					{#each needTypes as type}
						<option value={type.id}>{type.label}</option>
					{/each}
				</select>
				<input
					type="number"
					bind:value={newNeedQuantity}
					min="0"
					step="0.1"
				/>
				<button onclick={addNeedSlot} class="btn-primary">
					➕ Add Need
				</button>
			</div>
			
			<div class="slots-list">
				{#if needSlots.length === 0}
					<div class="empty-state">
						No need slots yet. Add one above!
					</div>
				{:else}
					{#each needSlots as slot (slot.id)}
						<div class="slot-card">
							<div class="slot-header">
								<strong>{slot.name}</strong>
								<button onclick={() => removeNeedSlot(slot.id)} class="btn-danger-small">
									🗑️
								</button>
							</div>
							<div class="slot-details">
								<span class="badge">{needTypes.find(t => t.id === slot.need_type_id)?.label || slot.need_type_id}</span>
								<div class="quantity-control">
									<input
										type="number"
										value={slot.quantity}
										min="0"
										step="0.1"
										onchange={(e) => updateNeedQuantity(slot.id, parseFloat(e.currentTarget.value))}
									/>
									<span>{slot.unit || 'units'}</span>
								</div>
							</div>
							{#if showRawData}
								<details class="raw-data">
									<summary>Raw data</summary>
									<pre>{JSON.stringify(slot, null, 2)}</pre>
								</details>
							{/if}
						</div>
					{/each}
				{/if}
			</div>
		</section>
		
		<!-- Capacity Slots Section -->
		<section class="slots-section capacity">
			<h2>🎁 My Capacity Slots ({capacitySlots.length})</h2>
			
			<div class="add-form">
				<input
					type="text"
					bind:value={newCapacityName}
					placeholder="Capacity name..."
					onkeydown={(e) => e.key === 'Enter' && addCapacitySlot()}
				/>
				<select bind:value={newCapacityType}>
					{#each needTypes as type}
						<option value={type.id}>{type.label}</option>
					{/each}
				</select>
				<input
					type="number"
					bind:value={newCapacityQuantity}
					min="0"
					step="0.1"
				/>
				<button onclick={addCapacitySlot} class="btn-primary">
					➕ Add Capacity
				</button>
			</div>
			
			<div class="slots-list">
				{#if capacitySlots.length === 0}
					<div class="empty-state">
						No capacity slots yet. Add one above!
					</div>
				{:else}
					{#each capacitySlots as slot (slot.id)}
						<div class="slot-card">
							<div class="slot-header">
								<strong>{slot.name}</strong>
								<button onclick={() => removeCapacitySlot(slot.id)} class="btn-danger-small">
									🗑️
								</button>
							</div>
							<div class="slot-details">
								<span class="badge">{needTypes.find(t => t.id === slot.need_type_id)?.label || slot.need_type_id}</span>
								<div class="quantity-control">
									<input
										type="number"
										value={slot.quantity}
										min="0"
										step="0.1"
										onchange={(e) => updateCapacityQuantity(slot.id, parseFloat(e.currentTarget.value))}
									/>
									<span>{slot.unit || 'units'}</span>
								</div>
							</div>
							{#if showRawData}
								<details class="raw-data">
									<summary>Raw data</summary>
									<pre>{JSON.stringify(slot, null, 2)}</pre>
								</details>
							{/if}
						</div>
					{/each}
				{/if}
			</div>
		</section>
		
		<!-- Commitment Section (Composed Output) -->
		<section class="commitment-section">
			<h2>📦 My Commitment (Composed)</h2>
			
			{#if commitment}
				<div class="commitment-summary">
					<div class="summary-item">
						<strong>Need Slots:</strong> {commitment.need_slots?.length || 0}
					</div>
					<div class="summary-item">
						<strong>Capacity Slots:</strong> {commitment.capacity_slots?.length || 0}
					</div>
					<div class="summary-item">
						<strong>Recognition Weights:</strong> {Object.keys(commitment.global_recognition_weights || {}).length}
					</div>
					<div class="summary-item">
						<strong>MR Values:</strong> {Object.keys(commitment.global_mr_values || {}).length}
					</div>
					<div class="summary-item">
						<strong>Timestamp:</strong> {new Date(commitment.timestamp).toLocaleString()}
					</div>
				</div>
				
				{#if showRawData}
					<details class="raw-data" open>
						<summary>Full commitment data</summary>
						<pre>{JSON.stringify(commitment, null, 2)}</pre>
					</details>
				{/if}
			{:else}
				<div class="empty-state">
					No commitment yet. Add some slots above!
				</div>
			{/if}
		</section>
		
		<!-- Allocation Flow Section -->
		<section class="allocation-section">
			<h2>🔄 Allocation Flow</h2>
			
			<!-- Outgoing Allocations -->
			<div class="allocation-subsection">
				<h3>🎁 My Outgoing Allocations (Capacity → Recipients)</h3>
				{#if commitment?.slot_allocations && commitment.slot_allocations.length > 0}
					<div class="allocation-summary">
						<div class="summary-stats">
							<div class="stat">
								<strong>Total Allocations:</strong> {commitment.slot_allocations.length}
							</div>
							<div class="stat">
								<strong>Mutual:</strong> {commitment.slot_allocations.filter(a => a.tier === 'mutual').length}
							</div>
							<div class="stat">
								<strong>Generous:</strong> {commitment.slot_allocations.filter(a => a.tier === 'non-mutual').length}
							</div>
						</div>
						
						<div class="allocation-table">
							<table>
								<thead>
									<tr>
										<th>Capacity Slot</th>
										<th>Recipient</th>
										<th>Need Slot</th>
										<th>Quantity</th>
										<th>Type</th>
										<th>Tier</th>
									</tr>
								</thead>
								<tbody>
									{#each commitment.slot_allocations as alloc}
										<tr class="allocation-row {alloc.tier}">
											<td class="slot-id">{alloc.availability_slot_id.slice(0, 8)}...</td>
											<td class="pubkey" title={alloc.recipient_pubkey}>
												{alloc.recipient_pubkey.slice(0, 12)}...{alloc.recipient_pubkey.slice(-4)}
											</td>
											<td class="slot-id">
												{alloc.recipient_need_slot_id ? alloc.recipient_need_slot_id.slice(0, 8) + '...' : 'N/A'}
											</td>
											<td class="quantity">{alloc.quantity.toFixed(2)}</td>
											<td class="type-badge">{alloc.need_type_id}</td>
											<td class="tier-badge {alloc.tier}">
												{alloc.tier === 'mutual' ? '🤝 Mutual' : '💝 Generous'}
											</td>
										</tr>
									{/each}
								</tbody>
							</table>
						</div>
					</div>
				{:else}
					<div class="empty-state">
						No allocations computed yet. Add capacity slots and wait for algorithm to run.
					</div>
				{/if}
			</div>
			
			<!-- Incoming Allocations -->
			<div class="allocation-subsection">
				<h3>📥 My Incoming Allocations (Providers → My Needs)</h3>
				{#if myPub}
					{@const incomingAllocations = Array.from($networkAllocations.entries())
						.flatMap(([pubKey, allocations]) => {
							if (!allocations || allocations.length === 0) return [];
							return allocations
								.filter(alloc => alloc.recipient_pubkey === myPub)
								.map(alloc => ({ ...alloc, providerPubKey: pubKey }));
						})}
					
					{#if incomingAllocations.length > 0}
					<div class="allocation-summary">
						<div class="summary-stats">
							<div class="stat">
								<strong>Total Incoming:</strong> {incomingAllocations.length}
							</div>
							<div class="stat">
								<strong>Mutual:</strong> {incomingAllocations.filter(a => a.tier === 'mutual').length}
							</div>
							<div class="stat">
								<strong>Generous:</strong> {incomingAllocations.filter(a => a.tier === 'non-mutual').length}
							</div>
							<div class="stat">
								<strong>Total Quantity:</strong> {incomingAllocations.reduce((sum, a) => sum + a.quantity, 0).toFixed(2)}
							</div>
						</div>
						
						<div class="allocation-table">
							<table>
								<thead>
									<tr>
										<th>Provider</th>
										<th>Their Capacity</th>
										<th>My Need Slot</th>
										<th>Quantity</th>
										<th>Type</th>
										<th>Tier</th>
									</tr>
								</thead>
								<tbody>
									{#each incomingAllocations as alloc}
										<tr class="allocation-row {alloc.tier}">
											<td class="pubkey" title={alloc.providerPubKey}>
												{alloc.providerPubKey.slice(0, 12)}...{alloc.providerPubKey.slice(-4)}
											</td>
											<td class="slot-id">{alloc.availability_slot_id.slice(0, 8)}...</td>
											<td class="slot-id">
												{alloc.recipient_need_slot_id ? alloc.recipient_need_slot_id.slice(0, 8) + '...' : 'N/A'}
											</td>
											<td class="quantity">{alloc.quantity.toFixed(2)}</td>
											<td class="type-badge">{alloc.need_type_id}</td>
											<td class="tier-badge {alloc.tier}">
												{alloc.tier === 'mutual' ? '🤝 Mutual' : '💝 Generous'}
											</td>
										</tr>
									{/each}
								</tbody>
							</table>
						</div>
					</div>
					{:else}
						<div class="empty-state">
							No incoming allocations yet. Add need slots and wait for others to allocate.
						</div>
					{/if}
				{:else}
					<div class="empty-state">
						Not authenticated. Log in to see incoming allocations.
					</div>
				{/if}
			</div>
		</section>
	</div>
	
	<footer>
		<h3>Architecture Trace (✅ SIMPLIFIED)</h3>
		<div class="trace">
			<div class="trace-step">
				<div class="trace-label">HELPERS</div>
				<code>setMyNeedSlots()</code>
				<code>setMyCapacitySlots()</code>
			</div>
			<div class="trace-arrow">→</div>
			<div class="trace-step">
				<div class="trace-label">SOURCE OF TRUTH</div>
				<code>myCommitmentStore</code>
				<small>(persisted to Holster)</small>
			</div>
			<div class="trace-arrow">→</div>
			<div class="trace-step">
				<div class="trace-label">DERIVED (LOCAL)</div>
				<code>myNeedSlotsStore</code>
				<code>myCapacitySlotsStore</code>
			</div>
			<div class="trace-arrow">→</div>
			<div class="trace-step">
				<div class="trace-label">NETWORK</div>
				<code>networkCommitments</code>
			</div>
			<div class="trace-arrow">→</div>
			<div class="trace-step">
				<div class="trace-label">DERIVED (NETWORK)</div>
				<code>networkNeedSlots</code>
				<code>networkCapacitySlots</code>
			</div>
		</div>
		<div class="trace-note">
			<strong>Key:</strong> 1 persistent store (commitment), 2 local derived stores, 2 network derived stores = SIMPLE! 🎯
		</div>
	</footer>
</div>

<style>
	.test-page {
		display: flex;
		flex-direction: column;
		height: 100vh;
		background: #f5f7fa;
	}
	
	header {
		background: white;
		border-bottom: 2px solid #e1e8ed;
		padding: 1.5rem;
		box-shadow: 0 2px 4px rgba(0,0,0,0.1);
	}
	
	h1 {
		margin: 0 0 0.25rem 0;
		color: #1a1a2e;
	}
	
	.subtitle {
		margin: 0 0 1rem 0;
		color: #666;
		font-size: 0.95rem;
	}
	
	.status-bar {
		display: flex;
		gap: 2rem;
		padding: 0.75rem;
		background: #f8f9fa;
		border-radius: 6px;
		margin-bottom: 1rem;
		flex-wrap: wrap;
	}
	
	.status-item {
		font-size: 0.9rem;
		color: #555;
	}
	
	.controls {
		display: flex;
		gap: 1rem;
		align-items: center;
	}
	
	.content {
		flex: 1;
		overflow-y: auto;
		padding: 1.5rem;
		display: grid;
		grid-template-columns: 1fr 1fr;
		gap: 1.5rem;
	}
	
	.slots-section {
		background: white;
		border-radius: 8px;
		padding: 1.5rem;
		box-shadow: 0 2px 8px rgba(0,0,0,0.1);
	}
	
	.slots-section.needs {
		border-top: 4px solid #3498db;
	}
	
	.slots-section.capacity {
		border-top: 4px solid #2ecc71;
	}
	
	.commitment-section {
		grid-column: 1 / -1;
		background: white;
		border-radius: 8px;
		padding: 1.5rem;
		box-shadow: 0 2px 8px rgba(0,0,0,0.1);
		border-top: 4px solid #9b59b6;
	}
	
	h2 {
		margin: 0 0 1rem 0;
		color: #2c3e50;
	}
	
	.add-form {
		display: flex;
		gap: 0.5rem;
		margin-bottom: 1rem;
		flex-wrap: wrap;
	}
	
	.add-form input[type="text"] {
		flex: 1;
		min-width: 150px;
	}
	
	.add-form input,
	.add-form select {
		padding: 0.5rem;
		border: 1px solid #ddd;
		border-radius: 4px;
		font-size: 0.95rem;
	}
	
	.add-form input[type="number"] {
		width: 80px;
	}
	
	.slots-list {
		display: flex;
		flex-direction: column;
		gap: 0.75rem;
	}
	
	.slot-card {
		border: 1px solid #e1e8ed;
		border-radius: 6px;
		padding: 1rem;
		background: #fafbfc;
	}
	
	.slot-header {
		display: flex;
		justify-content: space-between;
		align-items: center;
		margin-bottom: 0.5rem;
	}
	
	.slot-details {
		display: flex;
		justify-content: space-between;
		align-items: center;
		gap: 1rem;
	}
	
	.quantity-control {
		display: flex;
		align-items: center;
		gap: 0.5rem;
	}
	
	.quantity-control input {
		width: 80px;
		padding: 0.25rem 0.5rem;
		border: 1px solid #ddd;
		border-radius: 4px;
	}
	
	.badge {
		padding: 0.25rem 0.75rem;
		background: #e1e8ed;
		border-radius: 12px;
		font-size: 0.85rem;
	}
	
	.empty-state {
		text-align: center;
		padding: 2rem;
		color: #999;
		font-style: italic;
	}
	
	.commitment-summary {
		display: grid;
		grid-template-columns: repeat(auto-fit, minmax(200px, 1fr));
		gap: 1rem;
		margin-bottom: 1rem;
	}
	
	.summary-item {
		padding: 0.75rem;
		background: #f8f9fa;
		border-radius: 6px;
		font-size: 0.9rem;
	}
	
	.raw-data {
		margin-top: 0.75rem;
		font-size: 0.85rem;
	}
	
	.raw-data pre {
		background: #282c34;
		color: #abb2bf;
		padding: 1rem;
		border-radius: 4px;
		overflow-x: auto;
		max-height: 300px;
		overflow-y: auto;
	}
	
	footer {
		background: white;
		border-top: 2px solid #e1e8ed;
		padding: 1rem 1.5rem;
	}
	
	footer h3 {
		margin: 0 0 1rem 0;
		font-size: 0.9rem;
		text-transform: uppercase;
		color: #666;
	}
	
	.trace {
		display: flex;
		align-items: center;
		gap: 0.5rem;
		overflow-x: auto;
		padding: 0.5rem 0;
	}
	
	.trace-step {
		display: flex;
		flex-direction: column;
		gap: 0.25rem;
		padding: 0.75rem;
		background: #f8f9fa;
		border-radius: 6px;
		min-width: 150px;
	}
	
	.trace-label {
		font-size: 0.7rem;
		font-weight: bold;
		color: #666;
		text-transform: uppercase;
	}
	
	.trace-step code {
		font-size: 0.75rem;
		color: #e74c3c;
		background: #fff;
		padding: 0.25rem 0.5rem;
		border-radius: 3px;
	}
	
	.trace-arrow {
		font-size: 1.5rem;
		color: #95a5a6;
	}
	
	.trace-note {
		margin-top: 1rem;
		padding: 0.75rem;
		background: #e8f4f8;
		border-radius: 6px;
		border-left: 4px solid #3498db;
		font-size: 0.85rem;
		color: #2c3e50;
	}
	
	.trace-step small {
		font-size: 0.65rem;
		color: #95a5a6;
		font-style: italic;
	}
	
	.btn-primary {
		background: #3498db;
		color: white;
		border: none;
		padding: 0.5rem 1rem;
		border-radius: 4px;
		cursor: pointer;
		font-weight: 500;
	}
	
	.btn-primary:hover {
		background: #2980b9;
	}
	
	.btn-secondary {
		background: #95a5a6;
		color: white;
		border: none;
		padding: 0.5rem 1rem;
		border-radius: 4px;
		cursor: pointer;
		font-weight: 500;
	}
	
	.btn-secondary:hover {
		background: #7f8c8d;
	}
	
	.btn-danger-small {
		background: #e74c3c;
		color: white;
		border: none;
		padding: 0.25rem 0.5rem;
		border-radius: 4px;
		cursor: pointer;
		font-size: 0.85rem;
	}
	
	.btn-danger-small:hover {
		background: #c0392b;
	}
	
	label {
		display: flex;
		align-items: center;
		gap: 0.5rem;
		cursor: pointer;
	}
	
	/* Allocation Section Styles */
	.allocation-section {
		background: white;
		padding: 1.5rem;
		border-radius: 8px;
		box-shadow: 0 2px 4px rgba(0,0,0,0.1);
		grid-column: 1 / -1;
	}
	
	.allocation-subsection {
		margin-top: 2rem;
	}
	
	.allocation-subsection:first-child {
		margin-top: 1rem;
	}
	
	.allocation-subsection h3 {
		color: #2c3e50;
		margin: 0 0 1rem 0;
		font-size: 1.1rem;
	}
	
	.allocation-summary {
		margin-top: 1rem;
	}
	
	.summary-stats {
		display: flex;
		gap: 2rem;
		margin-bottom: 1rem;
		padding: 1rem;
		background: #f8f9fa;
		border-radius: 6px;
		flex-wrap: wrap;
	}
	
	.stat {
		color: #555;
		font-size: 0.9rem;
	}
	
	.stat strong {
		color: #2c3e50;
		margin-right: 0.5rem;
	}
	
	.allocation-table {
		overflow-x: auto;
	}
	
	.allocation-table table {
		width: 100%;
		border-collapse: collapse;
		font-size: 0.9rem;
	}
	
	.allocation-table thead {
		background: #34495e;
		color: white;
	}
	
	.allocation-table th {
		padding: 0.75rem;
		text-align: left;
		font-weight: 600;
	}
	
	.allocation-table td {
		padding: 0.75rem;
		border-bottom: 1px solid #e1e8ed;
	}
	
	.allocation-row {
		transition: background 0.2s;
	}
	
	.allocation-row:hover {
		background: #f8f9fa;
	}
	
	.allocation-row.mutual {
		border-left: 3px solid #3498db;
	}
	
	.allocation-row.non-mutual {
		border-left: 3px solid #f39c12;
	}
	
	.slot-id {
		font-family: 'Monaco', 'Menlo', 'Courier New', monospace;
		font-size: 0.85rem;
		color: #7f8c8d;
	}
	
	.pubkey {
		font-family: 'Monaco', 'Menlo', 'Courier New', monospace;
		font-size: 0.85rem;
		color: #2c3e50;
		font-weight: 500;
	}
	
	.quantity {
		font-weight: 600;
		color: #27ae60;
		text-align: right;
	}
	
	.type-badge {
		display: inline-block;
		padding: 0.25rem 0.5rem;
		background: #ecf0f1;
		border-radius: 4px;
		font-size: 0.85rem;
		color: #34495e;
	}
	
	.tier-badge {
		padding: 0.3rem 0.6rem;
		border-radius: 4px;
		font-size: 0.85rem;
		font-weight: 500;
	}
	
	.tier-badge.mutual {
		background: #e3f2fd;
		color: #1976d2;
	}
	
	.tier-badge.non-mutual {
		background: #fff3e0;
		color: #f57c00;
	}
	
	@media (max-width: 1024px) {
		.content {
			grid-template-columns: 1fr;
		}
		
		.commitment-section {
			grid-column: 1;
		}
		
		.allocation-section {
			grid-column: 1;
		}
		
		.summary-stats {
			flex-direction: column;
			gap: 0.75rem;
		}
		
		.allocation-table {
			font-size: 0.8rem;
		}
		
		.allocation-table th,
		.allocation-table td {
			padding: 0.5rem;
		}
	}
</style>

