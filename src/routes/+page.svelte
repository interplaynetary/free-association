<script lang="ts">
	import { onMount } from 'svelte';
	import Parent from '$lib/components/Parent.svelte';
	import Bar from '$lib/components/Bar.svelte';
	import Map from '$lib/components/Map.svelte';
	import Type from '$lib/components/Type.svelte';
	// V5: Import from v5 stores - fully reactive, no manual recalculation needed!
	import { 
		myRecognitionTreeStore, 
		myRecognitionWeights, 
		myMutualRecognition,
		myNeedSlotsStore,
		myCapacitySlotsStore,
		myNeedTypesStore,
		myCapacityTypesStore,
		myCommitmentStore,
		initializeAllocationStores,
		enableAutoCommitmentComposition,
		setMyNeedSlots,
		setMyCapacitySlots
	} from '$lib/protocol/stores.svelte';
	import { enableAutoAllocationPublishing } from '$lib/protocol/allocation.svelte';
	import { globalState } from '$lib/global.svelte';
	import { derived } from 'svelte/store';
	import { t, loading } from '$lib/translations';
	import type { NeedSlot, AvailabilitySlot } from '$lib/protocol/schemas';
	import { NEED_TYPES, formatNeedType } from '$lib/protocol/utils/needTypes';

	// Reactive view state
	const currentView = $derived(globalState.currentView);

	// Reactive state for inventory view (Svelte 5 runes)
	let needSlots = $state<NeedSlot[]>([]);
	let capacitySlots = $state<AvailabilitySlot[]>([]);

	// Form state for adding new slots
	let newNeedName = $state('');
	let newNeedType = $state('food');
	let newNeedQuantity = $state(10);

	let newCapacityName = $state('');
	let newCapacityType = $state('food');
	let newCapacityQuantity = $state(5);

	let showRawData = $state(false);

	// Cleanup functions
	let cleanupComposition: (() => void) | null = null;
	let cleanupAllocationPublishing: (() => void) | null = null;

	onMount(() => {
		console.log('[HOME] Initializing stores for inventory view...');
		
		// Initialize stores
		initializeAllocationStores();
		
		// Subscribe to stores (reactive)
		const unsubNeeds = myNeedSlotsStore.subscribe((slots) => {
			needSlots = slots || [];
		});
		
		const unsubCapacity = myCapacitySlotsStore.subscribe((slots) => {
			capacitySlots = slots || [];
		});
		
		// Enable auto-composition
		cleanupComposition = enableAutoCommitmentComposition();
		
		// Enable auto-allocation publishing
		cleanupAllocationPublishing = enableAutoAllocationPublishing();
		
		console.log('[HOME] ✅ Initialized and subscribed');
		
		return () => {
			unsubNeeds();
			unsubCapacity();
			if (cleanupComposition) cleanupComposition();
			if (cleanupAllocationPublishing) cleanupAllocationPublishing();
		};
	});

	// CRUD Operations - Needs
	function addNeedSlot() {
		if (!newNeedName.trim()) return;
		
		const newSlot: NeedSlot = {
			id: `need_${Date.now()}_${Math.random()}`,
			name: newNeedName,
			need_type_id: newNeedType,
			quantity: newNeedQuantity,
			unit: 'units',
			max_natural_div: 1,
			min_allocation_percentage: 0.01
		};
		
		setMyNeedSlots([...needSlots, newSlot]);
		
		// Reset form
		newNeedName = '';
		newNeedQuantity = 10;
	}
	
	function removeNeedSlot(id: string) {
		setMyNeedSlots(needSlots.filter(s => s.id !== id));
	}
	
	function updateNeedQuantity(id: string, quantity: number) {
		const updated = needSlots.map(s =>
			s.id === id ? { ...s, quantity } : s
		);
		setMyNeedSlots(updated);
	}

	// CRUD Operations - Capacity
	function addCapacitySlot() {
		if (!newCapacityName.trim()) return;
		
		const newSlot: AvailabilitySlot = {
			id: `capacity_${Date.now()}_${Math.random()}`,
			name: newCapacityName,
			need_type_id: newCapacityType,
			quantity: newCapacityQuantity,
			unit: 'units',
			max_natural_div: 1,
			min_allocation_percentage: 0.01
		};
		
		setMyCapacitySlots([...capacitySlots, newSlot]);
		
		// Reset form
		newCapacityName = '';
		newCapacityQuantity = 5;
	}
	
	function removeCapacitySlot(id: string) {
		setMyCapacitySlots(capacitySlots.filter(s => s.id !== id));
	}
	
	function updateCapacityQuantity(id: string, quantity: number) {
		const updated = capacitySlots.map(s =>
			s.id === id ? { ...s, quantity } : s
		);
		setMyCapacitySlots(updated);
	}

	// Batch update handlers for Type component
	function handleNeedTypeBatchUpdate(typeId: string, updates: Partial<NeedSlot>) {
		const updated = needSlots.map(slot =>
			slot.need_type_id === typeId ? { ...slot, ...updates } : slot
		);
		setMyNeedSlots(updated);
	}
	
	function handleCapacityTypeBatchUpdate(typeId: string, updates: Partial<AvailabilitySlot>) {
		const updated = capacitySlots.map(slot =>
			slot.need_type_id === typeId ? { ...slot, ...updates } : slot
		);
		setMyCapacitySlots(updated);
	}

	// Individual slot update handlers (for full slot editing with new editors)
	function handleNeedSlotUpdate(updatedSlot: NeedSlot) {
		const updated = needSlots.map(s =>
			s.id === updatedSlot.id ? updatedSlot : s
		);
		setMyNeedSlots(updated);
	}
	
	function handleCapacitySlotUpdate(updatedSlot: AvailabilitySlot) {
		const updated = capacitySlots.map(s =>
			s.id === updatedSlot.id ? updatedSlot : s
		);
		setMyCapacitySlots(updated);
	}

	// V5: Create reactive derived store from myRecognitionWeights (replaces userSogf)
	// Recognition weights are automatically computed from the tree in v5!
	const barSegments = derived(myRecognitionWeights, ($weights) => {
		console.log('[📊 UI-YR] Recognition weights changed - generating segments for bar...');
		
		if (!$weights || Object.keys($weights).length === 0) {
			console.log('[📊 UI-YR] ❌ No recognition weights available');
			return [];
		}

		const totalEntries = Object.keys($weights).length;
		const nonZeroEntries = Object.values($weights).filter(v => v > 0).length;
		console.log(`[📊 UI-YR] Recognition weights has ${totalEntries} entries (${nonZeroEntries} non-zero)`);

		// Transform recognition weights into segments for Bar
		const segments = Object.entries($weights)
			.filter(([_, value]) => value > 0) // Only include non-zero values
			.map(([id, value]) => ({
				id,
				value: value * 100 // Convert from decimal to percentage
			}))
			.sort((a, b) => b.value - a.value); // Sort by value descending
		
		console.log(`[📊 UI-YR] ✅ Generated ${segments.length} segments for recognition bar:`);
		segments.forEach(seg => {
			console.log(`  • ${seg.id.slice(0, 20)}... → ${seg.value.toFixed(2)}%`);
		});
		
		return segments;
	});

	// V5: Create reactive derived store from myMutualRecognition (replaces generalShares)
	// Mutual recognition is automatically computed from recognition weights + network data in v5!
	const providerSegments = derived(myMutualRecognition, ($mutualRec) => {
		console.log('[📊 UI-MR] Mutual recognition changed - generating segments for bar...');

		if (!$mutualRec || Object.keys($mutualRec).length === 0) {
			console.log('[📊 UI-MR] ❌ No mutual recognition data available');
			return [];
		}

		const totalEntries = Object.keys($mutualRec).length;
		const nonZeroEntries = Object.values($mutualRec).filter(v => v > 0).length;
		console.log(`[📊 UI-MR] Mutual recognition has ${totalEntries} entries (${nonZeroEntries} non-zero)`);

		// Transform mutual recognition data into segments for Bar
		const segments = Object.entries($mutualRec)
			.filter(([_, value]) => value > 0) // Only include non-zero values
			.map(([id, value]) => ({
				id,
				value: value * 100 // Convert from decimal to percentage
			}))
			.sort((a, b) => b.value - a.value); // Sort by value descending

		console.log(`[📊 UI-MR] ✅ Generated ${segments.length} segments for mutual recognition bar:`);
		segments.forEach(seg => {
			console.log(`  • ${seg.id.slice(0, 20)}... → ${seg.value.toFixed(2)}%`);
		});
		
		return segments;
	});

	// V5: No manual recalculation needed! Everything is reactive 🎉
	// Recognition weights auto-update when tree changes
	// Mutual recognition auto-updates when recognition weights or network data changes
</script>

<div class="layout root-page" class:full-width={currentView !== 'tree'}>
	<div class="view-content">
		{#if currentView === 'tree'}
			<Parent />
		{:else if currentView === 'map'}
			<Map fullHeight={true} />
		{:else if currentView === 'inventory'}
			<div class="inventory-view">
				<!-- Need Slots Section (LEFT) -->
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
							{#each NEED_TYPES as type}
								<option value={type.id}>{formatNeedType(type.id)}</option>
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
							<!-- Organize need slots by type -->
							{#each $myNeedTypesStore as typeId (typeId)}
								<Type 
									{typeId} 
									typeName={formatNeedType(typeId)}
									slots={$myNeedSlotsStore} 
									kind="need"
									capacityId="need-{typeId}"
									onBatchUpdate={handleNeedTypeBatchUpdate}
									onSlotUpdate={handleNeedSlotUpdate}
									onSlotDelete={removeNeedSlot}
								>
									{#snippet children({ slot }: { slot: NeedSlot })}
										{#if showRawData}
											<details class="raw-data">
												<summary>Raw data</summary>
												<pre>{JSON.stringify(slot, null, 2)}</pre>
											</details>
										{/if}
									{/snippet}
								</Type>
							{/each}
						{/if}
					</div>
				</section>
				
				<!-- Capacity Slots Section (RIGHT) -->
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
							{#each NEED_TYPES as type}
								<option value={type.id}>{formatNeedType(type.id)}</option>
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
							<!-- Organize capacity slots by type -->
							{#each $myCapacityTypesStore as typeId (typeId)}
								<Type 
									{typeId} 
									typeName={formatNeedType(typeId)}
									slots={$myCapacitySlotsStore} 
									kind="capacity"
									capacityId="capacity-{typeId}"
									onBatchUpdate={handleCapacityTypeBatchUpdate}
									onSlotUpdate={handleCapacitySlotUpdate}
									onSlotDelete={removeCapacitySlot}
								>
									{#snippet children({ slot }: { slot: AvailabilitySlot })}
										{#if showRawData}
											<details class="raw-data">
												<summary>Raw data</summary>
												<pre>{JSON.stringify(slot, null, 2)}</pre>
											</details>
										{/if}
									{/snippet}
								</Type>
							{/each}
						{/if}
					</div>
				</section>
			</div>
		{/if}
	</div>
	{#if currentView === 'tree'}
		{#key $loading}
		<div class="bars">
		<div class="bar-group">
			<div
				class="bar-label bar-label-yr"
				title={$t('home.your_recognition_description')}
			>
				<span class="label-mobile">{@html $t('home.your_recognition').toLowerCase().replace(' ', '<br />')}</span>
				<span class="label-desktop">{$t('home.your_recognition_abbr')}</span>
			</div>
			<div class="bar-area">
				{#if $barSegments.length > 0}
					<Bar
						segments={$barSegments}
						width="100%"
						height="100%"
						showLabelsOnSelect={true}
						showValues={false}
						rounded={false}
					/>
				{:else}
					<div class="placeholder">
						<p>
							{$t('home.no_contributors')}
						</p>
					</div>
				{/if}
			</div>
		</div>
		<div class="bar-group">
			<div
				class="bar-label bar-label-mr"
				title={$t('home.mutual_recognition_description')}
			>
				<span class="label-mobile">{@html $t('home.mutual_recognition').toLowerCase().replace(' ', '<br />')}</span>
				<span class="label-desktop">{$t('home.mutual_recognition_abbr')}</span>
			</div>
			<div class="bar-area">
				{#if $providerSegments.length > 0}
					<Bar
						segments={$providerSegments}
						width="100%"
						height="100%"
						showLabelsOnSelect={true}
						showValues={false}
						rounded={false}
					/>
				{:else}
					<div class="placeholder">
						<p>{$t('home.no_mutual_contributors')}</p>
					</div>
				{/if}
			</div>
		</div>
		</div>
		{/key}
	{/if}
</div>

<style>
	/* Removed conflicting :global(body) styles - handled by layout */

	.layout {
		display: grid;
		grid-template-columns: 9fr 1fr;
		width: 100%;
		height: 100%;
		max-height: 100%;
		overflow: hidden;
		user-select: none;
	}

	/* Full-width layout when bars are hidden */
	.layout.full-width {
		grid-template-columns: 1fr;
	}

	/* Root page specific: ensure it doesn't scroll */
	.layout.root-page {
		overflow: hidden;
		height: 100%;
		max-height: 100%;
		position: relative;
	}

	.view-content,
	.bars {
		width: 100%;
		height: 100%;
		overflow: auto;
	}

	.bars {
		display: flex;
		gap: 0.5rem;
		padding: 0.5rem;
	}

	/* Mobile: Horizontal bars stacked vertically */
	@media (max-width: 768px) {
		.bars {
			flex-direction: column;
			height: auto;
		}

		.bar-group {
			display: grid;
			grid-template-columns: auto 1fr;
			gap: 0.75rem;
			align-items: center;
			height: 2rem;
			width: 100%;
		}

		.bar-area {
			height: 100%;
			width: 100%;
		}

		.bar-label {
			white-space: normal;
		}

		.label-mobile {
			display: inline;
		}

		.label-desktop {
			display: none;
		}
	}

	/* Desktop: Vertical bars side by side */
	@media (min-width: 769px) {
		.bars {
			flex-direction: row;
			height: 100%;
		}

		.bar-group {
			display: flex;
			flex-direction: column;
			width: 2rem;
			height: 100%;
			min-height: 0;
			max-height: 100%;
			gap: 0.25rem;
			overflow: hidden;
		}

		.bar-area {
			flex: 1;
			order: 1;
			display: flex;
			align-items: flex-end;
			width: 100%;
			min-height: 0;
		}

		.bar-label {
			order: 2;
			font-size: min(0.5em, 1vw);
			padding: 0 0.25rem;
			max-width: 100%;
			text-align: center;
		}

		.label-mobile {
			display: none;
		}

		.label-desktop {
			display: inline;
		}
	}

	.bar-label {
		font-size: min(0.6em, 1.2vh);
		color: #666;
		text-transform: uppercase;
		letter-spacing: 0.05em;
		font-weight: 500;
		line-height: 1.1;
		overflow: hidden;
		text-overflow: ellipsis;
	}

	.placeholder {
		height: 100%;
		display: flex;
		align-items: center;
		justify-content: center;
		text-align: center;
		color: #666;
		font-size: 0.9em;
		padding: 1rem;
		background: #f5f5f5;
		border-radius: 4px;
	}

	.inventory-view {
		padding: 1rem;
		overflow-y: auto;
		height: 100%;
		display: grid;
		grid-template-columns: 1fr 1fr;
		gap: 1.5rem;
	}

	.inventory-view h2 {
		margin: 0 0 1rem 0;
		color: #2c3e50;
	}

	/* Slots Section Styles */
	.slots-section {
		background: white;
		border-radius: 8px;
		padding: 1.5rem;
		box-shadow: 0 2px 8px rgba(0,0,0,0.1);
		display: flex;
		flex-direction: column;
	}

	.slots-section.needs {
		border-top: 4px solid #3498db;
	}

	.slots-section.capacity {
		border-top: 4px solid #2ecc71;
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
		overflow-y: auto;
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

	.empty-state {
		text-align: center;
		padding: 2rem;
		color: #999;
		font-style: italic;
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

	/* Slot Actions Row (used in Type component snippet) */
	.slot-actions-row {
		display: flex;
		justify-content: space-between;
		align-items: center;
		gap: 1rem;
		flex-wrap: wrap;
	}

	.slot-actions-row .quantity-control {
		display: flex;
		align-items: center;
		gap: 0.5rem;
	}

	.slot-actions-row .quantity-control label {
		font-size: 0.85rem;
		font-weight: 500;
		color: #666;
	}

	.slot-actions-row .quantity-control input {
		width: 80px;
		padding: 0.4rem;
		border: 1px solid #ddd;
		border-radius: 4px;
	}

	/* Responsive layout for mobile */
	@media (max-width: 768px) {
		.layout {
			grid-template-columns: 1fr;
			grid-template-rows: 1fr auto;
		}

		.bars {
			flex-direction: column;
			height: auto;
			gap: 0.5rem;
		}

		.placeholder {
			padding: 0.5rem;
			font-size: 0.8em;
		}

		.inventory-view {
			grid-template-columns: 1fr;
			gap: 1rem;
		}

		.add-form {
			flex-direction: column;
		}

		.add-form input[type="number"] {
			width: 100%;
		}
	}
</style>
