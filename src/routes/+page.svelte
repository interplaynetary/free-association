<script lang="ts">
	import { onMount } from 'svelte';
	import Parent from '$lib/components/Parent.svelte';
	import Bar from '$lib/components/Bar.svelte';
	import Map from '$lib/components/Map.svelte';
	import ResourceSlots from '$lib/components/ResourceSlots.svelte';
	import Capacity from '$lib/components/Capacity.svelte';
	// V5: Import from v5 stores - fully reactive, no manual recalculation needed!
	import { 
		myRecognitionTreeStore, 
		myRecognitionWeights, 
		myMutualRecognition,
		myNeedSlotsStore,
		myCapacitySlotsStore,
		myCommitmentStore,
		enableAutoCommitmentComposition,
		setMyNeedSlots,
		setMyCapacitySlots
	} from '$lib/protocol/stores/stores.svelte';
	import { 
		enableAutoAllocationPublishing, 
		initializeAllocationStores 
	} from '$lib/protocol/stores/allocation.svelte';
	import { globalState } from '$lib/global.svelte';
	import { derived } from 'svelte/store';
	import { t, loading } from '$lib/translations';
	import type { NeedSlot, AvailabilitySlot } from '$lib/protocol/schemas';

	// Reactive view state
	const currentView = $derived(globalState.currentView);

	// Reactive state for inventory view (Svelte 5 runes) - USD only!
	let needSlots = $state<NeedSlot[]>([]);
	let capacitySlots = $state<AvailabilitySlot[]>([]);
	
	// Demo capacity for testing Capacity component display
	let demoCapacity = $state({
		id: 'demo-capacity-1',
		timestamp: Date.now(),
		capacity_slots: [
			{
				id: 'demo-slot-1',
				name: 'Demo Tutoring Session',
				emoji: '📚',
				quantity: 5,
				type_id: 'general',
				location_type: 'Online' as const,
				online_link: 'https://meet.example.com',
				start_date: new Date().toISOString().split('T')[0],
				end_date: undefined,
				start_time: '14:00',
				end_time: '16:00',
				all_day: false,
				time_zone: Intl.DateTimeFormat().resolvedOptions().timeZone,
				recurrence: 'weekly' as const,
				max_natural_div: 1,
				min_allocation_percentage: 0.01
			},
			{
				id: 'demo-slot-2',
				name: 'Demo Workshop',
				emoji: '🛠️',
				quantity: 10,
				type_id: 'general',
				location_type: 'Specific' as const,
				street_address: '123 Main St, City',
				latitude: 40.7128,
				longitude: -74.0060,
				start_date: new Date(Date.now() + 7 * 24 * 60 * 60 * 1000).toISOString().split('T')[0],
				end_date: undefined,
				start_time: '10:00',
				end_time: '12:00',
				all_day: false,
				time_zone: Intl.DateTimeFormat().resolvedOptions().timeZone,
				recurrence: 'monthly' as const,
				max_natural_div: 1,
				min_allocation_percentage: 0.01
			}
		]
	} as any);


	// Cleanup functions
	let cleanupComposition: (() => void) | null = null;
	let cleanupAllocationPublishing: (() => void) | null = null;

	onMount(() => {
		console.log('[TRACE] [ENTER] src/routes/+page.svelte: onMount');
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

	// CRUD Operations - Generalized for any need type
	function addNeedSlot(name: string, quantity: number, needTypeId: string) {
		console.log('[TRACE] [ENTER] src/routes/+page.svelte: addNeedSlot', { name, quantity, needTypeId });
		const newSlot: NeedSlot = {
			id: `need_${Date.now()}_${Math.random()}`,
			name: name,
			type_id: needTypeId,
			quantity: quantity,
			unit: needTypeId === 'money' ? 'USD' : 'units',
			max_natural_div: 1,
			min_allocation_percentage: 0.01,
			recurrence: 'monthly' // Default to per month
		};
		
		
		setMyNeedSlots([...needSlots, newSlot]);
		console.log('[TRACE] [EXIT] src/routes/+page.svelte: addNeedSlot');
	}
	
	function removeNeedSlot(id: string) {
		console.log('[TRACE] [ENTER] src/routes/+page.svelte: removeNeedSlot', { id });
		setMyNeedSlots(needSlots.filter(s => s.id !== id));
		console.log('[TRACE] [EXIT] src/routes/+page.svelte: removeNeedSlot');
	}
	
	function updateNeedSlot(updatedSlot: NeedSlot) {
		console.log('[TRACE] [ENTER] src/routes/+page.svelte: updateNeedSlot', { id: updatedSlot.id });
		const updated = needSlots.map(s =>
			s.id === updatedSlot.id ? updatedSlot : s
		);
		setMyNeedSlots(updated);
		console.log('[TRACE] [EXIT] src/routes/+page.svelte: updateNeedSlot');
	}

	// CRUD Operations - Generalized capacity for any need type
	function addCapacitySlot(name: string, quantity: number, needTypeId: string) {
		console.log('[TRACE] [ENTER] src/routes/+page.svelte: addCapacitySlot', { name, quantity, needTypeId });
		const newSlot: AvailabilitySlot = {
			id: `capacity_${Date.now()}_${Math.random()}`,
			name: name,
			type_id: needTypeId,
			quantity: quantity,
			unit: needTypeId === 'money' ? 'USD' : 'units',
			max_natural_div: 1,
			min_allocation_percentage: 0.01,
			recurrence: 'monthly' // Default to per month
		};
		
		setMyCapacitySlots([...capacitySlots, newSlot]);
		console.log('[TRACE] [EXIT] src/routes/+page.svelte: addCapacitySlot');
	}
	
	function removeCapacitySlot(id: string) {
		console.log('[TRACE] [ENTER] src/routes/+page.svelte: removeCapacitySlot', { id });
		setMyCapacitySlots(capacitySlots.filter(s => s.id !== id));
		console.log('[TRACE] [EXIT] src/routes/+page.svelte: removeCapacitySlot');
	}
	
	function updateCapacitySlot(updatedSlot: AvailabilitySlot) {
		console.log('[TRACE] [ENTER] src/routes/+page.svelte: updateCapacitySlot', { id: updatedSlot.id });
		const updated = capacitySlots.map(s =>
			s.id === updatedSlot.id ? updatedSlot : s
		);
		setMyCapacitySlots(updated);
		console.log('[TRACE] [EXIT] src/routes/+page.svelte: updateCapacitySlot');
	}

	// V5: Create reactive derived store from myRecognitionWeights (replaces userSogf)
	// Recognition weights are automatically computed from the tree in v5!
	const barSegments = derived(myRecognitionWeights, ($weights) => {
		console.log('[TRACE] [STEP] src/routes/+page.svelte: barSegments (derived recalculation)');
		console.log('[📊 UI-YR] Recognition weights changed - generating segments for bar...');
		
		// Defensive: Handle undefined/null weights (iOS Safari hydration timing)
		if (!$weights || typeof $weights !== 'object' || Object.keys($weights).length === 0) {
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
		console.log('[TRACE] [STEP] src/routes/+page.svelte: providerSegments (derived recalculation)');
		console.log('[📊 UI-MR] Mutual recognition changed - generating segments for bar...');

		// Defensive: Handle undefined/null mutual recognition (iOS Safari hydration timing)
		if (!$mutualRec || typeof $mutualRec !== 'object' || Object.keys($mutualRec).length === 0) {
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
				<!-- Resource Slots Component with type selector and tabs -->
				<ResourceSlots
					{needSlots}
					{capacitySlots}
					onNeedUpdate={updateNeedSlot}
					onNeedDelete={removeNeedSlot}
					onCapacityUpdate={updateCapacitySlot}
					onCapacityDelete={removeCapacitySlot}
					onNeedAdd={addNeedSlot}
					onCapacityAdd={addCapacitySlot}
				/>
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
					<div class="bar-wrapper">
						<Bar
							segments={$barSegments}
							width="100%"
							height="100%"
							showLabelsOnSelect={true}
							showValues={false}
							rounded={false}
						/>
					</div>
				{:else}
					<div class="placeholder">
						<p>
							{$t('home.no_contributors')}
						</p>
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
		/* Responsive grid: bar gets fixed width, treemap takes remaining space */
		grid-template-columns: 1fr minmax(60px, 80px);
		gap: 0;
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
		/* Changed from overflow: auto to prevent unwanted scrolling */
		overflow: hidden;
	}

	.bars {
		display: flex;
		gap: 0.5rem;
		padding: 0.5rem;
		box-sizing: border-box;
		min-width: 0;
	}

	.bar-wrapper {
		width: 100%;
		height: 100%;
		border-radius: 6px;
		overflow: hidden;
		position: relative;
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
			height: 4rem;
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

	/* Larger screens: give bar more space */
	@media (min-width: 1200px) {
		.layout:not(.full-width) {
			grid-template-columns: 1fr 100px;
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
			width: 100%;
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
			font-size: min(0.8em, 1.5vw);
			padding: 0 0.25rem;
			max-width: 100%;
			text-align: center;
			/* Prevent text overflow from expanding container */
			overflow: hidden;
			text-overflow: ellipsis;
			white-space: nowrap;
		}

		.label-mobile {
			display: none;
		}

		.label-desktop {
			display: inline;
		}
	}

	.bar-label {
		font-size: min(0.9em, 2vh);
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
		display: flex;
		flex-direction: column;
		height: 100%;
		overflow-y: auto;
		background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
		gap: 1rem;
		padding-bottom: 1rem;
	}
	
	.capacity-test-section {
		margin: 0 0.75rem;
		padding: 1rem;
		background: rgba(255, 255, 255, 0.95);
		border-radius: 8px;
		box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1);
	}
	
	.test-header {
		margin-bottom: 1rem;
		padding-bottom: 0.75rem;
		border-bottom: 2px solid #e5e7eb;
	}
	
	.test-header h3 {
		margin: 0 0 0.5rem 0;
		font-size: 1.125rem;
		font-weight: 700;
		color: #1f2937;
	}
	
	.test-header p {
		margin: 0;
		font-size: 0.875rem;
		color: #6b7280;
		font-style: italic;
	}

	/* Responsive layout for mobile */
	@media (max-width: 768px) {
		.layout {
			grid-template-columns: 1fr;
			grid-template-rows: minmax(0, 1fr) auto; /* Allow treemap to shrink, give bar minimum space */
		}

		/* Override height constraints for mobile stacking */
		.view-content {
			height: auto;
			min-height: 0;
		}

		.bars {
			flex-direction: column;
			height: auto;
			gap: 0.5rem;
			/* Maintain consistent padding on mobile, but increase horizontal padding */
			padding: 0.5rem 1rem;
		}

		.placeholder {
			padding: 0.5rem;
			font-size: 0.8em;
		}
	}
</style>
