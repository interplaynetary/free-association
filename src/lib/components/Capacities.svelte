<script lang="ts">
	import { onMount } from 'svelte';
	import { globalState } from '$lib/global.svelte';
	import type { Commitment, AvailabilitySlot } from '$lib/commons/v5/schemas';
	import { findNodeById, updateNodeById } from '$lib/commons/v5/protocol';
	import { Calendar, DatePicker, Button } from 'bits-ui';
	import { getLocalTimeZone, today } from '@internationalized/date';
	import { userAlias, userPub } from '$lib/state/auth.svelte';
	// V5: Import from v5 stores - CORRECT PATTERN: Use source stores + composition
	import {
		myRecognitionTreeStore as userTree,
		myCommitmentStore,
		myCapacitySlotsStore,
		composeCommitmentFromSources,
		setMyCapacitySlots // ✅ NEW: Helper to update capacity slots
	} from '$lib/commons/v5/stores.svelte';
	// V5: Composition features not yet implemented
	// import { userDesiredSlotComposeFrom, userDesiredSlotComposeInto } from '$lib/state/core.svelte';
	import Capacity from './Capacity.svelte';
	import { t } from '$lib/translations';

	// V5 Pure Types - No Backward Compatibility
	type CommitmentWithId = Commitment & { id: string };
	type CommitmentsCollection = Record<string, Commitment>;

	// Helper to extract searchable text from commitment's slots
	function getSearchableText(commitment: Commitment): string {
		const slots = commitment.capacity_slots || [];
		return slots
			.map(
				(slot) =>
					`${slot.name} ${slot.emoji || ''} ${slot.unit || ''} ${slot.description || ''} ${slot.need_type_id || ''}`
			)
			.join(' ')
			.toLowerCase();
	}

	// ═══════════════════════════════════════════════════════════════════
	// V5 VIRTUAL GROUPING (Option 1B): Group Slots by capacity_group_id
	// ═══════════════════════════════════════════════════════════════════
	//
	// ARCHITECTURE SHIFT:
	// - Data Model: ONE commitment with ALL capacity_slots (array)
	// - UI Layer: Slots grouped by capacity_group_id into "capacity" cards
	// - Virtual "capacity" ID = capacity_group_id (or slot.id if no group)
	//
	// This allows users to:
	// - Have multiple separate "capacity" cards
	// - Add multiple slots within each card
	// - Slots with same capacity_group_id stay grouped together
	// ═══════════════════════════════════════════════════════════════════
	const userCapacities = $derived(() => {
		const commitment = $myCommitmentStore;
		const pub = $userPub;
		if (!commitment || !pub) return {};

		const slots = commitment.capacity_slots || [];

		// Special case: No slots yet - show one empty "capacity" card
		if (slots.length === 0) {
			return {
				[`${pub}-empty`]: {
					...commitment,
					capacity_slots: []
				}
			};
		}

		// ✅ Group slots by capacity_group_id (or individual slot.id if no group)
		const virtualCapacities: Record<string, CommitmentWithId> = {};

		slots.forEach((slot) => {
			// Use capacity_group_id if available, otherwise slot.id (for ungrouped slots)
			const groupId = (slot as any).capacity_group_id || slot.id;
			const virtualCapacityId = `${pub}-${groupId}`;

			if (!virtualCapacities[virtualCapacityId]) {
				virtualCapacities[virtualCapacityId] = {
					...commitment,
					id: virtualCapacityId,
					capacity_slots: [],
					// Preserve all other commitment data
					need_slots: commitment.need_slots,
					global_recognition_weights: commitment.global_recognition_weights,
					global_mr_values: commitment.global_mr_values,
					multi_dimensional_damping: commitment.multi_dimensional_damping,
					itcStamp: commitment.itcStamp,
					timestamp: commitment.timestamp
				};
			}

			// Add this slot to the group
			virtualCapacities[virtualCapacityId].capacity_slots!.push(slot);
		});

		return virtualCapacities;
	});

	// Reactive derived values
	const capacityEntries = $derived(() => {
		let entries = Object.entries(userCapacities() || {})
			.filter(([id, commitment]) => id && commitment)
			.map(([id, commitment]) => ({ ...commitment, id }) as CommitmentWithId);

		// Apply search filter from global state
		if (globalState.inventorySearchQuery.trim()) {
			const query = globalState.inventorySearchQuery.toLowerCase().trim();
			entries = entries.filter((entry) => getSearchableText(entry).includes(query));
		}

		return entries;
	});

	// ═══════════════════════════════════════════════════════════════════
	// CREATE: Add new "capacity" (append slot(s) to array)
	// ═══════════════════════════════════════════════════════════════════
	// In v5 virtual grouping, creating a "capacity" means adding slot(s) to the
	// single capacity_slots array. Each new slot will appear as its own "capacity" card.
	// ═══════════════════════════════════════════════════════════════════
	function addCommitment(commitment: CommitmentWithId) {
		if (!$userAlias || !$userPub) return false;

		console.log('[CAPACITIES] Adding capacity slots:', commitment.capacity_slots);

		if (commitment.capacity_slots) {
			commitment.capacity_slots.forEach((slot, index) => {
				console.log(`[CAPACITIES] Slot ${index} (${slot.id}):`, {
					need_type_id: slot.need_type_id,
					name: slot.name,
					quantity: slot.quantity,
					location_type: slot.location_type,
					city: slot.city,
					country: slot.country
				});
			});
		}

		// ✅ SAFE: Append to existing slots array
		const currentSlots = $myCapacitySlotsStore || [];
		const newSlots = [...currentSlots, ...(commitment.capacity_slots || [])];
		setMyCapacitySlots(newSlots); // ✅ NEW: Use helper instead of store.set()

		// Highlight the new capacity slots
		if (commitment.capacity_slots) {
			commitment.capacity_slots.forEach((slot) => {
				globalState.highlightSlot(slot.id);
			});
		}

		// Show success toast with first slot name or generic message
		const firstName = commitment.capacity_slots?.[0]?.name || 'Capacity';
		globalState.showToast(`Created: ${firstName}`, 'success');
		return true;
	}

	// ═══════════════════════════════════════════════════════════════════
	// UPDATE: Update existing "capacity" (update specific slot(s), preserve others)
	// ═══════════════════════════════════════════════════════════════════
	// ⚠️ CRITICAL: Must preserve slots from other virtual "capacities"!
	// This virtual "capacity" contains only the slot(s) being updated.
	// We need to keep all OTHER slots unchanged in the array.
	// ═══════════════════════════════════════════════════════════════════
	function updateCommitment(commitment: CommitmentWithId) {
		try {
			if (!$userAlias || !$userPub) return false;

			console.log('[CAPACITIES] Updating capacity slots:', commitment.capacity_slots);

			if (commitment.capacity_slots) {
				commitment.capacity_slots.forEach((slot, index) => {
					console.log(`[CAPACITIES] Slot ${index} (${slot.id}):`, {
						need_type_id: slot.need_type_id,
						name: slot.name,
						quantity: slot.quantity,
						location_type: slot.location_type,
						city: slot.city,
						country: slot.country
					});
				});
			}

			// ✅ SAFE: Update specific slots while preserving others
			const allSlots = $myCapacitySlotsStore || [];
			const updatedSlotIds = new Set((commitment.capacity_slots || []).map((s) => s.id));

			// Keep slots that aren't being updated
			const unchangedSlots = allSlots.filter((s) => !updatedSlotIds.has(s.id));

			// Merge unchanged + updated slots
			const newSlots = [...unchangedSlots, ...(commitment.capacity_slots || [])];

			setMyCapacitySlots(newSlots); // ✅ NEW: Use helper instead of store.set()

			// Show success toast with first slot name or generic message
			const firstName = commitment.capacity_slots?.[0]?.name || 'Capacity';
			globalState.showToast(`Updated: ${firstName}`, 'success');
			return true;
		} catch (error) {
			console.error('[CAPACITIES] Error updating capacity slots:', error);
			globalState.showToast($t('errors.error_occurred'), 'error');
			return false;
		}
	}

	// Comprehensive slot composition cleanup when deleting a commitment
	function cleanupCapacitySlotData(capacityId: string) {
		console.log(`🧹 [CLEANUP] Starting comprehensive cleanup for commitment: ${capacityId}`);

		// V5: Get the commitment to find all its slot IDs before deletion
		const commitment = userCapacities()[capacityId];
		if (!commitment) {
			console.warn(`[CLEANUP] Commitment ${capacityId} not found, skipping slot cleanup`);
			return;
		}

		const slotIds = commitment.capacity_slots?.map((slot) => slot.id) || [];
		console.log(`[CLEANUP] Found ${slotIds.length} slots to clean up:`, slotIds);

		// V5: Composition features not yet implemented - cleanup code removed
		// TODO: Re-enable composition cleanup when v5 composition is implemented

		console.log(`✅ [CLEANUP] Completed cleanup for capacity: ${capacityId}`);
	}

	// ═══════════════════════════════════════════════════════════════════
	// DELETE: Remove "capacity" (filter out slot(s) from array, preserve others)
	// ═══════════════════════════════════════════════════════════════════
	// ⚠️ CRITICAL: Must only remove slots from THIS virtual "capacity"!
	// This virtual "capacity" contains only specific slot(s).
	// We need to keep all OTHER slots unchanged in the array.
	// ═══════════════════════════════════════════════════════════════════
	function deleteCommitment(commitmentId: string) {
		try {
			if (!$userAlias || !$userPub) return false;

			// STEP 1: Clean up slot composition data BEFORE deleting
			cleanupCapacitySlotData(commitmentId);

			// STEP 2: Get the virtual capacity to find which slots to remove
			const virtualCapacity = userCapacities()[commitmentId];
			if (!virtualCapacity) {
				console.warn(`[CAPACITIES] Virtual capacity ${commitmentId} not found`);
				return false;
			}

			// STEP 3: ✅ SAFE: Remove only the slots from this virtual capacity
			const allSlots = $myCapacitySlotsStore || [];
			const slotIdsToRemove = new Set((virtualCapacity.capacity_slots || []).map((s) => s.id));

			// Filter out the slots being deleted, keep all others
			const remainingSlots = allSlots.filter((s) => !slotIdsToRemove.has(s.id));

			setMyCapacitySlots(remainingSlots); // ✅ NEW: Use helper instead of store.set()

			const slotName = virtualCapacity.capacity_slots?.[0]?.name || 'Capacity';
			globalState.showToast(`Deleted: ${slotName}`, 'success');
			return true;
		} catch (error) {
			console.error('[CAPACITIES] Error deleting capacity slots:', error);
			globalState.showToast($t('errors.error_occurred'), 'error');
			return false;
		}
	}

	// Random capacity generation data
	const CAPACITY_VERBS = [
		'planting',
		'housing',
		'cuddling',
		'washing',
		'teaching',
		'cooking',
		'healing',
		'building',
		'painting',
		'singing',
		'dancing',
		'writing',
		'listening',
		'hugging',
		'feeding',
		'cleaning',
		'organizing',
		'mentoring',
		'debugging',
		'designing',
		'crafting',
		'gardening',
		'storytelling',
		'massaging',
		'translating',
		'editing',
		'photographing',
		'composing',
		'sculpting',
		'knitting',
		'baking',
		'brewing',
		'fixing',
		'tuning',
		'polishing',
		'assembling',
		'delivering',
		'packaging',
		'sorting',
		'folding',
		'watering',
		'harvesting',
		'pruning',
		'weeding',
		'composting',
		'recycling',
		'walking',
		'running',
		'cycling',
		'swimming',
		'stretching',
		'meditating',
		'breathing',
		'smiling',
		'laughing',
		'cheering',
		'encouraging',
		'inspiring',
		'motivating',
		'celebrating',
		'welcoming',
		'connecting',
		'bridging',
		'linking',
		'sharing',
		'gifting',
		'donating',
		'volunteering',
		'helping'
	];

	const CAPACITY_UNITS = [
		'trees',
		'people',
		'hours',
		'hugs',
		'meals',
		'songs',
		'stories',
		'smiles',
		'laughs',
		'tears',
		'words',
		'pages',
		'chapters',
		'verses',
		'notes',
		'beats',
		'steps',
		'breaths',
		'moments',
		'memories',
		'dreams',
		'wishes',
		'hopes',
		'ideas',
		'thoughts',
		'feelings',
		'emotions',
		'cups',
		'bowls',
		'plates',
		'loaves',
		'batches',
		'servings',
		'portions',
		'sips',
		'bites',
		'patches',
		'stitches',
		'rows',
		'loops',
		'knots',
		'folds',
		'layers',
		'coats',
		'strokes',
		'seeds',
		'sprouts',
		'blooms',
		'fruits',
		'leaves',
		'branches',
		'roots',
		'drops',
		'streams',
		'waves',
		'ripples',
		'bubbles',
		'sparks',
		'flames',
		'glows',
		'rays',
		'beams',
		'shadows',
		'bridges',
		'paths',
		'doors',
		'windows',
		'keys',
		'locks',
		'boxes',
		'gifts',
		'surprises',
		'adventures',
		'journeys',
		'discoveries',
		'explorations',
		'experiments',
		'creations',
		'innovations',
		'connections',
		'conversations',
		'exchanges',
		'meetings',
		'gatherings',
		'celebrations',
		'dances'
	];

	const CAPACITY_EMOJIS = [
		'🌱',
		'🏠',
		'🤗',
		'🧼',
		'📚',
		'🍳',
		'💚',
		'🔨',
		'🎨',
		'🎵',
		'💃',
		'✍️',
		'👂',
		'🫂',
		'🍽️',
		'🧹',
		'📋',
		'🎯',
		'🐛',
		'✨',
		'🛠️',
		'🌿',
		'📖',
		'💆',
		'🌍',
		'📝',
		'📸',
		'🎼',
		'🗿',
		'🧶',
		'🥧',
		'☕',
		'🔧',
		'🎹',
		'✨',
		'🔩',
		'📦',
		'📮',
		'📂',
		'👔',
		'💧',
		'🌾',
		'✂️',
		'🌿',
		'♻️',
		'🚶',
		'🏃',
		'🚴',
		'🏊',
		'🧘',
		'😊',
		'😄',
		'📣',
		'💪',
		'🎉',
		'👋',
		'🤝',
		'🌉',
		'🔗',
		'🎁',
		'❤️',
		'🙋',
		'🤲'
	];

	const INTERESTING_CITIES = [
		// Magical/Mystical
		{ name: 'Reykjavik', country: 'Iceland' },
		{ name: 'Kyoto', country: 'Japan' },
		{ name: 'Prague', country: 'Czech Republic' },
		{ name: 'Edinburgh', country: 'Scotland' },
		{ name: 'Marrakech', country: 'Morocco' },
		{ name: 'Bruges', country: 'Belgium' },
		{ name: 'Santorini', country: 'Greece' },
		{ name: 'Ubud', country: 'Bali' },

		// Creative/Artistic
		{ name: 'Florence', country: 'Italy' },
		{ name: 'Barcelona', country: 'Spain' },
		{ name: 'Vienna', country: 'Austria' },
		{ name: 'Copenhagen', country: 'Denmark' },
		{ name: 'Amsterdam', country: 'Netherlands' },
		{ name: 'Portland', country: 'USA' },
		{ name: 'Montreal', country: 'Canada' },
		{ name: 'Berlin', country: 'Germany' },

		// Nature/Adventure
		{ name: 'Queenstown', country: 'New Zealand' },
		{ name: 'Whistler', country: 'Canada' },
		{ name: 'Banff', country: 'Canada' },
		{ name: 'Interlaken', country: 'Switzerland' },
		{ name: 'Tromso', country: 'Norway' },
		{ name: 'Patagonia', country: 'Chile' },
		{ name: 'Zermatt', country: 'Switzerland' },
		{ name: 'Lofoten', country: 'Norway' },

		// Cozy/Community
		{ name: 'Salzburg', country: 'Austria' },
		{ name: 'Ghent', country: 'Belgium' },
		{ name: 'Ljubljana', country: 'Slovenia' },
		{ name: 'Tallinn', country: 'Estonia' },
		{ name: 'Heidelberg', country: 'Germany' },
		{ name: 'Colmar', country: 'France' },
		{ name: 'Sintra', country: 'Portugal' },
		{ name: 'Hallstatt', country: 'Austria' },

		// Vibrant/Energetic
		{ name: 'Lisbon', country: 'Portugal' },
		{ name: 'Buenos Aires', country: 'Argentina' },
		{ name: 'Melbourne', country: 'Australia' },
		{ name: 'Cape Town', country: 'South Africa' },
		{ name: 'Istanbul', country: 'Turkey' },
		{ name: 'Havana', country: 'Cuba' },
		{ name: 'Mumbai', country: 'India' },
		{ name: 'Rio de Janeiro', country: 'Brazil' }
	];

	const RECURRENCE_PATTERNS = [
		'Daily',
		'Weekly',
		'Monthly',
		'Bi-weekly',
		'Weekdays',
		'Weekends',
		'Custom'
	];

	const LOCATION_TYPES = [
		'In-Person',
		'Online',
		'Hybrid',
		'Mobile',
		'Outdoor',
		'Home-based',
		'Community Center',
		'Undefined'
	];

	// Helper function to generate playful quantities
	function generatePlayfulQuantity(): number {
		const patterns = [
			// Small precise numbers
			() => Math.floor(Math.random() * 5) + 1, // 1-5
			// Medium round numbers
			() => (Math.floor(Math.random() * 8) + 1) * 5, // 5, 10, 15, 20, 25, 30, 35, 40
			// Fun specific numbers
			() => [7, 11, 13, 17, 21, 42, 69, 99, 108, 144][Math.floor(Math.random() * 10)],
			// Powers and doubles
			() => Math.pow(2, Math.floor(Math.random() * 6) + 1), // 2, 4, 8, 16, 32, 64
			// Dozens
			() => (Math.floor(Math.random() * 10) + 1) * 12 // 12, 24, 36... up to 120
		];

		const selectedPattern = patterns[Math.floor(Math.random() * patterns.length)];
		return selectedPattern();
	}

	// Helper function to generate random time patterns
	function generateTimePattern() {
		const allDay = Math.random() < 0.6; // 60% chance of all-day
		const recurrence = RECURRENCE_PATTERNS[Math.floor(Math.random() * RECURRENCE_PATTERNS.length)];

		let startTime = null;
		let endTime = null;

		if (!allDay) {
			// Generate interesting time slots
			const timeSlots = [
				{ start: '06:00', end: '08:00' }, // Early morning
				{ start: '08:00', end: '10:00' }, // Morning
				{ start: '10:00', end: '12:00' }, // Late morning
				{ start: '12:00', end: '14:00' }, // Lunch
				{ start: '14:00', end: '16:00' }, // Afternoon
				{ start: '16:00', end: '18:00' }, // Late afternoon
				{ start: '18:00', end: '20:00' }, // Evening
				{ start: '20:00', end: '22:00' }, // Night
				{ start: '09:00', end: '17:00' }, // Full work day
				{ start: '19:00', end: '21:00' } // Dinner time
			];

			const slot = timeSlots[Math.floor(Math.random() * timeSlots.length)];
			startTime = slot.start;
			endTime = slot.end;
		}

		return { allDay, recurrence, startTime, endTime };
	}

	// Generate a random commitment with interesting verb + unit combination (v5)
	function generateRandomCommitment(): CommitmentWithId {
		if (!$userAlias || !$userPub) throw new Error('No user logged in');

		const verb = CAPACITY_VERBS[Math.floor(Math.random() * CAPACITY_VERBS.length)];
		const unit = CAPACITY_UNITS[Math.floor(Math.random() * CAPACITY_UNITS.length)];
		const emoji = CAPACITY_EMOJIS[Math.floor(Math.random() * CAPACITY_EMOJIS.length)];
		const city = INTERESTING_CITIES[Math.floor(Math.random() * INTERESTING_CITIES.length)];

		// Generate playful quantity
		const quantity = generatePlayfulQuantity();

		// Generate some variety in divisibility - make it more interesting
		const maxNaturalDiv = Math.floor(Math.random() * Math.min(quantity, 10)) + 1;
		const maxPercentageDiv = Math.random() * 0.8 + 0.2; // 0.2 to 1.0

		const todayString = today(getLocalTimeZone()).toString();
		const timePattern = generateTimePattern();
		const id = crypto.randomUUID();

		// ✅ Generate unique group ID for this new capacity
		const capacityGroupId = `group-${Date.now()}-${Math.random().toString(36).substr(2, 9)}`;

		// V5 Structure with capacity_slots
		return {
			id,
			capacity_slots: [
				{
					id: `slot-${Date.now()}-${Math.random().toString(36).substr(2, 9)}`,
					quantity: quantity,
					need_type_id: 'need_type_general', // Default need type - should be selectable in UI
					name: verb,
					emoji: emoji,
					unit: unit,
					description: '', // Leave blank for auto-generated commitments
					location_type: 'Specific',
					city: city.name,
					country: city.country,
					start_date: todayString,
					end_date: null,
					time_zone: getLocalTimeZone(),
					recurrence: timePattern.recurrence as any,
					// V5: Use availability_window for time ranges instead of start_time/end_time
					availability_window:
						timePattern.allDay || !timePattern.startTime || !timePattern.endTime
							? undefined
							: {
									time_ranges: [
										{
											start_time: timePattern.startTime,
											end_time: timePattern.endTime
										}
									]
								},
					max_natural_div: maxNaturalDiv,
					max_percentage_div: maxPercentageDiv,
					filter_rule: null,
					capacity_group_id: capacityGroupId // ✅ Group ID for virtual grouping
				} as any
			],
			timestamp: Date.now(),
			itcStamp: { id: 1, event: 0 } as any // Placeholder ITC stamp
		};
	}

	// Create a new commitment with default values (v5)
	function createDefaultCommitment(): CommitmentWithId {
		if (!$userAlias || !$userPub) throw new Error('No user logged in');

		const todayString = today(getLocalTimeZone()).toString();
		const id = crypto.randomUUID();

		// ✅ Generate unique group ID for this new capacity
		const capacityGroupId = `group-${Date.now()}-${Math.random().toString(36).substr(2, 9)}`;

		return {
			id,
			capacity_slots: [
				{
					id: `slot-${Date.now()}-${Math.random().toString(36).substr(2, 9)}`,
					quantity: 1,
					need_type_id: 'need_type_general', // Default need type
					name: '',
					emoji: '',
					unit: '',
					description: '',
					location_type: 'Undefined',
					start_date: todayString,
					end_date: null,
					time_zone: getLocalTimeZone(),
					recurrence: 'daily',
					max_natural_div: 1,
					max_percentage_div: 1.0,
					filter_rule: null,
					capacity_group_id: capacityGroupId // ✅ Group ID for virtual grouping
				} as any
			],
			timestamp: Date.now(),
			itcStamp: { id: 1, event: 0 } as any // Placeholder ITC stamp
		};
	}

	// Add a new commitment row
	function addCommitmentRow() {
		if (!$userAlias || !$userPub) return;

		const newCommitment = createDefaultCommitment();

		// Use the addCommitment function to properly add the commitment
		const success = addCommitment(newCommitment);
		if (!success) {
			globalState.showToast($t('errors.error_occurred'), 'error');
			return;
		}
	}

	// Add a random commitment with interesting verb + unit combination
	function addRandomCommitment() {
		if (!$userAlias || !$userPub) return;

		const newCommitment = generateRandomCommitment();

		// Use the addCommitment function to properly add the commitment
		const success = addCommitment(newCommitment);
		if (!success) {
			globalState.showToast($t('errors.error_occurred'), 'error');
			return;
		}

		// Extract location info for toast from first capacity slot
		const slot = newCommitment.capacity_slots![0];
		const locationInfo = slot.location_type === 'Online' ? 'online' : `in ${slot.city}`;
		const quantityInfo = `${slot.quantity} ${slot.unit}`;

		globalState.showToast(`🎲 ${slot.name} ${quantityInfo} ${locationInfo}!`, 'success');
	}

	// Handle commitment update from child component
	function handleCommitmentUpdate(commitment: CommitmentWithId) {
		const success = updateCommitment(commitment);
		if (!success) {
			globalState.showToast($t('errors.error_occurred'), 'error');
		}
	}

	// Handle commitment delete from child component
	function handleCommitmentDelete(id: string) {
		const success = deleteCommitment(id);
		if (!success) {
			globalState.showToast($t('errors.error_occurred'), 'error');
		}
	}
</script>

<div class="capacities-list grid grid-cols-1 gap-3 p-2 md:grid-cols-2 lg:grid-cols-3">
	{#each capacityEntries() as entry (entry.id)}
		<div
			class="capacity-wrapper"
			class:newly-created-capacity={globalState.highlightedCapacities.has(entry.id)}
		>
			<Capacity
				capacity={entry}
				canDelete={true}
				onupdate={handleCommitmentUpdate}
				ondelete={handleCommitmentDelete}
			/>
		</div>
	{/each}
	<div class="button-container my-2 flex justify-center gap-2">
		<button
			type="button"
			class="add-btn h-10 w-10"
			onclick={addCommitmentRow}
			title="Add blank commitment">+</button
		>
		<button
			type="button"
			class="dice-btn h-10 w-10"
			onclick={addRandomCommitment}
			title="Generate random commitment">🎲</button
		>
	</div>
</div>

<style>
	:global(body) {
		font-family: 'Inter', system-ui, sans-serif;
		background: #f7fafc;
	}

	/* Add button styles - override Bits UI Button with this class */
	.add-btn {
		background: #f0fdf4;
		color: #16a34a;
		border: none;
		border-radius: 50%;
		font-size: 1.3em;
		font-weight: 500;
		cursor: pointer;
		transition: all 0.2s ease;
		box-shadow: 0 1px 3px rgba(0, 0, 0, 0.05);
		display: flex;
		align-items: center;
		justify-content: center;
	}
	.add-btn:hover {
		background: #dcfce7;
		color: #15803d;
		transform: scale(1.05);
		box-shadow: 0 2px 5px rgba(0, 0, 0, 0.08);
	}

	/* Dice button styles - similar to add button but with different colors */
	.dice-btn {
		background: #fef7ff;
		color: #9333ea;
		border: none;
		border-radius: 50%;
		font-size: 1.2em;
		font-weight: 500;
		cursor: pointer;
		transition: all 0.2s ease;
		box-shadow: 0 1px 3px rgba(0, 0, 0, 0.05);
		display: flex;
		align-items: center;
		justify-content: center;
	}
	.dice-btn:hover {
		background: #f3e8ff;
		color: #7c3aed;
		transform: scale(1.05) rotate(20deg);
		box-shadow: 0 2px 5px rgba(0, 0, 0, 0.08);
	}

	/* Button container styles */
	.button-container {
		grid-column: 1 / -1; /* Span all grid columns */
	}

	/* Newly created capacity highlight animation */
	@keyframes capacityHighlightPulse {
		0% {
			background-color: rgba(59, 130, 246, 0.15);
			box-shadow: 0 0 0 0 rgba(59, 130, 246, 0.4);
		}
		50% {
			background-color: rgba(59, 130, 246, 0.1);
			box-shadow: 0 0 0 12px rgba(59, 130, 246, 0.1);
		}
		100% {
			background-color: rgba(59, 130, 246, 0.05);
			box-shadow: 0 0 0 0 rgba(59, 130, 246, 0);
		}
	}

	@keyframes capacityHighlightFadeOut {
		0% {
			background-color: rgba(59, 130, 246, 0.05);
		}
		100% {
			background-color: transparent;
		}
	}

	/* Apply highlight styling to newly created capacities */
	.capacity-wrapper.newly-created-capacity {
		animation:
			capacityHighlightPulse 2.5s ease-in-out,
			capacityHighlightFadeOut 1s ease-out 2.5s;
		background: rgba(239, 246, 255, 0.6);
		border: 2px solid rgba(59, 130, 246, 0.3);
		border-radius: 12px;
		padding: 8px;
		margin: -8px;
		transition: all 0.3s ease;
	}

	.capacity-wrapper.newly-created-capacity :global(.capacity-item) {
		background: rgba(248, 250, 252, 0.9) !important;
		border-color: rgba(59, 130, 246, 0.2) !important;
	}

	.capacity-wrapper.newly-created-capacity :global(.capacity-row) {
		background: rgba(255, 255, 255, 0.95) !important;
		border-color: rgba(59, 130, 246, 0.15) !important;
	}

	/* Ensure grid layout isn't affected by the wrapper */
	.capacity-wrapper {
		display: contents;
	}

	.capacity-wrapper.newly-created-capacity {
		display: block;
	}
</style>
