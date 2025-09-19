<script lang="ts">
	import { onMount } from 'svelte';
	import { globalState } from '$lib/global.svelte';
	import type {
		ProviderCapacity,
		CapacitiesCollection,
		CapacitiesCollectionData
	} from '$lib/schema';
	import {
		findNodeById,
		addCapacity as addCapacityToCollection,
		updateNodeById
	} from '$lib/protocol';
	import { Calendar, DatePicker, Button } from 'bits-ui';
	import { getLocalTimeZone, today } from '@internationalized/date';
	import { get } from 'svelte/store';
	import { userAlias, userPub } from '$lib/state/gun.svelte';
	import {
		userTree,
		userCapacities,
		updateStoreWithFreshTimestamp,
		userCapacitiesTimestamp,
		userDesiredSlotComposeFrom,
		userDesiredSlotComposeInto,
		userDesiredSlotComposeFromTimestamp,
		userDesiredSlotComposeIntoTimestamp
	} from '$lib/state/core.svelte';
	import Capacity from './Capacity.svelte';

	// Reactive derived values
	const capacityEntries = $derived(
		Object.entries($userCapacities || {})
			.filter(([id, capacity]) => id && capacity)
			.map(([id, capacity]) => ({ ...capacity, id }))
	);

	// Add a new capacity to the userCapacities store
	function addCapacity(capacity: ProviderCapacity) {
		if (!$userAlias || !$userPub) return false;

		// 🚨 DEBUG: Log incoming new capacity with location data
		console.log('[CAPACITIES] 🚨 DEBUG: addCapacity called with capacity:', capacity.id);
		console.log(
			'[CAPACITIES] 🚨 DEBUG: New capacity availability_slots:',
			capacity.availability_slots
		);
		if (capacity.availability_slots) {
			capacity.availability_slots.forEach((slot: any, index: number) => {
				console.log(`[CAPACITIES] 🚨 DEBUG: New slot ${index} (${slot.id}) location data:`, {
					location_type: slot.location_type,
					latitude: slot.latitude,
					longitude: slot.longitude,
					street_address: slot.street_address,
					city: slot.city,
					state_province: slot.state_province,
					postal_code: slot.postal_code,
					country: slot.country
				});
			});
		}

		// Create a deep clone of current capacities
		const newCapacities: CapacitiesCollectionData = structuredClone($userCapacities || {});

		// Create a plain object copy of the capacity
		const plainCapacity = { ...capacity };

		// 🚨 DEBUG: Log capacity after shallow copy
		console.log(
			'[CAPACITIES] 🚨 DEBUG: After shallow copy, plainCapacity availability_slots:',
			plainCapacity.availability_slots
		);
		if (plainCapacity.availability_slots) {
			plainCapacity.availability_slots.forEach((slot: any, index: number) => {
				console.log(
					`[CAPACITIES] 🚨 DEBUG: After copy - New slot ${index} (${slot.id}) location data:`,
					{
						location_type: slot.location_type,
						latitude: slot.latitude,
						longitude: slot.longitude,
						street_address: slot.street_address,
						city: slot.city,
						state_province: slot.state_province,
						postal_code: slot.postal_code,
						country: slot.country
					}
				);
			});
		}

		// Add capacity to the collection
		addCapacityToCollection(newCapacities, plainCapacity);

		// 🚨 DEBUG: Log what's in the collection after addCapacityToCollection
		const addedCapacity = newCapacities[plainCapacity.id];
		console.log(
			'[CAPACITIES] 🚨 DEBUG: After addCapacityToCollection, capacity in collection:',
			addedCapacity
		);
		if (addedCapacity?.availability_slots) {
			addedCapacity.availability_slots.forEach((slot: any, index: number) => {
				console.log(
					`[CAPACITIES] 🚨 DEBUG: Final collection - New slot ${index} (${slot.id}) location data:`,
					{
						location_type: slot.location_type,
						latitude: slot.latitude,
						longitude: slot.longitude,
						street_address: slot.street_address,
						city: slot.city,
						state_province: slot.state_province,
						postal_code: slot.postal_code,
						country: slot.country
					}
				);
			});
		}

		// 🚨 CRITICAL: Use atomic update to ensure data and timestamp sync
		updateStoreWithFreshTimestamp(userCapacities, userCapacitiesTimestamp, newCapacities);

		// Add to highlighted capacities using global state
		globalState.highlightCapacity(capacity.id);

		globalState.showToast(`Capacity "${capacity.name}" created`, 'success');
		return true;
	}

	// Update capacity in the userCapacities store
	function updateCapacity(capacity: ProviderCapacity) {
		try {
			if (!$userAlias || !$userPub) return false;

			// 🚨 DEBUG: Log incoming capacity with location data
			console.log('[CAPACITIES] 🚨 DEBUG: updateCapacity called with capacity:', capacity.id);
			console.log(
				'[CAPACITIES] 🚨 DEBUG: Capacity availability_slots:',
				capacity.availability_slots
			);
			if (capacity.availability_slots) {
				capacity.availability_slots.forEach((slot: any, index: number) => {
					console.log(`[CAPACITIES] 🚨 DEBUG: Slot ${index} (${slot.id}) location data:`, {
						location_type: slot.location_type,
						latitude: slot.latitude,
						longitude: slot.longitude,
						street_address: slot.street_address,
						city: slot.city,
						state_province: slot.state_province,
						postal_code: slot.postal_code,
						country: slot.country
					});
				});
			}

			// Create a deep clone of current capacities
			const newCapacities: CapacitiesCollectionData = structuredClone($userCapacities || {});

			// Create a deep copy of the capacity to ensure store updates
			const plainCapacity = structuredClone(capacity);

			// 🚨 DEBUG: Log capacity after structuredClone
			console.log(
				'[CAPACITIES] 🚨 DEBUG: After structuredClone, plainCapacity availability_slots:',
				plainCapacity.availability_slots
			);
			if (plainCapacity.availability_slots) {
				plainCapacity.availability_slots.forEach((slot: any, index: number) => {
					console.log(
						`[CAPACITIES] 🚨 DEBUG: After clone - Slot ${index} (${slot.id}) location data:`,
						{
							location_type: slot.location_type,
							latitude: slot.latitude,
							longitude: slot.longitude,
							street_address: slot.street_address,
							city: slot.city,
							state_province: slot.state_province,
							postal_code: slot.postal_code,
							country: slot.country
						}
					);
				});
			}

			// Add capacity to the collection
			newCapacities[plainCapacity.id] = plainCapacity;

			// 🚨 DEBUG: Log what's actually being set in the store
			console.log(
				'[CAPACITIES] 🚨 DEBUG: About to set userCapacities store with:',
				newCapacities[plainCapacity.id]
			);
			if (newCapacities[plainCapacity.id]?.availability_slots) {
				newCapacities[plainCapacity.id].availability_slots.forEach((slot: any, index: number) => {
					console.log(
						`[CAPACITIES] 🚨 DEBUG: Final store data - Slot ${index} (${slot.id}) location data:`,
						{
							location_type: slot.location_type,
							latitude: slot.latitude,
							longitude: slot.longitude,
							street_address: slot.street_address,
							city: slot.city,
							state_province: slot.state_province,
							postal_code: slot.postal_code,
							country: slot.country
						}
					);
				});
			}

			// 🚨 CRITICAL: Use atomic update to ensure data and timestamp sync
			updateStoreWithFreshTimestamp(userCapacities, userCapacitiesTimestamp, newCapacities);

			globalState.showToast(`Capacity "${plainCapacity.name}" updated`, 'success');
			return true;
		} catch (error) {
			console.error('Error updating capacity:', error);
			globalState.showToast('Error updating capacity', 'error');
			return false;
		}
	}

	// Comprehensive slot composition cleanup when deleting a capacity
	function cleanupCapacitySlotData(capacityId: string) {
		console.log(`🧹 [CLEANUP] Starting comprehensive cleanup for capacity: ${capacityId}`);

		// Get the capacity to find all its slot IDs before deletion
		const capacity = $userCapacities?.[capacityId];
		if (!capacity) {
			console.warn(`[CLEANUP] Capacity ${capacityId} not found, skipping slot cleanup`);
			return;
		}

		const slotIds = capacity.availability_slots?.map((slot: any) => slot.id) || [];
		console.log(`[CLEANUP] Found ${slotIds.length} slots to clean up:`, slotIds);

		// 1. Clean up userDesiredSlotComposeFrom (where deleted capacity is SOURCE)
		const currentComposeFrom = get(userDesiredSlotComposeFrom);
		const updatedComposeFrom = { ...currentComposeFrom };
		let changes = 0;

		// Remove entries where deleted capacity is the source
		if (updatedComposeFrom[capacityId]) {
			changes += Object.keys(updatedComposeFrom[capacityId]).length;
			delete updatedComposeFrom[capacityId];
		}

		// Remove entries where deleted capacity is the target
		Object.keys(updatedComposeFrom).forEach((sourceCapId) => {
			Object.keys(updatedComposeFrom[sourceCapId]).forEach((sourceSlotId) => {
				if (updatedComposeFrom[sourceCapId][sourceSlotId][capacityId]) {
					changes += Object.keys(updatedComposeFrom[sourceCapId][sourceSlotId][capacityId]).length;
					delete updatedComposeFrom[sourceCapId][sourceSlotId][capacityId];

					// Clean up empty objects
					if (Object.keys(updatedComposeFrom[sourceCapId][sourceSlotId]).length === 0) {
						delete updatedComposeFrom[sourceCapId][sourceSlotId];
					}
					if (Object.keys(updatedComposeFrom[sourceCapId]).length === 0) {
						delete updatedComposeFrom[sourceCapId];
					}
				}
			});
		});

		console.log(`[CLEANUP] Cleaned ${changes} userDesiredSlotComposeFrom entries`);
		// Use atomic update to ensure timestamp consistency
		updateStoreWithFreshTimestamp(
			userDesiredSlotComposeFrom,
			userDesiredSlotComposeFromTimestamp,
			updatedComposeFrom
		);

		// 2. Clean up userDesiredSlotComposeInto (where deleted capacity is SOURCE or TARGET)
		const currentComposeInto = get(userDesiredSlotComposeInto);
		const updatedComposeInto = { ...currentComposeInto };
		let changesInto = 0;

		// Remove entries where deleted capacity is the source
		if (updatedComposeInto[capacityId]) {
			changesInto += Object.keys(updatedComposeInto[capacityId]).length;
			delete updatedComposeInto[capacityId];
		}

		// Remove entries where deleted capacity is the target
		Object.keys(updatedComposeInto).forEach((sourceCapId) => {
			Object.keys(updatedComposeInto[sourceCapId]).forEach((sourceSlotId) => {
				if (updatedComposeInto[sourceCapId][sourceSlotId][capacityId]) {
					changesInto += Object.keys(
						updatedComposeInto[sourceCapId][sourceSlotId][capacityId]
					).length;
					delete updatedComposeInto[sourceCapId][sourceSlotId][capacityId];

					// Clean up empty objects
					if (Object.keys(updatedComposeInto[sourceCapId][sourceSlotId]).length === 0) {
						delete updatedComposeInto[sourceCapId][sourceSlotId];
					}
					if (Object.keys(updatedComposeInto[sourceCapId]).length === 0) {
						delete updatedComposeInto[sourceCapId];
					}
				}
			});
		});

		console.log(`[CLEANUP] Cleaned ${changesInto} userDesiredSlotComposeInto entries`);
		// Use atomic update to ensure timestamp consistency
		updateStoreWithFreshTimestamp(
			userDesiredSlotComposeInto,
			userDesiredSlotComposeIntoTimestamp,
			updatedComposeInto
		);

		// 3. Cleanup complete - slot claims are now handled via compose-from-self

		console.log(`✅ [CLEANUP] Completed comprehensive cleanup for capacity: ${capacityId}`);
	}

	// Delete capacity from the userCapacities store with comprehensive cleanup
	function deleteCapacity(capacityId: string) {
		try {
			if (!$userAlias || !$userPub) return false;

			// STEP 1: Clean up all slot composition data BEFORE deleting the capacity
			cleanupCapacitySlotData(capacityId);

			// STEP 2: Remove the capacity itself
			const currentCaps = get(userCapacities) || {};
			const newCaps: CapacitiesCollectionData = { ...currentCaps };
			if (newCaps[capacityId]) {
				delete newCaps[capacityId];
			}

			// 🚨 CRITICAL: Use atomic update to ensure data and timestamp sync
			updateStoreWithFreshTimestamp(userCapacities, userCapacitiesTimestamp, newCaps);

			globalState.showToast('Capacity and all related slot data deleted', 'success');
			return true;
		} catch (error) {
			console.error('Error deleting capacity:', error);
			globalState.showToast('Error deleting capacity', 'error');
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

	// Generate a random capacity with interesting verb + unit combination
	function generateRandomCapacity(): ProviderCapacity {
		if (!$userAlias || !$userPub) throw new Error('No user logged in');

		const verb = CAPACITY_VERBS[Math.floor(Math.random() * CAPACITY_VERBS.length)];
		const unit = CAPACITY_UNITS[Math.floor(Math.random() * CAPACITY_UNITS.length)];
		const emoji = CAPACITY_EMOJIS[Math.floor(Math.random() * CAPACITY_EMOJIS.length)];

		// Generate a random quantity between 1-10
		const quantity = Math.floor(Math.random() * 10) + 1;

		// Generate some variety in divisibility
		const maxNaturalDiv = Math.floor(Math.random() * 5) + 1;
		const maxPercentageDiv = Math.random() * 0.8 + 0.2; // 0.2 to 1.0

		const todayString = today(getLocalTimeZone()).toString();

		return {
			id: crypto.randomUUID(),
			name: verb,
			emoji: emoji,
			unit: unit,
			description: `Randomly generated capacity for ${verb} ${unit}`,
			max_natural_div: maxNaturalDiv,
			max_percentage_div: maxPercentageDiv,
			hidden_until_request_accepted: false,
			owner_id: $userPub,
			filter_rule: null,
			availability_slots: [
				{
					id: `slot-${Date.now()}-${Math.random().toString(36).substr(2, 9)}`,
					quantity: quantity,
					location_type: 'Undefined',
					all_day: true,
					start_date: todayString,
					start_time: null,
					end_date: null,
					end_time: null,
					time_zone: getLocalTimeZone(),
					recurrence: 'Daily',
					custom_recurrence_repeat_every: null,
					custom_recurrence_repeat_unit: null,
					custom_recurrence_end_type: null,
					custom_recurrence_end_value: null
				}
			]
		};
	}

	// Create a new capacity
	function createDefaultCapacity(): ProviderCapacity {
		if (!$userAlias || !$userPub) throw new Error('No user logged in');

		const todayString = today(getLocalTimeZone()).toString();
		return {
			id: crypto.randomUUID(),
			name: '',
			emoji: '',
			unit: '',
			description: '',
			max_natural_div: 1,
			max_percentage_div: 1.0,
			hidden_until_request_accepted: false,
			owner_id: $userPub,
			filter_rule: null,
			availability_slots: [
				{
					id: `slot-${Date.now()}-${Math.random().toString(36).substr(2, 9)}`,
					quantity: 1,
					location_type: 'Undefined',
					all_day: true,
					start_date: todayString,
					start_time: null,
					end_date: null,
					end_time: null,
					time_zone: getLocalTimeZone(),
					recurrence: 'Daily',
					custom_recurrence_repeat_every: null,
					custom_recurrence_repeat_unit: null,
					custom_recurrence_end_type: null,
					custom_recurrence_end_value: null
				}
			]
		};
	}

	// Add a new capacity row
	function addCapacityRow() {
		if (!$userAlias || !$userPub) return;

		const newCapacity = createDefaultCapacity();

		// Use the addCapacity function to properly add the capacity
		const success = addCapacity(newCapacity);
		if (!success) {
			globalState.showToast('Failed to add capacity', 'error');
			return;
		}
	}

	// Add a random capacity with interesting verb + unit combination
	function addRandomCapacity() {
		if (!$userAlias || !$userPub) return;

		const newCapacity = generateRandomCapacity();

		// Use the addCapacity function to properly add the capacity
		const success = addCapacity(newCapacity);
		if (!success) {
			globalState.showToast('Failed to add random capacity', 'error');
			return;
		}

		globalState.showToast(
			`Generated "${newCapacity.name} ${newCapacity.unit}" capacity!`,
			'success'
		);
	}

	// Handle capacity update from child component
	function handleCapacityUpdate(capacity: ProviderCapacity) {
		const success = updateCapacity(capacity);
		if (!success) {
			globalState.showToast('Failed to update capacity', 'error');
		}
	}

	// Handle capacity delete from child component
	function handleCapacityDelete(id: string) {
		const success = deleteCapacity(id);
		if (!success) {
			globalState.showToast('Failed to delete capacity', 'error');
		}
	}
</script>

<div class="capacities-list grid grid-cols-1 gap-3 p-2 md:grid-cols-2 lg:grid-cols-3">
	{#each capacityEntries as entry (entry.id)}
		<div
			class="capacity-wrapper"
			class:newly-created-capacity={globalState.highlightedCapacities.has(entry.id)}
		>
			<Capacity
				capacity={entry as ProviderCapacity}
				canDelete={true}
				onupdate={handleCapacityUpdate}
				ondelete={handleCapacityDelete}
			/>
		</div>
	{/each}
	<div class="button-container my-2 flex justify-center gap-2">
		<button
			type="button"
			class="add-btn h-10 w-10"
			onclick={addCapacityRow}
			title="Add blank capacity">+</button
		>
		<button
			type="button"
			class="dice-btn h-10 w-10"
			onclick={addRandomCapacity}
			title="Generate random capacity">🎲</button
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
