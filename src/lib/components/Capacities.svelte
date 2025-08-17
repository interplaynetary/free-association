<script lang="ts">
	import { onMount } from 'svelte';
	import { globalState } from '$lib/global.svelte';
	import type { ProviderCapacity, CapacitiesCollection } from '$lib/schema';
	import {
		findNodeById,
		addCapacity as addCapacityToCollection,
		updateNodeById
	} from '$lib/protocol';
	import { Calendar, DatePicker, Button } from 'bits-ui';
	import { getLocalTimeZone, today } from '@internationalized/date';
	import { get } from 'svelte/store';
	import { userAlias, userPub } from '$lib/state/gun.svelte';
	import { userTree, userCapacities, userCapacitiesWithShares } from '$lib/state/core.svelte';
	import Capacity from './Capacity.svelte';
	import {
		userDesiredSlotClaims,
		userDesiredSlotComposeFrom,
		userDesiredSlotComposeInto
	} from '$lib/state/core.svelte';

	// Reactive derived values
	const capacityEntries = $derived(
		Object.entries($userCapacitiesWithShares || {})
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
		const newCapacities = structuredClone($userCapacities || {});

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

		// Set the store with the new value
		userCapacities.set(newCapacities);

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
			const newCapacities = structuredClone($userCapacities || {});

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

			// Set the store with the new value
			userCapacities.set(newCapacities);

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
		userDesiredSlotComposeFrom.update((current) => {
			const updated = { ...current };
			let changes = 0;

			// Remove entries where deleted capacity is the source
			if (updated[capacityId]) {
				changes += Object.keys(updated[capacityId]).length;
				delete updated[capacityId];
			}

			// Remove entries where deleted capacity is the target
			Object.keys(updated).forEach((sourceCapId) => {
				Object.keys(updated[sourceCapId]).forEach((sourceSlotId) => {
					if (updated[sourceCapId][sourceSlotId][capacityId]) {
						changes += Object.keys(updated[sourceCapId][sourceSlotId][capacityId]).length;
						delete updated[sourceCapId][sourceSlotId][capacityId];

						// Clean up empty objects
						if (Object.keys(updated[sourceCapId][sourceSlotId]).length === 0) {
							delete updated[sourceCapId][sourceSlotId];
						}
						if (Object.keys(updated[sourceCapId]).length === 0) {
							delete updated[sourceCapId];
						}
					}
				});
			});

			console.log(`[CLEANUP] Cleaned ${changes} userDesiredSlotComposeFrom entries`);
			return updated;
		});

		// 2. Clean up userDesiredSlotComposeInto (where deleted capacity is SOURCE or TARGET)
		userDesiredSlotComposeInto.update((current) => {
			const updated = { ...current };
			let changes = 0;

			// Remove entries where deleted capacity is the source
			if (updated[capacityId]) {
				changes += Object.keys(updated[capacityId]).length;
				delete updated[capacityId];
			}

			// Remove entries where deleted capacity is the target
			Object.keys(updated).forEach((sourceCapId) => {
				Object.keys(updated[sourceCapId]).forEach((sourceSlotId) => {
					if (updated[sourceCapId][sourceSlotId][capacityId]) {
						changes += Object.keys(updated[sourceCapId][sourceSlotId][capacityId]).length;
						delete updated[sourceCapId][sourceSlotId][capacityId];

						// Clean up empty objects
						if (Object.keys(updated[sourceCapId][sourceSlotId]).length === 0) {
							delete updated[sourceCapId][sourceSlotId];
						}
						if (Object.keys(updated[sourceCapId]).length === 0) {
							delete updated[sourceCapId];
						}
					}
				});
			});

			console.log(`[CLEANUP] Cleaned ${changes} userDesiredSlotComposeInto entries`);
			return updated;
		});

		// 3. Clean up userDesiredSlotClaims (remove all slot claims for deleted capacity)
		userDesiredSlotClaims.update((current) => {
			const updated = { ...current };
			let changes = 0;

			if (updated[capacityId]) {
				changes = Object.keys(updated[capacityId]).length;
				delete updated[capacityId];
			}

			console.log(`[CLEANUP] Cleaned ${changes} userDesiredSlotClaims entries`);
			return updated;
		});

		console.log(`✅ [CLEANUP] Completed comprehensive cleanup for capacity: ${capacityId}`);
	}

	// Delete capacity from the userCapacities store with comprehensive cleanup
	function deleteCapacity(capacityId: string) {
		try {
			if (!$userAlias || !$userPub) return false;

			// STEP 1: Clean up all slot composition data BEFORE deleting the capacity
			cleanupCapacitySlotData(capacityId);

			// STEP 2: Remove the capacity itself
			userCapacities.update((caps) => {
				const newCaps = { ...(caps || {}) };
				if (newCaps[capacityId]) {
					delete newCaps[capacityId];
				}
				return newCaps;
			});

			globalState.showToast('Capacity and all related slot data deleted', 'success');
			return true;
		} catch (error) {
			console.error('Error deleting capacity:', error);
			globalState.showToast('Error deleting capacity', 'error');
			return false;
		}
	}

	// Create a new capacity
	function createDefaultCapacity(): ProviderCapacity {
		if (!$userAlias || !$userPub) throw new Error('No user logged in');

		const now = new Date().toISOString();
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
			recipient_shares: {},
			availability_slots: [
				{
					id: `slot-${Date.now()}-${Math.random().toString(36).substr(2, 9)}`,
					quantity: 1,
					location_type: 'Undefined',
					all_day: false,
					start_date: now,
					start_time: now,
					end_date: now,
					end_time: now,
					time_zone: getLocalTimeZone(),
					recurrence: 'Does not repeat',
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
		<Capacity
			capacity={entry as ProviderCapacity}
			canDelete={true}
			onupdate={handleCapacityUpdate}
			ondelete={handleCapacityDelete}
		/>
	{/each}
	<button type="button" class="add-btn mx-auto my-2 h-10 w-10" onclick={addCapacityRow}>+</button>
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
</style>
