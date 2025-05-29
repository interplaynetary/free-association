<script lang="ts">
	import { onMount } from 'svelte';
	import { globalState } from '$lib/global.svelte';
	import type { Capacity, CapacityShare, Node, RootNode, CapacitiesCollection } from '$lib/schema';
	import {
		findNodeById,
		addCapacity as addCapacityToCollection,
		updateNodeById
	} from '$lib/protocol';
	import { Calendar, DatePicker, Button } from 'bits-ui';
	import { getLocalTimeZone, today } from '@internationalized/date';
	import { get } from 'svelte/store';
	import { username, userpub, userTree, userCapacities, persist } from '$lib/state.svelte';
	import CapacityComponent from './Capacity.svelte';

	// Reactive derived values
	const capacityEntries = $derived(Object.values($userCapacities || {}));

	// Add a new capacity to the userCapacities store
	function addCapacity(capacity: Capacity) {
		if (!$username || !$userpub) return false;

		// Create a deep clone of current capacities
		const newCapacities = structuredClone($userCapacities || {});

		// Create a plain object copy of the capacity
		const plainCapacity = { ...capacity };

		// Add capacity to the collection
		addCapacityToCollection(newCapacities, plainCapacity);

		// Set the store with the new value
		userCapacities.set(newCapacities);

		globalState.showToast(`Capacity "${capacity.name}" created`, 'success');
		return true;
	}

	// Update capacity in the userCapacities store
	function updateCapacity(capacity: Capacity) {
		try {
			if (!$username || !$userpub) return false;

			// Create a deep clone of current capacities
			const newCapacities = structuredClone($userCapacities || {});

			// Create a deep copy of the capacity to ensure store updates
			const plainCapacity = structuredClone(capacity);

			// Add capacity to the collection
			newCapacities[plainCapacity.id] = plainCapacity;

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

	// Delete capacity from the userCapacities store
	function deleteCapacity(capacityId: string) {
		try {
			if (!$username || !$userpub) return false;

			// Update the store directly using the update method
			userCapacities.update((caps) => {
				const newCaps = { ...(caps || {}) };
				if (newCaps[capacityId]) {
					delete newCaps[capacityId];
				}
				return newCaps;
			});

			globalState.showToast('Capacity deleted', 'success');
			return true;
		} catch (error) {
			console.error('Error deleting capacity:', error);
			globalState.showToast('Error deleting capacity', 'error');
			return false;
		}
	}

	// Create a new capacity
	function createDefaultCapacity(): Capacity {
		if (!$username || !$userpub) throw new Error('No user logged in');

		const now = new Date().toISOString();
		return {
			id: crypto.randomUUID(),
			name: '',
			quantity: 0,
			unit: '',
			location_type: 'Undefined',
			all_day: false,
			start_date: now,
			start_time: now,
			end_date: now,
			end_time: now,
			time_zone: getLocalTimeZone(),
			max_natural_div: 1,
			max_percentage_div: 1.0,
			hidden_until_request_accepted: false,
			owner_id: $userpub,
			shares: [],
			recurrence: null,
			custom_recurrence_repeat_every: null,
			custom_recurrence_repeat_unit: null,
			custom_recurrence_end_type: null,
			custom_recurrence_end_value: null,
			filter_rule: null,
			recipient_shares: null
		};
	}

	// Add a new capacity row
	function addCapacityRow() {
		if (!$username || !$userpub) return;

		const newCapacity = createDefaultCapacity();

		// Use the addCapacity function to properly add the capacity
		const success = addCapacity(newCapacity);
		if (!success) {
			globalState.showToast('Failed to add capacity', 'error');
			return;
		}
	}

	// Handle capacity update from child component
	function handleCapacityUpdate(event: CustomEvent<Capacity>) {
		const success = updateCapacity(event.detail);
		if (!success) {
			globalState.showToast('Failed to update capacity', 'error');
		}
	}

	// Handle capacity delete from child component
	function handleCapacityDelete(event: CustomEvent<string>) {
		const success = deleteCapacity(event.detail);
		if (!success) {
			globalState.showToast('Failed to delete capacity', 'error');
		}
	}
</script>

<div class="inventory-list grid grid-cols-1 gap-3 p-2 md:grid-cols-2 lg:grid-cols-3">
	{#each capacityEntries as entry (entry.id)}
		<CapacityComponent
			capacity={entry}
			canDelete={true}
			on:update={handleCapacityUpdate}
			on:delete={handleCapacityDelete}
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
