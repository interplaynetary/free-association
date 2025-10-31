/**
 * User State Export/Import Utilities
 *
 * Handles exporting and importing primordial user data (non-derived state)
 * that can be backed up and restored.
 *
 * Primordial data includes:
 * - userTree: The hierarchical tree structure
 * - userSogf: Share-of-Gratitude-Flow (recognition/share map)
 * - userCapacities: Provider and recipient capacities
 * - userDesiredSlotComposeFrom: Composition desires (receiving from others)
 * - userDesiredSlotComposeInto: Composition desires (giving to others)
 * - userContacts: Contact management data
 */

import { get } from 'svelte/store';
// V5: Import from v5 schemas and stores
import type {
	RootNode,
	ShareMap,
	ContactsCollectionData
} from '$lib/protocol/schemas';
import { 
	myRecognitionTreeStore as userTree,
	myRecognitionWeights,
	myCapacitySlotsStore,
	myNeedSlotsStore,
	setMyCapacitySlots,
	setMyNeedSlots
} from '$lib/protocol/stores.svelte';
import { userContacts } from '$lib/network/users.svelte';

// V5 TODO: User slot composition types need to be defined in v5
// For now, we'll use simplified types
export interface UserSlotComposition {
	slotId: string;
	targetId: string;
	targetType: 'slot' | 'pubkey';
	quantity: number;
}

/**
 * Complete user state export format
 * V5: Simplified to reflect v5 architecture (Holster-based, slot-native)
 */
export interface UserStateExport {
	version: string;
	exported_at: string;
	data: {
		tree: RootNode | null;
		recognition_weights: ShareMap | null;  // V5: Computed from tree
		capacity_slots: any[] | null;  // V5: Slot-native
		need_slots: any[] | null;  // V5: Slot-native
		// V5 TODO: Composition desires need to be redesigned for v5
		compose_from: UserSlotComposition | null;
		compose_into: UserSlotComposition | null;
		contacts: ContactsCollectionData | null;
	};
}

/**
 * Export all primordial user data to a JSON-serializable object
 * V5: Updated to use v5 stores (Holster-based, slot-native)
 */
export function exportUserState(): UserStateExport {
	console.log('[USER-STATE-EXPORT] Exporting complete user state (v5)...');

	// V5: Get data from v5 stores
	const tree = get(userTree);
	const recognitionWeights = get(myRecognitionWeights);  // Computed from tree
	const capacitySlots = get(myCapacitySlotsStore);
	const needSlots = get(myNeedSlotsStore);
	const contacts = get(userContacts);

	const exportData: UserStateExport = {
		version: '2.0.0',  // V5 format
		exported_at: new Date().toISOString(),
		data: {
			tree,
			recognition_weights: recognitionWeights,
			capacity_slots: capacitySlots,
			need_slots: needSlots,
			// V5 TODO: Composition desires need to be redesigned
			compose_from: null,
			compose_into: null,
			contacts
		}
	};

	console.log('[USER-STATE-EXPORT] Export complete (v5):', {
		hasTree: !!exportData.data.tree,
		hasRecognitionWeights: !!exportData.data.recognition_weights,
		capacitySlotsCount: (exportData.data.capacity_slots || []).length,
		needSlotsCount: (exportData.data.need_slots || []).length,
		contactsCount: Object.keys(exportData.data.contacts || {}).length
	});

	return exportData;
}

/**
 * Validate imported user state data
 */
export function validateUserStateImport(data: any): { valid: boolean; errors: string[] } {
	const errors: string[] = [];

	// Check version
	if (!data.version) {
		errors.push('Missing version field');
	}

	// Check data structure
	if (!data.data || typeof data.data !== 'object') {
		errors.push('Invalid or missing data field');
		return { valid: false, errors };
	}

	// Validate tree structure (if present)
	if (data.data.tree !== null) {
		if (!data.data.tree || typeof data.data.tree !== 'object') {
			errors.push('Invalid tree data structure');
		} else {
			// Basic tree validation - RootNode only needs id, name, type, children, created_at, updated_at
			if (!data.data.tree.id || !data.data.tree.name) {
				errors.push('Tree missing required fields (id, name)');
			}
			if (!Array.isArray(data.data.tree.children)) {
				errors.push('Tree children must be an array');
			}
		}
	}

	// Validate SOGF (if present)
	if (data.data.sogf !== null && typeof data.data.sogf !== 'object') {
		errors.push('SOGF must be an object or null');
	}

	// Validate capacities (if present)
	if (data.data.capacities !== null && typeof data.data.capacities !== 'object') {
		errors.push('Capacities must be an object or null');
	}

	// Validate composition data (if present)
	if (data.data.compose_from !== null && typeof data.data.compose_from !== 'object') {
		errors.push('Compose-from must be an object or null');
	}

	if (data.data.compose_into !== null && typeof data.data.compose_into !== 'object') {
		errors.push('Compose-into must be an object or null');
	}

	// Validate contacts (if present)
	if (data.data.contacts !== null && typeof data.data.contacts !== 'object') {
		errors.push('Contacts must be an object or null');
	}

	return {
		valid: errors.length === 0,
		errors
	};
}

/**
 * Import user state from exported data
 * V5: Updated to use Holster stores (auto-persisting)
 * 
 * @param importData - The exported user state to import
 * @param options - Import options
 * @returns Success status and any errors
 */
export async function importUserState(
	importData: UserStateExport,
	options: {
		skipTree?: boolean;
		skipRecognitionWeights?: boolean;
		skipCapacitySlots?: boolean;
		skipNeedSlots?: boolean;
		skipComposeFrom?: boolean;
		skipComposeInto?: boolean;
		skipContacts?: boolean;
	} = {}
): Promise<{ success: boolean; errors: string[] }> {
	console.log('[USER-STATE-IMPORT] Starting user state import (v5)...');

	// Validate the import data
	const validation = validateUserStateImport(importData);
	if (!validation.valid) {
		console.error('[USER-STATE-IMPORT] Validation failed:', validation.errors);
		return { success: false, errors: validation.errors };
	}

	const errors: string[] = [];

	try {
		// V5: Import tree (Holster auto-persists)
		if (!options.skipTree && importData.data.tree) {
			console.log('[USER-STATE-IMPORT] Importing tree...');
			userTree.set(importData.data.tree);
			console.log('[USER-STATE-IMPORT] ✓ Tree imported (Holster auto-persisting)');
		}

		// V5: Recognition weights are computed from tree, no need to import

		// V5: Import capacity slots (Holster auto-persists)
		if (!options.skipCapacitySlots && importData.data.capacity_slots) {
			console.log('[USER-STATE-IMPORT] Importing capacity slots...');
			setMyCapacitySlots(importData.data.capacity_slots);
			console.log('[USER-STATE-IMPORT] ✓ Capacity slots imported (Holster auto-persisting)');
		}

		// V5: Import need slots (Holster auto-persists)
		if (!options.skipNeedSlots && importData.data.need_slots) {
			console.log('[USER-STATE-IMPORT] Importing need slots...');
			setMyNeedSlots(importData.data.need_slots);
			console.log('[USER-STATE-IMPORT] ✓ Need slots imported (Holster auto-persisting)');
		}

		// V5 TODO: Composition desires need to be redesigned
		// For now, skip these

		// Import contacts (still uses old persistence)
		if (!options.skipContacts && importData.data.contacts) {
			console.log('[USER-STATE-IMPORT] Importing contacts...');
			userContacts.set(importData.data.contacts);
			// Note: Contacts may need explicit persistence call if not using Holster
			console.log('[USER-STATE-IMPORT] ✓ Contacts imported');
		}

		console.log('[USER-STATE-IMPORT] ✓ All data imported successfully (v5)');
		return { success: true, errors: [] };
	} catch (error) {
		const errorMessage = error instanceof Error ? error.message : 'Unknown error during import';
		console.error('[USER-STATE-IMPORT] Import failed:', error);
		errors.push(errorMessage);
		return { success: false, errors };
	}
}

/**
 * Export user state as formatted JSON string
 */
export function exportUserStateAsJSON(pretty: boolean = true): string {
	const exportData = exportUserState();
	return JSON.stringify(exportData, null, pretty ? 2 : 0);
}

/**
 * Parse and import user state from JSON string
 * V5: Updated options to match v5 stores
 */
export async function importUserStateFromJSON(
	jsonString: string,
	options?: {
		skipTree?: boolean;
		skipRecognitionWeights?: boolean;
		skipCapacitySlots?: boolean;
		skipNeedSlots?: boolean;
		skipComposeFrom?: boolean;
		skipComposeInto?: boolean;
		skipContacts?: boolean;
	}
): Promise<{ success: boolean; errors: string[] }> {
	try {
		const parsedData = JSON.parse(jsonString);
		return await importUserState(parsedData, options);
	} catch (error) {
		const errorMessage = error instanceof Error ? error.message : 'Failed to parse JSON';
		console.error('[USER-STATE-IMPORT] JSON parsing failed:', error);
		return { success: false, errors: [errorMessage] };
	}
}
