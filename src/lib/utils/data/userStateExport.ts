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
	ContactsCollectionData,
	SlotSubscriptions,
	SlotFiltersCollection,
	UserMembershipLists,
	MembershipSubscriptions,
	Commitment
} from '$lib/protocol/schemas';
import { 
	myRecognitionTreeStore as userTree,
	myRecognitionWeights,
	myCapacitySlotsStore,
	myNeedSlotsStore,
	myCommitmentStore,
	setMyCapacitySlots,
	setMyNeedSlots
} from '$lib/protocol/stores.svelte';
import { userContacts } from '$lib/network/users.svelte';
import { slotSubscriptions, slotFilters } from '$lib/network/capacity-subscriptions.svelte';
import { myMembershipLists, myMembershipSubscriptions } from '$lib/network/membership.svelte';

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
 * V5: Comprehensive export including all persistent stores and configurations
 */
export interface UserStateExport {
	version: string;
	exported_at: string;
	data: {
		// Core data
		tree: RootNode | null;
		recognition_weights: ShareMap | null;  // V5: Computed from tree
		capacity_slots: any[] | null;  // V5: Slot-native
		need_slots: any[] | null;  // V5: Slot-native
		commitment: Commitment | null;  // V5: Full commitment with all metadata
		
		// Network configuration
		slot_subscriptions: SlotSubscriptions | null;  // Who you subscribe to for slots
		slot_filters: SlotFiltersCollection | null;  // Filters for auto-populating slots
		membership_lists: UserMembershipLists | null;  // Organization memberships you declare
		membership_subscriptions: MembershipSubscriptions | null;  // Membership lists you subscribe to
		
		// Social data
		contacts: ContactsCollectionData | null;
		
		// V5 TODO: Composition desires need to be redesigned for v5
		compose_from: UserSlotComposition | null;
		compose_into: UserSlotComposition | null;
	};
}

/**
 * Export all primordial user data to a JSON-serializable object
 * V5: Comprehensive export including all persistent stores and configurations
 */
export function exportUserState(): UserStateExport {
	console.log('[USER-STATE-EXPORT] Exporting complete user state (v5 - comprehensive)...');

	// Core data stores
	const tree = get(userTree);
	const recognitionWeights = get(myRecognitionWeights);  // Computed from tree
	const capacitySlots = get(myCapacitySlotsStore);
	const needSlots = get(myNeedSlotsStore);
	const commitment = get(myCommitmentStore);  // Full commitment with metadata
	
	// Network configuration stores
	const slotSubs = get(slotSubscriptions);
	const filters = get(slotFilters);
	const membershipLists = get(myMembershipLists);
	const membershipSubs = get(myMembershipSubscriptions);
	
	// Social data
	const contacts = get(userContacts);

	const exportData: UserStateExport = {
		version: '3.0.0',  // V5 comprehensive format
		exported_at: new Date().toISOString(),
		data: {
			// Core data
			tree,
			recognition_weights: recognitionWeights,
			capacity_slots: capacitySlots,
			need_slots: needSlots,
			commitment,
			
			// Network configuration
			slot_subscriptions: slotSubs,
			slot_filters: filters,
			membership_lists: membershipLists,
			membership_subscriptions: membershipSubs,
			
			// Social data
			contacts,
			
			// V5 TODO: Composition desires need to be redesigned
			compose_from: null,
			compose_into: null
		}
	};

	console.log('[USER-STATE-EXPORT] Export complete (v5 - comprehensive):', {
		hasTree: !!exportData.data.tree,
		hasRecognitionWeights: !!exportData.data.recognition_weights,
		hasCommitment: !!exportData.data.commitment,
		capacitySlotsCount: (exportData.data.capacity_slots || []).length,
		needSlotsCount: (exportData.data.need_slots || []).length,
		slotSubscriptionsCount: Object.keys(exportData.data.slot_subscriptions || {}).length,
		slotFiltersCount: Object.keys(exportData.data.slot_filters || {}).length,
		membershipListsCount: Object.keys(exportData.data.membership_lists || {}).length,
		membershipSubscriptionsCount: Object.keys(exportData.data.membership_subscriptions || {}).length,
		contactsCount: Object.keys(exportData.data.contacts || {}).length
	});

	return exportData;
}

/**
 * Validate imported user state data
 * V5: Updated to validate all comprehensive fields
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
	if (data.data.tree !== null && data.data.tree !== undefined) {
		if (typeof data.data.tree !== 'object') {
			errors.push('Invalid tree data structure');
		} else {
			// Basic tree validation - RootNode only needs id, name, children
			if (!data.data.tree.id || !data.data.tree.name) {
				errors.push('Tree missing required fields (id, name)');
			}
			if (!Array.isArray(data.data.tree.children)) {
				errors.push('Tree children must be an array');
			}
		}
	}

	// Validate slots (if present)
	if (data.data.capacity_slots !== null && data.data.capacity_slots !== undefined) {
		if (!Array.isArray(data.data.capacity_slots)) {
			errors.push('Capacity slots must be an array or null');
		}
	}

	if (data.data.need_slots !== null && data.data.need_slots !== undefined) {
		if (!Array.isArray(data.data.need_slots)) {
			errors.push('Need slots must be an array or null');
		}
	}

	// Validate commitment (if present)
	if (data.data.commitment !== null && data.data.commitment !== undefined) {
		if (typeof data.data.commitment !== 'object') {
			errors.push('Commitment must be an object or null');
		}
	}

	// Validate network configuration (if present)
	if (data.data.slot_subscriptions !== null && data.data.slot_subscriptions !== undefined) {
		if (typeof data.data.slot_subscriptions !== 'object') {
			errors.push('Slot subscriptions must be an object or null');
		}
	}

	if (data.data.slot_filters !== null && data.data.slot_filters !== undefined) {
		if (typeof data.data.slot_filters !== 'object') {
			errors.push('Slot filters must be an object or null');
		}
	}

	if (data.data.membership_lists !== null && data.data.membership_lists !== undefined) {
		if (typeof data.data.membership_lists !== 'object') {
			errors.push('Membership lists must be an object or null');
		}
	}

	if (data.data.membership_subscriptions !== null && data.data.membership_subscriptions !== undefined) {
		if (typeof data.data.membership_subscriptions !== 'object') {
			errors.push('Membership subscriptions must be an object or null');
		}
	}

	// Validate composition data (if present - legacy support)
	if (data.data.compose_from !== null && data.data.compose_from !== undefined) {
		if (typeof data.data.compose_from !== 'object') {
			errors.push('Compose-from must be an object or null');
		}
	}

	if (data.data.compose_into !== null && data.data.compose_into !== undefined) {
		if (typeof data.data.compose_into !== 'object') {
			errors.push('Compose-into must be an object or null');
		}
	}

	// Validate contacts (if present)
	if (data.data.contacts !== null && data.data.contacts !== undefined) {
		if (typeof data.data.contacts !== 'object') {
			errors.push('Contacts must be an object or null');
		}
	}

	return {
		valid: errors.length === 0,
		errors
	};
}

/**
 * Import user state from exported data
 * V5: Comprehensive import including all persistent stores and configurations
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
		skipCommitment?: boolean;
		skipSlotSubscriptions?: boolean;
		skipSlotFilters?: boolean;
		skipMembershipLists?: boolean;
		skipMembershipSubscriptions?: boolean;
		skipComposeFrom?: boolean;
		skipComposeInto?: boolean;
		skipContacts?: boolean;
	} = {}
): Promise<{ success: boolean; errors: string[] }> {
	console.log('[USER-STATE-IMPORT] Starting comprehensive user state import (v5)...');

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

		// V5: Import commitment (full metadata) if present
		if (!options.skipCommitment && importData.data.commitment) {
			console.log('[USER-STATE-IMPORT] Importing commitment...');
			myCommitmentStore.set(importData.data.commitment);
			console.log('[USER-STATE-IMPORT] ✓ Commitment imported (Holster auto-persisting)');
		} else {
			// Fall back to importing slots individually if no commitment
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
		}

		// Import network configuration
		if (!options.skipSlotSubscriptions && importData.data.slot_subscriptions) {
			console.log('[USER-STATE-IMPORT] Importing slot subscriptions...');
			slotSubscriptions.set(importData.data.slot_subscriptions);
			console.log('[USER-STATE-IMPORT] ✓ Slot subscriptions imported (Holster auto-persisting)');
		}

		if (!options.skipSlotFilters && importData.data.slot_filters) {
			console.log('[USER-STATE-IMPORT] Importing slot filters...');
			slotFilters.set(importData.data.slot_filters);
			console.log('[USER-STATE-IMPORT] ✓ Slot filters imported (Holster auto-persisting)');
		}

		if (!options.skipMembershipLists && importData.data.membership_lists) {
			console.log('[USER-STATE-IMPORT] Importing membership lists...');
			myMembershipLists.set(importData.data.membership_lists);
			console.log('[USER-STATE-IMPORT] ✓ Membership lists imported (Holster auto-persisting)');
		}

		if (!options.skipMembershipSubscriptions && importData.data.membership_subscriptions) {
			console.log('[USER-STATE-IMPORT] Importing membership subscriptions...');
			myMembershipSubscriptions.set(importData.data.membership_subscriptions);
			console.log('[USER-STATE-IMPORT] ✓ Membership subscriptions imported (Holster auto-persisting)');
		}

		// V5 TODO: Composition desires need to be redesigned
		// For now, skip these

		// Import contacts
		if (!options.skipContacts && importData.data.contacts) {
			console.log('[USER-STATE-IMPORT] Importing contacts...');
			userContacts.set(importData.data.contacts);
			console.log('[USER-STATE-IMPORT] ✓ Contacts imported');
		}

		console.log('[USER-STATE-IMPORT] ✓ All data imported successfully (v5 - comprehensive)');
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
 * V5: Comprehensive options to match v5 stores
 */
export async function importUserStateFromJSON(
	jsonString: string,
	options?: {
		skipTree?: boolean;
		skipRecognitionWeights?: boolean;
		skipCapacitySlots?: boolean;
		skipNeedSlots?: boolean;
		skipCommitment?: boolean;
		skipSlotSubscriptions?: boolean;
		skipSlotFilters?: boolean;
		skipMembershipLists?: boolean;
		skipMembershipSubscriptions?: boolean;
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
