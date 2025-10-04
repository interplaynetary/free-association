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
import type {
	RootNode,
	ShareMap,
	CapacitiesCollection,
	UserSlotComposition,
	ContactsCollectionData
} from '$lib/schema';
import {
	userTree,
	userSogf,
	userCapacities,
	userDesiredSlotComposeFrom,
	userDesiredSlotComposeInto
} from '$lib/state/core.svelte';
import { userContacts } from '$lib/state/users.svelte';
import {
	persistTree,
	persistSogf,
	persistCapacities,
	persistUserDesiredSlotComposeFrom,
	persistUserDesiredSlotComposeInto,
	persistContacts
} from '$lib/state/persistence.svelte';

/**
 * Complete user state export format
 */
export interface UserStateExport {
	version: string;
	exported_at: string;
	data: {
		tree: RootNode | null;
		sogf: ShareMap | null;
		capacities: CapacitiesCollection | null;
		compose_from: UserSlotComposition | null;
		compose_into: UserSlotComposition | null;
		contacts: ContactsCollectionData | null;
	};
}

/**
 * Export all primordial user data to a JSON-serializable object
 */
export function exportUserState(): UserStateExport {
	console.log('[USER-STATE-EXPORT] Exporting complete user state...');

	const exportData: UserStateExport = {
		version: '1.0.0',
		exported_at: new Date().toISOString(),
		data: {
			tree: get(userTree),
			sogf: get(userSogf),
			capacities: get(userCapacities),
			compose_from: get(userDesiredSlotComposeFrom),
			compose_into: get(userDesiredSlotComposeInto),
			contacts: get(userContacts)
		}
	};

	console.log('[USER-STATE-EXPORT] Export complete:', {
		hasTree: !!exportData.data.tree,
		hasSogf: !!exportData.data.sogf,
		capacitiesCount: Object.keys(exportData.data.capacities || {}).length,
		composeFromCount: Object.keys(exportData.data.compose_from || {}).length,
		composeIntoCount: Object.keys(exportData.data.compose_into || {}).length,
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
			// Basic tree validation
			if (!data.data.tree.id || !data.data.tree.name || !data.data.tree.user_id) {
				errors.push('Tree missing required fields (id, name, user_id)');
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
 * Import user state from exported data and persist to Gun
 * @param importData - The exported user state to import
 * @param options - Import options
 * @returns Success status and any errors
 */
export async function importUserState(
	importData: UserStateExport,
	options: {
		skipTree?: boolean;
		skipSogf?: boolean;
		skipCapacities?: boolean;
		skipComposeFrom?: boolean;
		skipComposeInto?: boolean;
		skipContacts?: boolean;
	} = {}
): Promise<{ success: boolean; errors: string[] }> {
	console.log('[USER-STATE-IMPORT] Starting user state import...');

	// Validate the import data
	const validation = validateUserStateImport(importData);
	if (!validation.valid) {
		console.error('[USER-STATE-IMPORT] Validation failed:', validation.errors);
		return { success: false, errors: validation.errors };
	}

	const errors: string[] = [];

	try {
		// Import tree
		if (!options.skipTree && importData.data.tree) {
			console.log('[USER-STATE-IMPORT] Importing tree...');
			userTree.set(importData.data.tree);
			await persistTree();
			console.log('[USER-STATE-IMPORT] ✓ Tree imported and persisted');
		}

		// Import SOGF
		if (!options.skipSogf && importData.data.sogf) {
			console.log('[USER-STATE-IMPORT] Importing SOGF...');
			userSogf.set(importData.data.sogf);
			await persistSogf();
			console.log('[USER-STATE-IMPORT] ✓ SOGF imported and persisted');
		}

		// Import capacities
		if (!options.skipCapacities && importData.data.capacities) {
			console.log('[USER-STATE-IMPORT] Importing capacities...');
			userCapacities.set(importData.data.capacities);
			await persistCapacities();
			console.log('[USER-STATE-IMPORT] ✓ Capacities imported and persisted');
		}

		// Import compose-from
		if (!options.skipComposeFrom && importData.data.compose_from) {
			console.log('[USER-STATE-IMPORT] Importing compose-from...');
			userDesiredSlotComposeFrom.set(importData.data.compose_from);
			await persistUserDesiredSlotComposeFrom();
			console.log('[USER-STATE-IMPORT] ✓ Compose-from imported and persisted');
		}

		// Import compose-into
		if (!options.skipComposeInto && importData.data.compose_into) {
			console.log('[USER-STATE-IMPORT] Importing compose-into...');
			userDesiredSlotComposeInto.set(importData.data.compose_into);
			await persistUserDesiredSlotComposeInto();
			console.log('[USER-STATE-IMPORT] ✓ Compose-into imported and persisted');
		}

		// Import contacts
		if (!options.skipContacts && importData.data.contacts) {
			console.log('[USER-STATE-IMPORT] Importing contacts...');
			userContacts.set(importData.data.contacts);
			await persistContacts();
			console.log('[USER-STATE-IMPORT] ✓ Contacts imported and persisted');
		}

		console.log('[USER-STATE-IMPORT] ✓ All data imported successfully');
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
 */
export async function importUserStateFromJSON(
	jsonString: string,
	options?: {
		skipTree?: boolean;
		skipSogf?: boolean;
		skipCapacities?: boolean;
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
