/**
 * Organizations Module
 * 
 * Provides organization management with multi-language names and recursive membership.
 * Uses createStore() from store.svelte.ts for Holster persistence and sync.
 * 
 * Features:
 * - Multi-language names: { en: "Name", es: "Nombre" }
 * - Global organizations list (freely-associating-organizations)
 * - User's organization collection (Holster-backed via createStore)
 * - CRUD operations with validation
 * - Auto-cleanup on logout
 */

import { writable, derived, get } from 'svelte/store';
import type { Writable } from 'svelte/store';
import { createStore } from '$lib/utils/primitives/store.svelte';
import { holster } from '$lib/network/holster';
import type {
	Organization,
	OrganizationsCollection
} from '@playnet/free-association/schemas';
import { OrganizationSchema, OrganizationsCollectionSchema } from '@playnet/free-association/schemas';
import { DEMO_ORGANIZATIONS } from '$lib/config/org-trees';

// ═══════════════════════════════════════════════════════════════════
// USER'S ORGANIZATIONS STORE (Holster-backed via createStore)
// ═══════════════════════════════════════════════════════════════════

/**
 * User's organizations collection
 * Uses createStore() - handles persistence, validation, conflict resolution automatically!
 */
export const holsterOrganizations = createStore({
	holsterPath: 'organizations',
	schema: OrganizationsCollectionSchema,
	persistDebounce: 200
});

export const isLoadingOrganizations = writable(false);
export const organizationSearchQuery = writable('');

// ═══════════════════════════════════════════════════════════════════
// GLOBAL ORGANIZATIONS LIST (Manual Subscription - like users list)
// ═══════════════════════════════════════════════════════════════════

/**
 * Global list of all organizations registered across the network
 * Maps org_id -> Organization metadata (names, description, emoji)
 */
export const globalOrganizations = writable<Record<string, Organization>>(DEMO_ORGANIZATIONS);

let orgListCallback: ((data: any) => void) | null = null;
let isOrgListInitialized = false;

/**
 * Subscribe to freely-associating-organizations list from Holster
 */
function subscribeToOrganizationsList() {
	if (isOrgListInitialized) {
		console.log('[ORGS-LIST] Already subscribed');
		return;
	}

	orgListCallback = (data: any) => {
		if (!data) return;

		// Filter out metadata fields and deleted entries
		const orgsData: Record<string, Organization> = {};
		for (const [key, value] of Object.entries(data)) {
			if (value && typeof value === 'object' && !key.startsWith('_')) {
				try {
					// Validate each organization entry
					const validation = OrganizationSchema.safeParse(value);
					if (validation.success) {
						orgsData[key] = validation.data;
					}
				} catch (error) {
					console.warn(`[ORGS-LIST] Invalid org data for ${key}:`, error);
				}
			}
		}

		// Update store
		globalOrganizations.set(orgsData);

		console.log('[ORGS-LIST] Updated:', {
			count: Object.keys(orgsData).length
		});
	};

	holster.get('freely-associating-organizations').on(orgListCallback, true);
	isOrgListInitialized = true;
}

/**
 * Initialize global organizations list subscription
 */
export function initializeOrganizationsList() {
	console.log('[ORGS-LIST] Initializing...');
	subscribeToOrganizationsList();
}

/**
 * Cleanup organizations list subscription
 */
function cleanupOrganizationsList() {
	if (orgListCallback) {
		holster.get('freely-associating-organizations').off(orgListCallback);
		orgListCallback = null;
	}
	globalOrganizations.set({});
	isOrgListInitialized = false;
	console.log('[ORGS-LIST] Cleaned up');
}

// ═══════════════════════════════════════════════════════════════════
// DERIVED STORES
// ═══════════════════════════════════════════════════════════════════

/**
 * User's organizations as array (for UI rendering)
 */
export const organizationsArray = derived(holsterOrganizations, ($orgs) => {
	if (!$orgs) return [];
	return Object.values($orgs);
});

/**
 * Filtered organizations (by search query)
 */
export const filteredOrganizations = derived(
	[organizationsArray, organizationSearchQuery],
	([$orgsArray, $searchQuery]) => {
		if (!$searchQuery.trim()) {
			return $orgsArray;
		}

		const query = $searchQuery.toLowerCase();
		return $orgsArray.filter((org) => {
			// Search in all language names
			const nameMatch = org.names ? Object.values(org.names).some((name) =>
				name.toLowerCase().includes(query)
			) : false;
			const descMatch = org.description?.toLowerCase().includes(query);
			const idMatch = org.org_id?.toLowerCase().includes(query);

			return nameMatch || descMatch || idMatch;
		});
	}
);

// ═══════════════════════════════════════════════════════════════════
// LIFECYCLE FUNCTIONS
// ═══════════════════════════════════════════════════════════════════

/**
 * Initialize user's organizations
 * Call this when user logs in
 */
export function initializeOrganizations() {
	holsterOrganizations.initialize();
	console.log('[ORGS] Initialized user organizations store');
}

/**
 * Cleanup organizations
 * Call this when user logs out
 */
export async function cleanupOrganizations() {
	await holsterOrganizations.cleanup();
	cleanupOrganizationsList();
	console.log('[ORGS] Cleaned up');
}

// ═══════════════════════════════════════════════════════════════════
// CRUD OPERATIONS
// ═══════════════════════════════════════════════════════════════════

/**
 * Create a new organization
 */
export function createOrganization(
	orgData: Omit<Organization, 'org_id' | 'created_at' | 'updated_at'>
): Organization {
	const now = Date.now();
	const org_id = `org_${now}_${Math.random().toString(36).substr(2, 9)}`;

	const newOrg: Organization = {
		org_id,
		names: orgData.names,
		emoji: orgData.emoji,
		description: orgData.description,
		created_at: now,
		updated_at: now
	};

	// Validate the organization data
	const validatedOrg = OrganizationSchema.parse(newOrg);

	// Add to organizations collection
	const currentOrgs = get(holsterOrganizations) || {};
	const updatedOrgs = {
		...currentOrgs,
		[org_id]: validatedOrg
	};

	// Update store - createStore handles persistence automatically!
	holsterOrganizations.set(updatedOrgs);

	console.log(`[ORGS] Created organization: ${org_id}`);
	return validatedOrg;
}

/**
 * Update an existing organization
 */
export function updateOrganization(org_id: string, updates: Partial<Organization>): void {
	const currentOrgs = get(holsterOrganizations);
	if (!currentOrgs) {
		console.warn(`[ORGS] No organizations loaded`);
		return;
	}

	const existingOrg = currentOrgs[org_id];
	if (!existingOrg) {
		console.warn(`[ORGS] Organization with ID ${org_id} not found`);
		return;
	}

	const updatedOrg = {
		...existingOrg,
		...updates,
		org_id, // Never allow changing org_id
		updated_at: Date.now()
	};

	// Validate the updated organization
	const validatedOrg = OrganizationSchema.parse(updatedOrg);

	// Update organizations collection
	const updatedOrgs = {
		...currentOrgs,
		[org_id]: validatedOrg
	};

	// Update store - createStore handles persistence automatically!
	holsterOrganizations.set(updatedOrgs);

	console.log(`[ORGS] Updated organization: ${org_id}`);
}

/**
 * Delete an organization
 */
export function deleteOrganization(org_id: string): void {
	const currentOrgs = get(holsterOrganizations);
	if (!currentOrgs) {
		console.warn(`[ORGS] No organizations loaded`);
		return;
	}

	// Remove from collection
	const { [org_id]: deleted, ...remaining } = currentOrgs;

	// Update store - createStore handles persistence automatically!
	holsterOrganizations.set(remaining);

	console.log(`[ORGS] Deleted organization: ${org_id}`);
}

/**
 * Get organization by ID
 */
export function getOrganizationById(org_id: string): Organization | undefined {
	const orgs = get(holsterOrganizations);
	return orgs?.[org_id];
}

/**
 * Get organization name in preferred language (with fallback)
 */
export function getOrganizationName(
	org: Organization,
	preferredLang: string = 'en'
): string {
	// Try preferred language
	if (org.names?.[preferredLang]) {
		return org.names[preferredLang];
	}

	// Try English fallback
	if (org.names?.['en']) {
		return org.names['en'];
	}

	// Return first available name
	const firstLang = org.names ? Object.keys(org.names)[0] : undefined;
	if (firstLang && org.names) {
		return org.names[firstLang];
	}

	// Last resort: return org_id
	return org.org_id || 'unknown';
}

/**
 * Register organization in global list
 * This publishes the organization to freely-associating-organizations
 * so others can discover and reference it
 */
export function registerOrganizationGlobally(org_id: string): void {
	const org = getOrganizationById(org_id);
	if (!org) {
		console.warn(`[ORGS] Cannot register: organization ${org_id} not found`);
		return;
	}

	// Publish to global list
	holster.get('freely-associating-organizations').next(org_id).put({
		org_id: org.org_id,
		names: org.names,
		emoji: org.emoji,
		description: org.description,
		created_at: org.created_at,
		updated_at: org.updated_at
	});

	console.log(`[ORGS] Registered organization globally: ${org_id}`);
}

/**
 * Unregister organization from global list
 */
export function unregisterOrganizationGlobally(org_id: string): void {
	// Remove from global list by setting to null
	holster.get('freely-associating-organizations').next(org_id).put(null);
	console.log(`[ORGS] Unregistered organization globally: ${org_id}`);
}

