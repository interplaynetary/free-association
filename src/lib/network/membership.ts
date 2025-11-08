/**
 * Membership Module - Pure Functions
 * 
 * Pure computational functions for organization membership management.
 * These functions operate on plain data structures without Svelte dependencies.
 * 
 * For Svelte store integration, see membership.svelte.ts
 * 
 * Architecture:
 * - Declared membership lists (what someone declares the membership to be)
 * - Subscription mappings (who defers to whom for org membership)
 * - Membership cache (cached membership from network)
 * - Resolution logic (combine sources to get final membership)
 */

import type {
	UserMembershipLists,
	MembershipSubscriptions
} from '$lib/protocol/schemas';

// ═══════════════════════════════════════════════════════════════════
// TYPE DEFINITIONS
// ═══════════════════════════════════════════════════════════════════

/**
 * Membership cache structure
 * Maps: source_pubkey → org_id → members[]
 */
export type MembershipCache = Record<string, Record<string, string[]>>;

// ═══════════════════════════════════════════════════════════════════
// DECLARED MEMBERSHIP OPERATIONS (Pure Functions)
// ═══════════════════════════════════════════════════════════════════

/**
 * Set membership list for an organization
 * Returns a new lists object with the updated membership
 */
export function setMembershipListPure(
	currentLists: UserMembershipLists | null | undefined,
	org_id: string,
	members: string[]
): UserMembershipLists {
	return {
		...(currentLists || {}),
		[org_id]: members
	};
}

/**
 * Remove declared membership list for an organization
 * Returns a new lists object without the specified org
 */
export function removeMembershipListPure(
	currentLists: UserMembershipLists | null | undefined,
	org_id: string
): UserMembershipLists {
	if (!currentLists) return {};
	
	const { [org_id]: removed, ...remaining } = currentLists;
	return remaining;
}

/**
 * Add member to an organization's membership list
 * Returns a new lists object with the member added (avoids duplicates)
 */
export function addMemberToListPure(
	currentLists: UserMembershipLists | null | undefined,
	org_id: string,
	member_id: string
): { updated: UserMembershipLists; wasAdded: boolean } {
	const lists = currentLists || {};
	const currentMembers = lists[org_id] || [];
	
	// Avoid duplicates
	if (currentMembers.includes(member_id)) {
		return { updated: lists, wasAdded: false };
	}
	
	const updatedMembers = [...currentMembers, member_id];
	const updatedLists = {
		...lists,
		[org_id]: updatedMembers
	};
	
	return { updated: updatedLists, wasAdded: true };
}

/**
 * Remove member from an organization's membership list
 * Returns a new lists object with the member removed
 */
export function removeMemberFromListPure(
	currentLists: UserMembershipLists | null | undefined,
	org_id: string,
	member_id: string
): { updated: UserMembershipLists; wasRemoved: boolean } {
	if (!currentLists || !currentLists[org_id]) {
		return { updated: currentLists || {}, wasRemoved: false };
	}
	
	const currentMembers = currentLists[org_id];
	const wasPresent = currentMembers.includes(member_id);
	
	if (!wasPresent) {
		return { updated: currentLists, wasRemoved: false };
	}
	
	const updatedMembers = currentMembers.filter((id) => id !== member_id);
	const updatedLists = {
		...currentLists,
		[org_id]: updatedMembers
	};
	
	return { updated: updatedLists, wasRemoved: true };
}

// ═══════════════════════════════════════════════════════════════════
// SUBSCRIPTION OPERATIONS (Pure Functions)
// ═══════════════════════════════════════════════════════════════════

/**
 * Subscribe to someone else's membership list for an organization
 * Returns a new subscriptions object with the subscription added
 */
export function subscribeMembershipListPure(
	currentSubs: MembershipSubscriptions | null | undefined,
	org_id: string,
	source_pubkey: string
): MembershipSubscriptions {
	return {
		...(currentSubs || {}),
		[org_id]: source_pubkey
	};
}

/**
 * Unsubscribe from a membership list
 * Returns new subscriptions and cache objects with subscription removed
 */
export function unsubscribeMembershipListPure(
	currentSubs: MembershipSubscriptions | null | undefined,
	currentCache: MembershipCache,
	org_id: string
): {
	subscriptions: MembershipSubscriptions;
	cache: MembershipCache;
	removedSource: string | undefined;
} {
	if (!currentSubs) {
		return {
			subscriptions: {},
			cache: currentCache,
			removedSource: undefined
		};
	}
	
	const { [org_id]: removedSource, ...remainingSubs } = currentSubs;
	
	// Also clear from cache
	let updatedCache = currentCache;
	if (removedSource && currentCache[removedSource]) {
		const { [org_id]: removedOrg, ...remainingOrgs } = currentCache[removedSource];
		updatedCache = {
			...currentCache,
			[removedSource]: remainingOrgs
		};
	}
	
	return {
		subscriptions: remainingSubs,
		cache: updatedCache,
		removedSource
	};
}

// ═══════════════════════════════════════════════════════════════════
// CACHE OPERATIONS (Pure Functions)
// ═══════════════════════════════════════════════════════════════════

/**
 * Update membership cache with data from a source user
 * Returns a new cache object with the updated data
 */
export function updateMembershipCachePure(
	currentCache: MembershipCache,
	source_pubkey: string,
	org_id: string,
	members: string[]
): MembershipCache {
	return {
		...currentCache,
		[source_pubkey]: {
			...(currentCache[source_pubkey] || {}),
			[org_id]: members
		}
	};
}

/**
 * Remove cached membership for a source/org combination
 * Returns a new cache object with the data removed
 */
export function removeMembershipCachePure(
	currentCache: MembershipCache,
	source_pubkey: string,
	org_id?: string
): MembershipCache {
	if (!currentCache[source_pubkey]) {
		return currentCache;
	}
	
	if (org_id) {
		// Remove specific org
		const { [org_id]: removed, ...remaining } = currentCache[source_pubkey];
		return {
			...currentCache,
			[source_pubkey]: remaining
		};
	} else {
		// Remove entire source
		const { [source_pubkey]: removed, ...remaining } = currentCache;
		return remaining;
	}
}

// ═══════════════════════════════════════════════════════════════════
// MEMBERSHIP RESOLUTION (Pure Functions)
// ═══════════════════════════════════════════════════════════════════

/**
 * Get membership list for an organization (declared or via subscription)
 * 
 * Resolution order:
 * 1. If there's a declared list → return it
 * 2. If there's a subscription → return cached list from subscribed source
 * 3. Otherwise → return undefined
 * 
 * @param declared - Declared membership lists
 * @param subscriptions - Subscription mappings
 * @param cache - Cached membership from network
 * @param org_id - Organization ID to resolve
 * @returns Array of member IDs or undefined
 */
export function resolveMembershipList(
	declared: UserMembershipLists | null | undefined,
	subscriptions: MembershipSubscriptions | null | undefined,
	cache: MembershipCache,
	org_id: string
): string[] | undefined {
	// Check declared lists first
	if (declared && declared[org_id]) {
		return declared[org_id];
	}
	
	// Check subscription + cache
	if (subscriptions && subscriptions[org_id]) {
		const source = subscriptions[org_id];
		if (cache[source] && cache[source][org_id]) {
			return cache[source][org_id];
		}
	}
	
	return undefined;
}

/**
 * Check if we have membership data for an organization
 */
export function hasMembershipData(
	declared: UserMembershipLists | null | undefined,
	subscriptions: MembershipSubscriptions | null | undefined,
	cache: MembershipCache,
	org_id: string
): boolean {
	return resolveMembershipList(declared, subscriptions, cache, org_id) !== undefined;
}

/**
 * Get all organization IDs that we have membership data for
 */
export function getAllKnownOrganizations(
	declared: UserMembershipLists | null | undefined,
	subscriptions: MembershipSubscriptions | null | undefined,
	cache: MembershipCache
): string[] {
	const orgIds = new Set<string>();
	
	// Add from declared lists
	if (declared) {
		Object.keys(declared).forEach(id => orgIds.add(id));
	}
	
	// Add from subscriptions (if cached)
	if (subscriptions) {
		Object.entries(subscriptions).forEach(([org_id, source]) => {
			if (cache[source] && cache[source][org_id]) {
				orgIds.add(org_id);
			}
		});
	}
	
	return Array.from(orgIds);
}

/**
 * Get source of membership data for an organization
 * Returns 'declared' if we declared it, or the source pubkey if subscribed
 */
export function getMembershipSource(
	declared: UserMembershipLists | null | undefined,
	subscriptions: MembershipSubscriptions | null | undefined,
	org_id: string
): 'declared' | string | undefined {
	// Check declared first
	if (declared && declared[org_id]) {
		return 'declared';
	}
	
	// Check subscription
	if (subscriptions && subscriptions[org_id]) {
		return subscriptions[org_id];
	}
	
	return undefined;
}

// ═══════════════════════════════════════════════════════════════════
// BULK OPERATIONS (Pure Functions)
// ═══════════════════════════════════════════════════════════════════

/**
 * Merge multiple membership lists together
 * Useful for combining data from different sources
 */
export function mergeMembershipLists(
	lists: UserMembershipLists[]
): UserMembershipLists {
	const merged: UserMembershipLists = {};
	
	for (const list of lists) {
		for (const [org_id, members] of Object.entries(list)) {
			if (!merged[org_id]) {
				merged[org_id] = [...members];
			} else {
				// Merge and deduplicate
				const existing = new Set(merged[org_id]);
				for (const member of members) {
					existing.add(member);
				}
				merged[org_id] = Array.from(existing);
			}
		}
	}
	
	return merged;
}

/**
 * Get difference between two membership lists for an organization
 * Returns members that were added and removed
 */
export function getMembershipDiff(
	oldList: string[] | undefined,
	newList: string[] | undefined
): {
	added: string[];
	removed: string[];
	unchanged: string[];
} {
	const oldSet = new Set(oldList || []);
	const newSet = new Set(newList || []);
	
	const added = Array.from(newSet).filter(id => !oldSet.has(id));
	const removed = Array.from(oldSet).filter(id => !newSet.has(id));
	const unchanged = Array.from(newSet).filter(id => oldSet.has(id));
	
	return { added, removed, unchanged };
}

/**
 * Validate membership list structure
 * Returns validation errors or empty array if valid
 */
export function validateMembershipList(
	org_id: string,
	members: string[]
): string[] {
	const errors: string[] = [];
	
	if (!org_id || org_id.trim() === '') {
		errors.push('Organization ID cannot be empty');
	}
	
	if (!Array.isArray(members)) {
		errors.push('Members must be an array');
		return errors;
	}
	
	if (members.length === 0) {
		errors.push('Membership list cannot be empty');
	}
	
	// Check for duplicates
	const seen = new Set<string>();
	for (const member of members) {
		if (!member || member.trim() === '') {
			errors.push('Member ID cannot be empty');
		}
		if (seen.has(member)) {
			errors.push(`Duplicate member ID: ${member}`);
		}
		seen.add(member);
	}
	
	return errors;
}

