/**
 * Membership Module - Svelte Store Integration
 * 
 * Manages organization membership lists with local-first caching pattern.
 * Uses createStore() from store.svelte.ts for Holster persistence and sync.
 * 
 * Architecture:
 * - myMembershipLists: Declared membership lists (what YOU say the membership is)
 * - myMembershipSubscriptions: Subscription mappings (who you defer to for org membership)
 * - membershipCache: Cached membership from network (local-first, trust until proven otherwise)
 * 
 * Pattern:
 * Similar to recognition weights in stores.svelte.ts:
 * 1. Source: Your declared lists or subscription mappings (persistent)
 * 2. Cache: Others' membership lists from network (cached, updated when proven otherwise)
 * 3. Resolution: Combine source + cache to get final membership
 * 
 * NOTE: Pure functions are in membership.ts - this file wraps them with Svelte stores
 */

import { writable, get } from 'svelte/store';
import type { Writable } from 'svelte/store';
import { createStore } from '$lib/utils/primitives/store.svelte';
import type {
	UserMembershipLists,
	MembershipSubscriptions
} from '$lib/protocol/schemas';
import { UserMembershipListsSchema, MembershipSubscriptionsSchema } from '$lib/protocol/schemas';
import type { MembershipCache } from '$lib/network/membership';
import {
	setMembershipListPure,
	removeMembershipListPure,
	addMemberToListPure,
	removeMemberFromListPure,
	subscribeMembershipListPure,
	unsubscribeMembershipListPure,
	updateMembershipCachePure,
	resolveMembershipList
} from '$lib/network/membership';

// Re-export pure functions for external use
export * from '$lib/network/membership';

// ═══════════════════════════════════════════════════════════════════
// MEMBERSHIP STORES (Holster-backed via createStore)
// ═══════════════════════════════════════════════════════════════════

/**
 * My Declared Membership Lists
 * 
 * Maps org_id -> array of members (pubkeys or org_ids)
 * This is what YOU declare for organizations you manage.
 * 
 * Uses createStore() - handles persistence, validation, conflict resolution!
 */
export const myMembershipLists = createStore({
	holsterPath: 'membership-lists',
	schema: UserMembershipListsSchema,
	persistDebounce: 200
});

/**
 * My Membership Subscriptions
 * 
 * Maps org_id -> pubkey of source user
 * Instead of declaring membership yourself, you defer to someone else's list.
 * 
 * Uses createStore() - handles persistence, validation, conflict resolution!
 */
export const myMembershipSubscriptions = createStore({
	holsterPath: 'membership-subscriptions',
	schema: MembershipSubscriptionsSchema,
	persistDebounce: 200
});

/**
 * Membership Cache (Local-First Pattern)
 * 
 * Caches membership lists from network users we subscribe to.
 * 
 * Structure: Map<source_pubkey, Map<org_id, members[]>>
 * 
 * Updated when:
 * - We subscribe to a user's membership list for an org
 * - Network proves the membership has changed
 * 
 * Local-first: "Trust until proven otherwise"
 */
export const membershipCache: Writable<MembershipCache> = writable({});

// ═══════════════════════════════════════════════════════════════════
// LIFECYCLE FUNCTIONS
// ═══════════════════════════════════════════════════════════════════

/**
 * Initialize membership stores
 * Call this when user logs in
 */
export function initializeMembership() {
	myMembershipLists.initialize();
	myMembershipSubscriptions.initialize();
	console.log('[MEMBERSHIP] Initialized stores');
}

/**
 * Cleanup membership stores
 * Call this when user logs out
 */
export async function cleanupMembership() {
	await myMembershipLists.cleanup();
	await myMembershipSubscriptions.cleanup();
	membershipCache.set({});
	console.log('[MEMBERSHIP] Cleaned up');
}

// ═══════════════════════════════════════════════════════════════════
// DECLARED MEMBERSHIP OPERATIONS
// ═══════════════════════════════════════════════════════════════════

/**
 * Set membership list for an organization
 * This declares: "I say organization X has members Y, Z, ..."
 */
export function setMembershipList(org_id: string, members: string[]): void {
	const currentLists = get(myMembershipLists);
	const updatedLists = setMembershipListPure(currentLists, org_id, members);

	// Update store - createStore handles persistence!
	myMembershipLists.set(updatedLists);

	console.log(`[MEMBERSHIP] Set membership list for ${org_id}: ${members.length} members`);
}

/**
 * Remove declared membership list for an organization
 */
export function removeMembershipList(org_id: string): void {
	const currentLists = get(myMembershipLists);
	if (!currentLists) return;

	const updatedLists = removeMembershipListPure(currentLists, org_id);

	// Update store - createStore handles persistence!
	myMembershipLists.set(updatedLists);

	console.log(`[MEMBERSHIP] Removed membership list for ${org_id}`);
}

/**
 * Add member to an organization's membership list
 */
export function addMemberToList(org_id: string, member_id: string): void {
	const currentLists = get(myMembershipLists);
	const { updated, wasAdded } = addMemberToListPure(currentLists, org_id, member_id);

	if (!wasAdded) {
		console.log(`[MEMBERSHIP] Member ${member_id} already in ${org_id}`);
		return;
	}

	// Update store - createStore handles persistence!
	myMembershipLists.set(updated);

	console.log(`[MEMBERSHIP] Added member ${member_id} to ${org_id}`);
}

/**
 * Remove member from an organization's membership list
 */
export function removeMemberFromList(org_id: string, member_id: string): void {
	const currentLists = get(myMembershipLists);
	const { updated, wasRemoved } = removeMemberFromListPure(currentLists, org_id, member_id);

	if (!wasRemoved) return;

	// Update store - createStore handles persistence!
	myMembershipLists.set(updated);

	console.log(`[MEMBERSHIP] Removed member ${member_id} from ${org_id}`);
}

// ═══════════════════════════════════════════════════════════════════
// SUBSCRIPTION OPERATIONS
// ═══════════════════════════════════════════════════════════════════

/**
 * Subscribe to someone else's membership list for an organization
 * This says: "I defer to user X's membership list for organization Y"
 */
export function subscribeMembershipList(org_id: string, source_pubkey: string): void {
	const currentSubs = get(myMembershipSubscriptions);
	const updatedSubs = subscribeMembershipListPure(currentSubs, org_id, source_pubkey);

	// Update store - createStore handles persistence!
	myMembershipSubscriptions.set(updatedSubs);

	console.log(`[MEMBERSHIP] Subscribed to ${source_pubkey.slice(0, 20)}...'s list for ${org_id}`);
}

/**
 * Unsubscribe from a membership list
 */
export function unsubscribeMembershipList(org_id: string): void {
	const currentSubs = get(myMembershipSubscriptions);
	if (!currentSubs) return;

	const currentCache = get(membershipCache);
	const { subscriptions, cache, removedSource } = unsubscribeMembershipListPure(
		currentSubs,
		currentCache,
		org_id
	);

	// Update store - createStore handles persistence!
	myMembershipSubscriptions.set(subscriptions);

	// Also clear from cache
	membershipCache.set(cache);

	console.log(`[MEMBERSHIP] Unsubscribed from membership list for ${org_id}`);
}

// ═══════════════════════════════════════════════════════════════════
// CROSS-USER SUBSCRIPTIONS (Called by stores.svelte.ts auto-sync)
// ═══════════════════════════════════════════════════════════════════

/**
 * Subscribe to a user's membership lists from Holster
 * 
 * This is called by the auto-sync system in stores.svelte.ts
 * when myMembershipSubscriptions changes.
 * 
 * Uses myMembershipLists.subscribeToUser() - provided by createStore()!
 */
export function subscribeToUserMembershipLists(
	org_id: string,
	source_pubkey: string,
	callback?: (lists: UserMembershipLists | null) => void
): void {
	myMembershipLists.subscribeToUser(source_pubkey, (theirLists) => {
		if (!theirLists) {
			console.log(
				`[MEMBERSHIP] No lists from ${source_pubkey.slice(0, 20)}... for ${org_id}`
			);
			callback?.(null);
			return;
		}

		// Update cache for this org using pure function
		if (theirLists[org_id]) {
			const currentCache = get(membershipCache);
			const updatedCache = updateMembershipCachePure(
				currentCache,
				source_pubkey,
				org_id,
				theirLists[org_id]
			);
			membershipCache.set(updatedCache);

			console.log(
				`[MEMBERSHIP] Cached ${theirLists[org_id].length} members from ${source_pubkey.slice(0, 20)}... for ${org_id}`
			);
		}

		callback?.(theirLists);
	});
}

// ═══════════════════════════════════════════════════════════════════
// MEMBERSHIP RESOLUTION
// ═══════════════════════════════════════════════════════════════════

/**
 * Get membership list for an organization (cached or declared)
 * 
 * Resolution order:
 * 1. If we have a declared list → return it
 * 2. If we subscribe to someone → return cached list from them
 * 3. Otherwise → return undefined
 */
export function getMembershipList(org_id: string): string[] | undefined {
	const declared = get(myMembershipLists);
	const subscriptions = get(myMembershipSubscriptions);
	const cache = get(membershipCache);

	return resolveMembershipList(declared, subscriptions, cache, org_id);
}

/**
 * Check if we have membership data for an organization
 */
export function hasMembershipList(org_id: string): boolean {
	return getMembershipList(org_id) !== undefined;
}

