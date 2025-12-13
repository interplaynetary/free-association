/**
 * View Context Store
 * 
 * Manages which user's data we're currently viewing in the UI.
 * Defaults to the authenticated user but can be switched to view any user's
 * commitment data, mutual recognition, slots, etc.
 * 
 * This enables features like:
 * - Viewing another user's public tree with their mutual recognition
 * - Browsing other users' needs and capacity
 * - Comparing different users' recognition patterns
 */

import { writable, get } from 'svelte/store';
import { holsterUserPub } from '$lib/network/holster.svelte';

/**
 * Current View Context - Which user are we viewing?
 * 
 * Defaults to null, then auto-syncs with holsterUserPub (authenticated user).
 * Can be changed to any user's pubkey to view their data.
 */
export const currentViewPubkey = writable<string | null>(null);

// Auto-sync with authenticated user's pubkey
holsterUserPub.subscribe($pub => {
    const current = get(currentViewPubkey);
    // Only set if not already set (don't override manual changes)
    if (!current && $pub) {
        currentViewPubkey.set($pub);
        console.log('[VIEW-CONTEXT] Initialized to authenticated user:', $pub.slice(0, 20) + '...');
    }
});

/**
 * Set view context to a specific user
 * Call this when navigating to view another user's data
 */
export function setViewContext(pubkey: string) {
    currentViewPubkey.set(pubkey);
    console.log('[VIEW-CONTEXT] Switched to user:', pubkey.slice(0, 20) + '...');
}

/**
 * Reset view context back to authenticated user
 * Call this when returning to main app or leaving user view
 */
export function resetViewContext() {
    const myPub = get(holsterUserPub);
    if (myPub) {
        currentViewPubkey.set(myPub);
        console.log('[VIEW-CONTEXT] Reset to authenticated user');
    }
}

/**
 * Check if currently viewing self
 */
export function isViewingSelf(): boolean {
    const viewPub = get(currentViewPubkey);
    const myPub = get(holsterUserPub);
    return viewPub === myPub;
}
