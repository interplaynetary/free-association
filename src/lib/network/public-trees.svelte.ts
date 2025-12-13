import { writable, derived } from 'svelte/store';
import type { Writable, Readable } from 'svelte/store';
import { holster } from '$lib/network/holster.svelte';

// ================================
// PUBLIC TREES LIST (Holster)
// ================================

/**
 * Public tree entry structure
 * Stored in Holster at 'freely-associating-public-trees'
 */
export interface PublicTreeEntry {
    alias: string;
    lastSeen: number;
    treePublished?: boolean; // Optional flag for future use
}

/**
 * Public trees data: pubkey -> PublicTreeEntry
 */
export type PublicTreesData = Record<string, PublicTreeEntry>;

// ================================
// STORES
// ================================

/**
 * All public tree entries indexed by pubkey
 */
export const publicTrees = writable<PublicTreesData>({});

/**
 * Array of public tree entries for easy iteration
 */
export const publicTreesArray: Readable<Array<PublicTreeEntry & { pubkey: string }>> = derived(
    publicTrees,
    ($publicTrees) => {
        return Object.entries($publicTrees).map(([pubkey, entry]) => ({
            ...entry,
            pubkey
        }));
    }
);

/**
 * Count of public trees
 */
export const publicTreesCount: Readable<number> = derived(
    publicTreesArray,
    ($publicTreesArray) => $publicTreesArray.length
);

// ================================
// SUBSCRIPTION MANAGEMENT
// ================================

let publicTreesCallback: ((data: any) => void) | null = null;
let isPublicTreesInitialized = false;

/**
 * Subscribe to freely-associating-public-trees list from Holster
 */
function subscribeToPublicTreesList() {
    if (isPublicTreesInitialized) {
        console.log('[PUBLIC-TREES] Already subscribed');
        return;
    }

    publicTreesCallback = (data: any) => {
        if (!data) return;

        // Process updates and deletions
        const updates: PublicTreesData = {};
        const deletions: string[] = [];

        for (const [key, value] of Object.entries(data)) {
            if (key.startsWith('_')) continue; // Skip metadata

            if (value === null) {
                // Null means deletion
                deletions.push(key);
            } else if (value && typeof value === 'object') {
                // Valid entry
                updates[key] = value as PublicTreeEntry;
            }
        }

        // Apply updates and deletions
        publicTrees.update($existing => {
            const updated = { ...$existing, ...updates };

            // Remove deleted entries
            for (const key of deletions) {
                delete updated[key];
            }

            return updated;
        });

        console.log('[PUBLIC-TREES] Updated:', {
            added: Object.keys(updates).length,
            removed: deletions.length,
            total: Object.keys(updates).length - deletions.length
        });
    };

    holster.get('freely-associating-public-trees').on(publicTreesCallback, true);
    isPublicTreesInitialized = true;
    console.log('[PUBLIC-TREES] Subscribed to public trees list');
}

/**
 * Initialize public trees subscription
 * Call this after holster authentication
 */
export function initializePublicTrees() {
    console.log('[PUBLIC-TREES] Initializing...');
    subscribeToPublicTreesList();
}

/**
 * Cleanup public trees subscription
 * Call this on logout
 */
export function cleanupPublicTrees() {
    if (publicTreesCallback) {
        holster.get('freely-associating-public-trees').off(publicTreesCallback);
        publicTreesCallback = null;
    }
    publicTrees.set({});
    isPublicTreesInitialized = false;
    console.log('[PUBLIC-TREES] Cleaned up');
}

// ================================
// PUBLISH/UNPUBLISH OPERATIONS
// ================================

/**
 * Publish my tree to the public index
 * @param alias - User's alias
 * @param pubkey - User's public key
 */
export async function publishMyTree(alias: string, pubkey: string): Promise<void> {
    return new Promise((resolve, reject) => {
        holster.get('freely-associating-public-trees').next(pubkey).put(
            {
                alias,
                lastSeen: Date.now(),
                treePublished: true
            },
            (err: any) => {
                if (err) {
                    console.error('[PUBLIC-TREES] ❌ Failed to publish tree:', err);
                    reject(new Error(`Failed to publish tree: ${err}`));
                } else {
                    console.log('[PUBLIC-TREES] ✅ Tree published successfully');
                    resolve();
                }
            }
        );
    });
}

/**
 * Unpublish my tree from the public index
 * @param pubkey - User's public key
 */
export async function unpublishMyTree(pubkey: string): Promise<void> {
    return new Promise((resolve, reject) => {
        holster.get('freely-associating-public-trees').next(pubkey).put(
            null,
            (err: any) => {
                if (err) {
                    console.error('[PUBLIC-TREES] ❌ Failed to unpublish tree:', err);
                    reject(new Error(`Failed to unpublish tree: ${err}`));
                } else {
                    console.log('[PUBLIC-TREES] ✅ Tree unpublished successfully');
                    resolve();
                }
            }
        );
    });
}

/**
 * Check if my tree is currently published
 * @param pubkey - User's public key
 * @returns true if tree is in public index
 */
export function isMyTreePublished(pubkey: string): boolean {
    const trees = publicTrees;
    let isPublished = false;

    trees.subscribe(($trees) => {
        isPublished = pubkey in $trees;
    })();

    return isPublished;
}

/**
 * Derived store: Is my tree published?
 */
export function createIsMyTreePublishedStore(pubkey: Readable<string>): Readable<boolean> {
    return derived(
        [publicTrees, pubkey],
        ([$publicTrees, $pubkey]) => {
            return $pubkey in $publicTrees;
        }
    );
}
