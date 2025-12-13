/**
 * Context-Aware Stores
 * 
 * These stores enable viewing ANY user's data, not just the authenticated user.
 * They use `currentViewPubkey` from context.svelte.ts to determine which user's
 * data to display.
 * 
 * Use these when building UI that shows another user's:
 * - Commitment data
 * - Recognition weights
 * - Need/capacity slots
 * - Mutual recognition
 */

import { derived, get } from 'svelte/store';
import type { Readable } from 'svelte/store';
import { currentViewPubkey } from './context.svelte';
import { holsterUserPub } from '$lib/network/holster.svelte';
import { myCommitmentStore, networkCommitments } from './stores.svelte';
import type {
    Commitment,
    GlobalRecognitionWeights,
    NeedSlot,
    AvailabilitySlot
} from '@playnet/free-association/schemas';

/**
 * Current User Commitment - Context-Aware
 * 
 * Returns the commitment for whoever we're currently viewing.
 * - If viewing self: returns myCommitmentStore
 * - If viewing others: looks up in networkCommitments
 */
export const currentUserCommitment: Readable<Commitment | null> = derived(
    [currentViewPubkey, myCommitmentStore, networkCommitments],
    ([$viewPub, $myCommit, $network]) => {
        if (!$viewPub) return null;

        const myPub = get(holsterUserPub);

        // Viewing self? Use my commitment store (more efficient)
        if ($viewPub === myPub) {
            return $myCommit;
        }

        // Viewing someone else? Look up in network
        const networkEntry = $network.get($viewPub);
        return networkEntry?.data || null;
    }
);

/**
 * Current User Recognition Weights - Context-Aware
 * 
 * Extracts recognition weights from current user's commitment.
 */
export const currentUserRecognitionWeights: Readable<GlobalRecognitionWeights> = derived(
    [currentUserCommitment],
    ([$commitment]) => {
        return $commitment?.global_recognition_weights || {};
    }
);

/**
 * Current User Need Slots - Context-Aware
 * 
 * Extracts need slots from current user's commitment.
 */
export const currentUserNeedSlots: Readable<NeedSlot[]> = derived(
    [currentUserCommitment],
    ([$commitment]) => {
        return $commitment?.need_slots || [];
    }
);

/**
 * Current User Capacity Slots - Context-Aware
 * 
 * Extracts capacity slots from current user's commitment.
 */
export const currentUserCapacitySlots: Readable<AvailabilitySlot[]> = derived(
    [currentUserCommitment],
    ([$commitment]) => {
        return $commitment?.capacity_slots || [];
    }
);

/**
 * Current User Need Types - Context-Aware
 * 
 * Extracts unique need type IDs from current user's need slots.
 */
export const currentUserNeedTypes: Readable<string[]> = derived(
    [currentUserNeedSlots],
    ([$slots]) => {
        if (!$slots || $slots.length === 0) return [];

        const typeIds = new Set<string>();
        for (const slot of $slots) {
            if (slot.need_type_id) {
                typeIds.add(slot.need_type_id);
            }
        }

        return Array.from(typeIds).sort();
    }
);

/**
 * Current User Capacity Types - Context-Aware
 * 
 * Extracts unique capacity type IDs from current user's capacity slots.
 */
export const currentUserCapacityTypes: Readable<string[]> = derived(
    [currentUserCapacitySlots],
    ([$slots]) => {
        if (!$slots || $slots.length === 0) return [];

        const typeIds = new Set<string>();
        for (const slot of $slots) {
            if (slot.need_type_id) {
                typeIds.add(slot.need_type_id);
            }
        }

        return Array.from(typeIds).sort();
    }
);

/**
 * Current User Mutual Recognition - Context-Aware ⭐
 * 
 * Calculates mutual recognition for whoever we're currently viewing.
 * This is the KEY store that enables viewing anyone's mutual recognition!
 * 
 * Algorithm (same as myMutualRecognition but for any user):
 * - Get their recognition weights (who they recognize)
 * - Get others' recognition of them (from their cache)
 * - MR = min(their_rec_of_other, other_rec_of_them)
 * 
 * Special case: Self-recognition (them recognizing themselves)
 * - MR(them, them) = their_rec[them]
 */
export const currentUserMutualRecognition: Readable<GlobalRecognitionWeights> = derived(
    [currentViewPubkey, currentUserCommitment],
    ([$viewPub, $commitment]) => {
        console.log('[🤝 CONTEXT-MR] Computing mutual recognition for viewed user...');

        if (!$viewPub || !$commitment) {
            console.log('[🤝 CONTEXT-MR] ❌ No view pubkey or commitment');
            return {};
        }

        // Source: Who they recognize (from their tree)
        const theirWeights = $commitment.global_recognition_weights || {};

        // Cache: Others' recognition of them (from network, in their commitment)
        const othersRecOfThem = $commitment.others_recognition_of_me || {};

        const mutualRec: GlobalRecognitionWeights = {};

        const theirRecCount = Object.keys(theirWeights).length;
        const cacheCount = Object.keys(othersRecOfThem).length;

        console.log(`[🤝 CONTEXT-MR] Viewed user: ${$viewPub.slice(0, 20)}...`);
        console.log(`[🤝 CONTEXT-MR] Their recognition: ${theirRecCount} entries`);
        console.log(`[🤝 CONTEXT-MR] Cached others' rec of them: ${cacheCount} entries`);

        // For everyone they recognize (including themselves!)
        for (const otherPub in theirWeights) {
            const theirRecOfOther = theirWeights[otherPub] || 0;

            // ✅ SPECIAL CASE: Self-recognition
            if (otherPub === $viewPub) {
                mutualRec[otherPub] = theirRecOfOther;  // MR(them, them) = theirRec[them]
                console.log(`[🤝 CONTEXT-MR]   ${otherPub.slice(0, 20)}... (SELF): MR=${(theirRecOfOther * 100).toFixed(2)}%`);
                continue;
            }

            // Get other's recognition of them from cache
            const otherWeights = othersRecOfThem[otherPub];
            const otherRecOfThem = otherWeights?.[$viewPub] || 0;

            // Compute MR
            const mr = Math.min(theirRecOfOther, otherRecOfThem);
            mutualRec[otherPub] = mr;

            if (mr > 0 || theirRecOfOther > 0 || otherRecOfThem > 0) {
                const source = otherWeights ? 'CACHED' : 'AWAITING';
                console.log(`[🤝 CONTEXT-MR]   ${otherPub.slice(0, 20)}...: them→other=${(theirRecOfOther * 100).toFixed(2)}%, other→them=${(otherRecOfThem * 100).toFixed(2)}%, MR=${(mr * 100).toFixed(2)}% [${source}]`);
            }
        }

        const mutualCount = Object.values(mutualRec).filter(mr => mr > 0).length;
        console.log(`[🤝 CONTEXT-MR] ✅ Computed ${mutualCount} mutual relationships for viewed user`);

        return mutualRec;
    }
);
