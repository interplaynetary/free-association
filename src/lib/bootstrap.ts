import { initializeProtocol, cleanupProtocol } from '$lib/protocol/startup';
import { initializeGlobalState } from '$lib/global.svelte';

/**
 * Bootstraps the client-side application.
 * Handles dynamic imports and initialization sequencing to ensure
 * all dependencies are ready before starting the protocol and UI state.
 */
export async function bootstrapApplication() {
    console.log('[BOOTSTRAP] Starting application...');

    // 1. Load services dynamically (avoids circular dependency/order issues)
    await import('$lib/services');

    // 2. Initialize protocol layer (Auth, Stores, Capacity)
    initializeProtocol();

    // 3. Initialize global state (UI Subscriptions)
    // This prevents TDZ errors on iOS Safari when accessing the page store
    initializeGlobalState();

    // 4. Request notification permissions (optional UI enhancement)
    if ('Notification' in window && Notification.permission === 'default') {
        Notification.requestPermission().then((permission) => {
            console.log('[BOOTSTRAP] Notification permission:', permission);
        });
    }

    // 5. Initialize Demo Data (if empty) - Uses explicit logic, no $effects!
    // We import dynamically to avoid circular dependencies if any
    const stores = await import('$lib/protocol/stores/stores.svelte');
    const example = await import('$lib/utils/example-stores');
    const { get } = await import('svelte/store');

    // Watch loading state
    const unsub = stores.myCommitmentStore.loading.subscribe(isLoading => {
        if (!isLoading) {
            // Once loaded, check if empty
            // We check local-derived stores which are now populated from commitment
            const caps = get(stores.myCapacitySlotsStore);
            const needs = get(stores.myNeedSlotsStore);

            const hasCaps = caps && caps.length > 0;
            const hasNeeds = needs && needs.length > 0;

            if (!hasCaps) {
                console.log('[BOOTSTRAP] 🆕 No capacities - Initializing demo capacities...');
                example.populateCapacitySlots();
            }

            if (!hasNeeds) {
                console.log('[BOOTSTRAP] 🆕 No needs - Initializing demo needs...');
                example.populateNeedSlots();
            }

            if (!hasCaps && !hasNeeds) {
                // Only populate tree if everything was empty (fresh start)
                // or check tree specifically? For now, we assume tree follows generic init
                example.populateRecognitionTree();
            }
            console.log(`[BOOTSTRAP] ✅ Existing user verified (${caps?.length || 0} caps, ${needs?.length || 0} needs)`);

            // Run once then unsubscribe
            unsub();
        }
    });

    console.log('[BOOTSTRAP] Application ready.');
}

export function teardownApplication() {
    cleanupProtocol();
}
