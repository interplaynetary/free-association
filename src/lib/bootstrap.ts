import { browser } from '$app/environment';
import { initializeProtocol, cleanupProtocol } from '$lib/protocol/startup';
import { initializeGlobalState } from '$lib/global.svelte';

/**
 * Bootstraps the client-side application.
 * Handles dynamic imports and initialization sequencing to ensure
 * all dependencies are ready before starting the protocol and UI state.
 */
export async function bootstrapApplication() {
    if (!browser) return;

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

    console.log('[BOOTSTRAP] Application ready.');
}

export function teardownApplication() {
    if (!browser) return;
    cleanupProtocol();
}
