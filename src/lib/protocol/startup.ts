import { initializeAuth } from '$lib/network/mesh.svelte';
import { startStoreService } from '$lib/protocol/stores/stores.svelte';
import { initializeCapacitySubscriptions, cleanupCapacitySubscriptions } from '$lib/network/slot-subscriptions.svelte';
import { startAllocationService } from '$lib/protocol/stores/allocation.svelte';
import { browser } from '$app/environment';

/**
 * STARTUP.TS
 * 
 * Central entry point for initializing the application's protocol layer.
 * This ensures that all side-effects (listeners, loops, network connections)
 * are started in a controlled, predictable order, rather than implicitly on import.
 */

const SERVICES = {
    auth: null as (() => void) | null,
    stores: null as (() => void) | null,
    allocation: null as (() => void) | null,
    capacity: null as (() => void) | null,
}; // Simple service registry

let isInitialized = false;

export async function initializeProtocol() {
    if (!browser) return;
    if (isInitialized) {
        console.warn('[STARTUP] Protocol already initialized, skipping.');
        return;
    }

    console.log('[STARTUP] 🚀 Initializing Protocol Layer (V6 - Service Architecture)...');
    isInitialized = true;

    // 1. Auth Service
    // We MUST await this because it initializes the Mesh proxy target.
    // Without this wait, subsequent services (stores) will crash when accessing Mesh.
    SERVICES.auth = await initializeAuth();

    // 2. Data Service (Persistent Stores + Sync)
    SERVICES.stores = startStoreService();

    // 3. Network Service (Capacity Subscriptions)
    initializeCapacitySubscriptions();
    SERVICES.capacity = () => { cleanupCapacitySubscriptions(); }; // Async wrapper

    // 4. Allocation Engine (Loops, Composition, Publishing)
    // SERVICES.allocation = startAllocationService();

    console.log('[STARTUP] ✅ Protocol initialization complete.');
    (window as any).__PROTOCOL_INITIALIZED__ = true;
}


export function cleanupProtocol() {
    if (!isInitialized) return;

    console.log('[STARTUP] 🛑 Cleaning up Protocol Layer...');

    if (SERVICES.allocation) SERVICES.allocation();
    if (SERVICES.stores) SERVICES.stores();
    if (SERVICES.auth) SERVICES.auth();
    if (SERVICES.capacity) SERVICES.capacity();

    // Reset registry
    SERVICES.auth = null;
    SERVICES.stores = null;
    SERVICES.allocation = null;
    SERVICES.capacity = null;

    isInitialized = false;

    console.log('[STARTUP] Protocol cleanup complete.');
}
