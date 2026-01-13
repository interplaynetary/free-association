import { initHolster } from '$lib/network/holster';

/** @type {import('@sveltejs/kit').Handle} */
export async function handle({ event, resolve }) {
    // Initialize Holster lazily on the server side
    // This ensures that any API route or server logic using holster
    // doesn't encounter a TLA error (though server runtimes usually support TLA better than Safari)
    // The main benefit here is consistency and ensuring the Proxy target is ready.
    try {
        await initHolster();
    } catch (err) {
        console.error('[HOOKS] Failed to initialize Holster on server:', err);
    }

    const response = await resolve(event);
    return response;
}
