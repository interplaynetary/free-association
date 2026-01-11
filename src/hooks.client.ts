import { bootstrapApplication } from '$lib/bootstrap';

/**
 * Client-side hooks.
 * This file is imported when the client application starts.
 * We use it to bootstrap the protocol layer and global state once.
 */

console.log('[HOOKS] Initializing client application...');
bootstrapApplication();
