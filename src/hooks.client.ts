import { bootstrapApplication } from '$lib/bootstrap';

// Monkey-patch HTMLCanvasElement.prototype.getContext to set willReadFrequently: true
// This silences the "Multiple readback operations..." warning from gun-avatar and optimizes performance
if (typeof HTMLCanvasElement !== 'undefined') {
    const originalGetContext = HTMLCanvasElement.prototype.getContext;
    // @ts-ignore - Override signature
    HTMLCanvasElement.prototype.getContext = function (
        contextId: string,
        options?: any
    ) {
        if (contextId === '2d') {
            options = options || {};
            // Force willReadFrequently to true for all 2D contexts 
            // to optimize for gun-avatar specific behavior
            options.willReadFrequently = true;
        }
        return originalGetContext.call(this, contextId, options);
    };
}

/**
 * Client-side hooks.
 * This file is imported when the client application starts.
 * We use it to bootstrap the protocol layer and global state once.
 */

console.log('[HOOKS] Initializing client application...');
bootstrapApplication();
