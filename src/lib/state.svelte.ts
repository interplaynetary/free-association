// Re-export everything from the modular state system
export * from './state/index.svelte';

// For backward compatibility, also export the main functions that were previously here
export { recalculateFromTree } from './state/calculations.svelte';
