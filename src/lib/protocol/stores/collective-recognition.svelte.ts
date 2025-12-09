/**
 * Collective Recognition & Resource Allocation Protocol (V5) - Svelte Integration
 * 
 * This file re-exports all pure functions from collective-recognition.ts
 * for use in Svelte components. The actual implementation is in the pure .ts file.
 * 
 * This separation allows running computations without Svelte dependencies.
 */

// Re-export everything from the pure TypeScript module
export * from '../../../../packages/protocol/src/collective/collective-recognition';

