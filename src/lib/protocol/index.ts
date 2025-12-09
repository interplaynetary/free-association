/**
 * Free Association Protocol
 * 
 * Main entry point for the Free Association protocol.
 * 
 * Structure:
 * - core/ - Pure protocol logic (framework-agnostic, npm-ready)
 * - stores/ - Svelte reactive wrappers (P2P synchronized stores)
 * 
 * Exports:
 * - Core Protocol (schemas, allocation, recognition, tree operations)
 * - Svelte Stores (reactive wrappers for real-time sync)
 * - Utilities (matching, filtering, memoization)
 * - ITC (Interval Tree Clocks for causality tracking)
 * 
 * V5 UPDATE: Global recognition model with event-driven allocation
 * V6 UPDATE: Clean separation of pure core from reactive stores
 */

import '$lib/network/holster.svelte'; // This initializes Holster (migration Phase 1)

// ═══════════════════════════════════════════════════════════════════
// CORE PROTOCOL (Pure, framework-agnostic)
// ═══════════════════════════════════════════════════════════════════

export * from '@playnet/free-association';

// ═══════════════════════════════════════════════════════════════════
// EXTERNAL DEPENDENCIES (not in core, but commonly used)
// ═══════════════════════════════════════════════════════════════════

// Interval Tree Clocks (ITC) - Safe for SSR
export * from '$lib/utils/primitives/itc';

// ═══════════════════════════════════════════════════════════════════
// SVELTE STORES (Browser-only, reactive wrappers)
// ═══════════════════════════════════════════════════════════════════

export * from './stores';
