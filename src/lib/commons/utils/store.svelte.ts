/**
 * Generic Holster Store Utility
 * 
 * Extracted pattern from tree-holster.svelte.ts to work with ANY data structure.
 * 
 * Features:
 * - Zod schema validation
 * - Automatic timestamp management
 * - Conflict resolution (network wins if newer)
 * - Incremental updates (only persist what changed)
 * - Queue management (handle updates during persistence)
 * - Cross-user subscriptions (for mutual contributors)
 * 
 * Usage:
 * ```typescript
 * const commitmentStore = createStore({
 *   holsterPath: 'commitment',
 *   schema: CommitmentSchema
 * });
 * 
 * // Initialize (subscribes to network)
 * commitmentStore.initialize();
 * 
 * // Update local data (triggers persistence)
 * commitmentStore.set(newCommitment);
 * 
 * // Subscribe to changes
 * commitmentStore.subscribe(data => console.log(data));
 * ```
 */

import { writable, get } from 'svelte/store';
import type { Writable, Readable } from 'svelte/store';
import { holsterUser } from '$lib/commons/v5/holster.svelte';
import * as z from 'zod';
import { getTimestamp, shouldPersist } from '$lib/utils/holsterTimestamp';

// ═══════════════════════════════════════════════════════════════════
// TYPES
// ═══════════════════════════════════════════════════════════════════

export interface StoreConfig<T extends z.ZodTypeAny> {
	/** Path in Holster user space (e.g., 'commitment', 'tree') */
	holsterPath: string;
	
	/** Zod schema for validation */
	schema: T;
	
	/** Custom comparison function (default: JSON equality) */
	isEqual?: (a: z.infer<T>, b: z.infer<T>) => boolean;
	
	/** Debounce persistence (ms, default: 0 = immediate) */
	persistDebounce?: number;
	
	/** Convert data to Holster format before persistence (arrays → Records) */
	toHolsterFormat?: (data: z.infer<T>) => any;
	
	/** Convert data from Holster format after reading (Records → arrays) */
	fromHolsterFormat?: (data: any) => z.infer<T>;
}

export interface HolsterStore<T> extends Readable<T | null> {
	/** Set local value (triggers persistence) */
	set: (value: T) => void;
	
	/** Update local value (triggers persistence) */
	update: (updater: (current: T | null) => T | null) => void;
	
	/** Initialize store (subscribe to network) */
	initialize: () => void;
	
	/** Cleanup (unsubscribe, clear state) */
	cleanup: () => Promise<void>;
	
	/** Subscribe to another user's data */
	subscribeToUser: (pubKey: string, callback: (data: T | null) => void) => void;
	
	/** Check if persistence is in progress */
	isPersisting: () => boolean;
	
	/** Wait for persistence to complete */
	waitForPersistence: () => Promise<void>;
	
	/** Force persistence (even if debounced) */
	persist: () => Promise<void>;
}

// ═══════════════════════════════════════════════════════════════════
// FACTORY
// ═══════════════════════════════════════════════════════════════════

export function createStore<T extends z.ZodTypeAny>(
	config: StoreConfig<T>
): HolsterStore<z.infer<T>> {
	type DataType = z.infer<T>;
	
	// Internal state
	const store = writable<DataType | null>(null);
	const isLoading = writable(false);
	
	let lastNetworkTimestamp: number | null = null;
	let networkCallback: ((data: any) => void) | null = null;
	let isPersisting = false;
	let hasPendingLocalChanges = false;
	let queuedNetworkUpdate: any = null;
	let isInitialized = false;
	let persistDebounceTimeout: ReturnType<typeof setTimeout> | null = null;
	
	// ────────────────────────────────────────────────────────────────
	// Equality Check
	// ────────────────────────────────────────────────────────────────
	
	const isEqual = config.isEqual || ((a: DataType, b: DataType) => {
		try {
			return JSON.stringify(a) === JSON.stringify(b);
		} catch {
			return false;
		}
	});
	
	// ────────────────────────────────────────────────────────────────
	// Network Update Processing
	// ────────────────────────────────────────────────────────────────
	
	function processNetworkUpdate(data: any) {
		if (!data) return;
		
		// Extract timestamp
		const networkTimestamp = getTimestamp(data);
		
		// Remove timestamp wrapper
		const { _updatedAt, ...actualData } = data;
		
		// Convert from Holster format if converter provided
		const convertedData = config.fromHolsterFormat ? config.fromHolsterFormat(actualData) : actualData;
		
		// Validate with schema
		const validation = config.schema.safeParse(convertedData);
		if (!validation.success) {
			console.warn(`[HOLSTER-STORE:${config.holsterPath}] Invalid network data:`, validation.error);
			return;
		}
		
		// Only update if newer
		if (!lastNetworkTimestamp || (networkTimestamp && networkTimestamp > lastNetworkTimestamp)) {
			store.set(validation.data);
			
			if (networkTimestamp) {
				lastNetworkTimestamp = networkTimestamp;
			}
		}
	}
	
	function processQueuedUpdate() {
		// Process queued network update
		if (queuedNetworkUpdate) {
			const data = queuedNetworkUpdate;
			queuedNetworkUpdate = null;
			processNetworkUpdate(data);
		}
		
		// Retry persistence if pending local changes
		if (hasPendingLocalChanges) {
			hasPendingLocalChanges = false;
			setTimeout(() => {
				persistNow();
			}, 50);
		}
	}
	
	// ────────────────────────────────────────────────────────────────
	// Subscription
	// ────────────────────────────────────────────────────────────────
	
	function subscribeToNetwork() {
		if (!holsterUser.is) {
			console.log(`[HOLSTER-STORE:${config.holsterPath}] Cannot subscribe: not authenticated`);
			return;
		}
		
		networkCallback = (data: any) => {
			if (!data) return;
			
			// Queue updates during persistence
			if (isPersisting) {
				const networkTimestamp = getTimestamp(data);
				
				// Only queue if different timestamp (external update)
				if (networkTimestamp && networkTimestamp !== lastNetworkTimestamp) {
					console.log(`[HOLSTER-STORE:${config.holsterPath}] External update during persistence - queueing`);
					queuedNetworkUpdate = data;
				}
				return;
			}
			
			// Process immediately if not persisting
			processNetworkUpdate(data);
		};
		
		holsterUser.get(config.holsterPath).on(networkCallback, true);
	}
	
	// ────────────────────────────────────────────────────────────────
	// Persistence
	// ────────────────────────────────────────────────────────────────
	
	async function persistNow(): Promise<void> {
		if (!holsterUser.is) {
			console.log(`[HOLSTER-STORE:${config.holsterPath}] Not authenticated, skipping persistence`);
			return;
		}
		
		// Check if already persisting
		if (isPersisting) {
			hasPendingLocalChanges = true;
			return;
		}
		
		const dataToSave = get(store);
		if (!dataToSave) {
			console.log(`[HOLSTER-STORE:${config.holsterPath}] No data to persist`);
			return;
		}
		
		// Set lock
		isPersisting = true;
		hasPendingLocalChanges = false;
		
		try {
			const localTimestamp = Date.now();
			
			// Check if safe to persist
			if (!shouldPersist(localTimestamp, lastNetworkTimestamp)) {
				console.warn(`[HOLSTER-STORE:${config.holsterPath}] Skipping persist - network has newer data`);
				isPersisting = false;
				processQueuedUpdate();
				return;
			}
			
			// Update lastNetworkTimestamp NOW (before writing)
			lastNetworkTimestamp = localTimestamp;
			
			// Convert to Holster format if converter provided
			const HolsterData = config.toHolsterFormat ? config.toHolsterFormat(dataToSave) : dataToSave;
			
			// Wrap with timestamp
			const dataWithTimestamp = {
				...HolsterData,
				_updatedAt: localTimestamp
			};
			
			// Persist to Holster
			await new Promise<void>((resolve, reject) => {
				holsterUser.get(config.holsterPath).put(dataWithTimestamp, (err: any) => {
					if (err) {
						console.error(`[HOLSTER-STORE:${config.holsterPath}] Error persisting:`, err);
						isPersisting = false;
						processQueuedUpdate();
						return reject(err);
					}
					
					isPersisting = false;
					processQueuedUpdate();
					resolve();
				});
			});
		} catch (error) {
			console.error(`[HOLSTER-STORE:${config.holsterPath}] Error processing:`, error);
			isPersisting = false;
			processQueuedUpdate();
			throw error;
		}
	}
	
	function persistDebounced(): void {
		if (persistDebounceTimeout) {
			clearTimeout(persistDebounceTimeout);
		}
		
		if (config.persistDebounce && config.persistDebounce > 0) {
			persistDebounceTimeout = setTimeout(() => {
				persistNow();
			}, config.persistDebounce);
		} else {
			persistNow();
		}
	}
	
	// ────────────────────────────────────────────────────────────────
	// Initialization & Cleanup
	// ────────────────────────────────────────────────────────────────
	
	function initialize() {
		if (!holsterUser.is) {
			console.log(`[HOLSTER-STORE:${config.holsterPath}] Cannot initialize: not authenticated`);
			return;
		}
		
		if (isInitialized) {
			console.log(`[HOLSTER-STORE:${config.holsterPath}] Already initialized`);
			return;
		}
		
		console.log(`[HOLSTER-STORE:${config.holsterPath}] Initializing...`);
		isInitialized = true;
		isLoading.set(true);
		
		// Subscribe to network
		subscribeToNetwork();
	}
	
	async function cleanup(): Promise<void> {
		// Wait for in-flight persistence
		if (isPersisting) {
			console.log(`[HOLSTER-STORE:${config.holsterPath}] Waiting for persistence to complete...`);
			const maxWait = 20000;
			const startTime = Date.now();
			
			while (isPersisting && (Date.now() - startTime) < maxWait) {
				await new Promise(resolve => setTimeout(resolve, 100));
			}
		}
		
		// Unsubscribe
		if (networkCallback && holsterUser.is) {
			holsterUser.get(config.holsterPath).off(networkCallback);
			networkCallback = null;
		}
		
		// Clear state
		store.set(null);
		lastNetworkTimestamp = null;
		isInitialized = false;
		isPersisting = false;
		hasPendingLocalChanges = false;
		queuedNetworkUpdate = null;
		
		if (persistDebounceTimeout) {
			clearTimeout(persistDebounceTimeout);
			persistDebounceTimeout = null;
		}
		
		console.log(`[HOLSTER-STORE:${config.holsterPath}] Cleaned up`);
	}
	
	// ────────────────────────────────────────────────────────────────
	// Cross-User Subscription
	// ────────────────────────────────────────────────────────────────
	
	function subscribeToUser(pubKey: string, callback: (data: DataType | null) => void) {
		if (!holsterUser.is) {
			console.log(`[HOLSTER-STORE:${config.holsterPath}] Not authenticated, cannot subscribe to ${pubKey.slice(0, 20)}...`);
			return;
		}
		
		holsterUser.get([pubKey, config.holsterPath]).on((data: any) => {
			if (!data) {
				callback(null);
				return;
			}
			
			try {
				// Remove timestamp wrapper
				const { _updatedAt, ...actualData } = data;
				
				// Convert from Holster format if converter provided
				const convertedData = config.fromHolsterFormat ? config.fromHolsterFormat(actualData) : actualData;
				
				// Validate with schema
				const validation = config.schema.safeParse(convertedData);
				if (!validation.success) {
					console.warn(
						`[HOLSTER-STORE:${config.holsterPath}] Invalid data from ${pubKey.slice(0, 20)}...`,
						validation.error
					);
					callback(null);
					return;
				}
				
				callback(validation.data);
			} catch (error) {
				console.error(
					`[HOLSTER-STORE:${config.holsterPath}] Error processing data from ${pubKey.slice(0, 20)}...`,
					error
				);
				callback(null);
			}
		});
	}
	
	// ────────────────────────────────────────────────────────────────
	// Public API
	// ────────────────────────────────────────────────────────────────
	
	return {
		// Readable interface
		subscribe: store.subscribe,
		
		// Writable interface
		set: (value: DataType) => {
			store.set(value);
			persistDebounced();
		},
		
		update: (updater: (current: DataType | null) => DataType | null) => {
			store.update(current => {
				const updated = updater(current);
				if (updated !== null) {
					persistDebounced();
				}
				return updated;
			});
		},
		
		// Lifecycle
		initialize,
		cleanup,
		
		// Cross-user
		subscribeToUser,
		
		// Utilities
		isPersisting: () => isPersisting,
		waitForPersistence: async () => {
			if (!isPersisting) return;
			
			const maxWait = 20000;
			const startTime = Date.now();
			
			while (isPersisting && (Date.now() - startTime) < maxWait) {
				await new Promise(resolve => setTimeout(resolve, 100));
			}
		},
		persist: persistNow
	};
}

