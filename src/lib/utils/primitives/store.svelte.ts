/**
 * Generic Holster Store Utility
 * 
 * Simple, reliable store backed by Holster with JSON serialization.
 * 
 * Core Flow:
 * 1. SAVE: JSON.stringify({ ...data, _updatedAt }) → Store string in Holster
 * 2. LOAD: Get string → JSON.parse() → Validate with Zod → Update if valid & different
 * 3. VALIDATION: Only valid data reaches the store (invalid data is rejected)
 * 
 * Features:
 * - JSON serialization (simple, standard, reliable!)
 * - Zod validation (only valid data gets through)
 * - Timestamp tracking (conflict resolution)
 * - Equality checking (skip duplicate updates)
 * - Queue management (handle updates during persistence)
 * - Cross-user subscriptions (for mutual contributors)
 * 
 * Storage Format:
 * ```typescript
 * holsterUser.get(path).put('{"field":"value",...,"_updatedAt":1234567890}')
 * // ↑ Just a JSON string, nothing fancy!
 * ```
 * 
 * Usage:
 * ```typescript
 * const store = createStore({
 *   holsterPath: 'allocation/commitment',
 *   schema: CommitmentSchema
 * });
 * 
 * store.initialize();  // Subscribe to network
 * store.set(data);     // Update & persist
 * store.subscribe(data => ...);  // React to changes
 * ```
 */

import { writable, get } from 'svelte/store';
import type { Writable, Readable } from 'svelte/store';
import { holsterUser } from '$lib/network/holster.svelte';
import * as z from 'zod';
import { shouldPersist } from '$lib/utils/data/holsterTimestamp';
import { fastExtractTimestamp, fastParse } from '$lib/utils/data/fastJsonParser';

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

	// NOTE: Converters removed! We now use JSON.stringify/parse for simplicity and reliability.
	// This eliminates 400+ lines of complex conversion logic and entire classes of bugs.

	/** 
	 * Custom validation logic.
	 * If this returns false, the data is considered CORRUPT or STALE and is effectively PURGED from the store (set to null).
	 * Use this to clean up bad states from the database.
	 */
	validate?: (data: z.infer<T>) => boolean;

	/**
	 * LocalStorage key for unauthenticated usage.
	 * If provided, the store will fallback to localStorage when the user is NOT logged in.
	 * This allows the SAME store to work in "Demo Mode".
	 */
	localStorageKey?: string;
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

	// Debug: Log store creation
	console.log(`[HOLSTER-STORE] 🏗️  Creating store for: ${config.holsterPath}`);

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

	async function processNetworkUpdate(data: any) {
		// Skip null/undefined/empty
		if (!data) return;

		// Debug: ALWAYS log what we received to diagnose issues
		console.log(`[HOLSTER-STORE:${config.holsterPath}] 📥 LOADING - Raw:`, typeof data, data);

		if (typeof data !== 'string') {
			console.warn(`[HOLSTER-STORE:${config.holsterPath}] ⚠️  Expected string, got ${typeof data}:`, data);
			console.warn(`[HOLSTER-STORE:${config.holsterPath}] ⚠️  This is OLD FORMAT data! Run: await window.clearAllV5Stores()`);
			return;
		}

		// Step 1: Fast timestamp extraction (avoids full parse if data is stale)
		const networkTimestamp = await fastExtractTimestamp(data, '_updatedAt').catch(() => null);

		// Early return if data is older than what we have
		if (lastNetworkTimestamp && networkTimestamp && networkTimestamp <= lastNetworkTimestamp) {
			console.log(`[HOLSTER-STORE:${config.holsterPath}] ⏭️  Stale data - skipping (network: ${networkTimestamp}, local: ${lastNetworkTimestamp})`);
			return;
		}

		// Step 2: Parse JSON string (only if timestamp check passed)
		let parsedData: any;
		try {
			parsedData = fastParse(data);
		} catch (error) {
			console.error(`[HOLSTER-STORE:${config.holsterPath}] ❌ JSON parse failed:`, error);
			return;
		}

		// Debug
		if (config.holsterPath.includes('tree') || config.holsterPath.includes('commitment')) {
			console.log(`[HOLSTER-STORE:${config.holsterPath}] 📥 Parsed:`, parsedData);
			console.log(`[HOLSTER-STORE:${config.holsterPath}] 📥 Timestamp:`, networkTimestamp);
		}

		// Step 3: Validate with Zod (auto-strips _updatedAt)
		const validation = config.schema.safeParse(parsedData);
		if (!validation.success) {
			console.warn(`[HOLSTER-STORE:${config.holsterPath}] ❌ Schema Validation failed:`, validation.error);
			return;
		}

		// Step 3b: Custom Validation (Purge on failure)
		if (config.validate && !config.validate(validation.data)) {
			console.warn(`[HOLSTER-STORE:${config.holsterPath}] ⚠️  Custom Validation failed! PURGING store to clear bad data...`);
			store.set(null); // This clears memory AND triggers persistence of null (clearing DB)
			return;
		}

		// Step 4: Only update if different/newer
		const current = get(store);
		if (current && isEqual(current, validation.data)) {
			console.log(`[HOLSTER-STORE:${config.holsterPath}] ⏭️  Data unchanged - skipping`);
			return;
		}

		// Update if newer (or no timestamp tracking)
		if (!lastNetworkTimestamp || !networkTimestamp || networkTimestamp > lastNetworkTimestamp) {
			store.set(validation.data);
			if (networkTimestamp) {
				lastNetworkTimestamp = networkTimestamp;
			}
			console.log(`[HOLSTER-STORE:${config.holsterPath}] ✅ Updated from network`);
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
			// fallback to localStorage if configured
			if (config.localStorageKey && typeof window !== 'undefined') {
				console.log(`[HOLSTER-STORE:${config.holsterPath}] Not authenticated - using LocalStorage: ${config.localStorageKey}`);
				try {
					const raw = localStorage.getItem(config.localStorageKey);
					if (raw) {
						// LocalStorage is standard JSON, not holster packed string with timestamp usually? 
						// Actually holster packed string format is just JSON with _updatedAt.
						// We can treat it simply as object parse.
						const parsed = JSON.parse(raw);
						// Validate
						const validation = config.schema.safeParse(parsed);
						if (validation.success) {
							store.set(validation.data);
							console.log(`[HOLSTER-STORE:${config.holsterPath}] ✅ Loaded from LocalStorage`);
						} else {
							console.warn(`[HOLSTER-STORE:${config.holsterPath}] ❌ LocalStorage validation failed`, validation.error);
						}
					}
				} catch (e) {
					console.warn(`[HOLSTER-STORE:${config.holsterPath}] Failed to load LocalStorage`, e);
				}
				return;
			}
			console.log(`[HOLSTER-STORE:${config.holsterPath}] Cannot subscribe: not authenticated (and no localStorageKey)`);
			return;
		}

		networkCallback = (data: any) => {
			if (!data) return;

			// Queue updates during persistence
			if (isPersisting) {
				// Fast timestamp extraction without full parsing
				if (typeof data === 'string') {
					fastExtractTimestamp(data, '_updatedAt')
						.then((networkTimestamp) => {
							// Only queue if different timestamp (external update)
							if (networkTimestamp && networkTimestamp !== lastNetworkTimestamp) {
								console.log(`[HOLSTER-STORE:${config.holsterPath}] External update during persistence - queueing`);
								queuedNetworkUpdate = data;
							}
						})
						.catch(() => {
							// If extraction fails, queue it anyway to be safe
							queuedNetworkUpdate = data;
						});
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
		// Debug: Log persistence attempt
		if (config.holsterPath.includes('tree')) {
			console.log(`[HOLSTER-STORE:${config.holsterPath}] 🚀 persistNow called`);
		}

		// 1. Unauthenticated / Local Mode
		// If not authenticated but we have a LocalStorage key, persist there instead!
		if (!holsterUser.is && config.localStorageKey) {
			const dataToSave = get(store);
			if (dataToSave) {
				try {
					localStorage.setItem(config.localStorageKey, JSON.stringify(dataToSave));
					console.log(`[HOLSTER-STORE:${config.holsterPath}] 💾 Persisted to LocalStorage (${config.localStorageKey})`);
				} catch (err) {
					console.error(`[HOLSTER-STORE:${config.holsterPath}] ❌ LocalStorage persist failed:`, err);
				}
			} else {
				// If data is null, remove from LocalStorage
				localStorage.removeItem(config.localStorageKey);
				console.log(`[HOLSTER-STORE:${config.holsterPath}] 🗑️  Removed from LocalStorage (${config.localStorageKey})`);
			}
			isPersisting = false;
			return;
		}

		// 2. Authenticated Mode - Guard Clause
		if (!holsterUser.is) {
			console.log(`[HOLSTER-STORE:${config.holsterPath}] ❌ Not authenticated, skipping persistence`);
			return;
		}

		// Check if already persisting
		if (isPersisting) {
			if (config.holsterPath.includes('tree')) {
				console.log(`[HOLSTER-STORE:${config.holsterPath}] ⏸️  Already persisting, queuing...`);
			}
			hasPendingLocalChanges = true;
			return;
		}

		const dataToSave = get(store);
		if (!dataToSave) {
			console.log(`[HOLSTER-STORE:${config.holsterPath}] ❌ No data to persist`);
			return;
		}

		// Debug: Log data about to be saved
		if (config.holsterPath.includes('tree')) {
			console.log(`[HOLSTER-STORE:${config.holsterPath}] ✅ Data ready to persist:`, {
				hasId: 'id' in dataToSave,
				hasChildren: 'children' in dataToSave,
				childrenType: Array.isArray((dataToSave as any).children) ? 'array' : typeof (dataToSave as any).children
			});
		}

		// Set lock
		isPersisting = true;
		hasPendingLocalChanges = false;

		// LocalStorage Mode
		if (!holsterUser.is && config.localStorageKey) {
			try {
				if (dataToSave) {
					// We just save raw object to local storage, no timestamp wrapping needed for simple usage?
					// Or stick to wrapping for consistency? Let's assume simple JSON for demo mode simplicity.
					// DemoTreeStore used simple JSON. Let's match that.
					localStorage.setItem(config.localStorageKey, JSON.stringify(dataToSave));
					console.log(`[HOLSTER-STORE:${config.holsterPath}] 💾 SAVED to LocalStorage`);
				} else {
					localStorage.removeItem(config.localStorageKey);
					console.log(`[HOLSTER-STORE:${config.holsterPath}] 🗑️ REMOVED from LocalStorage`);
				}
				isPersisting = false;
				processQueuedUpdate();
				return;
			} catch (e) {
				console.error(`[HOLSTER-STORE:${config.holsterPath}] Error saving to LocalStorage`, e);
				isPersisting = false;
				processQueuedUpdate();
				return;
			}
		}

		if (!holsterUser.is) {
			console.warn(`[HOLSTER-STORE:${config.holsterPath}] ⚠️  Cannot persist: not authenticated`);
			isPersisting = false;
			return;
		}

		const localTimestamp = Date.now();
		try {
			// Update local reference immediately (optimistic)
			lastNetworkTimestamp = localTimestamp;

			// Check if safe to persist
			if (!shouldPersist(localTimestamp, lastNetworkTimestamp)) {
				console.warn(`[HOLSTER-STORE:${config.holsterPath}] Skipping persist - network has newer data`);
				isPersisting = false;
				processQueuedUpdate();
				return;
			}

			// FULL JSON: Everything in one JSON string (including timestamp!)
			const dataWithTimestamp = {
				...dataToSave,
				_updatedAt: localTimestamp
			};

			const jsonString = JSON.stringify(dataWithTimestamp);

			// Debug: Log serialization for trees and commitments
			if (config.holsterPath.includes('tree') || config.holsterPath.includes('commitment')) {
				console.log(`[HOLSTER-STORE:${config.holsterPath}] 💾 SAVING - Data:`, dataToSave);
				console.log(`[HOLSTER-STORE:${config.holsterPath}] 💾 SAVING - Timestamp:`, localTimestamp);
				console.log(`[HOLSTER-STORE:${config.holsterPath}] 💾 SAVING - JSON size:`, jsonString.length, 'bytes');
			}

			// Persist to Holster as a single JSON string
			await new Promise<void>((resolve, reject) => {
				holsterUser.get(config.holsterPath).put(jsonString, (err: any) => {
					if (err) {
						console.error(`[HOLSTER-STORE:${config.holsterPath}] Error persisting:`, err);
						isPersisting = false;
						processQueuedUpdate();
						return reject(err);
					}

					console.log(`[HOLSTER-STORE:${config.holsterPath}] ✅ Saved successfully`);
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
		// Debug: Log persistence trigger
		if (config.holsterPath.includes('tree')) {
			console.log(`[HOLSTER-STORE:${config.holsterPath}] ⏱️  persistDebounced called, debounce=${config.persistDebounce}ms`);
		}

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
		if (!holsterUser.is && !config.localStorageKey) {
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
				// Parse JSON string
				if (typeof data !== 'string') {
					console.warn(`[HOLSTER-STORE:${config.holsterPath}] Expected string from ${pubKey.slice(0, 20)}...`);
					callback(null);
					return;
				}

				const parsedData = JSON.parse(data);

				// Validate with Zod (auto-strips _updatedAt)
				const validation = config.schema.safeParse(parsedData);
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
					`[HOLSTER-STORE:${config.holsterPath}] Error from ${pubKey.slice(0, 20)}...`,
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
			// Debug: ALWAYS log when set is called (to catch ALL stores)
			console.log(`[HOLSTER-STORE:${config.holsterPath}] 🔄 SET called`);

			// Debug: Log tree details
			if (config.holsterPath.includes('tree')) {
				console.log(`[HOLSTER-STORE:${config.holsterPath}] 🔄 SET data:`, {
					value,
					hasId: value && typeof value === 'object' && 'id' in value,
					hasChildren: value && typeof value === 'object' && 'children' in value
				});
			}
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

