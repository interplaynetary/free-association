/**
 * Generic Mesh Store Utility
 * 
 * Simple, reliable store backed by Mesh with JSON serialization.
 * 
 * Core Flow:
 * 1. SAVE: JSON.stringify({ ...data, _updatedAt }) → Store string in Mesh
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
 * meshUser.get(path).put('{"field":"value",...,"_updatedAt":1234567890}')
 * // ↑ Just a JSON string, nothing fancy!
 * ```
 * 
 * Usage:
 * ```typescript
 * const store = createStore({
 *   meshPath: 'allocation/commitment',
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
import { meshUser, meshUserPub } from '$lib/network/mesh.svelte';
import * as z from 'zod';
import { shouldPersist } from '$lib/utils/data/meshTimestamp';

import { fastExtractTimestamp, fastParse } from '$lib/utils/data/fastJsonParser';
import * as idb from '$lib/utils/primitives/idb-keyval';

// ═══════════════════════════════════════════════════════════════════
// TYPES
// ═══════════════════════════════════════════════════════════════════

export interface StoreConfig<T extends z.ZodTypeAny> {
	/** Path in Mesh user space (e.g., 'commitment', 'tree') */
	meshPath: string;

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

export interface MeshStore<T> extends Readable<T | null> {
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

	/** Loading state (true during initialization/network sync) */
	loading: Readable<boolean>;
}

// ═══════════════════════════════════════════════════════════════════
// FACTORY
// ═══════════════════════════════════════════════════════════════════

export function createStore<T extends z.ZodTypeAny>(
	config: StoreConfig<T>
): MeshStore<z.infer<T>> {
	type DataType = z.infer<T>;

	// Debug: Log store creation
	console.log(`[MESH-STORE] 🏗️  Creating store for: ${config.meshPath}`);

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

	// Timeout to detect "empty" state (if network gives no data)
	let loadingTimeout: ReturnType<typeof setTimeout> | null = null;

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
		console.log(`[MESH-STORE:${config.meshPath}] 📥 LOADING - Raw:`, typeof data, data);

		if (typeof data !== 'string') {
			console.warn(`[MESH-STORE:${config.meshPath}] ⚠️  Expected string, got ${typeof data}:`, data);
			console.warn(`[MESH-STORE:${config.meshPath}] ⚠️  This is OLD FORMAT data! Run: await window.clearAllV5Stores()`);
			return;
		}

		// Step 1: Fast timestamp extraction (avoids full parse if data is stale)
		const networkTimestamp = await fastExtractTimestamp(data, '_updatedAt').catch(() => null);

		// Early return if data is older than what we have
		if (lastNetworkTimestamp && networkTimestamp && networkTimestamp <= lastNetworkTimestamp) {
			console.log(`[MESH-STORE:${config.meshPath}] ⏭️  Stale data - skipping (network: ${networkTimestamp}, local: ${lastNetworkTimestamp})`);
			return;
		}

		// Step 2: Parse JSON string (only if timestamp check passed)
		let parsedData: any;
		try {
			parsedData = fastParse(data);
		} catch (error) {
			console.error(`[MESH-STORE:${config.meshPath}] ❌ JSON parse failed:`, error);
			return;
		}

		// Debug
		if (config.meshPath.includes('tree') || config.meshPath.includes('commitment')) {
			console.log(`[MESH-STORE:${config.meshPath}] 📥 Parsed:`, parsedData);
			console.log(`[MESH-STORE:${config.meshPath}] 📥 Timestamp:`, networkTimestamp);
		}

		// Step 3: Validate with Zod (auto-strips _updatedAt)
		const validation = config.schema.safeParse(parsedData);
		if (!validation.success) {
			console.warn(`[MESH-STORE:${config.meshPath}] ❌ Schema Validation failed:`, validation.error);
			return;
		}

		// Step 3b: Custom Validation (Purge on failure)
		if (config.validate && !config.validate(validation.data)) {
			console.warn(`[MESH-STORE:${config.meshPath}] ⚠️  Custom Validation failed! PURGING store to clear bad data...`);
			store.set(null); // This clears memory AND triggers persistence of null (clearing DB)
			return;
		}

		// Step 4: Only update if different/newer
		const current = get(store);
		if (current && isEqual(current, validation.data)) {
			console.log(`[MESH-STORE:${config.meshPath}] ⏭️  Data unchanged - skipping`);
			return;
		}

		// Update if newer (or no timestamp tracking)
		if (!lastNetworkTimestamp || !networkTimestamp || networkTimestamp > lastNetworkTimestamp) {
			store.set(validation.data);
			if (networkTimestamp) {
				lastNetworkTimestamp = networkTimestamp;
			}
		}
		console.log(`[MESH-STORE:${config.meshPath}] ✅ Updated from network`);


		// We have data, so we are definitely loaded
		if (get(isLoading)) {
			isLoading.set(false);
			if (loadingTimeout) {
				clearTimeout(loadingTimeout);
				loadingTimeout = null;
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
		if (!get(meshUserPub)) {
			// fallback to localStorage (now IndexedDB) if configured
			if (config.localStorageKey && typeof window !== 'undefined') {
				console.log(`[MESH-STORE:${config.meshPath}] Not authenticated - using IndexedDB: ${config.localStorageKey}`);

				// Async load from IndexedDB
				idb.get<string>(config.localStorageKey).then((raw) => {
					if (raw) {
						try {
							// If it's a string, try to parse it. If it was stored as an object (via IDB), it might already be an object.
							// Our idb keyval stores whatever we give it. We will assume we store JSON strings for consistency with fallback logic,
							// OR we can store objects directly. The localStorage logic parsed JSON.
							// Let's store objects directly in IDB for performance, but careful with existing logic.
							// Wait, the previous logic did: localStorage.setItem(key, JSON.stringify(data)).
							// So we should expect a string if we migrate blindly, or object if we switch convention.
							// Let's stick to storing the DATA OBJECT directly in IDB to save parsing costs.

							// BUT, for compatibility with the exact logic below, let's see. 
							// Logic: const parsed = JSON.parse(raw).
							// So let's try to handle both (legacy string string or new object).

							let parsed = raw;
							if (typeof raw === 'string') {
								try {
									parsed = JSON.parse(raw);
								} catch (e) {
									// maybe it wasn't json string, but the object itself? No, raw is T.
									console.warn(`[MESH-STORE:${config.meshPath}] Failed to parse IDB data`, e);
								}
							}

							const validation = config.schema.safeParse(parsed);
							if (validation.success) {
								store.set(validation.data);
								console.log(`[MESH-STORE:${config.meshPath}] ✅ Loaded from IndexedDB`);
							} else {
								console.warn(`[MESH-STORE:${config.meshPath}] ❌ IndexedDB validation failed`, validation.error);
								// Self-healing
								if (config.localStorageKey) idb.del(config.localStorageKey);
							}
						} catch (e) {
							console.warn(`[MESH-STORE:${config.meshPath}] Failed to load IndexedDB`, e);
						}
					}
				}).catch(e => {
					console.warn(`[MESH-STORE:${config.meshPath}] Failed to read IndexedDB`, e);
				});

				return;
			}
			console.log(`[MESH-STORE:${config.meshPath}] Cannot subscribe: not authenticated (and no localStorageKey)`);
			return;
		}

		networkCallback = (data: any) => {
			// Handle empty data (confirmed empty from network)
			if (data === null || data === undefined) {
				console.log(`[MESH-STORE:${config.meshPath}] ∅ Received empty/null from network - store is empty`);
				if (get(isLoading)) {
					isLoading.set(false);
					if (loadingTimeout) {
						clearTimeout(loadingTimeout);
						loadingTimeout = null;
					}
				}
				return;
			}


			// Queue updates during persistence
			if (isPersisting) {
				// Fast timestamp extraction without full parsing
				if (typeof data === 'string') {
					fastExtractTimestamp(data, '_updatedAt')
						.then((networkTimestamp) => {
							// Only queue if different timestamp (external update)
							if (networkTimestamp && networkTimestamp !== lastNetworkTimestamp) {
								console.log(`[MESH-STORE:${config.meshPath}] External update during persistence - queueing`);
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

		meshUser.get(config.meshPath).on(networkCallback, true);
	}

	// ────────────────────────────────────────────────────────────────
	// Persistence
	// ────────────────────────────────────────────────────────────────

	async function persistNow(): Promise<void> {
		// Debug: Log persistence attempt
		if (config.meshPath.includes('tree')) {
			console.log(`[MESH-STORE:${config.meshPath}] 🚀 persistNow called`);
		}



		// 2. Authenticated Mode - Guard Clause
		if (!get(meshUserPub)) {
			console.log(`[MESH-STORE:${config.meshPath}] ❌ Not authenticated, skipping persistence`);
			return;
		}

		// RACE CONDITION GUARD:
		// Check if Svelte auth store is consistent with Gun auth state.
		// If meshUser.is is true (Gun level) but meshUserPub (Svelte level) is not yet updated,
		// we are in the "Login Transition Window".
		// In this window, we MUST NOT persist, because the store still holds Unauthenticated (Demo) data!
		const currentPub = get(meshUserPub);
		if (!currentPub) {
			console.warn(`[MESH-STORE:${config.meshPath}] ⚠️ Race condition detected: Gun authenticated but stores not synced. Aborting persist to prevent overwriting remote data with local demo data.`);
			return;
		}

		// Check if already persisting
		if (isPersisting) {
			if (config.meshPath.includes('tree')) {
				console.log(`[MESH-STORE:${config.meshPath}] ⏸️  Already persisting, queuing...`);
			}
			hasPendingLocalChanges = true;
			return;
		}

		const dataToSave = get(store);
		if (!dataToSave) {
			console.log(`[MESH-STORE:${config.meshPath}] ❌ No data to persist`);
			return;
		}

		// Debug: Log data about to be saved
		if (config.meshPath.includes('tree')) {
			console.log(`[MESH-STORE:${config.meshPath}] ✅ Data ready to persist:`, {
				hasId: 'id' in dataToSave,
				hasChildren: 'children' in dataToSave,
				childrenType: Array.isArray((dataToSave as any).children) ? 'array' : typeof (dataToSave as any).children
			});
		}

		// Set lock
		isPersisting = true;
		hasPendingLocalChanges = false;

		// LocalStorage -> IndexedDB Mode
		if (!get(meshUserPub) && config.localStorageKey) {
			try {
				if (dataToSave) {
					// Save directly to IndexedDB (as object, no need to stringify for IDB usually, but ensures consistency)
					// Let's store the object directly to be efficient.
					await idb.set(config.localStorageKey, dataToSave);
					console.log(`[MESH-STORE:${config.meshPath}] 💾 SAVED to IndexedDB`);
				} else {
					await idb.del(config.localStorageKey);
					console.log(`[MESH-STORE:${config.meshPath}] 🗑️ REMOVED from IndexedDB`);
				}
				isPersisting = false;
				processQueuedUpdate();
				return;
			} catch (e) {
				console.error(`[MESH-STORE:${config.meshPath}] Error saving to IndexedDB`, e);
				isPersisting = false;
				processQueuedUpdate();
				return;
			}
		}

		if (!get(meshUserPub)) {
			console.warn(`[MESH-STORE:${config.meshPath}] ⚠️  Cannot persist: not authenticated`);
			isPersisting = false;
			return;
		}

		const localTimestamp = Date.now();
		try {
			// Update local reference immediately (optimistic)
			lastNetworkTimestamp = localTimestamp;

			// Check if safe to persist
			if (!shouldPersist(localTimestamp, lastNetworkTimestamp)) {
				console.warn(`[MESH-STORE:${config.meshPath}] Skipping persist - network has newer data`);
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
			if (config.meshPath.includes('tree') || config.meshPath.includes('commitment')) {
				console.log(`[MESH-STORE:${config.meshPath}] 💾 SAVING - Data:`, dataToSave);
				console.log(`[MESH-STORE:${config.meshPath}] 💾 SAVING - Timestamp:`, localTimestamp);
				console.log(`[MESH-STORE:${config.meshPath}] 💾 SAVING - JSON size:`, jsonString.length, 'bytes');
			}

			// Persist to Mesh as a single JSON string
			await new Promise<void>((resolve, reject) => {
				meshUser.get(config.meshPath).put(jsonString, (err: any) => {
					if (err) {
						console.error(`[MESH-STORE:${config.meshPath}] Error persisting:`, err);
						isPersisting = false;
						processQueuedUpdate();
						return reject(err);
					}

					console.log(`[MESH-STORE:${config.meshPath}] ✅ Saved successfully`);
					isPersisting = false;
					processQueuedUpdate();
					resolve();
				});
			});
		} catch (error) {
			console.error(`[MESH-STORE:${config.meshPath}] Error processing:`, error);
			isPersisting = false;
			processQueuedUpdate();
			throw error;
		}
	}

	function persistDebounced(): void {
		// Debug: Log persistence trigger
		if (config.meshPath.includes('tree')) {
			console.log(`[MESH-STORE:${config.meshPath}] ⏱️  persistDebounced called, debounce=${config.persistDebounce}ms`);
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

	// Track auth subscription
	let authUnsub: (() => void) | null = null;

	function initialize() {
		// If already initialized, do nothing? 
		// Actually with auth subscription we might want to just ensure we are subscribed to auth
		if (isInitialized) {
			console.log(`[MESH-STORE:${config.meshPath}] Already initialized`);
			return;
		}

		console.log(`[MESH-STORE:${config.meshPath}] Initializing...`);
		isInitialized = true;
		isLoading.set(true);

		// Subscribe to auth changes to handle Login/Logout transitions
		// This callback runs immediately with the current value!
		authUnsub = meshUserPub.subscribe((pub) => {
			const isAuthenticated = !!pub;

			// 1. Cleanup previous network subscriptions to avoid duplicates
			// Use store checks instead of proxy access to avoid errors
			if (networkCallback && get(meshUserPub)) {
				meshUser.get(config.meshPath).off(networkCallback);
				networkCallback = null;
			}

			// 2. Reset internal state to avoid leaking data or showing stale state
			store.set(null);
			lastNetworkTimestamp = null;
			hasPendingLocalChanges = false;
			queuedNetworkUpdate = null;

			// Reset loading state for new user
			isLoading.set(true);
			if (loadingTimeout) clearTimeout(loadingTimeout);
			// Fallback: If no data comes in 3s, assume loaded (empty)
			loadingTimeout = setTimeout(() => {
				if (get(isLoading)) {
					console.log(`[MESH-STORE:${config.meshPath}] ⏱️  Loading timeout (3000ms) - assuming empty`);
					isLoading.set(false);
					loadingTimeout = null;
				}
			}, 3000);

			// 3. Re-run subscription logic
			if (isAuthenticated) {
				console.log(`[MESH-STORE:${config.meshPath}] 🔐 Authenticated (${pub.substring(0, 8)}...) - connecting to Mesh`);
			} else {
				console.log(`[MESH-STORE:${config.meshPath}] 🔓 Unauthenticated - checking LocalStorage`);
			}

			subscribeToNetwork();
		});
	}

	async function cleanup(): Promise<void> {
		// Wait for in-flight persistence
		if (isPersisting) {
			console.log(`[MESH-STORE:${config.meshPath}] Waiting for persistence to complete...`);
			const maxWait = 20000;
			const startTime = Date.now();

			while (isPersisting && (Date.now() - startTime) < maxWait) {
				await new Promise(resolve => setTimeout(resolve, 100));
			}
		}

		// Unsubscribe from network
		if (networkCallback && get(meshUserPub)) {
			meshUser.get(config.meshPath).off(networkCallback);
			networkCallback = null;
		}

		// Unsubscribe from auth changes
		if (authUnsub) {
			authUnsub();
			authUnsub = null;
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

		if (loadingTimeout) {
			clearTimeout(loadingTimeout);
			loadingTimeout = null;
		}

		console.log(`[MESH-STORE:${config.meshPath}] Cleaned up`);
	}

	// ────────────────────────────────────────────────────────────────
	// Cross-User Subscription
	// ────────────────────────────────────────────────────────────────

	function subscribeToUser(pubKey: string, callback: (data: DataType | null) => void) {
		if (!get(meshUserPub)) {
			console.log(`[MESH-STORE:${config.meshPath}] Not authenticated, cannot subscribe to ${pubKey.slice(0, 20)}...`);
			return;
		}

		meshUser.get([pubKey, config.meshPath]).on((data: any) => {
			if (!data) {
				callback(null);
				return;
			}

			try {
				// Parse JSON string
				if (typeof data !== 'string') {
					console.warn(`[MESH-STORE:${config.meshPath}] Expected string from ${pubKey.slice(0, 20)}...`);
					callback(null);
					return;
				}

				const parsedData = JSON.parse(data);

				// Validate with Zod (auto-strips _updatedAt)
				const validation = config.schema.safeParse(parsedData);
				if (!validation.success) {
					console.warn(
						`[MESH-STORE:${config.meshPath}] Invalid data from ${pubKey.slice(0, 20)}...`,
						validation.error
					);
					callback(null);
					return;
				}

				callback(validation.data);
			} catch (error) {
				console.error(
					`[MESH-STORE:${config.meshPath}] Error from ${pubKey.slice(0, 20)}...`,
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
		loading: { subscribe: isLoading.subscribe },

		// Writable interface
		set: (value: DataType) => {
			// Debug: ALWAYS log when set is called (to catch ALL stores)
			console.log(`[MESH-STORE:${config.meshPath}] 🔄 SET called`);

			// Debug: Log tree details
			if (config.meshPath.includes('tree')) {
				console.log(`[MESH-STORE:${config.meshPath}] 🔄 SET data:`, {
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

