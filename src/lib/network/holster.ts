import type { HolsterInstance, HolsterUser } from '@mblaney/holster';
import { config } from '@playnet/free-association/config';

// ═══════════════════════════════════════════════════════════════════
// HOLSTER INITIALIZATION (LAZY / PROXY)
// ═══════════════════════════════════════════════════════════════════

// Internal state to hold the real instances
let _holster: HolsterInstance | undefined;
let _holsterUser: HolsterUser | undefined;

// Initialization Promise to prevent race conditions/double init
let initPromise: Promise<void> | null = null;

/**
 * Initialize Holster (Lazy Load)
 *
 * This function must be called and awaited before accessing any exported Holster objects.
 * It dynamically imports the Holster library to avoid Top-Level Await issues in Safari.
 */
export async function initHolster() {
	if (_holster) return; // Already initialized
	if (initPromise) return initPromise; // Initialization in progress

	initPromise = (async () => {
		try {
			console.log('[HOLSTER] 🔫 Initializing Holster (Lazy Load)...');
			// Dynamic import removes Top-Level Await from the main bundle entry
			const { default: Holster } = await import('@mblaney/holster/src/holster.js');

			_holster = Holster({
				peers: config.holster.peers,
				indexedDB: config.holster.indexedDB,
				file: config.holster.file
			});

			_holsterUser = _holster.user();
			console.log('[HOLSTER] ✅ Holster initialized!');
		} catch (err) {
			console.error('[HOLSTER] ❌ Failed to initialize Holster:', err);
			throw err;
		}
	})();

	return initPromise;
}

/**
 * Helper to create a Proxy that forwards calls to the lazy-loaded instance.
 * Throws an error if accessed before initHolster() completes.
 */
function createProxy<T extends object>(getter: () => T | undefined, name: string): T {
	return new Proxy({} as T, {
		get(_target, prop, _receiver) {
			const realInstance = getter();
			if (!realInstance) {
				throw new Error(
					`[HOLSTER] Accessing '${name}.${String(prop)}' before initialization. Call await initHolster() first.`
				);
			}
			const value = Reflect.get(realInstance, prop);
			return typeof value === 'function' ? value.bind(realInstance) : value;
		},
		set(_target, prop, value, _receiver) {
			const realInstance = getter();
			if (!realInstance) {
				throw new Error(
					`[HOLSTER] Setting '${name}.${String(prop)}' before initialization. Call await initHolster() first.`
				);
			}
			return Reflect.set(realInstance, prop, value);
		}
	});
}

/**
 * Helper to create a Proxy for a Holster Store (e.g., .get('key')).
 * Delays the .get() call until property access.
 */
function createStoreProxy(key: string) {
	return new Proxy(
		{},
		{
			get(_target, prop) {
				if (!_holster) {
					throw new Error(
						`[HOLSTER] Accessing store '${key}' before initialization. Call await initHolster() first.`
					);
				}
				// Call .get() on the real instance now
				const store = _holster.get(key);
				const value = Reflect.get(store, prop);
				return typeof value === 'function' ? value.bind(store) : value;
			}
		}
	);
}

// Export Proxies instead of direct instances
export const holster = createProxy(() => _holster, 'holster');
export const holsterUser = createProxy(() => _holsterUser, 'holsterUser');

// Export Proxied Stores
// These correspond to: holster.get('...')
export const holsterUsersList = createStoreProxy('freely-associating-players');
export const holsterOrganizationsList = createStoreProxy('freely-associating-organizations');

// ═══════════════════════════════════════════════════════════════════
// ERROR HANDLING
// ═══════════════════════════════════════════════════════════════════

export class NetworkError extends Error {
	constructor(message: string) {
		super(message);
		this.name = 'NetworkError';
	}
}

export class AuthError extends Error {
	constructor(message: string) {
		super(message);
		this.name = 'AuthError';
	}
}

export function isNetworkError(err: any): boolean {
	if (!err) return false;

	const errorMessage = typeof err === 'string' ? err : err.message || err.toString();
	const networkIndicators = [
		'timeout',
		'network',
		'connection',
		'offline',
		'unreachable',
		'failed to fetch',
		'net::',
		'cors',
		'no peers',
		'peer',
		'disconnect'
	];

	return networkIndicators.some((indicator) => errorMessage.toLowerCase().includes(indicator));
}

export function sleep(ms: number): Promise<void> {
	return new Promise((resolve) => setTimeout(resolve, ms));
}

// ═══════════════════════════════════════════════════════════════════
// AUTH STATE
// ═══════════════════════════════════════════════════════════════════

export interface AuthState {
	isAuthenticated: boolean;
	pub: string;
	alias: string;
}

export function getAuthState(): AuthState {
	// Accessing holsterUser via proxy is safe if we are sure initHolster() finished.
	// But getAuthState is often called in checks. We should be careful.
	// If NOT initialized, we should probably return empty state instead of throwing,
	// because `isAuthenticated()` check might happen early.

	if (!_holsterUser) {
		return {
			isAuthenticated: false,
			pub: '',
			alias: ''
		};
	}

	return {
		isAuthenticated: !!_holsterUser.is,
		pub: _holsterUser.is?.pub || '',
		alias: _holsterUser.is?.username || ''
	};
}

export function isAuthenticated(): boolean {
	if (!_holsterUser) return false;
	return !!_holsterUser.is;
}

// ═══════════════════════════════════════════════════════════════════
// PUBLIC NETWORK INITIALIZATION (Pre-Login)
// ═══════════════════════════════════════════════════════════════════

/**
 * Initialize public network data subscriptions (read-only)
 *
 * This enables browsing the network before logging in:
 * - Users list (freely-associating-players)
 * - Organizations list
 * - Public recognition trees
 *
 * These are READ-ONLY subscriptions. Writing/publishing still requires auth.
 * Safe to call multiple times (idempotent).
 */
export async function initializePublicNetworkData(): Promise<void> {
	// Ensure Holster is initialized first!
	await initHolster();

	console.log('[HOLSTER] 🌐 Initializing public network data (pre-login)...');

	try {
		// Initialize users list (read-only)
		const usersModule = await import('$lib/network/users.svelte');
		usersModule.initializeUsersList();
		console.log('[HOLSTER] ✅ Users list initialized (read-only)');

		// Initialize organizations list (read-only)
		const orgsModule = await import('$lib/network/organizations.svelte');
		orgsModule.initializeOrganizationsList();
		orgsModule.initializeOrganizations();
		console.log('[HOLSTER] ✅ Organizations initialized (read-only)');

		// Initialize public trees (read-only)
		const publicTreesModule = await import('$lib/network/public-trees.svelte');
		publicTreesModule.initializePublicTrees();
		console.log('[HOLSTER] ✅ Public trees initialized (read-only)');

		console.log('[HOLSTER] 🌐 Public network data ready for browsing!');
	} catch (error) {
		console.error('[HOLSTER] ❌ Failed to initialize public network data:', error);
	}
}

// ═══════════════════════════════════════════════════════════════════
// CORE AUTH OPERATIONS
// ═══════════════════════════════════════════════════════════════════

export interface AuthCallbacks {
	onSuccess?: (state: AuthState) => void;
	onError?: (error: Error) => void;
}

/**
 * Initialize stores after successful authentication
 */
async function initializeAfterAuth(callbacks?: AuthCallbacks): Promise<void> {
	try {
		// Update users list
		const authState = getAuthState();
		console.log(
			'[HOLSTER] Adding user to users list:',
			authState.alias,
			authState.pub.slice(0, 20) + '...'
		);

		// First, check the current state of the users list
		const currentUsersCount = await new Promise<number>((resolve) => {
			const checkCallback = (data: any) => {
				holster.get('freely-associating-players').off(checkCallback);
				if (data) {
					const userKeys = Object.keys(data).filter((key) => !key.startsWith('_'));
					console.log('[HOLSTER] Current users list has', userKeys.length, 'users');
					resolve(userKeys.length);
				} else {
					console.log('[HOLSTER] Users list is empty or not yet initialized');
					resolve(0);
				}
			};
			holster.get('freely-associating-players').on(checkCallback, true);
		});

		// Add/update our user entry using .next() to target only our pub key
		await new Promise<void>((resolve, reject) => {
			holster
				.get('freely-associating-players')
				.next(authState.pub)
				.put(
					{
						alias: authState.alias,
						lastSeen: Date.now()
					},
					(err: any) => {
						if (err) {
							console.error('[HOLSTER] ❌ Failed to add user to users list:', err);
							reject(new Error(`Failed to add user to users list: ${err}`));
						} else {
							console.log('[HOLSTER] ✅ User added to users list successfully');
							resolve();
						}
					}
				);
		});

		// Verify the list still has all users (plus potentially our new entry)
		await new Promise<void>((resolve) => {
			setTimeout(() => {
				const verifyCallback = (data: any) => {
					holster.get('freely-associating-players').off(verifyCallback);
					if (data) {
						const userKeys = Object.keys(data).filter((key) => !key.startsWith('_'));
						console.log('[HOLSTER] After adding user, list has', userKeys.length, 'users');
						if (userKeys.length >= currentUsersCount) {
							console.log('[HOLSTER] ✅ Verification passed: user list integrity maintained');
						} else {
							console.warn(
								'[HOLSTER] ⚠️  Warning: user count decreased from',
								currentUsersCount,
								'to',
								userKeys.length
							);
						}
					}
					resolve();
				};
				holster.get('freely-associating-players').on(verifyCallback, true);
			}, 500); // Wait briefly for the put operation to propagate
		});

		// Initialize data streams
		console.log('[HOLSTER] Initializing data streams...');
		const allocationModule = await import('$lib/protocol/stores/allocation.svelte');
		await allocationModule.initializeAllocationStores();

		const storesModule = await import('$lib/protocol/stores/stores.svelte');
		console.log('[HOLSTER] ✅ V5 stores initialized');

		// Initialize users list
		const usersModule = await import('$lib/network/users.svelte');
		usersModule.initializeUsersList();

		// Initialize contacts
		usersModule.initializeContacts();
		console.log('[HOLSTER] ✅ Contacts initialized');

		// Initialize organizations list
		const orgsModule = await import('$lib/network/organizations.svelte');
		orgsModule.initializeOrganizationsList();
		orgsModule.initializeOrganizations();
		console.log('[HOLSTER] ✅ Organizations initialized');

		// Initialize public trees list
		const publicTreesModule = await import('$lib/network/public-trees.svelte');
		publicTreesModule.initializePublicTrees();
		console.log('[HOLSTER] ✅ Public trees initialized');

		// Note: Membership is now handled by the unified entity/attribute system
		// No separate initialization needed - attributes auto-initialize

		// Initialize capacity subscriptions
		const capacitySubsModule = await import('$lib/network/capacity-subscriptions.svelte');
		capacitySubsModule.initializeCapacitySubscriptions();
		console.log('[HOLSTER] ✅ Capacity subscriptions initialized');

		// Initialize records
		const recordsModule = await import('$lib/network/records.svelte');
		recordsModule.initializeMyRecords();
		console.log('[HOLSTER] ✅ Records initialized');

		// Enable auto-subscription
		storesModule.enableAutoSubscriptionSync();
		console.log('[HOLSTER] ✅ Auto-subscription enabled');

		// Enable auto-composition
		storesModule.enableAutoCommitmentComposition();
		console.log('[HOLSTER] ✅ Auto-composition enabled');

		// Enable auto-membership sync
		storesModule.enableAutoMembershipSync();
		console.log('[HOLSTER] ✅ Auto-membership sync enabled');

		// Enable auto-capacity sync
		storesModule.enableAutoCapacitySync();
		console.log('[HOLSTER] ✅ Auto-capacity sync enabled');

		// Enable auto-need sync
		storesModule.enableAutoNeedSync();
		console.log('[HOLSTER] ✅ Auto-need sync enabled');

		callbacks?.onSuccess?.(authState);
	} catch (error) {
		console.error('[HOLSTER] Failed to initialize stores:', error);
		callbacks?.onError?.(error instanceof Error ? error : new Error(String(error)));
	}
}

export async function recall(callbacks?: AuthCallbacks): Promise<AuthState> {
	// Ensure initialized
	await initHolster();

	console.log('[HOLSTER RECALL] Checking authentication...');

	return new Promise((resolve) => {
		holsterUser.recall();

		if (holsterUser.is && holsterUser.is.username) {
			console.log('[HOLSTER RECALL] User authenticated:', holsterUser.is.username);
			const authState = getAuthState();

			initializeAfterAuth(callbacks).then(() => {
				resolve(authState);
			});
		} else {
			console.log('[HOLSTER RECALL] No authenticated user found');
			resolve({ isAuthenticated: false, pub: '', alias: '' });
		}
	});
}

export async function login(
	alias: string,
	password: string,
	callbacks?: AuthCallbacks
): Promise<AuthState> {
	// Ensure initialized
	await initHolster();

	console.log(`[HOLSTER LOGIN] Attempting login for alias: "${alias}"`);

	for (let attempt = 0; attempt < 3; attempt++) {
		try {
			await new Promise<void>((resolve, reject) => {
				console.log(`[HOLSTER LOGIN] Attempt ${attempt + 1} for alias: "${alias}"`);
				holsterUser.auth(alias, password, (err: any) => {
					if (err) {
						console.log(`[HOLSTER LOGIN] Auth failed for "${alias}":`, err);
						if (isNetworkError(err)) {
							reject(new NetworkError(err));
						} else {
							reject(new AuthError(err));
						}
					} else {
						console.log(`[HOLSTER LOGIN] Auth succeeded for "${alias}"`);
						resolve();
					}
				});
			});

			// Store credentials in sessionStorage (cleared when tab closes)
			holsterUser.store();

			// Get auth state
			const authState = getAuthState();

			// Initialize after successful auth
			await initializeAfterAuth(callbacks);

			return authState;
		} catch (error) {
			if (error instanceof AuthError) {
				console.log(`[HOLSTER LOGIN] AuthError - not retrying:`, (error as Error).message);
				callbacks?.onError?.(error as Error);
				throw error;
			}
			if (attempt === 2) {
				console.log(`[HOLSTER LOGIN] Final attempt failed:`, error);
				callbacks?.onError?.(error instanceof Error ? error : new Error(String(error)));
				throw error;
			}
			console.log(`[HOLSTER LOGIN] Retrying after network error, attempt ${attempt + 1}`);
			await sleep(1000 * Math.pow(2, attempt));
		}
	}

	throw new Error('Login failed after all retries');
}

export async function signup(
	alias: string,
	password: string,
	callbacks?: AuthCallbacks
): Promise<AuthState> {
	// Ensure initialized
	await initHolster();

	console.log(`[HOLSTER SIGNUP] Attempting signup for alias: "${alias}"`);

	for (let attempt = 0; attempt < 3; attempt++) {
		try {
			await new Promise<void>((resolve, reject) => {
				holsterUser.create(alias, password, (err: any) => {
					if (err) {
						if (isNetworkError(err)) {
							reject(new NetworkError(err));
						} else {
							reject(new AuthError(err));
						}
					} else {
						resolve();
					}
				});
			});

			// After successful creation, login
			return await login(alias, password, callbacks);
		} catch (error) {
			if (error instanceof AuthError) {
				callbacks?.onError?.(error as Error);
				throw error;
			}
			if (attempt === 2) {
				callbacks?.onError?.(error instanceof Error ? error : new Error(String(error)));
				throw error;
			}
			await sleep(1000 * Math.pow(2, attempt));
		}
	}

	throw new Error('Signup failed after all retries');
}

export async function signout(): Promise<void> {
	// Ensure initialized
	await initHolster();

	console.log('[HOLSTER SIGNOUT] Signing out...');

	// Cleanup users list
	try {
		const { cleanupUsersList, cleanupContacts } = await import('$lib/network/users.svelte');
		cleanupUsersList();
		cleanupContacts();
	} catch (error) {
		console.error('[HOLSTER SIGNOUT] Error cleaning up users list:', error);
	}

	// Cleanup public trees
	try {
		const { cleanupPublicTrees } = await import('$lib/network/public-trees.svelte');
		cleanupPublicTrees();
	} catch (error) {
		console.error('[HOLSTER SIGNOUT] Error cleaning up public trees:', error);
	}

	// Cleanup records
	try {
		const { cleanupRecords } = await import('$lib/network/records.svelte');
		cleanupRecords();
	} catch (error) {
		console.error('[HOLSTER SIGNOUT] Error cleaning up records:', error);
	}

	// Cleanup allocation stores (V5)
	try {
		const { cleanupAllocationStores } = await import('$lib/protocol/stores/allocation.svelte');
		await cleanupAllocationStores();
	} catch (error) {
		console.error('[HOLSTER SIGNOUT] Error cleaning up allocation stores:', error);
	}

	// Destroy session
	holsterUser.leave();

	// Re-initialize allocation stores in Demo Mode (Local Storage)
	try {
		const { initializeAllocationStores } = await import('$lib/protocol/stores/allocation.svelte');
		console.log('[HOLSTER SIGNOUT] Re-initializing stores for Demo Mode...');
		await initializeAllocationStores();
	} catch (error) {
		console.error('[HOLSTER SIGNOUT] Error re-initializing allocation stores:', error);
	}
}

export async function changePassword(currentPassword: string, newPassword: string): Promise<void> {
	// Ensure initialized
	await initHolster();

	const authState = getAuthState();
	if (!authState.isAuthenticated) {
		return Promise.reject(new Error('No authenticated user'));
	}

	return new Promise((resolve, reject) => {
		holsterUser.change(authState.alias, currentPassword, newPassword, (err: any) => {
			if (err) {
				reject(new Error(err));
			} else {
				resolve();
			}
		});
	});
}

// ═══════════════════════════════════════════════════════════════════
// TEST UTILITIES
// ═══════════════════════════════════════════════════════════════════

export interface MockAuthState {
	pub: string;
	alias: string;
}

let mockAuthState: MockAuthState | null = null;

export function mockAuth(pub: string, alias: string = 'test_user'): void {
	// This might fail if called before init in tests, but tests probably don't use the real lazy loader logic
	// or they should mock initHolster too.
	// For now, let's assume tests will await initHolster or we handle it.

	mockAuthState = { pub, alias };

	// We can't easily mock the proxy target if it's undefined.
	// We might need to manually set _holsterUser if it's null.
	if (!_holsterUser) {
		// Mock implementation? Or just wait for init?
		// For unit tests, we might want to expose a way to set _holsterUser directly.
		// But since we are exporting Proxies, we can just set the internal var.
		// But internal var is not exported.
	}
	if (_holsterUser) {
		// Override holsterUser.is for tests
		(_holsterUser as any).is = { pub, username: alias };
	}

	if (import.meta.env.VITEST) {
		console.log(`[HOLSTER] 🧪 Mock auth: ${alias} (${pub.slice(0, 20)}...)`);
	}
}

export function clearAuth(): void {
	mockAuthState = null;
	if (_holsterUser) {
		(_holsterUser as any).is = null;
	}
	if (import.meta.env.VITEST) {
		console.log('[HOLSTER] 🧪 Auth cleared');
	}
}

export function getMockAuthState(): MockAuthState | null {
	return mockAuthState;
}

// ═══════════════════════════════════════════════════════════════════
// DEBUGGING (Browser Only)
// ═══════════════════════════════════════════════════════════════════

if (typeof window !== 'undefined' && !import.meta.env.VITEST) {
	console.log('[HOLSTER] Peers:', config.holster.peers);
	console.log('[HOLSTER] IndexedDB:', config.holster.indexedDB);
	console.log('[HOLSTER] File:', config.holster.file);

	// Expose for debugging
	(window as any).holster = holster;
	(window as any).holsterUser = holsterUser;
	(window as any).initHolster = initHolster; // Expose init
	console.log('[HOLSTER] Exposed to window.holster and window.holsterUser for debugging');
}
