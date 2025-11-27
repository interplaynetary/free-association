import Holster from '@mblaney/holster/src/holster.js';
import type { HolsterInstance, HolsterUser } from '@mblaney/holster';
import { config } from '$lib/protocol/config';

// ═══════════════════════════════════════════════════════════════════
// HOLSTER INITIALIZATION
// ═══════════════════════════════════════════════════════════════════

export const holster: HolsterInstance = Holster({
	peers: config.holster.peers,
	indexedDB: config.holster.indexedDB,
	file: config.holster.file
});

export const holsterUser: HolsterUser = holster.user();
export const holsterUsersList = holster.get('freely-associating-players');
export const holsterOrganizationsList = holster.get('freely-associating-organizations');

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
	return {
		isAuthenticated: holsterUser.is ? true : false,
		pub: holsterUser.is?.pub || '',
		alias: holsterUser.is?.username || ''
	};
}

export function isAuthenticated(): boolean {
	return holsterUser.is ? true : false;
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
		holster.get('freely-associating-players').next(authState.pub).put({
			alias: authState.alias,
			lastSeen: Date.now()
		});

		// Initialize data streams
		console.log('[HOLSTER] Initializing data streams...');
		const storesModule = await import('$lib/protocol/stores.svelte');
		storesModule.initializeAllocationStores();
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

export async function login(alias: string, password: string, callbacks?: AuthCallbacks): Promise<AuthState> {
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

export async function signup(alias: string, password: string, callbacks?: AuthCallbacks): Promise<AuthState> {
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
	console.log('[HOLSTER SIGNOUT] Signing out...');

	// Cleanup users list
	try {
		const { cleanupUsersList, cleanupContacts } = await import('$lib/network/users.svelte');
		cleanupUsersList();
		cleanupContacts();
	} catch (error) {
		console.error('[HOLSTER SIGNOUT] Error cleaning up users list:', error);
	}

	// Cleanup records
	try {
		const { cleanupRecords } = await import('$lib/network/records.svelte');
		cleanupRecords();
	} catch (error) {
		console.error('[HOLSTER SIGNOUT] Error cleaning up records:', error);
	}

	// Destroy session
	holsterUser.leave();
}

export function changePassword(currentPassword: string, newPassword: string): Promise<void> {
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
	mockAuthState = { pub, alias };
	// Override holsterUser.is for tests
	(holsterUser as any).is = { pub, username: alias };
	if (import.meta.env.VITEST) {
		console.log(`[HOLSTER] 🧪 Mock auth: ${alias} (${pub.slice(0, 20)}...)`);
	}
}

export function clearAuth(): void {
	mockAuthState = null;
	(holsterUser as any).is = null;
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
	console.log('[HOLSTER] Exposed to window.holster and window.holsterUser for debugging');
}

