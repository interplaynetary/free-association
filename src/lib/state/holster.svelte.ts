import Holster from '@mblaney/holster/src/holster.js';
import { writable, get } from 'svelte/store';
import { initializeHolsterDataStreams } from './network.svelte';
import { config } from '../config';

// Initialize Holster - now uses config from environment variables
export const holster = Holster({
	peers: config.holster.peers,
	indexedDB: config.holster.indexedDB,
	file: config.holster.file
});

// Authentication state store
export const isHolsterAuthenticating = writable(true);

// Initialize Holster user immediately to avoid reference errors
// Named holsterUser to distinguish from Gun's user export
export const holsterUser = holster.user();

// Current User's alias and pub for Holster
export const holsterUserAlias = writable('');
export const holsterUserPub = writable('');

// Users list using Holster API
// Named holsterUsersList to distinguish from Gun's usersList export
export const holsterUsersList = holster.get('freely-associating-players');

// Check if user is already authenticated after recall
if (typeof window !== 'undefined') {
	const checkAuth = async () => {
		try {
			// Set authenticating state
			isHolsterAuthenticating.set(true);

			// Use Holster's recall method
			await new Promise((resolve) => {
				holsterUser.recall();
				// Check if user is authenticated
				console.log('[HOLSTER RECALL] holsterUser.is:', holsterUser.is);
				if (holsterUser.is && holsterUser.is.username) {
					console.log('[HOLSTER RECALL] User authenticated:', holsterUser.is.username);
					console.log('[HOLSTER RECALL] Setting stores - username:', holsterUser.is.username, 'pub:', holsterUser.is.pub);
					holsterUserAlias.set(holsterUser.is.username);
					holsterUserPub.set(holsterUser.is.pub);
					console.log('[HOLSTER RECALL] Stores set successfully');

					// Update users list
					holster.get('freely-associating-players').next(holsterUser.is.pub).put({
						alias: holsterUser.is.username,
						lastSeen: Date.now()
					});

					// Initialize Holster data streams (Phase 10 function)
					console.log('[HOLSTER RECALL] Initializing Holster data streams...');
					initializeHolsterDataStreams();
				} else {
					console.log('[HOLSTER RECALL] No authenticated user found');
					holsterUserAlias.set('');
					holsterUserPub.set('');
				}
				resolve(null);
			});
		} catch (error) {
			console.error('[HOLSTER RECALL] Error during authentication check:', error);
			holsterUserAlias.set('');
			holsterUserPub.set('');
		} finally {
			isHolsterAuthenticating.set(false);
		}
	};

	// Start the authentication check
	checkAuth();
}

// Custom error classes for better error handling
class NetworkError extends Error {
	constructor(message: string) {
		super(message);
		this.name = 'NetworkError';
	}
}

class AuthError extends Error {
	constructor(message: string) {
		super(message);
		this.name = 'AuthError';
	}
}

// Helper function to distinguish network errors from auth errors
function isNetworkError(err: any): boolean {
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

// Helper function for exponential backoff delay
function sleep(ms: number): Promise<void> {
	return new Promise((resolve) => setTimeout(resolve, ms));
}

export async function login(alias: string, password: string): Promise<void> {
	console.log(`[HOLSTER LOGIN] Attempting login for alias: "${alias}"`);

	for (let attempt = 0; attempt < 3; attempt++) {
		try {
			await new Promise<void>((resolve, reject) => {
				console.log(`[HOLSTER LOGIN] Attempt ${attempt + 1} for alias: "${alias}"`);
				holsterUser.auth(alias, password, (err: any) => {
					if (err) {
						console.log(`[HOLSTER LOGIN] Auth failed for "${alias}":`, err);
						// Retry on network errors, fail immediately on auth errors
						if (isNetworkError(err)) {
							reject(new NetworkError(err));
						} else {
							reject(new AuthError(err));
						}
					} else {
						console.log(`[HOLSTER LOGIN] Auth succeeded for "${alias}"`);

						// Update stores
						holsterUserAlias.set(holsterUser.is.username);
						holsterUserPub.set(holsterUser.is.pub);

						// Update users list
						holster.get('freely-associating-players').next(holsterUser.is.pub).put({
							alias: holsterUser.is.username,
							lastSeen: Date.now()
						});

						// Store credentials in localStorage for cross-tab access
						holsterUser.store(true);

						// Initialize Holster data streams (Phase 10 function)
						console.log('[HOLSTER LOGIN] Initializing Holster data streams...');
						initializeHolsterDataStreams();

						resolve();
					}
				});
			});
			return; // Success
		} catch (error) {
			if (error instanceof AuthError) {
				// Don't retry auth failures, throw error immediately for UI to handle
				console.log(`[HOLSTER LOGIN] AuthError - not retrying:`, error.message);
				throw error;
			}
			if (attempt === 2) {
				// Final attempt failed, throw error for UI to handle
				console.log(`[HOLSTER LOGIN] Final attempt failed:`, error);
				throw error;
			}
			// Wait before retrying with exponential backoff
			console.log(`[HOLSTER LOGIN] Retrying after network error, attempt ${attempt + 1}`);
			await sleep(1000 * Math.pow(2, attempt));
		}
	}
}

export async function signup(alias: string, password: string): Promise<void> {
	// Attempt to create user with retry logic
	for (let attempt = 0; attempt < 3; attempt++) {
		try {
			await new Promise<void>((resolve, reject) => {
				holsterUser.create(alias, password, (err: any) => {
					if (err) {
						// Retry on network errors, fail immediately on auth errors
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

			// If user creation succeeds, attempt login
			await login(alias, password);
			return; // Success
		} catch (error) {
			if (error instanceof AuthError) {
				// Don't retry auth failures, throw error immediately for UI to handle
				throw error;
			}
			if (attempt === 2) {
				// Final attempt failed, throw error for UI to handle
				throw error;
			}
			// Wait before retrying with exponential backoff
			await sleep(1000 * Math.pow(2, attempt));
		}
	}
}

/**
 * Cleanup all Holster subscriptions
 * Call this on logout to prevent memory leaks
 */
export function cleanupAllHolsterSubscriptions() {
	console.log('[HOLSTER] Cleaning up all subscriptions...');

	// Import cleanup functions dynamically to avoid circular dependencies
	return Promise.all([
		import('./contacts-holster.svelte').then(m => m.cleanupHolsterContacts()),
		import('./capacities-holster.svelte').then(m => m.cleanupHolsterCapacities()),
		import('./tree-holster.svelte').then(m => m.cleanupHolsterTree()),
		import('./recognition-holster.svelte').then(m => m.cleanupHolsterSogf()),
		import('./chat-holster.svelte').then(m => m.cleanupHolsterChat())
	]).then(() => {
		console.log('[HOLSTER] All subscriptions cleaned up');
	}).catch(err => {
		console.error('[HOLSTER] Error during cleanup:', err);
	});
}

export async function signout() {
	// Clean up all Holster subscriptions BEFORE leaving (while still authenticated)
	await cleanupAllHolsterSubscriptions();

	// Now safely destroy the session
	holsterUser.leave();
	holsterUserAlias.set('');
	holsterUserPub.set('');
}

export function changePassword(currentPassword: string, newPassword: string) {
	const currentAlias = get(holsterUserAlias);
	if (!currentAlias) {
		throw new Error('No authenticated user');
	}

	holsterUser.change(currentAlias, currentPassword, newPassword, (err: any) => {
		if (err) {
			throw new Error(err);
		}
	});
}

// Log initialization
console.log('[HOLSTER] Initialized successfully');
console.log('[HOLSTER] Peers:', config.holster.peers);
console.log('[HOLSTER] IndexedDB:', config.holster.indexedDB);
console.log('[HOLSTER] File:', config.holster.file);

// Expose for debugging (browser only)
if (typeof window !== 'undefined') {
	(window as any).holster = holster;
	(window as any).holsterUser = holsterUser;
	console.log('[HOLSTER] Exposed to window.holster and window.holsterUser for debugging');
}

// Initialize test utilities in development mode
if (import.meta.env.DEV && typeof window !== 'undefined') {
	console.log('[HOLSTER] Loading test utilities...');
	import('./holster-tests')
		.then((module) => {
			module.initializeHolsterTests();
		})
		.catch((err) => {
			console.error('[HOLSTER] Failed to load test utilities:', err);
		});
}
