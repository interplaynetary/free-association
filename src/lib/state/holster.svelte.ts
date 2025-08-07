import Holster from '@mblaney/holster/src/holster.js';
import { writable, get } from 'svelte/store';
import { initializeUserDataStreams } from './network.svelte';

// Initialize Holster with similar configuration to Gun
export const holster = Holster({
	peers: ['wss://holster.haza.website'],
	indexedDB: true,
	secure: true // Enable security for signed updates
});

// Authentication state store
export const isAuthenticating = writable(true);

// Initialize user immediately to avoid reference errors
export const user = holster.user();

// Current User's alias and pub
export const userAlias = writable('');
export const userPub = writable('');

// Users list using Holster API
export const usersList = holster.get('freely-associating-players');

// Check if user is already authenticated after recall
if (typeof window !== 'undefined') {
	const checkAuth = async () => {
		try {
			// Set authenticating state
			isAuthenticating.set(true);

			// Use Holster's recall method
			await new Promise((resolve) => {
				user.recall();
				// Check if user is authenticated
				if (user.is && user.is.username) {
					console.log('[HOLSTER RECALL] User authenticated:', user.is.username);
					userAlias.set(user.is.username);
					userPub.set(user.is.pub);

					// Update users list
					holster.get('freely-associating-players').get(user.is.pub).put({
						alias: user.is.username,
						lastSeen: Date.now()
					});

					// Load the data streams
					initializeUserDataStreams();
				} else {
					console.log('[HOLSTER RECALL] No authenticated user found');
					userAlias.set('');
					userPub.set('');
				}
				resolve(null);
			});
		} catch (error) {
			console.error('[HOLSTER RECALL] Error during authentication check:', error);
			userAlias.set('');
			userPub.set('');
		} finally {
			isAuthenticating.set(false);
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
				user.auth(alias, password, (err: any) => {
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
						userAlias.set(user.is.username);
						userPub.set(user.is.pub);

						// Update users list
						holster.get('freely-associating-players').get(user.is.pub).put({
							alias: user.is.username,
							lastSeen: Date.now()
						});

						// Store credentials
						user.store(false); // Use sessionStorage by default

						// Load existing user data
						initializeUserDataStreams();

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
				user.create(alias, password, (err: any) => {
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

export async function signout() {
	user.leave();
	userAlias.set('');
	userPub.set('');
}

export function changePassword(currentPassword: string, newPassword: string) {
	const currentAlias = get(userAlias);
	if (!currentAlias) {
		throw new Error('No authenticated user');
	}

	user.change(currentAlias, currentPassword, newPassword, (err: any) => {
		if (err) {
			throw new Error(err);
		}
	});
}

// Export SEA functionality
export const SEA = holster.SEA;

// Export the Holster instance for direct access
export { holster as HOLSTER };
