import Gun from 'gun';
import SEA from 'gun/sea';
import 'gun/axe';
import 'gun/lib/yson.js';
import 'gun/lib/radix'; // Required for radix tree logic
import 'gun/lib/radisk'; // Required for chunked disk storage
import 'gun/lib/store'; // Bridges GUN storage adapter logic
import 'gun/lib/rindexed'; // IndexedDB adapter for RAD in browser
import 'gun/lib/webrtc.js';

import { writable, get } from 'svelte/store';
import { initializeUserDataStreams } from './network.svelte';
import { config } from '../config';

if (typeof Gun.SEA === 'undefined') {
	Gun.SEA = SEA;
}

export const GUN = Gun;

// Database - now uses config from environment variables
export const gun = new Gun({
    peers: config.gun.peers,
    localStorage: config.gun.localStorage,
    radisk: config.gun.radisk
});

// Authentication state store
export const isAuthenticating = writable(true);

// Initialize user immediately to avoid reference errors
export const user =
	typeof window !== 'undefined'
		? gun.user().recall({ sessionStorage: true })
		: { _: { sea: null }, is: null };

// SEA.throw = true (do not use this in production)

// Current User's alias
export const userAlias = writable('');
export const userPub = writable('');

export const usersList = gun.get('freely-associating-players');

// Add debugging to usersList reference
console.log('[USERS-DEBUG] usersList initialized:', usersList);

// Initialize usersList subscription immediately when module loads
if (typeof window !== 'undefined') {
	console.log('[USERS-DEBUG] Setting up usersList subscription in browser environment');
	// Import and setup usersList subscription FIRST, before any auth checks
	import('./network.svelte').then(({ setupUsersListSubscription }) => {
		console.log('[USERS-DEBUG] About to call setupUsersListSubscription');
		setupUsersListSubscription();
		console.log('[USERS] Early usersList subscription initialized');
	});
}

// Check if user is already authenticated after recall
if (typeof window !== 'undefined') {
	const checkAuth = async () => {
		try {
			// Set authenticating state
			isAuthenticating.set(true);

			// Wait for a small delay to allow recall to complete
			await new Promise((resolve) => setTimeout(resolve, 100));

			// Check for SEA keys first as it's the most reliable way to verify authentication
			if (user._.sea && user.is?.alias) {
				console.log('[RECALL] User authenticated via SEA check:', user.is.alias);

				// Get the actual alias and pub from Gun storage instead of trusting user.is
				user.get('alias').once((actualAlias: any) => {
					user.get('pub').once((actualPub: any) => {
						const aliasToUse = actualAlias || user.is.alias;
						const pubToUse = actualPub || user.is.pub;
						console.log('[RECALL] Using alias from storage:', aliasToUse);
						console.log('[RECALL] Using pub from storage:', pubToUse?.slice(0, 20) + '...');
						userAlias.set(aliasToUse);
						userPub.set(pubToUse);
						console.log('[USERS-DEBUG] [RECALL] Adding user to usersList:', {
							pubKey: pubToUse?.slice(0, 20) + '...',
							alias: aliasToUse,
							timestamp: Date.now()
						});
						usersList.get(pubToUse).put(
							{
								alias: aliasToUse,
								lastSeen: Date.now()
							},
							(ack: any) => {
								if (ack.err) {
									console.error('[USERS-DEBUG] [RECALL] Error writing to usersList:', ack.err);
								} else {
									console.log('[USERS-DEBUG] [RECALL] Successfully wrote to usersList');
								}
							}
						);
					});
				});

				// Load the data since gun.on('auth') won't fire for recalled sessions
				initializeUserDataStreams();
				return;
			}

			// Fallback check for user.is.pub if SEA check fails
			if (user?.is?.pub && user.is?.alias) {
				console.log('[RECALL] User authenticated via pub check:', user.is.alias);

				// Get the actual alias and pub from Gun storage instead of trusting user.is
				user.get('alias').once((actualAlias: any) => {
					user.get('pub').once((actualPub: any) => {
						const aliasToUse = actualAlias || user.is.alias;
						const pubToUse = actualPub || user.is.pub;
						console.log('[RECALL] Using alias from storage (pub fallback):', aliasToUse);
						console.log(
							'[RECALL] Using pub from storage (pub fallback):',
							pubToUse?.slice(0, 20) + '...'
						);
						userAlias.set(aliasToUse);
						userPub.set(pubToUse);
						console.log('[USERS-DEBUG] [RECALL-FALLBACK] Adding user to usersList:', {
							pubKey: pubToUse?.slice(0, 20) + '...',
							alias: aliasToUse,
							timestamp: Date.now()
						});
						usersList.get(pubToUse).put({
							alias: aliasToUse,
							lastSeen: Date.now()
						});
					});
				});

				// Load the data since gun.on('auth') won't fire for recalled sessions
				initializeUserDataStreams();
			} else {
				console.log('[RECALL] No authenticated user found after recall');
				// Clear the stores to ensure consistent state
				userAlias.set('');
				userPub.set('');
			}
		} catch (error) {
			console.error('[RECALL] Error during authentication check:', error);
			// Clear the stores on error
			userAlias.set('');
			userPub.set('');
		} finally {
			// Clear authenticating state
			isAuthenticating.set(false);
		}
	};

	// Start the authentication check
	checkAuth();
}

gun.on('auth', async () => {
	try {
		// Set authenticating state
		isAuthenticating.set(true);

		// Check for SEA keys first as it's the most reliable way to verify authentication
		if (user._.sea) {
			console.log('[AUTH] User authenticated via SEA check:', user.is.alias);

			// Get the actual alias and pub from Gun storage instead of trusting user.is
			user.get('alias').once((actualAlias: any) => {
				user.get('pub').once((actualPub: any) => {
					const aliasToUse = actualAlias || user.is.alias;
					const pubToUse = actualPub || user.is.pub;
					console.log('[AUTH] Using alias from storage:', aliasToUse);
					console.log('[AUTH] Using pub from storage:', pubToUse?.slice(0, 20) + '...');
					userAlias.set(aliasToUse);
					userPub.set(pubToUse);
					console.log('[USERS-DEBUG] [AUTH] Adding user to usersList:', {
						pubKey: pubToUse?.slice(0, 20) + '...',
						alias: aliasToUse,
						timestamp: Date.now()
					});
					usersList.get(pubToUse).put(
						{
							alias: aliasToUse,
							lastSeen: Date.now()
						},
						(ack: any) => {
							if (ack.err) {
								console.error('[USERS-DEBUG] [AUTH] Error writing to usersList:', ack.err);
							} else {
								console.log('[USERS-DEBUG] [AUTH] Successfully wrote to usersList');
							}
						}
					);

					console.log(`signed in as ${aliasToUse}`);
					console.log(`userPub: ${pubToUse}`);

					// Load existing user data
					initializeUserDataStreams();
				});
			});

			// Remove these unreliable console.log statements
			// console.log(`signed in as ${user.is.alias}`);
			// console.log(`userPub: ${user.is?.pub}`);

			// Don't call manifest() here anymore - it's called inside the callback above
			return;
		}

		if (user.is.alias) {
			console.log('[AUTH] User authenticated via alias check:', user.is.alias);

			// Get the actual alias and pub from Gun storage instead of trusting user.is
			user.get('alias').once((actualAlias: any) => {
				user.get('pub').once((actualPub: any) => {
					const aliasToUse = actualAlias || user.is.alias;
					const pubToUse = actualPub || user.is.pub;
					console.log('[AUTH] Using alias from storage (fallback):', aliasToUse);
					console.log('[AUTH] Using pub from storage (fallback):', pubToUse?.slice(0, 20) + '...');
					userAlias.set(aliasToUse);
					userPub.set(pubToUse);
					console.log('[USERS-DEBUG] [AUTH-FALLBACK] Adding user to usersList:', {
						pubKey: pubToUse?.slice(0, 20) + '...',
						alias: aliasToUse,
						timestamp: Date.now()
					});
					usersList.get(pubToUse).put({
						alias: aliasToUse,
						lastSeen: Date.now()
					});

					console.log(`signed in as ${aliasToUse} (fallback)`);
					console.log(`userPub: ${pubToUse} (fallback)`);

					// Load existing user data
					initializeUserDataStreams();
				});
			});

			// Remove these unreliable console.log statements
			// console.log(`signed in as ${user.is.alias}`);
			// console.log(`userPub: ${user.is?.pub}`);

			// Don't call initializeUserDataSubscriptions() here anymore - it's called inside the callback above
			return;
		} else {
			throw new Error('Authentication failed - no alias found');
		}
	} catch (error) {
		console.error('[AUTH] Error during authentication:', error);
		// Clear the stores on error
		userAlias.set('');
		userPub.set('');
	} finally {
		// Clear authenticating state
		isAuthenticating.set(false);
	}
});

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
	console.log(`[LOGIN] Attempting login for alias: "${alias}"`);

	for (let attempt = 0; attempt < 3; attempt++) {
		try {
			await new Promise<void>((resolve, reject) => {
				console.log(`[LOGIN] Attempt ${attempt + 1} for alias: "${alias}"`);
				user.auth(alias, password, ({ err }: { err: any }) => {
					if (err) {
						console.log(`[LOGIN] Auth failed for "${alias}":`, err);
						// Retry on network errors, fail immediately on auth errors
						if (isNetworkError(err)) {
							reject(new NetworkError(err));
						} else {
							reject(new AuthError(err));
						}
					} else {
						console.log(`[LOGIN] Auth succeeded for "${alias}"`);
						resolve();
					}
				});
			});
			return; // Success
		} catch (error) {
			if (error instanceof AuthError) {
				// Don't retry auth failures, throw error immediately for UI to handle
				console.log(`[LOGIN] AuthError - not retrying:`, error.message);
				throw error;
			}
			if (attempt === 2) {
				// Final attempt failed, throw error for UI to handle
				console.log(`[LOGIN] Final attempt failed:`, error);
				throw error;
			}
			// Wait before retrying with exponential backoff
			console.log(`[LOGIN] Retrying after network error, attempt ${attempt + 1}`);
			await sleep(1000 * Math.pow(2, attempt));
		}
	}
}

export async function signup(alias: string, password: string): Promise<void> {
	// First check if alias exists
	const aliasExists = await new Promise<boolean>((resolve) => {
		gun.get(`~@${alias}`).once(
			(data: any) => {
				resolve(!!data);
			},
			{ wait: 2000 }
		);
	});

	if (aliasExists) {
		throw new AuthError('Alias already taken');
	}

	// Attempt to create user with retry logic
	for (let attempt = 0; attempt < 3; attempt++) {
		try {
			await new Promise<void>((resolve, reject) => {
				user.create(alias, password, ({ err }: { err: any }) => {
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
	while (user._.sea != null) {
		await new Promise(requestAnimationFrame);
	}
	userAlias.set('');
	userPub.set('');
}

export function changePassword(currentPassword: string, newPassword: string) {
	const currentAlias = get(userAlias);
	if (!currentAlias) {
		throw new Error('No authenticated user');
	}

	user.auth(
		currentAlias,
		currentPassword,
		({ err }: { err: any }) => {
			if (err) {
				throw new Error(err);
			}
		},
		{ change: newPassword }
	);
}

// Monitoring: Check connectivity with gun.back('opt.peers') and WebRTC stats:
export async function monitorWebRTC(pc: any) {
	const report = await pc.getStats();
	for (let dict of report.values()) {
		console.log(`${dict.type}: id=${dict.id}, timestamp=${dict.timestamp}`);
	}
}

// Expose gun and user to window for debugging
if (typeof window !== 'undefined') {
	(window as any).gun = gun;
	(window as any).user = user;
	console.log('[DEBUG] gun and user exposed to window for debugging');
}
