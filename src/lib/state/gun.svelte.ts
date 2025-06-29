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
import { initializeUserDataSubscriptions } from './network.svelte';

if (typeof Gun.SEA === 'undefined') {
	Gun.SEA = SEA;
}

export const GUN = Gun;

// Database
export const gun = new Gun({
	peers: [
		//'http://localhost:8765/gun',
		'https://gun-manhattan.herokuapp.com/gun',
		'https://peer.wallie.io/gun',
		'https://gun.defucc.me/gun'
	],
	localStorage: false
	//radisk: false
});

// Authentication state store
export const isAuthenticating = writable(true);

export let user: any;

if (typeof window !== 'undefined') {
	user = gun.user().recall({ sessionStorage: true });
} else {
	user = { _: { sea: null }, is: null };
}

// SEA.throw = true (do not use this in production)

// Current User's username
export const username = writable('');
export const userpub = writable('');

export const usersList = gun.get('freely-associating-players');

// Cache for user names to avoid repeated Gun lookups
export const userNamesCache = writable<Record<string, string>>({});

// Store for tracking all user IDs
export const userIds = writable<string[]>([]);

export async function getUserName(userId: string) {
	// First check the reactive cache
	const cache = get(userNamesCache);
	if (cache[userId]) {
		return cache[userId];
	}

	// If not found, try the user's protected space
	try {
		const alias = gun
			.get(`~${userId}`)
			.get('alias')
			.once((data: any) => {
				return data;
			});
		if (alias && typeof alias === 'string') {
			// Update cache
			userNamesCache.update((cache) => ({
				...cache,
				[userId]: alias
			}));
			return alias;
		}
	} catch (error) {
		console.log(`[USER-NAME] Could not fetch alias for user ${userId}:`, error);
	}

	// Fallback to truncated ID
	const fallbackName = userId.substring(0, 8) + '...';
	return fallbackName;
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
						username.set(aliasToUse);
						userpub.set(pubToUse);
						usersList.get(pubToUse).put({
							name: aliasToUse,
							lastSeen: Date.now()
						});
					});
				});

				// Load the data since gun.on('auth') won't fire for recalled sessions
				initializeUserDataSubscriptions();
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
						username.set(aliasToUse);
						userpub.set(pubToUse);
						usersList.get(pubToUse).put({
							name: aliasToUse,
							lastSeen: Date.now()
						});
					});
				});

				// Load the data since gun.on('auth') won't fire for recalled sessions
				initializeUserDataSubscriptions();
			} else {
				console.log('[RECALL] No authenticated user found after recall');
				// Clear the stores to ensure consistent state
				username.set('');
				userpub.set('');
			}
		} catch (error) {
			console.error('[RECALL] Error during authentication check:', error);
			// Clear the stores on error
			username.set('');
			userpub.set('');
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
					username.set(aliasToUse);
					userpub.set(pubToUse);
					usersList.get(pubToUse).put({
						name: aliasToUse,
						lastSeen: Date.now()
					});

					console.log(`signed in as ${aliasToUse}`);
					console.log(`userPub: ${pubToUse}`);

					// Load existing user data
					initializeUserDataSubscriptions();
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
					username.set(aliasToUse);
					userpub.set(pubToUse);
					usersList.get(pubToUse).put({
						name: aliasToUse,
						lastSeen: Date.now()
					});

					console.log(`signed in as ${aliasToUse} (fallback)`);
					console.log(`userPub: ${pubToUse} (fallback)`);

					// Load existing user data
					initializeUserDataSubscriptions();
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
		username.set('');
		userpub.set('');
	} finally {
		// Clear authenticating state
		isAuthenticating.set(false);
	}
});

export function login(username: string, password: string) {
	user.auth(username, password, ({ err }: { err: any }) => err && alert(err));
}

export function signup(username: string, password: string) {
	gun.get(`~@${username}`).once(
		(data: any) => {
			if (data) {
				console.log('[SIGNUP] checking alias user data', data);
				alert('Username already taken');
				return;
			}

			user.create(username, password, ({ err }: { err: any }) => {
				if (err) {
					alert(err);
				} else {
					login(username, password);
				}
			});
		},
		{ wait: 2000 }
	);
}

export async function signout() {
	user.leave();
	while (user._.sea != null) {
		await new Promise(requestAnimationFrame);
	}
	username.set('');
	userpub.set('');
}

// Monitoring: Check connectivity with gun.back('opt.peers') and WebRTC stats:
export async function monitorWebRTC(pc: any) {
	const report = await pc.getStats();
	for (let dict of report.values()) {
		console.log(`${dict.type}: id=${dict.id}, timestamp=${dict.timestamp}`);
	}
}
