import Gun from 'gun/gun';
import SEA from 'gun/sea.js';
import 'gun/axe';
import 'gun/lib/radix';
import 'gun/lib/radisk';
import 'gun/lib/store';
import 'gun/lib/rindexed';
import 'gun/lib/webrtc';
import 'gun/lib/then';
import 'gun/lib/yson.js';

// Export subscription handler types directly from gunSetup
export type SubscriptionHandler<T> = (value: T) => void;
export type SubscriptionCleanup = () => void;

// Define types for Gun user
interface GunUser {
	is?: {
		pub?: string;
		alias?: string;
	};
	auth: (alias: string, pass: string, callback: (ack: any) => void) => void;
	create: (alias: string, pass: string, callback: (ack: any) => void) => void;
	recall: (options: { sessionStorage: boolean }) => void;
	leave: () => void;
	get: (path: string) => any;
}

// Initialize Gun once
export const gun = Gun({
	peers: [
		'http://localhost:8765/gun'
		//"https://gun-manhattan.herokuapp.com/gun", // Public relay peer for cross-device syncing
	],
	localStorage: false
});

// Get authenticated user space
export const user = gun.user() as GunUser;
user.recall({ sessionStorage: true });

// Type for Gun metadata - helps with GunNode.ts linter errors
export interface GunMeta {
	'#'?: string; // Soul reference
	'>'?: Record<string, number>; // State/timestamp for properties
	[key: string]: any;
}

// Type for Gun data with metadata
export interface GunData {
	_?: GunMeta;
	[key: string]: any;
}

// Types for certificate management
export interface Grantee {
	pub: string;
}

// SEA's Policy interface for writableSpaces
export interface IPolicy {
	'*'?: string; // Path starts with
	'+'?: string; // Path ends with
	'='?: string; // Path equals exactly
	'^'?: string; // Path matches regex
}

export type WritableSpace = string | IPolicy;

export interface SEAPair {
	pub: string; // Public key
	priv: string; // Private key
	epub: string; // Elliptic curve public key
	epriv: string; // Elliptic curve private key
}

// Permission types for certificate contexts
export enum CertificatePermission {
	READ = 'read',
	WRITE = 'write',
	BOTH = 'both'
}

/*
// Add data sync logging
gun.on('in', function(msg) {
  if (msg.put) {
    console.log('[GUN] Received data:', Object.keys(msg.put).length, 'nodes');
  }
  // @ts-ignore - accessing Gun's private API
  this.to.next(msg);
});

// Add a debounce mechanism for processing nulls
// Create a set to track recently seen null values
const recentNulls = new Set<string>();
const NULL_DEBOUNCE_TIME = 500; // ms

// Add a middleware to filter redundant null values
// @ts-ignore - accessing Gun's private API
gun.on('out', function (msg: any) {
	// Only process put messages
	if (msg.put) {
		// console.log('[GUN] Sending data:', Object.keys(msg.put).length, 'nodes');
		const souls = Object.keys(msg.put);

		// For each soul (node) in the message
		souls.forEach((soul) => {
			const node = msg.put[soul];

			// Check if this is a null/delete operation
			if (node === null || (node && Object.keys(node).length === 0)) {
				const key = soul;

				// Check if we've seen this null recently
				if (recentNulls.has(key)) {
					// console.log('[DEBUG Gun] Filtering redundant null for:', key);
					delete msg.put[soul]; // Remove from the message

					// If message is now empty, prevent it from being sent
					if (Object.keys(msg.put).length === 0) {
						return; // Skip sending the message
					}
				} else {
					// Add to recent nulls and schedule removal
					recentNulls.add(key);
					setTimeout(() => {
						recentNulls.delete(key);
					}, NULL_DEBOUNCE_TIME);
				}
			}
		});
	}

	// Pass the possibly modified message to the next middleware
	// @ts-ignore - accessing Gun's private API
	this.to.next(msg);
});
*/

// ===== USER AUTHENTICATION FUNCTIONS =====

// Add a helper to register a user in the users list
const registerUserInList = (userId: string, userName: string) => {
	if (!userId) return;

	console.log('Registering user in users list:', {
		userId,
		userName
	});

	// Add to users for discovery
	gun.get('users').get(userId).put({
		name: userName,
		online: true,
		lastSeen: Date.now()
	});
};

// Recall user session - returns a promise for async/await support
export const recallUser = (): Promise<void> => {
	return new Promise((resolve) => {
		// Already authenticated? Resolve immediately
		if (user?.is?.pub) {
			// Fetch and restore alias if needed
			if (user?.is?.alias === user?.is?.pub) {
				// Use non-null assertion since we've already checked user.is.pub exists
				user.get('alias').once((alias: string) => {
					if (alias && typeof alias === 'string' && user.is?.pub && alias !== user.is.pub) {
						// Update user.is.alias with the correct value
						if (user?.is) {
							user.is.alias = alias;
						}
						console.log('Restored correct alias:', alias);

						// Register in users list
						registerUserInList(user?.is?.pub!, alias);
					} else if (user?.is?.pub && user?.is?.alias) {
						// Use alias even if it's the same as pub
						registerUserInList(user?.is?.pub, user?.is?.alias);
					}
					resolve();
				});
				return;
			} else if (user?.is?.pub && user?.is?.alias) {
				// Register in users list with current alias
				registerUserInList(user?.is?.pub, user?.is?.alias);
			}
			resolve();
			return;
		}

		// Set up one-time auth handler
		const authHandler = () => {
			(gun as any).off('auth', authHandler);

			// Check if alias equals pub key, which indicates a potential issue
			if (user?.is?.alias === user?.is?.pub && user?.is?.pub) {
				// Use non-null assertion since we've already checked user.is.pub exists
				user.get('alias').once((alias: string) => {
					if (alias && typeof alias === 'string' && user?.is?.pub && alias !== user?.is?.pub) {
						// Update user.is.alias with the correct value
						if (user?.is) {
							user.is.alias = alias;
						}
						console.log('Restored correct alias:', alias);

						// Register in users list
						registerUserInList(user?.is?.pub!, alias);
					} else if (user?.is?.pub && user?.is?.alias) {
						// Use alias even if it's the same as pub
						registerUserInList(user?.is?.pub, user?.is?.alias);
					}
					resolve();
				});
			} else if (user?.is?.pub && user?.is?.alias) {
				// Register in users list with current alias
				registerUserInList(user?.is?.pub, user?.is?.alias);
				resolve();
			} else {
				resolve();
			}
		};

		gun.on('auth', authHandler);

		// Attempt recall
		user.recall({ sessionStorage: true });

		// Safety timeout
		setTimeout(() => {
			(gun as any).off('auth', authHandler);

			// Same alias check on timeout
			if (user.is?.pub && user.is?.alias === user.is?.pub) {
				// Use non-null assertion since we've already checked user.is.pub exists
				user.get('alias').once((alias: string) => {
					if (alias && typeof alias === 'string' && user.is?.pub && alias !== user.is.pub) {
						if (user.is) {
							user.is.alias = alias;
						}
						console.log('Restored correct alias on timeout:', alias);

						// Register in users list
						registerUserInList(user.is.pub!, alias);
					} else if (user.is?.pub && user.is?.alias) {
						// Use current alias as fallback
						registerUserInList(user.is.pub, user.is.alias);
					}
					resolve();
				});
			} else if (user.is?.pub && user.is?.alias) {
				// Register in users list with current alias
				registerUserInList(user.is.pub, user.is.alias);
				resolve();
			} else {
				resolve();
			}
		}, 1000);
	});
};

// Single authentication function that handles both login and signup
export const authenticate = async (alias: string, pass: string): Promise<void> => {
	return new Promise((resolve, reject) => {
		// First try to login
		user.auth(alias, pass, (ack: any) => {
			if (!ack.err) {
				console.log('Authenticated user:', alias);

				// Register authenticated user in users list
				if (user?.is?.pub) {
					registerUserInList(user.is.pub, alias);
				}

				resolve();
				return;
			}

			// If login fails, create and authenticate new user
			user.create(alias, pass, (createAck: any) => {
				if (createAck.err) {
					reject(new Error(createAck.err));
					return;
				}

				// After creation, authenticate
				user.auth(alias, pass, (authAck: any) => {
					if (authAck.err) {
						reject(new Error(authAck.err));
					} else {
						console.log('Authenticated user:', alias);

						// Register newly created user in users list
						if (user?.is?.pub) {
							registerUserInList(user?.is?.pub, alias);
						}

						resolve();
					}
				});
			});
		});
	});
};

// Clean logout that clears storage
export const logout = () => {
	// Mark user as offline in the users list before logging out
	if (user.is?.pub) {
		console.log('Marking user as offline:', user.is.pub);
		gun.get('users').get(user.is.pub).put({
			online: false,
			lastSeen: Date.now()
		});
	}

	user.leave();
	sessionStorage.clear();
	localStorage.clear();
};

// Create a node reference at a path - helper function to work with GunNode
export const getNodeRef = (path: string[]) => {
	let ref: any;
	if (user.is?.pub) {
		// user space
		ref = user as any;
		for (const segment of path) {
			ref = ref.get(segment);
		}
	} else {
		// public space
		//ref = gun as any;
		console.log('You must be logged in!');
	}
	return ref;
};

// Helper to get encrypted user paths
export const getPath = async (path: string) => {
	if (!user.is) return null;
	const pair = (user as any)._.sea; // Type assertion for internal Gun property
	const proof = await SEA.work(path, pair);
	return proof ? `~${user.is.pub}/${proof}` : null;
};

// ===== CERTIFICATE MANAGEMENT UTILITIES =====

/**
 * Create a certificate to give permission to a user for a specific path
 *
 * @param granteePub Public key of the user who will be granted access
 * @param paths Array of paths the user will have access to
 * @param permission Type of permission (read, write, or both)
 * @param expiration Optional expiration time in milliseconds
 * @returns Promise that resolves with the certificate string or null if creation failed
 */
export async function createCertificate(
	granteePub: string,
	paths: string[],
	permission: CertificatePermission = CertificatePermission.WRITE,
	expiration?: number
): Promise<string | null> {
	// Need to be authenticated to create certificates
	if (!user.is?.pub) {
		console.error('Error creating certificate: Not authenticated');
		return null;
	}

	try {
		// Get the authenticated user's key pair
		const issuerPair = (user as any)._.sea;
		if (!issuerPair) {
			console.error('Error creating certificate: No key pair found');
			return null;
		}

		// Create writable spaces according to permission pattern
		const writableSpaces = paths.map((path) => {
			return { '*': path };
		});

		// Create timestamp for expiration
		const certOptions: any = {};
		if (expiration) {
			certOptions.expiry = Date.now() + expiration;
		}

		// Use type casting to bypass TypeScript's strict checking
		// SEA.certify has complex types that are hard to match exactly
		const certificate = await (SEA.certify as any)(
			[{ pub: granteePub }], // grantees
			writableSpaces, // writable spaces
			issuerPair, // issuer's key pair
			certOptions // options
		);

		return certificate;
	} catch (error) {
		console.error('Error creating certificate:', error);
		return null;
	}
}

// Store a certificate in a standard location for reuse
export async function storeCertificate(
	name: string,
	certificate: string,
	granteePub: string
): Promise<boolean> {
	if (!user.is?.pub) {
		console.error('Error storing certificate: Not authenticated');
		return false;
	}

	try {
		// Store in the user's certificates collection
		await new Promise<void>((resolve, reject) => {
			// Cast user to any to access get method without type errors
			const certNode = (user as any).get('certificates').get(name);
			certNode.put(
				{
					certificate,
					grantee: granteePub,
					created: Date.now()
				},
				(ack: any) => {
					if (ack.err) {
						reject(new Error(ack.err));
					} else {
						resolve();
					}
				}
			);
		});

		return true;
	} catch (error) {
		console.error('Error storing certificate:', error);
		return false;
	}
}

// Retrieve a previously stored certificate
export async function getCertificate(name: string): Promise<string | null> {
	if (!user.is?.pub) {
		console.error('Error retrieving certificate: Not authenticated');
		return null;
	}

	try {
		// Retrieve from the user's certificates collection
		const certData = await new Promise<any>((resolve) => {
			// Cast user to any to access get method without type errors
			(user as any)
				.get('certificates')
				.get(name)
				.once((data: any) => {
					resolve(data);
				});
		});

		if (!certData || !certData.certificate) {
			return null;
		}

		return certData.certificate;
	} catch (error) {
		console.error('Error retrieving certificate:', error);
		return null;
	}
}

/**
 * Create preset certificates for common permission patterns
 */
export const certificatePresets = {
	/**
	 * Create a read-only certificate for a specific path
	 */
	readOnly: async (
		granteePub: string,
		path: string,
		expiration?: number
	): Promise<string | null> => {
		return createCertificate(granteePub, [path], CertificatePermission.READ, expiration);
	},

	/**
	 * Create a write-only certificate for a specific path
	 */
	writeOnly: async (
		granteePub: string,
		path: string,
		expiration?: number
	): Promise<string | null> => {
		return createCertificate(granteePub, [path], CertificatePermission.WRITE, expiration);
	},

	/**
	 * Create a full access certificate for a specific path
	 */
	fullAccess: async (
		granteePub: string,
		path: string,
		expiration?: number
	): Promise<string | null> => {
		return createCertificate(granteePub, [path], CertificatePermission.BOTH, expiration);
	},

	/**
	 * Create a temporary certificate that expires after the specified time
	 */
	temporary: async (
		granteePub: string,
		path: string,
		durationMs = 3600000
	): Promise<string | null> => {
		return createCertificate(granteePub, [path], CertificatePermission.WRITE, durationMs);
	}
};

/*
var SEA = Gun.SEA;
;(async () => {
var pair = await SEA.pair();
var enc = await SEA.encrypt('hello self', pair);
var data = await SEA.sign(enc, pair);
console.log(data);
var msg = await SEA.verify(data, pair.pub);
var dec = await SEA.decrypt(msg, pair);
var proof = await SEA.work(dec, pair);
var check = await SEA.work('hello self', pair);
console.log(dec);
console.log(proof === check);
// now let's share private data with someone:
var alice = await SEA.pair();
var bob = await SEA.pair();
var enc = await SEA.encrypt('shared data', await SEA.secret(bob.epub, alice));
await SEA.decrypt(enc, await SEA.secret(alice.epub, bob));
// `.secret` is Elliptic-curve Diffie–Hellman
// Bob allows Alice to write to part of his graph, he creates a certificate for Alice
var certificate = await SEA.certify(alice.pub, ["^AliceOnly.*"], bob)
// Alice logs in 
const gun = Gun();
await gun.user().auth(alice);
// and uses the certificate
await gun.get('~'+bob.pub).get('AliceOnly').get('do-not-tell-anyone').put(enc, null, {opt: {cert: certificate}})
await gun.get('~'+bob.pub).get('AliceOnly').get('do-not-tell-anyone').once(console.log) // return 'enc'
})();
*/
