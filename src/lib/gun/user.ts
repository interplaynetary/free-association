import Gun from 'gun';
import 'gun/sea';
import 'gun/axe';
import { writable } from 'svelte/store';

// TODO:
// Modify Header to work with login/signup instead of auth
// Modify globalState to work with the writeables from here instead of creating its own redundant globalState methods for this stuff.
// upgrade calculations with refined model from haskell

// Database
export const gun = Gun({
	peers: [
		'http://localhost:8765/gun'
		//"https://gun-manhattan.herokuapp.com/gun", // Public relay peer for cross-device syncing
	],
	localStorage: false
});

export const usersList = gun.get('users');

export let user = gun.user().recall({ sessionStorage: true });

// Current User's username
export const username = writable('');
export const userpub = writable('');

user.get('alias').on((v: string) => username.set(v));

gun.on('auth', async () => {
	const alias = await user.get('alias'); // username string
	username.set(alias);
	userpub.set(user.is?.pub);
	usersList.get(user.is?.pub).put({
		name: alias,
		lastSeen: Date.now()
	});

	console.log(`signed in as ${alias}`);
});

export function login(username: string, password: string) {
	user.auth(username, password, ({ err }: { err: any }) => err && alert(err));
}

export function signup(username: string, password: string) {
	user.create(username, password, ({ err }: { err: any }) => {
		if (err) {
			alert(err);
		} else {
			login(username, password);
		}
	});
}

export function signout() {
	user.leave();
	username.set('');
	userpub.set('');
}

export const getNodeRef = (path: string[]) => {
	let ref: any;

	// Safety check for empty path
	if (!path || !Array.isArray(path) || path.length === 0) {
		console.warn('getNodeRef called with invalid path:', path);
		return null;
	}

	if (user.is?.pub) {
		// user space
		ref = user as any;

		// Safely traverse the path
		for (const segment of path) {
			// Ensure segment is not null or undefined
			if (segment === null || segment === undefined) {
				console.warn('Invalid path segment (null/undefined) in path:', path);
				return null;
			}

			// If ref is already null/undefined, don't try to get further
			if (!ref) {
				console.warn('getNodeRef reference is null during traversal');
				return null;
			}

			ref = ref.get(String(segment)); // Ensure segment is a string
		}

		return ref;
	} else {
		// Not authenticated
		console.log('You must be logged in to access user space data!');
		return null;
	}
};

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

export interface UserData {
	pub: string;
	alias: string;
	username?: string;
}

// Export subscription handler types directly from gunSetup
export type SubscriptionHandler<T> = (value: T) => void;
export type SubscriptionCleanup = () => void;
