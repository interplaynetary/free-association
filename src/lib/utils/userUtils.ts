import { gun } from './gun/gunSetup';

// Simple in-memory cache for user names
export const usersMap = new Map<string, string>();

// Get raw users reference from Gun
const usersRef = gun.get('users');

/**
 * Update user profile in Gun
 */
export function updateUserProfile(userId: string, name: string): void {
	if (!userId) return;

	// console.log('Updating user profile:', { userId, name });

	// Update cache immediately for responsive UI
	updateUserName(userId, name);

	// Update in Gun
	usersRef.get(userId).put({
		name: name,
		lastSeen: Date.now()
	});
}

/**
 * Helper to update user name in cache
 */
function updateUserName(userId: string, name: string): void {
	// Skip if no change
	if (usersMap.get(userId) === name) return;

	// Update the map
	usersMap.set(userId, name);

	// Notify listeners
	notifyUserMapChange();
}

/**
 * Get user name from cache or Gun
 */
export function getUserName(userId: string): string {
	// Return from cache if available
	if (usersMap.has(userId)) {
		const name = usersMap.get(userId)!;
		if (name !== userId) {
			// Only return if resolved
			return name;
		}
	}

	// Start subscription to get name asynchronously
	usersRef.get(userId).on((userData) => {
		if (userData && userData.name && userData.name !== userId) {
			updateUserName(userId, userData.name);
		}
	});

	// Return ID as fallback
	return userId;
}

// === Change notification system ===

const userMapChangeListeners = new Set<() => void>();

export function onUserMapChange(listener: () => void): () => void {
	userMapChangeListeners.add(listener);
	return () => userMapChangeListeners.delete(listener);
}

function notifyUserMapChange() {
	userMapChangeListeners.forEach((listener) => listener());
}

/**
 * Load users from Gun with pagination and efficient filtering
 *
 * This approach:
 * 1. Uses Gun's native map() for efficient data traversal
 * 2. Implements client-side pagination to avoid DOM overload
 * 3. Uses debouncing to prevent excessive updates
 * 4. Adds console logging to track data flow
 */
export function loadUsers(
	callback: (users: Array<{ id: string; name: string }>) => void,
	options?: {
		filterText?: string;
		excludeIds?: string[];
		rootId?: string;
	}
): () => void {
	const filterText = options?.filterText?.toLowerCase() || '';
	const excludeIds = options?.excludeIds || [];
	const rootId = options?.rootId;

	// console.log('Loading users with options:', { filterText, excludeIds, rootId });

	// Track state
	let isActive = true;
	let debounceTimer: ReturnType<typeof setTimeout> | null = null;
	let pendingUsers = new Map<string, { id: string; name: string }>();
	let lastResultsHash = '';

	// Register current user presence
	if (rootId) {
		const timestamp = Date.now();
		usersRef.get(rootId).put({
			lastSeen: timestamp
		});
	}

	// Process and emit results with debouncing (150ms)
	function processResults() {
		if (!isActive) return;

		// Clear any pending timer
		if (debounceTimer) {
			clearTimeout(debounceTimer);
			debounceTimer = null;
		}

		debounceTimer = setTimeout(() => {
			// Skip if component unmounted
			if (!isActive) return;

			const users: Array<{ id: string; name: string }> = [];

			// Convert pending users to sorted array
			for (const [_, user] of pendingUsers) {
				users.push(user);
			}

			// Sort by name
			const sortedUsers = users.sort((a, b) => a.name.localeCompare(b.name)).slice(0, 50); // Limit to 50 results max

			// Only emit if changed
			const resultsHash = sortedUsers.map((u) => `${u.id}:${u.name}`).join('|');
			if (resultsHash !== lastResultsHash) {
				// console.log(`Emitting ${sortedUsers.length} users to dropdown`);
				lastResultsHash = resultsHash;
				callback(sortedUsers);
			}
		}, 150);
	}

	// Process a single user from Gun data
	function processUser(userData: any, userId: string) {
		if (!isActive) return;
		if (!userData) return; // Skip nulls

		// Skip excluded users
		if ((rootId && userId === rootId) || excludeIds.includes(userId)) {
			return;
		}

		// Extract name from user data
		const name = userData.name;
		if (!name || name === userId) {
			return; // Skip users without names or unresolved names
		}

		// Apply text filter
		if (filterText && !name.toLowerCase().includes(filterText)) {
			// Remove from pending if filtered out
			pendingUsers.delete(userId);
			return;
		}

		// Add to pending users
		pendingUsers.set(userId, { id: userId, name });

		// Update name cache
		if (!usersMap.has(userId) || usersMap.get(userId) !== name) {
			updateUserName(userId, name);
		}

		// Schedule processing
		processResults();
	}

	// Set up map listener with proper cleanup
	const gunRef = usersRef.map();
	gunRef.on((userData, userId) => {
		// console.log('Gun data received:', { userId, userData });

		// Skip metadata
		if (userId === '_') return;

		processUser(userData, userId);
	});

	// Set up userMap change listener
	const mapChangeUnsubscribe = onUserMapChange(() => {
		// console.log('usersMap changed, reprocessing...');

		// Clear pending users
		pendingUsers.clear();

		// Process users from cache
		for (const [userId, name] of usersMap.entries()) {
			// Skip excluded users
			if ((rootId && userId === rootId) || excludeIds.includes(userId)) {
				continue;
			}

			// Skip unresolved names
			if (name === userId) {
				continue;
			}

			// Apply text filter
			if (filterText && !name.toLowerCase().includes(filterText)) {
				continue;
			}

			// Add to pending
			pendingUsers.set(userId, { id: userId, name });
		}

		// Process results
		processResults();
	});

	// Return cleanup function
	return () => {
		// console.log('Cleaning up user loader');
		isActive = false;

		// Clear debounce timer
		if (debounceTimer) {
			clearTimeout(debounceTimer);
			debounceTimer = null;
		}

		// Unsubscribe from Gun - use proper Gun off() method
		gunRef.off();

		// Unsubscribe from map changes
		mapChangeUnsubscribe();
	};
}

// Fix the modified Map prototype to emit change events
const originalSet = Map.prototype.set;
Map.prototype.set = function (...args) {
	const result = originalSet.apply(this, args);
	if (this === usersMap) {
		notifyUserMapChange();
	}
	return result;
};

// Register callback for name resolution
export function onUserNameResolved(
	userId: string,
	callback: (userId: string, name: string) => void
): () => void {
	// If already in cache, call immediately
	if (usersMap.has(userId)) {
		const name = usersMap.get(userId)!;
		if (name !== userId) {
			callback(userId, name);
		}
	}

	// Subscribe to user data
	const userRef = usersRef.get(userId);
	userRef.on((userData) => {
		if (userData && userData.name && userData.name !== userId) {
			updateUserName(userId, userData.name);
			callback(userId, userData.name);
		}
	});

	// Return cleanup function
	return () => userRef.off();
}
