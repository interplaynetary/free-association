import { gun, transientGun } from './gun/gunSetup';
import { GunSubscription } from './gun/GunSubscription';
import { GunNode } from './gun/GunNode';

// Use a simple type rather than importing from node: modules
type Timeout = ReturnType<typeof setTimeout>;

// Simple in-memory cache for user names
export const usersMap = new Map<string, string>();

// Create a single global transient users node that can be reused
// This prevents creating a new one on every loadUsers call
const transientUsersNode = new GunNode(['users'], undefined, true);
const transientUsersRef = transientGun.get('users');

// Get a user node reference (as a direct path now)
function getUserPath(userId: string): string[] {
  return ['users', userId];
}

/**
 * Register a callback that will be called when a user name is resolved
 */
export function onUserNameResolved(
    userId: string, 
    callback: (userId: string, name: string) => void
): () => void {
    // If we already have the name cached, call the callback immediately
    if (usersMap.has(userId)) {
        const name = usersMap.get(userId)!;
        // Only call back if the name is resolved (different from ID)
        if (name !== userId) {
            callback(userId, name);
        }
    }
    
    // Create a subscription to the user's name
    const subscription = new GunSubscription(getUserPath(userId));
    
    // Subscribe to name changes
    return subscription.on((userData: any) => {
        if (userData && userData.name && userData.name !== userId) {
            // Update the cache
            updateUserName(userId, userData.name);
            callback(userId, userData.name);
        }
    });
}

// Function to get user name from Gun
export function getUserName(userId: string): string {
    // Return from cache if available for immediate UI response
    if (usersMap.has(userId)) {
        const name = usersMap.get(userId)!;
        // Only return the name if it's truly resolved
        if (name !== userId) {
            return name;
        }
    }
    
    // Start subscription to get the name asynchronously
    const subscription = new GunSubscription(getUserPath(userId));
    
    subscription.on((userData: any) => {
        if (userData && userData.name && userData.name !== userId) {
            updateUserName(userId, userData.name);
        }
    });
    
    // Return ID as a fallback while data loads
    return userId;
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

// Update user profile in Gun
export function updateUserProfile(userId: string, name: string): void {
    if (!userId) return;
    
    // Store name in cache immediately for responsive UI
    updateUserName(userId, name);
    
    // Update in Gun
    const userRef = gun.get('users').get(userId);
    userRef.put({
        name: name,
        lastSeen: Date.now()
    });
}

// Event to notify listeners when userMap changes
const userMapChangeListeners = new Set<() => void>();

// Add a listener for userMap changes
export function onUserMapChange(listener: () => void): () => void {
    userMapChangeListeners.add(listener);
    return () => userMapChangeListeners.delete(listener);
}

// Notify listeners of userMap changes
function notifyUserMapChange() {
    userMapChangeListeners.forEach(listener => listener());
}

// Observer for userMap changes
const originalSet = usersMap.set;
usersMap.set = function(key, value) {
    const result = originalSet.call(this, key, value);
    notifyUserMapChange();
    return result;
};

// Load users from Gun database with optimized approach
export function loadUsers(
    callback: (users: Array<{id: string, name: string}>) => void,
    options?: {
      filterText?: string,
      excludeIds?: string[],
      rootId?: string // To exclude the current user/root
    }
): () => void {
    const filterText = options?.filterText?.toLowerCase() || '';
    const excludeIds = options?.excludeIds || [];
    const rootId = options?.rootId;
    
    // Create a throttle mechanism to avoid excessive updates
    let updateTimeout: Timeout | null = null;
    let pendingUpdate = false;
    let lastResultsHash = '';
    let lastUpdateTime = 0;
    const MIN_UPDATE_INTERVAL = 500; // Minimum ms between updates
    const MAX_RESULTS = 50; // Limit number of results to avoid DOM overload
    
    // Make ourselves discoverable if we're loading, but only if we're the root
    if (rootId) {
        const userRef = gun.get('users').get(rootId);
        // Use once() instead of put() to avoid writing to storage
        userRef.once((data) => {
            if (!data || !data.lastSeen || Date.now() - data.lastSeen > 60000) {
                // Only update lastSeen if it's been more than a minute
                userRef.put({ lastSeen: Date.now() });
            }
        });
    }
    
    // Add a filter function to limit the data processed
    const filterPredicate = (userData: any) => {
        if (!userData || userData._removed) return false;
        const userId = userData._key;
        if (!userId) return false;
        
        // Skip excluded users
        if ((rootId && userId === rootId) || excludeIds.includes(userId)) {
            return false;
        }
        
        // More aggressive filtering based on lastSeen to reduce data volume
        // Only include users seen in the last 24 hours if we have no filter
        if (!filterText && userData.lastSeen) {
            const oneDayAgo = Date.now() - 86400000; // 24 hours in ms
            if (userData.lastSeen < oneDayAgo) {
                return false;
            }
        }
        
        // If we have a name in the cache, apply text filter
        if (usersMap.has(userId)) {
            const name = usersMap.get(userId)!;
            
            // Skip users whose name is just their ID (unresolved)
            if (name === userId) {
                return false;
            }
            
            return !filterText || name.toLowerCase().includes(filterText);
        }
        
        // For users not yet in our cache, include only if they have a name
        // different from their ID, to avoid showing unresolved users
        return userData.name && userData.name !== userId;
    };
    
    // Track discovered users
    const discoveredUsers = new Set<string>();
    
    // Process all users in the usersMap and emit to callback
    const processUserResults = () => {
        // Enforce minimum update interval for UI performance
        const now = Date.now();
        if (now - lastUpdateTime < MIN_UPDATE_INTERVAL) {
            if (!updateTimeout) {
                updateTimeout = setTimeout(() => {
                    updateTimeout = null;
                    processUserResults();
                }, MIN_UPDATE_INTERVAL - (now - lastUpdateTime));
            }
            return;
        }
        
        const users: Array<{id: string, name: string}> = [];
        
        // Process only entries in the usersMap that pass our filter
        for (const [id, name] of usersMap.entries()) {
            // Skip excluded users
            if ((rootId && id === rootId) || excludeIds.includes(id)) {
                continue;
            }
            
            // Skip users whose name is just their ID (unresolved)
            if (name === id) {
                continue;
            }
            
            // Apply text filter if needed
            if (filterText && !name.toLowerCase().includes(filterText)) {
                continue;
            }
            
            users.push({ id, name });
            
            // Limit results to prevent DOM overload
            if (users.length >= MAX_RESULTS) break;
        }
        
        // Sort by name
        const sortedUsers = users.sort((a, b) => a.name.localeCompare(b.name));
        
        // Generate a hash of the results to detect changes
        const resultsHash = sortedUsers.map(u => `${u.id}:${u.name}`).join('|');
        
        // Only call the callback if results have changed
        if (resultsHash !== lastResultsHash) {
            lastResultsHash = resultsHash;
            callback(sortedUsers);
            lastUpdateTime = Date.now();
        }
        
        if (updateTimeout) {
            clearTimeout(updateTimeout);
            updateTimeout = null;
        }
        
        if (pendingUpdate) {
            pendingUpdate = false;
            setTimeout(processUserResults, MIN_UPDATE_INTERVAL);
        }
    };
    
    // Setup monitoring of usersMap changes
    const mapChangeUnsub = onUserMapChange(() => {
        // Schedule update instead of processing immediately
        if (!updateTimeout) {
            updateTimeout = setTimeout(() => {
                updateTimeout = null;
                processUserResults();
            }, 100); // Small delay to batch multiple rapid changes
        } else {
            pendingUpdate = true;
        }
    });
    
    // Limited number of users to process at once
    const MAX_QUERIES = 30;
    let queryCounter = 0;
    
    // If we have a filter, use a more focused query approach
    if (filterText) {
        // Use the GunNode filter for better data control
        transientUsersNode.filter((userData) => filterPredicate(userData), (userData) => {
            if (!userData || userData._removed) return;
            
            const userId = userData._key;
            if (!userId) return;
            
            // Only update if they have a proper name
            if (userData.name && userData.name !== userId && !usersMap.has(userId)) {
                updateUserName(userId, userData.name);
            }
            
            // Don't need to processUserResults here as the onUserMapChange will handle it
        });
        
        // Trigger an initial processing
        processUserResults();
    } else {
        // Use a focused one-time query instead of subscribing
        // This uses the global transientUsersRef already created
        transientUsersRef.once((data) => {
            if (!data) {
                processUserResults();
                return;
            }
            
            // Get all user IDs, excluding metadata
            const userIds = Object.keys(data).filter(key => key !== '_');
            
            // Limit to a reasonable number
            const idsToProcess = userIds.slice(0, MAX_QUERIES);
            
            // Process each ID but limit concurrent requests
            idsToProcess.forEach(userId => {
                // Skip excluded users
                if ((rootId && userId === rootId) || excludeIds.includes(userId)) {
                    return;
                }
                
                // Get user data once
                transientUsersRef.get(userId).once((userData) => {
                    if (!userData || typeof userData !== 'object') return;
                    
                    // Only process user with proper name
                    if (userData.name && userData.name !== userId && !usersMap.has(userId)) {
                        updateUserName(userId, userData.name);
                    }
                });
            });
            
            // Process what we have after a short delay
            setTimeout(processUserResults, 200);
        });
    }
    
    // Return a function to clean up
    return () => {
        mapChangeUnsub();
        if (updateTimeout) {
            clearTimeout(updateTimeout);
            updateTimeout = null;
        }
    };
}