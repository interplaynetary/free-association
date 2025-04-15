import { 
  createGunStore,
  createCollectionStore,
  combineStores,
  mapStore,
  debounceStore,
  switchMapStore,
  withLatestFromStore,
  createDeepStore,
  type CollectionItem
} from '../../svelte/reactiveStores';

// Example interfaces
interface User extends CollectionItem {
  name: string;
  email: string;
  role: string;
  preferences?: string; // Reference to preferences node
}

interface Preferences extends CollectionItem {
  theme: string;
  notifications: boolean;
  language: string;
}

interface Message extends CollectionItem {
  text: string;
  sender: string;
  timestamp: number;
  read: boolean;
}

/**
 * Example 1: Basic store for current user
 */
export const currentUser = createGunStore<User>(['users', 'current']);

/**
 * Example 2: User preferences using deep store pattern
 * This automatically resolves the preferences reference
 */
export const userWithPrefs = createDeepStore<User & { preferences?: Preferences }>(
  ['users', 'current'],
  1 // Resolve references one level deep
);

/**
 * Example 3: Messages collection
 */
export const messages = createCollectionStore<Message>(
  'messages',
  // Sort by timestamp (newest first)
  (a, b) => (b[1].timestamp || 0) - (a[1].timestamp || 0)
);

/**
 * Example 4: Unread messages count
 * Demonstrates creating a derived value from a collection
 */
export const unreadCount = mapStore(
  messages,
  (msgs) => msgs.filter(([_, msg]) => !msg.read).length
);

/**
 * Example 5: Combined user and preferences
 * Shows combining two stores for a unified view
 */
export const userPreferences = createGunStore<Preferences>('preferences');

export const userWithCombinedPrefs = combineStores(
  currentUser,
  userPreferences,
  (user, prefs) => ({
    ...user,
    preferences: prefs
  })
);

/**
 * Example 6: Debounced input store
 * Useful for search inputs to avoid excessive updates
 */
export const searchInput = createGunStore<string>('search');
export const debouncedSearch = debounceStore(searchInput, 300);

/**
 * Example 7: Dynamic content based on user role
 * Uses switchMap to dynamically change data source
 */
export const roleBasedContent = switchMapStore(
  currentUser,
  (user) => {
    // Choose different content based on user role
    if (user?.role === 'admin') {
      return createGunStore(['content', 'admin']);
    } else if (user?.role === 'moderator') {
      return createGunStore(['content', 'moderator']);
    } else {
      return createGunStore(['content', 'user']);
    }
  }
);

/**
 * Example 8: User-specific messages
 * Combines user data with message data when user changes
 */
export const userMessages = withLatestFromStore(
  currentUser,
  messages,
  (user, msgs) => {
    if (!user) return [];
    
    // Filter messages to only show those related to the current user
    return msgs.filter(([_, msg]) => 
      msg.sender === user._key || msg.recipient === user._key
    );
  }
);

/**
 * Example 9: Complex example - Chat system with typing indicators
 */

// Typing status store - { [userId]: timestamp }
export const typingStatus = createGunStore<Record<string, number>>('typing');

// User status store - { [userId]: 'online' | 'offline' | 'away' }
export const userStatus = createGunStore<Record<string, string>>('status');

// Chat room participants
export const participants = createCollectionStore<User>('participants');

// Combine everything for a rich chat UI
export const chatState = combineStores(
  combineStores(
    messages,
    typingStatus,
    (msgs, typing) => ({ messages: msgs, typing })
  ),
  combineStores(
    participants,
    userStatus,
    (parts, status) => ({ participants: parts, status })
  ),
  (msgData, userData) => ({
    messages: msgData.messages,
    typing: msgData.typing,
    participants: userData.participants,
    status: userData.status,
    // Add derived data
    activeParticipants: userData.participants.filter(([id]) => 
      userData.status[id] === 'online'
    ),
    // Users who are currently typing
    typingUsers: userData.participants
      .filter(([id]) => {
        const lastType = msgData.typing[id];
        return lastType && Date.now() - lastType < 5000; // Typing within last 5 seconds
      })
      .map(([_, user]) => user.name)
  })
);

/**
 * Example 10: User settings form with auto-save
 */
export const userSettingsForm = createGunStore<Preferences>('settings');

// Create a debounced version that will auto-save changes
export const autoSaveSettings = debounceStore(userSettingsForm, 1000);

// Save settings when the debounced store changes
autoSaveSettings.subscribe(settings => {
  if (settings) {
    // This would typically be done through an action or effect
    console.log('Auto-saving settings:', settings);
    createGunStore<Preferences>(['users', 'current', 'preferences']).node().put(settings);
  }
}); 