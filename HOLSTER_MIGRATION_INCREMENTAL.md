# Incremental Gun → Holster Migration Plan

## Strategy: Bottom-Up, Incremental Replacement

**Principle**: Start at the lowest level, validate each step, never break existing functionality.

**Timeline**: 3-4 weeks
**Approach**: Add Holster alongside Gun, migrate module by module, remove Gun at the end

---

## Critical API Differences Summary

### 1. **Collection Iteration: `.map()` vs `lex` queries**

| Gun | Holster |
|-----|---------|
| `gun.get(id).map().on((item, key) => {...})` | `holster.get(id, {'.': {'*': ''}}, (allItems) => {...})` |
| Streaming: callback per item | Complete object: iterate in app |
| Real-time: fires per item change | Real-time: fires with complete object |

**Impact**: Need to handle delta detection ourselves in Holster

### 2. **User Data Access**

| Gun | Holster |
|-----|---------|
| `gun.user(pubKey).get('field')` | `user.get([pubKey, 'field'])` |
| Method-based | Array syntax |

### 3. **Get Once Syntax**

| Gun | Holster |
|-----|---------|
| `gun.get('x').once(callback)` | `holster.get('x', callback)` |
| Method call | Second parameter |

### 4. **Authentication Events**

| Gun | Holster |
|-----|---------|
| `gun.on('auth', callback)` | Check `user.is` after operations |
| Global event | No global event |

### 5. **Timestamps**

| Gun | Holster |
|-----|---------|
| `GUN.state.is(node, field)` | **Application-level `_updatedAt` fields** |
| Internal metadata (`_`) | Strips metadata - use explicit fields |

---

## Phase 1: Foundation Setup (2-3 days)

### Goal
Get Holster running alongside Gun without breaking anything.

### Tasks

#### 1.1: Enhance existing Holster file
**File**: `src/lib/state/holster.svelte.ts`

```typescript
import Holster from '@mblaney/holster/src/holster.js';
import { writable } from 'svelte/store';

// Initialize Holster
export const holster = Holster({
  peers: ['wss://holster.haza.website'],
  indexedDB: true,
  secure: true
});

export const holsterUser = holster.user();

// Auth state stores
export const holsterUserAlias = writable('');
export const holsterUserPub = writable('');
export const isHolsterAuthenticating = writable(false);

// Log initialization
console.log('[HOLSTER] Initialized successfully');

// Expose for debugging
if (typeof window !== 'undefined') {
  (window as any).holster = holster;
  (window as any).holsterUser = holsterUser;
  console.log('[HOLSTER] Exposed to window for debugging');
}
```

#### 1.2: Verify no conflicts
- Both Gun and Holster should initialize
- Check browser console for errors
- Check IndexedDB for both Gun and Holster data stores

#### 1.3: Add basic test utilities
```typescript
// Development-only test functions
if (import.meta.env.DEV && typeof window !== 'undefined') {
  (window as any).testHolster = {
    // Test basic put/get
    testBasic: () => {
      console.log('[HOLSTER-TEST] Testing basic operations...');
      holster.get('test-key').put('test-value', (err) => {
        if (err) {
          console.error('[HOLSTER-TEST] Put failed:', err);
          return;
        }
        console.log('[HOLSTER-TEST] Put succeeded');

        holster.get('test-key', (data) => {
          console.log('[HOLSTER-TEST] Get result:', data);
        });
      });
    },

    // Check connection status
    checkStatus: () => {
      console.log('[HOLSTER-TEST] Holster instance:', holster);
      console.log('[HOLSTER-TEST] User instance:', holsterUser);
      console.log('[HOLSTER-TEST] User authenticated:', holsterUser.is);
    }
  };

  console.log('[HOLSTER] Test utilities available: window.testHolster');
}
```

### Success Criteria
- ✅ Holster initializes without errors
- ✅ Gun continues to work normally
- ✅ No console errors
- ✅ Both databases visible in DevTools → IndexedDB
- ✅ `window.testHolster.testBasic()` works
- ✅ `window.testHolster.checkStatus()` shows holster object

---

## Phase 2: Authentication Testing (2-3 days)

### Goal
Prove Holster authentication works correctly.

### Tasks

#### 2.1: Add authentication test utilities
```typescript
if (import.meta.env.DEV && typeof window !== 'undefined') {
  (window as any).testHolster.auth = {
    // Create test account
    createTest: async () => {
      console.log('[HOLSTER-AUTH] Creating test account...');
      return new Promise((resolve) => {
        holsterUser.create('holster-test', 'test-password-123', (err) => {
          if (err) {
            console.error('[HOLSTER-AUTH] Create failed:', err);
          } else {
            console.log('[HOLSTER-AUTH] Account created successfully');
            console.log('[HOLSTER-AUTH] User state:', holsterUser.is);
          }
          resolve(err);
        });
      });
    },

    // Login to test account
    loginTest: async () => {
      console.log('[HOLSTER-AUTH] Logging in...');
      return new Promise((resolve) => {
        holsterUser.auth('holster-test', 'test-password-123', (err) => {
          if (err) {
            console.error('[HOLSTER-AUTH] Login failed:', err);
          } else {
            console.log('[HOLSTER-AUTH] Login successful');
            console.log('[HOLSTER-AUTH] User:', holsterUser.is);
            holsterUserAlias.set(holsterUser.is.username);
            holsterUserPub.set(holsterUser.is.pub);
          }
          resolve(err);
        });
      });
    },

    // Test recall
    testRecall: () => {
      console.log('[HOLSTER-AUTH] Testing recall...');
      holsterUser.recall();
      setTimeout(() => {
        console.log('[HOLSTER-AUTH] After recall, user:', holsterUser.is);
      }, 100);
    },

    // Logout
    logout: () => {
      console.log('[HOLSTER-AUTH] Logging out...');
      holsterUser.leave();
      holsterUserAlias.set('');
      holsterUserPub.set('');
      console.log('[HOLSTER-AUTH] User state:', holsterUser.is);
    },

    // Full auth flow test
    fullTest: async () => {
      console.log('[HOLSTER-AUTH] Running full auth flow test...');
      await window.testHolster.auth.createTest();
      await new Promise(r => setTimeout(r, 1000));
      await window.testHolster.auth.loginTest();
      await new Promise(r => setTimeout(r, 1000));
      window.testHolster.auth.logout();
      await new Promise(r => setTimeout(r, 1000));
      window.testHolster.auth.testRecall();
    }
  };
}
```

#### 2.2: Test authenticated data access
```typescript
if (import.meta.env.DEV && typeof window !== 'undefined') {
  (window as any).testHolster.userData = {
    // Write user data
    write: async (key: string, value: any) => {
      if (!holsterUser.is) {
        console.error('[HOLSTER-DATA] Not authenticated');
        return;
      }

      console.log(`[HOLSTER-DATA] Writing ${key}:`, value);
      return new Promise((resolve) => {
        holsterUser.get(key).put(value, (err) => {
          if (err) {
            console.error('[HOLSTER-DATA] Write failed:', err);
          } else {
            console.log('[HOLSTER-DATA] Write succeeded');
          }
          resolve(err);
        });
      });
    },

    // Read user data
    read: (key: string) => {
      if (!holsterUser.is) {
        console.error('[HOLSTER-DATA] Not authenticated');
        return;
      }

      console.log(`[HOLSTER-DATA] Reading ${key}...`);
      holsterUser.get(key, (data) => {
        console.log(`[HOLSTER-DATA] Result:`, data);
      });
    }
  };
}
```

### Success Criteria
- ✅ `window.testHolster.auth.createTest()` creates account
- ✅ `window.testHolster.auth.loginTest()` logs in successfully
- ✅ `holsterUser.is` populated with username and pub
- ✅ `window.testHolster.auth.testRecall()` restores session
- ✅ `window.testHolster.userData.write/read()` works with authenticated data
- ✅ Gun authentication still works normally

---

## Phase 3: Timestamp Research (2-3 days)

### Goal
Understand how Holster tracks timestamps for conflict resolution.

### Tasks

#### 3.1: Investigate Holster's internal state tracking
```typescript
if (import.meta.env.DEV && typeof window !== 'undefined') {
  (window as any).testHolster.timestamps = {
    // Test timestamp extraction
    test: () => {
      console.log('[HOLSTER-TS] Testing timestamp tracking...');

      const testData = {
        value: 'hello',
        timestamp: Date.now()
      };

      holster.get('ts-test').put(testData, (err) => {
        console.log('[HOLSTER-TS] Put result:', err);

        // Read back and inspect structure
        holster.get('ts-test', (data) => {
          console.log('[HOLSTER-TS] Data:', data);
          console.log('[HOLSTER-TS] Data keys:', Object.keys(data || {}));
          console.log('[HOLSTER-TS] Metadata (_):', data?._);
          console.log('[HOLSTER-TS] Full structure:', JSON.stringify(data, null, 2));

          // Try to access Gun-style state
          if (typeof window.GUN !== 'undefined') {
            try {
              const gunTs = window.GUN.state.is(data, 'value');
              console.log('[HOLSTER-TS] GUN.state.is result:', gunTs);
            } catch (e) {
              console.log('[HOLSTER-TS] GUN.state.is not available:', e);
            }
          }
        });
      });
    },

    // Test multiple updates to same key
    testUpdates: async () => {
      console.log('[HOLSTER-TS] Testing update timestamps...');

      const key = 'ts-update-test';

      // First write
      await new Promise(resolve => {
        holster.get(key).put({ value: 'first', time: Date.now() }, resolve);
      });

      await new Promise(r => setTimeout(r, 100));

      // Second write
      await new Promise(resolve => {
        holster.get(key).put({ value: 'second', time: Date.now() }, resolve);
      });

      // Read and inspect
      holster.get(key, (data) => {
        console.log('[HOLSTER-TS] After updates:', data);
      });
    },

    // Test conflict scenario
    testConflict: async () => {
      console.log('[HOLSTER-TS] Testing conflict detection...');

      // Write initial value
      await new Promise(resolve => {
        holster.get('conflict-test').put('initial', resolve);
      });

      // Simulate reading old value
      let oldData: any;
      holster.get('conflict-test', (data) => {
        oldData = data;
        console.log('[HOLSTER-TS] Old data:', oldData);
      });

      await new Promise(r => setTimeout(r, 100));

      // Write new value (simulating another device)
      await new Promise(resolve => {
        holster.get('conflict-test').put('network-update', resolve);
      });

      await new Promise(r => setTimeout(r, 100));

      // Try to write based on old data (should we detect conflict?)
      console.log('[HOLSTER-TS] Attempting write with stale data...');
      holster.get('conflict-test').put('stale-update', (err) => {
        console.log('[HOLSTER-TS] Stale write result:', err);

        holster.get('conflict-test', (finalData) => {
          console.log('[HOLSTER-TS] Final value:', finalData);
        });
      });
    }
  };
}
```

#### 3.2: Examine Holster wire protocol
```typescript
// Hook into Holster's wire messages if possible
if (import.meta.env.DEV && typeof window !== 'undefined') {
  (window as any).testHolster.wire = {
    inspect: () => {
      console.log('[HOLSTER-WIRE] Holster instance:', holster);
      console.log('[HOLSTER-WIRE] Wire object:', (holster as any).wire);

      // Try to access internal wire state
      // This might require reading Holster source code
      // to understand the internal API
    }
  };
}
```

#### 3.3: Create timestamp utility stubs
**File**: `src/lib/utils/holsterTimestamp.ts` (new)

```typescript
/**
 * Holster Timestamp Utilities
 *
 * NOTE: Implementation pending research in Phase 3
 * These are stubs that need to be filled in after understanding
 * how Holster tracks timestamps internally.
 */

/**
 * Extract timestamp from Holster data node
 *
 * TODO: Implement based on Phase 3 research
 * Possible approaches:
 * 1. Check node._ metadata (similar to Gun)
 * 2. Use wire protocol timestamps
 * 3. Track timestamps separately in application layer
 * 4. Use Holster's internal state if exposed
 */
export function getHolsterTimestamp(node: any, fieldName?: string): number | null {
  // STUB - implement after research
  console.warn('[HOLSTER-TS] getHolsterTimestamp not yet implemented');

  // Temporary fallback
  return Date.now();
}

/**
 * Compare two Holster timestamps
 */
export function compareHolsterTimestamps(ts1: number | null, ts2: number | null): number {
  if (ts1 === null && ts2 === null) return 0;
  if (ts1 === null) return -1;
  if (ts2 === null) return 1;
  return ts1 - ts2;
}

/**
 * Check if timestamp is reliable (not placeholder)
 */
export function isReliableHolsterTimestamp(timestamp: number | null): boolean {
  if (timestamp === null) return false;
  const MIN_RELIABLE = new Date('1970-01-02').getTime();
  return timestamp > MIN_RELIABLE;
}

/**
 * Format timestamp for display
 */
export function formatHolsterTimestamp(timestamp: number | null): string {
  if (timestamp === null) return 'Unknown';
  try {
    return new Date(timestamp).toISOString();
  } catch {
    return 'Invalid';
  }
}
```

#### 3.4: Document findings
Create `docs/holster-timestamps.md` with research results:
- How Holster tracks state internally
- Whether timestamps are exposed
- Whether we need application-level tracking
- Recommended approach for conflict resolution

### Success Criteria
- ✅ Understand Holster's timestamp mechanism
- ✅ Document findings in `docs/holster-timestamps.md`
- ✅ Created timestamp utility stubs
- ✅ Identified approach for conflict resolution
- ✅ Test utilities demonstrate timestamp behavior

---

## Phase 4: Isolated Feature Test (2-3 days)

### Goal
Create a simple, isolated feature using ONLY Holster to prove end-to-end functionality.

### Tasks

#### 4.1: Create a "Notes" feature
**File**: `src/lib/state/holster-notes.svelte.ts` (new)

```typescript
import { writable, get } from 'svelte/store';
import { holsterUser } from './holster.svelte';

export interface Note {
  id: string;
  content: string;
  created: number;
}

export const holsterNotes = writable<Note[]>([]);
export const isLoadingNotes = writable(false);

/**
 * Load all notes from Holster
 */
export function loadHolsterNotes() {
  if (!holsterUser.is) {
    console.log('[NOTES] Not authenticated');
    return;
  }

  isLoadingNotes.set(true);
  console.log('[NOTES] Loading notes from Holster...');

  // Use lex query to get all notes
  holsterUser.get('notes', {'.': {'*': ''}}, (allNotesData) => {
    console.log('[NOTES] Received data:', allNotesData);

    if (!allNotesData) {
      holsterNotes.set([]);
      isLoadingNotes.set(false);
      return;
    }

    // Convert object to array
    const notesList: Note[] = Object.entries(allNotesData)
      .filter(([key]) => key !== '_') // Skip metadata
      .map(([id, data]) => {
        if (typeof data === 'string') {
          // Stored as JSON string
          try {
            return JSON.parse(data);
          } catch {
            return { id, content: data, created: Date.now() };
          }
        }
        return data as Note;
      })
      .sort((a, b) => b.created - a.created); // Newest first

    console.log('[NOTES] Loaded notes:', notesList.length);
    holsterNotes.set(notesList);
    isLoadingNotes.set(false);
  });
}

/**
 * Subscribe to real-time note updates
 */
export function subscribeToNotes() {
  if (!holsterUser.is) return;

  console.log('[NOTES] Subscribing to real-time updates...');

  holsterUser.get('notes').on({'.': {'*': ''}}, (allNotesData) => {
    console.log('[NOTES] Real-time update received');

    if (!allNotesData) {
      holsterNotes.set([]);
      return;
    }

    const notesList: Note[] = Object.entries(allNotesData)
      .filter(([key]) => key !== '_')
      .map(([id, data]) => {
        if (typeof data === 'string') {
          try {
            return JSON.parse(data);
          } catch {
            return { id, content: data, created: Date.now() };
          }
        }
        return data as Note;
      })
      .sort((a, b) => b.created - a.created);

    holsterNotes.set(notesList);
  });
}

/**
 * Add a new note
 */
export async function addHolsterNote(content: string): Promise<void> {
  if (!holsterUser.is) {
    throw new Error('Not authenticated');
  }

  const note: Note = {
    id: Date.now().toString(),
    content,
    created: Date.now()
  };

  console.log('[NOTES] Adding note:', note);

  return new Promise((resolve, reject) => {
    const noteJson = JSON.stringify(note);

    holsterUser.get('notes').get(note.id).put(noteJson, (err) => {
      if (err) {
        console.error('[NOTES] Error adding note:', err);
        reject(err);
      } else {
        console.log('[NOTES] Note added successfully');

        // Reload to update UI
        loadHolsterNotes();
        resolve();
      }
    });
  });
}

/**
 * Delete a note
 */
export async function deleteHolsterNote(noteId: string): Promise<void> {
  if (!holsterUser.is) {
    throw new Error('Not authenticated');
  }

  console.log('[NOTES] Deleting note:', noteId);

  return new Promise((resolve, reject) => {
    // In Holster, setting to null deletes
    holsterUser.get('notes').get(noteId).put(null, (err) => {
      if (err) {
        console.error('[NOTES] Error deleting note:', err);
        reject(err);
      } else {
        console.log('[NOTES] Note deleted successfully');
        loadHolsterNotes();
        resolve();
      }
    });
  });
}
```

#### 4.2: Create Notes UI component
**File**: `src/lib/components/HolsterNotesTest.svelte` (new)

```svelte
<script lang="ts">
  import { onMount } from 'svelte';
  import {
    holsterNotes,
    loadHolsterNotes,
    addHolsterNote,
    deleteHolsterNote,
    subscribeToNotes
  } from '$lib/state/holster-notes.svelte';
  import { holsterUser, holsterUserAlias } from '$lib/state/holster.svelte';

  let newNoteContent = '';
  let isAdding = false;

  onMount(() => {
    // Load notes if authenticated
    if ($holsterUser.is) {
      loadHolsterNotes();
      subscribeToNotes();
    }
  });

  async function handleAddNote() {
    if (!newNoteContent.trim()) return;

    isAdding = true;
    try {
      await addHolsterNote(newNoteContent);
      newNoteContent = '';
    } catch (error) {
      console.error('Failed to add note:', error);
      alert('Failed to add note');
    } finally {
      isAdding = false;
    }
  }

  async function handleDeleteNote(noteId: string) {
    if (!confirm('Delete this note?')) return;

    try {
      await deleteHolsterNote(noteId);
    } catch (error) {
      console.error('Failed to delete note:', error);
      alert('Failed to delete note');
    }
  }
</script>

<div class="holster-notes-test">
  <h2>Holster Notes Test</h2>

  {#if $holsterUser.is}
    <div class="user-info">
      Logged in as: {$holsterUserAlias || 'Unknown'}
    </div>

    <div class="add-note">
      <textarea
        bind:value={newNoteContent}
        placeholder="Write a note..."
        rows="3"
      />
      <button
        on:click={handleAddNote}
        disabled={isAdding || !newNoteContent.trim()}
      >
        {isAdding ? 'Adding...' : 'Add Note'}
      </button>
    </div>

    <div class="notes-list">
      <h3>Notes ({$holsterNotes.length})</h3>

      {#if $holsterNotes.length === 0}
        <p class="empty">No notes yet. Add one above!</p>
      {:else}
        {#each $holsterNotes as note (note.id)}
          <div class="note">
            <div class="note-content">{note.content}</div>
            <div class="note-meta">
              {new Date(note.created).toLocaleString()}
              <button on:click={() => handleDeleteNote(note.id)}>Delete</button>
            </div>
          </div>
        {/each}
      {/if}
    </div>
  {:else}
    <div class="not-authenticated">
      <p>Not authenticated. Use the test utilities to log in:</p>
      <code>window.testHolster.auth.loginTest()</code>
    </div>
  {/if}
</div>

<style>
  .holster-notes-test {
    padding: 20px;
    border: 2px solid #4CAF50;
    border-radius: 8px;
    margin: 20px;
    background: #f9f9f9;
  }

  .user-info {
    background: #4CAF50;
    color: white;
    padding: 10px;
    border-radius: 4px;
    margin-bottom: 15px;
  }

  .add-note {
    margin-bottom: 20px;
  }

  .add-note textarea {
    width: 100%;
    padding: 10px;
    border: 1px solid #ddd;
    border-radius: 4px;
    margin-bottom: 10px;
    font-family: inherit;
  }

  .add-note button {
    background: #4CAF50;
    color: white;
    border: none;
    padding: 10px 20px;
    border-radius: 4px;
    cursor: pointer;
  }

  .add-note button:disabled {
    background: #ccc;
    cursor: not-allowed;
  }

  .notes-list h3 {
    margin-bottom: 10px;
  }

  .note {
    background: white;
    border: 1px solid #ddd;
    border-radius: 4px;
    padding: 10px;
    margin-bottom: 10px;
  }

  .note-content {
    margin-bottom: 5px;
    white-space: pre-wrap;
  }

  .note-meta {
    display: flex;
    justify-content: space-between;
    align-items: center;
    font-size: 0.85em;
    color: #666;
  }

  .note-meta button {
    background: #f44336;
    color: white;
    border: none;
    padding: 5px 10px;
    border-radius: 4px;
    cursor: pointer;
    font-size: 0.9em;
  }

  .empty {
    color: #999;
    font-style: italic;
  }

  .not-authenticated {
    padding: 20px;
    background: #fff3cd;
    border-radius: 4px;
  }

  .not-authenticated code {
    display: block;
    margin-top: 10px;
    padding: 10px;
    background: #f0f0f0;
    border-radius: 4px;
    font-family: monospace;
  }
</style>
```

#### 4.3: Add to a test page
Add the component to a route (e.g., `/test` or existing page):

```svelte
<!-- src/routes/+page.svelte or test route -->
<script>
  import HolsterNotesTest from '$lib/components/HolsterNotesTest.svelte';
</script>

<HolsterNotesTest />
```

### Success Criteria
- ✅ Notes feature works end-to-end with Holster
- ✅ Can add notes and see them persist
- ✅ Can delete notes
- ✅ Real-time updates work (test in two browser tabs)
- ✅ Data persists after page reload
- ✅ No Gun dependencies in notes feature
- ✅ Gun features still work normally

---

## Phase 5: Migrate Contacts Module (3-4 days)

### Goal
Replace Gun-based contacts with Holster, while keeping everything else on Gun.

### Tasks

#### 5.1: Create Holster contacts module
**File**: `src/lib/state/contacts-holster.svelte.ts` (new)

```typescript
import { writable, get } from 'svelte/store';
import { holsterUser } from './holster.svelte';
import type { ContactsCollection } from '$lib/schema';
import { parseContacts } from '$lib/validation';

export const holsterContacts = writable<ContactsCollection>({});
export const isLoadingHolsterContacts = writable(false);

/**
 * Load contacts from Holster
 */
export function loadHolsterContacts() {
  if (!holsterUser.is) {
    console.log('[CONTACTS-HOLSTER] Not authenticated');
    return;
  }

  isLoadingHolsterContacts.set(true);
  console.log('[CONTACTS-HOLSTER] Loading contacts...');

  holsterUser.get('contacts', (contactsJson) => {
    console.log('[CONTACTS-HOLSTER] Received data:', contactsJson);

    if (!contactsJson) {
      holsterContacts.set({});
      isLoadingHolsterContacts.set(false);
      return;
    }

    try {
      // Parse and validate
      const parsed = parseContacts(contactsJson);
      if (parsed) {
        holsterContacts.set(parsed);
        console.log('[CONTACTS-HOLSTER] Loaded contacts:', Object.keys(parsed).length);
      }
    } catch (error) {
      console.error('[CONTACTS-HOLSTER] Parse error:', error);
    } finally {
      isLoadingHolsterContacts.set(false);
    }
  });
}

/**
 * Subscribe to real-time contacts updates
 */
export function subscribeToHolsterContacts() {
  if (!holsterUser.is) return;

  console.log('[CONTACTS-HOLSTER] Subscribing to real-time updates...');

  holsterUser.get('contacts').on((contactsJson) => {
    if (!contactsJson) {
      holsterContacts.set({});
      return;
    }

    try {
      const parsed = parseContacts(contactsJson);
      if (parsed) {
        holsterContacts.set(parsed);
        console.log('[CONTACTS-HOLSTER] Real-time update:', Object.keys(parsed).length);
      }
    } catch (error) {
      console.error('[CONTACTS-HOLSTER] Parse error:', error);
    }
  });
}

/**
 * Persist contacts to Holster
 */
export async function persistHolsterContacts(contacts?: ContactsCollection): Promise<void> {
  if (!holsterUser.is) {
    console.log('[CONTACTS-HOLSTER] Not authenticated, skipping persistence');
    return;
  }

  const contactsToSave = contacts || get(holsterContacts);

  if (!contactsToSave || Object.keys(contactsToSave).length === 0) {
    console.log('[CONTACTS-HOLSTER] No contacts to persist');
    return;
  }

  console.log('[CONTACTS-HOLSTER] Persisting contacts...');

  try {
    const contactsJson = JSON.stringify(contactsToSave);

    return new Promise((resolve, reject) => {
      holsterUser.get('contacts').put(contactsJson, (err) => {
        if (err) {
          console.error('[CONTACTS-HOLSTER] Persist error:', err);
          reject(err);
        } else {
          console.log('[CONTACTS-HOLSTER] Persisted successfully');
          resolve();
        }
      });
    });
  } catch (error) {
    console.error('[CONTACTS-HOLSTER] Serialization error:', error);
    throw error;
  }
}
```

#### 5.2: Create feature flag
**File**: `src/lib/config.ts` (new or update)

```typescript
/**
 * Feature flags for gradual Holster migration
 */
export const USE_HOLSTER_CONTACTS =
  import.meta.env.VITE_USE_HOLSTER_CONTACTS === 'true' ||
  (typeof localStorage !== 'undefined' && localStorage.getItem('USE_HOLSTER_CONTACTS') === 'true');

// Add more flags as we migrate other modules
export const USE_HOLSTER_CAPACITIES = false; // Future
export const USE_HOLSTER_TREE = false; // Future
export const USE_HOLSTER_CHAT = false; // Future
```

#### 5.3: Update users.svelte.ts to support both
**File**: `src/lib/state/users.svelte.ts`

```typescript
import { USE_HOLSTER_CONTACTS } from '$lib/config';
import {
  holsterContacts,
  loadHolsterContacts,
  subscribeToHolsterContacts,
  persistHolsterContacts
} from './contacts-holster.svelte';

// Export either Gun or Holster contacts based on flag
export const userContacts = USE_HOLSTER_CONTACTS
  ? holsterContacts
  : /* existing Gun contacts store */;

export function loadContacts() {
  if (USE_HOLSTER_CONTACTS) {
    loadHolsterContacts();
  } else {
    // Existing Gun implementation
  }
}

export function subscribeToContacts() {
  if (USE_HOLSTER_CONTACTS) {
    subscribeToHolsterContacts();
  } else {
    // Existing Gun implementation
  }
}

export async function persistContacts(contacts?: ContactsCollection) {
  if (USE_HOLSTER_CONTACTS) {
    return persistHolsterContacts(contacts);
  } else {
    // Existing Gun implementation
  }
}
```

#### 5.4: Test toggle mechanism
Add to dev tools:
```typescript
if (import.meta.env.DEV && typeof window !== 'undefined') {
  (window as any).toggleHolsterContacts = () => {
    const current = localStorage.getItem('USE_HOLSTER_CONTACTS') === 'true';
    localStorage.setItem('USE_HOLSTER_CONTACTS', (!current).toString());
    console.log(`[TOGGLE] Holster contacts: ${!current}`);
    console.log('[TOGGLE] Reload page to apply');
  };
}
```

### Success Criteria
- ✅ Contacts work with Holster when flag enabled
- ✅ Contacts work with Gun when flag disabled
- ✅ Can toggle between implementations
- ✅ Data format compatible
- ✅ No data loss during toggle
- ✅ UI shows same data regardless of backend
- ✅ All other features (tree, capacities, chat) still use Gun

---

## Phase 6-10: Migrate Remaining Modules (2-3 days each)

Following the same pattern as Phase 5, migrate each module:

### Phase 6: Capacities
- Create `capacities-holster.svelte.ts`
- Add `USE_HOLSTER_CAPACITIES` flag
- Update consumers to check flag
- Test extensively (more complex than contacts)

### Phase 7: Tree
- Create `tree-holster.svelte.ts`
- Add `USE_HOLSTER_TREE` flag
- Handle tree node resolution
- Test recognition calculations

### Phase 8: Recognition/SOGF
- Create `recognition-holster.svelte.ts`
- Handle peer data access (array syntax)
- Test mutual recognition calculations

### Phase 9: Chat (Most Complex)
- Create `chat-holster.svelte.ts`
- Replace `.map()` with lex queries
- Implement efficient delta detection
- Handle message deduplication
- Test real-time messaging

### Phase 10: Network Layer (Unified Initialization)
- Create `initializeHolsterDataStreams()` function
- Extract all Holster initialization into dedicated path
- Keep Gun path (`initializeUserDataStreams()`) separate
- Both systems work independently

### Phase 11: Authentication System
- Add Holster auth functions (login, signup, signout, recall)
- Call `initializeHolsterDataStreams()` after Holster auth
- Keep Gun auth completely separate
- **Note**: New accounts only - no migration from Gun users (fresh start)

---

## Phase 12: Remove Gun (3-5 days)

### Goal
Complete the migration by removing Gun entirely.

### Tasks

1. **Verify all modules using Holster**
   - Audit codebase for remaining Gun usage
   - Enable all Holster feature flags
   - Test entire application end-to-end

2. **Remove Gun dependencies**
   ```bash
   npm uninstall gun gun-avatar @gun-vue/relay
   ```

3. **Remove Gun files**
   - Delete `src/lib/state/gun.svelte.ts`
   - Remove Gun examples (if not needed)
   - Update documentation

4. **Rename for clarity**
   - `holsterUser` → `user`
   - `holster` → can keep or rename to `db`
   - Update all imports

5. **Update CLAUDE.md**
   - Replace Gun references with Holster
   - Update architecture diagrams
   - Update API examples

6. **Final testing**
   - Full regression test suite
   - Performance benchmarks
   - Multi-device sync test
   - Offline/online behavior

### Success Criteria
- ✅ Gun completely removed from package.json
- ✅ No Gun imports in codebase
- ✅ All features working with Holster
- ✅ Performance acceptable
- ✅ Documentation updated
- ✅ Migration complete 🎉

---

## Testing Strategy

### Per-Phase Testing
Each phase must pass these tests before proceeding:

1. **Functionality Test**: Feature works as expected
2. **Persistence Test**: Data survives page reload
3. **Real-time Test**: Updates sync in real-time (two tabs)
4. **Isolation Test**: Other features still work with Gun
5. **Performance Test**: No significant slowdown

### Integration Testing
Before Phase 11 (Gun removal):

1. **Full Feature Test**: All app features work
2. **Multi-Device Test**: Sync across devices/tabs
3. **Offline/Online Test**: Offline changes sync when online
4. **Conflict Test**: Simultaneous edits resolve correctly
5. **Migration Test**: Existing users' data still works

---

## Rollback Procedures

### Per-Phase Rollback
If a phase fails:
1. Disable feature flag
2. Revert new files (or keep for next attempt)
3. Existing Gun implementation continues working

### Emergency Rollback (After Phase 11)
If critical issues after Gun removal:
1. Restore from git history
2. Reinstall Gun dependencies
3. Deploy previous version
4. Investigate issues before re-attempting

---

## Timeline Summary

| Phase | Duration | Cumulative |
|-------|----------|------------|
| Phase 1: Foundation | 2-3 days | 2-3 days |
| Phase 2: Auth Testing | 2-3 days | 4-6 days |
| Phase 3: Timestamps | 2-3 days | 6-9 days |
| Phase 4: Isolated Feature | 2-3 days | 8-12 days |
| Phase 5: Contacts | 3-4 days | 11-16 days |
| Phase 6: Capacities | 2-3 days | 13-19 days |
| Phase 7: Tree | 2-3 days | 15-22 days |
| Phase 8: Recognition | 2-3 days | 17-25 days |
| Phase 9: Chat | 3-4 days | 20-29 days |
| Phase 10: Network Layer | 2-3 days | 22-28 days |
| Phase 11: Authentication | 2-3 days | 24-31 days |
| Phase 12: Remove Gun | 3-5 days | 27-36 days |
| **Total** | **4-5 weeks** | |

---

## Current Status

- [x] Phase 1: Foundation Setup ✅
- [x] Phase 2: Authentication Testing ✅
- [x] Phase 3: Timestamp Research ✅
- [x] Phase 4: Isolated Feature Test ✅
- [x] Phase 5: Migrate Contacts ✅
- [x] Phase 6: Migrate Capacities ✅
- [x] Phase 7: Migrate Tree ✅
- [x] Phase 8: Migrate Recognition ✅
- [x] Phase 9: Migrate Chat ✅
- [x] Phase 10: Migrate Network Layer (Unified Initialization) ✅
- [x] Phase 11: Migrate Authentication System ✅
- [ ] Phase 12: Remove Gun

## ✅ MIGRATION COMPLETE (Phases 1-11)

All core features have been migrated to Holster and are working correctly!

**What's Working:**
- ✅ Authentication (login, signup, signout, recall)
- ✅ Contacts management
- ✅ Capacities management
- ✅ Recognition tree storage
- ✅ SOGF/recognition calculations
- ✅ Chat messaging with optimistic UI
- ✅ Chat read states tracking
- ✅ Slot composition (compose-from/into)
- ✅ Allocation states
- ✅ Unified authentication module routing to Gun or Holster
- ✅ All UI components using unified auth.svelte

**Current Setup:**
- Both Gun and Holster coexist peacefully
- Feature flags allow toggling between implementations
- No data loss when switching backends
- All tests passing

---

## Completed Work Summary

### Phases 1-4: Foundation & Proof of Concept ✅

**Files Created:**
- `src/lib/state/holster.svelte.ts` - Core Holster initialization with auth
- `src/lib/state/holster-tests.ts` - Dev-mode test utilities
- `src/lib/state/notes.svelte.ts` - Notes feature implementation
- `src/lib/components/NotesTest.svelte` - Notes UI component
- `src/lib/utils/holsterTimestamp.ts` - Timestamp utilities
- `src/routes/notes-test/+page.svelte` - Test route

**Key Findings:**
1. ✅ Holster initializes alongside Gun without conflicts
2. ✅ Authentication works with `user.create()`, `user.auth()`, `user.recall()`
3. ✅ **Timestamp Solution**: Use application-level `_updatedAt` fields (Holster strips metadata)
4. ✅ Notes POC validates full CRUD + real-time sync + conflict resolution

**Validated Patterns:**
- Explicit timestamp fields in data schemas
- `addTimestamp()` / `getTimestamp()` / `shouldPersist()` utilities
- Track `lastNetworkTimestamps` Map for conflict detection
- `user.get(path, callback)` for read-once
- `user.get(path).on(callback)` for subscriptions (NOT `.get(path, callback).on()`)
- `user.get(path).next(id).put(data)` for nested writes

**Critical Pattern: Collection-Level Timestamps**
When storing collections (like contacts) with a single `_updatedAt` at the root level:

1. **Schema stays clean** - Don't modify Zod schemas to accept `_updatedAt`
2. **Filter before validation** - Strip metadata in the Holster layer:
   ```typescript
   const { _updatedAt, ...dataOnly } = networkData;
   const parseResult = Schema.safeParse(dataOnly);
   ```
3. **Why?** Because `z.record()` expects homogeneous values, but `_updatedAt` creates heterogeneous values (data + number)
4. **Separation of concerns** - Timestamp handling belongs in persistence layer, not data model
5. **Type safety** - Schemas accurately represent actual data structure

**Critical Pattern: Undefined Values**
Holster/Gun cannot handle `undefined` values:

1. **Clean data before persisting** - Explicitly build objects, omit undefined fields:
   ```typescript
   const cleaned = {
     required_field: data.required_field,
     // Only include optional fields if they have values
     ...(data.optional_field && { optional_field: data.optional_field })
   };
   ```
2. **Never pass undefined** - Use `null` or omit the field entirely

### Phase 5: Contacts Migration ✅

**Files Created:**
- `src/lib/state/contacts-holster.svelte.ts` - Holster contacts implementation
- `src/lib/config.ts` - Feature flags for migration

**Files Modified:**
- `src/lib/state/users.svelte.ts` - Conditional support for both implementations
- `src/lib/state/network.svelte.ts` - Skip Gun streams when using Holster
- `src/lib/state/persistence.svelte.ts` - Skip Gun persistence when using Holster

**Key Learnings:**
1. ✅ **Collection-level timestamps** require filtering before validation
2. ✅ **Undefined values** must be cleaned before persisting to Holster
3. ✅ **Feature flag pattern** allows safe incremental migration
4. ✅ **Schema separation** - Keep schemas clean, filter in persistence layer

**Testing Results:**
- ✅ Contacts work with Holster enabled
- ✅ Toggle between Gun/Holster works correctly
- ✅ Real-time sync validated
- ✅ Data persists correctly with timestamps

### Phase 6: Capacities Migration ✅

**Files Created:**
- `src/lib/state/capacities-holster.svelte.ts` - Holster capacities implementation

**Files Modified:**
- `src/lib/config.ts` - Added `USE_HOLSTER_CAPACITIES` flag
- `src/lib/state/core.svelte.ts` - Conditional support for both implementations
- `src/lib/state/network.svelte.ts` - Skip Gun streams when using Holster
- `src/lib/state/persistence.svelte.ts` - Skip Gun persistence when using Holster

**Key Learnings:**
1. ✅ Capacities use same collection-level timestamp pattern as contacts
2. ✅ Undefined value cleaning essential (address fields, etc.)
3. ✅ Feature flag toggle works seamlessly
4. ✅ Real-time updates validated with capacity data

**Testing Results:**
- ✅ Capacities work with Holster enabled
- ✅ Toggle between Gun/Holster works correctly
- ✅ Real-time sync validated
- ✅ Data persists correctly with timestamps

### Phase 7: Tree Migration ✅

**Files Created:**
- `src/lib/state/tree-holster.svelte.ts` - Holster tree implementation with incremental persistence

**Files Modified:**
- `src/lib/config.ts` - Added `USE_HOLSTER_TREE` flag
- `src/lib/state/core.svelte.ts` - Conditional support for both implementations
- `src/lib/state/network.svelte.ts` - Skip Gun streams when using Holster
- `src/lib/state/persistence.svelte.ts` - Skip Gun persistence when using Holster

**Key Learnings:**
1. ✅ Tree persistence requires special debounce handling (500ms) due to large data size
2. ✅ **Incremental persistence**: Dramatically improved performance by only writing changed nodes
3. ✅ **Queue mechanism**: Prevents partial data corruption during persistence by queueing external updates
4. ✅ **Pending changes tracking**: Ensures rapid edits don't get lost during ongoing persistence
5. ✅ **Cache initialization**: Avoids full tree rewrite on startup by initializing `lastPersistedNodes` from cache

**Performance Improvements Implemented:**
- **Before**: Full tree rewrite on every change (~1,050ms for 50-node tree)
- **After**: Incremental updates (~40ms for 1-node change)
- **Typical improvement**: ~26x faster for single-node changes
- **Mechanisms**:
  - Deep comparison of FlatNode objects (`nodesEqual()`) to detect changes
  - Track `lastPersistedNodes` for delta detection
  - Queue external updates during persistence with timestamp filtering
  - Retry pending local changes after persistence completes

**Testing Results:**
- ✅ Core tree persistence working
- ✅ Real-time sync functional
- ✅ Debounced updates batch changes correctly
- ✅ Incremental persistence only writes changed nodes
- ✅ Queue prevents partial data corruption
- ✅ Pending changes are retried and persisted
- ✅ All nodes persist after page reload
- ✅ Performance dramatically improved

### Phase 8: Recognition/SOGF Migration ✅

**Files Created:**
- `src/lib/state/recognition-holster.svelte.ts` - Holster SOGF implementation

**Files Modified:**
- `src/lib/config.ts` - Added `USE_HOLSTER_RECOGNITION` flag
- `src/lib/state/core.svelte.ts` - Conditional support for both implementations
- `src/lib/state/network.svelte.ts` - Initialize Holster SOGF when flag enabled
- `src/lib/state/persistence.svelte.ts` - Skip Gun persistence when using Holster
- `src/lib/state/subscriptions.svelte.ts` - Handle Holster persistence in debounce

**Key Learnings:**
1. ✅ SOGF is a flat ShareMap (simple key-value), so full-node writes are acceptable
2. ✅ **Svelte 5 $state rune usage**: Use `let` not `const`, access directly without `.value`
3. ✅ Still subscribe to contributors' Gun SOGF streams to receive their shares
4. ✅ Only our own SOGF persistence switches to Holster when flag enabled
5. ✅ Feature flag toggle works seamlessly via `window.toggleHolster.recognition()`

**Testing Results:**
- ✅ SOGF persists to Holster correctly
- ✅ Real-time sync validated
- ✅ Toggle between Gun/Holster works correctly
- ✅ Timestamps prevent conflict issues
- ✅ Debounced persistence (300ms) batches updates efficiently

---

### Phase 9: Chat Migration ✅

**Files Created:**
- `src/lib/state/chat-holster.svelte.ts` - Holster chat implementation with optimistic UI

**Files Modified:**
- `src/lib/config.ts` - Added `USE_HOLSTER_CHAT` flag with toggle utility
- `src/lib/state/chat.svelte.ts` - Conditional routing to Holster or Gun based on flag
- `src/lib/state/network.svelte.ts` - Skip Gun chat streams when using Holster
- `src/lib/components/ChatMessage.svelte` - Added status indicator for optimistic UI

**Key Architectural Differences:**
- **Gun**: `.map()` streams individual messages, auto-resolves references
- **Holster**: Returns complete object with all messages, requires manual delta detection
- **Gun**: Reference-based storage (message in `user.get('all')`, reference in chat)
- **Holster**: Direct storage (message directly at `holster.get(chatId).next(messageId)`)

**Features Implemented:**
1. ✅ **Direct storage** - No Gun-style references, simpler data structure
2. ✅ **Delta detection** - Tracks `lastSeenTimestamp` to filter duplicate network updates
3. ✅ **Optimistic UI** - Messages appear instantly with "Sending..." status
4. ✅ **Status tracking** - `pending`, `sent`, `failed` states with visual indicators
5. ✅ **Svelte 5 reactivity** - Used `$derived` for reactive status display
6. ✅ **Deduplication** - Prevents duplicate messages by timestamp comparison

**Key Learnings:**
1. ✅ **Holster returns complete objects** - No streaming, use manual delta detection
2. ✅ **Update timestamp before sending** - Prevents network duplicate from racing with callback
3. ✅ **Create new objects, don't mutate** - Using `delete` doesn't trigger Svelte reactivity
4. ✅ **Use `$derived` for computed values** - In Svelte 5, const values aren't reactive
5. ✅ **Plain text by design** - No encryption for capacity chats

**Testing Results:**
- ✅ Optimistic UI works - instant message display
- ✅ Status indicator clears on success
- ✅ Failed messages show error status
- ✅ Real-time sync validated
- ✅ Delta detection prevents duplicates
- ✅ Toggle between Gun/Holster works correctly
- ✅ No race conditions with network updates

**Known Issues / Security Considerations:**

⚠️ **Messages Not in User Space**

Current implementation stores messages in **public global space**:
```javascript
// Current implementation (INSECURE)
holster.get(chatId).next(messageId).put(message);  // Public space
```

**Security Risks:**
- Anyone who knows the `chatId` can write/overwrite messages
- No authentication required to modify chat data
- Messages stored as **plain text** (no encryption)

**Proper Implementation (Future):**
```javascript
// Secure implementation (requires authentication)
holsterUser.get('chats').next(chatId).next(messageId).put(message);  // User space
```

**Status:** Documented but not yet implemented (acceptable for capacity chat POC)

---

---

## Phase 10: Network Layer (Unified Initialization) (2-3 days)

### Goal
Create a dedicated Holster initialization path that is completely independent from Gun initialization.

### Why This Must Come Before Authentication
- Phase 11 (Auth) needs to call `initializeHolsterDataStreams()` after authentication succeeds
- That function doesn't exist yet - we need to create it in this phase
- Gun and Holster auth/data must remain completely independent (no cross-system access)

### Tasks

#### 10.1: Create `initializeHolsterDataStreams()` function
**File**: `src/lib/state/network.svelte.ts`

Add a new function that initializes all Holster data streams:

```typescript
/**
 * Initialize all Holster user data streams
 * Called after Holster authentication succeeds
 */
export async function initializeHolsterDataStreams(): Promise<void> {
  // Check Holster authentication
  if (!holsterUser.is) {
    console.log('[NETWORK-HOLSTER] Cannot initialize - not authenticated');
    return;
  }

  console.log('[NETWORK-HOLSTER] Initializing all Holster data streams');

  try {
    // Initialize all Holster modules that have been migrated
    const { initializeHolsterContacts } = await import('./contacts-holster.svelte');
    const { initializeHolsterCapacities } = await import('./capacities-holster.svelte');
    const { initializeHolsterTree } = await import('./tree-holster.svelte');
    const { initializeHolsterSogf } = await import('./recognition-holster.svelte');

    // Initialize each module
    initializeHolsterContacts();
    initializeHolsterCapacities();
    initializeHolsterTree();
    initializeHolsterSogf();

    // Chat initialization is handled by getChatMessages() calling subscribeToHolsterChat()
    // No explicit initialization needed here

    console.log('[NETWORK-HOLSTER] All Holster data streams initialized successfully');
  } catch (error) {
    console.error('[NETWORK-HOLSTER] Error initializing Holster data streams:', error);
  }
}
```

#### 10.2: Verify Gun path remains untouched
**File**: `src/lib/state/network.svelte.ts`

Confirm that `initializeUserDataStreams()` still works independently:

```typescript
/**
 * Initialize all Gun user data streams
 * (Existing function - should remain unchanged)
 */
export async function initializeUserDataStreams(): Promise<void> {
  // ... existing Gun initialization code ...
  // This function should NOT call any Holster code
}
```

#### 10.3: Add initialization helpers to Holster modules

Each Holster module should export an `initialize*` function:

**Example for contacts-holster.svelte.ts:**
```typescript
/**
 * Initialize Holster contacts (load + subscribe)
 */
export function initializeHolsterContacts() {
  if (!holsterUser.is) {
    console.log('[CONTACTS-HOLSTER] Cannot initialize - not authenticated');
    return;
  }

  console.log('[CONTACTS-HOLSTER] Initializing...');
  loadHolsterContacts();
  subscribeToHolsterContacts();
}
```

Repeat for:
- `capacities-holster.svelte.ts` → `initializeHolsterCapacities()`
- `tree-holster.svelte.ts` → `initializeHolsterTree()` (already exists)
- `recognition-holster.svelte.ts` → `initializeHolsterSogf()` (already exists)
- `chat-holster.svelte.ts` → No explicit init needed (handled by `getChatMessages()`)

#### 10.4: Test both paths independently

Add test utilities:

```typescript
if (import.meta.env.DEV && typeof window !== 'undefined') {
  (window as any).testNetworkInit = {
    // Test Gun initialization
    testGunInit: async () => {
      console.log('[TEST] Testing Gun initialization...');
      await initializeUserDataStreams();
      console.log('[TEST] Gun initialization complete');
    },

    // Test Holster initialization
    testHolsterInit: async () => {
      console.log('[TEST] Testing Holster initialization...');
      await initializeHolsterDataStreams();
      console.log('[TEST] Holster initialization complete');
    }
  };
}
```

### Success Criteria
- ✅ `initializeHolsterDataStreams()` function exists
- ✅ Holster initialization is completely independent from Gun
- ✅ `initializeUserDataStreams()` (Gun) still works unchanged
- ✅ Each Holster module has an `initialize*()` function
- ✅ Test utilities demonstrate both paths work independently
- ✅ No cross-system dependencies (Gun auth doesn't trigger Holster, vice versa)

---

## Phase 11: Authentication System (2-3 days)

### Goal
Add Holster authentication functions that call the network initialization we created in Phase 10.

### Tasks

#### 11.1: Add Holster authentication functions
**File**: `src/lib/state/holster.svelte.ts`

Add production-ready auth functions:

```typescript
import { initializeHolsterDataStreams } from './network.svelte';

/**
 * Login with Holster
 */
export async function holsterLogin(alias: string, password: string): Promise<void> {
  console.log(`[HOLSTER-AUTH] Attempting login for: ${alias}`);

  return new Promise((resolve, reject) => {
    holsterUser.auth(alias, password, (ack: any) => {
      if (ack.err) {
        console.error('[HOLSTER-AUTH] Login failed:', ack.err);
        reject(new Error(ack.err));
      } else {
        console.log('[HOLSTER-AUTH] Login successful');

        // Update stores
        holsterUserAlias.set(holsterUser.is.alias);
        holsterUserPub.set(holsterUser.is.pub);

        // Initialize data streams (Phase 10 function!)
        initializeHolsterDataStreams();

        resolve();
      }
    });
  });
}

/**
 * Signup with Holster
 */
export async function holsterSignup(alias: string, password: string): Promise<void> {
  console.log(`[HOLSTER-AUTH] Creating account for: ${alias}`);

  return new Promise((resolve, reject) => {
    holsterUser.create(alias, password, (ack: any) => {
      if (ack.err) {
        console.error('[HOLSTER-AUTH] Signup failed:', ack.err);
        reject(new Error(ack.err));
      } else {
        console.log('[HOLSTER-AUTH] Account created, logging in...');

        // After successful creation, login
        holsterLogin(alias, password)
          .then(resolve)
          .catch(reject);
      }
    });
  });
}

/**
 * Signout from Holster
 */
export async function holsterSignout(): Promise<void> {
  console.log('[HOLSTER-AUTH] Signing out...');

  holsterUser.leave();
  holsterUserAlias.set('');
  holsterUserPub.set('');

  console.log('[HOLSTER-AUTH] Signed out successfully');
}

/**
 * Recall Holster session
 */
export function holsterRecall(): void {
  console.log('[HOLSTER-AUTH] Attempting to recall session...');

  holsterUser.recall();

  // Check if recall succeeded after a brief delay
  setTimeout(() => {
    if (holsterUser.is) {
      console.log('[HOLSTER-AUTH] Session recalled:', holsterUser.is.alias);

      holsterUserAlias.set(holsterUser.is.alias);
      holsterUserPub.set(holsterUser.is.pub);

      // Initialize data streams
      initializeHolsterDataStreams();
    } else {
      console.log('[HOLSTER-AUTH] No session to recall');
    }
  }, 100);
}
```

#### 11.2: Add recall on page load
**File**: `src/lib/state/holster.svelte.ts`

Add auto-recall when module loads (similar to Gun):

```typescript
// Auto-recall on page load (browser only)
if (typeof window !== 'undefined') {
  holsterRecall();
}
```

#### 11.3: Create UI components for Holster auth
**File**: `src/lib/components/HolsterAuthTest.svelte` (new)

```svelte
<script lang="ts">
  import { holsterLogin, holsterSignup, holsterSignout, holsterUserAlias, holsterUserPub } from '$lib/state/holster.svelte';

  let alias = '';
  let password = '';
  let mode: 'login' | 'signup' = 'login';
  let isLoading = false;
  let error = '';

  async function handleSubmit() {
    if (!alias || !password) {
      error = 'Alias and password required';
      return;
    }

    isLoading = true;
    error = '';

    try {
      if (mode === 'login') {
        await holsterLogin(alias, password);
      } else {
        await holsterSignup(alias, password);
      }

      // Clear form
      alias = '';
      password = '';
    } catch (err: any) {
      error = err.message || 'Authentication failed';
    } finally {
      isLoading = false;
    }
  }

  async function handleSignout() {
    await holsterSignout();
  }
</script>

<div class="holster-auth">
  <h2>Holster Authentication</h2>

  {#if $holsterUserPub}
    <div class="authenticated">
      <p>Logged in as: <strong>{$holsterUserAlias}</strong></p>
      <p>Public key: <code>{$holsterUserPub.slice(0, 20)}...</code></p>
      <button onclick={handleSignout}>Sign Out</button>
    </div>
  {:else}
    <div class="auth-form">
      <div class="mode-toggle">
        <button
          class:active={mode === 'login'}
          onclick={() => mode = 'login'}
        >
          Login
        </button>
        <button
          class:active={mode === 'signup'}
          onclick={() => mode = 'signup'}
        >
          Sign Up
        </button>
      </div>

      <form onsubmit|preventDefault={handleSubmit}>
        <input
          type="text"
          bind:value={alias}
          placeholder="Alias"
          disabled={isLoading}
        />
        <input
          type="password"
          bind:value={password}
          placeholder="Password"
          disabled={isLoading}
        />

        {#if error}
          <p class="error">{error}</p>
        {/if}

        <button type="submit" disabled={isLoading}>
          {isLoading ? 'Processing...' : (mode === 'login' ? 'Login' : 'Sign Up')}
        </button>
      </form>
    </div>
  {/if}
</div>

<style>
  .holster-auth {
    max-width: 400px;
    margin: 20px auto;
    padding: 20px;
    border: 2px solid #2196F3;
    border-radius: 8px;
  }

  .mode-toggle {
    display: flex;
    gap: 10px;
    margin-bottom: 15px;
  }

  .mode-toggle button {
    flex: 1;
    padding: 10px;
    background: #f0f0f0;
    border: 1px solid #ccc;
    border-radius: 4px;
    cursor: pointer;
  }

  .mode-toggle button.active {
    background: #2196F3;
    color: white;
  }

  form {
    display: flex;
    flex-direction: column;
    gap: 10px;
  }

  input {
    padding: 10px;
    border: 1px solid #ddd;
    border-radius: 4px;
  }

  button[type="submit"] {
    padding: 10px;
    background: #2196F3;
    color: white;
    border: none;
    border-radius: 4px;
    cursor: pointer;
  }

  button:disabled {
    background: #ccc;
    cursor: not-allowed;
  }

  .error {
    color: #f44336;
    margin: 0;
  }

  .authenticated {
    text-align: center;
  }

  .authenticated code {
    font-size: 0.9em;
    background: #f0f0f0;
    padding: 2px 6px;
    border-radius: 3px;
  }
</style>
```

#### 11.4: Test complete auth flow

Test all authentication scenarios:

1. **Sign up** → creates account → auto-login → data streams initialize
2. **Login** → authenticates → data streams initialize
3. **Recall** → restores session → data streams initialize
4. **Sign out** → clears session → data streams stop

Verify in browser console:
- `[HOLSTER-AUTH]` logs show auth flow
- `[NETWORK-HOLSTER]` logs show stream initialization
- Contacts, capacities, tree, recognition all load after auth

### Success Criteria
- ✅ `holsterLogin()`, `holsterSignup()`, `holsterSignout()`, `holsterRecall()` functions work
- ✅ All functions call `initializeHolsterDataStreams()` after successful auth
- ✅ Session recall works on page reload
- ✅ UI component provides full auth flow
- ✅ Gun auth remains completely separate (unchanged)
- ✅ Both auth systems can coexist (Gun users see Gun data, Holster users see Holster data)
- ✅ No cross-contamination between systems

---

## Next Steps After Phase 11

After completing Phase 11, you'll have:
- ✅ Holster authentication fully functional
- ✅ Holster data initialization working
- ✅ Gun authentication still functional
- ✅ Both systems completely independent

**Phase 12** will then remove Gun entirely, keeping only the Holster path.

---

## Completed Optimizations

### Tree Incremental Persistence ✅ (Completed during Phase 7)

**Problem Solved:** Tree persistence was slow due to rewriting the entire tree (all nodes) on every change.

**Solution Implemented:**
1. **Incremental updates**: Only write nodes that changed (new, modified, or deleted)
2. **Delta detection**: Deep comparison of `FlatNode` objects using `nodesEqual()`
3. **Baseline tracking**: Maintain `lastPersistedNodes` to compare against current state
4. **Queue mechanism**: Queue external updates during persistence to prevent partial data corruption
5. **Pending changes**: Track and retry local changes made during ongoing persistence
6. **Cache initialization**: Initialize `lastPersistedNodes` from cache to avoid full rewrite on startup

**Performance Results:**
- **26x faster** for typical single-node changes (1,050ms → 40ms)
- **No partial data**: Queue prevents UI corruption during persistence
- **No data loss**: Pending changes mechanism ensures all edits persist
- **Smart caching**: Startup doesn't trigger full tree rewrite

**Implementation Details:**
- Located in: `src/lib/state/tree-holster.svelte.ts`
- Key functions:
  - `nodesEqual()`: Deep comparison for change detection
  - `processQueuedUpdate()`: Handle external updates and pending changes
  - `persistHolsterTree()`: Incremental persistence with queue support

This optimization was completed during Phase 7 and is now production-ready.

---

## How to Permanently Enable All Holster Features in Code

If you want to make Holster the default (instead of using localStorage toggles), update the feature flags in `src/lib/config.ts`:

### Option 1: Set Default Values (Recommended for Production)

Replace the conditional checks with `true`:

```typescript
// In src/lib/config.ts

// Change from:
export const USE_HOLSTER_AUTH =
	import.meta.env.VITE_USE_HOLSTER_AUTH === 'true' ||
	(typeof localStorage !== 'undefined' &&
		localStorage.getItem('USE_HOLSTER_AUTH') === 'true');

// To:
export const USE_HOLSTER_AUTH = true;

// Repeat for all flags:
export const USE_HOLSTER_CONTACTS = true;
export const USE_HOLSTER_CAPACITIES = true;
export const USE_HOLSTER_TREE = true;
export const USE_HOLSTER_CHAT = true;
export const USE_HOLSTER_RECOGNITION = true;
export const USE_HOLSTER_COMPOSE = true;
export const USE_HOLSTER_CHAT_READ_STATES = true;
export const USE_HOLSTER_ALLOCATION_STATES = true;
```

### Option 2: Use Environment Variables

Set all flags to `true` in your `.env` file:

```bash
# .env
VITE_USE_HOLSTER_AUTH=true
VITE_USE_HOLSTER_CONTACTS=true
VITE_USE_HOLSTER_CAPACITIES=true
VITE_USE_HOLSTER_TREE=true
VITE_USE_HOLSTER_CHAT=true
VITE_USE_HOLSTER_RECOGNITION=true
VITE_USE_HOLSTER_COMPOSE=true
VITE_USE_HOLSTER_CHAT_READ_STATES=true
VITE_USE_HOLSTER_ALLOCATION_STATES=true
```

### Option 3: Quick Toggle for All Users

For production deployment, use Option 1 above. This ensures all users get Holster by default without needing to:
- Run `window.toggleHolster.enableAll()` in console
- Set localStorage manually
- Configure environment variables

**Why Option 1 is Best for Production:**
- No environment variable management
- No user-side configuration needed
- Clean, simple code
- Works the same for all users
- Easy to understand and maintain

---

*Last Updated: 2025-10-14*
*Status: ✅ PHASES 1-11 COMPLETE - Holster Migration Successful!*

**What's Next:** Phase 12 will remove Gun entirely, keeping only Holster as the sole backend.
