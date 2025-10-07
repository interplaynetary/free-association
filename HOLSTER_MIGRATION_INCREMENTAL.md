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
| `GUN.state.is(node, field)` | **TBD** - needs research |
| Well-documented | Needs investigation |

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

### Phase 10: Network Layer
- Update all stream configs for Holster
- Update usersList iteration
- Update peer data access patterns
- Full integration testing

---

## Phase 11: Remove Gun (3-5 days)

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
| Phase 10: Network | 2-3 days | 22-32 days |
| Phase 11: Remove Gun | 3-5 days | 25-37 days |
| **Total** | **4-5 weeks** | |

---

## Current Status

- [ ] Phase 1: Foundation Setup
- [ ] Phase 2: Authentication Testing
- [ ] Phase 3: Timestamp Research
- [ ] Phase 4: Isolated Feature Test
- [ ] Phase 5: Migrate Contacts
- [ ] Phase 6: Migrate Capacities
- [ ] Phase 7: Migrate Tree
- [ ] Phase 8: Migrate Recognition
- [ ] Phase 9: Migrate Chat
- [ ] Phase 10: Migrate Network Layer
- [ ] Phase 11: Remove Gun

---

## Next Immediate Steps

1. Start Phase 1: Enhance `src/lib/state/holster.svelte.ts`
2. Add test utilities to window object
3. Verify Holster initializes without errors
4. Test basic put/get operations
5. Verify Gun still works normally

---

*Last Updated: 2025-10-07*
*Status: Planning Complete - Ready to Start Phase 1*
