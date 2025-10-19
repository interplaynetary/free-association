# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is **Free-Association**, a peer-to-peer distributed application implementing a mathematical framework for mutual recognition and capacity distribution. It's a working alternative to capitalism that enables spontaneous self-actualization through mutual aid without centralized control, private property, or state intervention.

**Core Concept:** Users recognize each other's contributions to their self-actualization, and surplus capacities (housing, skills, time, etc.) flow according to mutual recognition proportions. The system uses Gun (decentralized database) for P2P data sync and Svelte 5 for the UI.

**Live Interface:** https://interplaynetary.github.io/free-association/

## Development Commands

### Setup
```bash
bun install
```

### Development
Run these in **separate terminals**:
```bash
# Terminal 1: Dev server
bun run dev

# Terminal 2: Gun relay server
bun start
```

### Production
```bash
bun run build
```

### Other Commands
```bash
bun run check              # Type checking
bun run check:watch        # Watch mode type checking
bun run format             # Format with Prettier
bun run lint               # Lint with ESLint
bun run preview            # Preview production build
```

### Capacitor (Mobile)
```bash
bun run build:capacitor    # Build and sync for mobile
bun run cap:ios            # Open iOS project
bun run cap:android        # Open Android project
bun run cap:sync           # Sync web assets to native
```

## Architecture

### Core State Management (`src/lib/state/`)

The application uses **Svelte 5 runes** (`$state`, `$derived`, `$effect`) for reactive state management. State is organized into specialized modules:

#### **Core Data Flow**

```
gun.svelte.ts (Gun DB interface)
    ↓
network.svelte.ts (Real-time sync with timestamps)
    ↓
core.svelte.ts (App state stores)
    ↓
calculations.svelte.ts (Derived calculations)
    ↓
persistence.svelte.ts (Write back to Gun)
```

#### **Key State Modules**

- **`gun.svelte.ts`**: Gun database initialization, authentication, usersList management
- **`network.svelte.ts`**: Network subscriptions, stream management, timestamp-based conflict resolution
- **`persistence.svelte.ts`**: Persisting local changes back to Gun with timestamp validation
- **`core.svelte.ts`**: Central state stores (userTree, userCapacities, recognitionCache, etc.)
- **`calculations.svelte.ts`**: Reactive calculations (mutual recognition, shares, allocations)
- **`users.svelte.ts`**: User/contact management, public key resolution
- **`chat.svelte.ts`**: Per-capacity chat functionality
- **`collective-rec.svelte.ts`**: Collective recognition 
- **`collective-tree.svelte.ts`**: Collective trees
 
#### **Stream Management Pattern**

The codebase uses a sophisticated stream subscription system (`network.svelte.ts`) based on `StreamSubscriptionManager`:

- **Own Data Streams**: User's personal data (tree, capacities, contacts, compose-from/into, chatReadStates)
- **SOGF Streams**: Contributors' share-of-general-fulfillment data
- **Mutual Contributor Streams**: Capacities, allocation states, compose-from/into from mutual contributors
- **Chat Streams**: Messages for each capacity

Streams automatically subscribe/unsubscribe based on reactive dependencies (contributors, mutual contributors, capacity IDs).

### Timestamp-Based Conflict Resolution

**Critical Pattern**: Gun's native timestamps (`GUN.state.is()`) are used for conflict resolution:

1. **Network layer** (`network.svelte.ts`):
   - `createDataProcessor()` compares incoming Gun timestamps with last known timestamps
   - Only accepts updates if incoming data is newer (or value changed)
   - Tracks timestamps in `lastNetworkTimestamps` export object

2. **Persistence layer** (`persistence.svelte.ts`):
   - `safelyPersist()` reads current Gun data before writing
   - Compares network timestamp with local timestamp
   - **Blocks writes** if network has newer data (prevents overwriting)

3. **Utilities** (`src/lib/utils/gunTimestamp.ts`):
   - `getGunTimestamp()`: Extract Gun's internal timestamp
   - `compareGunTimestamps()`: Compare two timestamps
   - `isReliableGunTimestamp()`: Validate timestamp reliability

### Recognition Tree Protocol (`src/lib/protocol.ts`)

Two node types with clean separation:

1. **Contribution Nodes**: Represent actual work
   - Have positive contributors (people who did the work)
   - Have `manual_fulfillment` (quality assessment, defaults to 100%)
   - Can have anti-contributors (people who hampered work)

2. **Non-Contribution Nodes**: Structural decomposition
   - No direct contributors (abstract organization)
   - Fulfillment calculated from weighted children
   - Cannot have manual_fulfillment or anti-contributors

**Key Functions**:
- `weight()`: Calculate a node's weight in the tree (recursive proportion)
- `mutualRecognition()`: Calculate mutual recognition between two users
- `generalShare()`: Calculate share based on mutual recognition proportions

### Capacity Allocation System

**Modern Approach** (Provider-centric mutual desire):
- Implemented in `core.svelte.ts` via `computeProviderAllocationStates()`
- **Mutual Fulfillment**: Only recipients with mutual desires (both parties express interest) receive allocations
- **MR Normalization**: Proportions normalized among mutually interested recipients only
- **Zero-Waste**: Unused capacity redistributed to mutual interests

**Slot Composition** (`userDesiredSlotComposeFrom`, `userDesiredSlotComposeInto`):
- Recipients express desires: "I want X units FROM provider-slot INTO my-target"
- Providers express counter-desires: "I want Y units FROM my-slot INTO recipient-targets"
- System calculates mutual desires (minimum of both)

### Schema & Validation (`src/lib/schema.ts`, `src/lib/validation.ts`)

- **Zod schemas** define all data structures
- **Validation layer** parses and validates Gun data
- **Important**: Gun's native timestamps (`GUN.state.is()`) replace application-level timestamp wrappers
- No more `Timestamped<T>` wrappers - use `getGunTimestamp()` directly

### UI Components (`src/lib/components/`)

- **Svelte 5** with runes (`$state`, `$derived`, `$effect`)
- **Tailwind CSS** for styling
- **bits-ui** for accessible component primitives
- **MapLibre GL** for geographic visualization

## Gun Database Best Practices

### Memory Management
- **Always clean up subscriptions**: Store cleanup functions and call them
- Use `StreamSubscriptionManager` pattern for automatic lifecycle management

### Avoiding Hangs
- Don't `await` Gun chains directly (can hang forever)
- Use timeout-protected wrappers

### Reference Handling
- Gun returns references like `{#: 'soul'}` for nested data
- Use validation layer to properly resolve and parse data

### State Synchronization
- **Always update through Gun** for persistence
- Use reactive stores that trigger on Gun updates
- Follow the pattern: UI → Store → Persistence → Gun → Network → Store → UI

## Framework-Specific Patterns

### Svelte 5 Runes (from `.cursor/rules/`)

- `$state()`: Declare reactive state (replaces top-level `let`)
- `$derived()`: Computed values (replaces `$:` reactive declarations)
- `$effect()`: Side effects (replaces `$:` blocks for side effects)
- `$props()`: Component props (replaces `export let`)
- Event handlers: Use properties (`onclick={...}`) not `on:click`

### SvelteKit 2

- `error()` and `redirect()`: Don't throw, just call them
- Cookies require explicit path: `cookies.set(name, value, { path: '/' })`
- Promises not auto-awaited in load functions
- Use `resolveRoute()` instead of deprecated `resolvePath()`

## Critical Implementation Notes

### Contact ID Resolution
- **Before persistence**: Always resolve contact IDs to public keys
- Use `resolveContactIdsInTree()` for trees
- Use `resolveContactIdsInSlotComposition()` for slot compositions
- This ensures Gun stores canonical public keys, not ephemeral contact IDs

### Timestamp Validation
- **Before persisting**: Check if network has newer data via `safelyPersist()`
- **On network updates**: Compare timestamps before accepting updates
- **Loading flags**: Skip persistence while `isLoading*` flags are true

### Stream Lifecycle
- Streams auto-subscribe when dependencies change (via derived stores + debounce)
- Use `updateSubscriptions()` pattern to add/remove streams efficiently
- Clean up streams on component unmount or user logout

### Geocoding Cache
- Capacities with addresses get coordinates via `processCapacitiesLocations()`
- Caching prevents redundant API calls

## Common Patterns

### Reading from Gun
```typescript
// In network.svelte.ts - stream pattern
const config = {
  type: 'dataType',
  streamManager: ownDataStreamManager,
  getGunPath: (userId) => user.get('path'),
  processor: createDataProcessor({
    dataType: 'dataType',
    enableTimestampComparison: true,
    gunTimestampField: 'fieldName',
    timestampKey: 'dataType',
    validator: parseData,
    getCurrentData: () => get(store),
    updateStore: (data) => store.set(data),
    loadingFlag: isLoadingFlag
  }),
  errorHandler: (error) => console.error(error)
};
```

### Writing to Gun
```typescript
// In persistence.svelte.ts - safe persist pattern
export async function persistData() {
  if (!isUserInitialized()) return;
  if (get(isLoadingData)) return;

  const dataValue = get(dataStore);
  if (!dataValue) return;

  // Transform/resolve as needed
  const resolvedData = resolveContactIds(dataValue);
  const dataJson = JSON.stringify(resolvedData);

  await safelyPersist('path', dataJson, lastNetworkTimestamps.dataType, (err) => {
    if (err) console.error('[PERSIST] Error:', err);
    else console.log('[PERSIST] Success');
  });
}
```

### Reactive Calculations
```typescript
// In calculations.svelte.ts
export const derivedValue = derived(
  [dependency1, dependency2],
  ([$dep1, $dep2]) => {
    // Compute derived value
    return computedResult;
  }
);
```

## Testing & Debugging

- Check browser console for `[NETWORK]`, `[PERSIST]`, `[COMPOSE]` prefixed logs
- Monitor Gun relay logs in the terminal running `bun start`
- Use `$inspect()` rune for reactive debugging in Svelte 5
- Gun database stored in browser IndexedDB (use DevTools → Application → IndexedDB)

## Key Files to Understand

1. **`src/lib/state/network.svelte.ts`**: Network sync, stream management, timestamp logic
2. **`src/lib/state/persistence.svelte.ts`**: Write-back logic, timestamp validation
3. **`src/lib/state/core.svelte.ts`**: Central state stores, provider allocations
4. **`src/lib/state/calculations.svelte.ts`**: Reactive calculations (MR, shares)
5. **`src/lib/protocol.ts`**: Core free-association mathematics
6. **`src/lib/schema.ts`**: Zod schemas for all data structures
7. **`src/lib/validation.ts`**: Parsing/validation layer for Gun data
8. **`src/lib/collective.svelte.ts`**: Collective recognition system

## Mathematical Foundation

The system implements these formulas:

```
Mutual-Recognition(You, Them) = min(Your-Recognition-of-Them, Their-Recognition-of-You)

General-Share(You, Provider) =
  MR(You, Provider) / Σ MR(Provider, Each-Contributor)

Allocation(You, Provider, Capacity) =
  Mutual-Fulfillment-Allocation(You, Provider, Capacity)
  // Only mutual desires get fulfilled, normalized by MR among interested parties
```

The protocol ensures that **false recognition naturally decays** while true recognition strengthens through mathematical necessity.
