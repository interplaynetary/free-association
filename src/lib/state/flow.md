# Modular State Architecture

## Core Reactive Chain (src/lib/state/core.svelte.ts)

- **userTree** (writable) → triggers everything
- **nodesMap** (derived from userTree)
- **contributors** (calculated from tree)
- **recognitionCache** (ourShare + theirShare from network)
- **mutualRecognition** (derived: min of ourShare, theirShare)
- **providerShares** (derived: normalized mutualRecognition)
- **subtreeContributorMap** (derived from userTree + nodesMap)

## Module Breakdown & Dependencies

### 1. Core State (core.svelte.ts)

**Role**: Foundation layer - contains all reactive stores and derived values
**Dependencies**:

- `svelte/store` (writable, derived, get)
- `$lib/protocol` (business logic functions)
- `$lib/schema` (TypeScript types)

**Exports**: All reactive stores and derived values
**Imported by**: All other modules (gun, calculations, network, subscriptions, ui-providers, debug)

**Key Constraint**: Contains derived stores that cannot be exported due to Svelte 5 restrictions

### 2. Gun Database (gun.svelte.ts)

**Role**: Data persistence and authentication layer
**Dependencies**:

- `gun` and related packages
- `core.svelte` (imports stores to update them)
- `$lib/protocol` (createRootNode)
- `$lib/validation` (parsing functions)

**Exports**:

- Gun instance, authentication functions
- Persistence functions (persistTree, persistCapacities, etc.)
- Data loading functions (manifest, loadRecognitionCache)
- User management (getUserName, userNamesCache)

**Imported by**: calculations, network, subscriptions, ui-providers

### 3. Calculations (calculations.svelte.ts)

**Role**: Business logic and recalculation engine
**Dependencies**:

- `core.svelte` (imports ALL stores for read/write)
- `gun.svelte` (imports persistence functions)
- `$lib/protocol` (business logic functions)

**Exports**:

- Main recalculation functions (recalculateFromTree, recalculateFromCapacities)
- Recognition cache management functions
- Recipient shares calculation

**Imported by**: subscriptions, network, debug

### 4. Network (network.svelte.ts)

**Role**: External data synchronization layer
**Dependencies**:

- `core.svelte` (imports contributors, recognitionCache stores)
- `gun.svelte` (imports gun instance, user, persistRecognitionCache)
- `calculations.svelte` (imports updateRecognitionCache function)

**Exports**:

- Network subscription functions
- SOGF data fetching from other users
- Mutual recognition updates from network

**Imported by**: debug
**Side Effects**: Auto-subscribes to contributor changes via store subscriptions

### 5. Subscriptions (subscriptions.svelte.ts)

**Role**: Reactive orchestration layer
**Dependencies**:

- `core.svelte` (imports stores and loading flags)
- `gun.svelte` (imports persistence functions)
- `calculations.svelte` (imports recalculation functions)
- `$lib/schema` (Node type)

**Exports**: None (side-effects only)
**Side Effects**:

- Auto-triggers recalculations on store changes
- Implements debouncing (300ms)
- Forces immediate persistence on changes

### 6. UI Providers (ui-providers.svelte.ts)

**Role**: UI data transformation layer
**Dependencies**:

- `core.svelte` (imports userTree, nodesMap, recipientSharesMap)
- `gun.svelte` (imports usersList)
- `$lib/protocol` (getSubtreeContributorMap)

**Exports**:

- Dropdown data providers (users, subtrees)
- Generic dropdown interfaces
- User shares lookup functions

**Imported by**: UI components

### 7. Debug (debug.svelte.ts)

**Role**: Development utilities layer
**Dependencies**:

- `core.svelte` (imports all stores for debugging)
- `network.svelte` (imports subscription functions)

**Exports**:

- Debug state inspection functions
- Manual subscription triggers
- Global window function exposure

**Imported by**: None (used via global window functions)

### 8. Index (index.svelte.ts)

**Role**: Public API and activation layer
**Dependencies**: ALL other modules
**Exports**: Re-exports everything from all modules
**Side Effects**: Activates subscriptions and network by importing them

## Dependency Hierarchy

```
Level 0 (Foundation):
├── core.svelte.ts (reactive stores & derived values)

Level 1 (Data Layer):
├── gun.svelte.ts (persistence & auth)
│   └── depends on: core

Level 2 (Logic Layer):
├── calculations.svelte.ts (business logic)
│   └── depends on: core, gun
├── network.svelte.ts (external sync)
│   └── depends on: core, gun, calculations

Level 3 (Orchestration Layer):
├── subscriptions.svelte.ts (reactive triggers)
│   └── depends on: core, gun, calculations
├── ui-providers.svelte.ts (UI data)
│   └── depends on: core, gun

Level 4 (Utilities Layer):
├── debug.svelte.ts (development tools)
│   └── depends on: core, network

Level 5 (Public API):
├── index.svelte.ts (unified exports)
│   └── depends on: ALL modules
```

## Data Flow with Module Responsibilities

```
userTree (change in core.svelte.ts)
    ↓
subscriptions.svelte.ts (detects change, debounces)
    ↓
calculations.svelte.ts (recalculateFromTree)
    ↓
core.svelte.ts (updates contributors, userSogf, recognitionCache)
    ↓
core.svelte.ts (derived: mutualRecognition, providerShares)
    ↓
gun.svelte.ts (persistence via calculations)
    ↓
network.svelte.ts (subscribes to contributors' SOGF)
    ↓
core.svelte.ts (updates recognitionCache with theirShare)
    ↓
ui-providers.svelte.ts (transforms for UI consumption)
    ↓
UI Components (reactive updates)
```

## Circular Dependency Prevention

**Resolved Dependencies**:

- `persistRecognitionCache()` moved from calculations → gun to break cycle
- Network imports it from gun instead of calculations
- Clear hierarchy prevents circular imports

**Import Rules**:

- Lower levels cannot import from higher levels
- Core is imported by all but imports none
- Index imports all but is imported by none

## Benefits of Modular Structure

1. **Separation of Concerns**: Each module has a clear responsibility
2. **Svelte 5 Compliance**: Derived stores stay in core module to avoid export restrictions
3. **Maintainability**: Easier to find and modify specific functionality
4. **Testing**: Each module can be tested independently
5. **Circular Dependency Prevention**: Clear dependency hierarchy
6. **Code Organization**: Related functionality grouped together
7. **Dependency Clarity**: Easy to understand what each module needs and provides
