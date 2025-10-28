# Component V5 Migration - In Progress

## Status

**Completed:**
- ✅ ToolBar.svelte - Fully migrated to v5 stores
- ✅ Parent.svelte - Fully migrated to v5 stores

**Remaining Work:**

### Critical Components
These components still import from deleted `$lib/state/core.svelte` or `$lib/state/gun.svelte` files:

1. **Header.svelte** - Imports: gun, holster, userTree
2. **Capacity.svelte** - Imports: gun (userPub)
3. **Slot.svelte** - Imports: multiple from core.svelte
4. **Map.svelte** - Imports: userNetworkCapacitiesWithSlotQuantities
5. **MapSidePanel.svelte** - Imports: mutualRecognition
6. **Shares.svelte** - Imports: userNetworkCapacitiesWithSlotQuantities
7. **Share.svelte** - Imports: userDesiredSlotComposeFrom, mutualRecognition

### Migration Pattern

For each component, replace:

**OLD (Gun/Legacy)**:
```typescript
import { userTree } from '$lib/state/core.svelte';
import { userCapacities } from '$lib/state/core.svelte';
import { mutualRecognition } from '$lib/state/core.svelte';
import { userPub } from '$lib/state/gun.svelte';
```

**NEW (V5/Holster)**:
```typescript
import { 
  myRecognitionTreeStore as userTree,
  myCapacitySlotsStore,
  networkCommitments,
  getNetworkCommitmentsRecord
} from '$lib/commons/v5/stores.svelte';
import { userPub } from '$lib/state/auth.svelte';  // Already v5
```

### Key V5 Concepts

1. **Slots are first-class** - capacity_slots array, not nested objects
2. **Metadata on slots** - name, emoji, unit live on AvailabilitySlot, not Commitment
3. **Weighted contributors** - `{id, points}` objects, not string arrays
4. **Holster auto-persists** - no manual `.set()` needed for persistence
5. **Network data** - use `networkCommitments` and derived stores

### Next Steps

Continue migrating remaining components following the ToolBar.svelte pattern:
1. Replace imports
2. Create derived stores for backward compatibility where needed
3. Update any direct store mutations to use v5 stores
4. Test for linting errors
5. Verify functionality

## Key Files

- V5 Stores: `$lib/commons/v5/stores.svelte.ts`
- V5 Schemas: `$lib/commons/v5/schemas.ts`
- V5 Protocol: `$lib/commons/v5/protocol.ts`

