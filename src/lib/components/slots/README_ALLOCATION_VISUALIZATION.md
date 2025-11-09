# Slot Allocation Visualization Components

## Overview

New components for visualizing slot allocations in the Free-Association protocol, showing both the distribution of capacity and satisfaction of needs using the existing `Bar.svelte` component with color-coded segments.

## Components

### 1. `SlotAllocationBar.svelte`

**Compact satisfaction/distribution bar visualization**

- **For Need Slots**: Shows satisfaction percentage from each provider
  - Colored segments for each provider (using `getColorForUserId`)
  - Grey segment for unsatisfied portion
  - Status text with emoji indicators (🙏 for unmet needs, ✅ for satisfied)
  
- **For Capacity Slots**: Shows distribution percentage to each recipient
  - Colored segments for each recipient
  - Grey segment for unused capacity
  - Status text showing allocated/total

**Features**:
- Uses `Bar.svelte` with interactive hover labels (`showLabelsAboveOnSelect`)
- Real-time reactivity from network commitments
- Tier badges (🤝 mutual, 💝 generous)
- Self-allocation support (includes self-care allocations)

### 2. `SlotAllocationDetails.svelte`

**Expandable allocation details with data table**

Combines the compact bar visualization with a detailed expandable list of all allocations.

**Compact View**:
- Shows `SlotAllocationBar` component
- Expand/collapse toggle button

**Expanded View**:
- Data table with columns:
  - Provider/Recipient name (with "You" tag for self)
  - Amount allocated
  - Percentage of total
  - Tier badge (🤝 mutual / 💝 generous)
- Total row showing sum and percentage
- Sort by quantity (descending)

**Features**:
- Names from user cache (contacts > aliases > truncated pubkeys)
- Self-allocation highlighted with purple "You" badge
- Dark mode support
- Responsive design

### 3. `Bar.svelte` Updates

Enhanced to handle special segment IDs:

- `__unsatisfied__`: Rendered as grey (#d1d5db) for unmet needs/unused capacity
- Label: "Unsatisfied" when hovered
- Skips name lookup for special segments (starting with `__`)

## Usage in Home Page

```svelte
<script>
  import { SlotAllocationDetails } from '$lib/components/slots';
  import { holsterUserPub } from '$lib/network/holster.svelte';
</script>

<!-- For Need Slots -->
<SlotAllocationDetails 
  slot={needSlot} 
  isCapacity={false} 
  myPubKey={$holsterUserPub} 
/>

<!-- For Capacity Slots -->
<SlotAllocationDetails 
  slot={capacitySlot} 
  isCapacity={true} 
  myPubKey={$holsterUserPub} 
/>
```

## Integration

Added to `/routes/+page.svelte` inventory view:
- Each need slot shows incoming allocations (who's providing)
- Each capacity slot shows outgoing allocations (who's receiving)
- Compact by default, expandable for details
- Real-time updates as allocations change

## Data Flow

```
Network Commitments → SlotAllocationDetails
                           ↓
                    [Computes allocations]
                           ↓
                    SlotAllocationBar
                           ↓
                    Bar.svelte (with special segments)
                           ↓
                    Visual satisfaction/distribution bar
```

**Reactive Sources**:
- `$myCommitmentStore` - Your own allocations (outgoing, self-care)
- `$networkAllocations` - Others' allocations to you (incoming)
- `$userNamesOrAliasesCache` - Provider/recipient names

## Self-Care Support

Both components correctly handle self-allocations:
- For needs: Includes allocations from your own capacity to your own needs
- For capacity: Shows self-allocations as recipients
- Special "You" badge for self in detailed view

## Color Palette

- **Providers/Recipients**: Consistent colors via `getColorForUserId()`
- **Unsatisfied/Unused**: Grey (#d1d5db)
- **Mutual Tier**: Blue accent (#0ea5e9)
- **Generous Tier**: Yellow accent (#eab308)
- **Self**: Purple accent (#7c3aed)
- **Satisfied**: Green (#16a34a)

## Design Philosophy

1. **Progressive Disclosure**: Compact bar by default, expand for details
2. **Color Consistency**: Same colors across all visualizations
3. **Real-time**: Reactive to network changes
4. **Self-Care First**: Self-allocation is valid and visible
5. **Transparency**: Show exactly who's providing/receiving what
6. **Trust Building**: Visual confirmation of mutual aid flows

