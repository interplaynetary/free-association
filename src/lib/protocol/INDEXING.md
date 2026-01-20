# H3 Spatial-Temporal Indexing Guide

This document outlines the capabilities, use cases, and integration strategy for the H3-based indexing system in Free Association.

## 🚀 Capabilities

The `SlotIndex` class provides a three-tier indexing architecture:

1.  **Type Tier** (`TypeIndex`): O(1) exact lookup by resource type.
2.  **Spatial Tier** (`SpatialIndex`): O(k) geographic lookup using H3 hexagonal cells.
3.  **Temporal Tier** (`TemporalIndex`): O(M+R) time-based lookup using sparse month buckets and recurrence patterns.

### Performance Impact
- **Brute Force**: O(N × M) comparisons.
- **Indexed**: O(1 + k(M+R)) candidate generation + O(K) verification.
- **Speedup**: Estimated **~13,000x** faster for large datasets (e.g., 10M items).

---

## 💡 Core Use Cases

### 1. ⚡️ Supercharged Matching
**Problem**: The legacy matcher iterates through *every* capacity slot for *every* need slot to find matches.
**Solution**: Use the index to instantly retrieve only the relevant candidates.

```typescript
// Legacy (Slow)
const matches = capacitySlots.filter(cap => slotsCompatible(need, cap));

// Indexed (Fast)
const index = buildSlotIndex(capacitySlots);
const candidates = index.query(need); // Returns only spatially/temporally relevant slots
const matches = candidates.filter(cap => slotsCompatible(need, cap)); // Verify remaining complex logic
```

### 2. 🗺️ Map-Based Discovery
**Problem**: Rendering thousands of pins on a map is slow and cluttered.
**Solution**: Query the index by viewport or generate heatmaps.

- **Viewport Query**: Calculate the H3 cells visible in the user's map view and query the index for those specific cells.
- **Heatmaps**: Since the index stores slots in buckets (cells), we can return *counts* per cell instantly without loading the actual slot data.

```typescript
// meaningful pseudocode
const visibleCells = h3.polygonToCells(mapViewportPolygon, resolution);
const visibleSlots = visibleCells.flatMap(cell => index.spatialIndex.get(cell));
```

### 3. 📡 Distributed Sync / "Fog of War"
**Problem**: Syncing the entire global state to every user device is unscalable.
**Solution**: Users subscribe only to the H3 cells relevant to them.

- **Geographic Subscription**: "Sync all data in cell `85283473fffffff` and its 1-ring neighbors."
- **Moving Interest**: As the user moves across the map, they unsubscribe from old cells and subscribe to new ones.
- **Data Partitioning**: The server (or P2P network) can shard data deterministically by H3 index.

---

## 🔍 Deep Analysis: Integration with Matching Engine

The `SlotIndex` is designed to replace the **Broad Phase** of matching, but NOT the **Narrow Phase**.

### The Matching Pipeline

| Phase | Responsibility | Handled By | Logic |
|-------|----------------|------------|-------|
| **1. Broad Phase** | Candidate Generation | **`SlotIndex`** | "Find all slots that are roughly in the same place (H3) and same time (Month/Recurrence) and same type." |
| **2. Narrow Phase** | Verification | `slotsCompatible()` | "Do the exact start/end times overlap? Is the distance within 50km precise? Is the quantity sufficient?" |
| **3. Policy Phase** | Eligibility | `isEligible()` | "Is this user allowed to access this slot? Are they in the correct network/group?" |

### What `SlotIndex` Replaces
*   **Iterating the whole array**: No longer need `capacitySlots.filter(...)`.
*   **Gross Spatial Checks**: Replaces checking `country === country` or large distance checks.
*   **Gross Temporal Checks**: Replaces checking "is this in the same year/month".

### What `SlotIndex` Does NOT Replace
*   **Fine-Grained Geometry**: The H3 index gives you everything in the cell (and neighbors). It does *not* strictly enforce the "50km radius" circle. It gives you a set of hexagons that *cover* that circle. The corners of those hexagons might be 55km away. You still need `haversineDistance()` to trim the edges.
*   **Exact Time Overlaps**: The index puts slots in "Month Buckets". It knows "Slot A is in June". It does not know "Slot A represents 2:00 PM - 3:00 PM on June 15th". You still need `timeRangesOverlap()`.
*   **Quantity & Divisibility**: The index does not filter by `quantity >= needed`.
*   **Network Permissions**: The index is purely distinct properties of the slot (Space/Time/Type). It knows nothing about who *owns* the slot or who is *allowed* to see it.

### Recommendation for Implementation

**Do NOT rewrite `slotsCompatible`**. Instead, wrap it.

```typescript
// Proposed high-performance matcher function
function findMatchesEfficiently(need: NeedSlot, index: SlotIndex): AvailabilitySlot[] {
    // 1. Broad Phase (Index) - Reduces 10,000 items to ~50 items
    const candidates = index.query(need);
    
    // 2. Narrow Phase (Legacy Logic) - Verifies the 50 items
    return candidates.filter(candidate => {
        // This function remains the "Source of Truth" for complex logic
        return slotsCompatible(need, candidate); 
    });
}
```

This approach is:
1.  **Safe**: It cannot produce *false positives* because the legacy logic still verifies everything.
2.  **Fast**: It eliminates 99.9% of the work before the slow logic runs.
3.  **Maintainable**: You don't duplicate logic. `SlotIndex` focuses on retrieval, `match.ts` focuses on business rules.
