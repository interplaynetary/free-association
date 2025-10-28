# Free Association V5 - Svelte Component Architecture

## User Flow & Components

### 📍 **Phase 3: Slot Editor (Shared Abstraction)**

**User Flow:**
1. User clicks "Add Need" or "Add Capacity"
2. Fills out slot form (shared between needs/capacity!)
3. System validates and saves
4. User sees list of slots

**SHARED COMPONENTS** (work for both needs & capacity):
```
components/slots/
├── SlotEditor.svelte                # Generic slot CRUD
│   ├── SlotForm.svelte              # THE BIG ONE - shared form
│   │   ├── BasicInfo.svelte         # Name, emoji, description, type
│   │   ├── QuantityInput.svelte     # Quantity + unit
│   │   ├── TimeSelector.svelte      # Dates, recurrence, windows
│   │   │   ├── DateRangeInput.svelte
│   │   │   ├── RecurrenceSelect.svelte
│   │   │   └── AvailabilityWindow.svelte
│   │   │       ├── TimeRangeInput.svelte      # HH:MM - HH:MM
│   │   │       ├── DayScheduleEditor.svelte   # Days + times
│   │   │       ├── WeekScheduleEditor.svelte  # Weeks + days + times
│   │   │       └── MonthScheduleEditor.svelte # Months + weeks + days
│   │   ├── LocationSelector.svelte  # Address, coords, online
│   │   │   ├── AddressInput.svelte
│   │   │   ├── MapPicker.svelte
│   │   │   └── OnlineLinkInput.svelte
│   │   ├── FilterRuleEditor.svelte  # Bilateral filters
│   │   └── AdvancedOptions.svelte   # Divisibility, notice, priority
│   │
│   ├── SlotCard.svelte              # Display single slot
│   │   ├── SlotHeader.svelte        # Name, emoji, quantity
│   │   ├── SlotTime.svelte          # When (formatted)
│   │   ├── SlotLocation.svelte      # Where (formatted)
│   │   └── SlotActions.svelte       # Edit, delete buttons
│   │
│   └── SlotsList.svelte             # List all slots
│       ├── SlotsGrid.svelte         # Grid view
│       ├── SlotsTable.svelte        # Table view
│       └── SlotsFilter.svelte       # Filter by type, date, location
│
└── NeedTypeSelector.svelte          # Pick need type (food, housing, etc)
```

**Usage:**
```svelte
<!-- For Needs -->
<SlotEditor 
  mode="need" 
  store={myNeedSlotsStore} 
  schema={NeedSlotSchema}
/>

<!-- For Capacity -->
<SlotEditor 
  mode="capacity" 
  store={myCapacitySlotsStore} 
  schema={AvailabilitySlotSchema}
/>
```

**Schemas Used:**
- `NeedSlotSchema` (almost identical to AvailabilitySlotSchema!)
- `AvailabilitySlotSchema`
- `AvailabilityWindowSchema`
- `TimeRangeSchema`
- `DayScheduleSchema`
- `WeekScheduleSchema`
- `MonthScheduleSchema`
- `NeedTypeSchema`

**Stores:**
- `myNeedSlotsStore` (write)
- `myCapacitySlotsStore` (write)

---

### 🤝 **Phase 4: Network View**

**User Flow:**
1. User sees list of people in their recognition tree
2. System auto-subscribes to their commitments
3. User sees mutual recognition with each person
4. User can view their needs/capacity
5. User sees convergence status

**Components:**
```
components/network/
├── NetworkDashboard.svelte          # Main network view
│   ├── ParticipantsList.svelte      # All subscribed participants
│   │   ├── ParticipantCard.svelte   # Single participant
│   │   │   ├── MutualRecognitionBar.svelte  # MR(me, them)
│   │   │   ├── NeedsPreview.svelte  # Their needs (summary)
│   │   │   ├── CapacityPreview.svelte # Their capacity (summary)
│   │   │   └── ConvergenceStatus.svelte # Are their needs met?
│   │   └── ParticipantFilter.svelte # Filter/search
│   │
│   ├── MutualRecognitionMatrix.svelte # Full MR matrix (heatmap)
│   │
│   └── SubscriptionStats.svelte     # Network health stats
│
└── ParticipantDetail.svelte         # Drill-down view
    ├── ParticipantHeader.svelte     # Name, pub key, MR
    ├── CommitmentView.svelte        # Their full commitment
    │   ├── SlotsList.svelte         # Reuse from Phase 3!
    │   └── RecognitionWeightsView.svelte # Reuse from Phase 2!
    └── TreeVisualization.svelte     # Their tree (if subscribed)
```

**Schemas Used:**
- `Commitment`
- `GlobalRecognitionWeights`
- All slot schemas (for display)

**Stores:**
- `networkCommitments` (read)
- `networkRecognitionTrees` (read - optional)
- `myMutualRecognition` (read)
- `networkRecognitionWeights` (read)# Free Association V5 - Svelte Component Architecture

## User Flow & Components

### 📍 **Phase 3: Slot Editor (Shared Abstraction)**

**User Flow:**
1. User clicks "Add Need" or "Add Capacity"
2. Fills out slot form (shared between needs/capacity!)
3. System validates and saves
4. User sees list of slots

**SHARED COMPONENTS** (work for both needs & capacity):
```
components/slots/
├── SlotEditor.svelte                # Generic slot CRUD
│   ├── SlotForm.svelte              # THE BIG ONE - shared form
│   │   ├── BasicInfo.svelte         # Name, emoji, description, type
│   │   ├── QuantityInput.svelte     # Quantity + unit
│   │   ├── TimeSelector.svelte      # Dates, recurrence, windows
│   │   │   ├── DateRangeInput.svelte
│   │   │   ├── RecurrenceSelect.svelte
│   │   │   └── AvailabilityWindow.svelte
│   │   │       ├── TimeRangeInput.svelte      # HH:MM - HH:MM
│   │   │       ├── DayScheduleEditor.svelte   # Days + times
│   │   │       ├── WeekScheduleEditor.svelte  # Weeks + days + times
│   │   │       └── MonthScheduleEditor.svelte # Months + weeks + days
│   │   ├── LocationSelector.svelte  # Address, coords, online
│   │   │   ├── AddressInput.svelte
│   │   │   ├── MapPicker.svelte
│   │   │   └── OnlineLinkInput.svelte
│   │   ├── FilterRuleEditor.svelte  # Bilateral filters
│   │   └── AdvancedOptions.svelte   # Divisibility, notice, priority
│   │
│   ├── SlotCard.svelte              # Display single slot
│   │   ├── SlotHeader.svelte        # Name, emoji, quantity
│   │   ├── SlotTime.svelte          # When (formatted)
│   │   ├── SlotLocation.svelte      # Where (formatted)
│   │   └── SlotActions.svelte       # Edit, delete buttons
│   │
│   └── SlotsList.svelte             # List all slots
│       ├── SlotsGrid.svelte         # Grid view
│       ├── SlotsTable.svelte        # Table view
│       └── SlotsFilter.svelte       # Filter by type, date, location
│
└── NeedTypeSelector.svelte          # Pick need type (food, housing, etc)
```

**Usage:**
```svelte
<!-- For Needs -->
<SlotEditor 
  mode="need" 
  store={myNeedSlotsStore} 
  schema={NeedSlotSchema}
/>

<!-- For Capacity -->
<SlotEditor 
  mode="capacity" 
  store={myCapacitySlotsStore} 
  schema={AvailabilitySlotSchema}
/>
```

**Schemas Used:**
- `NeedSlotSchema` (almost identical to AvailabilitySlotSchema!)
- `AvailabilitySlotSchema`
- `AvailabilityWindowSchema`
- `TimeRangeSchema`
- `DayScheduleSchema`
- `WeekScheduleSchema`
- `MonthScheduleSchema`
- `NeedTypeSchema`

**Stores:**
- `myNeedSlotsStore` (write)
- `myCapacitySlotsStore` (write)

---

### 🤝 **Phase 4: Network View**

**User Flow:**
1. User sees list of people in their recognition tree
2. System auto-subscribes to their commitments
3. User sees mutual recognition with each person
4. User can view their needs/capacity
5. User sees convergence status

**Components:**
```
components/network/
├── NetworkDashboard.svelte          # Main network view
│   ├── ParticipantsList.svelte      # All subscribed participants
│   │   ├── ParticipantCard.svelte   # Single participant
│   │   │   ├── MutualRecognitionBar.svelte  # MR(me, them)
│   │   │   ├── NeedsPreview.svelte  # Their needs (summary)
│   │   │   ├── CapacityPreview.svelte # Their capacity (summary)
│   │   │   └── ConvergenceStatus.svelte # Are their needs met?
│   │   └── ParticipantFilter.svelte # Filter/search
│   │
│   ├── MutualRecognitionMatrix.svelte # Full MR matrix (heatmap)
│   │
│   └── SubscriptionStats.svelte     # Network health stats
│
└── ParticipantDetail.svelte         # Drill-down view
    ├── ParticipantHeader.svelte     # Name, pub key, MR
    ├── CommitmentView.svelte        # Their full commitment
    │   ├── SlotsList.svelte         # Reuse from Phase 3!
    │   └── RecognitionWeightsView.svelte # Reuse from Phase 2!
    └── TreeVisualization.svelte     # Their tree (if subscribed)
```

**Schemas Used:**
- `Commitment`
- `GlobalRecognitionWeights`
- All slot schemas (for display)

**Stores:**
- `networkCommitments` (read)
- `networkRecognitionTrees` (read - optional)
- `myMutualRecognition` (read)
- `networkRecognitionWeights` (read)

---

### 🎯 **Phase 5: Allocation View**

**User Flow:**
1. User sees what they're receiving (as recipient)
2. User sees what they're providing (as provider)
3. Breakdown by need type
4. Tier 1 (mutual) vs Tier 2 (non-mutual)
5. Real-time updates as network changes

**Components:**
```
components/allocation/
├── AllocationDashboard.svelte       # Main view
│   ├── AsRecipientView.svelte       # What I'm receiving
│   │   ├── AllocationsByType.svelte # Per need type
│   │   │   ├── TypeSection.svelte   # Single type
│   │   │   │   ├── TierBreakdown.svelte # Tier 1 vs 2
│   │   │   │   └── ProvidersList.svelte # Who's giving me X
│   │   │   └── AllocationCard.svelte # Single allocation
│   │   └── TotalReceivedSummary.svelte
│   │
│   ├── AsProviderView.svelte        # What I'm providing
│   │   ├── AllocationsBySlot.svelte # Per capacity slot
│   │   │   ├── SlotHeader.svelte    # Reuse from Phase 3
│   │   │   ├── RecipientsList.svelte # Who I'm allocating to
│   │   │   └── DenominatorDisplay.svelte # MR vs NonMR denominators
│   │   └── TotalGivenSummary.svelte
│   │
│   └── AllocationToggle.svelte      # Switch between recipient/provider
│
└── AllocationDetail.svelte          # Single allocation drill-down
    ├── AllocationMeta.svelte        # Quantity, type, tier
    ├── CompatibilityInfo.svelte     # Time/location match
    └── SlotPair.svelte              # Show both need & capacity slots
```

**Schemas Used:**
- `SlotAllocationRecord`
- All slot schemas (for context)

**Stores:**
- `myAllocationsAsProvider` (read - derived)
- Could create `myAllocationsAsRecipient` derived store

---

### 📊 **Phase 6: System Monitoring**

**User Flow:**
1. User sees convergence metrics
2. Tracks iterations to convergence
3. Views system-wide state
4. Monitors ITC causality

**Components:**
```
components/monitoring/
├── ConvergenceDashboard.svelte      # Main monitoring view
│   ├── ConvergenceMetrics.svelte    # Core metrics
│   │   ├── FrobeniusNorm.svelte     # System magnitude
│   │   ├── ContractionRate.svelte   # Convergence speed
│   │   ├── IterationsRemaining.svelte
│   │   └── PercentSatisfied.svelte
│   │
│   ├── PerTypeMetrics.svelte        # Per need type breakdown
│   │   └── TypeMetricCard.svelte    # Single type's convergence
│   │
│   ├── ConvergenceChart.svelte      # Time series graph
│   │   └── Chart.svelte             # D3/Chart.js wrapper
│   │
│   ├── SystemState.svelte           # Network-wide view
│   │   ├── ParticipantCount.svelte
│   │   ├── TotalNeeds.svelte
│   │   ├── TotalCapacity.svelte
│   │   └── NeedVariance.svelte      # Distribution inequality
│   │
│   └── ITCStatus.svelte             # Causality tracking
│       ├── MyStamp.svelte           # My ITC stamp
│       ├── StaleUpdates.svelte      # Rejected updates
│       └── MergedStamps.svelte      # Concurrent updates handled
│
└── DebugPanel.svelte                # Developer tools
    ├── StoreInspector.svelte        # Inspect store contents
    ├── LogViewer.svelte             # Console logs
    └── VersionedStoreDebug.svelte   # Field versions, ITC stamps
```

**Schemas Used:**
- `ConvergenceMetrics`
- `PerTypeConvergenceMetrics`
- `SystemState`
- `ITCStamp`

**Stores:**
- All stores (for inspection)
- `myAllocationsAsProvider.convergence` (derived field)

---

## 🔧 **Shared Utilities**

These components are used across multiple phases:

```
components/shared/
├── forms/
│   ├── TextInput.svelte             # Basic text input
│   ├── NumberInput.svelte           # Number with validation
│   ├── SelectInput.svelte           # Dropdown
│   ├── DateInput.svelte             # Date picker
│   ├── TimeInput.svelte             # Time picker (HH:MM)
│   ├── CheckboxInput.svelte         # Boolean toggle
│   ├── RadioGroup.svelte            # Multiple choice
│   └── FormField.svelte             # Wrapper with label/error
│
├── display/
│   ├── Card.svelte                  # Generic card container
│   ├── Badge.svelte                 # Status/label badge
│   ├── Avatar.svelte                # User avatar
│   ├── Icon.svelte                  # Icon wrapper
│   ├── Tooltip.svelte               # Hover info
│   ├── Modal.svelte                 # Modal dialog
│   └── EmptyState.svelte            # No data placeholder
│
├── navigation/
│   ├── Tabs.svelte                  # Tab navigation
│   ├── Breadcrumbs.svelte           # Breadcrumb trail
│   └── Sidebar.svelte               # Side navigation
│
└── feedback/
    ├── LoadingSpinner.svelte        # Loading state
    ├── ErrorMessage.svelte          # Error display
    ├── SuccessMessage.svelte        # Success feedback
    └── ProgressBar.svelte           # Progress indicator
```

---

## 📁 **Final Component Tree**

```
src/lib/commons/v5/components/
├── slots/                   # Phase 3 (SHARED between needs/capacity)
├── network/                 # Phase 4
├── allocation/              # Phase 5
├── monitoring/              # Phase 6
└── shared/                  # Utilities
```

---

## 🔑 **Key Abstractions**

### 1. **Slot Editor (Needs & Capacity)**
- **99% identical schemas** → Use same components
- Pass `mode="need" | "capacity"` prop
- Schema validation differs slightly, but form is identical
- Most complex component (location, time, recurrence)

### 2. **Display Components**
- `RecognitionWeightsView` - Used in both tree editor and network view
- `SlotsList` - Used for my slots, network slots, allocations
- `TreeVisualization` - Used for my tree and others' trees

### 3. **Form Components**
- All shared utilities in `components/shared/forms/`
- Consistent validation, styling, error handling

### 4. **Data Visualization**
- Chart components reused for convergence, needs over time, etc.
- D3-based tree visualization for recognition trees

---

## 🎯 **Development Priority**

**Phase 1 (MVP):**
3. ✅ Slot Editor (needs only, basic form)
4. ✅ Network View (basic list)
5. ✅ Allocation View (recipient only)

**Phase 2 (Full Features):**
6. Slot Editor (advanced: time windows, location, filters)
7. Allocation View (provider view, tier breakdown)
8. Monitoring Dashboard

**Phase 3 (Polish):**
9. Convergence visualization
10. Debug panel
11. Advanced tree visualization

---

## 📊 **Component Reusability Matrix**

| Component | Used In |
|-----------|---------|
| `SlotForm` | Needs Editor, Capacity Editor |
| `SlotCard` | My Needs, My Capacity, Network View, Allocations |
| `SlotsList` | All of the above |
| `RecognitionWeightsView` | Tree Editor, Network View, Participant Detail |
| `TreeVisualization` | My Tree, Others' Trees |
| `MutualRecognitionBar` | Network List, Allocation Details |
| `ConvergenceStatus` | Network View, Monitoring Dashboard |
| `ParticipantCard` | Network View, Allocation Recipients |
| Form utilities | Everywhere |

**Reusability Score: ~40%** of components are shared across multiple contexts!

---

## 🎨 **Svelte Component Best Practices**

### ✨ **Principle 1: You Almost Never Need `$effect`**

Svelte's reactivity is declarative. Most of the time, `$derived` is what you want.

❌ **DON'T:**
```svelte
<script lang="ts">
  let count = $state(0);
  let doubled = $state(0);
  
  $effect(() => {
    doubled = count * 2; // BAD: Using effect for derived state
  });
</script>
```

✅ **DO:**
```svelte
<script lang="ts">
  let count = $state(0);
  let doubled = $derived(count * 2); // GOOD: Declarative derived state
</script>
```

**When you ACTUALLY need `$effect`:**
- Side effects (logging, analytics, external APIs)
- DOM measurements (after render)
- Cleanup (unsubscribe, clear intervals)

```svelte
<script lang="ts">
  import { myStore } from './stores.svelte';
  
  let data = $state([]);
  
  // Valid use case: Subscribing to a store
  $effect(() => {
    const unsubscribe = myStore.subscribe(value => {
      data = value;
    });
    
    return () => unsubscribe(); // Cleanup
  });
  
  // Better: Use store directly in template
  // let data = $derived(get(myStore));
</script>
```

---

### 🚫 **Principle 2: Avoid Direct DOM Manipulation**

Use Svelte's template syntax instead of imperatively manipulating the DOM.

❌ **DON'T:**
```svelte
<script lang="ts">
  let isOpen = $state(false);
  
  $effect(() => {
    const modal = document.getElementById('modal');
    if (isOpen) {
      modal.style.display = 'block'; // BAD: Direct DOM manipulation
    } else {
      modal.style.display = 'none';
    }
  });
</script>

<div id="modal">...</div>
```

✅ **DO:**
```svelte
<script lang="ts">
  let isOpen = $state(false);
</script>

{#if isOpen}
  <div class="modal">...</div>
{/if}

<!-- Or with CSS -->
<div class="modal" class:open={isOpen}>...</div>

<style>
  .modal { display: none; }
  .modal.open { display: block; }
</style>
```

**When DOM access IS needed:**
- Measuring elements (use `bind:this` with `$effect`)
- Third-party libraries (D3, Chart.js)
- Focus management

```svelte
<script lang="ts">
  let inputElement = $state<HTMLInputElement>();
  
  function focusInput() {
    inputElement?.focus(); // Valid use case
  }
</script>

<input bind:this={inputElement} />
```

---

### 📦 **Principle 3: Props for Component Communication**

Use props for parent → child communication. Use callbacks for child → parent.

✅ **GOOD Component API:**
```svelte
<!-- SlotCard.svelte -->
<script lang="ts">
  import type { NeedSlot } from '../schemas';
  
  interface Props {
    slot: NeedSlot;              // Data in
    readonly?: boolean;          // Configuration
    onEdit?: (slot: NeedSlot) => void;  // Action out
    onDelete?: (id: string) => void;    // Action out
  }
  
  let { slot, readonly = false, onEdit, onDelete }: Props = $props();
</script>

<div class="slot-card">
  <h3>{slot.name}</h3>
  <p>{slot.quantity} {slot.unit}</p>
  
  {#if !readonly}
    <button onclick={() => onEdit?.(slot)}>Edit</button>
    <button onclick={() => onDelete?.(slot.id)}>Delete</button>
  {/if}
</div>
```

**Usage:**
```svelte
<SlotCard 
  slot={mySlot}
  onEdit={handleEdit}
  onDelete={handleDelete}
/>
```

**Prop Patterns:**
- ✅ Use TypeScript interfaces for props
- ✅ Provide sensible defaults
- ✅ Optional callbacks with `?:`
- ✅ Use `readonly` for display-only modes
- ✅ Destructure props with `$props()`

---

### 🌐 **Principle 4: Global State in `global.svelte.ts`**

For state shared across many components, create a global state file.

**File: `src/lib/commons/v5/global.svelte.ts`**
```typescript
/**
 * Global Application State
 * 
 * Use for:
 * - UI state (modals, sidebars, theme)
 * - Navigation state
 * - User preferences
 * - Toast notifications
 * 
 * DON'T use for:
 * - Domain data (use stores.svelte.ts instead)
 * - Form state (keep local to component)
 */

interface Toast {
  id: string;
  message: string;
  type: 'success' | 'error' | 'info';
}

// UI State
export const sidebarOpen = $state(true);
export const currentModal = $state<string | null>(null);
export const theme = $state<'light' | 'dark'>('light');

// Toast Notifications
const toasts = $state<Toast[]>([]);

export const toastState = {
  get items() { return toasts; },
  
  add(message: string, type: Toast['type'] = 'info') {
    const id = crypto.randomUUID();
    toasts.push({ id, message, type });
    setTimeout(() => this.remove(id), 5000);
  },
  
  remove(id: string) {
    const index = toasts.findIndex(t => t.id === id);
    if (index !== -1) toasts.splice(index, 1);
  },
  
  clear() {
    toasts.length = 0;
  }
};

// Navigation State
export const currentView = $state<'tree' | 'needs' | 'capacity' | 'network' | 'allocation'>('tree');

// User Preferences (persisted to localStorage)
export const userPrefs = $state({
  showHints: true,
  compactMode: false,
  defaultNeedType: 'food'
});

// Load preferences from localStorage on init
if (typeof localStorage !== 'undefined') {
  const saved = localStorage.getItem('userPrefs');
  if (saved) {
    Object.assign(userPrefs, JSON.parse(saved));
  }
}
```

**Usage in Components:**
```svelte
<script lang="ts">
  import { sidebarOpen, toastState, currentView } from '$lib/commons/v5/global.svelte';
  
  function handleSave() {
    // ... save logic
    toastState.add('Saved successfully!', 'success');
  }
</script>

{#if sidebarOpen}
  <aside>...</aside>
{/if}

<button onclick={() => sidebarOpen = !sidebarOpen}>
  Toggle Sidebar
</button>
```

**When to use `global.svelte.ts` vs props:**
- Use **global** for: UI chrome, app-wide state, cross-cutting concerns
- Use **props** for: Component-specific data, parent-child relationships

---

### 🧩 **Principle 5: Component Composition Over Configuration**

Build small, focused components and compose them together.

❌ **DON'T: Mega-component with all options**
```svelte
<!-- BAD: One component does everything -->
<SlotDisplay 
  slot={slot}
  mode="card"
  showTime={true}
  showLocation={true}
  showActions={true}
  editable={true}
  onEdit={...}
  onDelete={...}
  onDuplicate={...}
  compact={false}
  highlight={false}
/>
```

✅ **DO: Compose smaller components**
```svelte
<!-- GOOD: Compose what you need -->
<SlotCard slot={slot}>
  <SlotHeader name={slot.name} emoji={slot.emoji} />
  <SlotTime time={slot.start_date} recurrence={slot.recurrence} />
  <SlotLocation location={slot.city} online={slot.online_link} />
  <SlotActions onEdit={handleEdit} onDelete={handleDelete} />
</SlotCard>

<!-- Or simpler version -->
<SlotCard slot={slot}>
  <SlotHeader name={slot.name} emoji={slot.emoji} />
  <p>{slot.quantity} {slot.unit}</p>
</SlotCard>
```

**Benefits:**
- Easier to understand
- More flexible
- Better tree-shaking
- Easier testing

---

### ⚡ **Principle 6: Use Svelte's Reactivity, Not Workarounds**

Trust Svelte's reactivity system. Don't fight it.

❌ **DON'T:**
```svelte
<script lang="ts">
  let items = $state([1, 2, 3]);
  let total = $state(0);
  
  function addItem(item: number) {
    items.push(item);
    items = items; // BAD: Forcing reactivity
    updateTotal(); // BAD: Manual update
  }
  
  function updateTotal() {
    total = items.reduce((sum, i) => sum + i, 0);
  }
</script>
```

✅ **DO:**
```svelte
<script lang="ts">
  let items = $state([1, 2, 3]);
  let total = $derived(items.reduce((sum, i) => sum + i, 0)); // GOOD: Auto-updates
  
  function addItem(item: number) {
    items.push(item); // Svelte 5 tracks array mutations!
  }
</script>
```

**Svelte 5 Tracks:**
- Array mutations: `push`, `pop`, `shift`, `unshift`, `splice`
- Object property changes: `obj.prop = value`
- Map/Set operations: `map.set()`, `set.add()`

---

### 🎯 **Principle 7: Keep Logic Close to Usage**

Don't abstract too early. Keep related code together.

❌ **DON'T: Premature abstraction**
```typescript
// utils/formatters.ts - 50 different formatters
export const formatSlotTime = (slot: NeedSlot) => { ... }
export const formatSlotLocation = (slot: NeedSlot) => { ... }
export const formatSlotQuantity = (slot: NeedSlot) => { ... }
// ... 47 more
```

✅ **DO: Start local, extract when repeated 3+ times**
```svelte
<!-- SlotCard.svelte -->
<script lang="ts">
  let { slot }: Props = $props();
  
  // Keep formatting logic local until it's used elsewhere
  const formattedTime = $derived(
    slot.recurrence 
      ? `${slot.recurrence} at ${slot.start_date}`
      : slot.start_date
  );
</script>

<p>{formattedTime}</p>
```

**When to extract:**
- Used in 3+ places → Extract to utility
- Complex logic → Extract to function
- Reusable patterns → Extract to component

---

### 📝 **Principle 8: Template Syntax Over Imperative Code**

Use Svelte's built-in directives instead of imperative JavaScript.

**Conditional Rendering:**
```svelte
<!-- GOOD: Use {#if} -->
{#if isLoading}
  <LoadingSpinner />
{:else if error}
  <ErrorMessage {error} />
{:else}
  <DataDisplay {data} />
{/if}
```

**List Rendering:**
```svelte
<!-- GOOD: Use {#each} with key -->
{#each slots as slot (slot.id)}
  <SlotCard {slot} />
{/each}

<!-- GOOD: Empty state -->
{#each slots as slot (slot.id)}
  <SlotCard {slot} />
{:else}
  <EmptyState message="No slots yet" />
{/each}
```

**Event Handling:**
```svelte
<!-- GOOD: Inline handlers for simple logic -->
<button onclick={() => count++}>
  Increment
</button>

<!-- GOOD: Named handlers for complex logic -->
<button onclick={handleComplexOperation}>
  Complex Action
</button>
```

**Class Bindings:**
```svelte
<!-- GOOD: Conditional classes -->
<div 
  class="card"
  class:active={isActive}
  class:disabled={!isEnabled}
  class:highlight={hasHighlight}
>
  ...
</div>
```

**Style Bindings:**
```svelte
<!-- GOOD: Dynamic styles -->
<div style:color={textColor} style:opacity={isVisible ? 1 : 0.5}>
  ...
</div>
```

---

### 🧪 **Principle 9: Design for Testing**

Write components that are easy to test.

✅ **Testable Component:**
```svelte
<!-- SlotCard.svelte -->
<script lang="ts">
  import type { NeedSlot } from '../schemas';
  
  interface Props {
    slot: NeedSlot;
    onEdit?: (slot: NeedSlot) => void;
  }
  
  let { slot, onEdit }: Props = $props();
  
  // Pure function - easy to test
  function formatQuantity(qty: number, unit?: string): string {
    return `${qty}${unit ? ` ${unit}` : ''}`;
  }
</script>

<div class="slot-card" data-testid="slot-card-{slot.id}">
  <h3 data-testid="slot-name">{slot.name}</h3>
  <p data-testid="slot-quantity">{formatQuantity(slot.quantity, slot.unit)}</p>
  {#if onEdit}
    <button data-testid="edit-button" onclick={() => onEdit(slot)}>
      Edit
    </button>
  {/if}
</div>
```

**Testing Tips:**
- ✅ Use `data-testid` for test selectors
- ✅ Extract pure functions for unit testing
- ✅ Use props/callbacks for dependency injection
- ✅ Keep components small and focused
- ✅ Mock stores in tests

---

### 🔄 **Principle 10: Store Subscriptions Done Right**

Use stores directly in templates or with `$derived`.

❌ **DON'T:**
```svelte
<script lang="ts">
  import { myStore } from './stores.svelte';
  
  let value = $state();
  
  $effect(() => {
    const unsubscribe = myStore.subscribe(v => {
      value = v; // BAD: Manual subscription
    });
    return () => unsubscribe();
  });
</script>
```

✅ **DO:**
```svelte
<script lang="ts">
  import { get } from 'svelte/store';
  import { myStore } from './stores.svelte';
  
  // Option 1: Direct in template (auto-subscribes)
  // <p>{$myStore}</p>
  
  // Option 2: $derived for transformations
  let value = $derived(get(myStore));
  let doubled = $derived(get(myStore) * 2);
</script>

<p>{value}</p>
```

**For Svelte 5 runes-based stores:**
```svelte
<script lang="ts">
  import { networkCommitments } from './stores.svelte';
  
  // VersionedStore exposes reactive state directly
  let commits = $derived(networkCommitments.get());
  let count = $derived(commits.size);
</script>

<p>Total commitments: {count}</p>
```

---

### 🎨 **Principle 11: CSS Scoping & Styling**

Use Svelte's scoped styles effectively.

```svelte
<script lang="ts">
  let highlighted = $state(false);
</script>

<!-- Scoped styles (default) -->
<div class="card" class:highlighted>
  <h3>Title</h3>
</div>

<style>
  /* Scoped to this component */
  .card {
    padding: 1rem;
    border: 1px solid #ddd;
  }
  
  /* Conditional styling */
  .card.highlighted {
    border-color: gold;
  }
  
  /* :global() for global styles */
  .card :global(.external-class) {
    color: blue;
  }
</style>
```

**Styling Best Practices:**
- ✅ Use CSS custom properties for themes
- ✅ Use Tailwind classes for utility-first approach
- ✅ Use scoped styles for component-specific CSS
- ✅ Use `:global()` sparingly

---

### 📋 **Quick Reference: Common Patterns**

#### **1. Form Input Binding:**
```svelte
<script lang="ts">
  let name = $state('');
  let quantity = $state(0);
  let enabled = $state(false);
</script>

<input bind:value={name} />
<input type="number" bind:value={quantity} />
<input type="checkbox" bind:checked={enabled} />
```

#### **2. Derived State:**
```svelte
<script lang="ts">
  let firstName = $state('John');
  let lastName = $state('Doe');
  let fullName = $derived(`${firstName} ${lastName}`);
  let initials = $derived(`${firstName[0]}${lastName[0]}`);
</script>
```

#### **3. Async Data Loading:**
```svelte
<script lang="ts">
  let data = $state<User[]>([]);
  let loading = $state(true);
  let error = $state<Error | null>(null);
  
  async function loadData() {
    loading = true;
    error = null;
    try {
      data = await fetchUsers();
    } catch (e) {
      error = e as Error;
    } finally {
      loading = false;
    }
  }
  
  loadData(); // Call on mount
</script>

{#if loading}
  <LoadingSpinner />
{:else if error}
  <ErrorMessage {error} />
{:else}
  {#each data as user (user.id)}
    <UserCard {user} />
  {/each}
{/if}
```

#### **4. Event Modifiers:**
```svelte
<form onsubmit|preventDefault={handleSubmit}>
  <input />
</form>

<button onclick|once={handleClick}>Click Once</button>
<div onscroll|passive={handleScroll}>Scrollable</div>
```

#### **5. Snippet (Component Slot Pattern):**
```svelte
<!-- Card.svelte -->
<script lang="ts">
  interface Props {
    children: Snippet;
    header?: Snippet;
    footer?: Snippet;
  }
  
  let { children, header, footer }: Props = $props();
</script>

<div class="card">
  {#if header}
    <div class="header">{@render header()}</div>
  {/if}
  
  <div class="body">{@render children()}</div>
  
  {#if footer}
    <div class="footer">{@render footer()}</div>
  {/if}
</div>

<!-- Usage -->
<Card>
  {#snippet header()}
    <h2>Title</h2>
  {/snippet}
  
  <p>Body content</p>
  
  {#snippet footer()}
    <button>Action</button>
  {/snippet}
</Card>
```

---

### ⚠️ **Common Anti-Patterns to Avoid**

1. ❌ Using `$effect` for derived state → Use `$derived`
2. ❌ Direct DOM manipulation → Use template syntax
3. ❌ Prop drilling through 5+ levels → Use context or global state
4. ❌ Massive components (500+ lines) → Split into smaller pieces
5. ❌ Business logic in components → Extract to modules/stores
6. ❌ Ignoring TypeScript errors → Fix them properly
7. ❌ Not using keys in `{#each}` → Always provide keys
8. ❌ Over-abstracting (DRY obsession) → Start simple, refactor when needed

---

### 🚀 **Summary: Clean Svelte Components Checklist**

- ✅ Use `$derived` for computed state (not `$effect`)
- ✅ Use template syntax over imperative DOM manipulation
- ✅ Design clear prop interfaces with TypeScript
- ✅ Use `global.svelte.ts` for shared UI/app state
- ✅ Compose small components instead of giant ones
- ✅ Trust Svelte's reactivity system
- ✅ Keep logic close to usage (extract when repeated 3+ times)
- ✅ Use `{#if}`, `{#each}`, class bindings over manual updates
- ✅ Design for testability (data-testid, pure functions, props)
- ✅ Subscribe to stores directly in templates or with `$derived`



---

### 🎯 **Phase 5: Allocation View**

**User Flow:**
1. User sees what they're receiving (as recipient)
2. User sees what they're providing (as provider)
3. Breakdown by need type
4. Tier 1 (mutual) vs Tier 2 (non-mutual)
5. Real-time updates as network changes

**Components:**
```
components/allocation/
├── AllocationDashboard.svelte       # Main view
│   ├── AsRecipientView.svelte       # What I'm receiving
│   │   ├── AllocationsByType.svelte # Per need type
│   │   │   ├── TypeSection.svelte   # Single type
│   │   │   │   ├── TierBreakdown.svelte # Tier 1 vs 2
│   │   │   │   └── ProvidersList.svelte # Who's giving me X
│   │   │   └── AllocationCard.svelte # Single allocation
│   │   └── TotalReceivedSummary.svelte
│   │
│   ├── AsProviderView.svelte        # What I'm providing
│   │   ├── AllocationsBySlot.svelte # Per capacity slot
│   │   │   ├── SlotHeader.svelte    # Reuse from Phase 3
│   │   │   ├── RecipientsList.svelte # Who I'm allocating to
│   │   │   └── DenominatorDisplay.svelte # MR vs NonMR denominators
│   │   └── TotalGivenSummary.svelte
│   │
│   └── AllocationToggle.svelte      # Switch between recipient/provider
│
└── AllocationDetail.svelte          # Single allocation drill-down
    ├── AllocationMeta.svelte        # Quantity, type, tier
    ├── CompatibilityInfo.svelte     # Time/location match
    └── SlotPair.svelte              # Show both need & capacity slots
```

**Schemas Used:**
- `SlotAllocationRecord`
- All slot schemas (for context)

**Stores:**
- `myAllocationsAsProvider` (read - derived)
- Could create `myAllocationsAsRecipient` derived store

---

### 📊 **Phase 6: System Monitoring**

**User Flow:**
1. User sees convergence metrics
2. Tracks iterations to convergence
3. Views system-wide state
4. Monitors ITC causality

**Components:**
```
components/monitoring/
├── ConvergenceDashboard.svelte      # Main monitoring view
│   ├── ConvergenceMetrics.svelte    # Core metrics
│   │   ├── FrobeniusNorm.svelte     # System magnitude
│   │   ├── ContractionRate.svelte   # Convergence speed
│   │   ├── IterationsRemaining.svelte
│   │   └── PercentSatisfied.svelte
│   │
│   ├── PerTypeMetrics.svelte        # Per need type breakdown
│   │   └── TypeMetricCard.svelte    # Single type's convergence
│   │
│   ├── ConvergenceChart.svelte      # Time series graph
│   │   └── Chart.svelte             # D3/Chart.js wrapper
│   │
│   ├── SystemState.svelte           # Network-wide view
│   │   ├── ParticipantCount.svelte
│   │   ├── TotalNeeds.svelte
│   │   ├── TotalCapacity.svelte
│   │   └── NeedVariance.svelte      # Distribution inequality
│   │
│   └── ITCStatus.svelte             # Causality tracking
│       ├── MyStamp.svelte           # My ITC stamp
│       ├── StaleUpdates.svelte      # Rejected updates
│       └── MergedStamps.svelte      # Concurrent updates handled
│
└── DebugPanel.svelte                # Developer tools
    ├── StoreInspector.svelte        # Inspect store contents
    ├── LogViewer.svelte             # Console logs
    └── VersionedStoreDebug.svelte   # Field versions, ITC stamps
```

**Schemas Used:**
- `ConvergenceMetrics`
- `PerTypeConvergenceMetrics`
- `SystemState`
- `ITCStamp`

**Stores:**
- All stores (for inspection)
- `myAllocationsAsProvider.convergence` (derived field)

---

## 🔧 **Shared Utilities**

These components are used across multiple phases:

```
components/shared/
├── forms/
│   ├── TextInput.svelte             # Basic text input
│   ├── NumberInput.svelte           # Number with validation
│   ├── SelectInput.svelte           # Dropdown
│   ├── DateInput.svelte             # Date picker
│   ├── TimeInput.svelte             # Time picker (HH:MM)
│   ├── CheckboxInput.svelte         # Boolean toggle
│   ├── RadioGroup.svelte            # Multiple choice
│   └── FormField.svelte             # Wrapper with label/error
│
├── display/
│   ├── Card.svelte                  # Generic card container
│   ├── Badge.svelte                 # Status/label badge
│   ├── Avatar.svelte                # User avatar
│   ├── Icon.svelte                  # Icon wrapper
│   ├── Tooltip.svelte               # Hover info
│   ├── Modal.svelte                 # Modal dialog
│   └── EmptyState.svelte            # No data placeholder
│
├── navigation/
│   ├── Tabs.svelte                  # Tab navigation
│   ├── Breadcrumbs.svelte           # Breadcrumb trail
│   └── Sidebar.svelte               # Side navigation
│
└── feedback/
    ├── LoadingSpinner.svelte        # Loading state
    ├── ErrorMessage.svelte          # Error display
    ├── SuccessMessage.svelte        # Success feedback
    └── ProgressBar.svelte           # Progress indicator
```

---

## 📁 **Final Component Tree**

```
src/lib/commons/v5/components/
├── slots/                   # Phase 3 (SHARED between needs/capacity)
├── network/                 # Phase 4
├── allocation/              # Phase 5
├── monitoring/              # Phase 6
└── shared/                  # Utilities
```

---

## 🔑 **Key Abstractions**

### 1. **Slot Editor (Needs & Capacity)**
- **99% identical schemas** → Use same components
- Pass `mode="need" | "capacity"` prop
- Schema validation differs slightly, but form is identical
- Most complex component (location, time, recurrence)

### 2. **Display Components**
- `RecognitionWeightsView` - Used in both tree editor and network view
- `SlotsList` - Used for my slots, network slots, allocations
- `TreeVisualization` - Used for my tree and others' trees

### 3. **Form Components**
- All shared utilities in `components/shared/forms/`
- Consistent validation, styling, error handling

### 4. **Data Visualization**
- Chart components reused for convergence, needs over time, etc.
- D3-based tree visualization for recognition trees

---

## 🎯 **Development Priority**

**Phase 1 (MVP):**
3. ✅ Slot Editor (needs only, basic form)
4. ✅ Network View (basic list)
5. ✅ Allocation View (recipient only)

**Phase 2 (Full Features):**
6. Slot Editor (advanced: time windows, location, filters)
7. Allocation View (provider view, tier breakdown)
8. Monitoring Dashboard

**Phase 3 (Polish):**
9. Convergence visualization
10. Debug panel
11. Advanced tree visualization

---

## 📊 **Component Reusability Matrix**

| Component | Used In |
|-----------|---------|
| `SlotForm` | Needs Editor, Capacity Editor |
| `SlotCard` | My Needs, My Capacity, Network View, Allocations |
| `SlotsList` | All of the above |
| `RecognitionWeightsView` | Tree Editor, Network View, Participant Detail |
| `TreeVisualization` | My Tree, Others' Trees |
| `MutualRecognitionBar` | Network List, Allocation Details |
| `ConvergenceStatus` | Network View, Monitoring Dashboard |
| `ParticipantCard` | Network View, Allocation Recipients |
| Form utilities | Everywhere |

**Reusability Score: ~40%** of components are shared across multiple contexts!

---

## 🎨 **Svelte Component Best Practices**

### ✨ **Principle 1: You Almost Never Need `$effect`**

Svelte's reactivity is declarative. Most of the time, `$derived` is what you want.

❌ **DON'T:**
```svelte
<script lang="ts">
  let count = $state(0);
  let doubled = $state(0);
  
  $effect(() => {
    doubled = count * 2; // BAD: Using effect for derived state
  });
</script>
```

✅ **DO:**
```svelte
<script lang="ts">
  let count = $state(0);
  let doubled = $derived(count * 2); // GOOD: Declarative derived state
</script>
```

**When you ACTUALLY need `$effect`:**
- Side effects (logging, analytics, external APIs)
- DOM measurements (after render)
- Cleanup (unsubscribe, clear intervals)

```svelte
<script lang="ts">
  import { myStore } from './stores.svelte';
  
  let data = $state([]);
  
  // Valid use case: Subscribing to a store
  $effect(() => {
    const unsubscribe = myStore.subscribe(value => {
      data = value;
    });
    
    return () => unsubscribe(); // Cleanup
  });
  
  // Better: Use store directly in template
  // let data = $derived(get(myStore));
</script>
```

---

### 🚫 **Principle 2: Avoid Direct DOM Manipulation**

Use Svelte's template syntax instead of imperatively manipulating the DOM.

❌ **DON'T:**
```svelte
<script lang="ts">
  let isOpen = $state(false);
  
  $effect(() => {
    const modal = document.getElementById('modal');
    if (isOpen) {
      modal.style.display = 'block'; // BAD: Direct DOM manipulation
    } else {
      modal.style.display = 'none';
    }
  });
</script>

<div id="modal">...</div>
```

✅ **DO:**
```svelte
<script lang="ts">
  let isOpen = $state(false);
</script>

{#if isOpen}
  <div class="modal">...</div>
{/if}

<!-- Or with CSS -->
<div class="modal" class:open={isOpen}>...</div>

<style>
  .modal { display: none; }
  .modal.open { display: block; }
</style>
```

**When DOM access IS needed:**
- Measuring elements (use `bind:this` with `$effect`)
- Third-party libraries (D3, Chart.js)
- Focus management

```svelte
<script lang="ts">
  let inputElement = $state<HTMLInputElement>();
  
  function focusInput() {
    inputElement?.focus(); // Valid use case
  }
</script>

<input bind:this={inputElement} />
```

---

### 📦 **Principle 3: Props for Component Communication**

Use props for parent → child communication. Use callbacks for child → parent.

✅ **GOOD Component API:**
```svelte
<!-- SlotCard.svelte -->
<script lang="ts">
  import type { NeedSlot } from '../schemas';
  
  interface Props {
    slot: NeedSlot;              // Data in
    readonly?: boolean;          // Configuration
    onEdit?: (slot: NeedSlot) => void;  // Action out
    onDelete?: (id: string) => void;    // Action out
  }
  
  let { slot, readonly = false, onEdit, onDelete }: Props = $props();
</script>

<div class="slot-card">
  <h3>{slot.name}</h3>
  <p>{slot.quantity} {slot.unit}</p>
  
  {#if !readonly}
    <button onclick={() => onEdit?.(slot)}>Edit</button>
    <button onclick={() => onDelete?.(slot.id)}>Delete</button>
  {/if}
</div>
```

**Usage:**
```svelte
<SlotCard 
  slot={mySlot}
  onEdit={handleEdit}
  onDelete={handleDelete}
/>
```

**Prop Patterns:**
- ✅ Use TypeScript interfaces for props
- ✅ Provide sensible defaults
- ✅ Optional callbacks with `?:`
- ✅ Use `readonly` for display-only modes
- ✅ Destructure props with `$props()`

---

### 🌐 **Principle 4: Global State in `global.svelte.ts`**

For state shared across many components, create a global state file.

**File: `src/lib/commons/v5/global.svelte.ts`**
```typescript
/**
 * Global Application State
 * 
 * Use for:
 * - UI state (modals, sidebars, theme)
 * - Navigation state
 * - User preferences
 * - Toast notifications
 * 
 * DON'T use for:
 * - Domain data (use stores.svelte.ts instead)
 * - Form state (keep local to component)
 */

interface Toast {
  id: string;
  message: string;
  type: 'success' | 'error' | 'info';
}

// UI State
export const sidebarOpen = $state(true);
export const currentModal = $state<string | null>(null);
export const theme = $state<'light' | 'dark'>('light');

// Toast Notifications
const toasts = $state<Toast[]>([]);

export const toastState = {
  get items() { return toasts; },
  
  add(message: string, type: Toast['type'] = 'info') {
    const id = crypto.randomUUID();
    toasts.push({ id, message, type });
    setTimeout(() => this.remove(id), 5000);
  },
  
  remove(id: string) {
    const index = toasts.findIndex(t => t.id === id);
    if (index !== -1) toasts.splice(index, 1);
  },
  
  clear() {
    toasts.length = 0;
  }
};

// Navigation State
export const currentView = $state<'tree' | 'needs' | 'capacity' | 'network' | 'allocation'>('tree');

// User Preferences (persisted to localStorage)
export const userPrefs = $state({
  showHints: true,
  compactMode: false,
  defaultNeedType: 'food'
});

// Load preferences from localStorage on init
if (typeof localStorage !== 'undefined') {
  const saved = localStorage.getItem('userPrefs');
  if (saved) {
    Object.assign(userPrefs, JSON.parse(saved));
  }
}
```

**Usage in Components:**
```svelte
<script lang="ts">
  import { sidebarOpen, toastState, currentView } from '$lib/commons/v5/global.svelte';
  
  function handleSave() {
    // ... save logic
    toastState.add('Saved successfully!', 'success');
  }
</script>

{#if sidebarOpen}
  <aside>...</aside>
{/if}

<button onclick={() => sidebarOpen = !sidebarOpen}>
  Toggle Sidebar
</button>
```

**When to use `global.svelte.ts` vs props:**
- Use **global** for: UI chrome, app-wide state, cross-cutting concerns
- Use **props** for: Component-specific data, parent-child relationships

---

### 🧩 **Principle 5: Component Composition Over Configuration**

Build small, focused components and compose them together.

❌ **DON'T: Mega-component with all options**
```svelte
<!-- BAD: One component does everything -->
<SlotDisplay 
  slot={slot}
  mode="card"
  showTime={true}
  showLocation={true}
  showActions={true}
  editable={true}
  onEdit={...}
  onDelete={...}
  onDuplicate={...}
  compact={false}
  highlight={false}
/>
```

✅ **DO: Compose smaller components**
```svelte
<!-- GOOD: Compose what you need -->
<SlotCard slot={slot}>
  <SlotHeader name={slot.name} emoji={slot.emoji} />
  <SlotTime time={slot.start_date} recurrence={slot.recurrence} />
  <SlotLocation location={slot.city} online={slot.online_link} />
  <SlotActions onEdit={handleEdit} onDelete={handleDelete} />
</SlotCard>

<!-- Or simpler version -->
<SlotCard slot={slot}>
  <SlotHeader name={slot.name} emoji={slot.emoji} />
  <p>{slot.quantity} {slot.unit}</p>
</SlotCard>
```

**Benefits:**
- Easier to understand
- More flexible
- Better tree-shaking
- Easier testing

---

### ⚡ **Principle 6: Use Svelte's Reactivity, Not Workarounds**

Trust Svelte's reactivity system. Don't fight it.

❌ **DON'T:**
```svelte
<script lang="ts">
  let items = $state([1, 2, 3]);
  let total = $state(0);
  
  function addItem(item: number) {
    items.push(item);
    items = items; // BAD: Forcing reactivity
    updateTotal(); // BAD: Manual update
  }
  
  function updateTotal() {
    total = items.reduce((sum, i) => sum + i, 0);
  }
</script>
```

✅ **DO:**
```svelte
<script lang="ts">
  let items = $state([1, 2, 3]);
  let total = $derived(items.reduce((sum, i) => sum + i, 0)); // GOOD: Auto-updates
  
  function addItem(item: number) {
    items.push(item); // Svelte 5 tracks array mutations!
  }
</script>
```

**Svelte 5 Tracks:**
- Array mutations: `push`, `pop`, `shift`, `unshift`, `splice`
- Object property changes: `obj.prop = value`
- Map/Set operations: `map.set()`, `set.add()`

---

### 🎯 **Principle 7: Keep Logic Close to Usage**

Don't abstract too early. Keep related code together.

❌ **DON'T: Premature abstraction**
```typescript
// utils/formatters.ts - 50 different formatters
export const formatSlotTime = (slot: NeedSlot) => { ... }
export const formatSlotLocation = (slot: NeedSlot) => { ... }
export const formatSlotQuantity = (slot: NeedSlot) => { ... }
// ... 47 more
```

✅ **DO: Start local, extract when repeated 3+ times**
```svelte
<!-- SlotCard.svelte -->
<script lang="ts">
  let { slot }: Props = $props();
  
  // Keep formatting logic local until it's used elsewhere
  const formattedTime = $derived(
    slot.recurrence 
      ? `${slot.recurrence} at ${slot.start_date}`
      : slot.start_date
  );
</script>

<p>{formattedTime}</p>
```

**When to extract:**
- Used in 3+ places → Extract to utility
- Complex logic → Extract to function
- Reusable patterns → Extract to component

---

### 📝 **Principle 8: Template Syntax Over Imperative Code**

Use Svelte's built-in directives instead of imperative JavaScript.

**Conditional Rendering:**
```svelte
<!-- GOOD: Use {#if} -->
{#if isLoading}
  <LoadingSpinner />
{:else if error}
  <ErrorMessage {error} />
{:else}
  <DataDisplay {data} />
{/if}
```

**List Rendering:**
```svelte
<!-- GOOD: Use {#each} with key -->
{#each slots as slot (slot.id)}
  <SlotCard {slot} />
{/each}

<!-- GOOD: Empty state -->
{#each slots as slot (slot.id)}
  <SlotCard {slot} />
{:else}
  <EmptyState message="No slots yet" />
{/each}
```

**Event Handling:**
```svelte
<!-- GOOD: Inline handlers for simple logic -->
<button onclick={() => count++}>
  Increment
</button>

<!-- GOOD: Named handlers for complex logic -->
<button onclick={handleComplexOperation}>
  Complex Action
</button>
```

**Class Bindings:**
```svelte
<!-- GOOD: Conditional classes -->
<div 
  class="card"
  class:active={isActive}
  class:disabled={!isEnabled}
  class:highlight={hasHighlight}
>
  ...
</div>
```

**Style Bindings:**
```svelte
<!-- GOOD: Dynamic styles -->
<div style:color={textColor} style:opacity={isVisible ? 1 : 0.5}>
  ...
</div>
```

---

### 🧪 **Principle 9: Design for Testing**

Write components that are easy to test.

✅ **Testable Component:**
```svelte
<!-- SlotCard.svelte -->
<script lang="ts">
  import type { NeedSlot } from '../schemas';
  
  interface Props {
    slot: NeedSlot;
    onEdit?: (slot: NeedSlot) => void;
  }
  
  let { slot, onEdit }: Props = $props();
  
  // Pure function - easy to test
  function formatQuantity(qty: number, unit?: string): string {
    return `${qty}${unit ? ` ${unit}` : ''}`;
  }
</script>

<div class="slot-card" data-testid="slot-card-{slot.id}">
  <h3 data-testid="slot-name">{slot.name}</h3>
  <p data-testid="slot-quantity">{formatQuantity(slot.quantity, slot.unit)}</p>
  {#if onEdit}
    <button data-testid="edit-button" onclick={() => onEdit(slot)}>
      Edit
    </button>
  {/if}
</div>
```

**Testing Tips:**
- ✅ Use `data-testid` for test selectors
- ✅ Extract pure functions for unit testing
- ✅ Use props/callbacks for dependency injection
- ✅ Keep components small and focused
- ✅ Mock stores in tests

---

### 🔄 **Principle 10: Store Subscriptions Done Right**

Use stores directly in templates or with `$derived`.

❌ **DON'T:**
```svelte
<script lang="ts">
  import { myStore } from './stores.svelte';
  
  let value = $state();
  
  $effect(() => {
    const unsubscribe = myStore.subscribe(v => {
      value = v; // BAD: Manual subscription
    });
    return () => unsubscribe();
  });
</script>
```

✅ **DO:**
```svelte
<script lang="ts">
  import { get } from 'svelte/store';
  import { myStore } from './stores.svelte';
  
  // Option 1: Direct in template (auto-subscribes)
  // <p>{$myStore}</p>
  
  // Option 2: $derived for transformations
  let value = $derived(get(myStore));
  let doubled = $derived(get(myStore) * 2);
</script>

<p>{value}</p>
```

**For Svelte 5 runes-based stores:**
```svelte
<script lang="ts">
  import { networkCommitments } from './stores.svelte';
  
  // VersionedStore exposes reactive state directly
  let commits = $derived(networkCommitments.get());
  let count = $derived(commits.size);
</script>

<p>Total commitments: {count}</p>
```

---

### 🎨 **Principle 11: CSS Scoping & Styling**

Use Svelte's scoped styles effectively.

```svelte
<script lang="ts">
  let highlighted = $state(false);
</script>

<!-- Scoped styles (default) -->
<div class="card" class:highlighted>
  <h3>Title</h3>
</div>

<style>
  /* Scoped to this component */
  .card {
    padding: 1rem;
    border: 1px solid #ddd;
  }
  
  /* Conditional styling */
  .card.highlighted {
    border-color: gold;
  }
  
  /* :global() for global styles */
  .card :global(.external-class) {
    color: blue;
  }
</style>
```

**Styling Best Practices:**
- ✅ Use CSS custom properties for themes
- ✅ Use Tailwind classes for utility-first approach
- ✅ Use scoped styles for component-specific CSS
- ✅ Use `:global()` sparingly

---

### 📋 **Quick Reference: Common Patterns**

#### **1. Form Input Binding:**
```svelte
<script lang="ts">
  let name = $state('');
  let quantity = $state(0);
  let enabled = $state(false);
</script>

<input bind:value={name} />
<input type="number" bind:value={quantity} />
<input type="checkbox" bind:checked={enabled} />
```

#### **2. Derived State:**
```svelte
<script lang="ts">
  let firstName = $state('John');
  let lastName = $state('Doe');
  let fullName = $derived(`${firstName} ${lastName}`);
  let initials = $derived(`${firstName[0]}${lastName[0]}`);
</script>
```

#### **3. Async Data Loading:**
```svelte
<script lang="ts">
  let data = $state<User[]>([]);
  let loading = $state(true);
  let error = $state<Error | null>(null);
  
  async function loadData() {
    loading = true;
    error = null;
    try {
      data = await fetchUsers();
    } catch (e) {
      error = e as Error;
    } finally {
      loading = false;
    }
  }
  
  loadData(); // Call on mount
</script>

{#if loading}
  <LoadingSpinner />
{:else if error}
  <ErrorMessage {error} />
{:else}
  {#each data as user (user.id)}
    <UserCard {user} />
  {/each}
{/if}
```

#### **4. Event Modifiers:**
```svelte
<form onsubmit|preventDefault={handleSubmit}>
  <input />
</form>

<button onclick|once={handleClick}>Click Once</button>
<div onscroll|passive={handleScroll}>Scrollable</div>
```

#### **5. Snippet (Component Slot Pattern):**
```svelte
<!-- Card.svelte -->
<script lang="ts">
  interface Props {
    children: Snippet;
    header?: Snippet;
    footer?: Snippet;
  }
  
  let { children, header, footer }: Props = $props();
</script>

<div class="card">
  {#if header}
    <div class="header">{@render header()}</div>
  {/if}
  
  <div class="body">{@render children()}</div>
  
  {#if footer}
    <div class="footer">{@render footer()}</div>
  {/if}
</div>

<!-- Usage -->
<Card>
  {#snippet header()}
    <h2>Title</h2>
  {/snippet}
  
  <p>Body content</p>
  
  {#snippet footer()}
    <button>Action</button>
  {/snippet}
</Card>
```

---

### ⚠️ **Common Anti-Patterns to Avoid**

1. ❌ Using `$effect` for derived state → Use `$derived`
2. ❌ Direct DOM manipulation → Use template syntax
3. ❌ Prop drilling through 5+ levels → Use context or global state
4. ❌ Massive components (500+ lines) → Split into smaller pieces
5. ❌ Business logic in components → Extract to modules/stores
6. ❌ Ignoring TypeScript errors → Fix them properly
7. ❌ Not using keys in `{#each}` → Always provide keys
8. ❌ Over-abstracting (DRY obsession) → Start simple, refactor when needed

---

### 🚀 **Summary: Clean Svelte Components Checklist**

- ✅ Use `$derived` for computed state (not `$effect`)
- ✅ Use template syntax over imperative DOM manipulation
- ✅ Design clear prop interfaces with TypeScript
- ✅ Use `global.svelte.ts` for shared UI/app state
- ✅ Compose small components instead of giant ones
- ✅ Trust Svelte's reactivity system
- ✅ Keep logic close to usage (extract when repeated 3+ times)
- ✅ Use `{#if}`, `{#each}`, class bindings over manual updates
- ✅ Design for testability (data-testid, pure functions, props)
- ✅ Subscribe to stores directly in templates or with `$derived`

