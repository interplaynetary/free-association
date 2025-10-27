# data-testid Implementation Plan

## Overview
This document outlines the systematic approach to adding `data-testid` attributes to UI components to enable reliable E2E testing with proper assertions instead of exploratory console.logs.

## Phase 1: Authentication Components (Priority: High) 🔴

### Target Components
- `src/lib/components/Header.svelte` - Auth buttons
- Authentication modals/forms
- User profile indicators

### Required Test IDs
```typescript
// Authentication
data-testid="login-button"
data-testid="signup-button"
data-testid="logout-button"
data-testid="auth-modal"
data-testid="auth-alias"          // Alias input field
data-testid="auth-password"       // Password input field
data-testid="auth-submit"
data-testid="auth-error"          // Error message container

// User state
data-testid="authenticated"       // Shown when logged in
data-testid="user-profile"
data-testid="user-alias"
```

### Test Updates After Phase 1
Replace in `tests/auth.e2e.ts`:
```typescript
// Before:
console.log('Authentication state after signup:', isAuthenticated);

// After:
expect(isAuthenticated).toBe(true);
expect(page.locator('[data-testid="authenticated"]')).toBeVisible();
expect(page.locator('[data-testid="user-alias"]')).toContainText(testAlias);
```

## Phase 2: Navigation Components (Priority: High) 🔴

### Target Components
- `src/lib/components/Header.svelte`
- `src/lib/components/ToolBar.svelte`

### Required Test IDs
```typescript
// Header
data-testid="header"
data-testid="logo"
data-testid="nav-menu"

// Toolbar (bottom navigation)
data-testid="toolbar"
data-testid="tree-view"
data-testid="map-view"
data-testid="inventory-view"
data-testid="shares-view"

// Mobile
data-testid="mobile-menu"
data-testid="mobile-drawer"
```

### Test Updates After Phase 2
Replace in `tests/navigation.e2e.ts`:
```typescript
// Before:
console.log('Current view:', currentView);

// After:
expect(page.locator('[data-testid="tree-view"]')).toHaveAttribute('aria-current', 'page');
```

## Phase 3: Recognition Tree Components (Priority: High) 🔴

### Target Components
- `src/lib/components/Parent.svelte` - Tree visualization
- `src/lib/components/Child.svelte` - Tree nodes
- `src/lib/components/Bar.svelte` - Recognition bars
- `src/lib/components/DropDown.svelte` - Contributor selection

### Required Test IDs
```typescript
// Tree structure
data-testid="tree-container"
data-testid="tree-node"           // Individual nodes
data-testid="root-node"
data-testid="node-name"
data-testid="node-points"
data-testid="node-contributors"
data-testid="node-fulfillment"

// Node editing
data-testid="add-node"
data-testid="edit-node"
data-testid="delete-node"
data-testid="save-node"
data-testid="cancel-edit"
data-testid="node-name-input"
data-testid="node-points-input"
data-testid="manual-fulfillment"

// Contributors
data-testid="add-contributor"
data-testid="add-anti-contributor"
data-testid="contributor-dropdown"
data-testid="contributor-list"
data-testid="contributor-item"
data-testid="remove-contributor"

// Recognition bars
data-testid="recognition-bar"
data-testid="bar-segment"
```

### Test Updates After Phase 3
Replace in `tests/recognition-tree.e2e.ts`:
```typescript
// Before:
console.log('Recognition shares:', shares);

// After:
const aliceShare = page.locator('[data-testid="bar-segment"]').filter({ hasText: 'Alice' });
await expect(aliceShare).toContainText('30%');
```

## Phase 4: Capacity Management (Priority: Medium) 🟡

### Target Components
- `src/lib/components/Capacities.svelte`
- `src/lib/components/Capacity.svelte`
- `src/lib/components/Slot.svelte`
- Country/Timezone selectors

### Required Test IDs
```typescript
// Capacity list
data-testid="capacities-list"
data-testid="capacity-card"
data-testid="new-capacity"

// Capacity form
data-testid="capacity-name"
data-testid="capacity-emoji"
data-testid="capacity-unit"
data-testid="capacity-description"
data-testid="save-capacity"
data-testid="delete-capacity"

// Slots
data-testid="add-slot"
data-testid="slot-list"
data-testid="slot-item"
data-testid="slot-quantity"
data-testid="slot-need-type"
data-testid="slot-start-date"
data-testid="slot-recurrence"
data-testid="slot-location-type"
data-testid="slot-city"
data-testid="slot-country"
data-testid="slot-timezone"

// Filters
data-testid="filter-type"
data-testid="min-mutual-recognition"
data-testid="only-mutual"
data-testid="subtree-filter"
```

## Phase 5: Network Shares (Priority: Medium) 🟡

### Target Components
- `src/lib/components/Shares.svelte`
- `src/lib/components/Share.svelte`
- `src/lib/components/MapSidePanel.svelte`
- `src/lib/components/SlotCompositionItem.svelte`

### Required Test IDs
```typescript
// Shares view
data-testid="shares-list"
data-testid="share-card"
data-testid="network-capacities"
data-testid="network-capacity-card"

// Desire expression
data-testid="desire-input"
data-testid="express-desire"
data-testid="composition-quantity"

// Allocation info
data-testid="allocated-quantity"
data-testid="available-quantity"
data-testid="allocation-share"
data-testid="mutual-tier"
data-testid="non-mutual-tier"

// Map
data-testid="map-container"
data-testid="map-side-panel"
```

## Phase 6: Accessibility Indicators (Priority: Low) 🟢

### Target Components
- All interactive elements without visible labels
- Icon-only buttons
- Decorative images

### Required Attributes
```typescript
// For icon buttons without text
aria-label="Close"
aria-label="Save"
aria-label="Delete"

// For images
alt="User avatar"
alt="" // For decorative images

// For forms
<label for="input-id">Label Text</label>
// Or
aria-label="Input description"
// Or
aria-labelledby="label-element-id"
```

## Implementation Guidelines

### 1. Component Update Pattern

**Before:**
```svelte
<button on:click={handleLogin}>
  Login
</button>
```

**After:**
```svelte
<button data-testid="login-button" on:click={handleLogin}>
  Login
</button>
```

### 2. Dynamic Test IDs

For lists or repeated elements:
```svelte
{#each nodes as node}
  <div data-testid="tree-node" data-node-id={node.id}>
    <span data-testid="node-name">{node.name}</span>
  </div>
{/each}
```

### 3. Conditional Test IDs

```svelte
<div data-testid={isAuthenticated ? 'authenticated' : 'unauthenticated'}>
  <!-- content -->
</div>
```

### 4. Test ID Naming Convention

- Use kebab-case: `data-testid="my-element"`
- Be descriptive: `data-testid="add-contributor-button"` not `data-testid="btn"`
- Group related elements: `data-testid="auth-*"`, `data-testid="tree-*"`
- Action-oriented for buttons: `data-testid="save-button"`, `data-testid="delete-node"`

## Rollout Schedule

### Week 1: Phase 1 (Auth)
- Days 1-2: Add test IDs to components
- Days 3-4: Update auth.e2e.ts with assertions
- Day 5: Test and verify

### Week 2: Phase 2 (Navigation) + Phase 3 Start
- Days 1-2: Navigation components
- Days 3-5: Start recognition tree components

### Week 3: Phase 3 Completion
- Days 1-5: Complete recognition tree components and tests

### Week 4: Phase 4 (Capacity)
- Days 1-5: Capacity management components and tests

### Week 5: Phase 5 (Shares)
- Days 1-5: Network shares components and tests

### Week 6: Phase 6 (Accessibility) + Cleanup
- Days 1-3: Accessibility improvements
- Days 4-5: Final review and cleanup

## Success Metrics

### Before Implementation
- ❌ Tests with console.log: ~150+
- ❌ Tests with proper assertions: ~50
- ❌ Test reliability: Medium (flaky due to selectors)

### After Implementation
- ✅ Tests with console.log: 0
- ✅ Tests with proper assertions: 200+
- ✅ Test reliability: High (stable selectors)
- ✅ Test maintenance: Easy (declarative test IDs)

## Testing the Changes

### 1. Component-Level Testing
After adding test IDs to a component, verify:
```bash
# Run specific test file
bunx playwright test auth.e2e.ts

# Run in UI mode to see selectors
bunx playwright test auth.e2e.ts --ui

# Run in debug mode
bunx playwright test auth.e2e.ts --debug
```

### 2. Full Test Suite
```bash
# Run all tests
bun run test:e2e

# Run tests on specific browser
bun run test:e2e --project=chromium
```

### 3. Visual Verification
Use Playwright Inspector to verify selectors:
```bash
bunx playwright codegen http://localhost:5173
```

## Documentation Updates

After each phase, update:
1. `tests/README.md` - Document new test IDs added
2. `CONTRIBUTING.md` - Add guidelines for new components
3. Component documentation - Document test IDs in JSDoc

## Rollback Plan

If issues arise:
1. Test IDs are non-breaking (only add attributes)
2. Old tests continue to work with fallback selectors
3. Can roll out incrementally without breaking existing tests
4. Can revert component changes without affecting functionality

## Review Checklist

Before marking a phase complete:
- [ ] All test IDs added to components
- [ ] All tests updated with proper assertions
- [ ] No console.log statements remaining
- [ ] Tests pass on all browsers
- [ ] Tests pass in parallel execution
- [ ] Documentation updated
- [ ] Code reviewed and approved
- [ ] CI/CD passes

## Notes

- **Non-breaking**: Adding test IDs doesn't affect production functionality
- **Incremental**: Can be done phase by phase
- **Backward compatible**: Existing tests continue to work during transition
- **Future-proof**: Makes test maintenance significantly easier

---

**Status**: Ready for Implementation  
**Owner**: Development Team  
**Timeline**: 6 weeks  
**Priority**: High for Phases 1-3, Medium for Phases 4-5, Low for Phase 6

