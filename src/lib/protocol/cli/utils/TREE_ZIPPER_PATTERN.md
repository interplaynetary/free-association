# Tree Zipper Pattern for CLI Navigation

## The Problem

Our previous approach had multiple issues:
1. **Closure bugs**: `useInput` callbacks captured stale state values
2. **Complex state management**: Multiple useState hooks for navigation state
3. **Tight coupling**: Navigation logic mixed with rendering logic
4. **Hard to extend**: Adding new steps required modifying multiple functions

## The Solution: Tree Zipper

A **tree zipper** is a functional programming pattern that provides:
- Pure navigation through hierarchical structures
- Explicit focus management (where we are in the tree)
- Path tracking (how we got here)
- Clean separation of concerns

### Key Concepts

```typescript
// Tree structure (declarative)
const tree = {
  id: 'root',
  children: [
    { id: 'step1', type: 'input', ... },
    { id: 'step2', type: 'select', ... },
    { 
      id: 'step3', 
      type: 'branch',
      children: (state) => dynamicChildren(state)  // Dynamic!
    }
  ]
};

// Zipper state (current position)
{
  path: ['root', 'step1'],  // Where we are
  data: { ... },             // Collected values
  cursor: 0                  // UI cursor position
}
```

### Benefits

#### 1. No Closure Issues
Navigation is handled with explicit state, not closures:
```typescript
// OLD (closure bug):
const goNext = () => {
  if (currentIndex < items.length - 1) {  // Stale!
    setCurrentIndex(currentIndex + 1);
  }
};

// NEW (explicit state):
goForward() {
  // Read current state directly from zipperState
  const currentNode = getCurrentNode(zipperState);
  // Navigate based on tree structure
}
```

#### 2. Dynamic Children
Children can be computed based on current state:
```typescript
{
  id: 'times',
  children: (state) => {
    // Generate time inputs for each selected day
    return state.selectedDays.flatMap(day => [
      { id: `${day}-start`, type: 'input', ... },
      { id: `${day}-end`, type: 'input', ... }
    ]);
  }
}
```

#### 3. Declarative Structure
The entire wizard is a data structure:
```typescript
const capacityWizard = {
  id: 'capacity',
  children: [
    { id: 'type', type: 'select', options: typeOptions },
    { id: 'quantity', type: 'input', inputType: 'number' },
    { id: 'unit', type: 'select', options: unitOptions },
    { 
      id: 'schedule', 
      type: 'custom',
      component: TimeScheduleBuilder
    }
  ]
};
```

#### 4. Lifecycle Hooks
Each node can have enter/exit hooks:
```typescript
{
  id: 'days',
  type: 'multi-select',
  onEnter: (state) => {
    // Initialize if needed
    return state;
  },
  onExit: (state, value) => {
    // Store selected days
    return { ...state, selectedDays: value };
  }
}
```

## Usage Example

### Basic Wizard

```typescript
import { TreeZipper, TreeNode } from './utils/TreeZipper';

const tree: TreeNode = {
  id: 'root',
  type: 'branch',
  children: [
    {
      id: 'name',
      type: 'input',
      label: 'What is your name?',
      onExit: (state, value) => ({ ...state, name: value })
    },
    {
      id: 'age',
      type: 'input',
      inputType: 'number',
      label: 'How old are you?',
      validate: (value) => {
        const age = parseInt(value);
        if (isNaN(age) || age < 0) return 'Invalid age';
        return true;
      },
      onExit: (state, value) => ({ ...state, age: parseInt(value) })
    },
    {
      id: 'hobbies',
      type: 'multi-select',
      label: 'Select your hobbies:',
      options: [
        { value: 'reading', label: 'Reading' },
        { value: 'sports', label: 'Sports' },
        { value: 'coding', label: 'Coding' }
      ],
      onExit: (state, value) => ({ ...state, hobbies: value })
    }
  ]
};

<TreeZipper
  tree={tree}
  initialState={{}}
  onComplete={(state) => console.log('Done:', state)}
  onCancel={() => console.log('Cancelled')}
/>
```

### Dynamic Children Example

```typescript
const tree: TreeNode = {
  id: 'root',
  children: [
    {
      id: 'days',
      type: 'multi-select',
      label: 'Select days',
      options: dayOptions,
      onExit: (state, value) => ({ ...state, days: value })
    },
    {
      id: 'times',
      type: 'branch',
      // Children depend on selected days!
      children: (state) => {
        return state.days.flatMap(day => [
          {
            id: `${day}-start`,
            type: 'input',
            label: `${day} start time`,
            onExit: (s, val) => ({
              ...s,
              times: { ...s.times, [day]: { ...s.times[day], start: val }}
            })
          },
          {
            id: `${day}-end`,
            type: 'input',
            label: `${day} end time`,
            onExit: (s, val) => ({
              ...s,
              times: { ...s.times, [day]: { ...s.times[day], end: val }}
            })
          }
        ]);
      }
    }
  ]
};
```

## Navigation

Users navigate with:
- `←` **Left arrow**: Go back to previous node
- `→` **Right arrow**: Go forward to next node/child
- `↑↓` **Up/Down arrows**: Navigate options in select/multi-select
- `Space`: Toggle selection in multi-select
- `Enter`: Submit value and auto-advance
- `Esc`: Cancel entire wizard

## Architecture

```
TreeZipper (Container)
├─ ZipperState (path, data, cursor)
├─ Navigation Logic (goForward, goBack, goToSibling)
└─ NodeRenderer (renders current node)
   ├─ InputNode
   ├─ SelectNode
   ├─ MultiSelectNode
   └─ CustomNode
```

## Extending

### Add a New Node Type

```typescript
// 1. Add to TreeNodeType
export type TreeNodeType = 
  | 'input'
  | 'select'
  | 'multi-select'
  | 'date-picker'  // New!
  | 'custom'
  | 'branch';

// 2. Add to TreeNode interface
export interface TreeNode {
  // ... existing fields
  dateFormat?: string;  // New field for date-picker
}

// 3. Add renderer in NodeRenderer
if (node.type === 'date-picker') {
  return <DatePickerNode {...props} />;
}
```

### Add a Custom Component

```typescript
{
  id: 'custom-step',
  type: 'custom',
  component: MyCustomComponent,
  componentProps: (state, path) => ({
    // Pass any props based on current state
    value: state.someValue,
    onSubmit: (val) => { /* handled by zipper */ }
  })
}
```

## Comparison

### Before (Ad-hoc Navigation)
- ❌ 200+ lines of navigation logic
- ❌ Closure bugs
- ❌ Hard to extend
- ❌ Tight coupling

### After (Tree Zipper)
- ✅ Declarative tree structure
- ✅ No closure issues
- ✅ Easy to extend
- ✅ Clean separation
- ✅ Reusable across all wizards

## Next Steps

1. Refactor `CapacityAdd.tsx` to use TreeZipper
2. Refactor `NeedAdd.tsx` to use TreeZipper
3. Create shared wizard templates
4. Add more node types as needed (date-picker, file-upload, etc.)

