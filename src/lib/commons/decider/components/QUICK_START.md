# Quick Start Guide

Get up and running with Decider components in 5 minutes!

## Installation

Components are already in your project at:
```
src/lib/commons/decider/components/
```

## Basic Usage

### 1. Import the Main Widget

```svelte
<script lang="ts">
  import { DeciderWidget } from '$lib/commons/decider/components';
  
  // Your GUN user (from your app's auth system)
  export let user: any;
  
  // Unique identifier for this decision game
  let gameId = 'dinner-decision-2024';
  
  // Optional: Set custom agenda
  let agenda = ['What should we have for dinner?'];
</script>

<DeciderWidget {user} {gameId} {agenda} variant="inline" />
```

That's it! You now have a fully functional Decider interface.

---

## Variants

### Compact (Sidebar Widget)
Perfect for sidebars or small spaces (~300px wide):

```svelte
<DeciderWidget {user} {gameId} variant="compact" />
```

### Inline (Embedded in Page)
Ideal for embedding in existing pages (~600px max):

```svelte
<DeciderWidget {user} {gameId} variant="inline" />
```

### Full (Full-Page Experience)
Best for dedicated decision pages (~800px max):

```svelte
<DeciderWidget {user} {gameId} variant="full" />
```

---

## Complete Example

```svelte
<script lang="ts">
  import { DeciderWidget } from '$lib/commons/decider/components';
  import { onMount } from 'svelte';
  
  // Initialize your GUN user
  let user: any;
  
  onMount(() => {
    // Your GUN initialization here
    // user = gun.user();
    // user.auth(...)
  });
  
  let gameId = `decision-${Date.now()}`;
  let agenda = [
    'What should we have for dinner?',
    'What movie should we watch?',
    'When should we meet next?'
  ];
</script>

{#if user}
  <div class="container">
    <h1>Group Decision Time!</h1>
    <DeciderWidget {user} {gameId} {agenda} variant="inline" />
  </div>
{:else}
  <p>Please log in to participate</p>
{/if}

<style>
  .container {
    max-width: 1200px;
    margin: 0 auto;
    padding: 2rem;
  }
  
  h1 {
    text-align: center;
    margin-bottom: 2rem;
  }
</style>
```

---

## Using Individual Components

You can also compose components manually:

```svelte
<script lang="ts">
  import { 
    DeciderHeader,
    ProposalCardMini,
    ChallengeList,
    PhaseIndicator
  } from '$lib/commons/decider/components';
  
  // Your state
  let currentPhase = 'proposing';
  let participants = ['alice', 'bob', 'carol'];
  let currentUserPub = 'alice';
</script>

<DeciderHeader
  agendaItem="What should we build next?"
  {currentPhase}
  phaseStartTime={Date.now()}
  timeWindow={86400000}
  {participants}
  {currentUserPub}
/>

<PhaseIndicator {currentPhase} />

<!-- More components... -->
```

---

## Customizing Styles

Override CSS variables to match your brand:

```svelte
<div class="my-decider">
  <DeciderWidget {user} {gameId} variant="inline" />
</div>

<style>
  .my-decider {
    --primary-color: #ff6b6b;
    --success-color: #51cf66;
    --border-color: #dee2e6;
    --text-primary: #212529;
  }
</style>
```

---

## Available CSS Variables

```css
:root {
  /* Colors */
  --primary-color: #667eea;
  --success-color: #4caf50;
  --border-color: #e0e0e0;
  
  /* Backgrounds */
  --bg-muted: #f0f0f0;
  --bg-light: #f8f9fa;
  --card-bg: white;
  
  /* Text */
  --text-primary: #333;
  --text-secondary: #555;
  --text-muted: #666;
  
  /* Badge colors (for author badges, etc.) */
  --badge-bg: #e3f2fd;
  --badge-color: #1976d2;
  
  /* Avatar colors */
  --avatar-bg: #e3f2fd;
  --avatar-color: #1976d2;
}
```

---

## Common Patterns

### Pattern 1: Single Decision
```svelte
<script>
  import { DeciderWidget } from '$lib/commons/decider/components';
  
  let user = /* your user */;
  let gameId = 'single-decision';
  let agenda = ['Should we implement feature X?'];
</script>

<DeciderWidget {user} {gameId} {agenda} variant="inline" />
```

### Pattern 2: Multiple Related Decisions
```svelte
<script>
  import { DeciderWidget } from '$lib/commons/decider/components';
  
  let user = /* your user */;
  let gameId = 'sprint-planning';
  let agenda = [
    'Which features should we prioritize?',
    'How many story points can we commit to?',
    'Who will lead each initiative?'
  ];
</script>

<DeciderWidget {user} {gameId} {agenda} variant="full" />
```

### Pattern 3: Embedded in Dashboard
```svelte
<div class="dashboard">
  <aside class="sidebar">
    <DeciderWidget {user} gameId="quick-poll" variant="compact" />
  </aside>
  
  <main class="content">
    <!-- Your main content -->
  </main>
</div>
```

---

## Understanding the Flow

### Phase Progression

1. **Proposing** → Everyone submits their ideas
2. **Challenging** → Participants raise concerns
3. **Commenting** → Discussion and modifications
4. **Supporting** → Allocate points to preferred versions
5. **Complete** → View final consensus

### Early Exits

- No challenges? → Proposal passes immediately
- No modifications? → Original proposal passes as-is
- Modifications proposed? → Support phase determines winner

---

## Next Steps

- Read [README.md](./README.md) for design principles
- See [COMPONENT_INDEX.md](./COMPONENT_INDEX.md) for component reference
- Check [SUMMARY.md](./SUMMARY.md) for complete overview
- Explore individual components in `./components/`

---

## Need Help?

All components are documented with JSDoc comments. Hover over props in your editor for instant documentation!

```svelte
<DeciderWidget
  {user}          <!-- Required: GUN user object -->
  {gameId}        <!-- Required: Unique game ID -->
  variant="inline" <!-- Optional: Display variant -->
  {agenda}        <!-- Optional: Array of agenda items -->
/>
```

---

## Tips

1. **Unique Game IDs**: Use unique `gameId` for each decision to avoid conflicts
2. **Persist Game IDs**: Store `gameId` if you want to revisit decisions later
3. **User Authentication**: Ensure users are authenticated before showing Decider
4. **Multiple Instances**: You can have multiple Decider instances on one page
5. **Responsive**: Components adapt to screen size automatically

---

Happy Deciding! 🎯

