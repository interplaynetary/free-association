# Decider Components

A comprehensive library of Svelte 5 components for building collaborative decision-making interfaces.

## Architecture

This component library follows a modular, composable architecture with clear separation of concerns:

```
components/
├── core/                    # Main entry point widgets
│   └── DeciderWidget.svelte    # Primary embeddable component
├── header/                  # Header and status components
│   ├── DeciderHeader.svelte
│   ├── PhaseIndicator.svelte
│   ├── TimeWindowBadge.svelte
│   └── ParticipantAvatars.svelte
├── proposal/                # Proposal display components
│   ├── ProposalCardMini.svelte
│   ├── ProposalCardExpanded.svelte
│   └── ProposalStatusBadge.svelte
├── phases/                  # Phase-specific action components
│   ├── ProposingPhaseCard.svelte
│   ├── ChallengeCard.svelte
│   ├── CommentingCard.svelte
│   └── SupportCard.svelte
├── data/                    # Data display components
│   ├── ChallengeList.svelte
│   ├── CommentThread.svelte
│   ├── ModificationCandidates.svelte
│   ├── SupportSummary.svelte
│   └── ConsensusResult.svelte
├── navigation/              # Navigation and routing components
│   ├── ProposalCarousel.svelte
│   ├── DeciderTabs.svelte
│   └── QuickActions.svelte
├── modals/                  # Modal and overlay components
│   ├── ActionModal.svelte
│   └── ExpandedProposalOverlay.svelte
└── shared/                  # Shared utility components
    ├── AuthorBadge.svelte
    ├── ContentCard.svelte
    ├── PointAllocator.svelte
    ├── EmptyState.svelte
    └── LoadingState.svelte
```

## Design Principles

### 1. Svelte 5 Best Practices
- Use `$props()`, `$derived`, and `$state` runes
- Minimize `$effect` usage
- No direct DOM manipulation
- Leverage Svelte's reactivity system

### 2. Component Design
- **Single Responsibility**: Each component has one clear purpose
- **Composability**: Components work together seamlessly
- **Reusability**: Shared components are truly generic
- **Props Over State**: Prefer props and derived values over internal state

### 3. Clean & Minimal Design
- Elegant, intuitive UI
- Clear visual hierarchy
- Consistent spacing and typography
- Purposeful use of icons and emojis
- Responsive and accessible

## Usage Examples

### Basic Usage

```svelte
<script>
  import { DeciderWidget } from '$lib/commons/decider/components';
  
  let user = /* your GUN user */;
  let gameId = 'my-decision-game';
</script>

<DeciderWidget {user} {gameId} variant="inline" />
```

### Variants

#### Compact Widget (300x400px card)
```svelte
<DeciderWidget {user} {gameId} variant="compact" />
```

#### Inline Widget (Embedded in page)
```svelte
<DeciderWidget {user} {gameId} variant="inline" />
```

#### Full Widget (Full screen experience)
```svelte
<DeciderWidget {user} {gameId} variant="full" />
```

### Custom Composition

You can also compose individual components:

```svelte
<script>
  import { 
    DeciderHeader, 
    ProposalCardMini,
    ChallengeList 
  } from '$lib/commons/decider/components';
</script>

<DeciderHeader
  agendaItem="What should we have for dinner?"
  currentPhase="challenging"
  phaseStartTime={Date.now()}
  timeWindow={86400000}
  participants={['alice', 'bob', 'carol']}
  currentUserPub="alice"
/>

<ProposalCardMini
  proposal={{ content: 'Pizza', authorPub: 'alice' }}
  currentUserPub="alice"
  challengeCount={2}
  status="in-process"
  onExpand={() => console.log('expand')}
/>
```

## Component Categories

### Core Components
**DeciderWidget** - The main entry point. Handles initialization, state management, and composition of all sub-components.

### Header Components
Display current state and progress:
- **DeciderHeader**: Main header with agenda, phase, and meta info
- **PhaseIndicator**: Visual progress through phases
- **TimeWindowBadge**: Time remaining countdown
- **ParticipantAvatars**: Participant list display

### Proposal Components
Display proposal information:
- **ProposalCardMini**: Collapsed proposal card with essential info
- **ProposalCardExpanded**: Full detailed proposal view
- **ProposalStatusBadge**: Visual status indicator

### Phase-Specific Components
Actions for each phase:
- **ProposingPhaseCard**: Submit new proposals
- **ChallengeCard**: Raise challenges
- **CommentingCard**: Add comments or modifications
- **SupportCard**: Allocate support points

### Data Display Components
Show various data types:
- **ChallengeList**: List of challenges with metadata
- **CommentThread**: Discussion thread display
- **ModificationCandidates**: Original + modified versions
- **SupportSummary**: Support distribution visualization
- **ConsensusResult**: Final consensus decision

### Navigation Components
Navigate through the interface:
- **ProposalCarousel**: Horizontal proposal navigation
- **DeciderTabs**: Tab-based view switching
- **QuickActions**: Floating quick action button

### Modal Components
Overlay interfaces:
- **ActionModal**: Generic modal for actions
- **ExpandedProposalOverlay**: Full-screen proposal view

### Shared Components
Reusable utilities:
- **AuthorBadge**: Author identification badge
- **ContentCard**: Generic content card
- **PointAllocator**: Point distribution UI
- **EmptyState**: Empty state placeholders
- **LoadingState**: Loading indicators

## Styling

All components use CSS custom properties for theming:

```css
:root {
  --primary-color: #667eea;
  --success-color: #4caf50;
  --border-color: #e0e0e0;
  --bg-muted: #f0f0f0;
  --text-primary: #333;
  --text-muted: #666;
}
```

Override these in your app to customize the appearance.

## Reactivity

Components consume reactive state from `ReactiveP2PDecider`:
- `currentPhase` - Current phase name
- `allProposals` - All proposals array
- `allChallenges` - Map of challenges by proposal
- `allComments` - Map of comments by proposal
- `allModifications` - Map of modifications by proposal
- `allSupport` - Map of support expressions by proposal
- `consensusResults` - Map of consensus results by proposal
- `config` - Game configuration

All state updates automatically propagate through the component tree.

## Accessibility

All components include:
- Proper ARIA labels
- Keyboard navigation support
- Screen reader friendly markup
- Focus management
- Semantic HTML

## Performance

- Minimal re-renders through proper use of `$derived`
- No unnecessary effects
- Efficient list rendering with keyed each blocks
- Lazy loading of heavy components
- Optimized animations

## Future Enhancements

Potential additions:
- Dark mode support
- Animation presets
- Keyboard shortcuts panel
- Export/import functionality
- Activity notifications
- Real-time presence indicators
- Accessibility options panel

