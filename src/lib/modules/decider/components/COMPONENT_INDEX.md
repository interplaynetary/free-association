# Decider Component Index

Quick reference guide for all Decider components.

## 🎯 Core Components

### DeciderWidget
**Path**: `core/DeciderWidget.svelte`  
**Purpose**: Main entry point - fully integrated Decider interface  
**Props**:
- `user` (object) - GUN user object
- `gameId` (string) - Unique game identifier
- `variant` ('compact'|'inline'|'full') - Display size variant
- `agenda` (string[]) - Array of agenda items

**Usage**:
```svelte
<DeciderWidget {user} gameId="game-123" variant="inline" />
```

---

## 📋 Header Components

### DeciderHeader
**Path**: `header/DeciderHeader.svelte`  
**Purpose**: Main header with game info and current phase  
**Props**: `agendaItem`, `currentPhase`, `phaseStartTime`, `timeWindow`, `participants`, `currentUserPub`, `compact`, `onExpand`

### PhaseIndicator
**Path**: `header/PhaseIndicator.svelte`  
**Purpose**: Visual progress indicator through phases  
**Props**: `currentPhase`, `compact`

### TimeWindowBadge
**Path**: `header/TimeWindowBadge.svelte`  
**Purpose**: Displays time remaining with urgency colors  
**Props**: `phaseStartTime`, `timeWindow`

### ParticipantAvatars
**Path**: `header/ParticipantAvatars.svelte`  
**Purpose**: Shows participant list with avatars  
**Props**: `participants`, `currentUserPub`, `compact`

---

## 📝 Proposal Components

### ProposalCardMini
**Path**: `proposal/ProposalCardMini.svelte`  
**Purpose**: Collapsed proposal view with stats  
**Props**: `proposal`, `currentUserPub`, `challengeCount`, `commentCount`, `modificationCount`, `status`, `onExpand`

### ProposalCardExpanded
**Path**: `proposal/ProposalCardExpanded.svelte`  
**Purpose**: Full detailed proposal view with all data  
**Props**: `proposal`, `currentUserPub`, `currentPhase`, `challenges`, `comments`, `modifications`, `supportExpressions`, `consensusResult`, `onChallenge`, `onComment`, `onModify`, `onSupport`

### ProposalStatusBadge
**Path**: `proposal/ProposalStatusBadge.svelte`  
**Purpose**: Visual status indicator badge  
**Props**: `status` (passed-no-challenges | passed-as-is | in-process | awaiting-support | complete)

---

## ⚡ Phase Action Components

### ProposingPhaseCard
**Path**: `phases/ProposingPhaseCard.svelte`  
**Purpose**: Interface for submitting proposals  
**Props**: `onSubmit`, `participants`, `submittedParticipants`

### ChallengeCard
**Path**: `phases/ChallengeCard.svelte`  
**Purpose**: Interface for raising challenges  
**Props**: `proposalContent`, `proposalAuthor`, `onSubmit`, `onCancel`

### CommentingCard
**Path**: `phases/CommentingCard.svelte`  
**Purpose**: Interface for comments and modifications  
**Props**: `proposalContent`, `challenges`, `onSubmitComment`, `onSubmitModification`, `onClose`

### SupportCard
**Path**: `phases/SupportCard.svelte`  
**Purpose**: Interface for allocating support points  
**Props**: `candidates`, `totalPoints`, `onSubmit`, `onCancel`

---

## 📊 Data Display Components

### ChallengeList
**Path**: `data/ChallengeList.svelte`  
**Purpose**: Displays all challenges for a proposal  
**Props**: `challenges`, `currentUserPub`

### CommentThread
**Path**: `data/CommentThread.svelte`  
**Purpose**: Displays discussion comments  
**Props**: `comments`, `currentUserPub`

### ModificationCandidates
**Path**: `data/ModificationCandidates.svelte`  
**Purpose**: Shows original + all modifications  
**Props**: `originalContent`, `modifications`, `currentUserPub`

### SupportSummary
**Path**: `data/SupportSummary.svelte`  
**Purpose**: Visualizes support distribution  
**Props**: `candidates`, `supportExpressions`, `winningCandidate`

### ConsensusResult
**Path**: `data/ConsensusResult.svelte`  
**Purpose**: Displays final consensus decision  
**Props**: `originalProposal`, `finalDecision`, `authorPub`, `currentUserPub`, `totalSupport`

---

## 🧭 Navigation Components

### ProposalCarousel
**Path**: `navigation/ProposalCarousel.svelte`  
**Purpose**: Horizontal carousel for proposals  
**Props**: `proposals`, `selectedIndex` (bindable), `onSelect`

### DeciderTabs
**Path**: `navigation/DeciderTabs.svelte`  
**Purpose**: Tab navigation between views  
**Props**: `activeTab` (bindable), `onTabChange`, `actionCount`

### QuickActions
**Path**: `navigation/QuickActions.svelte`  
**Purpose**: Floating quick action button  
**Props**: `currentPhase`, `pendingCount`, `onActionClick`

---

## 🪟 Modal Components

### ActionModal
**Path**: `modals/ActionModal.svelte`  
**Purpose**: Generic modal overlay for actions  
**Props**: `isOpen` (bindable), `onClose`, `children` (snippet)

### ExpandedProposalOverlay
**Path**: `modals/ExpandedProposalOverlay.svelte`  
**Purpose**: Full-screen proposal detail view  
**Props**: `isOpen` (bindable), `onClose`, `children` (snippet)

---

## 🔧 Shared/Utility Components

### AuthorBadge
**Path**: `shared/AuthorBadge.svelte`  
**Purpose**: Author identification badge  
**Props**: `authorPub`, `currentUserPub`, `compact`

### ContentCard
**Path**: `shared/ContentCard.svelte`  
**Purpose**: Reusable content card  
**Props**: `content`, `authorPub`, `currentUserPub`, `timestamp`, `type` (challenge|comment|modification)

### PointAllocator
**Path**: `shared/PointAllocator.svelte`  
**Purpose**: Point distribution UI control  
**Props**: `value` (bindable), `max`, `onChange`

### EmptyState
**Path**: `shared/EmptyState.svelte`  
**Purpose**: Empty state placeholder  
**Props**: `message`, `emoji`, `actionLabel`, `onAction`

### LoadingState
**Path**: `shared/LoadingState.svelte`  
**Purpose**: Loading indicator  
**Props**: `message`, `inline`

---

## 🎨 Component Hierarchy

```
DeciderWidget
├─ DeciderHeader
│  ├─ PhaseIndicator
│  ├─ TimeWindowBadge
│  └─ ParticipantAvatars
│
├─ DeciderTabs
│
├─ ProposingPhaseCard (if proposing phase)
│
├─ ProposalCardMini (multiple, in grid/carousel)
│  └─ ProposalStatusBadge
│     └─ AuthorBadge
│
├─ QuickActions
│
├─ ActionModal
│  ├─ ChallengeCard
│  ├─ CommentingCard
│  └─ SupportCard
│     └─ PointAllocator
│
└─ ExpandedProposalOverlay
   └─ ProposalCardExpanded
      ├─ ChallengeList
      │  └─ ContentCard
      ├─ CommentThread
      │  └─ ContentCard
      ├─ ModificationCandidates
      │  └─ ContentCard
      ├─ SupportSummary
      └─ ConsensusResult
         └─ AuthorBadge
```

---

## 📦 Import Patterns

### Import Everything
```typescript
import * as Decider from '$lib/commons/decider/components';
```

### Import Specific Components
```typescript
import { 
  DeciderWidget,
  ProposalCardMini,
  ChallengeList 
} from '$lib/commons/decider/components';
```

### Import Individual Component
```typescript
import DeciderWidget from '$lib/commons/decider/components/core/DeciderWidget.svelte';
```

---

## 🎯 Common Use Cases

### 1. Simple Embedded Decider
```svelte
<DeciderWidget {user} {gameId} variant="inline" />
```

### 2. Compact Sidebar Widget
```svelte
<DeciderWidget {user} {gameId} variant="compact" />
```

### 3. Full Page Experience
```svelte
<DeciderWidget {user} {gameId} variant="full" />
```

### 4. Custom Composition
```svelte
<DeciderHeader {...headerProps} />
<ProposalCarousel {...carouselProps} />
<ProposalCardExpanded {...proposalProps} />
```

---

## 🔄 Data Flow

```
ReactiveP2PDecider (state source)
         ↓
   DeciderWidget (orchestration)
         ↓
  Child Components (presentation)
         ↓
   User Actions (callbacks)
         ↓
ReactiveP2PDecider (state update)
```

All components are reactive and update automatically when the underlying Decider state changes.

