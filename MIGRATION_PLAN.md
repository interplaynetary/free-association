# Gun to Holster Migration Plan

## Overview
This document outlines the step-by-step process to migrate from Gun.js to Holster in the free-association application.

## Phase 1: Preparation and Setup

### 1.1 Install Holster
```bash
# Install Holster from GitHub (assuming no npm package yet)
npm install https://github.com/mblaney/holster.git
```

### 1.2 Key API Differences

| Gun API | Holster API | Notes |
|---------|-------------|-------|
| `Gun()` | `Holster()` | Constructor |
| `gun.user()` | `holster.user()` | User instance |
| `user.create()` | `user.create()` | Same signature |
| `user.auth()` | `user.auth()` | Same signature |
| `user.recall()` | `user.recall()` | Simplified - no options |
| `user.leave()` | `user.leave()` | Same |
| `gun.get()` | `holster.get()` | Same signature |
| `gun.put()` | `holster.put()` | Same signature |
| `gun.on()` | `holster.on()` | Same signature |
| `gun.once()` | `holster.get(...).once()` | Slightly different |
| `Gun.SEA` | `holster.SEA` | Same functionality |

## Phase 2: Core Migration Steps

### 2.1 Replace State Management (Priority: High)

Files to update:
- `src/lib/state/holster.svelte.ts` → Replace Gun imports with Holster
- `src/lib/state/gun.svelte.ts` → Deprecate this file
- `src/lib/state/index.svelte.ts` → Update exports

### 2.2 Update Authentication System

Current Gun patterns:
```typescript
// Gun
const gun = new Gun({ peers: [...], localStorage: false, radisk: true });
const user = gun.user().recall({ sessionStorage: true });
gun.on('auth', callback);
```

New Holster patterns:
```typescript
// Holster
const holster = Holster({ peers: [...], indexedDB: true, secure: true });
const user = holster.user();
user.recall(); // Automatically checks localStorage and sessionStorage
// No global auth event - check user.is after operations
```

### 2.3 Update Data Access Patterns

Current Gun patterns:
```typescript
// Gun
gun.get('data').put(value);
gun.get('data').on(callback);
gun.get('data').once(callback);
user.get('private-data').put(value);
```

New Holster patterns:
```typescript
// Holster
holster.get('data').put(value);
holster.get('data').on(callback);
holster.get('data', callback); // once is integrated
user.get('private-data').put(value);
```

## Phase 3: File-by-File Migration

### 3.1 Core State Files

#### `src/lib/state/holster.svelte.ts`
- ✅ **Created**: `src/lib/state/holster-new.svelte.ts`
- Replace Gun imports with Holster
- Update peer configuration (Gun uses HTTP, Holster uses WebSockets)
- Simplify authentication flow (Holster has cleaner API)

#### `src/lib/state/network.svelte.ts`
- Update import: `import { holster, user, userPub, usersList } from './holster-new.svelte';`
- Replace Gun-specific patterns:
  - `gun.user(messageData)` → `user.get([pubkey, 'path'])`
  - `GUN.state.is()` → Use timestamps differently
  - `gun.back('opt.peers')` → `holster.wire` API

#### `src/lib/state/chat.svelte.ts`
- Update import from Gun to Holster
- Chat message storage pattern remains similar
- Update real-time subscriptions

#### `src/lib/state/users.svelte.ts`
- Update user resolution patterns
- Replace `gun.user(pubkey)` with `user.get([pubkey, 'field'])`

### 3.2 Component Updates

#### Files using Gun state:
- `src/lib/components/Chat.svelte`
- `src/lib/components/extra/SimpleNotificationTest.svelte`
- `src/lib/examples/messaging/*.svelte.ts`
- `src/lib/examples/notifications/*.ts`

### 3.3 Service Worker Updates
- `src/service-worker.ts`
- `src/lib/examples/notifications/service-worker.ts`

Update Gun peer references and initialization patterns.

## Phase 4: Testing and Validation

### 4.1 Data Migration Considerations
- Holster uses similar graph database structure
- User accounts should be compatible (both use SEA for crypto)
- Data keys and references should work similarly
- Test data persistence and retrieval

### 4.2 Peer Network Configuration
Current Gun peers:
```
'https://gun-manhattan.herokuapp.com/gun'
'https://peer.wallie.io/gun'
'https://gun.defucc.me/gun'
```

Need Holster-compatible peers:
```
'wss://holster-peer-1.example.com'
'wss://holster-peer-2.example.com'
```

### 4.3 Key Testing Areas
1. **Authentication**: Login, signup, recall, logout
2. **Real-time sync**: Message broadcasting, presence updates
3. **Data persistence**: Offline/online behavior
4. **Cross-device sync**: Multi-device authentication
5. **Network resilience**: Peer connection handling

## Phase 5: Deployment Strategy

### 5.1 Gradual Migration Approach
1. **Parallel Run**: Keep both Gun and Holster systems running
2. **Feature Flag**: Toggle between Gun and Holster per user/session
3. **Data Bridge**: Sync data between both systems during transition
4. **Gradual Rollout**: Move users to Holster incrementally
5. **Full Cutover**: Remove Gun dependency once stable

### 5.2 Rollback Plan
- Keep Gun code available for quick rollback
- Monitor error rates and performance metrics
- Have database backup/restore procedures

## Phase 6: Benefits Expected

### Performance Improvements
- Better disk write reliability (primary reason for migration)
- Cleaner codebase with less complex authentication flows
- Better error handling and debugging

### Code Quality Improvements
- More readable API patterns
- Better separation of concerns
- Improved test coverage opportunities

## Migration Checklist

- [ ] Install Holster package
- [ ] Create new Holster state management
- [ ] Update authentication system
- [ ] Migrate chat functionality
- [ ] Update network data streams
- [ ] Update all import statements
- [ ] Configure Holster-compatible peers
- [ ] Test data persistence
- [ ] Test real-time synchronization
- [ ] Test multi-device authentication
- [ ] Performance testing
- [ ] Deploy with feature flag
- [ ] Monitor and validate
- [ ] Complete cutover
- [ ] Remove Gun dependency

## Estimated Timeline
- **Phase 1-2**: 1-2 days (Setup and core migration)
- **Phase 3**: 3-5 days (File-by-file updates)
- **Phase 4**: 2-3 days (Testing and validation)
- **Phase 5**: 1-2 weeks (Gradual deployment)

**Total Estimated Time**: 2-3 weeks for complete migration 