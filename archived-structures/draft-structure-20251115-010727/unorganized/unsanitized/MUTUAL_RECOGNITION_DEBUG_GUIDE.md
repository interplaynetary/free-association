# Mutual Recognition Debug Guide

## Overview
This guide explains how to trace the complete mutual recognition flow using the console logs.

## The Complete Flow (Step by Step)

When two users recognize each other, here's what should happen:

### 1. 🌳 Tree Update (User A adds User B as contributor)
**What to look for:**
```
[🌳 RECOGNITION-WEIGHTS] Computing from tree...
[🌳 RECOGNITION-WEIGHTS] ✅ Computed X contributors (Y non-zero):
  • <UserB_pubkey>... → XX.XX%
```

**What this means:** User A's recognition tree has been updated, and recognition weights are being computed.

---

### 2. 🔄 Auto-Subscription (User A subscribes to User B)
**What to look for:**
```
[🔄 AUTO-SUB] Syncing subscriptions with tree...
[🔄 AUTO-SUB] Tree has X contributors
[🔄 AUTO-SUB] ➕ Subscribing to: <UserB_pubkey>... (will receive their commitment)
[📡 NETWORK-SUB] ✅ Subscribed to <UserB_pubkey>... commitment
```

**What this means:** Auto-subscription detected User B in the tree and created a Holster subscription to receive their commitment.

---

### 3. 📝 Commitment Composition (User A publishes updated recognition)
**What to look for:**
```
[📝 COMPOSE] Composing commitment from sources...
[📝 COMPOSE] ✅ Composed commitment:
  • Recognition: X entries (Y non-zero)
  • Mutual Recognition: X entries (Y non-zero)
[📝 COMPOSE] Recognition weights being published:
    • <UserB_pubkey>... → XX.XX%
[💾 AUTO-COMPOSE] Publishing updated commitment to network (tree changed)...
[💾 AUTO-COMPOSE] ✅ Updated commitment recognition (tree changed) - now persisting to Holster
```

**What this means:** User A's commitment has been composed with the new recognition weights and is being published to Holster for User B to receive.

---

### 4. 📡 Network Reception (User B receives User A's commitment)
**On User B's console, look for:**
```
[📡 NETWORK-SUB] Received commitment from <UserA_pubkey>...
[📡 NETWORK-SUB] Commitment contains X recognition entries (Y non-zero)
[📡 NETWORK-SUB] Normalized recognition weights for <UserA_pubkey>...
[📡 NETWORK-SUB] ✅ Updated [recognition] from <UserA_pubkey>...
```

**What this means:** User B has received User A's commitment via Holster, including User A's recognition of User B.

---

### 5. 🤝 Mutual Recognition Computation (Both users)
**What to look for:**
```
[🤝 MUTUAL-REC] Computing mutual recognition...
[🤝 MUTUAL-REC] My recognition: X entries (Y non-zero)
[🤝 MUTUAL-REC] Network has X users' recognition data
[🤝 MUTUAL-REC]   <OtherUser_pubkey>...: I→them=XX.XX%, them→me=XX.XX%, MR=XX.XX%
[🤝 MUTUAL-REC] ✅ Computed X mutual relationships
```

**What this means:** Mutual recognition is being computed using the formula: MR = min(myRecognition[them], theirRecognition[me])

**Key check:** If `them→me=0.00%`, it means you haven't received their commitment yet, or they don't recognize you.

---

### 6. 📊 UI Visualization (Bars update)
**What to look for:**
```
[📊 UI-YR] Recognition weights changed - generating segments for bar...
[📊 UI-YR] ✅ Generated X segments for recognition bar:
  • <User_pubkey>... → XX.XX%

[📊 UI-MR] Mutual recognition changed - generating segments for bar...
[📊 UI-MR] ✅ Generated X segments for mutual recognition bar:
  • <User_pubkey>... → XX.XX%
```

**What this means:** The UI is rendering the bars with the computed recognition and mutual recognition values.

---

## Common Issues & What to Look For

### Issue: Mutual Recognition is 0% even though both users recognize each other

**Debug Steps:**

1. **Check User A's recognition weights:**
   - Look for `[🌳 RECOGNITION-WEIGHTS]` - Does it show User B with >0%?
   - If NO: User A's tree doesn't properly recognize User B

2. **Check User A's commitment publication:**
   - Look for `[📝 COMPOSE] Recognition weights being published` - Is User B listed?
   - If NO: Recognition weights aren't being included in commitment

3. **Check User B received User A's commitment:**
   - On User B's console, look for `[📡 NETWORK-SUB] Received commitment from <UserA_pubkey>`
   - If NO: Network synchronization issue (Holster not delivering)

4. **Check User B's mutual recognition computation:**
   - Look for `[🤝 MUTUAL-REC] <UserA_pubkey>...: I→them=XX%, them→me=XX%`
   - If `them→me=0%`: User B hasn't received User A's commitment
   - If `I→them=0%`: User B doesn't recognize User A in their tree

5. **Check the reverse flow (User B → User A):**
   - Repeat steps 1-4 from User B's perspective

---

## Quick Diagnostic Commands

Open browser console and run:

```javascript
// Check current subscriptions
window.debugStoresV5()

// Check mutual recognition directly
const { myMutualRecognition } = await import('$lib/commons/v5/stores.svelte');
const { get } = await import('svelte/store');
console.log('Current MR:', get(myMutualRecognition));

// Check recognition weights
const { myRecognitionWeights } = await import('$lib/commons/v5/stores.svelte');
console.log('My Recognition:', get(myRecognitionWeights));

// Check network commitments
const { getNetworkCommitmentsRecord } = await import('$lib/commons/v5/stores.svelte');
console.log('Network Commitments:', getNetworkCommitmentsRecord());
```

---

## Expected Console Output Order

For successful mutual recognition between User A and User B:

**User A's Console:**
1. 🌳 Tree update → Recognition weights computed
2. 🔄 Auto-subscription → Subscribed to User B
3. 📝 Commitment composed → Recognition published
4. 💾 Commitment persisted to Holster
5. 📡 Received User B's commitment (if User B also recognizes A)
6. 🤝 Mutual recognition computed → MR > 0%
7. 📊 UI bars updated

**User B's Console:**
1. 📡 Received User A's commitment
2. 🤝 Mutual recognition computed → Updated with User A's recognition
3. 📊 UI bars updated

---

## Testing Checklist

- [ ] User A adds User B to recognition tree
- [ ] User A's console shows recognition weights computed
- [ ] User A's console shows subscription to User B created
- [ ] User A's console shows commitment published with User B's recognition
- [ ] User B's console shows received commitment from User A
- [ ] User B adds User A to recognition tree (for mutual recognition)
- [ ] User B's console shows recognition weights computed
- [ ] User B's console shows commitment published with User A's recognition
- [ ] User A's console shows received commitment from User B
- [ ] Both users' consoles show mutual recognition computed with non-zero MR
- [ ] Both users' UI bars display mutual recognition segments

---

## Log Emoji Legend

- 🌳 = Recognition tree operations
- 🔄 = Auto-subscription management
- 📝 = Commitment composition
- 💾 = Persistence to Holster
- 📡 = Network synchronization (receiving data)
- 🤝 = Mutual recognition computation
- 📊 = UI visualization updates
- ✅ = Success
- ❌ = Error or missing data
- ⏭️ = Skipped operation


