# Holster vs API Architecture: Responsibilities & Boundaries

## Overview

Your server has **two distinct architectural layers** working in harmony:

1. **Holster** - P2P/realtime data synchronization layer
2. **API Routes** - Traditional HTTP/REST service layer

Understanding when to use each is crucial for maintaining a clean, scalable architecture.

---

## 🎯 Holster Server Responsibilities

### What Holster IS:
**A decentralized, cryptographically-secured, realtime graph database server**

#### Core Responsibilities:

1. **User Space Management**
   - Store user-owned data under their public key
   - Sign all writes to verify authenticity
   - Provide cryptographic guarantees (SEA)

2. **Realtime Synchronization**
   - WebSocket-based pub/sub
   - Push updates to all connected clients instantly
   - Cross-device state synchronization

3. **P2P Data Model**
   - Graph-based storage (nodes and references)
   - Conflict resolution through timestamps
   - Eventually consistent data

4. **Client-Owned Data**
   - Users control their own data
   - Data lives under user's public key
   - Encrypted by default (if desired)

5. **Offline-First Capabilities**
   - Clients can read/write locally
   - Sync when connection available
   - No server required for local operations

---

## 🎯 API Routes Responsibilities

### What API Routes ARE:
**Traditional server-side logic and external integrations**

#### Core Responsibilities:

1. **Server-Side Validation & Business Logic**
   - Invite code validation and claiming
   - Feed subscription limits
   - Email validation flows
   - Password reset flows

2. **External Service Integration**
   - RSS feed management (external API calls)
   - Email sending (SMTP)
   - AI/LLM services (OpenRouter)
   - Payment processing (if needed)

3. **Centralized Rate Limiting**
   - API key management
   - Request throttling
   - Token-based limits

4. **Administrative Operations**
   - Creating invite codes
   - Updating feed limits
   - Performance monitoring
   - Health checks

5. **Stateless Operations**
   - One-off requests
   - Validation endpoints
   - Status checks
   - Non-reactive queries

---

## 🔀 The Key Distinction

### Use Holster (User Space) When:

✅ **Data is USER-OWNED and needs to be:**
- Synced across devices
- Updated in realtime
- Accessed offline
- Encrypted/signed by the user
- Shared peer-to-peer

**Examples from your codebase:**
```typescript
// User's personal data (belongs to them)
user.get("capacities").put(userCapacities)
user.get("quests").put(userQuests)
user.get("preferences").put(settings)

// Realtime updates
user.get("messages").on((msg) => updateUI(msg))
user.get("presence").on((presence) => showOnlineStatus(presence))
```

**Use Cases:**
- User profiles and preferences
- User's quest progress
- Personal notes and data
- Realtime chat/presence
- Client-side state that needs sync
- Collaborative documents
- Activity feeds (user's view)

---

### Use API Routes When:

✅ **Operations require SERVER AUTHORITY or:**
- External service calls
- Centralized validation
- Administrative privileges
- Rate limiting
- Stateless requests
- Non-reactive data

**Examples from your codebase:**
```typescript
// Server-owned operations
POST /api/private/create-invite-codes  // Admin action
POST /api/add-feed                      // External RSS API
POST /api/ai/completion                 // External AI service
GET  /api/health                        // Server status
POST /api/validate-email                // Email sending

// Validation with server authority
POST /api/claim-invite-code             // One-time claim validation
POST /api/check-invite-code             // Server verifies availability
```

**Use Cases:**
- Account creation/claiming (one-time setup)
- Invite code management (server-controlled resource)
- RSS feed integration (external service)
- AI requests (expensive, rate-limited)
- Email operations (server-side SMTP)
- Performance monitoring
- Administrative operations
- Payment processing
- Analytics aggregation

---

## 🏗️ Current Architecture Analysis

### Your Holster Server (`/lib/server/holster/`)

```typescript
// Core Holster operations (src/lib/server/holster/core.ts)
const holster = Holster({secure: true, memoryLimit: 1536})
const user = holster.user()

// Server owns the "host" account
user.auth(username, password)

// Server can manage shared resources
user.get("accounts")        // Account records
user.get("feeds")           // RSS feed metadata
user.get("available")       // Available invite codes
user.get("shared")          // Shared invite codes
```

**Responsibilities:**
1. ✅ Authenticate as "host" user
2. ✅ Store account metadata (encrypted emails, validation codes)
3. ✅ Manage shared resources (feeds, invite codes)
4. ✅ Provide realtime access to graph data
5. ✅ Handle WebSocket connections from clients

---

### Your API Routes (`/routes/api/`)

```typescript
// API endpoints handle:
1. Invite code lifecycle (claim, check, validate)
2. RSS feed operations (add, remove, subscriber management)
3. AI/LLM requests (quest generation, completions)
4. Email operations (validation, password reset)
5. Admin operations (create codes, update limits)
6. Health & monitoring
7. Data relay (external sources)
```

**Responsibilities:**
1. ✅ Validate requests with schemas (Zod)
2. ✅ Authenticate with unified auth
3. ✅ Rate limit requests
4. ✅ Call external services
5. ✅ Perform server-side logic
6. ✅ Write results to Holster when needed

---

## 🔄 Hybrid Patterns (The Interesting Part!)

Many of your operations use **BOTH** layers in sequence:

### Pattern 1: API → Holster Write → Client Reads

```typescript
// API route handles validation + external call
POST /api/claim-invite-code
  ↓
1. Validate invite code (API logic)
2. Create account in Holster user space
3. Send validation email (external service)
4. Return success to client
  ↓
// Client now reads their account from Holster
user.get("accounts").next(code).on((account) => {
  // Realtime updates if account changes
})
```

**This is CORRECT!** The API provides the "write authority" and validation, then stores the result in Holster for sync/realtime access.

---

### Pattern 2: Client → Holster Read → Display

```typescript
// Client reads their own data directly from Holster
const user = holster.user()
await user.auth(username, password)

// No API needed - direct P2P access
user.get("quests").on((quests) => {
  renderQuests(quests)
})

user.get("capacities").on((capacities) => {
  updateCapacities(capacities)
})
```

**This is CORRECT!** User-owned data doesn't need API validation to read.

---

### Pattern 3: API Orchestration → Holster + External

```typescript
// API orchestrates complex operations
POST /api/add-feed
  ↓
1. Validate user's feed limit (server logic)
2. Call external RSS API (external service)
3. Store feed metadata in Holster (for sync)
4. Update user's subscription count (server logic)
5. Return feed details to client
  ↓
// Clients can now subscribe to feed updates
user.get("feeds").next(feedUrl).on((feedData) => {
  // Realtime feed updates
})
```

**This is CORRECT!** API handles complexity and external calls, stores result in Holster for realtime access.

---

## 📋 Decision Matrix

### Should I use Holster or API?

Ask these questions:

| Question | Holster | API |
|----------|---------|-----|
| **Does data belong to a specific user?** | ✅ | ❌ |
| **Needs realtime updates?** | ✅ | ❌ |
| **Needs to work offline?** | ✅ | ❌ |
| **Should sync across devices?** | ✅ | ❌ |
| **Requires server validation?** | ❌ | ✅ |
| **Calls external services?** | ❌ | ✅ |
| **Administrative action?** | ❌ | ✅ |
| **Rate limited?** | ❌ | ✅ |
| **Needs server-side processing?** | ❌ | ✅ |
| **One-time operation?** | ❌ | ✅ |
| **Requires payment/billing?** | ❌ | ✅ |

---

## 🎨 Architecture Patterns in Your App

### Pattern A: API as Gateway, Holster as State Store

```typescript
// API validates and writes
POST /api/private/update-feed-limit
  → Validates admin auth
  → Updates: user.get("accounts").next(code).put({feeds: limit})
  → Returns success

// Client reads the updated state
user.get("accounts").next(myCode).on((account) => {
  showFeedLimit(account.feeds)
})
```

**Why this works:**
- API enforces business rules (only admins can update)
- Holster stores the result (syncs to all devices)
- Clients get realtime updates automatically

---

### Pattern B: Direct Client Access for User Data

```typescript
// No API needed - client owns this data
user.get("preferences").put({
  theme: "dark",
  notifications: true
})

user.get("quests").get(questId).put({
  completed: true,
  completedAt: Date.now()
})
```

**Why this works:**
- User owns this data completely
- No server validation needed
- Works offline
- Syncs automatically
- End-to-end encrypted (if desired)

---

### Pattern C: API for Expensive/External Operations

```typescript
// AI generation - too expensive for client to do directly
POST /api/llm/quest-generation
  → Rate limiting (server control)
  → API key management (server secret)
  → Call OpenRouter (external service)
  → Return generated quests
  
// Client stores the result in their space
user.get("quests").put(generatedQuests, true)
```

**Why this works:**
- Server controls expensive operations
- Rate limiting prevents abuse
- API keys stay secret
- Client stores result in their user space
- Future access is direct (no API needed)

---

## 🚨 Anti-Patterns to Avoid

### ❌ DON'T: Use API for User-Owned Data Access

```typescript
// BAD: API route to get user's quests
GET /api/user/quests
  → Fetch from database
  → Return to client

// GOOD: Client reads directly from Holster
user.get("quests").once((quests) => {
  renderQuests(quests)
})
```

**Why Bad:**
- Unnecessary API request
- No realtime updates
- Doesn't work offline
- Server becomes bottleneck

---

### ❌ DON'T: Use Holster for Administrative Operations

```typescript
// BAD: Let any client create invite codes
user.get("available").next("invite_codes").put(newCode)

// GOOD: API route with auth
POST /api/private/create-invite-codes
  → Check admin authentication
  → Create codes with server authority
```

**Why Bad:**
- No access control
- Anyone could create codes
- No rate limiting
- Security vulnerability

---

### ❌ DON'T: Use API for Realtime Data

```typescript
// BAD: Poll API for updates
setInterval(() => {
  fetch('/api/messages').then(...)
}, 1000)

// GOOD: Subscribe to Holster updates
user.get("messages").on((messages) => {
  updateMessagesUI(messages)
})
```

**Why Bad:**
- Wasteful polling
- Higher latency
- Server load
- No offline support

---

### ❌ DON'T: Use Holster for External Service Calls

```typescript
// BAD: Client calls OpenRouter directly
// (exposes API keys, no rate limiting)

// GOOD: API route proxies the call
POST /api/ai/completion
  → Server manages API keys
  → Rate limiting applied
  → Logs requests
  → Returns result
```

**Why Bad:**
- Exposes API keys
- No rate limiting
- No audit trail
- Client-side secrets

---

## 🔐 Security Considerations

### Holster Security Model:
1. **User Space is Signed** - All writes verified with user's private key
2. **Encryption Available** - SEA provides E2E encryption
3. **Public Keys as Identity** - No username collisions
4. **Server Can't Forge** - Users own their private keys

### API Security Model:
1. **Server Authority** - Server validates all requests
2. **Centralized Auth** - JWT/API keys/Basic auth
3. **Rate Limiting** - Prevent abuse
4. **Secret Management** - Server keeps secrets safe

### Hybrid Security:
```typescript
// API creates account (server authority)
POST /api/claim-invite-code
  → Validates invite code (server check)
  → Creates account in Holster (writes to user space)
  → Encrypts email (SEA encryption)
  
// User can now read their own data (cryptographic proof)
user.auth(username, password)
user.get("email").once(async (encrypted) => {
  const email = await user.SEA.decrypt(encrypted, user.is)
})
```

**Best of Both Worlds:**
- Server validates the operation
- User owns the resulting data
- Cryptographic proof of ownership
- Server can't access encrypted data

---

## 📊 Your Current Architecture (Analyzed)

### Excellent Separation ✅

1. **Invite Code System**
   - API: Claim, check, validate (server authority)
   - Holster: Store account records (user ownership)
   - ✅ Correct separation

2. **RSS Feeds**
   - API: Add/remove feeds (external service)
   - Holster: Store feed metadata (shared state)
   - ✅ Correct separation

3. **AI/LLM**
   - API: Generate quests (expensive operation)
   - Holster: Store user's quests (user ownership)
   - ✅ Correct separation

4. **Authentication**
   - API: Unified auth (JWT/API key for APIs)
   - Holster: User auth (SEA for user space)
   - ✅ Correct separation

---

## 🎯 Recommendations

### 1. Keep Your Current Model ✅
Your architecture is **sound**! The separation makes sense:
- API for operations requiring server authority
- Holster for user-owned, synced data

### 2. Document the Boundary
Create clear guidelines for new features:
- "Does this need server authority?" → API
- "Is this user-owned data?" → Holster

### 3. Consider Adding:

#### Client-Side Holster Library
```typescript
// Shared logic for clients
class UserDataStore {
  async saveQuest(quest) {
    return this.user.get("quests").put(quest, true)
  }
  
  onQuestsChange(callback) {
    return this.user.get("quests").on(callback)
  }
}
```

#### API Middleware for Holster Writes
```typescript
// Helper for API routes that write to Holster
export async function writeToUserSpace(pub: string, key: string, data: any) {
  return holsterNextPut(["users", pub, key], data)
}
```

---

## 📖 Mental Model

Think of it this way:

**Holster = Your Database (with superpowers)**
- Realtime
- Offline-first  
- Cryptographically secured
- P2P capable

**API Routes = Your Business Logic Server**
- Validation
- External integrations
- Administrative operations
- Expensive computations

**Together:**
```
Client Request
     ↓
Is it user data read? → Holster (direct)
     ↓
Needs validation? → API (then write to Holster)
     ↓
External service? → API (then optionally store result in Holster)
     ↓
Admin operation? → API (with auth, then write to Holster if needed)
```

---

## 🎬 Conclusion

Your architecture uses **both layers correctly**:

✅ **Holster** handles user-owned, realtime, syncable data  
✅ **API** handles server authority, validation, external services  
✅ **Hybrid** patterns use API for orchestration, Holster for state

This gives you:
- **Scalability** - Clients sync P2P, server handles authority
- **Offline** - User data works without connection
- **Realtime** - Updates pushed instantly via WebSocket
- **Security** - Cryptographic ownership + server validation
- **Flexibility** - Best tool for each job

**Keep this model!** It's sophisticated and well-designed for a modern, distributed application.

---

## 📚 Further Reading

- [Holster Wiki](../holster.wiki/) - Complete Holster API docs
- [ARCHITECTURE.md](../src/lib/server/ARCHITECTURE.md) - Server refactoring guide
- [GunDB Documentation](https://gun.eco/docs/) - Original Gun concepts
- [Local-First Software](https://www.inkandswitch.com/local-first/) - Architecture philosophy

