# RSStream SvelteKit File Structure

## Complete File Tree

```
src/
├── hooks.server.ts                              # Server initialization
├── lib/
│   └── server/
│       ├── schemas/
│       │   └── rsstream.ts                      # Zod schemas & types
│       └── rsstream/
│           ├── auth.ts                          # Authentication middleware
│           ├── feed-items.ts                    # Feed item processing
│           ├── holster.ts                       # Holster initialization
│           ├── invite-codes.ts                  # Invite code management
│           ├── monitoring.ts                    # Performance monitoring
│           └── utils.ts                         # Utilities (email, hashing, etc.)
└── routes/
    └── api/
        ├── health/
        │   └── +server.ts                       # GET /api/health
        ├── host-public-key/
        │   └── +server.ts                       # GET /api/host-public-key
        ├── check-codes/
        │   └── +server.ts                       # POST /api/check-codes
        ├── check-invite-code/
        │   └── +server.ts                       # POST /api/check-invite-code
        ├── request-invite-code/
        │   └── +server.ts                       # POST /api/request-invite-code
        ├── claim-invite-code/
        │   └── +server.ts                       # POST /api/claim-invite-code
        ├── validate-email/
        │   └── +server.ts                       # POST /api/validate-email
        ├── reset-password/
        │   └── +server.ts                       # POST /api/reset-password
        ├── update-password/
        │   └── +server.ts                       # POST /api/update-password
        ├── add-feed/
        │   └── +server.ts                       # POST /api/add-feed
        ├── add-subscriber/
        │   └── +server.ts                       # POST /api/add-subscriber
        ├── remove-subscriber/
        │   └── +server.ts                       # POST /api/remove-subscriber
        └── private/
            ├── create-invite-codes/
            │   └── +server.ts                   # POST /api/private/create-invite-codes
            ├── send-invite-code/
            │   └── +server.ts                   # POST /api/private/send-invite-code
            ├── update-feed-limit/
            │   └── +server.ts                   # POST /api/private/update-feed-limit
            ├── performance/
            │   └── +server.ts                   # GET /api/private/performance
            ├── remove-feed/
            │   └── +server.ts                   # POST /api/private/remove-feed
            └── add-item/
                └── +server.ts                   # POST /api/private/add-item
```

## File Responsibilities

### Core Infrastructure

#### `src/hooks.server.ts`
- Initializes Holster on server startup
- Authenticates with configured credentials
- Starts monitoring intervals
- Runs once at server initialization

#### `src/lib/server/schemas/rsstream.ts` (244 lines)
- **Request Schemas**: Zod schemas for all API endpoints
- **Response Schemas**: Type-safe response structures
- **Internal Schemas**: Account, Feed, FeedItem, InviteCode data structures
- **Type Exports**: TypeScript types inferred from schemas
- **Purpose**: Single source of truth for all data validation

#### `src/lib/server/rsstream/holster.ts` (181 lines)
- **Holster Instance**: Initialized with secure mode and memory limit
- **Environment Config**: Username, password, host from env vars
- **In-Memory Caches**:
  - `inviteCodes`: Map of codes to invite data
  - `removeDays`: Set of processed cleanup days
  - `cleanupQueue`: Batched cleanup operations
  - `pendingRequests`: Request deduplication
  - `contentHashCache`: 2-week TTL content cache
- **Statistics**: Processing, request, and database metrics
- **Helper Functions**:
  - `timeDbOperation()`: Monitors DB operation performance
  - `day()`: Converts timestamp to day key
  - `mapInviteCodes()`: Syncs invite codes to memory
  - `initializeHolster()`: Auth and initialization
  - `getAccount()`: Retrieve account by code
  - Update/reset statistics functions

#### `src/lib/server/rsstream/auth.ts` (20 lines)
- `requireBasicAuth()`: Validates Basic Auth header
- `checkAuth()`: Middleware for private routes
- Returns 401 Response if unauthorized

### Utility Modules

#### `src/lib/server/rsstream/utils.ts` (168 lines)
- **Code Generation**: `newCode()` - 8-character random codes
- **Content Hashing**: MD5 hash for duplicate detection
- **Data Mapping**:
  - `mapEnclosure()`: Photo/audio/video enclosures
  - `mapCategory()`: Feed item categories
- **Email Functions**:
  - `mail()`: Nodemailer wrapper
  - `requestInvite()`: Invite request emails
  - `sendInviteCode()`: Send invite codes
  - `validateEmail()`: Email validation links
  - `resetPassword()`: Password reset emails

#### `src/lib/server/rsstream/invite-codes.ts` (78 lines)
- `checkCodes()`: Verify local code uniqueness
- `checkHosts()`: Federated host code checking
- `createInviteCodes()`: Generate and store new codes
- Handles encryption/decryption of invite data
- Supports federated architecture

#### `src/lib/server/rsstream/feed-items.ts` (141 lines)
- `processItem()`: Save feed items with parallel DB ops
- `scheduleCleanup()`: Batch cleanup scheduler with debouncing
- `processCleanupForDay()`: Remove items older than 2 weeks
- Implements batch processing (50 items per batch)
- Updates processing statistics

#### `src/lib/server/rsstream/monitoring.ts` (56 lines)
- `startCacheCleanup()`: Every 60s, clean expired caches
- `startPerformanceMonitoring()`: Every 60s, log system stats
- `initializeMonitoring()`: Start all monitoring tasks
- Tracks: memory, requests, DB ops, caches, uptime

### API Routes (Public)

#### `src/routes/api/health/+server.ts` (16 lines)
**GET** - No auth required
- Returns: status, uptime, memory, requests, timestamp
- Used for health checks and basic monitoring

#### `src/routes/api/host-public-key/+server.ts` (11 lines)
**GET** - No auth required
- Returns: host's public key from Holster
- Required by browser clients to locate data

#### `src/routes/api/check-codes/+server.ts` (24 lines)
**POST** - No auth required
- Body: `{codes: string[]}`
- Validates code uniqueness across accounts
- Returns 400 if duplicate found

#### `src/routes/api/check-invite-code/+server.ts` (34 lines)
**POST** - No auth required
- Body: `{code?: string}` (defaults to "admin")
- Returns 200 if available, 404 if not found, 400 if already used

#### `src/routes/api/request-invite-code/+server.ts` (16 lines)
**POST** - No auth required
- Body: `{email: string}`
- Sends waitlist confirmation email
- Returns: "Invite code requested"

#### `src/routes/api/claim-invite-code/+server.ts` (114 lines)
**POST** - No auth required
- Body: `{code?, pub, epub, username, email}`
- Creates new account with encrypted email
- Maps code to public key
- Sends validation email
- Removes code from available pool
- Updates shared codes for invite owner

#### `src/routes/api/validate-email/+server.ts` (53 lines)
**POST** - No auth required
- Body: `{code: string, validate: string}`
- Verifies validation code matches
- Removes validation requirement from account
- Returns: "Email validated"

#### `src/routes/api/reset-password/+server.ts` (73 lines)
**POST** - No auth required
- Body: `{code: string, email: string}`
- Validates email matches account
- Generates reset code with 24h expiry
- Tracks reset count (max 9 resets)
- Sends reset email

#### `src/routes/api/update-password/+server.ts` (103 lines)
**POST** - No auth required
- Body: `{code, reset, pub, epub, username, name}`
- Verifies reset code and expiry
- Updates account with new keys
- Re-encrypts shared invite codes with new keys
- Maps new public key to code
- Returns: old public key

### API Routes (Feed Management)

#### `src/routes/api/add-feed/+server.ts` (118 lines)
**POST** - No auth required
- Body: `{code: string, url: string}` (signed URL)
- Verifies URL signature
- Checks feed limit
- Calls external RSS service to add feed
- Stores feed metadata
- Increments account's subscribed count
- Returns: feed data from RSS service

#### `src/routes/api/add-subscriber/+server.ts` (75 lines)
**POST** - No auth required
- Body: `{code: string, url: string}` (signed URL)
- Verifies URL signature
- Checks feed exists
- Increments feed's subscriber_count
- Increments account's subscribed count

#### `src/routes/api/remove-subscriber/+server.ts` (100 lines)
**POST** - No auth required
- Body: `{code: string, url: string}` (signed URL)
- Verifies URL signature
- Decrements subscriber_count
- If last subscriber, removes feed from RSS service
- Decrements account's subscribed count

### API Routes (Private/Admin)

All private routes require Basic Authentication.

#### `src/routes/api/private/create-invite-codes/+server.ts` (45 lines)
**POST** - Requires Basic Auth
- Body: `{code: string, count?: number}`
- Validates account exists and email validated
- Creates specified number of invite codes
- Encrypts and stores codes
- Returns 500 if creation fails

#### `src/routes/api/private/send-invite-code/+server.ts` (22 lines)
**POST** - Requires Basic Auth
- Body: `{code: string, email: string}`
- Sends invite code email to specified address
- No validation (admin tool)

#### `src/routes/api/private/update-feed-limit/+server.ts` (49 lines)
**POST** - Requires Basic Auth
- Body: `{code: string, limit: number}`
- Updates account's feed subscription limit
- Requires validated email

#### `src/routes/api/private/performance/+server.ts` (70 lines)
**GET** - Requires Basic Auth
- Returns detailed performance statistics:
  - Memory usage (RSS, heap, external)
  - Request stats (total, slow, avg time)
  - Database stats (ops, errors, slow queries)
  - Processing stats (queue, delays)
  - Cache sizes
  - Process info (PID, version, platform)

#### `src/routes/api/private/remove-feed/+server.ts` (47 lines)
**POST** - Requires Basic Auth
- Body: `{url: string}`
- Clears feed metadata (title, description, etc.)
- Keeps subscriber_count for cleanup purposes
- Admin tool for removing problematic feeds

#### `src/routes/api/private/add-item/+server.ts` (128 lines)
**POST** - Requires Basic Auth (called by RSS fetcher)
- Body: `{url, guid, title?, content?, author?, permalink?, timestamp, enclosure?, category?}`
- **Request Deduplication**: Prevents duplicate processing
- **Content Hash Caching**: Skips unchanged items (2-week TTL)
- **Adaptive Throttling**: Delays based on system load
- **Age Filtering**: Ignores items >2 weeks old
- **Async Processing**: Direct processing with statistics tracking
- **Cleanup Scheduling**: Triggers removal of old items
- Returns: `{status: "success" | "unchanged" | "processing"}`

## Key Design Patterns

### 1. Request Validation
All routes use Zod schemas for type-safe request validation:
```typescript
const result = schema.safeParse(body)
if (!result.success) {
  error(400, result.error.message)
}
```

### 2. Error Handling
Consistent error responses using SvelteKit's `error()`:
```typescript
if (!condition) {
  error(statusCode, "Error message")
}
```

### 3. Authentication
Private routes use middleware pattern:
```typescript
const authError = checkAuth(event)
if (authError) return authError
```

### 4. Promise-based DB Operations
Holster callbacks wrapped in Promises:
```typescript
await new Promise(res => {
  user.get("path").put(data, res)
})
```

### 5. Type Safety
All operations use TypeScript with proper type annotations and Zod inference:
```typescript
type AccountData = z.infer<typeof accountDataSchema>
```

## Performance Optimizations

1. **Request Deduplication**: Prevents concurrent processing of same item
2. **Content Hash Caching**: 2-week TTL avoids reprocessing unchanged content
3. **Batch Cleanup**: Processes 50 items per batch with 10ms delays
4. **Debounced Cleanup**: 1-second debounce batches cleanup operations
5. **Adaptive Throttling**: Dynamic delays based on system load
6. **Parallel DB Operations**: Promise.allSettled for concurrent saves
7. **Memory Management**: Limits cache sizes (30 days of cleanup tracking)
8. **Statistics Tracking**: Identifies slow requests and DB operations

## Security Features

1. **Basic Authentication**: Password-protected admin endpoints
2. **Signed URLs**: Feed operations require signature verification
3. **Email Validation**: Required before sensitive operations
4. **Reset Limits**: Maximum 9 password resets per account
5. **Expiring Reset Codes**: 24-hour validity for password resets
6. **Encrypted Storage**: Emails and codes encrypted with Holster SEA
7. **Server-Only Modules**: All logic in `$lib/server` directory

