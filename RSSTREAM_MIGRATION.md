# RSStream Server Migration to SvelteKit

This document outlines the migration of the Express.js rsstream server (`app.js`) to SvelteKit routes with TypeScript.

## Structure Overview

### Core Utilities

#### `/src/lib/server/schemas/rsstream.ts`
- Zod schemas for request/response validation
- Type definitions for all API endpoints
- Internal data structure schemas

#### `/src/lib/server/rsstream/holster.ts`
- Holster initialization and management
- In-memory caches (inviteCodes, removeDays, cleanupQueue, etc.)
- Database operation helpers
- Processing and request statistics

#### `/src/lib/server/rsstream/auth.ts`
- Basic authentication middleware for private routes
- Authentication check utilities

#### `/src/lib/server/rsstream/utils.ts`
- Code generation (`newCode()`)
- Content hashing (`createContentHash()`)
- Data mapping (`mapEnclosure()`, `mapCategory()`)
- Email utilities (nodemailer integration)

#### `/src/lib/server/rsstream/invite-codes.ts`
- Invite code management
- Code uniqueness checking (local and federated)
- Invite code creation logic

#### `/src/lib/server/rsstream/feed-items.ts`
- Feed item processing
- Cleanup scheduling and batching
- Item storage and removal logic

#### `/src/lib/server/rsstream/monitoring.ts`
- Cache cleanup intervals
- Performance monitoring
- Memory and request statistics logging

#### `/src/hooks.server.ts`
- Server initialization
- Holster authentication on startup
- Monitoring initialization

## API Routes

### Public Routes

#### `/api/health` (GET)
- Health check endpoint
- Returns uptime, memory, and request stats

#### `/api/host-public-key` (GET)
- Returns the host's public key

#### `/api/request-invite-code` (POST)
- Request an invite code via email
- Body: `{email: string}`

#### `/api/check-codes` (POST)
- Check if codes are available/unique
- Body: `{codes: string[]}`

#### `/api/check-invite-code` (POST)
- Check if a specific invite code exists
- Body: `{code?: string}`

#### `/api/claim-invite-code` (POST)
- Claim an invite code and create account
- Body: `{code?, pub, epub, username, email}`

#### `/api/validate-email` (POST)
- Validate email with validation code
- Body: `{code, validate}`

#### `/api/reset-password` (POST)
- Request password reset
- Body: `{code, email}`

#### `/api/update-password` (POST)
- Update password with reset code
- Body: `{code, reset, pub, epub, username, name}`

### Feed Management Routes

#### `/api/add-feed` (POST)
- Add a new feed subscription
- Body: `{code, url}` (url must be signed)

#### `/api/add-subscriber` (POST)
- Add subscriber to existing feed
- Body: `{code, url}` (url must be signed)

#### `/api/remove-subscriber` (POST)
- Remove subscriber from feed
- Body: `{code, url}` (url must be signed)

### Private Admin Routes (Basic Auth Required)

#### `/api/private/create-invite-codes` (POST)
- Create new invite codes
- Body: `{code, count?}`

#### `/api/private/send-invite-code` (POST)
- Send invite code via email
- Body: `{code, email}`

#### `/api/private/update-feed-limit` (POST)
- Update user's feed limit
- Body: `{code, limit}`

#### `/api/private/performance` (GET)
- Detailed performance statistics
- Returns memory, request, DB, processing, and cache stats

#### `/api/private/remove-feed` (POST)
- Remove a feed from the system
- Body: `{url}`

#### `/api/private/add-item` (POST)
- Add a feed item (called by RSS fetcher)
- Body: `{url, guid, title?, content?, author?, permalink?, timestamp, enclosure?, category?}`
- Includes deduplication, content hashing, and throttling

## Key Features Preserved

1. **Request Deduplication**: Prevents duplicate processing of the same feed item
2. **Content Hash Caching**: 2-week TTL to avoid reprocessing unchanged content
3. **Aggressive Throttling**: Adaptive delays based on system load
4. **Batch Cleanup**: Scheduled cleanup of old feed items (>2 weeks)
5. **Performance Monitoring**: Comprehensive logging of request, DB, and processing stats
6. **Basic Authentication**: Password protection for admin routes
7. **Federated Code Checking**: Support for checking invite codes across federated hosts
8. **Email Integration**: nodemailer for sending invites, validations, and password resets

## Environment Variables Required

```env
# Holster Configuration
HOLSTER_USER_NAME=host
HOLSTER_USER_PASSWORD=password
APP_HOST=http://localhost:3000

# RSS Feed Integration
ADD_FEED_URL=https://your-rss-service.com/api
ADD_FEED_ID=your-id
ADD_FEED_API_KEY=your-api-key

# Email Configuration (optional)
MAIL_FROM=noreply@example.com
MAIL_BCC=admin@example.com

# Federation (optional)
FEDERATED_HOSTS=https://host1.com,https://host2.com
```

## Dependencies to Install

The following dependencies have been added to `package.json`:

```json
{
  "dependencies": {
    "@types/node": "^22.10.7",
    "nodemailer": "^6.9.16",
    "@types/nodemailer": "^6.4.17",
    "zod": "^3.25.0"
  }
}
```

To install all dependencies:
```bash
npm install
```

Note: Zod v3 is used as it's the latest stable version. The schemas are compatible with future versions.

## Migration Notes

1. **Static File Serving**: The original Express app served static files from `../browser/build`. In SvelteKit, this is handled automatically through the standard build process and static adapter.

2. **Redirects**: The original Express app had several redirect routes (`/invite`, `/register`, etc.). These can be handled by SvelteKit's routing or client-side navigation.

3. **Error Handling**: Using SvelteKit's `error()` helper for consistent error responses.

4. **Type Safety**: All routes now have proper TypeScript types and Zod validation.

5. **Async/Await**: Converted callback-based code to modern async/await patterns where possible.

6. **Request Handler Types**: Using SvelteKit's `RequestHandler` type from `./$types` for proper typing.

## Testing

After installation:

1. Install dependencies: `npm install`
2. Set environment variables in `.env`
3. Run development server: `npm run dev`
4. Test public endpoints: `curl http://localhost:5173/api/health`
5. Test private endpoints with basic auth: `curl -u username:password http://localhost:5173/api/private/performance`

## Future Improvements

1. **Rate Limiting**: Add rate limiting middleware for public endpoints
2. **Request Logging**: Integrate structured logging (e.g., pino)
3. **Database Abstraction**: Consider abstracting Holster operations for easier testing
4. **OpenAPI Documentation**: Generate API documentation from Zod schemas
5. **Unit Tests**: Add comprehensive tests for each route and utility function
6. **WebSocket Support**: Consider WebSocket endpoints for real-time feed updates

