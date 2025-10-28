# Generic Data Relay System for Holster

A flexible, type-safe system for relaying data from external APIs and sources to Holster distributed storage.

## Overview

The Data Relay System abstracts the RSS stream implementation into a generic framework that can handle **any type of data** from **any source**. It provides:

- ✅ **Configurable storage hierarchies** - Define how data is organized in Holster
- ✅ **Request deduplication** - Prevent duplicate processing
- ✅ **Content hash caching** - Skip reprocessing unchanged data
- ✅ **Adaptive throttling** - Dynamic rate limiting based on system load
- ✅ **Automatic cleanup** - Time-based retention policies
- ✅ **Type safety** - Full TypeScript + Zod validation
- ✅ **Pluggable transformers** - Custom data mapping and validation
- ✅ **Performance monitoring** - Built-in statistics and metrics

## Architecture

### Core Components

```
data-relay/
├── config.ts          # Configuration interface and helpers
├── engine.ts          # Generic relay engine implementation
├── registry.ts        # Multi-engine registry and management
├── index.ts           # Public API exports
└── presets/
    ├── rss-feed.ts    # RSS feed configuration
    ├── social-media.ts # Twitter, Mastodon, Reddit configs
    └── generic-apis.ts # Webhook, IoT, Email, Log configs
```

### Data Flow

```
External API/Source
      ↓
   Validate (Zod schema)
      ↓
  Deduplication Check
      ↓
  Content Hash Check (unchanged?)
      ↓  (if changed/new)
    Transform Data
      ↓
   Apply Throttling
      ↓
   Store to Holster
      ↓
  Update Metadata
      ↓
  Schedule Cleanup
```

## Usage

### Quick Start

```typescript
import {getRegistry} from "$lib/server/data-relay"
import {user} from "$lib/server/holster" // Your Holster user instance

// Get the registry
const registry = getRegistry(user)

// Register preset configurations
registry.registerPresets(["rss-feed", "twitter", "webhook"])

// Start automatic cache cleanup
registry.startCacheCleanup(60000) // Every 60 seconds

// Process data
const result = await registry.process("twitter", {
  tweetId: "123456789",
  userId: "alice",
  username: "alice",
  text: "Hello world!",
  createdAt: Date.now(),
})

if (result.success) {
  console.log(`Status: ${result.status}`) // "stored" | "unchanged"
} else {
  console.error(`Error: ${result.error}`)
}
```

### SvelteKit API Route Example

```typescript
// src/routes/api/relay/[type]/+server.ts
import {json, error} from "@sveltejs/kit"
import type {RequestHandler} from "./$types"
import {getRegistry} from "$lib/server/data-relay"
import {user} from "$lib/server/holster"

export const POST: RequestHandler = async ({params, request}) => {
  const {type} = params
  const data = await request.json()

  const registry = getRegistry(user)
  const result = await registry.process(type, data)

  if (!result.success) {
    error(400, result.error)
  }

  return json({status: result.status})
}
```

## Configuration

### Creating a Custom Data Relay

```typescript
import type {DataRelayConfig} from "$lib/server/data-relay"
import {z} from "zod"

// 1. Define your input schema
const myDataSchema = z.object({
  id: z.string(),
  source: z.string(),
  timestamp: z.number(),
  payload: z.record(z.any()),
})

// 2. Create configuration
export const myDataConfig: DataRelayConfig = {
  type: "my-data",
  displayName: "My Data Source",

  inputSchema: myDataSchema,

  storage: {
    collection: "myData",
    getResourceId: data => data.source,
    getItemId: data => data.id,
    getTimestamp: data => data.timestamp,
    timeGrouping: "day", // "hour" | "day" | "week" | "month" | "none"
    buildPath: (user, resourceId, timeKey, itemId) => {
      let chain = user.get("myData").next(resourceId)
      if (timeKey !== null) chain = chain.next(timeKey)
      return chain.next(itemId)
    },
  },

  deduplication: {
    buildKey: data => `${data.source}_${data.id}`,
    hashFields: ["payload"], // Fields to include in content hash
    cacheTTL: 3600000, // 1 hour
  },

  retention: {
    maxAge: 604800000, // 7 days (null = keep forever)
    enableCleanup: true,
    cleanupBatchSize: 50,
  },

  transform: {
    toStorage: data => ({
      id: data.id,
      source: data.source,
      timestamp: data.timestamp,
      payload: data.payload,
      processedAt: Date.now(),
    }),
    validate: data => {
      // Optional custom validation
      return data.payload !== null
    },
  },

  throttling: {
    delayPerRequest: 200, // ms per pending request
    maxDelay: 60000, // max total delay
    slowRequestThreshold: 100, // ms threshold for "slow"
  },

  ageFilter: {
    maxItemAge: 2592000000, // 30 days
    rejectionMessage: "Data too old",
  },
}
```

### Registering Custom Configurations

```typescript
import {DataRelayRegistry} from "$lib/server/data-relay"
import {user} from "$lib/server/holster"
import {myDataConfig} from "./my-data-config"

const registry = new DataRelayRegistry(user)

// Register custom config
registry.register(myDataConfig)

// Or register multiple
registry.register(config1)
registry.register(config2)

// Use it
await registry.process("my-data", dataPayload)
```

## Built-in Presets

### RSS Feeds (`rss-feed`)
- **Storage**: `feedItems/{url}/{day}/{guid}`
- **Retention**: 2 weeks
- **Grouping**: By day
- **Features**: Enclosure/category mapping, age filtering

### Twitter/X (`twitter`)
- **Storage**: `tweets/{username}/{day}/{tweetId}`
- **Retention**: 30 days
- **Grouping**: By day
- **Features**: Hashtag/mention mapping, engagement stats

### Mastodon (`mastodon`)
- **Storage**: `mastodonPosts/{acct}/{day}/{id}`
- **Retention**: 30 days
- **Grouping**: By day
- **Features**: Visibility filtering, media tracking

### Reddit (`reddit`)
- **Storage**: `redditPosts/{subreddit}/{day}/{id}`
- **Retention**: 7 days
- **Grouping**: By day
- **Features**: Score tracking, video detection

### Webhooks (`webhook`)
- **Storage**: `webhookEvents/{source}/{hour}/{eventId}`
- **Retention**: 7 days
- **Grouping**: By hour
- **Features**: High-volume event processing

### IoT Sensors (`iot-sensor`)
- **Storage**: `sensorData/{deviceId}_{sensorType}/{hour}/{timestamp}`
- **Retention**: 30 days
- **Grouping**: By hour
- **Features**: Location tracking, value validation

### Email Archive (`email`)
- **Storage**: `emails/{fromEmail}/{day}/{messageId}`
- **Retention**: Indefinite
- **Grouping**: By day
- **Features**: Label management, attachment tracking

### Log Aggregation (`logs`)
- **Storage**: `logs/{source}/{hour}/{logId}`
- **Retention**: 7 days
- **Grouping**: By hour
- **Features**: High-volume processing, level filtering

### JSON Documents (`json-document`)
- **Storage**: `documents/{collection}/{documentId}`
- **Retention**: Indefinite
- **Grouping**: None
- **Features**: Arbitrary JSON storage, tag system

## Advanced Features

### Metadata Management

Track aggregate statistics for resources:

```typescript
metadata: {
  collection: "feedMetadata",
  getKey: data => data.feedUrl,
  onItemAdd: async (user, feedUrl, current) => {
    return {
      url: feedUrl,
      itemCount: (current?.itemCount || 0) + 1,
      lastUpdate: Date.now(),
    }
  },
  onItemRemove: async (user, feedUrl, current) => {
    return {
      url: feedUrl,
      itemCount: Math.max(0, (current?.itemCount || 1) - 1),
    }
  },
}
```

### Custom Path Builders

Create complex storage hierarchies:

```typescript
buildPath: (user, resourceId, timeKey, itemId) => {
  // User-scoped with categories
  const [userId, category] = resourceId.split(":")
  let chain = user.get("items").next(userId).next(category)
  
  if (timeKey !== null) {
    chain = chain.next(timeKey)
  }
  
  return chain.next(itemId)
}
```

### Content Validation

Add custom validation after transformation:

```typescript
transform: {
  toStorage: data => processData(data),
  validate: data => {
    // Ensure required fields
    if (!data.title || !data.content) return false
    
    // Check content length
    if (data.content.length > 100000) return false
    
    return true
  },
}
```

## Performance & Monitoring

### Statistics

Get performance metrics for all or specific engines:

```typescript
const registry = getRegistry(user)

// All engines
const allStats = registry.getStats()

// Specific engine
const twitterStats = registry.getEngineStats("twitter")
console.log(twitterStats)
// {
//   processing: { averageProcessingTime, totalProcessed, ... },
//   requests: { totalRequests, slowRequests, duplicates, unchanged, ... },
//   caches: { pending, contentHash, cleanupQueue, ... }
// }
```

### Throttling Strategy

Adaptive throttling based on:
1. **Pending requests**: `size * delayPerRequest`
2. **System load**: Increases when processing is slow
3. **Combined**: `min(pendingDelay + systemDelay, maxDelay)`

```typescript
throttling: {
  delayPerRequest: 200,    // Base delay per request
  maxDelay: 60000,         // Cap at 60 seconds
  slowRequestThreshold: 100 // Consider >100ms "slow"
}
```

### Cache Management

Automatic cleanup of expired entries:

```typescript
registry.startCacheCleanup(60000) // Every 60s

// Manual cleanup
const engine = registry.get("twitter")
engine?.cleanupCaches()
```

## Migration from RSS Stream

The original RSS stream has been generalized:

| Original Concept | Generalized Concept |
|-----------------|---------------------|
| Feed URL | Resource ID |
| Item GUID | Item ID |
| Day grouping | Configurable time grouping |
| 2-week retention | Configurable retention |
| RSS-specific fields | Pluggable transformers |
| `feedItems/{url}/{day}/{guid}` | Configurable path builder |
| Feed subscriber counts | Generic metadata management |

### Converting Existing Routes

**Before** (RSS-specific):
```typescript
// src/routes/api/private/add-item/+server.ts
await processRSSItem(feedUrl, guid, itemData)
```

**After** (Generic):
```typescript
// src/routes/api/relay/[type]/+server.ts
await registry.process(type, itemData)
```

## Best Practices

### 1. Choose Appropriate Time Grouping

- **Hour**: High-volume data (sensors, logs, webhooks)
- **Day**: Medium-volume data (social media, RSS)
- **Week/Month**: Low-volume data (reports, summaries)
- **None**: Timeless documents

### 2. Set Realistic Retention Policies

- **Ephemeral data**: 7-30 days (tweets, logs)
- **Archival data**: Indefinite (emails, documents)
- **Sensor data**: Based on storage capacity

### 3. Configure Hash Fields Carefully

Include only fields that indicate **content changes**:
```typescript
// Good: Core content fields
hashFields: ["title", "content", "author"]

// Bad: Including metadata that changes frequently
hashFields: ["title", "content", "viewCount", "likeCount"]
```

### 4. Balance Deduplication TTL

- **Short TTL** (5-30 min): Frequently updated data
- **Long TTL** (hours-days): Static content
- **Zero TTL**: Always allow duplicates (logs)

### 5. Monitor Performance

```typescript
setInterval(() => {
  const stats = registry.getStats()
  
  for (const [type, engineStats] of Object.entries(stats)) {
    if (engineStats.processing.currentDelay > 5000) {
      console.warn(`High delay for ${type}: ${engineStats.processing.currentDelay}ms`)
    }
  }
}, 60000)
```

## Extending the System

### Adding New Presets

1. Create configuration in `presets/my-preset.ts`
2. Export from preset file
3. Import in `registry.ts`
4. Add to `allPresets` object
5. Document in this README

### Creating Data Source Adapters

For specific external APIs, create adapters that transform API responses to relay format:

```typescript
// adapters/github-adapter.ts
export async function fetchGitHubEvents(repo: string) {
  const events = await fetch(`https://api.github.com/repos/${repo}/events`)
  const json = await events.json()
  
  return json.map(event => ({
    source: "github",
    eventType: event.type,
    eventId: event.id,
    timestamp: new Date(event.created_at).getTime(),
    payload: event.payload,
  }))
}

// Then relay them
for (const event of events) {
  await registry.process("webhook", event)
}
```

## Examples

See the `examples/` directory for complete implementations:
- **RSS Aggregator**: Multi-feed RSS reader
- **Social Media Dashboard**: Twitter + Mastodon + Reddit
- **IoT Hub**: Multi-sensor data collection
- **Log Aggregator**: Multi-service log collection
- **Email Archive**: IMAP email backup

## License

This system is part of the free-association project.

