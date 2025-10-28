# 🚀 Data Relay System - Quick Start

Get started with the Generic Data Relay System in 5 minutes!

## 1. Enable Relay Types

Edit your `.env` file:

```env
# Enable specific types
ENABLED_RELAYS=rss-feed,twitter,webhook,iot-sensor

# Or enable all presets
ENABLED_RELAYS=all

# Or just RSS (default)
ENABLED_RELAYS=rss-feed
```

## 2. Start the Server

```bash
npm run dev
```

The relay system auto-initializes on startup!

## 3. Send Data

### RSS Feed (Original Use Case)

```bash
curl -X POST http://localhost:5173/api/relay/rss-feed \
  -H "Content-Type: application/json" \
  -H "Authorization: Basic $(echo -n 'host:password' | base64)" \
  -d '{
    "url": "https://example.com/feed",
    "guid": "article-123",
    "title": "My Article",
    "content": "Article content here...",
    "author": "John Doe",
    "timestamp": 1699123456789
  }'
```

### Twitter Feed (New!)

```bash
curl -X POST http://localhost:5173/api/relay/twitter \
  -H "Content-Type: application/json" \
  -H "Authorization: Basic $(echo -n 'host:password' | base64)" \
  -d '{
    "tweetId": "123456",
    "userId": "alice",
    "username": "alice",
    "text": "Hello world!",
    "createdAt": 1699123456789
  }'
```

### Webhook Event (New!)

```bash
curl -X POST http://localhost:5173/api/relay/webhook \
  -H "Content-Type: application/json" \
  -H "Authorization: Basic $(echo -n 'host:password' | base64)" \
  -d '{
    "source": "github",
    "eventType": "push",
    "eventId": "evt_abc123",
    "timestamp": 1699123456789,
    "payload": {
      "repo": "my-repo",
      "branch": "main",
      "commits": 3
    }
  }'
```

### IoT Sensor (New!)

```bash
curl -X POST http://localhost:5173/api/relay/iot-sensor \
  -H "Content-Type: application/json" \
  -H "Authorization: Basic $(echo -n 'host:password' | base64)" \
  -d '{
    "deviceId": "sensor-001",
    "sensorType": "temperature",
    "value": 72.5,
    "unit": "F",
    "timestamp": 1699123456789,
    "location": {
      "lat": 37.7749,
      "lon": -122.4194
    }
  }'
```

## 4. View the Dashboard

Open your browser:

```
http://localhost:5173/relay-dashboard
```

**Login:** Use your basic auth credentials (host:password by default)

You'll see:
- 📊 System overview with totals
- 🎯 All active relay types
- 📈 Real-time statistics
- 🔍 Recent items from each type

## 5. Query Data via API

### Get All Stats

```bash
curl http://localhost:5173/api/relay/stats \
  -H "Authorization: Basic $(echo -n 'host:password' | base64)"
```

### Get Type-Specific Stats

```bash
curl http://localhost:5173/api/relay/twitter \
  -H "Authorization: Basic $(echo -n 'host:password' | base64)"
```

### Query Stored Data

```bash
# Get recent tweets
curl "http://localhost:5173/api/relay/twitter/query?limit=20" \
  -H "Authorization: Basic $(echo -n 'host:password' | base64)"

# Get specific user's tweets
curl "http://localhost:5173/api/relay/twitter/query?resourceId=alice&limit=10" \
  -H "Authorization: Basic $(echo -n 'host:password' | base64)"
```

## 6. Create Custom Data Type

Create a new config file:

```typescript
// src/lib/server/data-relay/presets/my-custom.ts
import {z} from "zod"
import type {DataRelayConfig} from "../config"

const myDataSchema = z.object({
  id: z.string(),
  name: z.string(),
  value: z.number(),
  timestamp: z.number(),
})

export const myCustomConfig: DataRelayConfig = {
  type: "my-custom",
  displayName: "My Custom Data",
  inputSchema: myDataSchema,
  
  storage: {
    collection: "myData",
    getResourceId: data => data.name,
    getItemId: data => data.id,
    getTimestamp: data => data.timestamp,
    timeGrouping: "day",
    buildPath: (user, name, day, id) => 
      user.get("myData").next(name).next(day).next(id)
  },
  
  deduplication: {
    buildKey: data => `${data.name}_${data.id}`,
    hashFields: ["value"],
    cacheTTL: 3600000, // 1 hour
  },
  
  retention: {
    maxAge: 604800000, // 7 days
    enableCleanup: true,
    cleanupBatchSize: 50,
  },
  
  transform: {
    toStorage: data => ({...data, processedAt: Date.now()})
  },
}
```

Register it:

```typescript
// src/hooks.server.ts
import {myCustomConfig} from "$lib/server/data-relay/presets/my-custom"

registry.register(myCustomConfig)
```

Send data:

```bash
curl -X POST http://localhost:5173/api/relay/my-custom \
  -H "Content-Type: application/json" \
  -H "Authorization: Basic $(echo -n 'host:password' | base64)" \
  -d '{
    "id": "item-1",
    "name": "sensor-a",
    "value": 42,
    "timestamp": 1699123456789
  }'
```

**Done!** Your custom type now appears in the dashboard automatically! ✨

## 🎯 Common Patterns

### Pattern 1: Time-Series Data (IoT, Metrics)

```typescript
storage: {
  timeGrouping: "hour", // Group by hour for efficiency
},
retention: {
  maxAge: 2592000000, // 30 days
  enableCleanup: true,
}
```

### Pattern 2: Social Media (Twitter, Mastodon)

```typescript
storage: {
  timeGrouping: "day", // Group by day
  getResourceId: data => data.username, // Group by user
},
deduplication: {
  hashFields: ["text", "likeCount"], // Detect content/stat changes
  cacheTTL: 3600000, // 1 hour (updates frequently)
}
```

### Pattern 3: Webhooks (GitHub, Stripe)

```typescript
storage: {
  timeGrouping: "hour", // High volume
  getResourceId: data => data.source, // Group by source
},
retention: {
  maxAge: 604800000, // 7 days (ephemeral)
}
```

### Pattern 4: Documents (Permanent Storage)

```typescript
storage: {
  timeGrouping: "none", // No time grouping
},
retention: {
  maxAge: null, // Keep forever
  enableCleanup: false,
}
```

## 🔧 Troubleshooting

### No Data Showing in Dashboard

1. Check relay is enabled in `.env`:
   ```env
   ENABLED_RELAYS=rss-feed,twitter
   ```

2. Verify data was sent successfully:
   ```bash
   # Should return 200 OK
   curl -v http://localhost:5173/api/relay/twitter ...
   ```

3. Check server logs for errors

### Authentication Failing

Verify credentials:
```env
HOLSTER_USER_NAME=host
HOLSTER_USER_PASSWORD=password
```

Encode properly:
```bash
echo -n 'host:password' | base64
# Use this in: Authorization: Basic <base64>
```

### Data Not Persisting

1. Check Holster is initialized:
   - Look for "host logged in" in server logs

2. Verify storage path in config:
   ```typescript
   buildPath: (user, resource, time, id) => 
     user.get("collection").next(resource)...
   ```

3. Check Holster database files are writable

## 💡 Tips

1. **Start simple** - Begin with RSS or a single custom type
2. **Check the dashboard** - Visual feedback is instant
3. **Use the query endpoint** - Test data storage directly
4. **Monitor stats** - Watch processing times and cache hits
5. **Adjust configs** - Tune retention and caching per type

## 🎉 You're Ready!

The system is fully operational. Start relaying data! 🚀

**Questions?** Check the documentation files or inspect the source code - it's well-commented!

