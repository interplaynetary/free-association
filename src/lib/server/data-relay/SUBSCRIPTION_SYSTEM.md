# Data Relay Subscription System

## Overview

The Data Relay Subscription System provides optional subscription management for relay types that require users to explicitly subscribe to resources before receiving data. This creates a unified, decentralized approach to managing subscriptions across different data types (RSS feeds, IoT sensors, webhooks, etc.).

## Key Features

- **Optional**: Only relay types that need subscriptions enable it
- **Generic**: Works with any relay type (RSS, IoT, webhooks, social media, etc.)
- **Enforces Limits**: Account-based subscription limits with flexible configuration
- **External Service Integration**: Coordinate with external APIs during subscription lifecycle
- **Secure**: Built-in signed URL verification and authentication
- **Comprehensive**: Handles subscription, unsubscription, validation, and statistics

## Architecture

### Components

```
┌─────────────────────────────────────────────────────────────┐
│                     Data Relay Registry                      │
│  - Manages multiple relay engines                           │
│  - Provides unified subscription API                         │
└─────────────────────────┬───────────────────────────────────┘
                          │
         ┌────────────────┴────────────────┐
         │                                  │
┌────────▼─────────┐            ┌──────────▼──────────┐
│  Data Relay      │            │  Subscription       │
│  Engine          │◄───────────│  Manager            │
│  - Item ingestion│            │  - Lifecycle mgmt   │
│  - Validation    │            │  - Limit checking   │
└──────────────────┘            └─────────────────────┘
```

### Configuration Structure

```typescript
export interface SubscriptionConfig {
  required: boolean                    // Block items without subscription?
  subscriptionSchema: z.ZodSchema      // Validate subscription requests
  resourceCollection: string           // Where to store metadata
  getResourceId: (data) => string      // Extract resource identifier
  
  // Optional hooks
  verifyResourceId?: (id, account) => Promise<string | null>
  fetchResourceMetadata?: (id) => Promise<ResourceMetadata | null>
  
  // External service integration
  externalService?: {
    subscribe?: (id) => Promise<SubscriptionResult>
    unsubscribe?: (id) => Promise<SubscriptionResult>
  }
  
  // Limit enforcement
  limits: {
    maxPerAccount: number | null
    accountCountField: string
    accountLimitField: string
    checkLimit?: (account, id) => Promise<LimitCheckResult>
  }
  
  // Lifecycle handlers
  lifecycle: {
    onSubscribe: (user, id, context, metadata?) => Promise<SubscriptionResult>
    onUnsubscribe: (user, id, context) => Promise<SubscriptionResult>
    validateSubscription?: (user, id, accountCode?) => Promise<boolean>
  }
}
```

## Usage

### 1. Enable Subscriptions for a Relay Type

#### Simple Configuration (Using Helpers)

```typescript
import {buildSimpleSubscriptionConfig} from "$lib/server/data-relay"

export const myRelayConfig: DataRelayConfig = {
  type: "my-relay",
  // ... other config ...
  
  subscription: buildSimpleSubscriptionConfig("my-relay", "myResources", {
    required: true,
    countField: "subscribed",
    limitField: "maxSubscriptions",
  }),
}
```

#### Custom Configuration

```typescript
export const myRelayConfig: DataRelayConfig = {
  type: "my-relay",
  // ... other config ...
  
  subscription: {
    required: true,
    subscriptionSchema: z.object({
      code: z.string(),
      resourceId: z.string(),
    }),
    resourceCollection: "myResources",
    getResourceId: (data) => data.resourceId,
    
    limits: {
      maxPerAccount: 10,
      accountCountField: "subscribed",
      accountLimitField: "maxSubscriptions",
    },
    
    lifecycle: {
      onSubscribe: async (user, resourceId, context) => {
        // Custom subscription logic
        return {success: true}
      },
      onUnsubscribe: async (user, resourceId, context) => {
        // Custom unsubscription logic
        return {success: true}
      },
    },
  },
}
```

### 2. API Endpoints

The system provides generic subscription endpoints:

#### Subscribe to a Resource

```http
POST /api/relay/{type}/subscribe
Content-Type: application/json

{
  "code": "account-code",
  "url": "resource-url-or-id"
}
```

#### Unsubscribe from a Resource

```http
POST /api/relay/{type}/unsubscribe
Content-Type: application/json

{
  "code": "account-code",
  "url": "resource-url-or-id"
}
```

#### Get Subscription Info

```http
GET /api/relay/{type}/subscriptions
```

### 3. Item Ingestion with Subscription Validation

When a relay type has `required: true`, items will be rejected if no active subscription exists:

```typescript
// Item processing automatically validates subscription
const result = await registry.process("my-relay", itemData, accountCode)

if (!result.success && result.status === "no_subscription") {
  console.error("No active subscription for this resource")
}
```

## Examples

### RSS Feed Subscriptions

```typescript
subscription: buildSimpleSubscriptionConfig("rss-feed", "feeds", {
  required: true,
  countField: "subscribed",
  limitField: "feeds",
  
  // Verify signed URLs
  verifyResourceId: async (resourceId, account) => {
    return await meshVerify(resourceId, account)
  },
  
  // Fetch feed metadata from external service
  fetchMetadata: async (feedUrl) => {
    const response = await fetch(rssServiceUrl, {
      method: "POST",
      body: `action=add-feed&xmlUrl=${feedUrl}`,
    })
    const data = await response.json()
    return {
      title: data.title,
      description: data.description,
    }
  },
  
  // Call external RSS aggregation service
  externalService: {
    subscribe: async (feedUrl) => {
      await fetch(rssServiceUrl, {
        method: "POST",
        body: `action=add-feed&xmlUrl=${feedUrl}`,
      })
      return {success: true}
    },
    unsubscribe: async (feedUrl) => {
      await fetch(rssServiceUrl, {
        method: "POST",
        body: `action=remove-feed&xmlUrl=${feedUrl}`,
      })
      return {success: true}
    },
  },
}),
```

### IoT Sensor Subscriptions

```typescript
subscription: buildSimpleSubscriptionConfig("iot-sensor", "sensors", {
  required: true,
  countField: "activeSensors",
  limitField: "maxSensors",
  
  fetchMetadata: async (sensorId) => {
    // Query sensor registry
    const sensor = await getSensorInfo(sensorId)
    return {
      name: sensor.name,
      type: sensor.type,
      location: sensor.location,
    }
  },
}),
```

### Webhook Endpoints

```typescript
subscription: {
  required: false, // Optional - allow any webhook without subscription
  subscriptionSchema: simpleSubscriptionSchema,
  resourceCollection: "webhooks",
  getResourceId: (data) => data.resourceId,
  
  limits: {
    maxPerAccount: null, // Unlimited
    accountCountField: "webhooks",
    accountLimitField: "maxWebhooks",
  },
  
  lifecycle: {
    onSubscribe: createStandardSubscribeHandler("webhook", "webhooks", "webhooks"),
    onUnsubscribe: createStandardUnsubscribeHandler("webhook", "webhooks", "webhooks"),
  },
}
```

## Helper Functions

### Configuration Builders

- `buildSimpleSubscriptionConfig()` - Create standard subscription config
- `createFieldBasedLimitChecker()` - Simple field-based limits
- `createStandardSubscribeHandler()` - Standard subscription logic
- `createStandardUnsubscribeHandler()` - Standard unsubscription logic
- `createStandardValidationHandler()` - Standard validation logic

### Data Operations

- `trackAccountSubscription()` - Track subscription in accountSubscriptions
- `isAccountSubscribed()` - Check if account is subscribed
- `updateSubscriberCount()` - Update resource subscriber count
- `updateAccountSubscriptionCount()` - Update account subscription count
- `getOrCreateResource()` - Get or initialize resource metadata

## Data Structure

### Resource Metadata

Stored in the configured `resourceCollection` (e.g., "feeds", "sensors"):

```javascript
{
  "resource-id": {
    // Custom fields from fetchMetadata
    title: "Resource Title",
    description: "...",
    
    // Standard field
    subscriber_count: 5
  }
}
```

### Account Subscriptions

Tracked in `accountSubscriptions` collection:

```javascript
{
  "account-code": {
    "relay-type": {
      "resource-id-1": true,
      "resource-id-2": true
    }
  }
}
```

### Account Data

Updated fields:

```javascript
{
  "account-code": {
    subscribed: 3,        // Current subscription count
    feeds: 10,            // Subscription limit
    // ... other fields
  }
}
```

## Migration from Legacy Routes

### Before (RSS-specific routes)

```
POST /api/add-feed          → Subscribe to feed
POST /api/add-subscriber    → Add subscriber
POST /api/remove-subscriber → Remove subscriber
```

### After (Generic relay subscriptions)

```
POST /api/relay/rss-feed/subscribe    → Subscribe to feed
POST /api/relay/rss-feed/unsubscribe  → Unsubscribe from feed
GET  /api/relay/rss-feed/subscriptions → Get subscription info
```

## Benefits

1. **Unified API**: Same subscription endpoints for all relay types
2. **Decentralized**: Users control what data they receive
3. **Flexible Limits**: Per-account limits with custom validation
4. **Secure**: Built-in verification and authentication
5. **Extensible**: Easy to add subscription support to new relay types
6. **External Integration**: Coordinate with external services seamlessly
7. **Statistics**: Built-in subscription tracking and metrics

## Future Enhancements

- Subscription expiration and renewal
- Subscription tiers (basic, premium, etc.)
- Batch subscription operations
- Subscription transfer between accounts
- Webhook notifications for subscription events
- Subscription activity logs

