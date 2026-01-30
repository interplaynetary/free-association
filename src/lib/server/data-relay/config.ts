import { z } from "zod"

// Type for Gun/Mesh user instance
type User = any

// ============================================================================
// Subscription Types
// ============================================================================

export interface SubscriptionContext {
  /** Account code for the subscribing user */
  accountCode: string
  /** Full account object */
  account: any
  /** Optional signed URL for verification */
  signedUrl?: string
  /** Optional additional context data */
  metadata?: Record<string, any>
}

export interface SubscriptionResult {
  success: boolean
  error?: string
  data?: any
}

export interface LimitCheckResult {
  allowed: boolean
  error?: string
  current?: number
  limit?: number
}

export interface ResourceMetadata {
  [key: string]: any
}

/**
 * Optional subscription/resource lifecycle configuration
 * Enables users to subscribe to specific resources before receiving data
 */
export interface SubscriptionConfig {
  /** Whether subscription is required before accepting items */
  required: boolean

  /** Zod schema for subscription requests (separate from item schema) */
  subscriptionSchema: z.ZodSchema

  /** Collection name for resource metadata (e.g., "feeds", "sensors", "endpoints") */
  resourceCollection: string

  /** Extract resource identifier from subscription data */
  getResourceId: (subscriptionData: any) => string

  /** Optional: Verify and transform resource identifier (e.g., signed URL verification) */
  verifyResourceId?: (resourceId: string, account: any) => Promise<string | null>

  /** Optional: Fetch initial metadata from external service */
  fetchResourceMetadata?: (resourceId: string) => Promise<ResourceMetadata | null>

  /** Optional: External service integration for subscription lifecycle */
  externalService?: {
    /** Called when subscribing to a resource */
    subscribe?: (resourceId: string) => Promise<SubscriptionResult>
    /** Called when unsubscribing from a resource */
    unsubscribe?: (resourceId: string) => Promise<SubscriptionResult>
  }

  /** Subscription limits configuration */
  limits: {
    /** Maximum subscriptions per account (null = unlimited) */
    maxPerAccount: number | null

    /** Field in account object tracking current subscription count */
    accountCountField: string

    /** Field in account object defining the limit */
    accountLimitField: string

    /** Optional: Custom limit validation function */
    checkLimit?: (account: any, resourceId: string) => Promise<LimitCheckResult>
  }

  /** Lifecycle hook implementations */
  lifecycle: {
    /**
     * Called when user subscribes to a resource
     * Should handle:
     * - Creating/updating resource metadata
     * - Incrementing subscriber counts
     * - Updating account subscription count
     */
    onSubscribe: (
      user: User,
      resourceId: string,
      context: SubscriptionContext,
      resourceMetadata?: ResourceMetadata,
    ) => Promise<SubscriptionResult>

    /**
     * Called when user unsubscribes from a resource
     * Should handle:
     * - Decrementing subscriber counts
     * - Updating account subscription count
     * - Cleanup if no subscribers remain
     */
    onUnsubscribe: (
      user: User,
      resourceId: string,
      context: SubscriptionContext,
    ) => Promise<SubscriptionResult>

    /**
     * Optional: Validate subscription exists before processing items
     * If not provided, subscription is not validated during item processing
     */
    validateSubscription?: (user: User, resourceId: string, accountCode?: string) => Promise<boolean>
  }
}

// ============================================================================
// Main Configuration Interface
// ============================================================================

/**
 * Generic data relay configuration for different data types
 */
export interface DataRelayConfig<TInput = any, TStored = any> {
  /** Unique identifier for this data type */
  type: string

  /** Display name for logging */
  displayName: string

  /** Zod schema for validating incoming data */
  inputSchema: z.ZodSchema<TInput>

  /** Storage configuration */
  storage: {
    /** Base collection name in Mesh (e.g., "feedItems", "tweets", "events") */
    collection: string

    /** Function to extract resource identifier (e.g., feed URL, user handle) */
    getResourceId: (data: TInput) => string

    /** Function to extract unique item identifier (e.g., GUID, tweet ID) */
    getItemId: (data: TInput) => string

    /** Function to extract timestamp (milliseconds) */
    getTimestamp: (data: TInput) => number

    /** Time grouping strategy */
    timeGrouping: "day" | "hour" | "week" | "month" | "none"

    /** Build the Mesh storage path */
    buildPath: (
      user: User,
      resourceId: string,
      timeKey: number | null,
      itemId: string,
    ) => any // Returns Gun chain
  }

  /** Deduplication configuration */
  deduplication: {
    /** Build deduplication key */
    buildKey: (data: TInput) => string

    /** Fields to include in content hash (for change detection) */
    hashFields: (keyof TStored)[]

    /** Content hash TTL in milliseconds */
    cacheTTL: number
  }

  /** Retention policy */
  retention: {
    /** Maximum age of items in milliseconds (null = no cleanup) */
    maxAge: number | null

    /** Whether to track items for cleanup */
    enableCleanup: boolean

    /** Batch size for cleanup operations */
    cleanupBatchSize: number
  }

  /** Data transformation */
  transform: {
    /** Transform incoming data to storage format */
    toStorage: (data: TInput) => TStored

    /** Optional: Transform stored data for retrieval */
    fromStorage?: (data: TStored) => any

    /** Optional: Custom validation after transformation */
    validate?: (data: TStored) => boolean
  }

  /** Optional: Metadata management (like feed subscriber counts) */
  metadata?: {
    /** Collection name for metadata */
    collection: string

    /** Extract metadata key from data */
    getKey: (data: TInput) => string

    /** Update metadata on item add */
    onItemAdd?: (
      user: User,
      key: string,
      currentMetadata: any,
    ) => Promise<any>

    /** Update metadata on item remove */
    onItemRemove?: (
      user: User,
      key: string,
      currentMetadata: any,
    ) => Promise<any>
  }

  /** Optional: Subscription/Resource lifecycle management */
  subscription?: SubscriptionConfig

  /** Optional: Custom throttling strategy */
  throttling?: {
    /** Base delay per pending request (ms) */
    delayPerRequest: number

    /** Maximum total delay (ms) */
    maxDelay: number

    /** Threshold for considering a request "slow" (ms) */
    slowRequestThreshold: number
  }

  /** Optional: Age filtering */
  ageFilter?: {
    /** Reject items older than this (ms) */
    maxItemAge: number

    /** Message to return when rejected */
    rejectionMessage: string
  }
}

/**
 * Default configurations for common data types
 */
export const DEFAULT_THROTTLING = {
  delayPerRequest: 200,
  maxDelay: 60000,
  slowRequestThreshold: 100,
}

export const DEFAULT_RETENTION = {
  maxAge: 1209600000, // 2 weeks
  enableCleanup: true,
  cleanupBatchSize: 50,
}

/**
 * Helper to compute time-based grouping keys
 */
export function computeTimeKey(
  timestamp: number,
  grouping: "day" | "hour" | "week" | "month" | "none",
): number | null {
  if (grouping === "none") return null

  const date = new Date(timestamp)

  switch (grouping) {
    case "day":
      return Date.UTC(date.getUTCFullYear(), date.getUTCMonth(), date.getUTCDate())

    case "hour":
      return Date.UTC(
        date.getUTCFullYear(),
        date.getUTCMonth(),
        date.getUTCDate(),
        date.getUTCHours(),
      )

    case "week": {
      const dayOfWeek = date.getUTCDay()
      const diff = date.getUTCDate() - dayOfWeek
      return Date.UTC(date.getUTCFullYear(), date.getUTCMonth(), diff)
    }

    case "month":
      return Date.UTC(date.getUTCFullYear(), date.getUTCMonth(), 1)

    default:
      return null
  }
}

/**
 * Helper to create content hash from object fields
 */
export function createContentHash(
  data: Record<string, any>,
  fields: string[],
): string {
  const crypto = require("crypto")
  const hashInput = fields
    .map(field => String(data[field] ?? ""))
    .join("|")
  return crypto.createHash("md5").update(hashInput).digest("hex")
}

/**
 * Default path builder for flat storage
 */
export function buildFlatPath(
  user: User,
  collection: string,
  resourceId: string,
  timeKey: number | null,
  itemId: string,
) {
  let chain = user.get(collection).next(resourceId)

  if (timeKey !== null) {
    chain = chain.next(timeKey)
  }

  return chain.next(itemId)
}

/**
 * Default path builder for user-scoped storage
 */
export function buildUserScopedPath(
  user: User,
  collection: string,
  userId: string,
  timeKey: number | null,
  itemId: string,
) {
  let chain = user.get(collection).next(userId)

  if (timeKey !== null) {
    chain = chain.next(timeKey)
  }

  return chain.next(itemId)
}

