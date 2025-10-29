import {z} from "zod"
import type {User} from "@mblaney/holster/src/holster.js"

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
    /** Base collection name in Holster (e.g., "feedItems", "tweets", "events") */
    collection: string

    /** Function to extract resource identifier (e.g., feed URL, user handle) */
    getResourceId: (data: TInput) => string

    /** Function to extract unique item identifier (e.g., GUID, tweet ID) */
    getItemId: (data: TInput) => string

    /** Function to extract timestamp (milliseconds) */
    getTimestamp: (data: TInput) => number

    /** Time grouping strategy */
    timeGrouping: "day" | "hour" | "week" | "month" | "none"

    /** Build the Holster storage path */
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

