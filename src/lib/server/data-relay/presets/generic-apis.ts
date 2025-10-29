import {z} from "zod"
import type {DataRelayConfig} from "../config"
import {buildFlatPath, DEFAULT_RETENTION, DEFAULT_THROTTLING} from "../config"
import type {User} from "@mblaney/holster/src/holster.js"

// ============================================================================
// Webhook Events Configuration
// ============================================================================

const webhookEventInputSchema = z.object({
  source: z.string().min(1), // e.g., "github", "stripe", "slack"
  eventType: z.string().min(1), // e.g., "push", "payment_succeeded"
  eventId: z.string().min(1),
  timestamp: z.number(),
  payload: z.record(z.any()),
  metadata: z.record(z.string()).optional(),
})

export const webhookConfig: DataRelayConfig = {
  type: "webhook",
  displayName: "Webhook Events",

  inputSchema: webhookEventInputSchema,

  storage: {
    collection: "webhookEvents",
    getResourceId: data => data.source,
    getItemId: data => data.eventId,
    getTimestamp: data => data.timestamp,
    timeGrouping: "hour", // Webhooks might be high-volume
    buildPath: (user: User, source: string, timeKey: number | null, eventId: string) => {
      return buildFlatPath(user, "webhookEvents", source, timeKey, eventId)
    },
  },

  deduplication: {
    buildKey: data => `${data.source}_${data.eventId}`,
    hashFields: ["eventType", "timestamp"], // Just check for exact duplicate
    cacheTTL: 3600000, // 1 hour
  },

  retention: {
    maxAge: 604800000, // 7 days
    enableCleanup: true,
    cleanupBatchSize: 100,
  },

  transform: {
    toStorage: data => ({
      source: data.source,
      eventType: data.eventType,
      eventId: data.eventId,
      timestamp: data.timestamp,
      payload: data.payload,
      metadata: data.metadata || {},
      receivedAt: Date.now(),
    }),
  },

  throttling: {
    ...DEFAULT_THROTTLING,
    delayPerRequest: 50, // Fast processing for webhooks
    maxDelay: 10000,
  },
}

// ============================================================================
// IoT Sensor Data Configuration
// ============================================================================

const sensorDataInputSchema = z.object({
  deviceId: z.string().min(1),
  sensorType: z.string(), // e.g., "temperature", "humidity", "motion"
  value: z.union([z.number(), z.string(), z.boolean()]),
  unit: z.string().optional(),
  timestamp: z.number(),
  location: z
    .object({
      lat: z.number(),
      lon: z.number(),
      altitude: z.number().optional(),
    })
    .optional(),
  metadata: z.record(z.any()).optional(),
})

export const iotSensorConfig: DataRelayConfig = {
  type: "iot-sensor",
  displayName: "IoT Sensor Data",

  inputSchema: sensorDataInputSchema,

  storage: {
    collection: "sensorData",
    getResourceId: data => `${data.deviceId}_${data.sensorType}`,
    getItemId: data => data.timestamp.toString(),
    getTimestamp: data => data.timestamp,
    timeGrouping: "hour", // Sensor data by hour for efficient querying
    buildPath: (user: User, deviceSensor: string, timeKey: number | null, timestamp: string) => {
      return buildFlatPath(user, "sensorData", deviceSensor, timeKey, timestamp)
    },
  },

  deduplication: {
    buildKey: data => `${data.deviceId}_${data.sensorType}_${data.timestamp}`,
    hashFields: ["value"], // Only reprocess if value changed
    cacheTTL: 300000, // 5 minutes (sensors update frequently)
  },

  retention: {
    maxAge: 2592000000, // 30 days (configurable based on needs)
    enableCleanup: true,
    cleanupBatchSize: 200, // High volume data
  },

  transform: {
    toStorage: data => ({
      deviceId: data.deviceId,
      sensorType: data.sensorType,
      value: data.value,
      unit: data.unit || "",
      timestamp: data.timestamp,
      location: data.location,
      metadata: data.metadata || {},
    }),
    validate: data => {
      // Ensure numeric values are within reasonable ranges
      if (typeof data.value === "number") {
        return !isNaN(data.value) && isFinite(data.value)
      }
      return true
    },
  },

  metadata: {
    collection: "devices",
    getKey: data => data.deviceId,
    onItemAdd: async (user: User, deviceId: string, current: any) => {
      return {
        deviceId,
        lastReading: Date.now(),
        readingCount: (current?.readingCount || 0) + 1,
      }
    },
  },

  throttling: {
    delayPerRequest: 10, // Very fast for high-volume sensor data
    maxDelay: 5000,
    slowRequestThreshold: 50,
  },
}

// ============================================================================
// Email/Newsletter Archive Configuration
// ============================================================================

const emailInputSchema = z.object({
  messageId: z.string().min(1),
  from: z.object({
    email: z.string().email(),
    name: z.string().optional(),
  }),
  to: z.array(z.string().email()),
  subject: z.string(),
  body: z.object({
    text: z.string().optional(),
    html: z.string().optional(),
  }),
  receivedAt: z.number(),
  labels: z.array(z.string()).optional(),
  attachments: z
    .array(
      z.object({
        filename: z.string(),
        contentType: z.string(),
        size: z.number(),
      }),
    )
    .optional(),
})

export const emailArchiveConfig: DataRelayConfig = {
  type: "email",
  displayName: "Email Archive",

  inputSchema: emailInputSchema,

  storage: {
    collection: "emails",
    getResourceId: data => data.from.email,
    getItemId: data => data.messageId,
    getTimestamp: data => data.receivedAt,
    timeGrouping: "day",
    buildPath: (user: User, fromEmail: string, timeKey: number | null, messageId: string) => {
      return buildFlatPath(user, "emails", fromEmail, timeKey, messageId)
    },
  },

  deduplication: {
    buildKey: data => data.messageId,
    hashFields: ["subject", "receivedAt"],
    cacheTTL: 86400000, // 24 hours
  },

  retention: {
    maxAge: null, // Keep emails indefinitely
    enableCleanup: false,
    cleanupBatchSize: 50,
  },

  transform: {
    toStorage: data => ({
      messageId: data.messageId,
      from: data.from,
      to: data.to,
      subject: data.subject,
      body: data.body,
      receivedAt: data.receivedAt,
      labels: data.labels?.reduce((acc, label) => ({...acc, [label]: true}), {}) || {},
      hasAttachments: (data.attachments?.length || 0) > 0,
      attachmentCount: data.attachments?.length || 0,
    }),
  },

  throttling: DEFAULT_THROTTLING,
}

// ============================================================================
// Log Aggregation Configuration
// ============================================================================

const logEntryInputSchema = z.object({
  source: z.string(), // service name
  level: z.enum(["debug", "info", "warn", "error", "fatal"]),
  message: z.string(),
  timestamp: z.number(),
  context: z.record(z.any()).optional(),
  stackTrace: z.string().optional(),
  userId: z.string().optional(),
  requestId: z.string().optional(),
})

export const logAggregationConfig: DataRelayConfig = {
  type: "logs",
  displayName: "Log Aggregation",

  inputSchema: logEntryInputSchema,

  storage: {
    collection: "logs",
    getResourceId: data => data.source,
    getItemId: data => `${data.timestamp}_${Math.random().toString(36).substr(2, 9)}`,
    getTimestamp: data => data.timestamp,
    timeGrouping: "hour",
    buildPath: (user: User, source: string, timeKey: number | null, logId: string) => {
      return buildFlatPath(user, "logs", source, timeKey, logId)
    },
  },

  deduplication: {
    buildKey: data => `${data.source}_${data.timestamp}_${data.message}`,
    hashFields: [], // Don't check content hash for logs, allow duplicates
    cacheTTL: 0, // Disable content caching
  },

  retention: {
    maxAge: 604800000, // 7 days (logs can be high volume)
    enableCleanup: true,
    cleanupBatchSize: 500,
  },

  transform: {
    toStorage: data => ({
      source: data.source,
      level: data.level,
      message: data.message,
      timestamp: data.timestamp,
      context: data.context || {},
      stackTrace: data.stackTrace,
      userId: data.userId,
      requestId: data.requestId,
    }),
  },

  throttling: {
    delayPerRequest: 5,
    maxDelay: 2000,
    slowRequestThreshold: 20,
  },
}

// ============================================================================
// Generic JSON Document Store
// ============================================================================

const jsonDocumentInputSchema = z.object({
  collection: z.string().min(1),
  documentId: z.string().min(1),
  timestamp: z.number().optional().default(() => Date.now()),
  data: z.record(z.any()),
  tags: z.array(z.string()).optional(),
})

export const jsonDocumentConfig: DataRelayConfig = {
  type: "json-document",
  displayName: "JSON Document Store",

  inputSchema: jsonDocumentInputSchema,

  storage: {
    collection: "documents",
    getResourceId: data => data.collection,
    getItemId: data => data.documentId,
    getTimestamp: data => data.timestamp,
    timeGrouping: "none", // No time-based grouping for documents
    buildPath: (user: User, collection: string, timeKey: number | null, docId: string) => {
      return buildFlatPath(user, "documents", collection, timeKey, docId)
    },
  },

  deduplication: {
    buildKey: data => `${data.collection}_${data.documentId}`,
    hashFields: [], // Hash entire data object
    cacheTTL: 86400000, // 24 hours
  },

  retention: {
    maxAge: null, // Keep indefinitely
    enableCleanup: false,
    cleanupBatchSize: 50,
  },

  transform: {
    toStorage: data => ({
      documentId: data.documentId,
      collection: data.collection,
      timestamp: data.timestamp,
      data: data.data,
      tags: data.tags?.reduce((acc, tag) => ({...acc, [tag]: true}), {}) || {},
      updatedAt: Date.now(),
    }),
  },

  throttling: DEFAULT_THROTTLING,
}

