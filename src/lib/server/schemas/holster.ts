import {z} from "zod"

// ============================================================================
// Request Schemas
// ============================================================================

export const requestInviteCodeSchema = z.object({
  email: z.string().email(),
})

export const checkCodesSchema = z.object({
  codes: z.array(z.string()).min(1),
})

export const checkInviteCodeSchema = z.object({
  code: z.string().optional().default("admin"),
})

export const claimInviteCodeSchema = z.object({
  code: z.string().optional().default("admin"),
  pub: z.string().min(1),
  epub: z.string().min(1),
  username: z.string().regex(/^\w+$/, "Username must contain only numbers, letters and underscore"),
  email: z.string().email(),
})

export const validateEmailSchema = z.object({
  code: z.string().min(1),
  validate: z.string().min(1),
})

export const resetPasswordSchema = z.object({
  code: z.string().min(1),
  email: z.string().email(),
})

export const updatePasswordSchema = z.object({
  code: z.string().min(1),
  reset: z.string().min(1),
  pub: z.string().min(1),
  epub: z.string().min(1),
  username: z.string().min(1),
  name: z.string().min(1),
})

export const addFeedSchema = z.object({
  code: z.string().min(1),
  url: z.string().min(1), // This should be a signed URL
})

export const addSubscriberSchema = z.object({
  code: z.string().min(1),
  url: z.string().min(1), // This should be a signed URL
})

export const removeSubscriberSchema = z.object({
  code: z.string().min(1),
  url: z.string().min(1), // This should be a signed URL
})

export const createInviteCodesSchema = z.object({
  code: z.string().min(1),
  count: z.number().int().positive().optional().default(1),
})

export const sendInviteCodeSchema = z.object({
  code: z.string().min(1),
  email: z.string().email(),
})

export const updateFeedLimitSchema = z.object({
  code: z.string().min(1),
  limit: z.number().int().positive(),
})

export const removeFeedSchema = z.object({
  url: z.string().min(1),
})

// Note: Feed item schemas (addItemSchema, enclosureSchema, etc.) 
// have been moved to data-relay/presets/rss-feed.ts

// ============================================================================
// Response Schemas
// ============================================================================

export const healthResponseSchema = z.object({
  status: z.literal("ok"),
  uptime: z.number(),
  memory: z.number(),
  requests: z.number(),
  timestamp: z.number(),
})

export const performanceResponseSchema = z.object({
  timestamp: z.string(),
  uptime: z.number(),
  memory: z.object({
    rss: z.number(),
    heapUsed: z.number(),
    heapTotal: z.number(),
    external: z.number(),
  }),
  requests: z.object({
    totalRequests: z.number(),
    slowRequests: z.number(),
    averageTime: z.number(),
    lastReset: z.number(),
    slowRequestPercentage: z.number(),
  }),
  database: z.object({
    totalOps: z.number(),
    slowOps: z.number(),
    averageDbTime: z.number(),
    errorCount: z.number(),
    lastReset: z.number(),
    slowDbPercentage: z.number(),
    errorPercentage: z.number(),
  }),
  processing: z.object({
    averageProcessingTime: z.number(),
    totalProcessed: z.number(),
    lastProcessedTime: z.number(),
    delayThreshold: z.number(),
    currentDelay: z.number(),
    pendingRequests: z.number(),
  }),
  caches: z.object({
    inviteCodes: z.number(),
    removeDays: z.number(),
    cleanupQueue: z.number(),
    contentHash: z.number(),
  }),
  process: z.object({
    pid: z.number(),
    version: z.string(),
    platform: z.string(),
  }),
})

export const addFeedResponseSchema = z.object({
  add: z.object({
    url: z.string(),
    title: z.string(),
    description: z.string().optional(),
    html_url: z.string().optional(),
    language: z.string().optional(),
    image: z.string().optional(),
  }),
})

export const errorResponseSchema = z.object({
  error: z.string(),
})

// ============================================================================
// Internal Data Structures
// ============================================================================

export const accountDataSchema = z.object({
  pub: z.string(),
  epub: z.string(),
  username: z.string(),
  name: z.string(),
  email: z.string(), // encrypted
  validate: z.string().nullable().optional(), // encrypted validation code
  ref: z.string().optional(),
  host: z.string(),
  feeds: z.number(),
  subscribed: z.number(),
  reset: z.string().optional(), // encrypted reset code
  expiry: z.number().optional(),
  prev: z.string().optional(), // previous public key after password reset
})

export const feedDataSchema = z.object({
  title: z.string(),
  description: z.string(),
  html_url: z.string(),
  language: z.string(),
  image: z.string(),
  subscriber_count: z.number().int().nonnegative(),
})

// Note: feedItemDataSchema moved to data-relay/presets/rss-feed.ts

export const inviteCodeSchema = z.object({
  code: z.string(),
  owner: z.string(),
  key: z.string().optional(),
})

// ============================================================================
// Type Exports
// ============================================================================

export type RequestInviteCode = z.infer<typeof requestInviteCodeSchema>
export type CheckCodes = z.infer<typeof checkCodesSchema>
export type CheckInviteCode = z.infer<typeof checkInviteCodeSchema>
export type ClaimInviteCode = z.infer<typeof claimInviteCodeSchema>
export type ValidateEmail = z.infer<typeof validateEmailSchema>
export type ResetPassword = z.infer<typeof resetPasswordSchema>
export type UpdatePassword = z.infer<typeof updatePasswordSchema>
export type AddFeed = z.infer<typeof addFeedSchema>
export type AddSubscriber = z.infer<typeof addSubscriberSchema>
export type RemoveSubscriber = z.infer<typeof removeSubscriberSchema>
export type CreateInviteCodes = z.infer<typeof createInviteCodesSchema>
export type SendInviteCode = z.infer<typeof sendInviteCodeSchema>
export type UpdateFeedLimit = z.infer<typeof updateFeedLimitSchema>
export type RemoveFeed = z.infer<typeof removeFeedSchema>
export type HealthResponse = z.infer<typeof healthResponseSchema>
export type PerformanceResponse = z.infer<typeof performanceResponseSchema>
export type AccountData = z.infer<typeof accountDataSchema>
export type FeedData = z.infer<typeof feedDataSchema>
export type InviteCode = z.infer<typeof inviteCodeSchema>

// Note: AddItem and FeedItemData types moved to data-relay/presets/rss-feed.ts

