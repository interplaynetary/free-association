import { z } from 'zod';

/**
 * Key Health Status
 */
export const KeyHealthStatus = z.enum([
  'healthy',
  'degraded',
  'failed',
  'rate_limited',
  'depleted'
]);

export type KeyHealthStatus = z.infer<typeof KeyHealthStatus>;

/**
 * Usage Statistics for a key
 */
export const UsageStatsSchema = z.object({
  requestsToday: z.number().default(0),
  costToday: z.number().default(0),
  successCount: z.number().default(0),
  failureCount: z.number().default(0),
  creditsRemaining: z.number().nullable().default(null)
});

export type UsageStats = z.infer<typeof UsageStatsSchema>;

/**
 * Key Pool Structure
 */
export const KeyPoolSchema = z.object({
  keys: z.array(z.string()),
  currentIndex: z.number().default(0),
  healthStatus: z.record(z.string(), KeyHealthStatus),
  rateLimits: z.record(z.string(), z.any()),
  lastUsed: z.record(z.string(), z.number()),
  usageStats: z.record(z.string(), UsageStatsSchema)
});

export type KeyPool = z.infer<typeof KeyPoolSchema>;

/**
 * Health Report Request
 */
export const HealthReportRequestSchema = z.object({
  key: z.string().min(1),
  status: KeyHealthStatus,
  error: z.string().optional().nullable(),
  cost: z.number().optional().nullable()
});

export type HealthReportRequest = z.infer<typeof HealthReportRequestSchema>;

/**
 * Add Key Request
 */
export const AddKeyRequestSchema = z.object({
  key: z.string().min(1),
  metadata: z.object({
    donorId: z.string().optional(),
    donorName: z.string().optional(),
    addedAt: z.number().optional(),
    notes: z.string().optional()
  }).optional()
});

export type AddKeyRequest = z.infer<typeof AddKeyRequestSchema>;

/**
 * Remove Key Request
 */
export const RemoveKeyRequestSchema = z.object({
  key: z.string().min(1)
});

export type RemoveKeyRequest = z.infer<typeof RemoveKeyRequestSchema>;

/**
 * Model Health Summary
 */
export const ModelHealthSummarySchema = z.object({
  totalKeys: z.number(),
  health: z.object({
    healthy: z.number(),
    degraded: z.number(),
    failed: z.number(),
    rate_limited: z.number(),
    depleted: z.number()
  }),
  currentIndex: z.number(),
  stats: z.object({
    totalRequests: z.number(),
    successRate: z.string(),
    totalCostToday: z.string()
  })
});

export type ModelHealthSummary = z.infer<typeof ModelHealthSummarySchema>;

