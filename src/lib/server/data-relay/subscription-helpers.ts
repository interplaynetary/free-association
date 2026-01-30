import type {
  SubscriptionConfig,
  SubscriptionContext,
  SubscriptionResult,
  LimitCheckResult,
} from "./config"
import { z } from "zod"

// Type for Gun/Mesh user instance
type User = any

/**
 * Helper utilities for common subscription patterns
 */

// ============================================================================
// Common Schemas
// ============================================================================

/**
 * Standard subscription schema with account code and signed URL
 */
export const standardSubscriptionSchema = z.object({
  code: z.string().min(1, "Account code required"),
  url: z.string().min(1, "Resource URL required"),
})

/**
 * Simple subscription schema with just resource ID
 */
export const simpleSubscriptionSchema = z.object({
  code: z.string().min(1, "Account code required"),
  resourceId: z.string().min(1, "Resource ID required"),
})

// ============================================================================
// Field-based Limit Checker
// ============================================================================

/**
 * Create a simple field-based limit checker
 */
export function createFieldBasedLimitChecker(
  countField: string,
  limitField: string,
): (account: any, resourceId: string) => Promise<LimitCheckResult> {
  return async (account: any, resourceId: string) => {
    const current = account[countField] || 0
    const limit = account[limitField]

    if (limit === null || limit === undefined) {
      return { allowed: true, current }
    }

    if (current >= limit) {
      return {
        allowed: false,
        error: `Subscription limit reached (${limit} maximum)`,
        current,
        limit,
      }
    }

    return { allowed: true, current, limit }
  }
}

// ============================================================================
// Account Subscription Tracking
// ============================================================================

/**
 * Track subscription in accountSubscriptions collection
 */
export async function trackAccountSubscription(
  user: User,
  relayType: string,
  accountCode: string,
  resourceId: string,
  subscribe: boolean,
): Promise<void> {
  return new Promise((resolve, reject) => {
    user
      .get("accountSubscriptions")
      .next(accountCode)
      .next(relayType)
      .next(resourceId)
      .put(subscribe ? true : null, (err: any) => {
        if (err) reject(err)
        else resolve()
      })
  })
}

/**
 * Check if account is subscribed to resource
 */
export async function isAccountSubscribed(
  user: User,
  relayType: string,
  accountCode: string,
  resourceId: string,
): Promise<boolean> {
  return new Promise(resolve => {
    user
      .get("accountSubscriptions")
      .next(accountCode)
      .next(relayType)
      .next(resourceId, (value: any) => {
        resolve(value === true)
      })
  })
}

// ============================================================================
// Resource Metadata Helpers
// ============================================================================

/**
 * Get or create resource metadata
 */
export async function getOrCreateResource(
  user: User,
  collection: string,
  resourceId: string,
  initialData?: any,
): Promise<any> {
  return new Promise(resolve => {
    user.get(collection).next(resourceId, (existing: any) => {
      if (existing) {
        resolve(existing)
      } else if (initialData) {
        user.get(collection).next(resourceId).put(initialData, () => {
          resolve(initialData)
        })
      } else {
        resolve(null)
      }
    })
  })
}

/**
 * Update resource subscriber count
 */
export async function updateSubscriberCount(
  user: User,
  collection: string,
  resourceId: string,
  delta: number,
): Promise<number> {
  return new Promise((resolve, reject) => {
    user.get(collection).next(resourceId, (resource: any) => {
      if (!resource) {
        reject(new Error("Resource not found"))
        return
      }

      const newCount = Math.max(0, (resource.subscriber_count || 0) + delta)

      user
        .get(collection)
        .next(resourceId)
        .next("subscriber_count")
        .put(newCount, (err: any) => {
          if (err) reject(err)
          else resolve(newCount)
        })
    })
  })
}

/**
 * Update account subscription count
 */
export async function updateAccountSubscriptionCount(
  user: User,
  accountCode: string,
  countField: string,
  delta: number,
): Promise<number> {
  return new Promise((resolve, reject) => {
    user.get("accounts").next(accountCode, (account: any) => {
      if (!account) {
        reject(new Error("Account not found"))
        return
      }

      const newCount = Math.max(0, (account[countField] || 0) + delta)

      user
        .get("accounts")
        .next(accountCode)
        .next(countField)
        .put(newCount, (err: any) => {
          if (err) reject(err)
          else resolve(newCount)
        })
    })
  })
}

// ============================================================================
// Standard Lifecycle Implementations
// ============================================================================

/**
 * Create standard onSubscribe handler
 */
export function createStandardSubscribeHandler(
  relayType: string,
  resourceCollection: string,
  countField: string,
) {
  return async (
    user: User,
    resourceId: string,
    context: SubscriptionContext,
    resourceMetadata?: any,
  ): Promise<SubscriptionResult> => {
    try {
      // Get or create resource
      const resource = await getOrCreateResource(
        user,
        resourceCollection,
        resourceId,
        resourceMetadata
          ? { ...resourceMetadata, subscriber_count: 0 }
          : { subscriber_count: 0 },
      )

      // Update subscriber count
      await updateSubscriberCount(user, resourceCollection, resourceId, 1)

      // Update account subscription count
      await updateAccountSubscriptionCount(user, context.accountCode, countField, 1)

      // Track subscription
      await trackAccountSubscription(user, relayType, context.accountCode, resourceId, true)

      return {
        success: true,
        data: resource,
      }
    } catch (error) {
      return {
        success: false,
        error: error instanceof Error ? error.message : "Subscription failed",
      }
    }
  }
}

/**
 * Create standard onUnsubscribe handler
 */
export function createStandardUnsubscribeHandler(
  relayType: string,
  resourceCollection: string,
  countField: string,
) {
  return async (
    user: User,
    resourceId: string,
    context: SubscriptionContext,
  ): Promise<SubscriptionResult> => {
    try {
      // Update subscriber count
      const newCount = await updateSubscriberCount(user, resourceCollection, resourceId, -1)

      // Update account subscription count
      await updateAccountSubscriptionCount(user, context.accountCode, countField, -1)

      // Untrack subscription
      await trackAccountSubscription(user, relayType, context.accountCode, resourceId, false)

      return {
        success: true,
        data: { subscriber_count: newCount },
      }
    } catch (error) {
      return {
        success: false,
        error: error instanceof Error ? error.message : "Unsubscription failed",
      }
    }
  }
}

/**
 * Create standard validateSubscription handler
 */
export function createStandardValidationHandler(resourceCollection: string) {
  return async (user: User, resourceId: string, accountCode?: string): Promise<boolean> => {
    return new Promise(resolve => {
      user.get(resourceCollection).next(resourceId, (resource: any) => {
        if (!resource) {
          resolve(false)
          return
        }

        // Check if resource has active subscribers
        if (!resource.subscriber_count || resource.subscriber_count === 0) {
          resolve(false)
          return
        }

        // If account code provided, verify this specific account is subscribed
        if (accountCode) {
          // This would need to check accountSubscriptions
          // For now, just check resource exists with subscribers
        }

        resolve(true)
      })
    })
  }
}

// ============================================================================
// Configuration Builders
// ============================================================================

/**
 * Build a simple subscription configuration
 */
export function buildSimpleSubscriptionConfig(
  relayType: string,
  resourceCollection: string,
  options: {
    required?: boolean
    schema?: z.ZodSchema
    countField?: string
    limitField?: string
    maxPerAccount?: number | null
    fetchMetadata?: (resourceId: string) => Promise<any>
    verifyResourceId?: (resourceId: string, account: any) => Promise<string | null>
    externalService?: {
      subscribe?: (resourceId: string) => Promise<SubscriptionResult>
      unsubscribe?: (resourceId: string) => Promise<SubscriptionResult>
    }
  } = {},
): SubscriptionConfig {
  const {
    required = false,
    schema = standardSubscriptionSchema,
    countField = "subscribed",
    limitField = "feeds",
    maxPerAccount = null,
    fetchMetadata,
    verifyResourceId,
    externalService,
  } = options

  return {
    required,
    subscriptionSchema: schema,
    resourceCollection,
    getResourceId: (data) => data.url || data.resourceId,
    verifyResourceId,
    fetchResourceMetadata: fetchMetadata,
    externalService,
    limits: {
      maxPerAccount,
      accountCountField: countField,
      accountLimitField: limitField,
      checkLimit: createFieldBasedLimitChecker(countField, limitField),
    },
    lifecycle: {
      onSubscribe: createStandardSubscribeHandler(relayType, resourceCollection, countField),
      onUnsubscribe: createStandardUnsubscribeHandler(relayType, resourceCollection, countField),
      validateSubscription: createStandardValidationHandler(resourceCollection),
    },
  }
}

