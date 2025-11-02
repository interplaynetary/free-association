import type {
  SubscriptionConfig,
  SubscriptionContext,
  SubscriptionResult,
  LimitCheckResult,
  ResourceMetadata,
} from "./config"

// Type for Gun/Holster user instance
type User = any

/**
 * Manages subscription lifecycle for data relay resources
 * Handles validation, limits, and external service coordination
 */
export class SubscriptionManager {
  constructor(
    private config: SubscriptionConfig,
    private user: User,
    private relayType: string,
  ) {}

  /**
   * Subscribe a user to a resource
   */
  async subscribe(
    subscriptionData: any,
    accountCode: string,
  ): Promise<SubscriptionResult> {
    // Validate subscription data
    const validationResult = this.config.subscriptionSchema.safeParse(subscriptionData)
    if (!validationResult.success) {
      return {
        success: false,
        error: validationResult.error.message,
      }
    }

    const data = validationResult.data

    // Get account
    const account = await this.getAccount(accountCode)
    if (!account) {
      return {
        success: false,
        error: "Account not found",
      }
    }

    // Extract and verify resource ID
    let resourceId = this.config.getResourceId(data)
    if (this.config.verifyResourceId) {
      const verifiedId = await this.config.verifyResourceId(resourceId, account)
      if (!verifiedId) {
        return {
          success: false,
          error: "Could not verify resource identifier",
        }
      }
      resourceId = verifiedId
    }

    // Check if already subscribed
    const existingResource = await this.getResource(resourceId)
    if (existingResource && existingResource.subscriber_count > 0) {
      // Check if this specific account is already subscribed
      const accountSubscriptions = await this.getAccountSubscriptions(accountCode)
      if (accountSubscriptions.includes(resourceId)) {
        return {
          success: false,
          error: "Already subscribed to this resource",
        }
      }
    }

    // Check subscription limits
    const limitCheck = await this.checkLimits(account, resourceId)
    if (!limitCheck.allowed) {
      return {
        success: false,
        error: limitCheck.error || "Subscription limit reached",
      }
    }

    // Fetch resource metadata if needed
    let resourceMetadata: ResourceMetadata | undefined
    if (!existingResource && this.config.fetchResourceMetadata) {
      try {
        const fetched = await this.config.fetchResourceMetadata(resourceId)
        if (fetched) {
          resourceMetadata = fetched
        }
      } catch (error) {
        console.error(`[${this.relayType}] Failed to fetch resource metadata:`, error)
        return {
          success: false,
          error: "Failed to fetch resource information",
        }
      }
    }

    // Call external service if configured (only for first subscriber)
    if (
      !existingResource &&
      this.config.externalService?.subscribe
    ) {
      try {
        const externalResult = await this.config.externalService.subscribe(resourceId)
        if (!externalResult.success) {
          return {
            success: false,
            error: externalResult.error || "External service subscription failed",
          }
        }
      } catch (error) {
        console.error(`[${this.relayType}] External service error:`, error)
        return {
          success: false,
          error: "External service unavailable",
        }
      }
    }

    // Build subscription context
    const context: SubscriptionContext = {
      accountCode,
      account,
      signedUrl: data.url,
      metadata: data,
    }

    // Execute lifecycle hook
    try {
      const result = await this.config.lifecycle.onSubscribe(
        this.user,
        resourceId,
        context,
        resourceMetadata,
      )

      if (result.success) {
        console.log(`[${this.relayType}] Subscribed: ${accountCode} -> ${resourceId}`)
      }

      return result
    } catch (error) {
      console.error(`[${this.relayType}] Subscription lifecycle error:`, error)
      return {
        success: false,
        error: error instanceof Error ? error.message : "Subscription failed",
      }
    }
  }

  /**
   * Unsubscribe a user from a resource
   */
  async unsubscribe(
    subscriptionData: any,
    accountCode: string,
  ): Promise<SubscriptionResult> {
    // Validate subscription data
    const validationResult = this.config.subscriptionSchema.safeParse(subscriptionData)
    if (!validationResult.success) {
      return {
        success: false,
        error: validationResult.error.message,
      }
    }

    const data = validationResult.data

    // Get account
    const account = await this.getAccount(accountCode)
    if (!account) {
      return {
        success: false,
        error: "Account not found",
      }
    }

    // Extract and verify resource ID
    let resourceId = this.config.getResourceId(data)
    if (this.config.verifyResourceId) {
      const verifiedId = await this.config.verifyResourceId(resourceId, account)
      if (!verifiedId) {
        return {
          success: false,
          error: "Could not verify resource identifier",
        }
      }
      resourceId = verifiedId
    }

    // Check if resource exists
    const resource = await this.getResource(resourceId)
    if (!resource) {
      return {
        success: false,
        error: "Resource not found",
      }
    }

    // Build subscription context
    const context: SubscriptionContext = {
      accountCode,
      account,
      signedUrl: data.url,
      metadata: data,
    }

    // Execute lifecycle hook
    try {
      const result = await this.config.lifecycle.onUnsubscribe(
        this.user,
        resourceId,
        context,
      )

      // Call external service if this was the last subscriber
      if (
        result.success &&
        resource.subscriber_count <= 1 &&
        this.config.externalService?.unsubscribe
      ) {
        try {
          await this.config.externalService.unsubscribe(resourceId)
        } catch (error) {
          console.error(`[${this.relayType}] External service unsubscribe error:`, error)
          // Don't fail the unsubscribe if external service fails
        }
      }

      if (result.success) {
        console.log(`[${this.relayType}] Unsubscribed: ${accountCode} -> ${resourceId}`)
      }

      return result
    } catch (error) {
      console.error(`[${this.relayType}] Unsubscription lifecycle error:`, error)
      return {
        success: false,
        error: error instanceof Error ? error.message : "Unsubscription failed",
      }
    }
  }

  /**
   * Validate that a subscription exists for a resource
   */
  async validateSubscription(resourceId: string, accountCode?: string): Promise<boolean> {
    if (!this.config.lifecycle.validateSubscription) {
      // If no validation function provided, allow all
      return true
    }

    try {
      return await this.config.lifecycle.validateSubscription(
        this.user,
        resourceId,
        accountCode,
      )
    } catch (error) {
      console.error(`[${this.relayType}] Subscription validation error:`, error)
      return false
    }
  }

  /**
   * Check subscription limits for an account
   */
  private async checkLimits(account: any, resourceId: string): Promise<LimitCheckResult> {
    // Use custom limit check if provided
    if (this.config.limits.checkLimit) {
      return await this.config.limits.checkLimit(account, resourceId)
    }

    // Default limit check using field names
    const current = account[this.config.limits.accountCountField] || 0
    const limit = account[this.config.limits.accountLimitField]

    // If no limit set, allow unlimited
    if (limit === null || limit === undefined) {
      return {allowed: true}
    }

    if (current >= limit) {
      return {
        allowed: false,
        error: `Subscription limit reached (${limit} maximum)`,
        current,
        limit,
      }
    }

    return {
      allowed: true,
      current,
      limit,
    }
  }

  /**
   * Get account from Holster
   */
  private async getAccount(accountCode: string): Promise<any> {
    return new Promise(resolve => {
      this.user.get("accounts").next(accountCode, resolve)
    })
  }

  /**
   * Get resource metadata from Holster
   */
  private async getResource(resourceId: string): Promise<any> {
    return new Promise(resolve => {
      this.user.get(this.config.resourceCollection).next(resourceId, resolve)
    })
  }

  /**
   * Get list of resource IDs that an account is subscribed to
   */
  private async getAccountSubscriptions(accountCode: string): Promise<string[]> {
    return new Promise(resolve => {
      this.user.get("accountSubscriptions").next(accountCode).next(this.relayType, (data: any) => {
        if (!data) {
          resolve([])
          return
        }

        const subscriptions: string[] = []
        for (const [key, value] of Object.entries(data)) {
          if (key !== "_" && value === true) {
            subscriptions.push(key)
          }
        }
        resolve(subscriptions)
      })
    })
  }

  /**
   * Get subscription statistics
   */
  async getStats(): Promise<{
    totalResources: number
    totalSubscriptions: number
  }> {
    return new Promise(resolve => {
      this.user.get(this.config.resourceCollection, (resources: any) => {
        if (!resources) {
          resolve({totalResources: 0, totalSubscriptions: 0})
          return
        }

        let totalResources = 0
        let totalSubscriptions = 0

        for (const [key, value] of Object.entries(resources)) {
          if (key !== "_" && value && typeof value === "object") {
            totalResources++
            const resource = value as any
            if (resource.subscriber_count) {
              totalSubscriptions += resource.subscriber_count
            }
          }
        }

        resolve({totalResources, totalSubscriptions})
      })
    })
  }
}

