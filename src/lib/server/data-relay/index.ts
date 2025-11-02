/**
 * Generic Data Relay System for Holster
 *
 * A flexible system for relaying data from external APIs to Holster storage.
 * Supports multiple data types with configurable:
 * - Storage paths and hierarchies
 * - Deduplication and content hashing
 * - Retention policies and cleanup
 * - Throttling and performance optimization
 * - Data transformation and validation
 * - Optional subscription management
 */

export {DataRelayEngine} from "./engine"
export {DataRelayRegistry, getRegistry, resetRegistry} from "./registry"
export {SubscriptionManager} from "./subscription-manager"

export type {
  DataRelayConfig,
  SubscriptionConfig,
  SubscriptionContext,
  SubscriptionResult,
  LimitCheckResult,
  ResourceMetadata,
} from "./config"

export {
  computeTimeKey,
  createContentHash,
  buildFlatPath,
  buildUserScopedPath,
  DEFAULT_THROTTLING,
  DEFAULT_RETENTION,
} from "./config"

export {
  buildSimpleSubscriptionConfig,
  createFieldBasedLimitChecker,
  createStandardSubscribeHandler,
  createStandardUnsubscribeHandler,
  createStandardValidationHandler,
  trackAccountSubscription,
  isAccountSubscribed,
  updateSubscriberCount,
  updateAccountSubscriptionCount,
  standardSubscriptionSchema,
  simpleSubscriptionSchema,
} from "./subscription-helpers"

// Export all preset configurations
export * from "./presets/rss-feed"
export * from "./presets/social-media"
export * from "./presets/generic-apis"

