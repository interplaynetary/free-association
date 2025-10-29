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
 */

export {DataRelayEngine} from "./engine"
export {DataRelayRegistry, getRegistry, resetRegistry} from "./registry"
export type {DataRelayConfig} from "./config"
export {
  computeTimeKey,
  createContentHash,
  buildFlatPath,
  buildUserScopedPath,
  DEFAULT_THROTTLING,
  DEFAULT_RETENTION,
} from "./config"

// Export all preset configurations
export * from "./presets/rss-feed"
export * from "./presets/social-media"
export * from "./presets/generic-apis"

