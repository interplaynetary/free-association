import type {User} from "@mblaney/holster/src/holster.js"
import {DataRelayEngine} from "./engine"
import type {DataRelayConfig} from "./config"

// Import all preset configurations
import {rssFeedConfig} from "./presets/rss-feed"
import {twitterConfig, mastodonConfig, redditConfig} from "./presets/social-media"
import {
  webhookConfig,
  iotSensorConfig,
  emailArchiveConfig,
  logAggregationConfig,
  jsonDocumentConfig,
} from "./presets/generic-apis"

/**
 * Registry for managing multiple data relay engines
 */
export class DataRelayRegistry {
  private engines = new Map<string, DataRelayEngine>()
  private user: User
  private cleanupInterval: ReturnType<typeof setInterval> | null = null

  constructor(user: User) {
    this.user = user
  }

  /**
   * Register a new data relay configuration
   */
  register(config: DataRelayConfig): DataRelayEngine {
    if (this.engines.has(config.type)) {
      throw new Error(`Data relay type "${config.type}" already registered`)
    }

    const engine = new DataRelayEngine(config, this.user)
    this.engines.set(config.type, engine)

    console.log(`[DataRelay] Registered: ${config.displayName} (${config.type})`)

    return engine
  }

  /**
   * Get a registered engine by type
   */
  get(type: string): DataRelayEngine | undefined {
    return this.engines.get(type)
  }

  /**
   * Get all registered engine types
   */
  getTypes(): string[] {
    return Array.from(this.engines.keys())
  }

  /**
   * Process data through the appropriate relay
   */
  async process(type: string, data: unknown): Promise<ReturnType<DataRelayEngine["processItem"]>> {
    const engine = this.engines.get(type)
    if (!engine) {
      return {
        success: false,
        error: `Unknown data relay type: ${type}`,
        status: "invalid",
      }
    }

    return engine.processItem(data)
  }

  /**
   * Get statistics for all engines
   */
  getStats() {
    const stats: Record<string, any> = {}
    for (const [type, engine] of this.engines.entries()) {
      stats[type] = engine.getStats()
    }
    return stats
  }

  /**
   * Get statistics for a specific engine
   */
  getEngineStats(type: string) {
    const engine = this.engines.get(type)
    return engine ? engine.getStats() : null
  }

  /**
   * Start periodic cache cleanup for all engines
   */
  startCacheCleanup(intervalMs: number = 60000): void {
    if (this.cleanupInterval) {
      clearInterval(this.cleanupInterval)
    }

    this.cleanupInterval = setInterval(() => {
      for (const [type, engine] of this.engines.entries()) {
        try {
          engine.cleanupCaches()
        } catch (error) {
          console.error(`[DataRelay] Error cleaning caches for ${type}:`, error)
        }
      }
    }, intervalMs)

    console.log(`[DataRelay] Cache cleanup started (${intervalMs}ms interval)`)
  }

  /**
   * Stop cache cleanup
   */
  stopCacheCleanup(): void {
    if (this.cleanupInterval) {
      clearInterval(this.cleanupInterval)
      this.cleanupInterval = null
      console.log("[DataRelay] Cache cleanup stopped")
    }
  }

  /**
   * Register all preset configurations
   */
  registerPresets(presets: string[] = ["all"]): void {
    const allPresets: Record<string, DataRelayConfig> = {
      "rss-feed": rssFeedConfig,
      twitter: twitterConfig,
      mastodon: mastodonConfig,
      reddit: redditConfig,
      webhook: webhookConfig,
      "iot-sensor": iotSensorConfig,
      email: emailArchiveConfig,
      logs: logAggregationConfig,
      "json-document": jsonDocumentConfig,
    }

    const toRegister =
      presets.includes("all") ? Object.keys(allPresets) : presets

    for (const preset of toRegister) {
      const config = allPresets[preset]
      if (config) {
        try {
          this.register(config)
        } catch (error) {
          console.error(`[DataRelay] Failed to register ${preset}:`, error)
        }
      } else {
        console.warn(`[DataRelay] Unknown preset: ${preset}`)
      }
    }
  }

  /**
   * Unregister a data relay type
   */
  unregister(type: string): boolean {
    return this.engines.delete(type)
  }

  /**
   * Clear all registered engines
   */
  clear(): void {
    this.stopCacheCleanup()
    this.engines.clear()
  }
}

// ============================================================================
// Singleton instance (optional)
// ============================================================================

let registryInstance: DataRelayRegistry | null = null

/**
 * Get or create the singleton registry instance
 */
export function getRegistry(user: User): DataRelayRegistry {
  if (!registryInstance || registryInstance["user"] !== user) {
    registryInstance = new DataRelayRegistry(user)
  }
  return registryInstance
}

/**
 * Reset the singleton registry
 */
export function resetRegistry(): void {
  if (registryInstance) {
    registryInstance.clear()
    registryInstance = null
  }
}

// ============================================================================
// Preset exports
// ============================================================================

export {rssFeedConfig} from "./presets/rss-feed"
export {twitterConfig, mastodonConfig, redditConfig} from "./presets/social-media"
export {
  webhookConfig,
  iotSensorConfig,
  emailArchiveConfig,
  logAggregationConfig,
  jsonDocumentConfig,
} from "./presets/generic-apis"

