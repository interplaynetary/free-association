import {initializeHolster, user} from "$lib/server/holster/core"
import {initializeMonitoring} from "$lib/server/holster/monitoring"
import {getRegistry} from "$lib/server/data-relay"
import {env} from "$env/dynamic/private"

// Initialize Holster on server startup
let initialized = false

if (!initialized) {
  initializeHolster()
    .then(() => {
      console.log("Holster initialized successfully")
      initializeMonitoring()

      // Initialize Data Relay System
      const registry = getRegistry(user)

      // Register presets based on environment configuration
      const enabledPresets = env.ENABLED_RELAYS
        ? env.ENABLED_RELAYS.split(",").map(s => s.trim())
        : ["rss-feed"] // Default to RSS only for backward compatibility

      registry.registerPresets(enabledPresets)

      // Start cache cleanup
      registry.startCacheCleanup(60000) // Every 60 seconds

      console.log(`Data Relay System initialized with: ${enabledPresets.join(", ")}`)
    })
    .catch(err => {
      console.error("Failed to initialize Holster:", err)
    })
  initialized = true
}
