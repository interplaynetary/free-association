import {initializeHolster, user} from "$lib/server/holster/core"
import {initializeMonitoring} from "$lib/server/holster/monitoring"
import {getRegistry} from "$lib/server/data-relay"
import {env} from "$env/dynamic/private"
import {startScheduler} from "$lib/server/collective"
import {createCallbacks} from "$lib/server/collective/callbacks"
import {startCollectiveTreeScheduler} from "$lib/server/collective-tree"
import {createCollectiveTreeCallbacks} from "$lib/server/collective-tree/callbacks"

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

      // Initialize Collective Recognition Scheduler
      try {
        const callbacks = createCallbacks();
        startScheduler(callbacks);
        console.log("✅ Collective Recognition Scheduler initialized")
      } catch (schedErr) {
        console.error("❌ Failed to initialize Collective Recognition Scheduler:", schedErr)
      }

      // Initialize Collective Tree Scheduler
      try {
        const treeCallbacks = createCollectiveTreeCallbacks();
        startCollectiveTreeScheduler(treeCallbacks);
        console.log("✅ Collective Tree Scheduler initialized")
      } catch (treeErr) {
        console.error("❌ Failed to initialize Collective Tree Scheduler:", treeErr)
      }
    })
    .catch(err => {
      console.error("Failed to initialize Holster:", err)
    })
  initialized = true
}
