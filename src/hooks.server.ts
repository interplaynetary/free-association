import {initializeHolster} from "$lib/server/rsstream/holster"
import {initializeMonitoring} from "$lib/server/rsstream/monitoring"

// Initialize Holster on server startup
let initialized = false

if (!initialized) {
  initializeHolster()
    .then(() => {
      console.log("Holster initialized successfully")
      initializeMonitoring()
    })
    .catch(err => {
      console.error("Failed to initialize Holster:", err)
    })
  initialized = true
}

