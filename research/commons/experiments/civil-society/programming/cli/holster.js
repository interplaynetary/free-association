/**
 * Holster instance for CLI
 * Shared between CLI commands
 */
import Holster from "@mblaney/holster/src/holster.js"

export const holster = Holster({
  peers: ['wss://holster.haza.website'],
  indexedDB: false,
  file: '.holster-data'
})

export const user = holster.user()

