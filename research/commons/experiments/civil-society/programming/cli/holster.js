/**
 * Mesh instance for CLI
 * Shared between CLI commands
 */
import Mesh from "@playnet/mesh/src/mesh.js"

export const mesh = Mesh({
  peers: ['wss://free.playnet.lol'],
  indexedDB: false,
  file: '.mesh-data'
})

export const user = mesh.user()

