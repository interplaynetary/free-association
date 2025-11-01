/**
 * Holster-specific auth functions
 * 
 * Re-exports unified auth with Holster-specific defaults
 */

import type {RequestEvent} from "@sveltejs/kit"
import {checkAuth as unifiedCheckAuth} from "../middleware/unified-auth"

/**
 * Basic authentication middleware for private routes
 */
export function requireBasicAuth(event: RequestEvent): boolean {
  const result = unifiedCheckAuth(event, {
    allowBasic: true,
    allowJwt: false,
    allowApiKey: false
  })
  return result === null
}

/**
 * Check if user is authenticated and return 401 if not
 */
export function checkAuth(event: RequestEvent): Response | null {
  return unifiedCheckAuth(event, {
    allowBasic: true,
    allowJwt: false,
    allowApiKey: false
  })
}

