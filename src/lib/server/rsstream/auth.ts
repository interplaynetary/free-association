import type {RequestEvent} from "@sveltejs/kit"
import {username, password} from "./holster"

/**
 * Basic authentication middleware for private routes
 */
export function requireBasicAuth(event: RequestEvent): boolean {
  const auth = event.request.headers.get("authorization")
  if (!auth) return false

  const [scheme, credentials] = auth.split(" ")
  if (scheme !== "Basic" || !credentials) return false

  const [u, p] = Buffer.from(credentials, "base64").toString().split(":")
  return u === username && p === password
}

/**
 * Check if user is authenticated and return 401 if not
 */
export function checkAuth(event: RequestEvent): Response | null {
  if (!requireBasicAuth(event)) {
    return new Response("Unauthorized", {status: 401})
  }
  return null
}

