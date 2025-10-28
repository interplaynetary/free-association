import Holster from "@mblaney/holster/src/holster.js"
import {env} from "$env/dynamic/private"
import type {AccountData, InviteCode} from "$lib/server/schemas/holster"

// ============================================================================
// Holster Instance
// ============================================================================

export const holster = Holster({secure: true, memoryLimit: 1536})
export const user = holster.user()

export const username = env.HOLSTER_USER_NAME ?? "host"
export const password = env.HOLSTER_USER_PASSWORD ?? "password"
export const host = env.APP_HOST ?? "http://localhost:3000"

// ============================================================================
// Account Management Caches
// ============================================================================

// inviteCodes is a map of invite codes and their (random) holster keys, stored
// in memory to avoid decrypting them in each of the functions they're required.
export const inviteCodes = new Map<string, InviteCode>()

// ============================================================================
// Performance Statistics
// ============================================================================

export let requestStats = {
  totalRequests: 0,
  slowRequests: 0,
  averageTime: 0,
  lastReset: Date.now(),
}

export let dbStats = {
  totalOps: 0,
  slowOps: 0,
  averageDbTime: 0,
  errorCount: 0,
  lastReset: Date.now(),
}

// ============================================================================
// Helper Functions
// ============================================================================

/**
 * Helper function to time database operations
 */
export function timeDbOperation<T>(
  operation: (callback: (err: any, result?: T) => void) => void,
  operationName = "db-op",
): Promise<T> {
  return new Promise((resolve, reject) => {
    const startTime = Date.now()

    operation((err, result) => {
      const endTime = Date.now()
      const duration = endTime - startTime

      // Update DB stats
      dbStats.totalOps++
      dbStats.averageDbTime =
        (dbStats.averageDbTime * (dbStats.totalOps - 1) + duration) /
        dbStats.totalOps

      if (err) {
        dbStats.errorCount++
        console.log(`[DB-ERROR] ${operationName}: ${err} (${duration}ms)`)
        reject(err)
      } else {
        if (duration > 2000) {
          // Log slow DB operations over 2 seconds
          dbStats.slowOps++
          console.log(`[DB-SLOW] ${operationName}: ${duration}ms`)
        }
        resolve(result as T)
      }
    })
  })
}

/**
 * Map invite codes from Holster to in-memory cache
 */
export function mapInviteCodes() {
  if (!user.is) {
    console.log("mapInviteCodes: Host error")
    return
  }

  const mapCodes = async (codes: any) => {
    if (!codes) return

    for (const [key, enc] of Object.entries(codes)) {
      const invite = (await holster.SEA.decrypt(enc, user.is)) as InviteCode
      if (invite && invite.code && !inviteCodes.has(invite.code)) {
        invite.key = key
        inviteCodes.set(invite.code, invite)
      }
    }
  }
  user.get("available").next("invite_codes").on(mapCodes, true)
}

/**
 * Initialize Holster and authenticate
 */
export async function initializeHolster(): Promise<void> {
  return new Promise((resolve, reject) => {
    console.log("Trying auth credentials for " + username)
    user.auth(username, password, (err: any) => {
      if (err) {
        console.log(err)
        reject(err)
      } else {
        console.log(username + " logged in")
        mapInviteCodes()
        resolve()
      }
    })
  })
}

/**
 * Get account data by code
 */
export async function getAccount(code: string): Promise<AccountData | null> {
  return new Promise(res => {
    user.get("accounts").next(code, res)
  })
}

/**
 * Update request statistics
 */
export function updateRequestStats(processingTime: number) {
  requestStats.totalRequests++
  requestStats.averageTime =
    (requestStats.averageTime * (requestStats.totalRequests - 1) +
      processingTime) /
    requestStats.totalRequests

  if (processingTime > 100) {
    requestStats.slowRequests++
  }
}

/**
 * Reset statistics if needed (hourly)
 */
export function resetStatsIfNeeded() {
  if (Date.now() - requestStats.lastReset > 3600000) {
    requestStats = {
      totalRequests: 0,
      slowRequests: 0,
      averageTime: 0,
      lastReset: Date.now(),
    }
    dbStats = {
      totalOps: 0,
      slowOps: 0,
      averageDbTime: 0,
      errorCount: 0,
      lastReset: Date.now(),
    }
  }
}

