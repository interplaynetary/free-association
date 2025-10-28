import {
  user,
  cleanupQueue,
  removeDays,
  processingCleanup,
  cleanupTimer,
  day,
  timeDbOperation,
  updateProcessingStats,
} from "./holster"
import type {FeedItemData} from "$lib/server/schemas/rsstream"

/**
 * Process and save a feed item
 */
export async function processItem(
  data: FeedItemData,
  dayKey: number,
  itemKey: string,
): Promise<{success: boolean; error?: string}> {
  const startTime = Date.now()

  try {
    // Use Promise.all for parallel operations with timing
    const [saveErr, removeErr] = await Promise.allSettled([
      timeDbOperation(cb => {
        user
          .get("feedItems")
          .next(data.url)
          .next(dayKey)
          .next(data.guid)
          .put(data, cb)
      }, `save-item-${data.url}`),
      timeDbOperation(cb => {
        const remove = {
          guid: data.guid,
          url: data.url,
        }
        user.get("remove").next(dayKey).put(remove, true, cb)
      }, `track-cleanup-${dayKey}`),
    ])

    const processingTime = Date.now() - startTime
    updateProcessingStats(processingTime)

    if (saveErr.status === "rejected" || saveErr.value) {
      console.log(
        "Error saving item:",
        saveErr.status === "rejected" ? saveErr.reason : saveErr.value,
      )
      return {success: false, error: "Error saving item"}
    }

    // Schedule cleanup for later to avoid blocking the response
    setImmediate(() => scheduleCleanup(Date.now() - 1209600000))

    return {success: true}
  } catch (error) {
    console.log("Error processing item:", error)
    return {success: false, error: "Error processing item"}
  }
}

/**
 * Optimized cleanup scheduler to batch operations and limit memory growth
 */
export function scheduleCleanup(twoWeeksAgo: number) {
  const removeKey = day(twoWeeksAgo)

  // Skip if already processed or queued
  if (removeDays.has(removeKey.toString()) || cleanupQueue.has(removeKey.toString())) {
    return
  }

  cleanupQueue.add(removeKey.toString())

  // Debounce cleanup operations to batch them
  if (cleanupTimer) {
    clearTimeout(cleanupTimer)
  }

  const timer = setTimeout(async () => {
    const keysToProcess = Array.from(cleanupQueue)
    cleanupQueue.clear()

    console.log(`Processing cleanup for ${keysToProcess.length} day(s)`)

    for (const key of keysToProcess) {
      if (removeDays.has(key) || processingCleanup.has(key)) continue

      processingCleanup.add(key) // Mark as currently processing
      try {
        await processCleanupForDay(Number(key))
        removeDays.add(key)
      } catch (error) {
        console.error(`Cleanup failed for day ${key}:`, error)
        // Re-queue for retry with exponential backoff
        setTimeout(() => {
          if (!removeDays.has(key)) {
            cleanupQueue.add(key)
            scheduleCleanup(Date.now() - 1209600000)
          }
        }, 5000)
      } finally {
        processingCleanup.delete(key) // Remove from processing set
      }
    }

    // Limit removeDays memory growth - keep only last 30 days of tracking
    if (removeDays.size > 30) {
      const sortedDays = Array.from(removeDays).sort()
      const oldestDays = sortedDays.slice(0, sortedDays.length - 30)
      oldestDays.forEach(day => removeDays.delete(day))
    }
  }, 1000) // 1 second debounce

  // Update the global timer reference
  ;(cleanupTimer as any) = timer
}

/**
 * Process cleanup for a specific day
 */
async function processCleanupForDay(removeKey: number): Promise<void> {
  return new Promise(resolve => {
    user.get("remove").next(removeKey, async (remove: any) => {
      if (!remove) {
        resolve()
        return
      }

      const items = Object.values(remove).filter(item => item)
      console.log(`Cleaning up ${items.length} items for day ${removeKey}`)

      // Process cleanup in batches to avoid overwhelming the system
      const BATCH_SIZE = 50
      for (let i = 0; i < items.length; i += BATCH_SIZE) {
        const batch = items.slice(i, i + BATCH_SIZE) as Array<{url: string; guid: string}>

        await Promise.allSettled(
          batch.map(
            item =>
              new Promise(res => {
                user
                  .get("feedItems")
                  .next(item.url)
                  .next(removeKey)
                  .next(item.guid)
                  .put(null, res)
              }),
          ),
        )

        // Small delay between batches to prevent overwhelming the system
        if (i + BATCH_SIZE < items.length) {
          await new Promise(r => setTimeout(r, 10))
        }
      }

      // Remove the cleanup tracking data (only if it existed)
      user
        .get("remove")
        .next(removeKey)
        .put(null, (err: any) => {
          if (err && err !== "error " + removeKey + " not found") {
            console.log(`Error removing cleanup key ${removeKey}:`, err)
          }
          resolve()
        })
    })
  })
}

