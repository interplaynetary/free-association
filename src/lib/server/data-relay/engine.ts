import type {DataRelayConfig} from "./config"
import {computeTimeKey, createContentHash, DEFAULT_THROTTLING} from "./config"
import type {User} from "@mblaney/holster/src/holster.js"

/**
 * Generic data relay engine for processing and storing data to Holster
 */
export class DataRelayEngine<TInput = any, TStored = any> {
  private config: DataRelayConfig<TInput, TStored>
  private user: User

  // In-memory caches (per instance)
  private pendingRequests = new Map<string, {startTime: number}>()
  private contentHashCache = new Map<string, {hash: string; timestamp: number}>()
  private cleanupQueue = new Set<string>()
  private processingCleanup = new Set<string>()
  private cleanupTimer: ReturnType<typeof setTimeout> | null = null

  // Statistics
  private stats = {
    processing: {
      averageProcessingTime: 0,
      totalProcessed: 0,
      lastProcessedTime: 0,
      currentDelay: 0,
    },
    requests: {
      totalRequests: 0,
      slowRequests: 0,
      averageTime: 0,
      duplicates: 0,
      unchanged: 0,
      ageFiltered: 0,
    },
  }

  constructor(config: DataRelayConfig<TInput, TStored>, user: User) {
    this.config = config
    this.user = user
  }

  /**
   * Process and store a data item
   */
  async processItem(
    rawData: unknown,
  ): Promise<
    | {success: true; status: "stored" | "unchanged"}
    | {success: false; error: string; status: "duplicate" | "invalid" | "age_filtered" | "error"}
  > {
    const startTime = Date.now()

    // Validate input
    const validationResult = this.config.inputSchema.safeParse(rawData)
    if (!validationResult.success) {
      return {
        success: false,
        error: validationResult.error.message,
        status: "invalid",
      }
    }

    const data = validationResult.data as TInput

    // Extract identifiers
    const resourceId = this.config.storage.getResourceId(data)
    const itemId = this.config.storage.getItemId(data)
    const timestamp = this.config.storage.getTimestamp(data)

    // Build deduplication key
    const dedupKey = this.config.deduplication.buildKey(data)

    // Check for duplicate request
    if (this.pendingRequests.has(dedupKey)) {
      this.stats.requests.duplicates++
      return {success: false, error: "Already processing", status: "duplicate"}
    }

    // Mark as processing
    this.pendingRequests.set(dedupKey, {startTime: Date.now()})

    try {
      // Age filtering
      if (this.config.ageFilter) {
        const age = Date.now() - timestamp
        if (age > this.config.ageFilter.maxItemAge) {
          this.stats.requests.ageFiltered++
          return {
            success: false,
            error: this.config.ageFilter.rejectionMessage,
            status: "age_filtered",
          }
        }
      }

      // Transform data
      const storedData = this.config.transform.toStorage(data)

      // Custom validation
      if (this.config.transform.validate && !this.config.transform.validate(storedData)) {
        return {success: false, error: "Validation failed after transformation", status: "invalid"}
      }

      // Content hash check
      const contentHash = createContentHash(
        storedData as Record<string, any>,
        this.config.deduplication.hashFields as string[],
      )
      const hashCacheKey = dedupKey
      const cachedHash = this.contentHashCache.get(hashCacheKey)

      const now = Date.now()
      if (
        cachedHash &&
        cachedHash.hash === contentHash &&
        now - cachedHash.timestamp < this.config.deduplication.cacheTTL
      ) {
        this.stats.requests.unchanged++
        return {success: true, status: "unchanged"}
      }

      // Apply throttling
      const throttling = this.config.throttling || DEFAULT_THROTTLING
      const pendingDelay = this.pendingRequests.size * throttling.delayPerRequest
      const systemDelay =
        this.stats.processing.currentDelay > 0
          ? Math.min(this.stats.processing.currentDelay, 10000)
          : 0
      const totalThrottle = Math.min(pendingDelay + systemDelay, throttling.maxDelay)

      if (totalThrottle > 0) {
        await new Promise(resolve => setTimeout(resolve, totalThrottle))
      }

      // Update content hash cache
      this.contentHashCache.set(hashCacheKey, {
        hash: contentHash,
        timestamp: now,
      })

      // Compute time key
      const timeKey = computeTimeKey(timestamp, this.config.storage.timeGrouping)

      // Store item
      await this.storeItem(resourceId, timeKey, itemId, storedData)

      // Update metadata if configured
      if (this.config.metadata?.onItemAdd) {
        const metadataKey = this.config.metadata.getKey(data)
        await this.updateMetadata(metadataKey, "add")
      }

      // Schedule cleanup if enabled
      if (this.config.retention.enableCleanup && this.config.retention.maxAge) {
        this.scheduleCleanup(Date.now() - this.config.retention.maxAge)
      }

      // Update statistics
      const processingTime = Date.now() - startTime
      this.updateStats(processingTime)

      console.log(
        `[${this.config.type}] Stored: ${resourceId}/${itemId} (${processingTime}ms)`,
      )

      return {success: true, status: "stored"}
    } catch (error) {
      console.error(`[${this.config.type}] Error processing item:`, error)
      return {
        success: false,
        error: error instanceof Error ? error.message : "Unknown error",
        status: "error",
      }
    } finally {
      this.pendingRequests.delete(dedupKey)
    }
  }

  /**
   * Store item to Holster
   */
  private async storeItem(
    resourceId: string,
    timeKey: number | null,
    itemId: string,
    data: TStored,
  ): Promise<void> {
    const itemPromise = new Promise<void>((resolve, reject) => {
      const path = this.config.storage.buildPath(this.user, resourceId, timeKey, itemId)
      path.put(data, (err: any) => {
        if (err) reject(err)
        else resolve()
      })
    })

    // Track for cleanup if enabled
    const cleanupPromise =
      this.config.retention.enableCleanup && timeKey !== null
        ? new Promise<void>((resolve, reject) => {
            const remove = {itemId, resourceId}
            this.user.get("remove").next(this.config.type).next(timeKey).put(remove, true, (err: any) => {
              if (err) reject(err)
              else resolve()
            })
          })
        : Promise.resolve()

    await Promise.all([itemPromise, cleanupPromise])
  }

  /**
   * Update metadata (e.g., subscriber counts)
   */
  private async updateMetadata(key: string, action: "add" | "remove"): Promise<void> {
    if (!this.config.metadata) return

    const currentMetadata = await new Promise(resolve => {
      this.user.get(this.config.metadata!.collection).next(key, resolve)
    })

    const handler =
      action === "add" ? this.config.metadata.onItemAdd : this.config.metadata.onItemRemove

    if (!handler) return

    const updatedMetadata = await handler(this.user, key, currentMetadata)

    if (updatedMetadata) {
      await new Promise<void>((resolve, reject) => {
        this.user
          .get(this.config.metadata!.collection)
          .next(key)
          .put(updatedMetadata, (err: any) => {
            if (err) reject(err)
            else resolve()
          })
      })
    }
  }

  /**
   * Schedule cleanup of old items
   */
  private scheduleCleanup(cutoffTime: number): void {
    if (!this.config.retention.enableCleanup) return

    const timeKey = computeTimeKey(cutoffTime, this.config.storage.timeGrouping)
    if (timeKey === null) return

    const removeKey = timeKey.toString()

    if (this.cleanupQueue.has(removeKey) || this.processingCleanup.has(removeKey)) {
      return
    }

    this.cleanupQueue.add(removeKey)

    if (this.cleanupTimer) {
      clearTimeout(this.cleanupTimer)
    }

    this.cleanupTimer = setTimeout(async () => {
      const keysToProcess = Array.from(this.cleanupQueue)
      this.cleanupQueue.clear()

      console.log(`[${this.config.type}] Processing cleanup for ${keysToProcess.length} time period(s)`)

      for (const key of keysToProcess) {
        if (this.processingCleanup.has(key)) continue

        this.processingCleanup.add(key)
        try {
          await this.processCleanupForKey(Number(key))
        } catch (error) {
          console.error(`[${this.config.type}] Cleanup failed for ${key}:`, error)
        } finally {
          this.processingCleanup.delete(key)
        }
      }
    }, 1000) // 1 second debounce
  }

  /**
   * Process cleanup for a specific time key
   */
  private async processCleanupForKey(timeKey: number): Promise<void> {
    return new Promise(resolve => {
      this.user.get("remove").next(this.config.type).next(timeKey, async (remove: any) => {
        if (!remove) {
          resolve()
          return
        }

        const items = Object.values(remove).filter(item => item) as Array<{
          itemId: string
          resourceId: string
        }>

        console.log(`[${this.config.type}] Cleaning up ${items.length} items for time ${timeKey}`)

        const batchSize = this.config.retention.cleanupBatchSize
        for (let i = 0; i < items.length; i += batchSize) {
          const batch = items.slice(i, i + batchSize)

          await Promise.allSettled(
            batch.map(item =>
              new Promise<void>(res => {
                const path = this.config.storage.buildPath(
                  this.user,
                  item.resourceId,
                  timeKey,
                  item.itemId,
                )
                path.put(null, res)
              }),
            ),
          )

          if (i + batchSize < items.length) {
            await new Promise(r => setTimeout(r, 10))
          }
        }

        // Remove cleanup tracking
        this.user
          .get("remove")
          .next(this.config.type)
          .next(timeKey)
          .put(null, () => resolve())
      })
    })
  }

  /**
   * Update processing statistics
   */
  private updateStats(processingTime: number): void {
    this.stats.processing.totalProcessed++
    this.stats.processing.averageProcessingTime =
      (this.stats.processing.averageProcessingTime *
        (this.stats.processing.totalProcessed - 1) +
        processingTime) /
      this.stats.processing.totalProcessed
    this.stats.processing.lastProcessedTime = processingTime

    this.stats.requests.totalRequests++
    this.stats.requests.averageTime =
      (this.stats.requests.averageTime * (this.stats.requests.totalRequests - 1) +
        processingTime) /
      this.stats.requests.totalRequests

    const throttling = this.config.throttling || DEFAULT_THROTTLING
    if (processingTime > throttling.slowRequestThreshold) {
      this.stats.requests.slowRequests++
    }

    // Adaptive delay
    if (processingTime > 2000) {
      this.stats.processing.currentDelay = Math.max(
        this.stats.processing.currentDelay,
        processingTime - 2000,
      )
    } else {
      this.stats.processing.currentDelay = Math.max(
        0,
        this.stats.processing.currentDelay - 100,
      )
    }
  }

  /**
   * Get current statistics
   */
  getStats() {
    return {
      ...this.stats,
      caches: {
        pending: this.pendingRequests.size,
        contentHash: this.contentHashCache.size,
        cleanupQueue: this.cleanupQueue.size,
        processingCleanup: this.processingCleanup.size,
      },
    }
  }

  /**
   * Clean up expired cache entries
   */
  cleanupCaches(): void {
    const now = Date.now()

    // Clean content hash cache
    const expired: string[] = []
    for (const [key, value] of this.contentHashCache.entries()) {
      if (now - value.timestamp > this.config.deduplication.cacheTTL) {
        expired.push(key)
      }
    }
    expired.forEach(key => this.contentHashCache.delete(key))

    // Clean stale pending requests (over 10 minutes)
    const stale: string[] = []
    for (const [key, value] of this.pendingRequests.entries()) {
      if (now - value.startTime > 600000) {
        stale.push(key)
      }
    }
    stale.forEach(key => this.pendingRequests.delete(key))

    if (expired.length + stale.length > 0) {
      console.log(
        `[${this.config.type}] Cleaned ${expired.length} hash cache, ${stale.length} stale requests`,
      )
    }
  }
}

