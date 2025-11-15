import {z} from "zod"
import Dup, {type DupInterface} from "./dup.js"
import Get from "./get.js"
import Ham from "./ham.js"
import Store, {type StoreReturnType} from "./store.js"
import * as utils from "./utils.js"
import type {GraphType} from "./get.js"
import type {ListenerType} from "./ham.js"

const isNode = typeof document === "undefined"

// Import WebSocket dynamically for type and value
const wsModule = isNode ? await import("ws") : undefined
type WebSocketType = typeof wsModule extends {default: infer WS} ? WS : WebSocket

if (typeof globalThis.WebSocket === "undefined" && wsModule) {
  (globalThis as any).WebSocket = wsModule.WebSocket
}

// Zod schemas for Wire operations
export const wireOptionsSchema = z.object({
  peers: z.array(z.string()).optional(),
  port: z.number().optional(),
  server: z.any().optional(),
  wss: z.any().optional(),
  maxAge: z.number().optional(),
  secure: z.boolean().optional(),
  maxConnections: z.number().optional(),
  maxMessageSize: z.number().optional(),
  maxQueueLength: z.number().optional(),
  file: z.string().optional(),
  indexedDB: z.boolean().optional(),
})

export type WireOptions = z.infer<typeof wireOptionsSchema>

export const wireMessageSchema = z.object({
  "#": z.string().optional(),
  "@": z.string().optional(),
  get: z.record(z.any()).optional(),
  put: z.record(z.any()).optional(),
  err: z.string().optional(),
  throttle: z.number().optional(),
})

export type WireMessage = z.infer<typeof wireMessageSchema>

export interface WireAPI {
  get: (lex: any, cb?: (msg: any) => void, _opt?: any) => void
  put: (data: any, cb?: (err?: string | null) => void) => void
  on: (lex: any, cb: () => void, _get?: boolean, _opt?: any) => void
  off: (lex: any, cb?: () => void) => void
}

type TimeoutType = ReturnType<typeof setTimeout>

// Rate limiting with throttling
const createRateLimiter = (isTestEnv: boolean) => {
  const clients = new Map<
    string,
    {
      requests: number[]
      lastCleanup: number
      throttleCount: number
    }
  >()
  const maxRequests = 1000 // requests per minute
  const windowMs = 60000 // 1 minute window
  const disconnectThreshold = 10 // Disconnect after 10 violations
  let cleanupInterval: NodeJS.Timeout | null = null

  const cleanup = () => {
    const now = Date.now()
    for (const [clientId, data] of clients.entries()) {
      if (now - data.lastCleanup > windowMs) {
        data.requests = []
        data.lastCleanup = now
      }
      data.requests = data.requests.filter(time => now - time < windowMs)
      // Reset throttle counts periodically
      if (now - data.lastCleanup > windowMs * 10) {
        data.throttleCount = 0
      }
    }
  }

  if (!isTestEnv) {
    cleanupInterval = setInterval(cleanup, windowMs / 4)
  }

  return {
    getDelay: (clientId: string): number => {
      const now = Date.now()
      const client = clients.get(clientId) || {
        requests: [],
        lastCleanup: now,
        throttleCount: 0,
      }

      // Filter old requests
      client.requests = client.requests.filter(time => now - time < windowMs)

      if (client.requests.length >= maxRequests) {
        // Calculate delay based on oldest request that will expire
        const oldestRequest = Math.min(...client.requests)
        const delay = windowMs - (now - oldestRequest)
        // Increment throttle count and update client data
        client.throttleCount = (client.throttleCount || 0) + 1
        clients.set(clientId, client)
        return Math.max(0, delay)
      }

      // No delay needed, track this request
      client.requests.push(now)
      clients.set(clientId, client)
      return 0
    },

    getRemainingRequests: (clientId: string): number => {
      const client = clients.get(clientId)
      if (!client) return maxRequests

      const now = Date.now()
      const validRequests = client.requests.filter(time => now - time < windowMs)
      return Math.max(0, maxRequests - validRequests.length)
    },

    getThrottleCount: (clientId: string): number => {
      const client = clients.get(clientId)
      return client ? client.throttleCount || 0 : 0
    },

    shouldDisconnect: (clientId: string): boolean => {
      const client = clients.get(clientId)
      if (!client) return false
      return client.throttleCount >= disconnectThreshold
    },

    destroy: () => {
      if (cleanupInterval) {
        clearInterval(cleanupInterval)
        cleanupInterval = null
      }
      clients.clear()
    },
  }
}

// Safe JSON parser with size limit
const safeJSONParse = (
  data: string | Buffer,
  maxSize = 1024 * 1024,
): {success: boolean; data?: any; error?: string} => {
  try {
    if (typeof data === "string" && data.length > maxSize) {
      throw new Error("Message too large")
    }
    // Fast JSON parsing with size check
    const jsonString = data.toString();
    return {success: true, data: JSON.parse(jsonString)}
  } catch (error: any) {
    return {success: false, error: error.message}
  }
}

// Message size validator
const validateMessage = (
  data: string | Buffer,
  maxSize = 1024 * 1024,
): {valid: boolean; error?: string} => {
  // 1MB default
  if (typeof data === "string" && data.length > maxSize) {
    return {valid: false, error: "Message too large"}
  }
  if (Buffer.isBuffer(data) && data.length > maxSize) {
    return {valid: false, error: "Message too large"}
  }
  return {valid: true}
}

// Connection manager to limit concurrent connections
const createConnectionManager = (maxConnections = 1000) => {
  const connections = new Set<any>()

  return {
    add: (ws: any): boolean => {
      if (connections.size >= maxConnections) {
        return false
      }
      connections.add(ws)

      // Clean up on close
      const originalClose = ws.close
      ws.close = (...args: any[]) => {
        connections.delete(ws)
        return originalClose.apply(ws, args)
      }

      return true
    },

    remove: (ws: any) => {
      connections.delete(ws)
    },

    count: () => connections.size,

    isFull: () => connections.size >= maxConnections,
  }
}

// Enhanced retry mechanism with exponential backoff
const createRetryHandler = (maxRetries = 5) => {
  let retryCount = 0

  return {
    shouldRetry: () => retryCount < maxRetries,
    getDelay: () => Math.min(1000 * Math.pow(2, retryCount), 30000), // Max 30s
    increment: () => retryCount++,
    reset: () => (retryCount = 0),
  }
}

// Wire starts a websocket client or server and returns get and put methods
// for access to the wire spec and storage.
const Wire = (opt: Partial<WireOptions>): WireAPI => {
  const dup: DupInterface = Dup(opt.maxAge)
  const store: StoreReturnType = Store(opt)
  const graph: GraphType = {}
  const queue: Record<string, (msg: any) => void> = {}
  const listen: ListenerType = {}

  // Track references we want but don't have yet
  const pendingReferences = new Set<string>()

  // Track pending timeouts that should start when message is sent
  const pendingTimeouts = new Map<
    string,
    {
      lex: any
      wait: number
      timeoutId?: TimeoutType
    }
  >()

  // Helper to check if we have a soul in memory or storage
  const hasSoul = async (soul: string): Promise<boolean> => {
    if (graph[soul]) return true
    return new Promise(resolve => {
      store.get({"#": soul}, (err, data) => {
        resolve(!err && !!data && !!data[soul])
      })
    })
  }

  // Initialize rate limiting and connection management
  // Check if test environment (mock-socket usage)
  const isTestEnv = opt.wss && (opt.wss as any).constructor.name === "Server"
  const rateLimiter = createRateLimiter(isTestEnv)
  const connectionManager = createConnectionManager(opt.maxConnections || 1000)

  // The check function is required because user data must provide a public key
  // so that it can be verified. The public key might verify the provided
  // signature but not actually match the user under which the data is being
  // stored. To avoid this, the current data on a soul needs to be checked to
  // make sure the stored public key matches the one provided with the update.
  const check = async (
    data: GraphType,
    send: (msg: string) => any,
    cb?: (err?: string) => void,
  ): Promise<boolean> => {
    for (const soul of Object.keys(data)) {
      const msg = await new Promise<any>(res => {
        getWithCallback({"#": soul}, res, send)
      })
      if (msg.err) {
        if (cb) cb(msg.err)
        return false
      }

      const node = data[soul]
      const key = utils.userPublicKey
      // If there is no current node then the data is ok to write without
      // matching public keys, as the provided soul also needs a rel on the
      // parent node which then also requires checking. Otherwise publc keys
      // need to match for existing data.
      if (!msg.put || !msg.put[soul] || msg.put[soul][key] === node[key]) {
        continue
      }

      // If a soul exists but does not have a public key, then one should not be
      // added because the node is not user data. The above check fails in this
      // case if a public key is provided. Note that this is only an error case
      // if called via the API, which is when a callback is provided here.
      // (The wire spec can fetch and put data on the wire without a signature
      // or public key and this can be ignored.)
      if (cb) {
        cb(`error in wire check public key does not match for soul: ${soul}`)
      }
      return false
    }

    return true
  }

  const get = (msg: WireMessage, send: (data: string) => void) => {
    const ack = Get(msg.get!, graph)
    if (ack) {
      send(
        JSON.stringify({
          "#": dup.track(utils.text.random(9)),
          "@": msg["#"],
          put: ack,
        }),
      )
    } else {
      store.get(msg.get!, (err, ack) => {
        send(
          JSON.stringify({
            "#": dup.track(utils.text.random(9)),
            "@": msg["#"],
            put: ack,
            err: err,
          }),
        )
      })
    }
  }

  const put = async (msg: WireMessage, send: (data: string) => void) => {
    // Store updates returned from Ham.mix and defer updates if required.
    const update = await Ham.mix(msg.put!, graph, !!opt.secure, listen)
    if (Object.keys(update.now).length === 0) {
      // No updates to store, check deferred.
      if (Object.keys(update.defer).length !== 0) {
        setTimeout(
          () => put({put: update.defer, "#": msg["#"]}, send),
          update.wait,
        )
      }
      return
    }

    if (!(await check(update.now, send))) return

    store.put(update.now, err => {
      send(
        JSON.stringify({
          "#": dup.track(utils.text.random(9)),
          "@": msg["#"],
          err: err,
        }),
      )
    })

    if (Object.keys(update.defer).length !== 0) {
      setTimeout(
        () => put({put: update.defer, "#": msg["#"]}, send),
        update.wait,
      )
    }
  }

  const getWithCallback = (
    lex: any,
    cb: (msg: any) => void,
    send: (request: string) => any,
    _opt?: any,
  ) => {
    if (!cb) return

    if (!utils.obj.is(_opt)) _opt = {}

    const ack = Get(lex, graph)
    const track = utils.text.random(9)
    // Request the whole node in secure mode for verification.
    const request = JSON.stringify({
      "#": dup.track(track),
      get: opt.secure ? {"#": lex["#"]} : lex,
    })

    if (ack) {
      // Also send request on the wire to check for updates.
      const sendResult = send(request)
      if (sendResult && sendResult.err) {
        cb({err: sendResult.err})
        return
      }
      cb({put: ack})
      return
    }

    store.get(lex, (err, ack) => {
      if (ack) {
        // Also send request on the wire to check for updates.
        const sendResult = send(request)
        if (sendResult && sendResult.err) {
          cb({err: sendResult.err})
          return
        }
        cb({put: ack, err: err})
        return
      }

      if (err) console.log(err)

      queue[track] = cb

      // Store timeout config to start after message is sent from queue
      pendingTimeouts.set(track, {
        lex: lex,
        wait: _opt.wait || 100,
      })

      const sendResult = send(request)
      if (sendResult && sendResult.err) {
        cb({err: sendResult.err})
        delete queue[track]
        pendingTimeouts.delete(track)
        return
      }
    })
  }

  const api = (send: (data: string) => any): WireAPI => {
    return {
      get: (lex: any, cb?: (msg: any) => void, _opt?: any) => {
        // Mark requested soul as something we want to store
        if (lex && lex["#"]) {
          pendingReferences.add(lex["#"])
        }
        getWithCallback(lex, cb!, send, _opt)
      },
      put: async (data: any, cb?: (err?: string | null) => void) => {
        // Deferred updates are only stored using wire spec, they're ignored
        // here using the api. This is ok because correct timestamps should be
        // used whereas wire spec needs to handle clock skew for updates
        // across the network.
        const update = await Ham.mix(data, graph, !!opt.secure, listen)
        if (Object.keys(update.now).length === 0) {
          // No updates, still respond to callback.
          if (cb) cb(null)
          return
        }

        if (!(await check(update.now, send, cb))) return

        // Seed pendingReferences with any new references from API calls
        for (const [soul, node] of Object.entries(update.now)) {
          if (node && typeof node === "object") {
            for (const [key, value] of Object.entries(node)) {
              const soulId = utils.rel.is(value)
              if (soulId) {
                // Add referenced soul to pending list
                pendingReferences.add(soulId)
              }
            }
          }
        }

        store.put(update.now, cb)

        // Always put data on the wire spec even if no local update needed
        const sendResult = send(
          JSON.stringify({
            "#": dup.track(utils.text.random(9)),
            put: data,
          }),
        )
        // Handle queue overflow error.
        if (sendResult && sendResult.err) {
          if (cb) cb(sendResult.err)
          return
        }
      },
      on: (lex: any, cb: () => void, _get?: boolean, _opt?: any) => {
        const soul = lex && lex["#"]
        if (!soul || !cb) return

        if (listen[soul]) {
          listen[soul].push({".": lex["."], cb: cb})
        } else {
          listen[soul] = [{".": lex["."], cb: cb}]
        }
        if (_get) getWithCallback(lex, cb as any, send, _opt)
      },
      off: (lex: any, cb?: () => void) => {
        const soul = lex && lex["#"]
        if (!soul || !listen[soul]) return

        if (cb) {
          const found = listen[soul].find(l => l.cb === cb)
          if (found) {
            listen[soul].splice(listen[soul].indexOf(found), 1)
          }
        } else {
          // Remove all callbacks when none provided.
          delete listen[soul]
        }
      },
    }
  }

  if (isNode) {
    let wss = opt.wss
    // Node's websocket server provides clients as an array, whereas
    // mock-sockets provides clients as a function that returns an array.
    let clients = () => (wss as any).clients()
    if (!wss) {
      const config = opt.server
        ? {server: opt.server}
        : {port: opt.port || 8765}
      wss = new (wsModule!.WebSocketServer as any)(config)
      clients = () => (wss as any).clients
    }

    const send = (data: string, isBinary?: boolean): void => {
      // Start timeout for this message now that it's being sent
      const msg = JSON.parse(data)
      const trackId = msg["#"]
      if (trackId && pendingTimeouts.has(trackId)) {
        const timeoutConfig = pendingTimeouts.get(trackId)!
        pendingTimeouts.delete(trackId)

        // Respond to callback with null if no response.
        timeoutConfig.timeoutId = setTimeout(() => {
          const cb = queue[trackId]
          if (cb) {
            const id = timeoutConfig.lex["#"]
            const ack: any = {[id]: null}
            if (typeof timeoutConfig.lex["."] === "string") {
              ack[id] = {[timeoutConfig.lex["."]]: null}
            }
            cb({put: ack})
            delete queue[trackId]
          }
        }, timeoutConfig.wait)
      }

      clients().forEach((client: any) => {
        const WS = (globalThis as any).WebSocket || wsModule?.WebSocket
        if (client && client.readyState === WS.OPEN) {
          client.send(data, {binary: isBinary})
        } else {
          const retryHandler = client._retryHandler || createRetryHandler()
          client._retryHandler = retryHandler

          if (retryHandler.shouldRetry()) {
            const delay = retryHandler.getDelay()
            retryHandler.increment()

            setTimeout(() => {
              const WS = (globalThis as any).WebSocket || wsModule?.WebSocket
              if (client && client.readyState === WS.OPEN) {
                retryHandler.reset()
                client.send(data, {binary: isBinary})
              }
            }, delay)
          }
        }
      })
    }
    ;(wss as any).on("connection", (ws: any) => {
      // Check connection limit
      if (!connectionManager.add(ws)) {
        console.log("Connection limit reached, rejecting connection")
        ws.close(1013, "Connection limit reached - try again later")
        return
      }

      // Generate unique client ID for rate limiting
      const clientId = utils.text.random(9)
      console.log(`New WebSocket client connected: ${clientId}`)

      ws.on("error", (error: Error) => {
        console.log("WebSocket error:", error)
        connectionManager.remove(ws)
      })

      ws.on("close", () => {
        console.log(`WebSocket client disconnected: ${clientId}`)
        connectionManager.remove(ws)
      })

      ws.on("message", (data: string | Buffer, isBinary: boolean) => {
        // Validate message size
        const validation = validateMessage(data, opt.maxMessageSize)
        if (!validation.valid) {
          console.warn(`Invalid message: ${validation.error}`)
          ws.send(JSON.stringify({error: validation.error}))
          return
        }

        // Safe JSON parsing
        const parseResult = safeJSONParse(data, opt.maxMessageSize)
        if (!parseResult.success) {
          console.warn(`JSON parse error: ${parseResult.error}`)
          ws.send(JSON.stringify({error: "Invalid JSON"}))
          return
        }

        const msg = parseResult.data
        if (dup.check(msg["#"])) return

        // Check rate limit and get delay
        const delay = rateLimiter.getDelay(clientId)
        // Log rate limiting activity and enforce stricter limits
        if (delay > 0) {
          const throttleCount = rateLimiter.getThrottleCount(clientId)
          console.log(
            `Client ${clientId}: rate limit exceeded, delay would be ${delay}ms, dropping message (throttle count: ${throttleCount})`,
          )
          // Check if we should disconnect the bad actor
          if (rateLimiter.shouldDisconnect(clientId)) {
            console.log(
              `Client ${clientId}: Disconnecting after ${throttleCount} throttle violations`,
            )
            ws.close(1008, "Rate limit violations")
            return
          }

          // Send throttle warning back to client instead of processing
          ws.send(
            JSON.stringify({
              "#": dup.track(utils.text.random(9)),
              "@": msg["#"],
              err: `Rate limit exceeded. Slow down requests. Wait ${Math.ceil(delay / 1000)}s`,
              throttle: delay,
            }),
          )
          return
        }

        const processMessage = () => {
          dup.track(msg["#"])

          if (msg.get) get(msg, send)
          if (msg.put) put(msg, send)
          send(data.toString(), isBinary)

          const id = msg["@"]
          const cb = queue[id]
          if (cb) {
            delete msg["#"]
            delete msg["@"]
            cb(msg)

            delete queue[id]
          }
        }

        if (delay > 0) {
          // Throttle the client by delaying message processing
          setTimeout(processMessage, delay)
        } else {
          // Process immediately
          processMessage()
        }
      })
    })
    return api(send)
  }

  // Browser logic.
  const peers: any[] = []
  // Client-side throttling state
  let clientThrottled = false
  let throttleUntil = 0
  let messageQueue: string[] = []
  let queueProcessor: TimeoutType | null = null
  const maxQueueLength = opt.maxQueueLength || 10000

  const processQueue = () => {
    if (messageQueue.length === 0) {
      queueProcessor = null
      return
    }

    const now = Date.now()
    if (clientThrottled && now < throttleUntil) {
      // Still throttled, schedule next queue check
      queueProcessor = setTimeout(
        processQueue,
        Math.min(1000, throttleUntil - now),
      )
      return
    }

    // Throttle period ended, resume processing
    if (clientThrottled && now >= throttleUntil) {
      console.log(`Throttle period ended, resuming queue processing`)
      clientThrottled = false
      throttleUntil = 0
    }

    // Process next message in queue
    const data = messageQueue[0]
    const sent = sendToPeers(data)

    if (sent) {
      // Only remove from queue if successfully sent
      messageQueue.shift()

      // Start timeout for this message now that it's been sent
      const msg = JSON.parse(data)
      const trackId = msg["#"]
      if (trackId && pendingTimeouts.has(trackId)) {
        const timeoutConfig = pendingTimeouts.get(trackId)!
        pendingTimeouts.delete(trackId)

        // Respond to callback with null if no response.
        timeoutConfig.timeoutId = setTimeout(() => {
          const cb = queue[trackId]
          if (cb) {
            const id = timeoutConfig.lex["#"]
            const ack: any = {[id]: null}
            if (typeof timeoutConfig.lex["."] === "string") {
              ack[id] = {[timeoutConfig.lex["."]]: null}
            }
            cb({put: ack})
            delete queue[trackId]
          }
        }, timeoutConfig.wait)
      }
    }

    // Schedule processing of next message if queue not empty
    if (messageQueue.length > 0) {
      // Add delay between messages to respect rate limit
      queueProcessor = setTimeout(processQueue, 100)
    } else {
      queueProcessor = null
    }
  }

  const sendToPeers = (data: string): boolean => {
    let sentToAtLeastOne = false
    const WS = (globalThis as any).WebSocket
    peers.forEach(peer => {
      if (peer && peer.readyState === WS.OPEN) {
        peer.send(data)
        sentToAtLeastOne = true
      } else {
        const retryHandler = peer._retryHandler || createRetryHandler()
        peer._retryHandler = retryHandler

        if (retryHandler.shouldRetry()) {
          const delay = retryHandler.getDelay()
          retryHandler.increment()

          setTimeout(() => {
            const WS = (globalThis as any).WebSocket
            if (peer && peer.readyState === WS.OPEN) {
              retryHandler.reset()
              peer.send(data)
            }
          }, delay)
        }
      }
    })
    return sentToAtLeastOne
  }

  const send = (data: string): {err?: string; queueLength?: number; maxQueueLength?: number} | void => {
    if (messageQueue.length >= maxQueueLength) {
      return {
        err: `Message queue exceeded maximum length (${maxQueueLength}). Update query logic to request less data.`,
        queueLength: messageQueue.length,
        maxQueueLength: maxQueueLength,
      }
    }

    messageQueue.push(data)
    // Start queue processor if not already running
    if (!queueProcessor) {
      queueProcessor = setTimeout(processQueue, 100)
    }
  }

  if (!(opt.peers instanceof Array)) {
    opt.peers = [`ws://localhost:${opt.port || 8765}`]
  }
  opt.peers.forEach(peer => {
    const start = () => {
      const WS = (globalThis as any).WebSocket
      let ws = new WS(peer)
      peers.push(ws)
      const retryHandler = createRetryHandler()

      ws.onclose = () => {
        if (peers.indexOf(ws) !== -1) {
          peers.splice(peers.indexOf(ws), 1)
        }
        ws = null as any

        if (retryHandler.shouldRetry()) {
          const delay = retryHandler.getDelay()
          retryHandler.increment()
          setTimeout(start, delay)
        }
      }

      ws.onopen = () => {
        retryHandler.reset()
      }

      ws.onerror = e => {
        console.log(e)
      }
      ws.onmessage = async m => {
        const msg = JSON.parse((m as any).data)
        if (dup.check(msg["#"])) return

        // Handle throttle messages from server
        if (msg.throttle && msg.err) {
          console.log(`Server throttling: ${msg.err}`)
          clientThrottled = true
          throttleUntil = Date.now() + msg.throttle
          return
        }

        dup.track(msg["#"])
        if (msg.get) get(msg, send as any)
        if (msg.put) {
          // Handle selective storage for client WebSocket messages
          const filteredPut: GraphType = {}
          for (const [soul, node] of Object.entries(msg.put)) {
            let shouldStore = false
            // Case 1: We already have this soul - always update
            if (await hasSoul(soul)) {
              shouldStore = true
              // Check if this update adds new references
              if (node && typeof node === "object") {
                for (const [key, value] of Object.entries(node as any)) {
                  const soulId = utils.rel.is(value)
                  if (soulId) {
                    // Add referenced soul to pending list
                    pendingReferences.add(soulId)
                  }
                }
              }
            }
            // Case 2: This soul was referenced by something we have
            if (pendingReferences.has(soul)) {
              shouldStore = true
              pendingReferences.delete(soul) // Got it, remove from pending
            }
            if (shouldStore) {
              filteredPut[soul] = node as any
            }
          }
          // Store filtered data if any
          if (Object.keys(filteredPut).length > 0) {
            // Create filtered message for Ham.mix with original message ID
            const filteredMsg: WireMessage = {put: filteredPut, "#": msg["#"]}
            put(filteredMsg, send as any)
          }
        }

        const id = msg["@"]
        const cb = queue[id]
        if (cb) {
          delete msg["#"]
          delete msg["@"]
          cb(msg)

          delete queue[id]
        }
      }
    }
    start()
  })

  return api(send as any)
}

export default Wire

