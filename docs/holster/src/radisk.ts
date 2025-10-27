import {z} from "zod"
import Radix, {type RadixFunction} from "./radix.js"
import * as utils from "./utils.js"

// ASCII character for end of text
const etx = String.fromCharCode(3)
// ASCII character for enquiry
const enq = String.fromCharCode(5)
// ASCII character for unit separator
const unit = String.fromCharCode(31)

// Zod schemas for Radisk operations
export const radiskOptionsSchema = z.object({
  log: z.function().args(z.any()).returns(z.void()).optional(),
  batch: z.number().optional(),
  write: z.number().optional(),
  size: z.number().optional(),
  memoryLimit: z.number().optional(),
  cache: z.boolean().optional(),
  maxMessageSize: z.number().optional(),
  store: z
    .object({
      get: z.function().args(z.string(), z.function()).returns(z.void()),
      put: z.function().args(z.string(), z.string(), z.function()).returns(z.void()),
      list: z.function().args(z.function()).returns(z.void()),
    })
    .optional(),
})

export type RadiskOptions = z.infer<typeof radiskOptionsSchema>

export interface StoreInterface {
  get: (file: string, cb: (err?: string, data?: string) => void) => void
  put: (file: string, data: string, cb: (err?: string) => void) => void
  list: (cb: (file?: string) => void) => void
}

export interface RadiskFunction {
  (key: string, value: any, cb?: (err?: string) => void): void
  (key: string, cb: (err?: string, value?: any) => void): void
  batch: RadixFunction & {
    acks: Array<(err?: string) => void>
    ed: number
    timeout?: ReturnType<typeof setTimeout>
  }
  thrash: {
    (): void
    ing?: boolean
    more?: boolean
    at?: RadixFunction
  }
  save: (rad: RadixFunction, cb: (err?: string) => void) => void
  write: (file: string, rad: RadixFunction, cb: (err?: string) => void) => void
  read: (key: string, cb: (err?: string, value?: any) => void) => void
  parse: (file: string, cb: (err?: string, disk?: RadixFunction) => void) => void
}

// Radisk provides access to a radix tree that is stored in the provided
// opt.store interface
const Radisk = (opt?: Partial<RadiskOptions>): RadiskFunction | undefined => {
  let u: undefined

  // Multi-file cache for all parsed radix trees
  const cache = new Map<string, RadixFunction>()
  // File listing cache to avoid repeated directory scans
  let fileListCache: string[] | null = null
  let fileListCacheTime = 0
  const FILE_LIST_CACHE_TTL = 10000 // 10 seconds
  // Pending reads queue to avoid duplicate reads for the same key
  const pendingReads = new Map<string, Array<(err?: string, value?: any) => void>>()

  // Memory monitoring and cleanup
  let lastMemoryCheck = 0
  const MEMORY_CHECK_INTERVAL = 30000 // Check every 30 seconds
  const HEAP_WARNING_THRESHOLD = 0.8 // Warn at 80% of max heap size
  const HEAP_CLEANUP_THRESHOLD = 0.9 // Clean cache at 90% of max heap size

  if (!opt) opt = {}
  if (!opt.log) opt.log = console.log
  if (!opt.batch) opt.batch = 100 // Balanced batch size for performance
  if (!opt.write) opt.write = 1 // Wait time before write in milliseconds
  if (!opt.size) opt.size = 1024 * 1024 // File size on disk, default 1MB
  if (!opt.memoryLimit) opt.memoryLimit = 500 // Memory limit in MB
  if (typeof opt.cache === "undefined") opt.cache = true

  if (!opt.store) {
    opt.log!(
      "Radisk needs `store` interface with `{get: fn, put: fn, list: fn}`",
    )
    return
  }
  if (!opt.store.get) {
    opt.log!("Radisk needs `store.get` interface with `(file, cb)`")
    return
  }
  if (!opt.store.put) {
    opt.log!("Radisk needs `store.put` interface with `(file, data, cb)`")
    return
  }
  if (!opt.store.list) {
    opt.log!("Radisk needs a streaming `store.list` interface with `(cb)`")
    return
  }

  // Performance logging
  const perfLog = (operation: string, startTime: number, key: string, size?: number): void => {
    const duration = Date.now() - startTime
    if (duration > 2000) {
      console.log(
        `[RADISK-SLOW] ${operation}: ${duration}ms for ${key}${size ? ` (${size} bytes)` : ""}`,
      )
    }
  }

  // Memory monitoring and cache cleanup
  const checkMemoryUsage = (): void => {
    // Check if we're in Node.js environment with process.memoryUsage available
    const nodeProcess = typeof process !== "undefined" ? process : undefined
    if (!nodeProcess || typeof nodeProcess.memoryUsage !== "function") return

    const now = Date.now()
    if (now - lastMemoryCheck < MEMORY_CHECK_INTERVAL) return
    lastMemoryCheck = now

    const memUsage = nodeProcess.memoryUsage()
    const heapUsed = memUsage.heapUsed
    const maxHeapSize = opt.memoryLimit! * 1024 * 1024
    const heapUsageRatio = heapUsed / maxHeapSize

    if (heapUsageRatio > HEAP_CLEANUP_THRESHOLD) {
      const cacheSize = cache.size
      console.log(
        `[RADISK-MEMORY] High memory usage: ${Math.round(heapUsageRatio * 100)}% of heap limit. Clearing ${cacheSize} cached files.`,
      )
      cache.clear()
      // Force garbage collection if available
      const globalWithGc = globalThis as typeof globalThis & {gc?: () => void}
      if (globalWithGc.gc) {
        globalWithGc.gc()
      }
      // Reset timer to avoid immediate re-check until GC has time to work
      lastMemoryCheck = now + MEMORY_CHECK_INTERVAL
    } else if (heapUsageRatio > HEAP_WARNING_THRESHOLD) {
      console.log(
        `[RADISK-MEMORY] Memory warning: ${Math.round(heapUsageRatio * 100)}% of heap limit used. Cache size: ${cache.size} files.`,
      )
    }
  }

  // Any and all storage adapters should:
  // 1. Because writing to disk takes time, we should batch data to disk.
  //    This improves performance, and reduces potential disk corruption.
  // 2. If a batch exceeds a certain number of writes, we should immediately
  //    write to disk when physically possible. This caps total performance,
  //    but reduces potential loss.
  const radisk: RadiskFunction = ((key: string, value?: any, cb?: any) => {
    key = "" + key

    // If no value is provided then the second parameter is the callback
    // function. Read value from memory or disk and call callback with it.
    if (typeof value === "function") {
      cb = value
      value = radisk.batch(key)
      if (typeof value !== "undefined") {
        return cb(u, value)
      }

      if (radisk.thrash.at) {
        value = radisk.thrash.at(key)
        if (typeof value !== "undefined") {
          return cb(u, value)
        }
      }

      // Check if there's already a pending read for this key
      if (pendingReads.has(key)) {
        // Add callback to existing pending read
        pendingReads.get(key)!.push(cb)
        return
      }

      // Start new read and create callback queue
      pendingReads.set(key, [cb])

      const readStart = Date.now()
      return radisk.read(key, (err, result) => {
        perfLog("read", readStart, key)
        // Execute all pending callbacks for this key
        const callbacks = pendingReads.get(key) || []
        pendingReads.delete(key)
        callbacks.forEach(callback => callback(err, result))
      })
    }

    // Otherwise store the value provided.
    radisk.batch(key, value)
    // Check memory usage after writing to in-memory radix tree
    checkMemoryUsage()
    if (cb) {
      radisk.batch.acks.push(cb)
    }
    // Don't wait if we have batched too many.
    if (++radisk.batch.ed >= opt.batch!) {
      return radisk.thrash()
    }

    // Otherwise wait for more updates before writing.
    clearTimeout(radisk.batch.timeout)
    radisk.batch.timeout = setTimeout(radisk.thrash, opt.write!)
  }) as RadiskFunction

  radisk.batch = Radix() as RadixFunction & {
    acks: Array<(err?: string) => void>
    ed: number
    timeout?: ReturnType<typeof setTimeout>
  }
  radisk.batch.acks = []
  radisk.batch.ed = 0

  radisk.thrash = (() => {
    if (radisk.thrash.ing) {
      radisk.thrash.more = true
      return
    }

    const thrashStart = Date.now()
    clearTimeout(radisk.batch.timeout)
    radisk.thrash.more = false
    radisk.thrash.ing = true
    const batch = (radisk.thrash.at = radisk.batch)
    radisk.batch = Radix() as RadixFunction & {
      acks: Array<(err?: string) => void>
      ed: number
      timeout?: ReturnType<typeof setTimeout>
    }
    radisk.batch.acks = []
    radisk.batch.ed = 0
    let i = 0
    radisk.save(batch, err => {
      // This is to ignore multiple callbacks from radisk.save calling
      // radisk.write? It looks like multiple callbacks will be made if a
      // file needs to be split.
      if (++i > 1) return

      perfLog("thrash", thrashStart, `batch-${batch.ed}`)
      if (err) opt.log!(err)
      batch.acks.forEach(cb => cb(err))
      radisk.thrash.at = undefined
      radisk.thrash.ing = false
      if (radisk.thrash.more) radisk.thrash()
    })
  }) as RadiskFunction["thrash"]

  // 1. Find the first radix item in memory
  // 2. Use that as the starting index in the directory of files
  // 3. Find the first file that is lexically larger than it
  // 4. Read the previous file into memory
  // 5. Scan through in memory radix for all values lexically less than limit
  // 6. Merge and write all of those to the in-memory file and back to disk
  radisk.save = (rad: RadixFunction, cb: (err?: string) => void) => {
    const save: {
      find: (tree: any, key: string) => boolean | undefined
      lex: (file?: string) => void
      mix: (file: string, start: string, end?: string) => void
      next: (err?: string) => void
      start?: string
      end?: string
      file?: string
    } = {
      find: (tree, key) => {
        // This is false for any key until save.start is set to an initial key.
        if (key < (save.start || "")) return

        save.start = key
        opt.store!.list(save.lex)
        return true
      },
      lex: file => {
        if (!file || (save.start && file > save.start)) {
          save.end = file
          // ! is used as the first file name as it's the first printable
          // character, so always matches as lexically less than any node.
          // Also save.start can be set to undefined by a previous call to
          // save.mix, so don't continue in this case.
          if (save.start) save.mix(save.file || "!", save.start, save.end)
        } else {
          save.file = file
        }
      },
      mix: (file, start, end) => {
        save.start = save.end = save.file = u
        // Use cache if available, otherwise parse from disk
        if (cache.has(file)) {
          const disk = cache.get(file)!
          Radix.map(rad, (value, key) => {
            if (key < start) return

            if (end && end < key) {
              save.start = key
              return
            }

            disk(key, value)
          })
          radisk.write(file, disk, save.next)
        } else {
          radisk.parse(file, (err, disk) => {
            if (err) return cb(err)

            Radix.map(rad, (value, key) => {
              if (key < start) return

              if (end && end < key) {
                save.start = key
                return
              }

              disk!(key, value)
            })
            radisk.write(file, disk!, save.next)
          })
        }
      },
      next: err => {
        if (err) return cb(err)

        if (save.start) return Radix.map(rad, save.find)

        cb(err)
      },
    }
    Radix.map(rad, save.find)
  }

  radisk.write = (file: string, rad: RadixFunction, cb: (err?: string) => void) => {
    const write: {
      text: string
      limit: string
      done: boolean
      count: number
      sub?: RadixFunction
      each: (value: any, key: string, k: string, pre: string[]) => void
      slice: (value: any, key: string) => void
    } = {
      text: "",
      limit: "",
      done: false,
      count: 0,
      each: (value, key, k, pre) => {
        // each is called for all keys, but stop adding to write.text when
        // write.slice is called, then the current contents of write.text
        // will be written to file.
        if (write.done) return

        write.count++
        value = typeof value === "undefined" ? "" : "=" + Radisk.encode(value)
        const enc =
          Radisk.encode(pre.length) + "#" + Radisk.encode(k) + value + "\n"
        // Cannot split the file if only have one entry to write. Also don't
        // start a split if in the middle of writing a node (pre > 0).
        if (
          write.count > 1 &&
          pre.length === 0 &&
          write.text.length + enc.length > opt.size!
        ) {
          const end = k.indexOf(enq)
          write.limit = end === -1 ? k : k.substring(0, end)
          // Cannot split if they key is the same as the current file name.
          if (write.limit !== file) {
            write.done = true
            write.sub = Radix()
            Radix.map(rad, write.slice)
            radisk.write(write.limit, write.sub, cb)
            return
          }
        }

        write.text += enc
      },
      slice: (value, key) => {
        if (key < write.limit) return

        write.sub!(key, value)
      },
    }
    Radix.map(rad, write.each, true)
    // There is always accumulated write.text to store once write.each has
    // finished.
    const writeStart = Date.now()
    opt.store!.put(file, write.text, err => {
      perfLog("file-write", writeStart, file, write.text.length)
      cb(err)
    })
  }

  radisk.read = (key: string, cb: (err?: string, value?: any) => void) => {
    // Only the soul of the key is compared to filenames (see radisk.write).
    const end = key.indexOf(enq)
    const soul = end === -1 ? key : key.substring(0, end)

    const read: {
      lex: (file?: string) => void
      it: (err?: string, disk?: RadixFunction) => void
      file?: string
      value?: any
    } = {
      lex: file => {
        // store.list should call lex without a file last, which means all file
        // names were compared to soul, so the current read.file is ok to use.
        if (!file) {
          if (!read.file) {
            cb("no file found", u)
            return
          }

          // Check multi-file cache first
          if (opt.cache && cache.has(read.file)) {
            const cachedRadix = cache.get(read.file)!
            read.value = cachedRadix(key)
            // Return cached result (defined or undefined) since in-memory
            // radix tree is authoritative
            return cb(u, read.value)
          }

          radisk.parse(read.file, read.it)
          return
        }

        // Want the filename closest to soul.
        if (file > soul || (read.file && file < read.file)) return

        read.file = file
      },
      it: (err, disk) => {
        if (err) opt.log!(err)
        if (disk) {
          if (opt.cache) {
            cache.set(read.file!, disk)
            checkMemoryUsage() // Check memory usage after adding to cache
          }
          read.value = disk(key)
        }
        cb(err, read.value)
      },
    }
    const now = Date.now()
    if (
      opt.cache &&
      fileListCache &&
      now - fileListCacheTime < FILE_LIST_CACHE_TTL
    ) {
      // Use cached file list
      fileListCache.forEach(file => read.lex(file))
      read.lex() // Signal end of list
    } else {
      // Refresh cache
      const files: string[] = []
      const originalLex = read.lex
      read.lex = file => {
        if (file) files.push(file)
        return originalLex(file)
      }

      opt.store!.list(file => {
        read.lex(file)
        if (!file && opt.cache) {
          // End of list - cache the results
          fileListCache = files
          fileListCacheTime = now
        }
      })
    }
  }

  // Let us start by assuming we are the only process that is
  // changing the directory or bucket. Not because we do not want
  // to be multi-process/machine, but because we want to experiment
  // with how much performance and scale we can get out of only one.
  // Then we can work on the harder problem of being multi-process.
  radisk.parse = (file: string, cb: (err?: string, disk?: RadixFunction) => void) => {
    const parse: {
      disk: RadixFunction
      read: (err?: string, data?: string) => void
      split: (data: string) => [string, any, string] | undefined
    } = {
      disk: Radix(),
      read: (err, data) => {
        if (err) return cb(err)

        if (!data) return cb(u, parse.disk)

        let pre: string[] = []
        let preString = ""
        // Work though data by splitting into 3 values. The first value says
        // if the second value is one of: the radix level for a key, the key
        // iteself, or a value. The third is the rest of the data to work with.
        let tmp: [string, any, string] | undefined = parse.split(data)
        while (tmp) {
          let key: string | undefined
          let value: any
          let i = tmp[1]
          tmp = parse.split(tmp[2])
          if (tmp && tmp[0] === "#") {
            key = tmp[1]
            // Optimize prefix building - avoid repeated joins
            if (typeof key === "string") {
              if (i < pre.length) {
                pre.length = i
              }
              if (i <= pre.length) {
                pre[i] = key
                pre.length = i + 1
              }
              // Only rebuild preString when prefix changes
              preString = pre.join("")
            }
          }
          tmp = parse.split((tmp as any)[2])
          if (tmp && tmp[0] === "\n") continue

          if (tmp && tmp[0] === "=") value = tmp[1]
          if (typeof key !== "undefined" && typeof value !== "undefined") {
            parse.disk(preString, value)
          }
          tmp = tmp && parse.split(tmp[2])
        }
        cb(u, parse.disk)
      },
      split: data => {
        if (!data) return

        const i = data.indexOf(unit)
        if (i === -1) return

        const a = data.slice(0, i)
        const o: {i?: number} = {}
        return [a, Radisk.decode(data.slice(i), o), data.slice(i + o.i!)]
      },
    }
    opt.store!.get(file, parse.read)
  }

  return radisk
}

Radisk.encode = (data: any): string => {
  // A key should be passed in as a string to encode, a value can optionally be
  // an array of 2 items to include the value's state, as is done by store.js.
  let state = ""
  if (data instanceof Array && data.length === 2) {
    state = etx + data[1]
    data = data[0]
  }

  if (typeof data === "string") {
    let i = 0
    let current = null
    let text = unit
    while ((current = data[i++])) {
      if (current === unit) text += unit
    }
    return text + '"' + data + state + unit
  }

  const rel = utils.rel.is(data)
  if (rel) return unit + "#" + rel + state + unit

  if (utils.num.is(data)) return unit + "+" + (data || 0) + state + unit

  if (data === true) return unit + "+" + state + unit

  if (data === false) return unit + "-" + state + unit

  if (data === null) return unit + " " + state + unit

  return ""
}

Radisk.decode = (data: string, obj?: {i?: number}): any => {
  let i = -1
  let n = 0
  let current: string | null = null
  let previous: string | true | null = null
  let textStart = -1
  let textEnd = -1
  if (data[0] !== unit) return

  // Find a control character previous to the text we want, skipping
  // consecutive unit separator characters at the beginning of the data.
  while ((current = data[++i])) {
    if (previous) {
      if (textStart === -1) textStart = i
      if (current === unit) {
        if (--n <= 0) {
          textEnd = i
          break
        }
      }
    } else if (current === unit) {
      n++
    } else {
      previous = current || true
    }
  }

  const text =
    textStart !== -1 ? data.slice(textStart, textEnd !== -1 ? textEnd : i) : ""

  if (obj) obj.i = i + 1

  const [value, stateStr] = text.split(etx)
  const state = stateStr ? parseFloat(stateStr) : undefined
  
  if (!state) {
    if (previous === '"') return text

    if (previous === "#") return utils.rel.ify(text)

    if (previous === "+") {
      if (text.length === 0) return true

      return parseFloat(text)
    }

    if (previous === "-") return false

    if (previous === " ") return null
  } else {
    // If state was found then return an array.
    if (previous === '"') return [value, state]

    if (previous === "#") return [utils.rel.ify(value), state]

    if (previous === "+") {
      if (value.length === 0) return [true, state]

      return [parseFloat(value), state]
    }

    if (previous === "-") return [false, state]

    if (previous === " ") return [null, state]
  }
}

export default Radisk

