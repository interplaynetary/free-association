import {z} from "zod"
import Radisk, {type StoreInterface, type RadiskFunction} from "./radisk.js"
import Radix from "./radix.js"
import * as utils from "./utils.js"
import type {GraphType} from "./get.js"

const isNode = typeof document === "undefined"
const fs = isNode ? await import(/*webpackIgnore: true*/ "node:fs") : undefined

// ASCII character for enquiry.
const enq = String.fromCharCode(5)
// ASCII character for unit separator.
const unit = String.fromCharCode(31)
// On-disk root node format.
const root = unit + "+0" + unit + "#" + unit + '"root' + unit

// Zod schemas for Store operations
export const storeOptionsSchema = z.object({
  file: z.string().optional(),
  indexedDB: z.boolean().optional(),
  store: z
    .object({
      get: z.function(),
      put: z.function(),
      list: z.function(),
    })
    .optional(),
})

export const lexStoreSchema = z.object({
  "#": z.string(),
  ".": z.union([z.string(), z.record(z.string(), z.any())]).optional(),
})

export type StoreOptions = z.infer<typeof storeOptionsSchema>
export type LexStore = z.infer<typeof lexStoreSchema>

const fileSystem = (opt: {file: string; indexedDB?: boolean}): StoreInterface => {
  const dir = opt.file

  if (isNode) {
    if (!fs!.existsSync(dir)) fs!.mkdirSync(dir)
    if (!fs!.existsSync(dir + "/!")) fs!.writeFileSync(dir + "/!", root)

    return {
      get: (file: string, cb: (err?: string, data?: string) => void) => {
        fs!.readFile(dir + "/" + file, (err: NodeJS.ErrnoException | null, data: Buffer) => {
          if (err) {
            if (err.code === "ENOENT") {
              cb()
              return
            }

            console.log("fs.readFile error:", err)
            cb(err.message)
            return
          }
          const dataStr = data ? data.toString() : undefined
          cb(undefined, dataStr)
        })
      },
      put: (file: string, data: string, cb: (err?: string) => void) => {
        // Don't put tmp files under dir so that they're not listed.
        const tmp = file + "." + utils.text.random(9) + ".tmp"
        fs!.writeFile(tmp, data, (err: NodeJS.ErrnoException | null) => {
          if (err) {
            console.log("fs.writeFile error:", err)
            cb(err.message)
            return
          }

          fs!.rename(tmp, dir + "/" + file, (err: NodeJS.ErrnoException | null) => cb(err ? err.message : undefined))
        })
      },
      list: (cb: (file?: string) => void) => {
        fs!.readdir(dir, (err: NodeJS.ErrnoException | null, files: string[]) => {
          if (err) {
            console.log("fs.readdir error:", err)
            cb()
            return
          }

          files.forEach(cb)
          cb()
        })
      },
    }
  }

  if (opt.indexedDB) {
    let db: IDBDatabase | null = null
    const o = indexedDB.open(dir, 1)
    o.onupgradeneeded = event => {
      (event.target as IDBOpenDBRequest).result.createObjectStore(dir)
    }
    o.onerror = event => {
      console.log(event)
    }
    o.onsuccess = () => {
      db = o.result
      // Create the root node if it doesn't exist.
      if (db) {
        const tx = db.transaction([dir], "readonly")
        const req = tx.objectStore(dir).getKey("!")
        req.onerror = () => {
          console.log(`error getting key ${dir}/!`)
        }
        req.onsuccess = () => {
          if (!req.result && db) {
            const tx = db.transaction([dir], "readwrite")
            const req = tx.objectStore(dir).put(root, "!")
            req.onerror = () => {
              console.log(`error putting root on ${dir}/!`)
            }
          }
        }
      } else {
        console.log("error indexedDB not available")
      }
    }

    return {
      get: (file: string, cb: (err?: string, data?: string) => void) => {
        const _get = (file: string, cb: (err?: string, data?: string) => void) => {
          if (!db) return
          const tx = db.transaction([dir], "readonly")
          const req = tx.objectStore(dir).get(file)
          req.onerror = () => {
            console.log(`error getting ${dir}/${file}`)
          }
          req.onsuccess = () => {
            cb(undefined, req.result)
          }
        }
        if (db) {
          _get(file, cb)
          return
        }

        let retry = 0
        const interval = setInterval(() => {
          if (db) {
            clearInterval(interval)
            _get(file, cb)
            return
          }

          if (retry++ > 5) {
            clearInterval(interval)
            cb("error indexedDB not available")
          }
        }, 1000)
      },
      put: (file: string, data: string, cb: (err?: string) => void) => {
        const _put = (file: string, data: string, cb: (err?: string) => void) => {
          if (!db) return
          const tx = db.transaction([dir], "readwrite")
          const req = tx.objectStore(dir).put(data, file)
          req.onerror = () => {
            console.log(`error putting data on ${dir}/${file}`)
          }
          req.onsuccess = () => {
            cb(undefined)
          }
        }
        if (db) {
          _put(file, data, cb)
          return
        }

        let retry = 0
        const interval = setInterval(() => {
          if (db) {
            clearInterval(interval)
            _put(file, data, cb)
            return
          }

          if (retry++ > 5) {
            clearInterval(interval)
            cb("error indexedDB not available")
          }
        }, 1000)
      },
      list: (cb: (file?: string) => void) => {
        const _list = (cb: (file?: string) => void) => {
          if (!db) return
          const tx = db.transaction([dir], "readonly")
          const req = tx.objectStore(dir).getAllKeys()
          req.onerror = () => console.log("error getting keys for", dir)
          req.onsuccess = () => {
            req.result.forEach(key => cb(String(key)))
            cb()
          }
        }
        if (db) {
          _list(cb)
          return
        }

        let retry = 0
        const interval = setInterval(() => {
          if (db) {
            clearInterval(interval)
            _list(cb)
            return
          }

          if (retry++ > 5) {
            clearInterval(interval)
            console.log("error indexedDB not available")
            cb()
          }
        }, 1000)
      },
    }
  }

  // No browser storage.
  return {
    get: (_file: string, cb: (err?: string, data?: string) => void) => {
      cb(undefined, root)
    },
    put: (_file: string, _data: string, cb: (err?: string) => void) => {
      cb(undefined)
    },
    list: (cb: (file?: string) => void) => {
      cb("!")
      cb()
    },
  }
}

export interface StoreReturnType {
  get: (lex: LexStore, cb: (err?: string, graph?: GraphType) => void) => void
  put: (graph: GraphType, cb?: (err?: string) => void) => void
}

// Store provides get and put methods that can access radisk.
const Store = (opt?: Partial<StoreOptions>): StoreReturnType => {
  if (!utils.obj.is(opt)) opt = {}
  opt.file = String(opt.file || "radata")
  if (!opt.store) opt.store = fileSystem(opt as {file: string})
  const radisk = Radisk(opt) as RadiskFunction

  return {
    get: (lex: LexStore, cb: (err?: string, graph?: GraphType) => void) => {
      if (!lex || !utils.obj.is(lex)) {
        cb("lex required")
        return
      }
      if (!lex["#"]) {
        cb("soul required in lex")
        return
      }

      const soul = lex["#"]
      const key = typeof lex["."] === "string" ? lex["."] : ""
      let node: any
      const each = (value: [any, number], key: string) => {
        if (!utils.match(lex["."], key)) return

        if (!node) node = {_: {"#": soul, ">": {}}}
        node[key] = value[0]
        node._[">"][key] = value[1]
      }

      radisk(soul + enq + key, (err: string | undefined, value: any) => {
        let graph: GraphType | undefined
        if (utils.obj.is(value)) {
          // value is a radix tree object, map over it
          Radix.map(value, each)
          graph = node ? {[soul]: node} : undefined
        } else if (value) {
          // value is a tuple [val, state]
          each(value, key)
          graph = {[soul]: node}
        }
        cb(err, graph)
      })
    },
    put: (graph: GraphType, cb?: (err?: string) => void) => {
      if (!graph) {
        cb?.("graph required")
        return
      }

      let count = 0
      let finished = false
      const ack = (err?: string) => {
        count--
        if (finished) return

        if (err) {
          finished = true
          cb?.(err)
          return
        }

        if (count === 0) {
          finished = true
          cb?.()
        }
      }

      Object.keys(graph).forEach(soul => {
        const node = graph[soul]
        Object.keys(node).forEach(key => {
          if (key === "_") return

          count++
          const value = node[key]
          const state = node._[">"][key]
          radisk(soul + enq + key, [value, state] as [any, number], ack)
        })
      })
    },
  }
}

export default Store

