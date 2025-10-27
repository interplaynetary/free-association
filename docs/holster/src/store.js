import Radisk from "./radisk.js"
import Radix from "./radix.js"
import * as utils from "./utils.js"

const isNode = typeof document === "undefined"
const fs = isNode ? await import(/*webpackIgnore: true*/ "node:fs") : undefined

// ASCII character for enquiry.
const enq = String.fromCharCode(5)
// ASCII character for unit separator.
const unit = String.fromCharCode(31)
// On-disk root node format.
const root = unit + "+0" + unit + "#" + unit + '"root' + unit

const fileSystem = opt => {
  const dir = opt.file

  if (isNode) {
    if (!fs.existsSync(dir)) fs.mkdirSync(dir)
    if (!fs.existsSync(dir + "/!")) fs.writeFileSync(dir + "/!", root)

    return {
      get: (file, cb) => {
        fs.readFile(dir + "/" + file, (err, data) => {
          if (err) {
            if (err.code === "ENOENT") {
              cb()
              return
            }

            console.log("fs.readFile error:", err)
          }
          if (data) data = data.toString()
          cb(err, data)
        })
      },
      put: (file, data, cb) => {
        // Don't put tmp files under dir so that they're not listed.
        var tmp = file + "." + utils.text.random(9) + ".tmp"
        fs.writeFile(tmp, data, err => {
          if (err) {
            console.log("fs.writeFile error:", err)
            cb(err)
            return
          }

          fs.rename(tmp, dir + "/" + file, cb)
        })
      },
      list: cb => {
        fs.readdir(dir, (err, files) => {
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
    let db
    const o = indexedDB.open(dir, 1)
    o.onupgradeneeded = event => {
      event.target.result.createObjectStore(dir)
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
          if (!req.result) {
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
      get: (file, cb) => {
        const _get = (file, cb) => {
          const tx = db.transaction([dir], "readonly")
          const req = tx.objectStore(dir).get(file)
          req.onerror = () => {
            console.log(`error getting ${dir}/${file}`)
          }
          req.onsuccess = () => {
            cb(null, req.result)
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
      put: (file, data, cb) => {
        const _put = (file, data, cb) => {
          const tx = db.transaction([dir], "readwrite")
          const req = tx.objectStore(dir).put(data, file)
          req.onerror = () => {
            console.log(`error putting data on ${dir}/${file}`)
          }
          req.onsuccess = () => {
            cb(null)
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
      list: cb => {
        const _list = cb => {
          const tx = db.transaction([dir], "readonly")
          const req = tx.objectStore(dir).getAllKeys()
          req.onerror = () => console.log("error getting keys for", dir)
          req.onsuccess = () => {
            req.result.forEach(cb)
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
    get: (file, cb) => {
      cb(null, root)
    },
    put: (file, data, cb) => {
      cb(null)
    },
    list: cb => {
      cb("!")
      cb()
    },
  }
}

// Store provides get and put methods that can access radisk.
const Store = opt => {
  if (!utils.obj.is(opt)) opt = {}
  opt.file = String(opt.file || "radata")
  if (!opt.store) opt.store = fileSystem(opt)
  const radisk = Radisk(opt)

  return {
    get: (lex, cb) => {
      if (!lex || !utils.obj.is(lex)) {
        cb("lex required")
        return
      }
      if (!lex["#"]) {
        cb("soul required in lex")
        return
      }

      var soul = lex["#"]
      var key = typeof lex["."] === "string" ? lex["."] : ""
      var node
      const each = (value, key) => {
        if (!utils.match(lex["."], key)) return

        if (!node) node = {_: {"#": soul, ">": {}}}
        node[key] = value[0]
        node._[">"][key] = value[1]
      }

      radisk(soul + enq + key, (err, value) => {
        let graph
        if (utils.obj.is(value)) {
          Radix.map(value, each)
          if (!node) each(value, key)
          graph = {[soul]: node}
        } else if (value) {
          each(value, key)
          graph = {[soul]: node}
        }
        cb(err, graph)
      })
    },
    put: (graph, cb) => {
      if (!graph) {
        cb("graph required")
        return
      }

      let count = 0
      let finished = false
      const ack = err => {
        count--
        if (finished) return

        if (err) {
          finished = true
          cb(err)
          return
        }

        if (count === 0) {
          finished = true
          cb(null)
        }
      }

      Object.keys(graph).forEach(soul => {
        var node = graph[soul]
        Object.keys(node).forEach(key => {
          if (key === "_") return

          count++
          let value = node[key]
          let state = node._[">"][key]
          radisk(soul + enq + key, [value, state], ack)
        })
      })
    },
  }
}

export default Store
