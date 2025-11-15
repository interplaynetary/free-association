import {z} from "zod"
import * as utils from "./utils.js"
import Wire, {type WireAPI, type WireOptions} from "./wire.js"
import User, {type UserAPI, type UserIs} from "./user.js"
import SEA from "./sea.js"

// Zod schemas for Holster operations
export const holsterOptionsSchema = z.union([
  z.string(),
  z.array(z.string()),
  z.object({
    peers: z.union([z.string(), z.array(z.string())]).optional(),
    secure: z.boolean().optional(),
  }).passthrough(),
])

export type HolsterOptions = z.infer<typeof holsterOptionsSchema>

export interface HolsterContext {
  chain: Array<{item: string | null; soul: string | null}>
  cb?: (data: any) => void
  on?: boolean
  user?: UserIs
}

export interface HolsterAPI {
  get: (key: string, lex?: any, cb?: (data: any) => void, _opt?: any) => HolsterChainAPI | void
  user: () => UserAPI & HolsterAPI
  wire: WireAPI
  SEA: typeof SEA
}

export interface HolsterChainAPI extends HolsterAPI {
  next: (key: string | null, lex?: any, cb?: (data: any) => void, _opt?: any) => HolsterChainAPI | void
  put: (data: any, set?: boolean | ((data: any) => void), cb?: (data: any) => void) => void
  on: (lex?: any, cb?: (data: any) => void, _get?: boolean, _opt?: any) => void
  off: (cb?: (data: any) => void) => void
}

const Holster = (opt?: HolsterOptions): HolsterAPI => {
  if (typeof opt === "string") opt = {peers: [opt]}
  else if (opt instanceof Array) opt = {peers: opt}
  else if (!utils.obj.is(opt)) opt = {}

  const wire: WireAPI = Wire(opt as Partial<WireOptions>)
  const user: UserAPI = User(null, wire)
  // Map callbacks since the user's callback is not passed to wire.on.
  const map = new Map<Function, () => void>()
  // Allow concurrent calls to the api by storing each context.
  const allctx = new Map<string, HolsterContext>()

  const ok = (data: any): boolean => {
    return (
      data === null ||
      data === true ||
      data === false ||
      typeof data === "string" ||
      Boolean(utils.rel.is(data)) ||
      utils.num.is(data)
    )
  }

  // check returns true if data is ok to add to a graph, an error string if
  // the data can't be converted, and the keys on the data object otherwise.
  const check = (data: any): true | string | string[] => {
    if (ok(data)) return true

    if (utils.obj.is(data)) {
      const keys: string[] = []
      for (const [key, value] of Object.entries(data)) {
        if (key === "_") {
          return "error underscore cannot be used as a property name"
        }
        if (utils.obj.is(value) || ok(value)) {
          keys.push(key)
          continue
        }
        if (typeof value === "undefined") {
          return `error undefined ${key} cannot be converted to a graph`
        }
        const error = JSON.stringify({[key]: value})
        return `error ${error} cannot be converted to a graph`
      }
      if (keys.length !== 0) return keys
      return true
    }
    const error = JSON.stringify(data)
    return `error ${error} cannot be converted to a graph`
  }

  const api = (ctxid?: string): HolsterChainAPI => {
    const get = (lex: any, soul: string, ack: (data: any) => void, _opt?: any) => {
      wire.get(
        utils.obj.put(lex, "#", soul),
        async (msg: any) => {
          if (msg.err) console.log(msg.err)
          if (msg.put && msg.put[soul]) {
            delete msg.put[soul]._
            delete msg.put[soul][utils.userPublicKey]
            delete msg.put[soul][utils.userSignature]
            // Resolve any rels on the node before returning to the user.
            for (const key of Object.keys(msg.put[soul])) {
              const id = utils.rel.is(msg.put[soul][key])
              if (id) {
                // Due to message queuing, the related node might not be available yet
                // Retry with delays if we get null
                const attemptRead = async (retries = 0): Promise<any> => {
                  const data = await new Promise(res => {
                    const _ctxid = utils.text.random()
                    allctx.set(_ctxid, {chain: [{item: null, soul: id}]})
                    api(_ctxid).next(null, res, _opt)
                  })
                  if (data !== null || retries >= 5) {
                    return data
                  }
                  // Data not ready, retry after delay
                  await new Promise(resolve => setTimeout(resolve, 50))
                  return attemptRead(retries + 1)
                }
                msg.put[soul][key] = await attemptRead()
              }
            }
            ack(msg.put[soul])
          } else {
            // No data callback.
            ack(null)
          }
        },
        _opt,
      )
    }

    const graph = async (
      soul: string,
      data: any,
      userctx?: UserIs,
      cb?: (err?: string | null) => void,
    ): Promise<any> => {
      if (userctx) {
        const signed = await SEA.sign(data, userctx)
        return utils.graph(soul, signed!.m, signed!.s, userctx.pub)
      }

      if ((opt as any).secure) {
        if (!cb) cb = console.log
        cb(`error putting data on ${soul}: user required in secure mode`)
        return null
      }

      return utils.graph(soul, data)
    }

    // done takes a context id and returns a new callback function so that the
    // callback is not overwritten by simultaneous requests.
    const done = (ctxid: string) => {
      return (data: any) => {
        const ctx = allctx.get(ctxid)
        if (ctx && typeof ctx.cb !== "undefined") {
          // Use a timeout so that the context can be removed before data is
          // returned to the callback (allows nested get calls).
          setTimeout(() => ctx.cb!(data), 1)
        } else if (data) {
          console.log("error no callback for data", data, "ctx", ctx)
        }
        // A context updated by "on" should only be removed by "off".
        if (ctx && !ctx.on) allctx.delete(ctxid)
      }
    }

    const resolve = (request: any, cb?: (data: any) => void): false | {item: string | null; soul: string | null} => {
      if (!request) {
        console.log("error resolve request parameter required")
        return false
      }

      const getReq = typeof request.get !== "undefined"
      const putReq = typeof request.put !== "undefined"
      const onReq = typeof request.on !== "undefined"
      const offReq = typeof request.off !== "undefined"

      let found = false
      const ctx = allctx.get(ctxid!)!
      for (var i = 1; i < ctx.chain.length; i++) {
        if (ctx.chain[i].soul !== null) continue

        found = true
        break
      }

      if (found) {
        // Found a soul that needs resolving, need the previous context
        // (ie the parent node) to find a soul relation for it.
        const {item, soul} = ctx.chain[i - 1]
        // If this is a user context or using secure mode then need to get the
        // whole node for verification.
        const lex =
          ctx.user || (opt as any).secure ? {"#": soul} : {"#": soul, ".": item}
        wire.get(
          lex,
          async (msg: any) => {
            if (msg.err) {
              if (ctx.user || (opt as any).secure) {
                console.log(`error getting ${soul}: ${msg.err}`)
              } else {
                console.log(`error getting ${item} on ${soul}: ${msg.err}`)
              }
              if (cb) cb(null)
              return
            }

            let node = msg.put && msg.put[soul!]
            if (node && typeof node[item!] !== "undefined") {
              const relResult = utils.rel.is(node[item!])
              const id = typeof relResult === "string" ? relResult : null
              if (id) {
                ctx.chain[i].soul = id
                allctx.set(ctxid!, {...ctx})
                // Call api again using the updated context.
                if (getReq) {
                  api(ctxid).next(null, request.get, cb, request._opt)
                } else if (putReq) {
                  api(ctxid).put(request.put, cb)
                } else if (onReq) {
                  api(ctxid).on(request.on, cb, request._get, request._opt)
                } else if (offReq) {
                  api(ctxid).off(cb)
                }
              } else if (getReq) {
                // Request was not for a node, return property on current soul.
                cb!(node[item!])
              } else if (putReq) {
                // Request was chained before put, so rel doesn't exist yet.
                const newId = utils.text.random()
                node[item!] = utils.rel.ify(newId)
                const g = await graph(soul!, node, ctx.user, cb)
                if (g === null) return

                wire.put(g, err => {
                  if (err) {
                    cb!(`error putting ${item} on ${soul}: ${err}`)
                    return
                  }

                  ctx.chain[i].soul = newId
                  api(ctxid).put(request.put, cb)
                })
              } else if (onReq) {
                // Allow listening to a node that doesn't exist.
                cb!(null)
              } else if (offReq) {
                // Allow stop listening to a node that doesn't exist.
                if (cb) cb(null)
              }
            } else if (putReq) {
              // Request was chained before put, so rel doesn't exist yet.
              const id = utils.text.random()
              if (!node) node = {}
              node[item!] = utils.rel.ify(id)
              const g = await graph(soul!, node, ctx.user, cb)
              if (g === null) return

              wire.put(g, err => {
                if (err) {
                  cb!(`error putting ${item} on ${soul}: ${err}`)
                  return
                }

                ctx.chain[i].soul = id
                api(ctxid).put(request.put, cb)
              })
            } else {
              // Allow querying a node that doesn't exist.
              if (cb) cb(null)
            }
          },
          request._opt,
        )
        // Callback has been passed to next soul lookup or called above, so
        // return false as the calling code should not continue.
        return false
      }

      if (getReq && ctx.chain[ctx.chain.length - 1].item !== null) {
        // The context has been resolved but it does not include the requested
        // node, which requires one more lookup.
        ctx.chain.push({item: null, soul: null})
        api(ctxid).next(null, request.get, cb, request._opt)
        return false
      }

      // Return the last context, ie the soul required by the calling code.
      return ctx.chain[ctx.chain.length - 1]
    }

    return {
      get: (key: string, lex?: any, cb?: (data: any) => void, _opt?: any) => {
        if (typeof lex === "function") {
          _opt = cb
          cb = lex
          lex = null
        }
        if (key === null || key === "" || key === "_") {
          console.log("error please provide a key")
          if (cb) cb(null)
          return
        }

        // lex requires a callback as it's not included in the chain below.
        if (lex && typeof cb !== "function") {
          console.log("error lex requires a callback function")
          return
        }

        // Top level keys are added to a root node so their values don't need
        // to be objects.
        ctxid = utils.text.random()
        allctx.set(ctxid, {chain: [{item: key, soul: "root"}], cb: cb})
        if (!cb) return api(ctxid)

        const _done = done(ctxid)
        // When there's a callback need to resolve the context first.
        const result = resolve({get: lex, _opt: _opt}, _done)
        if (result && result.soul) get(lex, result.soul, _done, _opt)
        return
      },
      next: (key: string | null, lex?: any, cb?: (data: any) => void, _opt?: any) => {
        // ack needs to work the same as done, pass it the context id and then
        // return a new function for the actual callback.
        const ack = (ctxid: string) => {
          return (data: any) => {
            if (cb) {
              cb(data)
            } else {
              done(ctxid)(data)
            }
          }
        }
        if (typeof lex === "function") {
          _opt = cb
          cb = lex
          lex = null
        }
        if (!ctxid) {
          console.log("error please provide a key using get(key)")
          if (cb) cb(null)
          return
        }

        const _ack = ack(ctxid)
        if (key === "" || key === "_") {
          _ack(null)
          return
        }

        // lex requires a callback as it's not included in the chain below.
        if (lex && typeof cb !== "function") {
          console.log("error lex requires a callback function")
          return
        }

        const ctx = allctx.get(ctxid)
        // ctx already removed by another chained callback is ok?
        if (!ctx) return

        if (cb && typeof ctx.cb === "undefined") {
          // This (and ack) allows nested objects to set their own callbacks.
          ctx.cb = cb
          cb = undefined
        }

        // Push the key to the context as it needs a soul lookup.
        // (null is used to call the api with updated context)
        if (key !== null) ctx.chain.push({item: key, soul: null})
        if (!ctx.cb) return api(ctxid)

        // When there's a callback need to resolve the context first.
        const result = resolve({get: lex, _opt: _opt}, _ack)
        if (result && result.soul) get(lex, result.soul, _ack, _opt)
        return
      },
      put: (data: any, set?: boolean | ((data: any) => void), cb?: (data: any) => void) => {
        if (typeof set === "function") {
          cb = set
          set = false
        }
        const ack = (ctxid: string) => {
          return (data: any) => {
            if (cb) {
              cb(data)
            } else {
              done(ctxid)(data)
            }
          }
        }
        if (!ctxid) {
          if (cb) cb("error please provide a key using get(key)")
          return
        }

        const ctx = allctx.get(ctxid)
        // ctx already removed by another chained callback is ok?
        if (!ctx) return

        if (!ctx.cb) {
          if (!cb) return

          // This (and ack) allows nested objects to set their own callbacks.
          ctx.cb = cb
          cb = undefined
        }
        if (set) data = {[utils.text.random()]: data}

        const _ack = ack(ctxid)
        const result = check(data)
        if (typeof result === "string") {
          // All strings returned from check are errors, cannot continue.
          _ack(result)
          return
        }

        // Resolve the current context before putting data. (Note that set is
        // not passed to resolve because it's already been applied above.)
        const resolveResult = resolve({put: data}, _ack)
        if (!resolveResult) return
        const {item, soul} = resolveResult

        if (result === true) {
          // When result is true data is a property to put on the current soul.
          // Need to check if item is a rel and also set the node to null. (This
          // applies for any update from a rel to a property, not just null.)
          // If using secure mode need to get the whole node for verification.
          const lex =
            ctx.user || (opt as any).secure ? {"#": soul} : {"#": soul, ".": item}
          wire.get(lex, async (msg: any) => {
            if (msg.err) {
              _ack(`error getting ${soul}: ${msg.err}`)
              return
            }

            let node = msg.put && msg.put[soul!]
            const current = node && node[item!]
            const id = utils.rel.is(current)
            if (!id) {
              // Not a rel, can just put the data.
              if (!node) node = {}
              node[item!] = data
              const g = await graph(soul!, node, ctx.user, _ack)
              if (g === null) return

              wire.put(g, _ack)
              return
            }

            wire.get({"#": id}, async (msg: any) => {
              if (msg.err) {
                _ack(`error getting ${id}: ${msg.err}`)
                return
              }

              if (!msg.put || !msg.put[id]) {
                _ack(`error ${id} not found`)
                return
              }

              // null each of the properties on the node before putting data.
              for (const key of Object.keys(msg.put[id])) {
                if (
                  key === "_" ||
                  key === utils.userPublicKey ||
                  key === utils.userSignature
                ) {
                  continue
                }

                const err = await new Promise<string | null | undefined>(res => {
                  const _ctxid = utils.text.random()
                  const chain = [{item: key, soul: id}]
                  allctx.set(_ctxid, {chain: chain, user: ctx.user})
                  api(_ctxid).put(null, res)
                })
                if (err) {
                  _ack(err)
                  return
                }
              }
              if (!node) node = {}
              node[item!] = data
              const g = await graph(soul!, node, ctx.user, _ack)
              if (g === null) return

              wire.put(g, _ack)
            })
          })
          return
        }

        // Otherwise put the data using the keys returned in result.
        // Need to check if a rel has already been added on the current node.
        // If using secure mode need to get the whole node for verification.
        const lex =
          ctx.user || (opt as any).secure ? {"#": soul} : {"#": soul, ".": item}
        wire.get(lex, async (msg: any) => {
          if (msg.err) {
            _ack(`error getting ${soul}.${item}: ${msg.err}`)
            return
          }

          let node = msg.put && msg.put[soul!]
          const current = node && node[item!]
          const id = utils.rel.is(current)
          if (!id) {
            // The current rel doesn't exist, so add it first.
            if (!node) node = {}
            node[item!] = utils.rel.ify(utils.text.random())
            const g = await graph(soul!, node, ctx.user, _ack)
            if (g === null) return

            wire.put(g, err => {
              if (err) {
                _ack(`error putting ${item} on ${soul}: ${err}`)
              } else {
                const _ctxid = utils.text.random()
                const chain = [{item: item, soul: soul}]
                // Pass on the previous context's callback and user flag here.
                allctx.set(_ctxid, {chain: chain, user: ctx.user, cb: ctx.cb})
                api(_ctxid).put(data)
              }
            })
            return
          }

          const update: string[] = []
          for (const key of result as string[]) {
            const err = await new Promise<string | null | undefined>(res => {
              if (utils.obj.is(data[key]) && !utils.rel.is(data[key])) {
                // Use the current rel as the context for nested objects.
                const _ctxid = utils.text.random()
                const chain = [{item: key, soul: id}]
                allctx.set(_ctxid, {chain: chain, user: ctx.user})
                api(_ctxid).put(data[key], res)
              } else {
                // Group the rest of the updates for put below.
                update.push(key)
                res(null)
              }
            })
            if (err) {
              _ack(err)
              return
            }
          }

          if (update.length === 0) {
            _ack(null)
            return
          }

          // The nested objects created above will also have rels on the parent
          // object, so fetch the node so the rest of the updates can be added.
          wire.get({"#": id}, async (msg: any) => {
            if (msg.err) {
              _ack(`error getting ${id}: ${msg.err}`)
              return
            }

            let node = msg.put && msg.put[id]
            if (!node) node = {}
            update.forEach(key => {
              node[key] = data[key]
            })
            const g = await graph(id, node, ctx.user, _ack)
            if (g === null) return

            wire.put(g, _ack)
          })
        })
      },
      on: (lex?: any, cb?: ((data: any) => void) | boolean, _get?: boolean, _opt?: any) => {
        let actualCallback: ((data: any) => void) | undefined
        let actualGet: boolean | undefined = _get
        let actualOpt: any = _opt
        
        if (typeof lex === "function") {
          actualCallback = lex
          actualGet = typeof cb === "boolean" ? cb : undefined
          actualOpt = _get
          lex = null
        } else {
          actualCallback = typeof cb === "function" ? cb : undefined
        }
        
        if (typeof actualCallback !== "function") {
          console.log("error on() requires a callback function")
          return
        }

        if (!ctxid) {
          console.log("error please provide a key using get(key)")
          actualCallback(null)
          return
        }

        // Resolve the current context before adding event listener.
        const resolveResult = resolve({on: lex, _get: actualGet, _opt: actualOpt}, actualCallback)
        if (resolveResult === false) return
        const {item, soul} = resolveResult

        // Flag that this context is set from on and shouldn't be removed.
        allctx.set(ctxid, {chain: [{item: item, soul: soul}], on: true})
        // Map the user's callback because it can also be passed to off,
        // so need a reference to it to compare them.
        // Create a new context for each listener invocation to avoid mutation
        map.set(actualCallback, () => {
          // When listener fires, re-check if the item is now a rel.
          wire.get(
            {"#": soul!, ".": item},
            (msg: any) => {
              const current = msg.put && msg.put[soul!] && msg.put[soul!][item!]
              const relResult = utils.rel.is(current)
              const id = typeof relResult === "string" ? relResult : null
              if (id) {
                // It's a rel, read the related node. It might not be in the graph
                // yet when the listener fires, so we retry with delays.
                const attemptRead = async (retries = 0): Promise<void> => {
                  const data = await new Promise(res => {
                    const _ctxid = utils.text.random()
                    allctx.set(_ctxid, {chain: [{item: null, soul: id}]})
                    api(_ctxid).next(null, res, actualOpt)
                  })
                  if (data !== null || retries >= 5) {
                    actualCallback(data)
                  } else {
                    // Data not ready, retry after delay
                    await new Promise(resolve => setTimeout(resolve, 50))
                    attemptRead(retries + 1)
                  }
                }
                attemptRead()
              } else {
                // It's a direct property or null
                actualCallback(current !== undefined ? current : null)
              }
            },
            actualOpt,
          )
        })

        // Register listener immediately to avoid missing updates due to
        // queueing. Initially register the soul without _get, then update if
        // it's a rel.
        let initialLex: any
        if (lex) initialLex = utils.obj.put(lex, "#", soul)
        else initialLex = {"#": soul, ".": item}
        wire.on(initialLex, map.get(actualCallback)!, false, actualOpt)

        // Check if item is a rel and update listener if needed.
        // This happens async but the listener is already registered above.
        wire.get(
          {"#": soul!, ".": item},
          (msg: any) => {
            if (msg.err) {
              console.log(`error getting ${soul}.${item}: ${msg.err}`)
              return
            }

            const current = msg.put && msg.put[soul!] && msg.put[soul!][item!]
            const id = utils.rel.is(current)
            if (id) {
              // It's a rel, need to switch listener to the related node
              // First remove the initial listener
              wire.off(initialLex, map.get(actualCallback))
              // Then add listener on the related node
              let relLex: any
              if (lex) relLex = utils.obj.put(lex, "#", id)
              else relLex = {"#": id, ".": null}
              wire.on(relLex, map.get(actualCallback)!, actualGet, actualOpt)
            } else if (actualGet) {
              // Not a rel, but _get was requested, so trigger callback.
              map.get(actualCallback)!()
            }
          },
          actualOpt,
        )
      },
      off: (cb?: (data: any) => void) => {
        if (!ctxid) {
          console.log("error please provide a key using get(key)")
          if (cb) cb(null)
          return
        }

        // Resolve the current context before removing event listener.
        const resolveResult = resolve({off: true}, cb)
        if (!resolveResult) return
        const {item, soul} = resolveResult

        // Check if item is a rel and remove event listener for the node.
        wire.get({"#": soul!, ".": item}, (msg: any) => {
          if (msg.err) {
            console.log(`error getting ${soul}.${item}: ${msg.err}`)
            return
          }

          const current = msg.put && msg.put[soul!] && msg.put[soul!][item!]
          const id = utils.rel.is(current)
          if (id) wire.off({"#": id}, map.get(cb!))
          else wire.off({"#": soul}, map.get(cb!))
          map.delete(cb!)
          allctx.delete(ctxid!)
        })
      },
      user: () => {
        if (!(user as any).get) {
          // Return the combined Holster and User APIs.
          Object.assign(user, api())
          // Need to provide a user specific get() function to know if user
          // context should be checked.
          ;(user as any).get = (keys: any, lex?: any, cb?: (data: any) => void, _opt?: any) => {
            if (typeof lex === "function") {
              _opt = cb
              cb = lex
              lex = null
            }

            let pub: string | null = null
            let key: string | null = null
            if (user.is) pub = user.is.pub
            if (typeof keys === "string") {
              key = keys
            } else if (keys instanceof Array) {
              if (keys.length === 2) {
                pub = keys[0]
                key = keys[1]
              } else if (keys.length === 1) {
                key = keys[0]
              }
            }
            if (!pub) {
              console.log("error please log in or provide a public key")
              if (cb) cb(null)
              return
            }

            if (key === null || key === "" || key === "_") {
              console.log("error please provide a key")
              if (cb) cb(null)
              return
            }

            // lex requires a callback as it's not included in the chain below.
            if (lex && !cb) {
              console.log("error lex requires a callback function")
              return
            }

            ctxid = utils.text.random()
            const chain = [{item: key, soul: "~" + pub}]
            allctx.set(ctxid, {chain: chain, user: user.is!, cb: cb})
            if (!cb) return api(ctxid)

            // When there's a callback need to resolve the context first.
            const _done = done(ctxid)
            const result = resolve({get: lex, _opt: _opt}, _done)
            if (result && result.soul) get(lex, result.soul, _done, _opt)
            return
          }
        }
        return user as UserAPI & HolsterAPI
      },
      // Allow the wire spec to be used via holster.
      wire: wire,
      // Allow SEA functions to be used via holster.
      SEA: SEA,
    } as HolsterChainAPI
  }
  return api()
}

export default Holster

