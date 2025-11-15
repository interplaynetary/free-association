import * as utils from "../utils.js"
import SEA from "../sea.js"

const LISTENER_DELAY = 100
// Maximum number of souls to keep in memory
const MAX_GRAPH_SIZE = 10000

// state and value are the incoming changes.
// currentState and currentValue are the current graph data.
const Ham = (state, currentState, value, currentValue) => {
  if (state < currentState) return {historical: true}

  if (state > currentState) return {incoming: true}

  // state is equal to currentState, lexically compare to resolve conflict.
  if (typeof value !== "string") {
    value = JSON.stringify(value) || ""
  }
  if (typeof currentValue !== "string") {
    currentValue = JSON.stringify(currentValue) || ""
  }
  // No update required.
  if (value === currentValue) return {state: true}

  // Keep the current value.
  if (value < currentValue) return {current: true}

  // Otherwise update using the incoming value.
  return {incoming: true}
}

Ham.mix = async (change, graph, secure, listen) => {
  if (!change || typeof change !== "object") {
    throw new TypeError("change must be an object")
  }
  if (!graph || typeof graph !== "object") {
    throw new TypeError("graph must be an object")
  }
  if (!listen || typeof listen !== "object") {
    throw new TypeError("listen must be an object")
  }

  const machine = Date.now()
  const now = {}
  const defer = {}
  let wait = 0

  for (const soul of Object.keys(change)) {
    const node = change[soul]
    let updated = false
    let alias = false
    let nodeWait = 0
    let verify = secure

    if (!node || !node._) continue

    const sig = node[utils.userSignature]
    const pub = node[utils.userPublicKey]
    // If a signature and public key are provided then always verify.
    if (sig && pub) verify = true

    // Special case if soul starts with "~". Node must be system data ie,
    // ~@alias or ~publickey. For aliases, key and value must be a self
    // identifying rel. For public keys, data needs to be signed and verified.
    // (This is also true for any data when the secure flag is used.)
    if (soul.length > 1 && soul[0] === "~") {
      if (soul[1] === "@") {
        alias = true
        verify = false
      } else {
        if (pub && soul != "~" + pub) {
          console.log(`error public key does not match for soul: ${soul}`)
          continue
        }

        verify = true
      }
    }
    if (verify) {
      // Partial nodes can be read from disk, which are ignored. This could be
      // a missing signature or public key, or missing properties on the node
      // which means verify won't work either.
      if (!sig || !pub || !(await SEA.verify({m: node, s: sig}, {pub: pub}))) {
        continue
      }
    }

    for (const key of Object.keys(node)) {
      if (key === "_") continue

      const value = node[key]
      const state = node._ && node._[">"] ? node._[">"][key] : 0
      const currentValue = (graph[soul] || {})[key]
      const currentState = (graph[soul] || {_: {">": {}}})._[">"][key] || 0

      if (alias && key !== utils.rel.is(value)) {
        console.log(`error alias ${alias}: ${key} !== ${utils.rel.is(value)}`)
        continue
      }

      // Defer the update if ahead of machine time.
      const skew = state - machine
      if (skew > 0) {
        // Ignore update if ahead by more than 24 hours.
        if (skew > 86400000) continue

        // Wait the shortest difference before trying the updates again.
        if (wait === 0 || skew < wait) wait = nodeWait = skew
        if (!defer[soul]) {
          defer[soul] = {_: {"#": soul, ">": {}}}
        }
        defer[soul][key] = value
        defer[soul]._[">"][key] = state
      } else {
        const result = Ham(state, currentState, value, currentValue)
        if (result.incoming) {
          if (!now[soul]) {
            now[soul] = {_: {"#": soul, ">": {}}}
          }
          if (!graph[soul]) {
            graph[soul] = {_: {"#": soul, ">": {}}}
          }
          graph[soul][key] = now[soul][key] = value
          graph[soul]._[">"][key] = now[soul]._[">"][key] = state
          // Call event listeners for update on key, mix is called before
          // put has finished so wait for what could be multiple nested
          // updates on a node.
          if (listen[soul]) {
            setTimeout(() => {
              if (listen[soul]) {
                listen[soul]
                  .filter(l => utils.match(l["."], key))
                  .forEach(l => l.cb())
              }
            }, LISTENER_DELAY)
          }
          updated = true
        }
      }
    }

    if (verify && nodeWait !== 0 && now[soul]) {
      // Secure updates can't be split, so move now to deferred as well.
      Object.assign(defer[soul], now[soul])
      delete now[soul]
    }
    // Call event listeners for update on soul.
    if (updated && listen[soul]) {
      setTimeout(() => {
        if (listen[soul]) {
          listen[soul].filter(l => utils.match(l["."])).forEach(l => l.cb())
        }
      }, LISTENER_DELAY)
    }
  }

  const souls = Object.keys(graph)
  if (souls.length > MAX_GRAPH_SIZE) {
    // Sort by oldest state timestamp and remove oldest entries
    const soulsByAge = souls
      .map(soul => {
        const states = graph[soul]._ && graph[soul]._[">"]
        const maxState = states ? Math.max(...Object.values(states)) : 0
        return {soul, maxState}
      })
      .sort((a, b) => a.maxState - b.maxState)
    const remove = soulsByAge.slice(0, souls.length - MAX_GRAPH_SIZE)
    remove.forEach(({soul}) => delete graph[soul])
  }

  return {now: now, defer: defer, wait: wait}
}

export default Ham
