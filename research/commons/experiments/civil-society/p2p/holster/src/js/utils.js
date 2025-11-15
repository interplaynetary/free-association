export const num = {
  is: n => {
    if (n instanceof Array) return false

    if (typeof n === "number") return !isNaN(n)

    if (typeof n === "string") {
      const parsed = parseFloat(n)
      return !isNaN(parsed) && isFinite(parsed)
    }

    return false
  },
}

export const obj = {
  is: o => {
    if (!o || typeof o !== "object") return false

    const toString = Object.prototype.toString.call(o)
    const match = toString.match(/^\[object (\w+)\]$/)
    return o.constructor === Object || (match && match[1] === "Object")
  },
  map: (list, cb, o) => {
    var keys = Object.keys(list)
    for (let i = 0; i < keys.length; i++) {
      let result = cb(list[keys[i]], keys[i], o)
      if (typeof result !== "undefined") return result
    }
  },
  put: (o, key, value) => {
    if (!o) o = {}
    o[key] = value
    return o
  },
  del: (o, key) => {
    if (!o) return

    o[key] = null
    delete o[key]
    return o
  },
}

const map_soul = (soul, key, o) => {
  // If id is already defined AND we're still looping through the object,
  // then it is considered invalid.
  if (o.id) {
    o.id = false
    return
  }

  if (key === "#" && typeof soul === "string") {
    o.id = soul
    return
  }

  // If there exists anything else on the object that isn't the soul,
  // then it is considered invalid.
  o.id = false
}

// Check if an object is a soul relation, ie {'#': 'UUID'}
export const rel = {
  is: value => {
    if (value && value["#"] && !value._ && obj.is(value)) {
      let o = {}
      obj.map(value, map_soul, o)
      if (o.id) return o.id
    }

    return false
  },
  // Convert a soul into a relation and return it.
  ify: soul => obj.put({}, "#", soul),
}

export const userSignature = "_holster_user_signature"
export const userPublicKey = "_holster_user_public_key"

// graph converts objects to graph format with updated states,
// with optional meta data to verify signed data.
export const graph = (soul, data, sig, pub) => {
  const timestamp = Date.now()
  const g = {[soul]: {_: {"#": soul, ">": {}}}}

  for (const [key, value] of Object.entries(data)) {
    if (key !== "_" && key !== userPublicKey && key !== userSignature) {
      g[soul][key] = value
      g[soul]._[">"][key] = timestamp
    }
  }
  // If a signature and public key are provided they also need to be stored on
  // the node to ensure that future updates are only possible with the same
  // public key. The signature is requried because later get requests will
  // broadcast the data as a put, which other devices will need to verify.
  if (sig && pub) {
    g[soul][userSignature] = sig
    g[soul]._[">"][userSignature] = timestamp
    g[soul][userPublicKey] = pub
    g[soul]._[">"][userPublicKey] = timestamp
  }
  return g
}

export const match = (lex, key) => {
  // Null is used to match listeners on souls, which don't provide a key.
  if (typeof key === "undefined") return lex === null

  if (typeof lex === "undefined") return true

  if (typeof lex === "string") return lex === key

  if (!obj.is(lex) || !key) return false

  const prefix = lex["*"]
  if (prefix) return key.slice(0, prefix.length) === prefix

  const gt = lex[">"]
  const lt = lex["<"]
  if (gt && lt) return key >= gt && key <= lt

  if (gt) return key >= gt

  if (lt) return key <= lt

  return false
}

export const text = {
  random: length => {
    var s = ""
    const c = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
    if (!length) length = 24
    for (let i = 0; i < length; i++) {
      s += c.charAt(Math.floor(Math.random() * c.length))
    }
    return s
  },
}
