import {z} from "zod"

// Zod schemas for primitive types
export const numSchema = z.union([
  z.number().finite(),
  z.string().refine(
    val => {
      const parsed = parseFloat(val)
      return !isNaN(parsed) && isFinite(parsed)
    },
    {message: "Must be a valid finite number string"},
  ),
])

export const objSchema = z
  .record(z.any())
  .refine(
    val => {
      if (!val || typeof val !== "object") return false
      const toString = Object.prototype.toString.call(val)
      const match = toString.match(/^\[object (\w+)\]$/)
      return val.constructor === Object || (match && match[1] === "Object")
    },
    {message: "Must be a plain object"},
  )

export const relSchema = z.object({
  "#": z.string(),
})

// Type exports
export type NumType = z.infer<typeof numSchema>
export type ObjType = z.infer<typeof objSchema>
export type RelType = z.infer<typeof relSchema>

export const num = {
  is: (n: unknown): n is number | string => {
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
  is: (o: unknown): o is Record<string, any> => {
    if (!o || typeof o !== "object") return false

    const toString = Object.prototype.toString.call(o)
    const match = toString.match(/^\[object (\w+)\]$/)
    return (
      (o as any).constructor === Object || (match && match[1] === "Object")
    )
  },
  map: <T, R>(
    list: Record<string, T>,
    cb: (value: T, key: string, o?: any) => R | undefined,
    o?: any,
  ): R | undefined => {
    const keys = Object.keys(list)
    for (let i = 0; i < keys.length; i++) {
      const result = cb(list[keys[i]], keys[i], o)
      if (typeof result !== "undefined") return result
    }
    return undefined
  },
  put: <T>(o: Record<string, T> | null | undefined, key: string, value: T): Record<string, T> => {
    if (!o) o = {} as Record<string, T>
    o[key] = value
    return o
  },
  del: <T>(o: Record<string, T> | null | undefined, key: string): Record<string, T> | undefined => {
    if (!o) return undefined

    o[key] = null as any
    delete o[key]
    return o
  },
}

const map_soul = (soul: unknown, key: string, o: {id?: string | false}): void => {
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
  is: (value: unknown): string | false => {
    if (value && typeof value === "object" && "#" in value && !("_" in value) && obj.is(value)) {
      const o: {id?: string | false} = {}
      obj.map(value as Record<string, any>, map_soul, o)
      if (o.id) return o.id
    }

    return false
  },
  // Convert a soul into a relation and return it.
  ify: (soul: string): RelType => obj.put({}, "#", soul),
}

export const userSignature = "_holster_user_signature"
export const userPublicKey = "_holster_user_public_key"

// Zod schema for graph metadata
export const graphMetadataSchema = z.object({
  "#": z.string(),
  ">": z.record(z.string(), z.number()),
})

export const graphNodeSchema = z.record(
  z.string(),
  z.union([
    z.string(),
    z.number(),
    z.boolean(),
    z.null(),
    relSchema,
    z.object({"#": z.string()}),
    graphMetadataSchema,
  ]),
)

export type GraphMetadata = z.infer<typeof graphMetadataSchema>
export type GraphNode = z.infer<typeof graphNodeSchema>

// graph converts objects to graph format with updated states,
// with optional meta data to verify signed data.
export const graph = (
  soul: string,
  data: Record<string, any>,
  sig?: string,
  pub?: string,
): Record<string, GraphNode & {_: GraphMetadata}> => {
  const timestamp = Date.now()
  const g: Record<string, GraphNode & {_: GraphMetadata}> = {
    [soul]: {_: {"#": soul, ">": {}}},
  }

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

// Zod schema for lexical query
export const lexSchema = z.union([
  z.string(),
  z.object({
    "*": z.string().optional(),
    ">": z.string().optional(),
    "<": z.string().optional(),
  }),
  z.undefined(),
  z.null(),
])

export type LexType = z.infer<typeof lexSchema>

export const match = (lex: LexType, key?: string): boolean => {
  // Null is used to match listeners on souls, which don't provide a key.
  if (typeof key === "undefined") return lex === null

  if (typeof lex === "undefined") return true

  if (typeof lex === "string") return lex === key

  if (!obj.is(lex) || !key) return false

  const prefix = (lex as any)["*"]
  if (prefix) return key.slice(0, prefix.length) === prefix

  const gt = (lex as any)[">"]
  const lt = (lex as any)["<"]
  if (gt && lt) return key >= gt && key <= lt

  if (gt) return key >= gt

  if (lt) return key <= lt

  return false
}

export const text = {
  random: (length?: number): string => {
    let s = ""
    const c = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
    if (!length) length = 24
    for (let i = 0; i < length; i++) {
      s += c.charAt(Math.floor(Math.random() * c.length))
    }
    return s
  },
}

