import {z} from "zod"
import SafeBuffer from "./buffer.js"

const isNode = typeof document === "undefined"
const crypto = isNode
  ? (await import(/*webpackIgnore: true*/ "node:crypto")).webcrypto
  : globalThis.crypto

export const subtle = crypto.subtle

// Zod schemas for cryptographic operations
export const jwkSchema = z.object({
  kty: z.string(),
  crv: z.string().optional(),
  x: z.string().optional(),
  y: z.string().optional(),
  d: z.string().optional(),
  ext: z.boolean().optional(),
  key_ops: z.array(z.string()).optional(),
  k: z.string().optional(),
  alg: z.string().optional(),
})

export type JwkType = z.infer<typeof jwkSchema>

export const stringify = (data: any): string => {
  return typeof data === "string" ? data : JSON.stringify(data)
}

export const parse = (text: string): any => {
  try {
    return JSON.parse(text)
  } catch {
    return text
  }
}

export const random = (length: number): SeaArray => {
  const array = new Uint8Array(SafeBuffer.alloc(length))
  return SafeBuffer.from(crypto.getRandomValues(array))
}

export const jwk = (pub: string, priv?: string, withKeyOps: boolean = true): JwkType => {
  const [x, y] = pub.split(".")
  const ops = priv ? ["sign"] : ["verify"]
  const result: JwkType = {
    kty: "EC",
    crv: "P-256",
    x: x,
    y: y,
    ext: true,
  }
  if (priv) {
    result.d = priv
  }
  if (withKeyOps) {
    result.key_ops = ops
  }
  return result
}

export const sha256 = async (data: any): Promise<SeaArray> => {
  const hash = await subtle.digest(
    {name: "SHA-256"},
    new TextEncoder().encode(stringify(data)),
  )
  return SafeBuffer.from(hash)
}

export const aeskey = async (key: string, salt: SeaArray): Promise<CryptoKey> => {
  const combined = key + salt.toString("utf8")
  const hash = SafeBuffer.from(await sha256(combined), "binary")
  const jwkKey = keyToJwk(hash)
  return await subtle.importKey("jwk", jwkKey, {name: "AES-GCM"}, false, [
    "encrypt",
    "decrypt",
  ])
}

const keyToJwk = (key: SeaArray): JwkType => {
  const k = key
    .toString("base64")
    .replace(/\+/g, "-")
    .replace(/\//g, "_")
    .replace(/\=/g, "")
  return {kty: "oct", k: k, ext: false, alg: "A256GCM"}
}

