import SafeBuffer from "../buffer.js"

const isNode = typeof document === "undefined"
const crypto = isNode
  ? (await import(/*webpackIgnore: true*/ "node:crypto")).webcrypto
  : globalThis.crypto
export const subtle = crypto.subtle

export const stringify = data => {
  return typeof data === "string" ? data : JSON.stringify(data)
}

export const parse = text => {
  try {
    return JSON.parse(text)
  } catch {
    return text
  }
}

export const random = length => {
  const array = new Uint8Array(SafeBuffer.alloc(length))
  return SafeBuffer.from(crypto.getRandomValues(array))
}

export const jwk = (pub, priv) => {
  const [x, y] = pub.split(".")
  const ops = priv ? ["sign"] : ["verify"]
  return {
    kty: "EC",
    crv: "P-256",
    x: x,
    y: y,
    d: priv,
    ext: true,
    key_ops: ops,
  }
}

export const sha256 = async data => {
  const hash = await subtle.digest(
    {name: "SHA-256"},
    new TextEncoder().encode(stringify(data)),
  )
  return SafeBuffer.from(hash)
}

export const aeskey = async (key, salt) => {
  const combined = key + salt.toString("utf8")
  const hash = SafeBuffer.from(await sha256(combined), "binary")
  const jwk = keyToJwk(hash)
  return await subtle.importKey("jwk", jwk, {name: "AES-GCM"}, false, [
    "encrypt",
    "decrypt",
  ])
}

const keyToJwk = key => {
  const k = key
    .toString("base64")
    .replace(/\+/g, "-")
    .replace(/\//g, "_")
    .replace(/\=/g, "")
  return {kty: "oct", k: k, ext: false, alg: "A256GCM"}
}
