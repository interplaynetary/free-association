import {userPublicKey, userSignature} from "../utils.js"
import * as utils from "../sea-utils.js"
import SafeBuffer from "../buffer.js"

// Security, Encryption, and Authorization: SEA.js from GunDB.
const SEA = {
  pair: async cb => {
    // ECDSA keys for signing/verifying.
    const ecdsa = await utils.subtle
      .generateKey({name: "ECDSA", namedCurve: "P-256"}, true, [
        "sign",
        "verify",
      ])
      .then(async keys => {
        const pub = await utils.subtle.exportKey("jwk", keys.publicKey)
        return {
          priv: (await utils.subtle.exportKey("jwk", keys.privateKey)).d,
          pub: pub.x + "." + pub.y,
        }
      })

    // ECDH keys for encryption/decryption.
    const ecdh = await utils.subtle
      .generateKey({name: "ECDH", namedCurve: "P-256"}, true, ["deriveKey"])
      .then(async keys => {
        const pub = await utils.subtle.exportKey("jwk", keys.publicKey)
        return {
          epriv: (await utils.subtle.exportKey("jwk", keys.privateKey)).d,
          epub: pub.x + "." + pub.y,
        }
      })

    const pair = {
      pub: ecdsa.pub,
      priv: ecdsa.priv,
      epub: ecdh.epub,
      epriv: ecdh.epriv,
    }
    if (cb) cb(pair)
    return pair
  },
  encrypt: async (data, pair, cb) => {
    if (!pair || !pair.epriv) {
      if (cb) cb(null)
      return null
    }

    const rand = {s: utils.random(9), iv: utils.random(15)}
    const ct = await utils.aeskey(pair.epriv, rand.s).then(aes => {
      return utils.subtle.encrypt(
        {
          name: "AES-GCM",
          iv: new Uint8Array(rand.iv),
        },
        aes,
        new TextEncoder().encode(utils.stringify(data)),
      )
    })
    const enc = {
      ct: SafeBuffer.from(ct, "binary").toString("base64"),
      iv: rand.iv.toString("base64"),
      s: rand.s.toString("base64"),
    }
    if (cb) cb(enc)
    return enc
  },
  decrypt: async (enc, pair, cb) => {
    if (!enc || !enc.ct || !enc.iv || !enc.s || !pair || !pair.epriv) {
      if (cb) cb(null)
      return null
    }

    const data = {
      ct: SafeBuffer.from(enc.ct, "base64"),
      iv: SafeBuffer.from(enc.iv, "base64"),
      s: SafeBuffer.from(enc.s, "base64"),
    }
    try {
      const ct = await utils.aeskey(pair.epriv, data.s).then(aes => {
        return utils.subtle.decrypt(
          {
            name: "AES-GCM",
            iv: new Uint8Array(data.iv),
            tagLength: 128,
          },
          aes,
          new Uint8Array(data.ct),
        )
      })
      const dec = utils.parse(new TextDecoder("utf8").decode(ct))
      if (cb) cb(dec)
      return dec
    } catch (err) {
      // An error will be thrown if the wrong key is used.
      if (cb) cb(null)
      return null
    }
  },
  verify: async (data, pair, cb) => {
    if (!pair || !pair.pub) {
      if (cb) cb(null)
      return null
    }

    const signed = utils.parse(data)
    const key = await utils.subtle.importKey(
      "jwk",
      utils.jwk(pair.pub),
      {name: "ECDSA", namedCurve: "P-256"},
      false,
      ["verify"],
    )

    let msg = {}
    if (typeof signed.m === "string") {
      msg = signed.m
    } else {
      // Allow data to be passed in with graph meta data,
      // (which should not be part of signature, so not verified).
      for (const k of Object.keys(signed.m).sort()) {
        if (k !== "_" && k != userPublicKey && k != userSignature) {
          msg[k] = signed.m[k]
        }
      }
    }
    const hash = await utils.sha256(msg)
    const sig = new Uint8Array(SafeBuffer.from(signed.s, "base64"))
    const alg = {name: "ECDSA", hash: {name: "SHA-256"}}
    if (await utils.subtle.verify(alg, key, sig, new Uint8Array(hash))) {
      const verified = utils.parse(signed.m)
      if (cb) cb(verified)
      return verified
    }

    if (cb) cb(null)
    return null
  },
  sign: async (data, pair, cb) => {
    if (!pair || !pair.pub || !pair.priv) {
      if (cb) cb(null)
      return null
    }

    let msg = {}
    if (typeof data === "string") {
      msg = data
    } else {
      const check = utils.parse(data)
      for (const k of Object.keys(check).sort()) {
        if (k !== "_" && k != userPublicKey && k != userSignature) {
          msg[k] = check[k]
        }
      }
    }
    const hash = await utils.sha256(msg)
    const jwk = utils.jwk(pair.pub, pair.priv)
    const alg = {name: "ECDSA", namedCurve: "P-256"}
    const sig = await utils.subtle
      .importKey("jwk", jwk, alg, false, ["sign"])
      .then(key =>
        utils.subtle.sign(
          {name: "ECDSA", hash: {name: "SHA-256"}},
          key,
          new Uint8Array(hash),
        ),
      )
    const signed = {
      m: msg,
      s: SafeBuffer.from(sig, "binary").toString("base64"),
    }

    if (cb) cb(signed)
    return signed
  },
  work: async (data, salt, cb) => {
    if (typeof salt === "function") {
      cb = salt
      salt = undefined
    }
    if (typeof salt === "undefined") salt = utils.random(9)

    const key = await utils.subtle.importKey(
      "raw",
      new TextEncoder().encode(utils.stringify(data)),
      {name: "PBKDF2"},
      false,
      ["deriveBits"],
    )
    const alg = {
      name: "PBKDF2",
      iterations: 100000,
      salt: new TextEncoder().encode(salt),
      hash: {name: "SHA-256"},
    }
    const work = await utils.subtle.deriveBits(alg, key, 512)
    // Use "pair" format so that work can be used as epriv by decrypt.
    const pair = {epriv: SafeBuffer.from(work, "binary").toString("base64")}
    if (cb) cb(pair)
    return pair
  },
  secret: async (to, from, cb) => {
    if (!to || !to.epub || !from || !from.epub || !from.epriv) {
      if (cb) cb(null)
      return null
    }

    const alg = {name: "ECDH", namedCurve: "P-256"}
    const pub = utils.jwk(to.epub)
    const pubKey = await utils.subtle.importKey("jwk", pub, alg, true, [])
    const priv = utils.jwk(from.epub, from.epriv, false)
    // utils.jwk provides default key_ops but it shouldn't be used here.
    delete priv.key_ops
    const derived = await utils.subtle
      .importKey("jwk", priv, alg, false, ["deriveBits"])
      .then(async key => {
        const derivedBits = await utils.subtle.deriveBits(
          {public: pubKey, name: "ECDH", namedCurve: "P-256"},
          key,
          256,
        )
        const derivedKey = await utils.subtle.importKey(
          "raw",
          new Uint8Array(derivedBits),
          {name: "AES-GCM", length: 256},
          true,
          ["encrypt", "decrypt"],
        )
        return utils.subtle.exportKey("jwk", derivedKey).then(({k}) => k)
      })
    // Use "pair" format so that secret can be used as epriv by encrypt.
    if (cb) cb({epriv: derived})
    return {epriv: derived}
  },
}

export default SEA
