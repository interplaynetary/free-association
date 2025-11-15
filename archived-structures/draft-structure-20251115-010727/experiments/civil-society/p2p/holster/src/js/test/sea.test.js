import {describe, test} from "node:test"
import assert from "node:assert/strict"
import SEA from "../src/sea.js"

// The JWK format uses "base64url" encoding, which means "+" replaced with "-"
// and "/" with "_" and removing "=" padding. So public and private keys here
// are strings of alphanumeric characters and underscores (regex: \w), as well
// as dashes. The x and y values of public keys are joined by a full stop, so
// these conditions match the following two regular expressions:
const pubRegex = /^(\w|\-)+\.(\w|\-)+$/
const privRegex = /^(\w|\-)+$/

describe("SEA", () => {
  test("pair await", (t, done) => {
    ;(async () => {
      const pair = await SEA.pair()
      assert.match(pair.pub, pubRegex)
      assert.match(pair.priv, privRegex)
      assert.match(pair.epub, pubRegex)
      assert.match(pair.epriv, privRegex)
      done()
    })()
  })

  test("pair callback", (t, done) => {
    SEA.pair(pair => {
      assert.match(pair.pub, pubRegex)
      assert.match(pair.priv, privRegex)
      assert.match(pair.epub, pubRegex)
      assert.match(pair.epriv, privRegex)
      done()
    })
  })

  test("encrypt and decrypt string await", (t, done) => {
    ;(async () => {
      const pair = await SEA.pair()
      const enc = await SEA.encrypt("hello self", pair)
      const dec = await SEA.decrypt(enc, pair)
      assert.equal(dec, "hello self")
      done()
    })()
  })

  test("encrypt and decrypt with wrong key", (t, done) => {
    ;(async () => {
      const alice = await SEA.pair()
      const bob = await SEA.pair()
      const enc = await SEA.encrypt("alice secret", alice)
      const dec = await SEA.decrypt(enc, bob)
      assert.equal(dec, null)
      done()
    })()
  })

  test("encrypt and decrypt string callback", (t, done) => {
    SEA.pair(pair => {
      SEA.encrypt("hello self", pair, enc => {
        SEA.decrypt(enc, pair, dec => {
          assert.equal(dec, "hello self")
          done()
        })
      })
    })
  })

  test("encrypt and decrypt object await", (t, done) => {
    ;(async () => {
      const pair = await SEA.pair()
      const enc = await SEA.encrypt({test: "hello self"}, pair)
      const dec = await SEA.decrypt(enc, pair)
      assert.deepEqual(dec, {test: "hello self"})
      done()
    })()
  })

  test("encrypt and decrypt object callback", (t, done) => {
    SEA.pair(pair => {
      SEA.encrypt({test: "hello self"}, pair, enc => {
        SEA.decrypt(enc, pair, dec => {
          assert.deepEqual(dec, {test: "hello self"})
          done()
        })
      })
    })
  })

  test("sign and verify string await", (t, done) => {
    ;(async () => {
      const pair = await SEA.pair()
      const signed = await SEA.sign("hello self", pair)
      const verified = await SEA.verify(signed, pair)
      assert.equal(verified, "hello self")
      done()
    })()
  })

  test("sign and verify with wrong key", (t, done) => {
    ;(async () => {
      const alice = await SEA.pair()
      const bob = await SEA.pair()
      const signed = await SEA.sign("signed by alice", alice)
      const verified = await SEA.verify(signed, bob)
      assert.equal(verified, null)
      done()
    })()
  })

  test("sign and verify string callback", (t, done) => {
    SEA.pair(pair => {
      SEA.sign("hello self", pair, signed => {
        SEA.verify(signed, pair, verified => {
          assert.equal(verified, "hello self")
          done()
        })
      })
    })
  })

  test("sign and verify object await", (t, done) => {
    ;(async () => {
      const pair = await SEA.pair()
      const signed = await SEA.sign({test: "hello self"}, pair)
      const verified = await SEA.verify(signed, pair)
      assert.deepEqual(verified, {test: "hello self"})
      done()
    })()
  })

  test("sign and verify object callback", (t, done) => {
    SEA.pair(pair => {
      SEA.sign({test: "hello self"}, pair, signed => {
        SEA.verify(signed, pair, verified => {
          assert.deepEqual(verified, {test: "hello self"})
          done()
        })
      })
    })
  })

  test("work with salt encrypt string callback", (t, done) => {
    SEA.work("hello", "salt", work => {
      SEA.encrypt("hello work", work, enc => {
        SEA.decrypt(enc, work, dec => {
          assert.equal(dec, "hello work")
          done()
        })
      })
    })
  })

  test("work with salt encrypt string await", (t, done) => {
    ;(async () => {
      const work = await SEA.work("hello", "salt")
      const enc = await SEA.encrypt("hello work", work)
      const dec = await SEA.decrypt(enc, work)
      assert.equal(dec, "hello work")
      done()
    })()
  })

  test("work with salt encrypt object callback", (t, done) => {
    SEA.work("hello", "salt", work => {
      SEA.encrypt({test: "hello work"}, work, enc => {
        SEA.decrypt(enc, work, dec => {
          assert.deepEqual(dec, {test: "hello work"})
          done()
        })
      })
    })
  })

  test("work with salt encrypt object await", (t, done) => {
    ;(async () => {
      const work = await SEA.work("hello", "salt")
      const enc = await SEA.encrypt({test: "hello work"}, work)
      const dec = await SEA.decrypt(enc, work)
      assert.deepEqual(dec, {test: "hello work"})
      done()
    })()
  })

  test("work no salt encrypt string callback", (t, done) => {
    SEA.work("hello", work => {
      SEA.encrypt("hello work", work, enc => {
        SEA.decrypt(enc, work, dec => {
          assert.equal(dec, "hello work")
          done()
        })
      })
    })
  })

  test("work no salt encrypt string await", (t, done) => {
    ;(async () => {
      const work = await SEA.work("hello")
      const enc = await SEA.encrypt("hello work", work)
      const dec = await SEA.decrypt(enc, work)
      assert.equal(dec, "hello work")
      done()
    })()
  })

  test("work no salt encrypt object callback", (t, done) => {
    SEA.work("hello", work => {
      SEA.encrypt({test: "hello work"}, work, enc => {
        SEA.decrypt(enc, work, dec => {
          assert.deepEqual(dec, {test: "hello work"})
          done()
        })
      })
    })
  })

  test("work no salt encrypt object await", (t, done) => {
    ;(async () => {
      const work = await SEA.work("hello")
      const enc = await SEA.encrypt({test: "hello work"}, work)
      const dec = await SEA.decrypt(enc, work)
      assert.deepEqual(dec, {test: "hello work"})
      done()
    })()
  })

  test("secret string callback", (t, done) => {
    SEA.pair(alice => {
      SEA.pair(bob => {
        SEA.secret(bob, alice, to => {
          SEA.encrypt("shared data", to, enc => {
            SEA.secret(alice, bob, from => {
              SEA.decrypt(enc, from, dec => {
                assert.equal(dec, "shared data")
                done()
              })
            })
          })
        })
      })
    })
  })

  test("secret string await", (t, done) => {
    ;(async () => {
      const alice = await SEA.pair()
      const bob = await SEA.pair()
      const to = await SEA.secret(bob, alice)
      const enc = await SEA.encrypt("shared data", to)
      const from = await SEA.secret(alice, bob)
      const dec = await SEA.decrypt(enc, from)
      assert.equal(dec, "shared data")
      done()
    })()
  })

  test("secret object callback", (t, done) => {
    SEA.pair(alice => {
      SEA.pair(bob => {
        SEA.secret(bob, alice, to => {
          SEA.encrypt({test: "shared data"}, to, enc => {
            SEA.secret(alice, bob, from => {
              SEA.decrypt(enc, from, dec => {
                assert.deepEqual(dec, {test: "shared data"})
                done()
              })
            })
          })
        })
      })
    })
  })

  test("secret object await", (t, done) => {
    ;(async () => {
      const alice = await SEA.pair()
      const bob = await SEA.pair()
      const to = await SEA.secret(bob, alice)
      const enc = await SEA.encrypt({test: "shared data"}, to)
      const from = await SEA.secret(alice, bob)
      const dec = await SEA.decrypt(enc, from)
      assert.deepEqual(dec, {test: "shared data"})
      done()
    })()
  })
})
