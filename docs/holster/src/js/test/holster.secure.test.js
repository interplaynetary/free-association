import fs from "fs"
import {Server} from "mock-socket"
import {describe, test} from "node:test"
import assert from "node:assert/strict"
import Holster from "../src/holster.js"

describe("holster.secure", () => {
  const wss = new Server("ws://localhost:1234")
  const holster = Holster({
    file: "test/holster.secure",
    wss: wss,
    maxAge: 100,
    secure: true,
  })
  const user = holster.user()
  const expect = "error putting data on root: user required in secure mode"

  test("empty key callback null", (t, done) => {
    holster.get("", data => {
      assert.equal(data, null)
      done()
    })
  })

  test("null key callback null", (t, done) => {
    holster.get(null, data => {
      assert.equal(data, null)
      done()
    })
  })

  test("underscore as key callback null", (t, done) => {
    holster.get("_", data => {
      assert.equal(data, null)
      done()
    })
  })

  test("get unknown key callback null", (t, done) => {
    holster.get("unknown", data => {
      assert.equal(data, null)
      done()
    })
  })

  test("next chained unknown keys callback null", (t, done) => {
    holster.get("chained").next("unknown", data => {
      assert.equal(data, null)
      done()
    })
  })

  test("put string on root error", (t, done) => {
    holster.get("key").put("value", err => {
      assert.equal(err, expect)
      done()
    })
  })

  test("chained get before put", (t, done) => {
    holster
      .get("hello")
      .next("world!")
      .put("ok", err => {
        assert.equal(err, expect)
        done()
      })
  })

  test("chained get before put object in graph format", (t, done) => {
    const plain = {
      key: "hello plain value",
    }
    holster
      .get("hello")
      .next("plain")
      .put(plain, err => {
        assert.equal(err, expect)
        done()
      })
  })

  test("user create", (t, done) => {
    user.create("alice", "password", err => {
      assert.equal(err, null)
      done()
    })
  })

  test("user auth", (t, done) => {
    user.auth("alice", "password", err => {
      assert.equal(err, null)
      done()
    })
  })

  test("user put and get string", (t, done) => {
    user.get("key").put("value", err => {
      assert.equal(err, null)

      user.get("key", data => {
        assert.equal(data, "value")
        done()
      })
    })
  })

  test("user chained get before put", (t, done) => {
    user
      .get("hello")
      .next("world!")
      .put("ok", err => {
        assert.equal(err, null)

        user.get("hello").next("world!", data => {
          assert.equal(data, "ok")
          done()
        })
      })
  })

  test("user put and get object in graph format", (t, done) => {
    const plain = {
      key: "plain value",
      true: true,
      false: false,
      number: 42,
    }
    user.get("plain").put(plain, err => {
      assert.equal(err, null)

      user.get("plain", data => {
        assert.deepEqual(data, plain)
        done()
      })
    })
  })

  test("user put object to null", (t, done) => {
    user.get("plain").put(null, err => {
      assert.equal(err, null)

      user.get("plain", data => {
        assert.equal(data, null)
        done()
      })
    })
  })

  test("user chained get before put object in graph format", (t, done) => {
    const plain = {
      key: "hello plain value",
    }
    user
      .get("hello")
      .next("plain")
      .put(plain, err => {
        assert.equal(err, null)

        user.get("hello").next("plain", data => {
          assert.deepEqual(data, plain)
          done()
        })
      })
  })

  test("user put and get nested object", (t, done) => {
    const nested = {
      key: "nested value",
      child: {
        has: "child value",
      },
    }
    user.get("nested").put(nested, err => {
      assert.equal(err, null)

      // Getting a nested object requires waiting for radisk to write to disk,
      // as it will batch the writes. (Default wait is 1 millisecond.)
      setTimeout(() => {
        user.get("nested", data => {
          assert.deepEqual(data, nested)

          user.get("nested").next("key", data => {
            assert.equal(data, "nested value")

            user.get("nested").next("child", data => {
              assert.deepEqual(data, {has: "child value"})
              done()
            })
          })
        })
      }, 2)
    })
  })

  test("cleanup", (t, done) => {
    fs.rm("test/holster.secure", {recursive: true, force: true}, err => {
      assert.equal(err, null)
      done()
    })
  })
})
