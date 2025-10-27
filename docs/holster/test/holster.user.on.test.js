import fs from "fs"
import {Server} from "mock-socket"
import {describe, test} from "node:test"
import assert from "node:assert/strict"
import Holster from "../src/holster.js"

describe("holster.user.on", () => {
  const wss = new Server("ws://localhost:1234")
  const holster = Holster({file: "test/holster.user.on", wss: wss, maxAge: 100})
  const user = holster.user()

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

  test("calling on without get callback null", (t, done) => {
    user.on(data => {
      assert.equal(data, null)
      done()
    })
  })

  test("on for property then update - event", (t, done) => {
    // The property needs to exist before it can be listend to for updates.
    user.get("key").put("value", err => {
      assert.equal(err, null)

      const callback = data => {
        assert.equal(data, "update")
        user.get("key").off(callback)
        done()
      }
      user.get("key").on(callback)

      user.get("key").put("update", err => {
        assert.equal(err, null)
      })
    })
  })

  test("on for different property - no event", (t, done) => {
    user.get("key1").on(data => {
      console.log("should not be called for key1:", data)
      done() // This done is not called and test would fail if it was.
    })

    user.get("key2").put("value2", err => {
      assert.equal(err, null)
      done()
    })
  })

  test("on for property two updates - two events", (t, done) => {
    user.get("key3").put("value3", err => {
      assert.equal(err, null)

      let first = true
      const callback = data => {
        if (first) {
          assert.equal(data, "update1")
        } else {
          assert.equal(data, "update2")
          user.get("key3").off(callback)
          done()
        }
      }
      user.get("key3").on(callback)

      user.get("key3").put("update1", err => {
        assert.equal(err, null)
      })

      setTimeout(() => {
        first = false
        user.get("key3").put("update2", err => {
          assert.equal(err, null)
        })
      }, 200)
    })
  })

  test("on for properties in for loop", (t, done) => {
    ;(async () => {
      for (let i = 0; i < 5; i++) {
        // Need to be careful putting user properties in a loop as they are
        // verified per node, so wait for each update to return.
        const err = await new Promise(res => {
          user.get("for" + i).put(i, res)
        })
        assert.equal(err, null)
      }
    })()

    // Need to wait until after all initial puts are done to set on listeners.
    setTimeout(async () => {
      let count = 0
      for (let i = 0; i < 5; i++) {
        const idx = i // Capture value
        const callback = data => {
          assert.equal(data, "update" + idx)
          user.get("for" + idx).off(callback)
          count++
        }
        user.get("for" + idx).on(callback)
      }

      setTimeout(async () => {
        for (let i = 0; i < 5; i++) {
          // Same as above put, wait for each update to return.
          const err = await new Promise(res => {
            user.get("for" + i).put("update" + i, res)
          })
          assert.equal(err, null)
        }
        setTimeout(() => {
          if (count === 5) done()
        }, 500)
      }, 500)
    }, 1000)
  })

  test("on with get flag set - data returned", (t, done) => {
    user.get("key4").put("value4", err => {
      assert.equal(err, null)

      setTimeout(() => {
        const callback = data => {
          assert.equal(data, "value4")
          user.get("key4").off(callback)
          done()
        }
        user.get("key4").on(callback, true)
      }, 100)
    })
  })

  test("chained next before on", (t, done) => {
    // The node needs to exist before it can be listend to for updates.
    user
      .get("hello")
      .next("world!")
      .put("ok", err => {
        assert.equal(err, null)

        const callback = data => {
          assert.equal(data, "update")
          user.get("hello").next("world!").off(callback)
          done()
        }
        user.get("hello").next("world!").on(callback)

        setTimeout(() => {
          user
            .get("hello")
            .next("world!")
            .put("update", err => {
              assert.equal(err, null)
            })
        }, 10)
      })
  })

  test("on and put object in graph format", (t, done) => {
    const plain = {
      key: "plain value",
      number: 42,
    }

    user.get("plain").put(plain, err => {
      assert.equal(err, null)

      const update = {
        key: "update",
        number: 42,
      }

      const callback = data => {
        assert.deepEqual(data, update)
        user.get("plain").off(callback)
        done()
      }
      user.get("plain").on(callback)

      setTimeout(() => {
        user.get("plain").put(update, err => {
          assert.equal(err, null)
        })
      }, 10)
    })
  })

  test("on and put nested object", (t, done) => {
    const nested = {
      key: "nested key",
      child: {
        has: "child key",
      },
    }
    // The node needs to exist before it can be listend to for updates.
    user.get("nested").put(nested, err => {
      assert.equal(err, null)

      const update = {
        key: "nested update",
        child: {
          has: "child update",
        },
      }

      const parentCallback = data => {
        assert.deepEqual(data, update)
      }
      user.get("nested").on(parentCallback)

      const childCallback = data => {
        assert.deepEqual(data, update.child)
        user.get("nested").off(parentCallback)
        user.get("nested").next("child").off(childCallback)
        done()
      }
      user.get("nested").next("child").on(childCallback)

      setTimeout(() => {
        user.get("nested").put(update, err => {
          assert.equal(err, null)
        })
      }, 10)
    })
  })

  test("cleanup", (t, done) => {
    setTimeout(() => {
      fs.rm("test/holster.user.on", {recursive: true, force: true}, err => {
        assert.equal(err, null)
        done()
      })
    }, 100)
  })
})
