import fs from "fs"
import {Server} from "mock-socket"
import {describe, test} from "node:test"
import assert from "node:assert/strict"
import Holster from "../src/holster.js"

describe("holster.on", () => {
  const wss = new Server("ws://localhost:1234")
  const holster = Holster({file: "test/holster.on", wss: wss, maxAge: 100})

  test("calling on without get callback null", (_t, done) => {
    holster.on(data => {
      assert.equal(data, null)
      done()
    })
  })

  test("on for property on root then update - event", (_t, done) => {
    // The property needs to exist before it can be listend to for updates.
    holster.get("key").put("value", err => {
      assert.equal(err, null)

      holster.get("key").on(data => {
        assert.equal(data, "update")
        done()
      })

      holster.get("key").put("update", err => {
        assert.equal(err, null)
      })
    })
  })

  test("on for different property on root - no event", (_t, done) => {
    holster.get("key1").on(data => {
      console.log("should not be called for key1:", data)
      done() // This done is not called and test would fail if it was.
    })

    setTimeout(() => {
      holster.get("key2").put("value2", err => {
        assert.equal(err, null)
        done()
      })
    }, 100)
  })

  test("on for property on root two updates - two events", (_t, done) => {
    holster.get("key3").put("value3", err => {
      assert.equal(err, null)

      let first = true
      holster.get("key3").on(data => {
        if (first) {
          assert.equal(data, "update1")
        } else {
          assert.equal(data, "update2")
          done()
        }
      })

      holster.get("key3").put("update1", err => {
        assert.equal(err, null)
      })

      setTimeout(() => {
        first = false
        holster.get("key3").put("update2", err => {
          assert.equal(err, null)
        })
      }, 200)
    })
  })

  test("on for properties on root in for loop", (_t, done) => {
    for (let i = 0; i < 5; i++) {
      holster.get("for" + i).put(i, err => {
        assert.equal(err, null)
      })
    }

    // Need to wait until after all initial puts are done to set on listeners.
    setTimeout(() => {
      for (let i = 0; i < 5; i++) {
        holster.get("for" + i).on(data => {
          assert.equal(data, "update" + i)
          if (i === 4) done()
        })
      }

      setTimeout(() => {
        for (let i = 0; i < 5; i++) {
          holster.get("for" + i).put("update" + i, err => {
            assert.equal(err, null)
          })
        }
      }, 100)
    }, 500)
  })

  test("on with get flag set - data returned", (_t, done) => {
    holster.get("key4").put("value4", err => {
      assert.equal(err, null)

      setTimeout(() => {
        holster.get("key4").on(data => {
          assert.equal(data, "value4")
          done()
        }, true)
      }, 100)
    })
  })

  test("chained next before on", (_t, done) => {
    // The node needs to exist before it can be listend to for updates.
    holster
      .get("hello")
      .next("world!")
      .put("ok", err => {
        assert.equal(err, null)

        holster
          .get("hello")
          .next("world!")
          .on(data => {
            assert.equal(data, "update")
            holster.get("hello").next("world!").off()
            done()
          })

        setTimeout(() => {
          holster
            .get("hello")
            .next("world!")
            .put("update", err => {
              assert.equal(err, null)
            })
        }, 10)
      })
  })

  test("on and put object on root in graph format", (_t, done) => {
    const plain = {
      key: "plain value",
      number: 42,
    }
    holster.get("plain").put(plain, err => {
      assert.equal(err, null)

      const update = {
        key: "update",
        number: 42,
      }
      holster.get("plain").on(data => {
        assert.deepEqual(data, update)
        done()
      })

      setTimeout(() => {
        holster.get("plain").put(update, err => {
          assert.equal(err, null)
        })
      }, 10)
    })
  })

  test("on and put nested object", (_t, done) => {
    const nested = {
      key: "nested key",
      child: {
        has: "child key",
      },
    }
    // The node needs to exist before it can be listend to for updates.
    holster.get("nested").put(nested, err => {
      assert.equal(err, null)

      const update = {
        child: {
          has: "child update",
        },
      }
      holster.get("nested").on(data => {
        assert.deepEqual(data, {key: nested.key, ...update})
      })

      holster
        .get("nested")
        .next("child")
        .on(data => {
          assert.deepEqual(data, update.child)
          done()
        })

      setTimeout(() => {
        holster.get("nested").put(update, err => {
          assert.equal(err, null)
        })
      }, 10)
    })
  })

  test("cleanup", (_t, done) => {
    fs.rm("test/holster.on", {recursive: true, force: true}, err => {
      assert.equal(err, null)
      done()
    })
  })
})
