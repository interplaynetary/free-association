import fs from "fs"
import {Server} from "mock-socket"
import {describe, test} from "node:test"
import assert from "node:assert/strict"
import Holster from "../src/holster.js"

describe("holster.lex", () => {
  const wss = new Server("ws://localhost:1234")
  const holster = Holster({file: "test/holster.lex", wss: wss, maxAge: 100})

  test("object on root in graph format", (t, done) => {
    const plain = {
      key: "plain value",
      true: true,
      false: false,
      number: 42,
    }
    holster.get("plain").put(plain, err => {
      assert.equal(err, null)

      // Prefix tests. (Need to be sequential to match return values.)
      holster.get("plain", {".": {"*": "k"}}, data => {
        assert.deepEqual(data, {key: "plain value"})

        holster.get("plain", {".": {"*": "t"}}, data => {
          assert.deepEqual(data, {true: true})

          holster.get("plain", {".": {"*": "f"}}, data => {
            assert.deepEqual(data, {false: false})

            holster.get("plain", {".": {"*": "n"}}, data => {
              assert.deepEqual(data, {number: 42})
            })

            // Both less than and greater than.
            holster.get("plain", {".": {"<": "n", ">": "falsy"}}, data => {
              assert.deepEqual(data, {key: "plain value"})

              // Only less than.
              holster.get("plain", {".": {"<": "k"}}, data => {
                assert.deepEqual(data, {false: false})

                // Only greater than.
                holster.get("plain", {".": {">": "numbers"}}, data => {
                  assert.deepEqual(data, {true: true})
                  done()
                })
              })
            })
          })
        })
      })
    })
  })

  test("put and get nested object", (t, done) => {
    const nested = {
      key: "nested value",
      other: "other value",
      child: {
        has: "child value",
        other: "other child value",
      },
    }
    holster.get("nested").put(nested, err => {
      assert.equal(err, null)

      // Getting a nested object requires waiting for radisk to write to disk,
      // as it will batch the writes. (Default wait is 1 millisecond.)
      setTimeout(() => {
        holster.get("nested", {".": {"*": "k"}}, data => {
          assert.deepEqual(data, {key: "nested value"})

          holster.get("nested").next("child", {".": {"*": "h"}}, data => {
            assert.deepEqual(data, {has: "child value"})

            holster.get("nested").next("child", {".": {"<": "has"}}, data => {
              // Less than means less than or equal to in lex.
              assert.deepEqual(data, {has: "child value"})
              done()
            })
          })
        })
      }, 2)
    })
  })

  test("chained get before put nested object", (t, done) => {
    const nested = {
      key: "hello nested value",
      other: "other value",
      child: {
        has: "hello child value",
        other: "other child value",
      },
    }
    holster
      .get("hello")
      .next("nested")
      .put(nested, err => {
        assert.equal(err, null)

        setTimeout(() => {
          holster.get("hello").next("nested", {".": {"*": "k"}}, data => {
            assert.deepEqual(data, {key: "hello nested value"})

            holster
              .get("hello")
              .next("nested")
              .next("child", {".": {"*": "h"}}, data => {
                assert.deepEqual(data, {has: "hello child value"})

                holster
                  .get("hello")
                  .next("nested")
                  .next("child", {".": {">": "other"}}, data => {
                    // Greater than means greater than or equal to in lex.
                    assert.deepEqual(data, {other: "other child value"})
                    done()
                  })
              })
          })
        }, 2)
      })
  })

  test("put and get two nested objects", (t, done) => {
    const two = {
      key: "two nested values",
      child1: {
        has: "child value 1",
      },
      child2: {
        has: "child value 2",
      },
    }
    holster.get("two").put(two, err => {
      assert.equal(err, null)

      setTimeout(() => {
        holster.get("two", {".": {"*": "child"}}, data => {
          assert.deepEqual(data, {
            child1: two.child1,
            child2: two.child2,
          })

          holster
            .get("two")
            .next("child1", {".": {">": "g", "<": "i"}}, data => {
              assert.deepEqual(data, {has: "child value 1"})
              done()
            })
        })
      }, 2)
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
    holster.get("on-nested").put(nested, err => {
      assert.equal(err, null)

      const update = {
        child: {
          has: "child update",
        },
      }
      holster
        .get("on-nested")
        .next("child")
        .on({".": {">": "h"}}, data => {
          assert.deepEqual(data, update.child)
          done()
        })

      setTimeout(() => {
        holster.get("on-nested").put(update, err => {
          assert.equal(err, null)
        })
      }, 10)
    })
  })

  test("cleanup", (t, done) => {
    fs.rm("test/holster.lex", {recursive: true, force: true}, err => {
      assert.equal(err, null)
      done()
    })
  })
})
