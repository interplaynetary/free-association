import fs from "fs"
import {Server} from "mock-socket"
import {describe, test} from "node:test"
import assert from "node:assert/strict"
import Holster from "../src/holster.js"

describe("holster.user.lex", () => {
  const wss = new Server("ws://localhost:1234")
  const holster = Holster({
    file: "test/holster.user.lex",
    wss: wss,
    maxAge: 100,
  })
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
      assert.equal(user.is.username, "alice")
      done()
    })
  })

  test("put and get object in graph format", (t, done) => {
    const plain = {
      key: "plain value",
      true: true,
      false: false,
      number: 42,
    }
    user.get("plain").put(plain, err => {
      assert.equal(err, null)

      // Prefix tests. (Need to be sequential to match return values.)
      user.get("plain", {".": {"*": "k"}}, data => {
        assert.deepEqual(data, {key: "plain value"})

        user.get("plain", {".": {"*": "t"}}, data => {
          assert.deepEqual(data, {true: true})

          user.get("plain", {".": {"*": "f"}}, data => {
            assert.deepEqual(data, {false: false})

            user.get("plain", {".": {"*": "n"}}, data => {
              assert.deepEqual(data, {number: 42})
            })

            // Both less than and greater than.
            user.get("plain", {".": {"<": "n", ">": "falsy"}}, data => {
              assert.deepEqual(data, {key: "plain value"})

              // Only less than.
              user.get("plain", {".": {"<": "k"}}, data => {
                assert.deepEqual(data, {false: false})

                // Only greater than.
                user.get("plain", {".": {">": "numbers"}}, data => {
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

    user.get("nested").put(nested, err => {
      assert.equal(err, null)

      // Getting a nested object requires waiting for radisk to write to disk,
      // as it will batch the writes. (Default wait is 1 millisecond.)
      setTimeout(() => {
        user.get("nested", {".": {"*": "k"}}, data => {
          assert.deepEqual(data, {key: "nested value"})

          user.get("nested").next("child", {".": {"*": "h"}}, data => {
            assert.deepEqual(data, {has: "child value"})

            user.get("nested").next("child", {".": {"<": "has"}}, data => {
              // Less than means less than or equal to in lex.
              assert.deepEqual(data, {has: "child value"})
              done()
            })
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
    user.get("on-nested").put(nested, err => {
      assert.equal(err, null)

      const update = {
        child: {
          has: "child update",
        },
      }

      user
        .get("on-nested")
        .next("child")
        .on({".": {">": "h"}}, data => {
          assert.deepEqual(data, update.child)
          done()
        })

      setTimeout(() => {
        user.get("on-nested").put(update, err => {
          assert.equal(err, null)
        })
      }, 10)
    })
  })

  test("cleanup", (t, done) => {
    fs.rm("test/holster.user.lex", {recursive: true, force: true}, err => {
      assert.equal(err, null)
      done()
    })
  })
})
