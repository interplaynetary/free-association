import fs from "fs"
import {Server} from "mock-socket"
import {describe, test} from "node:test"
import assert from "node:assert/strict"
import Holster from "../src/holster.js"

describe("holster.user.off", () => {
  const wss = new Server("ws://localhost:1234")
  const holster = Holster({
    file: "test/holster.user.off",
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
      done()
    })
  })

  test("calling off without get callback null", (t, done) => {
    user.off(data => {
      assert.equal(data, null)
      done()
    })
  })

  test("off with cb for property then update - no event", (t, done) => {
    const cb = data => {
      console.log("should not be called for key:", data)
      done()
    }
    user.get("key").on(cb)
    user.get("key").off(cb)

    user.get("key").put("value", err => {
      assert.equal(err, null)
      done()
    })
  })

  test("off no cb for property then update - no event", (t, done) => {
    user.get("key1").on(data => {
      console.log("should not be called for key1:", data)
      done()
    })
    setTimeout(() => {
      user.get("key1").off()

      user.get("key1").put("value1", err => {
        assert.equal(err, null)
        done()
      })
    }, 10)
  })

  test("cleanup", (t, done) => {
    fs.rm("test/holster.user.off", {recursive: true, force: true}, err => {
      assert.equal(err, null)
      done()
    })
  })
})
