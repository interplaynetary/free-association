import fs from "fs"
import {Server} from "mock-socket"
import {describe, test} from "node:test"
import assert from "node:assert/strict"
import Holster from "../../src/holster.js"

describe("system - user chained get with on", () => {
  const wss = new Server("ws://localhost:9012")
  const holster = Holster({file: "test/system/user-chained-on", wss: wss})
  const user = holster.user()

  test("user create and auth", (t, done) => {
    user.create("testuser", "password", err => {
      assert.equal(err, null)
      user.auth("testuser", "password", err => {
        assert.equal(err, null)
        done()
      })
    })
  })

  test("on listener fires with null when no data exists", (t, done) => {
    user.get("key").on(data => {
      // This is the actual behavior - on fires with null for non-existent keys
      assert.equal(data, null)
      done()
    }, true)
  })

  test("put then on listener receives data", (t, done) => {
    // Put data first
    user.get("key2").put({value: "test"}, err => {
      assert.equal(err, null)

      // Then set up listener - should get the existing data
      setTimeout(() => {
        user.get("key2").on(data => {
          assert.notEqual(data, null)
          assert.deepEqual(data, {value: "test"})
          done()
        }, true)
      }, 50)
    })
  })

  test("cleanup", (t, done) => {
    fs.rm(
      "test/system/user-chained-on",
      {recursive: true, force: true},
      err => {
        assert.equal(err, null)
        done()
      },
    )
  })
})
