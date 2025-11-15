import fs from "fs"
import {Server} from "mock-socket"
import {describe, test} from "node:test"
import assert from "node:assert/strict"
import Holster from "../../../holster.js"

describe("system - user listener before data", () => {
  const wss = new Server("ws://localhost:9017")
  const holster = Holster({
    file: "test/system/user-listener-before-data",
    wss: wss,
  })
  const user = holster.user()

  test("user create and auth", (_t, done) => {
    user.create("testuser", "password", err => {
      assert.equal(err, null)
      user.auth("testuser", "password", err => {
        assert.equal(err, null)
        done()
      })
    })
  })

  test("listener set up before data exists receives data when populated", (_t, done) => {
    const results = []

    // Set up listener first before data exists
    user.get("testkey").on(data => {
      results.push(data)
    })

    // Give listener time to set up
    setTimeout(() => {
      user.get("testkey").put({value: "test"}, err => {
        assert.equal(err, null)

        // Give listener time to fire
        setTimeout(() => {
          // Should have received the test value
          const nonNull = results.filter(r => r !== null)
          assert.equal(
            nonNull.length >= 1,
            true,
            "Should have at least one non-null result",
          )
          assert.deepEqual(nonNull[0], {value: "test"})
          done()
        }, 200)
      })
    }, 100)
  })

  test("cleanup", (_t, done) => {
    fs.rm(
      "test/system/user-listener-before-data",
      {recursive: true, force: true},
      err => {
        assert.equal(err, null)
        done()
      },
    )
  })
})
