import fs from "fs"
import {Server} from "mock-socket"
import {describe, test} from "node:test"
import assert from "node:assert/strict"
import Holster from "../../../holster.js"

describe("system - chained get with on", () => {
  const wss = new Server("ws://localhost:9002")
  const holster = Holster({file: "test/system/chained-on", wss: wss})

  test("on listener fires with null when no data exists", (_t, done) => {
    holster.get("key").on(data => {
      // This is the actual behavior - on fires with null for non-existent keys
      assert.equal(data, null)
      done()
    }, true)
  })

  test("put then on listener receives data", (_t, done) => {
    // Put data first
    holster.get("key2").put({value: "test"}, err => {
      assert.equal(err, null)

      // Then set up listener - should get the existing data
      setTimeout(() => {
        holster.get("key2").on(data => {
          assert.notEqual(data, null)
          assert.equal(data.value, "test")
          done()
        }, true)
      }, 50)
    })
  })

  test("cleanup", (_t, done) => {
    fs.rm("test/system/chained-on", {recursive: true, force: true}, err => {
      assert.equal(err, null)
      done()
    })
  })
})
