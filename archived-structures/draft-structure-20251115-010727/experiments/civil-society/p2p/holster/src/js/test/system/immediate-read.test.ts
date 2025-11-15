import fs from "fs"
import {Server} from "mock-socket"
import {describe, test} from "node:test"
import assert from "node:assert/strict"
import Holster from "../../../holster.js"

describe("system - immediate read after write", () => {
  const wss = new Server("ws://localhost:9008")
  const holster = Holster({file: "test/system/immediate-read", wss: wss})

  test("get immediately after put returns data despite queue delays", (_t, done) => {
    holster.get("testkey").put({value: "test"}, err => {
      assert.equal(err, null)

      // Immediate get after put - validates retry logic handles queued writes
      holster.get("testkey", data => {
        assert.notEqual(data, null)
        assert.equal(data.value, "test")
        done()
      })
    })
  })

  test("cleanup", (_t, done) => {
    fs.rm("test/system/immediate-read", {recursive: true, force: true}, err => {
      assert.equal(err, null)
      done()
    })
  })
})
