import fs from "fs"
import {Server} from "mock-socket"
import {describe, test} from "node:test"
import assert from "node:assert/strict"
import Holster from "../../../holster.js"

describe("system - promise wrapped calls", () => {
  const wss = new Server("ws://localhost:9004")
  const holster = Holster({file: "test/system/promise-wrapper", wss: wss})

  test("promise wrapped get returns null for missing key", async t => {
    const data = await new Promise(res => {
      holster.get("missing", res)
    })
    assert.equal(data, null)
  })

  test("promise wrapped get returns data for existing key", async t => {
    await new Promise(res => {
      holster.get("key").put({value: "test"}, res)
    })

    const data = await new Promise(res => {
      holster.get("key", res)
    })
    assert.notEqual(data, null)
    assert.equal(data.value, "test")
  })

  test("promise wrapped next returns null for missing nested key", async t => {
    const data = await new Promise(res => {
      holster.get("key1").next("missing", res)
    })
    assert.equal(data, null)
  })

  test("cleanup", (_t, done) => {
    fs.rm(
      "test/system/promise-wrapper",
      {recursive: true, force: true},
      err => {
        assert.equal(err, null)
        done()
      },
    )
  })
})
