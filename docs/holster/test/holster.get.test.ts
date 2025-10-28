import fs from "fs"
import {Server} from "mock-socket"
import {describe, test} from "node:test"
import assert from "node:assert/strict"
import Holster from "../src/holster.js"

describe("holster.get", () => {
  const wss = new Server("ws://localhost:1234")
  const holster = Holster({file: "test/holster.get", wss: wss, maxAge: 100})

  test("empty key callback null", (_t, done) => {
    holster.get("", data => {
      assert.equal(data, null)
      done()
    })
  })

  test("null key callback null", (_t, done) => {
    holster.get(null, data => {
      assert.equal(data, null)
      done()
    })
  })

  test("underscore as key callback null", (_t, done) => {
    holster.get("_", data => {
      assert.equal(data, null)
      done()
    })
  })

  test("get unknown key callback null", (_t, done) => {
    holster.get("unknown", data => {
      assert.equal(data, null)
      done()
    })
  })

  test("get unknown keys in for loop callbacks null", (_t, done) => {
    let count = 0
    for (let i = 0; i < 5; i++) {
      holster.get("unknown" + i, data => {
        assert.equal(data, null)
        count++
      })
    }
    setTimeout(() => {
      if (count === 5) done()
    }, 200)
  })

  test("nested unknown keys both callbacks null", (_t, done) => {
    holster.get("unknown", data => {
      assert.equal(data, null)

      holster.get("unknown", data => {
        assert.equal(data, null)
        done()
      })
    })
  })

  test("get already called callback null", (_t, done) => {
    holster.get("chained").get("unknown", data => {
      assert.equal(data, null)
      done()
    })
  })

  test("next chained unknown keys callback null", (_t, done) => {
    holster.get("chained").next("unknown", data => {
      assert.equal(data, null)
      done()
    })
  })

  test("nested next chained unknown keys callback null", (_t, done) => {
    holster.get("chained").next("unknown", data => {
      assert.equal(data, null)

      holster.get("chained").next("unknown", data => {
        assert.equal(data, null)
        done()
      })
    })
  })

  test("cleanup", (_t, done) => {
    fs.rm("test/holster.get", {recursive: true, force: true}, err => {
      assert.equal(err, null)
      done()
    })
  })
})
