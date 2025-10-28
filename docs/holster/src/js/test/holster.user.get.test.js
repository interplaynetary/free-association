import fs from "fs"
import {Server} from "mock-socket"
import {describe, test} from "node:test"
import assert from "node:assert/strict"
import Holster from "../src/holster.js"

describe("holster.user.get", () => {
  const wss = new Server("ws://localhost:1234")
  const holster = Holster({
    file: "test/holster.user.get",
    wss: wss,
    maxAge: 100,
  })
  const user = holster.user()

  test("user is not authenticated", (t, done) => {
    user.get("test", data => {
      assert.equal(data, null)
      done()
    })
  })

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

  test("empty key callback null", (t, done) => {
    user.get("", data => {
      assert.equal(data, null)
      done()
    })
  })

  test("null key callback null", (t, done) => {
    user.get(null, data => {
      assert.equal(data, null)
      done()
    })
  })

  test("underscore as key callback null", (t, done) => {
    user.get("_", data => {
      assert.equal(data, null)
      done()
    })
  })

  test("get unknown key callback null", (t, done) => {
    user.get("unknown", data => {
      assert.equal(data, null)
      done()
    })
  })

  test("get unknown keys in for loop callbacks null", (t, done) => {
    let count = 0
    for (let i = 0; i < 5; i++) {
      user.get("unknown" + i, data => {
        assert.equal(data, null)
        count++
      })
    }
    setTimeout(() => {
      if (count === 5) done()
    }, 200)
  })

  test("nested get unknown key callback null", (t, done) => {
    user.get("unknown", data => {
      assert.equal(data, null)

      user.get("unknown", data => {
        assert.equal(data, null)
        done()
      })
    })
  })

  test("next chained unknown keys callback null", (t, done) => {
    user.get("chained").next("unknown", data => {
      assert.equal(data, null)
      done()
    })
  })

  test("nested next chained unknown keys callback null", (t, done) => {
    user.get("chained").next("unknown", data => {
      assert.equal(data, null)

      user.get("chained").next("unknown", data => {
        assert.equal(data, null)
        done()
      })
    })
  })

  test("cleanup", (t, done) => {
    fs.rm("test/holster.user.get", {recursive: true, force: true}, err => {
      assert.equal(err, null)
      done()
    })
  })
})
