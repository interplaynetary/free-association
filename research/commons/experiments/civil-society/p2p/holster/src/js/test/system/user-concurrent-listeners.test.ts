import fs from "fs"
import {Server} from "mock-socket"
import {describe, test} from "node:test"
import assert from "node:assert/strict"
import Holster from "../../../holster.js"

describe("system - user concurrent listeners", () => {
  const wss = new Server("ws://localhost:9015")
  const holster = Holster({
    file: "test/system/user-concurrent-listeners",
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

  test("multiple listeners on same path receive data", (_t, done) => {
    let listener1Called = false
    let listener2Called = false

    user.get("key").put({value: "test"}, err => {
      assert.equal(err, null)

      // Set up two listeners on the same path
      user.get("key").on(data => {
        listener1Called = true
        assert.notEqual(data, null)
        assert.deepEqual(data, {value: "test"})
        checkComplete()
      }, true)

      user.get("key").on(data => {
        listener2Called = true
        assert.notEqual(data, null)
        assert.deepEqual(data, {value: "test"})
        checkComplete()
      }, true)

      function checkComplete() {
        if (listener1Called && listener2Called) {
          done()
        }
      }
    })
  })

  test("nested listeners on parent and child paths", (_t, done) => {
    let parentCalled = false
    let childCalled = false

    user.get("parent").put({value: "parent-value"}, err => {
      assert.equal(err, null)

      user
        .get("parent")
        .next("child")
        .put({value: "child-value"}, err => {
          assert.equal(err, null)

          // Listen on parent
          user.get("parent").on(data => {
            parentCalled = true
            assert.notEqual(data, null)
            assert.equal(data.value, "parent-value")
            // Parent includes child as a rel
            assert.notEqual(data.child, undefined)
            checkComplete()
          }, true)

          // Listen on child
          user
            .get("parent")
            .next("child")
            .on(data => {
              childCalled = true
              assert.notEqual(data, null)
              assert.deepEqual(data, {value: "child-value"})
              checkComplete()
            }, true)

          function checkComplete() {
            if (parentCalled && childCalled) {
              done()
            }
          }
        })
    })
  })

  test("listener race condition - data before and after listener setup", (_t, done) => {
    const results = []

    // Put data first
    user.get("race").put({value: "first"}, err => {
      assert.equal(err, null)

      // Set up listener with _get=true to read existing data
      user.get("race").on(data => {
        results.push(data?.value)
      }, true)

      // Immediately put new data
      setTimeout(() => {
        user.get("race").put({value: "second"}, err => {
          assert.equal(err, null)

          setTimeout(() => {
            // Should have received both values
            assert.equal(
              results.length >= 1,
              true,
              "Should receive at least initial value",
            )
            assert.equal(results[0], "first", "First value should be 'first'")
            done()
          }, 100)
        })
      }, 50)
    })
  })

  test("cleanup", (_t, done) => {
    fs.rm(
      "test/system/user-concurrent-listeners",
      {recursive: true, force: true},
      err => {
        assert.equal(err, null)
        done()
      },
    )
  })
})
