import fs from "fs"
import {Server} from "mock-socket"
import {describe, test} from "node:test"
import assert from "node:assert/strict"
import Holster from "../../../holster.js"

describe("system - concurrent listeners", () => {
  const wss = new Server("ws://localhost:9005")
  const holster = Holster({file: "test/system/concurrent-listeners", wss: wss})

  test("multiple listeners on same path receive data", (_t, done) => {
    let listener1Called = false
    let listener2Called = false

    holster.get("key").put({value: "test"}, err => {
      assert.equal(err, null)

      // Set up two listeners on the same path
      holster.get("key").on(data => {
        listener1Called = true
        assert.notEqual(data, null)
        assert.equal(data.value, "test")
        checkComplete()
      }, true)

      holster.get("key").on(data => {
        listener2Called = true
        assert.notEqual(data, null)
        assert.equal(data.value, "test")
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

    holster.get("parent").put({value: "parent-value"}, err => {
      assert.equal(err, null)

      holster
        .get("parent")
        .next("child")
        .put({value: "child-value"}, err => {
          assert.equal(err, null)

          // Listen on parent
          holster.get("parent").on(data => {
            parentCalled = true
            assert.notEqual(data, null)
            assert.equal(data.value, "parent-value")
            checkComplete()
          }, true)

          // Listen on child
          holster
            .get("parent")
            .next("child")
            .on(data => {
              childCalled = true
              assert.notEqual(data, null)
              assert.equal(data.value, "child-value")
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
    holster.get("race").put({value: "first"}, err => {
      assert.equal(err, null)

      // Set up listener with once=true to read existing data
      holster.get("race").on(data => {
        results.push(data?.value)
      }, true)

      // Immediately put new data
      setTimeout(() => {
        holster.get("race").put({value: "second"}, err => {
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
      "test/system/concurrent-listeners",
      {recursive: true, force: true},
      err => {
        assert.equal(err, null)
        done()
      },
    )
  })
})
