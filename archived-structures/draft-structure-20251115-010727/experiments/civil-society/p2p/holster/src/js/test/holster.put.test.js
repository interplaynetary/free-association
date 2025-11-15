import fs from "fs"
import {Server} from "mock-socket"
import {describe, test} from "node:test"
import assert from "node:assert/strict"
import Holster from "../src/holster.js"

describe("holster.put", () => {
  const wss = new Server("ws://localhost:1234")
  const holster = Holster({file: "test/holster.put", wss: wss, maxAge: 100})

  test("put and get string on root", (t, done) => {
    holster.get("key").put("value", err => {
      assert.equal(err, null)

      holster.get("key", data => {
        assert.equal(data, "value")
        done()
      })
    })
  })

  test("put value to null on root", (t, done) => {
    holster.get("key").put(null, err => {
      assert.equal(err, null)

      holster.get("key", data => {
        assert.equal(data, null)
        done()
      })
    })
  })

  test("put and get true on root", (t, done) => {
    holster.get("true").put(true, err => {
      assert.equal(err, null)

      holster.get("true", data => {
        assert.equal(data, true)
        done()
      })
    })
  })

  test("put and get false on root", (t, done) => {
    holster.get("false").put(false, err => {
      assert.equal(err, null)

      holster.get("false", data => {
        assert.equal(data, false)
        done()
      })
    })
  })

  test("put and get number on root", (t, done) => {
    holster.get("pi").put(3.14159, err => {
      assert.equal(err, null)

      holster.get("pi", data => {
        assert.equal(data, 3.14159)
        done()
      })
    })
  })

  test("put and get values in for loop on root", (t, done) => {
    for (let i = 0; i < 5; i++) {
      holster.get("for" + i).put(i, err => {
        assert.equal(err, null)
      })
    }

    setTimeout(() => {
      let count = 0
      for (let i = 0; i < 5; i++) {
        holster.get("for" + i, data => {
          assert.equal(data, i)
          count++
        })
      }
      setTimeout(() => {
        if (count === 5) done()
      }, 200)
    }, 200)
  })

  test("chained get before put", (t, done) => {
    holster
      .get("hello")
      .next("world!")
      .put("ok", err => {
        assert.equal(err, null)

        holster.get("hello").next("world!", data => {
          assert.equal(data, "ok")
          done()
        })
      })
  })

  test("more chained gets before put", (t, done) => {
    holster
      .get("1")
      .next("2")
      .next("3")
      .put("4", err => {
        assert.equal(err, null)

        holster
          .get("1")
          .next("2")
          .next("3", data => {
            assert.equal(data, "4")
            done()
          })
      })
  })

  test("put and get object on root in graph format", (t, done) => {
    const plain = {
      key: "plain value",
      true: true,
      false: false,
      number: 42,
    }
    holster.get("plain").put(plain, err => {
      assert.equal(err, null)

      holster.get("plain", data => {
        assert.deepEqual(data, plain)
        done()
      })
    })
  })

  test("put object to null on root", (t, done) => {
    holster.get("plain").put(null, err => {
      assert.equal(err, null)

      holster.get("plain", data => {
        assert.equal(data, null)
        done()
      })
    })
  })

  test("chained get before put object in graph format", (t, done) => {
    const plain = {
      key: "hello plain value",
    }
    holster
      .get("hello")
      .next("plain")
      .put(plain, err => {
        assert.equal(err, null)

        holster.get("hello").next("plain", data => {
          assert.deepEqual(data, plain)
          done()
        })
      })
  })

  test("put and get nested object", (t, done) => {
    const nested = {
      key: "nested value",
      child: {
        has: "child value",
      },
    }
    holster.get("nested").put(nested, err => {
      assert.equal(err, null)

      // Getting a nested object requires waiting for radisk to write to disk,
      // as it will batch the writes. (Default wait is 1 millisecond.)
      setTimeout(() => {
        holster.get("nested", data => {
          assert.deepEqual(data, nested)

          holster.get("nested").next("key", data => {
            assert.equal(data, "nested value")

            holster.get("nested").next("child", data => {
              assert.deepEqual(data, {has: "child value"})
              done()
            })
          })
        })
      }, 2)
    })
  })

  test("chained get before put nested object", (t, done) => {
    const nested = {
      key: "hello nested value",
      child: {
        has: "hello child value",
      },
    }
    holster
      .get("hello")
      .next("nested")
      .put(nested, err => {
        assert.equal(err, null)

        setTimeout(() => {
          holster.get("hello").next("nested", data => {
            assert.deepEqual(data, nested)

            holster
              .get("hello")
              .next("nested")
              .next("key", data => {
                assert.equal(data, "hello nested value")

                holster
                  .get("hello")
                  .next("nested")
                  .next("child", data => {
                    assert.deepEqual(data, {has: "hello child value"})
                    done()
                  })
              })
          })
        }, 2)
      })
  })

  test("put and get two nested objects", (t, done) => {
    const two = {
      key: "two nested values",
      child1: {
        has: "child value 1",
      },
      child2: {
        has: "child value 2",
      },
    }
    holster.get("two").put(two, err => {
      assert.equal(err, null)

      setTimeout(() => {
        holster.get("two", data => {
          assert.deepEqual(data, two)

          holster.get("two").next("child1", data => {
            assert.deepEqual(data, {has: "child value 1"})

            holster.get("two").next("child2", data => {
              assert.deepEqual(data, {has: "child value 2"})
              done()
            })
          })
        })
      }, 2)
    })
  })

  test("put and get multiple nested object", (t, done) => {
    const multiple = {
      key: "multiple nested values",
      child: {
        has: "child value",
        grandchild: {
          has: "grandchild value",
          other: "(not returned in lex query)",
        },
      },
    }
    holster.get("multiple").put(multiple, err => {
      assert.equal(err, null)

      setTimeout(() => {
        holster.get("multiple", data => {
          assert.deepEqual(data, multiple)

          holster.get("multiple").next("child", {".": "has"}, data => {
            assert.deepEqual(data, {has: "child value"})

            holster
              .get("multiple")
              .next("child")
              .next("grandchild", {".": "has"}, data => {
                assert.deepEqual(data, {has: "grandchild value"})
                done()
              })
          })
        })
      }, 2)
    })
  })

  test("put string with set", (t, done) => {
    holster.get("set").put("first", true, err => {
      assert.equal(err, null)

      holster.get("set", data => {
        assert.equal(Object.values(data)[0], "first")

        setTimeout(() => {
          holster.get("set").put("second", true, err => {
            assert.equal(err, null)

            holster.get("set", data => {
              for (const value of Object.values(data)) {
                assert.ok(value === "first" || value === "second")
              }
              done()
            })
          })
        }, 2)
      })
    })
  })

  test("put object with set", (t, done) => {
    const set1 = {
      key: "value 1",
      child: "child value 1",
    }
    const set2 = {
      key: "value 2",
      child: "child value 2",
    }
    holster.get("set2").put(set1, true, err => {
      assert.equal(err, null)

      holster.get("set2", data => {
        assert.deepEqual(Object.values(data)[0], set1)

        setTimeout(() => {
          holster.get("set2").put(set2, true, err => {
            assert.equal(err, null)

            holster.get("set2", data => {
              for (const value of Object.values(data)) {
                if (value.key === "value 1") {
                  assert.deepEqual(value, set1)
                }
                if (value.key === "value 2") {
                  assert.deepEqual(value, set2)
                }
              }
              done()
            })
          })
        }, 2)
      })
    })
  })

  test("cleanup", (t, done) => {
    fs.rm("test/holster.put", {recursive: true, force: true}, err => {
      assert.equal(err, null)
      done()
    })
  })
})
