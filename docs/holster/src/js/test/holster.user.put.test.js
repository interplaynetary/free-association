import fs from "fs"
import {Server} from "mock-socket"
import {describe, test} from "node:test"
import assert from "node:assert/strict"
import Holster from "../../holster.js"

describe("holster.user.put", () => {
  const wss = new Server("ws://localhost:1234")
  const holster = Holster({
    file: "test/holster.user.put",
    wss: wss,
    maxAge: 100,
  })
  const user = holster.user()
  let alice = ""

  test("user create", (t, done) => {
    user.create("alice", "password", err => {
      assert.equal(err, null)
      done()
    })
  })

  test("user auth", (t, done) => {
    user.auth("alice", "password", err => {
      assert.equal(err, null)
      assert.equal(user.is.username, "alice")
      alice = user.is.pub
      done()
    })
  })

  test("put and get string", (t, done) => {
    user.get("key").put("value", err => {
      assert.equal(err, null)

      user.get("key", data => {
        assert.equal(data, "value")
        done()
      })
    })
  })

  test("put value to null", (t, done) => {
    user.get("key").put(null, err => {
      assert.equal(err, null)

      user.get("key", data => {
        assert.equal(data, null)
        done()
      })
    })
  })

  test("put and get true", (t, done) => {
    user.get("true").put(true, err => {
      assert.equal(err, null)

      user.get("true", data => {
        assert.equal(data, true)
        done()
      })
    })
  })

  test("put and get false", (t, done) => {
    user.get("false").put(false, err => {
      assert.equal(err, null)

      user.get("false", data => {
        assert.equal(data, false)
        done()
      })
    })
  })

  test("put and get number", (t, done) => {
    user.get("pi").put(3.14159, err => {
      assert.equal(err, null)

      user.get("pi", data => {
        assert.equal(data, 3.14159)
        done()
      })
    })
  })

  test("put and get values in for loop", (t, done) => {
    ;(async () => {
      for (let i = 0; i < 5; i++) {
        // Need to be careful putting user properties in a loop as they are
        // verified per node, so wait for each update to return.
        const err = await new Promise(res => {
          user.get("for" + i).put(i, res)
        })
        assert.equal(err, null)
      }
    })()

    setTimeout(async () => {
      let count = 0
      for (let i = 0; i < 5; i++) {
        user.get("for" + i, data => {
          assert.equal(data, i)
          count++
        })
      }
      setTimeout(() => {
        if (count === 5) done()
      }, 200)
    }, 500)
  })

  test("chained get before put", (t, done) => {
    user
      .get("hello")
      .next("world!")
      .put("ok", err => {
        assert.equal(err, null)

        user.get("hello").next("world!", data => {
          assert.equal(data, "ok")
          done()
        })
      })
  })

  test("more chained gets before put", (t, done) => {
    user
      .get("1")
      .next("2")
      .next("3")
      .put("4", err => {
        assert.equal(err, null)

        user
          .get("1")
          .next("2")
          .next("3", data => {
            assert.equal(data, "4")
            done()
          })
      })
  })

  test("put and get object in graph format", (t, done) => {
    const plain = {
      key: "plain value",
      true: true,
      false: false,
      number: 42,
    }
    user.get("plain").put(plain, err => {
      assert.equal(err, null)

      user.get("plain", data => {
        assert.deepEqual(data, plain)
        done()
      })
    })
  })

  test("put object to null", (t, done) => {
    user.get("plain").put(null, err => {
      assert.equal(err, null)

      user.get("plain", data => {
        assert.equal(data, null)
        done()
      })
    })
  })

  test("chained get before put object in graph format", (t, done) => {
    const plain = {
      key: "hello plain value",
    }
    user
      .get("hello")
      .next("plain")
      .put(plain, err => {
        assert.equal(err, null)

        user.get("hello").next("plain", data => {
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
    user.get("nested").put(nested, err => {
      assert.equal(err, null)

      // Getting a nested object requires waiting for radisk to write to disk,
      // as it will batch the writes. (Default wait is 1 millisecond.)
      setTimeout(() => {
        user.get("nested", data => {
          assert.deepEqual(data, nested)

          user.get("nested").next("key", data => {
            assert.equal(data, "nested value")

            user.get("nested").next("child", data => {
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
    user
      .get("hello")
      .next("nested")
      .put(nested, err => {
        assert.equal(err, null)

        setTimeout(() => {
          user.get("hello").next("nested", data => {
            assert.deepEqual(data, nested)

            user
              .get("hello")
              .next("nested")
              .next("key", data => {
                assert.equal(data, "hello nested value")

                user
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
    user.get("two").put(two, err => {
      assert.equal(err, null)

      setTimeout(() => {
        user.get("two", data => {
          assert.deepEqual(data, two)

          user.get("two").next("child1", data => {
            assert.deepEqual(data, {has: "child value 1"})

            user.get("two").next("child2", data => {
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
    user.get("multiple").put(multiple, err => {
      assert.equal(err, null)

      setTimeout(() => {
        user.get("multiple", data => {
          assert.deepEqual(data, multiple)

          user.get("multiple").next("child", {".": "has"}, data => {
            assert.deepEqual(data, {has: "child value"})

            user
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
    user.get("set").put("first", true, err => {
      assert.equal(err, null)

      setTimeout(() => {
        user.get("set", data => {
          assert.equal(Object.values(data)[0], "first")

          user.get("set").put("second", true, err => {
            assert.equal(err, null)

            setTimeout(() => {
              user.get("set", data => {
                for (const value of Object.values(data)) {
                  assert.ok(value === "first" || value === "second")
                }
                done()
              })
            }, 2)
          })
        })
      }, 2)
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
    user.get("set2").put(set1, true, err => {
      assert.equal(err, null)

      setTimeout(() => {
        user.get("set2", data => {
          assert.deepEqual(Object.values(data)[0], set1)

          user.get("set2").put(set2, true, err => {
            assert.equal(err, null)

            setTimeout(() => {
              user.get("set2", data => {
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
            }, 100)
          })
        })
      }, 100)
    })
  })

  test("get number using public key - logged in", (t, done) => {
    user.get([alice, "pi"], data => {
      assert.equal(data, 3.14159)
      done()
    })
  })

  test("get number using public key - logged out", (t, done) => {
    user.leave()

    user.get([alice, "pi"], data => {
      assert.equal(data, 3.14159)
      done()
    })
  })

  test("chained get in graph format using public key", (t, done) => {
    const plain = {
      key: "hello plain value",
    }
    user.get([alice, "hello"]).next("plain", data => {
      assert.deepEqual(data, plain)
      done()
    })
  })

  test("get nested object using public key", (t, done) => {
    const nested = {
      key: "nested value",
      child: {
        has: "child value",
      },
    }
    user.get([alice, "nested"], data => {
      assert.deepEqual(data, nested)

      user.get([alice, "nested"]).next("key", data => {
        assert.equal(data, "nested value")

        user.get([alice, "nested"]).next("child", data => {
          assert.deepEqual(data, {has: "child value"})
          done()
        })
      })
    })
  })

  test("chained get nested object using public key", (t, done) => {
    const nested = {
      key: "hello nested value",
      child: {
        has: "hello child value",
      },
    }
    user.get([alice, "hello"]).next("nested", data => {
      assert.deepEqual(data, nested)

      user
        .get([alice, "hello"])
        .next("nested")
        .next("key", data => {
          assert.equal(data, "hello nested value")

          user
            .get([alice, "hello"])
            .next("nested")
            .next("child", data => {
              assert.deepEqual(data, {has: "hello child value"})
              done()
            })
        })
    })
  })

  test("cleanup", (t, done) => {
    setTimeout(() => {
      fs.rm("test/holster.user.put", {recursive: true, force: true}, err => {
        assert.equal(err, null)
        done()
      })
    }, 100)
  })
})
