import {describe, test} from "node:test"
import assert from "node:assert/strict"
import Radisk from "../src/radisk.js"

describe("radisk", () => {
  const puts: Record<string, string> = {}
  const opt = {
    write: 1,
    batch: 2,
    size: 100,
    cache: false,
    store: {
      get: (file: string, cb: (err?: string, data?: string) => void) => {
        cb(undefined, puts[file])
      },
      put: (file: string, data: string, cb: (err?: string) => void) => {
        puts[file] = data
        cb(undefined)
      },
      list: (cb: (file?: string) => void) => {
        Object.keys(puts).sort().forEach(cb)
        cb(undefined)
      },
    },
  }
  const radisk = Radisk(opt)
  const big =
    "file size is only 100 bytes so writing this value requires calling slice"

  test("write and read from memory", (_t, done) => {
    radisk("key", "value", () => {
      assert.deepEqual(puts, {"!": '\x1F+0\x1F#\x1F"key\x1F=\x1F"value\x1F\n'})
    })

    // Reading after write means radisk.batch is still available.
    radisk("key", (_err, value) => {
      assert.equal(value, "value")
      done()
    })
  })

  test("read from store", (_t, done) => {
    // Waiting until after opt.write means radisk.batch has been reset, so a
    // call to radisk.read is required which means the file needs to be parsed.
    setTimeout(() => {
      radisk("key", (_err, value) => {
        assert.equal(value, "value")
        done()
      })
    }, opt.write + 1)
  })

  test("write and read value with state", (_t, done) => {
    radisk("key", ["value", 1234])
    setTimeout(() => {
      assert.deepEqual(puts, {
        "!": '\x1F+0\x1F#\x1F"key\x1F=\x1F"value\x031234\x1F\n',
      })
      radisk("key", (_err, value) => {
        assert.deepEqual(value, ["value", 1234])
        done()
      })
    }, 10)
  })

  test("write and read a plain object is undefined", (_t, done) => {
    radisk("key", {object: true})
    setTimeout(() => {
      assert.deepEqual(puts, {
        "!": '\x1F+0\x1F#\x1F"key\x1F=undefined\n',
      })
      radisk("key", (_err, value) => {
        assert.deepEqual(value, undefined)
        done()
      })
    }, 10)
  })

  test("write a plain object with state is also undefined", (_t, done) => {
    radisk("key", [{object: true}, 1234])
    setTimeout(() => {
      assert.deepEqual(puts, {
        "!": '\x1F+0\x1F#\x1F"key\x1F=undefined\n',
      })
      radisk("key", (_err, value) => {
        assert.deepEqual(value, undefined)
        done()
      })
    }, 10)
  })

  test("write and read a soul relation is ok", (_t, done) => {
    const rel = {"#": "soul"}
    radisk("key", rel)
    setTimeout(() => {
      assert.deepEqual(puts, {
        "!": '\x1F+0\x1F#\x1F"key\x1F=\x1F#soul\x1F\n',
      })
      radisk("key", (_err, value) => {
        assert.deepEqual(value, rel)
        done()
      })
    }, 10)
  })

  test("write and read a soul relation with state is ok", (_t, done) => {
    const rel = [{"#": "soul"}, 1234]
    radisk("key", rel)
    setTimeout(() => {
      assert.deepEqual(puts, {
        "!": '\x1F+0\x1F#\x1F"key\x1F=\x1F#soul\x031234\x1F\n',
      })
      radisk("key", (_err, value) => {
        assert.deepEqual(value, rel)
        done()
      })
    }, 10)
  })

  test("write and read value with newline", (_t, done) => {
    radisk("key", "value\ncontinued")
    setTimeout(() => {
      assert.deepEqual(puts, {
        "!": '\x1F+0\x1F#\x1F"key\x1F=\x1F"value\ncontinued\x1F\n',
      })

      radisk("key", (_err, value) => {
        assert.equal(value, "value\ncontinued")
        done()
      })
    }, 10)
  })

  test("write and read value with newline and state", (_t, done) => {
    radisk("key", ["value\ncontinued", 12345])
    setTimeout(() => {
      assert.deepEqual(puts, {
        "!": '\x1F+0\x1F#\x1F"key\x1F=\x1F"value\ncontinued\x0312345\x1F\n',
      })

      radisk("key", (_err, value) => {
        assert.deepEqual(value, ["value\ncontinued", 12345])
        done()
      })
    }, 10)
  })

  test("write more than batch size", (_t, done) => {
    radisk("keyA", "valueA")
    radisk("keyB", "valueB")
    radisk("keyC", "valueC")
    setTimeout(() => {
      assert.deepEqual(puts, {
        "!":
          '\x1F+0\x1F#\x1F"key\x1F=\x1F"value\ncontinued\x0312345\x1F\n' +
          '\x1F+1\x1F#\x1F"A\x1F=\x1F"valueA\x1F\n' +
          '\x1F+1\x1F#\x1F"B\x1F=\x1F"valueB\x1F\n' +
          '\x1F+1\x1F#\x1F"C\x1F=\x1F"valueC\x1F\n',
      })
      radisk("keyA", (_err, value) => {
        assert.equal(value, "valueA")
        done()
      })
    }, 10)
  })

  test("write and read value bigger than file size", (_t, done) => {
    radisk("newFile", big)
    radisk("newFile", (_err, value) => {
      assert.equal(value, big)
    })
    setTimeout(() => {
      assert.deepEqual(puts, {
        "!":
          '\x1F+0\x1F#\x1F"key\x1F=\x1F"value\ncontinued\x0312345\x1F\n' +
          '\x1F+1\x1F#\x1F"A\x1F=\x1F"valueA\x1F\n' +
          '\x1F+1\x1F#\x1F"B\x1F=\x1F"valueB\x1F\n' +
          '\x1F+1\x1F#\x1F"C\x1F=\x1F"valueC\x1F\n',
        newFile: '\x1F+0\x1F#\x1F"newFile\x1F=\x1F"' + big + "\x1F\n",
      })
      radisk("newFile", (_err, value) => {
        assert.equal(value, big)
        done()
      })
    }, 10)
  })

  // This tests trying to add to a file that is already too long, so needs to
  // split again. Previously wasn't able to do this and would loop.
  test("write and read a small value after split", (_t, done) => {
    radisk("small", "small value")
    radisk("small", (_err, value) => {
      assert.equal(value, "small value")
    })
    setTimeout(() => {
      assert.deepEqual(puts, {
        "!":
          '\x1F+0\x1F#\x1F"key\x1F=\x1F"value\ncontinued\x0312345\x1F\n' +
          '\x1F+1\x1F#\x1F"A\x1F=\x1F"valueA\x1F\n' +
          '\x1F+1\x1F#\x1F"B\x1F=\x1F"valueB\x1F\n' +
          '\x1F+1\x1F#\x1F"C\x1F=\x1F"valueC\x1F\n',
        newFile: '\x1F+0\x1F#\x1F"newFile\x1F=\x1F"' + big + "\x1F\n",
        small: '\x1F+0\x1F#\x1F"small\x1F=\x1F"small value\x1F\n',
      })
      done()
    }, 10)
  })

  // This tests how existing files are handled when they are made smaller than
  // the maximum file size. (They should be kept as they are created so that
  // we don't have to deal with removing files.)
  test("write small value to newFile", (_t, done) => {
    radisk("newFile", "removed...")
    radisk("newFile", (_err, value) => {
      assert.equal(value, "removed...")
    })
    setTimeout(() => {
      assert.deepEqual(puts, {
        "!":
          '\x1F+0\x1F#\x1F"key\x1F=\x1F"value\ncontinued\x0312345\x1F\n' +
          '\x1F+1\x1F#\x1F"A\x1F=\x1F"valueA\x1F\n' +
          '\x1F+1\x1F#\x1F"B\x1F=\x1F"valueB\x1F\n' +
          '\x1F+1\x1F#\x1F"C\x1F=\x1F"valueC\x1F\n',
        newFile: '\x1F+0\x1F#\x1F"newFile\x1F=\x1F"removed...\x1F\n',
        small: '\x1F+0\x1F#\x1F"small\x1F=\x1F"small value\x1F\n',
      })
      done()
    }, 10)
  })

  // This tests ignoring the maximum file size when writing sub-keys. Since
  // radisk.write has already been called for a key it would previously get
  // duplicated when trying to split due to reaching the max length.
  test("write big value to first file", (_t, done) => {
    radisk("keyA", big)
    radisk("keyA", (_err, value) => {
      assert.equal(value, big)
    })
    setTimeout(() => {
      assert.deepEqual(puts, {
        "!":
          '\x1F+0\x1F#\x1F"key\x1F=\x1F"value\ncontinued\x0312345\x1F\n' +
          '\x1F+1\x1F#\x1F"A\x1F=\x1F"' +
          big +
          "\x1F\n" +
          '\x1F+1\x1F#\x1F"B\x1F=\x1F"valueB\x1F\n' +
          '\x1F+1\x1F#\x1F"C\x1F=\x1F"valueC\x1F\n',
        newFile: '\x1F+0\x1F#\x1F"newFile\x1F=\x1F"removed...\x1F\n',
        small: '\x1F+0\x1F#\x1F"small\x1F=\x1F"small value\x1F\n',
      })
      done()
    }, 10)
  })

  test("add to last file", (_t, done) => {
    radisk("smallContinued", "continued value")
    radisk("smallContinued", (_err, value) => {
      assert.equal(value, "continued value")
    })
    setTimeout(() => {
      assert.deepEqual(puts, {
        "!":
          '\x1F+0\x1F#\x1F"key\x1F=\x1F"value\ncontinued\x0312345\x1F\n' +
          '\x1F+1\x1F#\x1F"A\x1F=\x1F"' +
          big +
          "\x1F\n" +
          '\x1F+1\x1F#\x1F"B\x1F=\x1F"valueB\x1F\n' +
          '\x1F+1\x1F#\x1F"C\x1F=\x1F"valueC\x1F\n',
        newFile: '\x1F+0\x1F#\x1F"newFile\x1F=\x1F"removed...\x1F\n',
        small:
          '\x1F+0\x1F#\x1F"small\x1F=\x1F"small value\x1F\n' +
          '\x1F+1\x1F#\x1F"Continued\x1F=\x1F"continued value\x1F\n',
      })
      done()
    }, 10)
  })

  test("set a key to null and add a value to that file", (_t, done) => {
    radisk("keyC", null)
    radisk("keyC", (_err, value) => {
      assert.equal(value, null)
    })
    radisk("keyD", "valueD")
    radisk("keyD", (_err, value) => {
      assert.equal(value, "valueD")
    })
    setTimeout(() => {
      assert.deepEqual(puts, {
        "!":
          '\x1F+0\x1F#\x1F"key\x1F=\x1F"value\ncontinued\x0312345\x1F\n' +
          '\x1F+1\x1F#\x1F"A\x1F=\x1F"' +
          big +
          "\x1F\n" +
          '\x1F+1\x1F#\x1F"B\x1F=\x1F"valueB\x1F\n' +
          '\x1F+1\x1F#\x1F"C\x1F=\x1F \x1F\n' +
          '\x1F+1\x1F#\x1F"D\x1F=\x1F"valueD\x1F\n',
        newFile: '\x1F+0\x1F#\x1F"newFile\x1F=\x1F"removed...\x1F\n',
        small:
          '\x1F+0\x1F#\x1F"small\x1F=\x1F"small value\x1F\n' +
          '\x1F+1\x1F#\x1F"Continued\x1F=\x1F"continued value\x1F\n',
      })
      done()
    }, 10)
  })

  test("set first keys to null", (_t, done) => {
    radisk("key", null)
    radisk("key", (_err, value) => {
      assert.equal(value, null)
    })
    radisk("keyA", null)
    radisk("keyA", (_err, value) => {
      assert.equal(value, null)
    })
    setTimeout(() => {
      assert.deepEqual(puts, {
        "!":
          '\x1F+0\x1F#\x1F"key\x1F=\x1F \x1F\n' +
          '\x1F+1\x1F#\x1F"A\x1F=\x1F \x1F\n' +
          '\x1F+1\x1F#\x1F"B\x1F=\x1F"valueB\x1F\n' +
          '\x1F+1\x1F#\x1F"C\x1F=\x1F \x1F\n' +
          '\x1F+1\x1F#\x1F"D\x1F=\x1F"valueD\x1F\n',
        newFile: '\x1F+0\x1F#\x1F"newFile\x1F=\x1F"removed...\x1F\n',
        small:
          '\x1F+0\x1F#\x1F"small\x1F=\x1F"small value\x1F\n' +
          '\x1F+1\x1F#\x1F"Continued\x1F=\x1F"continued value\x1F\n',
      })
      done()
    }, 10)
  })
})

