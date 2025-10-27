import {describe, test} from "node:test"
import assert from "node:assert/strict"
import Store from "../src/store.js"
import fs from "fs"

describe("store", () => {
  const kitty = Store({file: "test/kitty"})
  const multiple = Store({file: "test/multiple", size: 100})
  const special = Store({file: "test/special"})
  const newFileValue =
    "file size is only 100 bytes so writing this value requires calling slice"

  test("get graph", (t, done) => {
    kitty.get({"#": "FDSA"}, (err, value) => {
      assert.deepEqual(value, {
        FDSA: {
          _: {
            "#": "FDSA",
            ">": {
              color: 3,
              name: 2,
              slave: 2,
              species: 2,
            },
          },
          color: "ginger",
          name: "Fluffy",
          slave: {
            "#": "ASDF",
          },
          species: "felis silvestris",
        },
      })
      done()
    })
  })

  test("get item", (t, done) => {
    kitty.get({"#": "FDSA", ".": "species"}, (err, value) => {
      assert.deepEqual(value, {
        FDSA: {
          _: {
            "#": "FDSA",
            ">": {
              species: 2,
            },
          },
          species: "felis silvestris",
        },
      })
      done()
    })
  })

  test("put keys to multiple files", (t, done) => {
    multiple.put(
      {
        keyA: {
          _: {"#": "keyA", ">": {value: 1}},
          value: "valueA",
        },
        keyB: {
          _: {"#": "keyB", ">": {value: 1}},
          value: "valueB",
        },
        keyC: {
          _: {"#": "keyC", ">": {value: 1, extraField: 1}},
          value: "valueC",
          extraField: true,
        },
        newFile: {
          _: {"#": "newFile", ">": {value: 1}},
          value: newFileValue,
        },
      },
      err => {
        assert.equal(err, null)
        fs.access("test/multiple/!", err => {
          assert.equal(err, null)
          fs.access("test/multiple/newFile", err => {
            assert.equal(err, null)
            done()
          })
        })
      },
    )
  })

  test("get key from first file", (t, done) => {
    multiple.get({"#": "keyA"}, (err, value) => {
      assert.deepEqual(value, {
        keyA: {
          _: {
            "#": "keyA",
            ">": {
              value: 1,
            },
          },
          value: "valueA",
        },
      })
      done()
    })
  })

  test("get another key from first file", (t, done) => {
    multiple.get({"#": "keyC"}, (err, value) => {
      assert.deepEqual(value, {
        keyC: {
          _: {
            "#": "keyC",
            ">": {
              extraField: 1,
              value: 1,
            },
          },
          extraField: true,
          value: "valueC",
        },
      })
    })
    multiple.get({"#": "keyC", ".": "value"}, (err, value) => {
      assert.deepEqual(value, {
        keyC: {
          _: {
            "#": "keyC",
            ">": {
              value: 1,
            },
          },
          value: "valueC",
        },
      })
      done()
    })
  })

  test("get key from second file", (t, done) => {
    multiple.get({"#": "newFile"}, (err, value) => {
      assert.deepEqual(value, {
        newFile: {
          _: {
            "#": "newFile",
            ">": {
              value: 1,
            },
          },
          value: newFileValue,
        },
      })
      fs.rm("test/multiple", {recursive: true, force: true}, err => {
        assert.equal(err, null)
        done()
      })
    })
  })

  test("put keys with special characters", (t, done) => {
    special.put(
      {
        "#": {
          _: {"#": "#", ">": {value: 1}},
          value: "ok got #",
        },
        ">": {
          _: {"#": ">", ">": {value: 1, extraField: 1}},
          value: "ok got >",
          extraField: true,
        },
        ".": {
          _: {"#": ".", ">": {value: 1, extraField: 1}},
          value: "ok got .",
          extraField: true,
        },
        "+": {
          _: {"#": "+", ">": {value: 1}},
          value: "ok got +",
        },
        "[": {
          _: {"#": "[", ">": {value: 1}},
          value: "ok got [",
        },
        ",": {
          _: {"#": ",", ">": {value: 1}},
          value: "ok got ,",
        },
        '"': {
          _: {"#": '"', ">": {value: 1}},
          value: 'ok got "',
        },
        _: {
          _: {"#": "_", ">": {value: 1}},
          value: "ok got _",
        },
      },
      err => {
        assert.equal(err, null)
        fs.access("test/special/!", err => {
          assert.equal(err, null)
          done()
        })
      },
    )
  })

  test("get keys with special characters", (t, done) => {
    special.get({"#": "#"}, (err, value) => {
      assert.deepEqual(value, {
        "#": {
          _: {
            "#": "#",
            ">": {
              value: 1,
            },
          },
          value: "ok got #",
        },
      })
    })
    special.get({"#": ">", ".": "value"}, (err, value) => {
      assert.deepEqual(value, {
        ">": {
          _: {
            "#": ">",
            ">": {
              value: 1,
            },
          },
          value: "ok got >",
        },
      })
    })
    special.get({"#": ".", ".": "value"}, (err, value) => {
      assert.deepEqual(value, {
        ".": {
          _: {
            "#": ".",
            ">": {
              value: 1,
            },
          },
          value: "ok got .",
        },
      })
    })
    special.get({"#": "+"}, (err, value) => {
      assert.deepEqual(value, {
        "+": {
          _: {
            "#": "+",
            ">": {
              value: 1,
            },
          },
          value: "ok got +",
        },
      })
    })
    special.get({"#": "["}, (err, value) => {
      assert.deepEqual(value, {
        "[": {
          _: {
            "#": "[",
            ">": {
              value: 1,
            },
          },
          value: "ok got [",
        },
      })
    })
    special.get({"#": ","}, (err, value) => {
      assert.deepEqual(value, {
        ",": {
          _: {
            "#": ",",
            ">": {
              value: 1,
            },
          },
          value: "ok got ,",
        },
      })
    })
    special.get({"#": '"'}, (err, value) => {
      assert.deepEqual(value, {
        '"': {
          _: {
            "#": '"',
            ">": {
              value: 1,
            },
          },
          value: 'ok got "',
        },
      })
    })
    special.get({"#": "_"}, (err, value) => {
      assert.deepEqual(value, {
        _: {
          _: {
            "#": "_",
            ">": {
              value: 1,
            },
          },
          value: "ok got _",
        },
      })
      fs.rm("test/special", {recursive: true, force: true}, err => {
        assert.equal(err, null)
        done()
      })
    })
  })
})
