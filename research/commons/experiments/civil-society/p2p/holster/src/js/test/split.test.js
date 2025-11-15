import {describe, test} from "node:test"
import assert from "node:assert/strict"
import Radisk from "../src/radisk.js"

// ASCII character for enquiry.
const enq = String.fromCharCode(5)

describe("split", () => {
  const puts = {}
  const opt = {
    size: 100,
    store: {
      get: (file, cb) => {
        cb(null, puts[file])
      },
      put: (file, data, cb) => {
        puts[file] = data
        cb(null)
      },
      list: cb => {
        Object.keys(puts).sort().forEach(cb)
        cb(null)
      },
    },
  }
  const radisk = Radisk(opt)
  const now = Date.now()
  const lorem =
    "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."
  const parent = () => {
    return (
      '\x1F+0\x1F#\x1F"parent\x05\x1F\n' +
      '\x1F+1\x1F#\x1F"boolean\x1F=\x1F+\x03' +
      now +
      "\x1F\n" +
      '\x1F+1\x1F#\x1F"number\x1F=\x1F+42\x03' +
      now +
      "\x1F\n" +
      '\x1F+1\x1F#\x1F"rel\x1F=\x1F#child\x03' +
      now +
      "\x1F\n" +
      '\x1F+1\x1F#\x1F"string\x1F=\x1F"parent value\x03' +
      now +
      "\x1F\n"
    )
  }
  const child = () => {
    return (
      '\x1F+0\x1F#\x1F"child\x05\x1F\n' +
      '\x1F+1\x1F#\x1F"boolean\x1F=\x1F+\x03' +
      now +
      "\x1F\n" +
      '\x1F+1\x1F#\x1F"number\x1F=\x1F+43\x03' +
      now +
      "\x1F\n" +
      '\x1F+1\x1F#\x1F"rel\x1F=\x1F#grandchild\x03' +
      now +
      "\x1F\n" +
      '\x1F+1\x1F#\x1F"string\x1F=\x1F"child value\x03' +
      now +
      "\x1F\n"
    )
  }
  const grandchild = () => {
    return (
      '\x1F+0\x1F#\x1F"grandchild\x05\x1F\n' +
      '\x1F+1\x1F#\x1F"boolean\x1F=\x1F-\x03' +
      now +
      "\x1F\n" +
      '\x1F+1\x1F#\x1F"number\x1F=\x1F+44\x03' +
      now +
      "\x1F\n" +
      '\x1F+1\x1F#\x1F"rel\x1F=\x1F#toys\x03' +
      now +
      "\x1F\n" +
      '\x1F+1\x1F#\x1F"string\x1F=\x1F"grandchild value\x03' +
      now +
      "\x1F\n"
    )
  }
  const toys = () => {
    return (
      '\x1F+0\x1F#\x1F"toys\x05\x1F\n' +
      '\x1F+1\x1F#\x1F"b\x1F\n' +
      '\x1F+2\x1F#\x1F"all\x1F=\x1F"green\x03' +
      now +
      "\x1F\n" +
      '\x1F+2\x1F#\x1F"ook\x1F=\x1F"' +
      lorem +
      "\x03" +
      now +
      "\x1F\n" +
      '\x1F+1\x1F#\x1F"car\x1F=\x1F"red\x03' +
      now +
      "\x1F\n"
    )
  }
  const alpha = () => {
    return (
      '\x1F+0\x1F#\x1F"alpha\x05\x1F\n' +
      '\x1F+1\x1F#\x1F"boolean\x1F=\x1F+\x03' +
      now +
      "\x1F\n" +
      '\x1F+1\x1F#\x1F"number\x1F=\x1F+1\x03' +
      now +
      "\x1F\n" +
      '\x1F+1\x1F#\x1F"rel\x1F=\x1F#beta\x03' +
      now +
      "\x1F\n" +
      '\x1F+1\x1F#\x1F"string\x1F=\x1F"alpha value\x03' +
      now +
      "\x1F\n"
    )
  }

  test("write a parent node", (t, done) => {
    radisk("parent" + enq + "string", ["parent value", now])
    radisk("parent" + enq + "number", [42, now])
    radisk("parent" + enq + "boolean", [true, now])
    radisk("parent" + enq + "rel", [{"#": "child"}, now])

    setTimeout(() => {
      assert.deepEqual(puts, {
        "!": parent(),
      })
      done()
    }, 10)
  })

  test("write a child node", (t, done) => {
    radisk("child" + enq + "string", ["child value", now])
    radisk("child" + enq + "number", [43, now])
    radisk("child" + enq + "boolean", [true, now])
    radisk("child" + enq + "rel", [{"#": "grandchild"}, now])

    setTimeout(() => {
      assert.deepEqual(puts, {
        "!": child(),
        parent: parent(),
      })
      done()
    }, 10)
  })

  test("write a grandchild node", (t, done) => {
    radisk("grandchild" + enq + "string", ["grandchild value", now])
    radisk("grandchild" + enq + "number", [44, now])
    radisk("grandchild" + enq + "boolean", [false, now])
    radisk("grandchild" + enq + "rel", [{"#": "toys"}, now])

    setTimeout(() => {
      assert.deepEqual(puts, {
        "!": child(),
        parent: parent(),
        grandchild: grandchild(),
      })
      done()
    }, 10)
  })

  test("write grandchild toys", (t, done) => {
    radisk("toys" + enq + "car", ["red", now])
    radisk("toys" + enq + "ball", ["green", now])
    radisk("toys" + enq + "book", [lorem, now])

    setTimeout(() => {
      assert.deepEqual(puts, {
        "!": child(),
        parent: parent(),
        grandchild: grandchild(),
        toys: toys(),
      })
      done()
    }, 10)
  })

  test("write new first node", (t, done) => {
    radisk("alpha" + enq + "string", ["alpha value", now])
    radisk("alpha" + enq + "number", [1, now])
    radisk("alpha" + enq + "boolean", [true, now])
    radisk("alpha" + enq + "rel", [{"#": "beta"}, now])
    radisk("beta" + enq + "string", ["beta value", now])

    setTimeout(() => {
      assert.deepEqual(puts, {
        "!": alpha(),
        beta:
          '\x1F+0\x1F#\x1F"beta\x05string\x1F=\x1F"beta value\x03' +
          now +
          "\x1F\n" +
          child(),
        parent: parent(),
        grandchild: grandchild(),
        toys: toys(),
      })
      done()
    }, 10)
  })

  // Note that "beta" remains a separate file here (nodes aren't combined).
  test("write new second node", (t, done) => {
    radisk("be" + enq + "string", ["be value", now])

    setTimeout(() => {
      assert.deepEqual(puts, {
        "!": alpha(),
        be:
          '\x1F+0\x1F#\x1F"be\x05string\x1F=\x1F"be value\x03' + now + "\x1F\n",
        beta:
          '\x1F+0\x1F#\x1F"beta\x05string\x1F=\x1F"beta value\x03' +
          now +
          "\x1F\n" +
          child(),
        parent: parent(),
        grandchild: grandchild(),
        toys: toys(),
      })
      done()
    }, 10)
  })

  test("add a parent item", (t, done) => {
    radisk("parent" + enq + "after", ["this was added after", now])

    setTimeout(() => {
      assert.deepEqual(puts, {
        "!": alpha(),
        be:
          '\x1F+0\x1F#\x1F"be\x05string\x1F=\x1F"be value\x03' + now + "\x1F\n",
        beta:
          '\x1F+0\x1F#\x1F"beta\x05string\x1F=\x1F"beta value\x03' +
          now +
          "\x1F\n" +
          child(),
        grandchild: grandchild(),
        parent:
          '\x1F+0\x1F#\x1F"parent\x05\x1F\n' +
          '\x1F+1\x1F#\x1F"after\x1F=\x1F"this was added after\x03' +
          now +
          "\x1F\n" +
          '\x1F+1\x1F#\x1F"boolean\x1F=\x1F+\x03' +
          now +
          "\x1F\n" +
          '\x1F+1\x1F#\x1F"number\x1F=\x1F+42\x03' +
          now +
          "\x1F\n" +
          '\x1F+1\x1F#\x1F"rel\x1F=\x1F#child\x03' +
          now +
          "\x1F\n" +
          '\x1F+1\x1F#\x1F"string\x1F=\x1F"parent value\x03' +
          now +
          "\x1F\n",
        toys: toys(),
      })
      done()
    }, 10)
  })

  test("add two child items", (t, done) => {
    radisk("child" + enq + "book", [lorem, now])
    radisk("child" + enq + "pi", [3.14159, now])

    setTimeout(() => {
      assert.deepEqual(puts, {
        "!": alpha(),
        be:
          '\x1F+0\x1F#\x1F"be\x05string\x1F=\x1F"be value\x03' + now + "\x1F\n",
        beta:
          '\x1F+0\x1F#\x1F"beta\x05string\x1F=\x1F"beta value\x03' +
          now +
          "\x1F\n" +
          '\x1F+0\x1F#\x1F"child\x05\x1F\n' +
          '\x1F+1\x1F#\x1F"boo\x1F\n' +
          '\x1F+2\x1F#\x1F"k\x1F=\x1F"' +
          lorem +
          "\x03" +
          now +
          "\x1F\n" +
          '\x1F+2\x1F#\x1F"lean\x1F=\x1F+\x03' +
          now +
          "\x1F\n" +
          '\x1F+1\x1F#\x1F"number\x1F=\x1F+43\x03' +
          now +
          "\x1F\n" +
          '\x1F+1\x1F#\x1F"pi\x1F=\x1F+3.14159\x03' +
          now +
          "\x1F\n" +
          '\x1F+1\x1F#\x1F"rel\x1F=\x1F#grandchild\x03' +
          now +
          "\x1F\n" +
          '\x1F+1\x1F#\x1F"string\x1F=\x1F"child value\x03' +
          now +
          "\x1F\n",
        grandchild: grandchild(),
        parent:
          '\x1F+0\x1F#\x1F"parent\x05\x1F\n' +
          '\x1F+1\x1F#\x1F"after\x1F=\x1F"this was added after\x03' +
          now +
          "\x1F\n" +
          '\x1F+1\x1F#\x1F"boolean\x1F=\x1F+\x03' +
          now +
          "\x1F\n" +
          '\x1F+1\x1F#\x1F"number\x1F=\x1F+42\x03' +
          now +
          "\x1F\n" +
          '\x1F+1\x1F#\x1F"rel\x1F=\x1F#child\x03' +
          now +
          "\x1F\n" +
          '\x1F+1\x1F#\x1F"string\x1F=\x1F"parent value\x03' +
          now +
          "\x1F\n",
        toys: toys(),
      })
      done()
    }, 10)
  })
})
