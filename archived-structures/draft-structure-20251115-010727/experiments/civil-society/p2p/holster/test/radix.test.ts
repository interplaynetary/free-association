import {describe, test} from "node:test"
import assert from "node:assert/strict"
import Radix from "../src/radix.js"
import Names from "./names.js"
const names = Names()

// ASCII character for group separator.
const group = String.fromCharCode(29)
// ASCII character for record separator.
const record = String.fromCharCode(30)

describe("radix", () => {
  const radix = Radix()

  test("unit", () => {
    radix("asdf.pub", "yum")
    radix("ablah", "cool")
    radix("abc", {yes: 1})
    radix("node/circle.bob", "awesome")

    assert.deepEqual(radix("asdf."), {pub: {[record]: "yum"}})
    assert.equal(radix("nv/foo.bar"), undefined)
    assert.equal(radix("ablah"), "cool")
    assert.deepEqual(radix("abc"), {yes: 1})
    assert.equal(radix("abcd"), undefined)
    assert.deepEqual(radix(), {
      a: {
        [group]: {
          "sdf.pub": {[record]: "yum"},
          b: {
            [group]: {
              c: {
                [record]: {
                  yes: 1,
                },
              },
              lah: {
                [record]: "cool",
              },
            },
          },
        },
      },
      "node/circle.bob": {[record]: "awesome"},
    })
  })

  test("replace", () => {
    radix("asdf.pub", "yuck")
    radix("ablah", "cool!")
    radix("abc", {yes: 2})
    radix("node/circle.bob", "awe")
    radix("abcd", true)

    assert.deepEqual(radix("asdf."), {pub: {[record]: "yuck"}})
    assert.equal(radix("ablah"), "cool!")
    assert.equal(radix("nv/foo.bar"), undefined)
    assert.deepEqual(radix("abc"), {yes: 2})
    assert.equal(radix("abcd"), true)
    assert.deepEqual(radix(), {
      a: {
        [group]: {
          "sdf.pub": {[record]: "yuck"},
          b: {
            [group]: {
              c: {
                [group]: {
                  d: {
                    [record]: true,
                  },
                },
                [record]: {
                  yes: 2,
                },
              },
              lah: {
                [record]: "cool!",
              },
            },
          },
        },
      },
      "node/circle.bob": {[record]: "awe"},
    })
  })

  test("radix write read", () => {
    // Add some shorter values to also force group matching in Radix.map.
    radix("A", 1)
    radix("Be", 2)
    radix("Bo", 3)
    radix("L", 4)
    radix("Mi", 5)
    const all: Record<string, number> = {}
    names.forEach((value, index) => {
      all[value] = index
      radix(value, index)
    })
    assert.equal(Object.keys(all).length, names.length)
    Radix.map(radix, (value: any, key: string) => {
      delete all[key]
    })
    assert.equal(Object.keys(all).length, 0)
  })

  test("radix read again", () => {
    const all: Record<string, number> = {}
    names.forEach((value, index) => {
      all[value] = index
    })
    assert.equal(Object.keys(all).length, names.length)
    Radix.map(radix, (value: any, key: string) => {
      delete all[key]
    })
    assert.equal(Object.keys(all).length, 0)
  })
})

