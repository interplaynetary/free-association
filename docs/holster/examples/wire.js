// Switch src to build for the production version.
import Holster from "../src/holster.js"

// Set readFromDisk to true to force reading from disk.
// (Restart the server so that graph is not in memory.)
const readFromDisk = false

const holster = Holster({indexedDB: !readFromDisk})
globalThis.holster = holster

if (readFromDisk) {
  // Wait for the websocket to connect.
  setTimeout(() => {
    holster.wire.get({"#": "FDSA", ".": "species"}, msg => {
      console.log("read from disk:", msg)
    })
  }, 1000)
} else {
  // Create two updates that could happen in either order.
  setTimeout(() => {
    const update1 = {
      ASDF: {
        _: {"#": "ASDF", ">": {name: 2, boss: 2}},
        name: "Mark Nadal",
        boss: {"#": "FDSA"},
      },
      FDSA: {
        _: {"#": "FDSA", ">": {name: 2, species: 2, slave: 2}},
        name: "Fluffy",
        species: "a kitty",
        slave: {"#": "ASDF"},
      },
    }
    holster.wire.put(update1, msg => console.log("update1:", msg))
  }, 1000 * Math.random())

  setTimeout(() => {
    const update2 = {
      ASDF: {_: {"#": "ASDF", ">": {name: 1}}, name: "Mark"},
      FDSA: {
        _: {"#": "FDSA", ">": {species: 2, color: 3}},
        species: "felis silvestris",
        color: "ginger",
      },
    }
    holster.wire.put(update2, msg => console.log("update2:", msg))
  }, 1000 * Math.random())

  // They should always produce the same result no matter which order.
  setTimeout(() => {
    holster.wire.get({"#": "FDSA", ".": "species"}, msg => {
      console.log("get:", msg)
    })
  }, 2000)
}
