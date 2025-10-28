import fs from "fs"
import {Server} from "mock-socket"
import {describe, test} from "node:test"
import assert from "node:assert/strict"
import User from "../src/user.js"

describe("user", () => {
  const wss = new Server("ws://localhost:1234")
  const user = User({file: "test/user", wss: wss, maxAge: 100})

  test("create", (_t, done) => {
    user.create("alice", "password", err => {
      assert.equal(err, null)
      done()
    })
  })

  test("create no username", (_t, done) => {
    user.create("", "password", err => {
      assert.equal(err, "Please provide a username")
      done()
    })
  })

  test("create no password", (_t, done) => {
    user.create("alice", "", err => {
      assert.equal(err, "Please provide a password")
      done()
    })
  })

  test("username already exists", (_t, done) => {
    user.create("alice", "password", err => {
      assert.equal(err, "Username already exists")
      done()
    })
  })

  test("user is already being created", (_t, done) => {
    user.create("bob", "password", err => {
      assert.equal(err, null)
      done()
    })
    user.create("bob", "password", err => {
      assert.equal(err, "User is already being created")
    })
  })

  test("auth", (_t, done) => {
    user.auth("alice", "password", err => {
      assert.equal(err, null)
      assert.equal(user.is!.username, "alice")
      done()
    })
  })

  test("auth no username", (_t, done) => {
    user.auth("", "password", err => {
      assert.equal(err, "Please provide a username")
      assert.equal(user.is, null)
      done()
    })
  })

  test("auth no password", (_t, done) => {
    user.auth("alice", "", err => {
      assert.equal(err, "Please provide a password")
      assert.equal(user.is, null)
      done()
    })
  })

  test("auth wrong username", (_t, done) => {
    user.auth("wrong", "password", err => {
      assert.equal(err, "Wrong username or password")
      assert.equal(user.is, null)
      done()
    })
  })

  test("auth wrong password", (_t, done) => {
    user.auth("alice", "wrong", err => {
      assert.equal(err, "Wrong username or password")
      assert.equal(user.is, null)
      done()
    })
  })

  test("user is already authenticating", (_t, done) => {
    user.auth("bob", "password", err => {
      assert.equal(err, null)
      assert.equal(user.is!.username, "bob")
      done()
    })
    user.auth("bob", "password", err => {
      assert.equal(err, "User is already authenticating")
      assert.equal(user.is, null)
    })
  })

  test("change", (_t, done) => {
    user.change("alice", "password", "new password", err => {
      assert.equal(err, null)
      assert.equal(user.is!.username, "alice")
      done()
    })
  })

  test("change no username", (_t, done) => {
    user.change("", "password", "new password", err => {
      assert.equal(err, "Please provide a username")
      assert.equal(user.is, null)
      done()
    })
  })

  test("change no password", (_t, done) => {
    user.change("alice", "", "new password", err => {
      assert.equal(err, "Please provide a password")
      assert.equal(user.is, null)
      done()
    })
  })

  test("change no new password", (_t, done) => {
    user.change("alice", "password", "", err => {
      assert.equal(err, "Please provide a new password")
      assert.equal(user.is, null)
      done()
    })
  })

  test("check password change and leave", (_t, done) => {
    user.auth("alice", "new password", err => {
      assert.equal(err, null)
      assert.equal(user.is!.username, "alice")
      user.leave()
      assert.equal(user.is, null)
      done()
    })
  })

  test("delete wrong password", (_t, done) => {
    user.delete("alice", "password", err => {
      assert.equal(err, "Wrong username or password")
      assert.equal(user.is, null)
      done()
    })
  })

  test("delete", (_t, done) => {
    user.delete("alice", "new password", err => {
      assert.equal(err, null)
      assert.equal(user.is, null)
      done()
    })
  })

  test("auth after delete", (_t, done) => {
    user.auth("alice", "new password", err => {
      assert.equal(err, "Wrong username or password")
      assert.equal(user.is, null)
      done()
    })
  })

  test("cleanup", (_t, done) => {
    fs.rm("test/user", {recursive: true, force: true}, err => {
      assert.equal(err, null)
      done()
    })
  })
})

