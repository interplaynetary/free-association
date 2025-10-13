You can import SEA as a standalone module, otherwise the same functions are available under `holster.SEA`. All SEA functions can either be passed a callback to receive the results, or `await` can be used to return the results directly.

In the descriptions below _return_ means either returned to your callback function if one was provided, or returned directly using `await` without a callback.

## SEA API Functions

### Pair

`pair()` is used to generate a pair of public and private keys. The first set of keys are generated using `ECDSA` and are used for signing and verifying data. The second set of keys are generated using `ECDH` and are used for encryption. `pair()` takes no parameters and returns an object containing four properties: `pub`, `priv`, `epub` and `epriv`.

### Encrypt

`encrypt()` is passed data in any format to encrypt, a pair object as returned from `pair()` and an optional callback function. It returns an object containing three properties: `ct`, `iv` and `s`, which is suitable for passing to `decrypt()`.

### Decrypt

`decrypt()` is passed an object containing data to decrypt, as returned from `encrypt()`, a pair object as returned from `pair()`, and an optional callback function. The returned data will match the original data passed to `encrypt()` or `null` will be returned on error.

Example using `pair()`, `encrypt()` and `decrypt()` with `await`:

```
const pair = await holster.SEA.pair()
const enc = await holster.SEA.encrypt("hello self", pair)
const dec = await holster.SEA.decrypt(enc, pair)
console.log(dec) // "hello "self"
```

Example using `pair()`, `encrypt()` and `decrypt()` with callbacks:

```
holster.SEA.pair(pair => {
  holster.SEA.encrypt("hello self", pair, enc => {
    holster.SEA.decrypt(enc, pair, dec => {
      console.log(dec) // "hello self"
    })
  })
})
```

### Verify

`verify()` is passed an object to verify, a pair object as returned from `pair()` and an optional callback function. The object to verify has two properties, `m` and `s`, which are the message to verify and it's signature. `verify()` returns the message on success or `null` on error.

### Sign

`sign()` is passed data to sign in any format, a pair object as returned from `pair()` and an optional callback function. It returns an object with `m` and `s` properties suitable for passing to `verify()`.

Example using `sign()` and `verify()` with `await`:

```
const pair = await holster.SEA.pair()
const signed = await holster.SEA.sign("hello self", pair)
const verified = await holster.SEA.verify(signed, pair)
console.log(verified) // "hello self"
```

Example using `sign()` and `verify()` with callbacks:

```
holster.SEA.pair(pair => {
  holster.SEA.sign("hello self", pair, signed => {
    holster.SEA.verify(signed, pair, verified => {
      console.log(verified) // "hello self"
    })
  })
})
```

### Work

`work()` is passed data in any format to perform proof of work on, an optional salt if you want to be able to re-derive the work, and an optional callback function. `work()` returns a private key in pair object format, so that it can be used with `decrypt()`.

Example using `work()` with `await`:

```
const work = await holster.SEA.work("hello")
const enc = await holster.SEA.encrypt("hello work", work)
const dec = await holster.SEA.decrypt(enc, work)
console.log(dec) // "hello work"
```

Example using `work()` with callbacks:

```
holster.SEA.work("hello", work => {
  holster.SEA.encrypt("hello work", work, enc => {
    holster.SEA.decrypt(enc, work, dec => {
      console.log(dec) // "hello work"
    })
  })
})
```

### Secret

`secret()` is passed two pair objects and an optional callback. It returns an object in pair format that is suitable for passing to `encrypt()` and `decrypt()`. The first pair object only needs to contain a user's public encryption key, `epub`. The second pair object needs both public and private encryption keys, so should be an authenticated user.

Example using `secret()` with `await`:

```
const alice = await holster.SEA.pair()
const bob = await holster.SEA.pair()
// Alice creates a secret for Bob and uses it to encrypt a message.
const to = await holster.SEA.secret(bob, alice)
const enc = await holster.SEA.encrypt("shared data", to)

// Bob can create the same secret and use it to read the message.
const from = await holster.SEA.secret(alice, bob)
const dec = await holster.SEA.decrypt(enc, from)
console.log(dec) // "shared data"
```

Example using `secret()` with callbacks:

```
holster.SEA.pair(alice => {
  holster.SEA.pair(bob => {
    holster.SEA.secret(bob, alice, to => {
      holster.SEA.encrypt("shared data", to, enc => {
        holster.SEA.secret(alice, bob, from => {
          holster.SEA.decrypt(enc, from, dec => {
            console.log(dec) // "shared data"
          })
        })
      })
    })
  })
})
```
