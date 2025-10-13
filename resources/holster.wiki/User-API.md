## Getting Started

The Holster.user API is accessed by calling for example, `const user = holster.user()` in either Node.js or the browser. With Node.js you can create a file containing the following:

```
import Holster from "./src/holster.js"
const holster = Holster()
const user = holster.user()
```

In the browser, create an html file that loads the following JavaScript:

```
<script type="module">
  import Holster from "./src/holster.js"
  const holster = Holster()
  globalThis.holster = holster

  const user = holster.user()
</script>
```

## Holster.user API Functions

### Create

`create()` is passed a username, password and an optional callback function. When a user is created they are given a public key that is associated with their username. This public key is then used to store all their data, which is signed and verified when written to disk. Note that usernames aren't guaranteed to be unique, so public keys are used for identity in Holster.

`create()` returns `null` to your callback on success, and an error message on failure. Example request using `console.log` as the callback function:

```
user.create("username", "password", console.log)
```

### Auth

`auth()` has the same signature as `create()`, it's passed a username, password and an optional callback function. When a user successfully authenticates, their credentials are made available in the `user.is` object. This object includes their `username`, `pub` and `priv`, which are used for signing and verifying, and also `epub` and `epriv` which are used for encryption. This object also meets the requirements of a "pair object" which is mentioned in the [SEA API](https://github.com/mblaney/holster/wiki/SEA-API) documentation, so an authenticated user can be passed to many of those functions via `user.is`.

`auth()` returns `null` to your callback on success, and an error message on failure. Example request using `console.log` as the callback function:

```
user.auth("username", "password", console.log)
```

### Change

`change()` is used to change a password. It's passed a username, current password, new password and an optional callback function. Internally `change()` first calls `auth()` which verifies the current credentials before updating the password to the new value.

`change()` returns `null` to your callback on success, and an error message on failure. Example request using `console.log` as the callback function:

```
user.change("username", "password", "new password", console.log)
```

### Store

`store()` is used to store credentials in the browser. It is passed an optional boolean value, which if set to true will use local storage. Otherwise it will default to session storage if not provided or the parameter is set to false. Note that session storage is removed when the browser is closed whereas local storage credentials are not removed until the user logs out.

`store()` will log an error if the user is not authenticated when called. It will store the item under the name `user.is` in either local storage or session storage.

### Recall

`recall()` is used to automatically log in a user from stored credentials. It is not passed any parameters and will look for user credentials in both local storage and session storage.

### Leave

`leave()` is used to log out a user and remove their credentials from storage. It does not take any parameters, it will try and remove credentials from both local storage and session storage.

### Delete

`delete()` is used to remove account data under a public key. It's passed a username, password and an optional callback function. Internally `delete()` first calls `auth()` which verifies the credentials before deleting them.

`delete()` returns `null` to your callback on success, and an error message on failure. Example request using `console.log` as the callback function:

```
user.delete("username", "password", console.log)
```

### Get

`user.get()` is very similar to the Holster `get()` function, except it can take an array of two parameters instead of a string for the key parameter. If a string is provided then the key is for data under the user's account, so they must be logged in to resolve the request. If an array is given then the first value must be a public key of an account and the second value is the key. The other parameters are the same and all optional: a lex object, a callback function and an options object. 

```
// Query data on my account after logging in:
user.get("key", console.log)

// Query data on another account, user does not need to be logged in:
user.get(["public-key", "key"], console.log)
```

### The user API is an extension of the Holster API

Besides replacing the Holster `get()` function, the user API is extended with all other properties of the Holster API. This means you can chain requests in exactly the same way to use `next()`, `put()`, `on()` and `off()` functions. The user API also has the same access to the wire API via `user.wire` and SEA functions via `user.SEA`.