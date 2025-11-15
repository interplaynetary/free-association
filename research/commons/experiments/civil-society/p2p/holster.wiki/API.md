## Getting Started

The Holster API is made available by calling for example, `const holster = Holster()` in either Node.js or the browser. With Node.js you can create a file containing the following, which will run a WebSocket server that browsers can connect to:

```
import Holster from "./src/holster.js"
const holster = Holster()
```

In the browser, creating an html file that loads the following JavaScript will start a WebSocket client to interact with the server:

```
<script type="module">
  import Holster from "./src/holster.js"
  const holster = Holster()
  globalThis.holster = holster
</script>
```

## Holster options

You can pass similar options to Holster as you can pass to GunDB. In the browser, if the parameter to Holster is a string it will be used as the peer to connect to. Holster does not rewrite the scheme to connect to peer WebSockets, so urls should start with `ws://` or `wss://` for secure connections. If options is an array, then Holster will try to connect to all the peers listed. Both of these browser options are shorthand for providing a `peers` property in the options object.

**Core Options**
- `secure` if set to true then it will prevent updates being written to disk if they're not signed by a user.
- `peers` default is `["ws://localhost:8765"]` when not provided, or shorthand mentioned above is not used.

**Storage Options**
- `file` directory name for file storage, default is `radata`.
- `indexedDB` if set to true then indexedDB will be used for storage in the browser, otherwise no browser storage is used and clients rely on the server for storage.
- `store` allows custom storage interfaces to be written.

**Radisk Options**
- `batch` number of operations to batch before writing to disk, default is 100.
- `write` wait time in milliseconds before writing batched data, default is 1.
- `size` maximum file size on disk, default is 1MB.
- `cache` enable in-memory caching of parsed files, default is true.
- `memoryLimit` maximum memory in MB to use for cached files, default is 500.
- `log` custom logging function, default is `console.log`.

**Network Options**
- `port` for WebSocket server, default is `8765`.
- `server` existing HTTP server to attach WebSocket to.
- `wss` existing WebSocket server can be passed in for testing.
- `maxConnections` maximum WebSocket connections, default is 1000.
- `maxMessageSize` maximum message size over WebSockets, default is 1MB.
- `maxQueueLength` maximum length of client message queues, default is 10000.

Example of passing an options object to Holster:

```
const holster = Holster({indexedDB: true, secure: true})
```

## Lex

Lex is a query object that can be passed to some of the API functions below. It allows a subset of properties on a node to be returned that match the query, otherwise the full node is returned when lex is not provided. It currently can't be chained with other function calls, which means it needs to be used with a callback function.

The format is the same as GunDB, but only supports the query object specified with the `"."` property. There is currently no paging which GunDB allows with the `"%"` property. If a string is given rather than a query object, then an exact match will be made when checking a key. Query object options are:
* `"*"`: For prefix match (cannot be used with other options).
* `"<"`: For lexically less than or equal to
* `">"`: For lexically greater than or equal to

Example lex queries that will all match a property named "key":
```
{".": "key"}
{".": {"*": "k"}}
{".": {">": "j", "<": "m"}}
```

## Holster API Functions

### Get

`get()` can be passed up to four parameters: a key, an optional lex query, an optional callback function and an optional options object. The callback is optional to support chained requests. `get()` must be called before any other function available in the API. The options object currently only supports the `wait` property, which is a number in milliseconds to wait for a response to your query. The default is 100 milliseconds, but a query will only wait this long if no data is found.

`get()` will return data to your callback on success, and `null` on failure. It logs errors rather than returning them. Example requests using `console.log` as the callback function:

```
holster.get("key", console.log)

// With lex.
holster.get("item", {".": "property"}, console.log, {wait: 1000})
```

### Put

`put()` can be passed up to three parameters: a value to store, an optional `set` boolean flag and an optional callback function. The value can be a string, number, object, `true`, `false` or `null`. If you provide a `set` value and set it to true, the provided data will be added to the current node under a random key. This allows you to create a "set" in a similar way to calling `set()` from GunDB. Since Holster returns the full node, you can iterate over the entries in an object to retrieve your values. If you provide a callback, it will return an error if one occurs or `null` on success. It must be called after `get()` and optionally `next()`. Example requests using `console.log` as the callback function:

```
// Put a string.
holster.get("key").put("value", console.log)

// Put an object.
holster.get("item").put({"property": true}, console.log)

// Add an item to a set.
holster.get("set").put("always a new entry", true, console.log)
```

### Next

`next()` is used to chain queries to return nested data. It has the same signature as `get()`, taking a key, an optional lex query, an optional callback function and an optional options object. Providing a callback ends the query chain. It must be called after `get()` and can be called multiple times. Example request using `console.log` as the callback function:

```
holster.get("item").next("property", console.log)
```

### On

`on()` can be passed up to four parameters: an optional lex query, a callback function, an optional get boolean to return data immediately as well as listen, and an optional options object the same as `get()`. The callback will be called whenever the data associated with the request changes. To associate the callback with a particular node, use a combination of `get()` and `next()` before calling `on()`. Example requests using `console.log` as the callback function:

```
// Wait for "key" to change.
holster.get("key").on(console.log)

// Also returns data to the callback immediately.
holster.get("item").next("property").on(console.log, true)

// With lex.
holster.get("item").on({".": {"*": "prop"}}, console.log)
```

### Off

`off()` is passed an optional callback function, which when provided removes the specific callback from the list of event listeners created using `on()`. If no callback function is provided then all event listeners will be removed for the request. It can use the same combination of `get()` and `next()` functions that were used with `on()`. It does not call the provided callback and does not return a value. Example requests using `console.log` as the callback function:

```
// Create a reference to the callback function.
const cb = data => console.log(data)
holster.get("key").on(cb)

// Later...
holster.get("key").off(cb)

// Remove all event listeners for a property.
holster.get("item").next("property").off()
```