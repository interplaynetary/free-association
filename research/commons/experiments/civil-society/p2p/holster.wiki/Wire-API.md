## Wire API Functions

### Get

`wire.get()` is passed a lex query, callback function and an options object. The options are currently only used by tests and otherwise have defaults that are used by the Holster API. Example request using `console.log` as the callback function:

```
holster.wire.get({"#": "root", ".": "key"}, console.log)
```

### Put

`wire.put()` is passed data to store and a callback function. The data must be in graph format (see below), and can contain one or more node ids and their values.  Example request using `console.log` as the callback function:

```
holster.wire.put({
  root: {
    _: {"#": "root", ">": {key: 1}},
    key: "value",
  },
}, console.log)
````

### On

`wire.on()` is passed a lex query and a callback function. The lex query can be for either a node or a specific property. Example request using `console.log` as the callback function:

```
holster.wire.on({"#": "root"}, console.log)

holster.wire.on({"#": "root", ".": "key"}, console.log)
```

### Off

`wire.off()` is passed a lex query and an optional callback function. If a callback is not provided then all event listeners are removed for the given lex query. Examples:

```
holster.wire.off({"#": "root", ".": "key"})

// Create a reference to the callback function.
const cb = data => console.log(data)
holster.wire.on({"#": "root", ".": "key"}, cb)

// Later...
holster.wire.off({"#": "root", ".": "key"}, cb)
```

## Graph Format

This is the same format as GunDB, where nodes are not nested but instead refer to each other by id. A graph is an object where the keys are node ids and the values are objects with properties which are key value pairs. The `"_"` (underscore) key is reserved for graph meta data and has a specific structure. The other properties can have any key as a name, and the values can be strings, numbers, `true`, `false`, `null` or a "rel". A rel is a specific type of object that references another node by id, creating a relationship between the two nodes. The format for a rel value is `{"#": "id"}`.

The meta data for a node is it's id and a state vector for the properties being updated, specified using `">"` as the key, and an object consisting of property names and their updated states as the value. The Holster API uses the current timestamp as the state when a value is updated using `put()`.