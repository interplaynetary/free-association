API
Think these docs could be improved? Contribute to the wiki! Or comment.

Core API

Gun constructor
gun.put
gun.get
gun.opt
gun.chain
gun.back
Main API

gun.on
gun.once (gun.val)
gun.set
gun.map
User API (authenticated)

gun.user.create
gun.user.auth TBD
gun.user.leave TBD
gun.user.delete TBD
gun.user.recall TBD
gun.user.alive TBD
gun.user.trust TBD
gun.user.grant TBD
gun.user.secret TBD
Extended API

gun.path
gun.not
gun.open
gun.load
gun.then
gun.bye
gun.later
gun.unset
lib/hub.js
Utils

Gun.node
-- Core API -- 
Gun(options) 

Used to create a new gun database instance.

var gun = Gun(options)
note: Gun works with or without the new operator.

Options 
no parameters undefined creates a local datastore using the default persistence layer, either localStorage or Radisk.

passing a URL string creates the above local datastore that also tries to sync with the URL.

or you can pass in an array of URLs to sync with multiple peers within the options object. An array alone is also valid. e.g.
var gun = Gun({peers: ['http://localhost:8765/gun', 'http://gun-manhattan.herokuapp.com/gun']})
var gun = Gun(['http://localhost:8765/gun', 'http://gun-manhattan.herokuapp.com/gun'])
the previous options are actually aggregated into an object, which you can pass in yourself.

options.peers is an object, where the URLs are properties, and the value is an empty object.

options.radisk (boolean, default: true) creates and persists local (nodejs) data using Radisk.

options.localStorage (boolean, default: true) persists local (browser) data to localStorage.

options.uuid allows you to override the default 24 random alphanumeric soul generator with your own function.

option.web allows to define a web socket server to which web pages can connect. This is can be used for Gun instances running as a Node server to provide access for web page clients under http://domainname/gun.

options['module name'] allows you to pass options to a 3rd party module. Their project README will likely list the exposed options. Here is a list of such modules...

Examples 
Sync with one peer

Gun('http://yourdomain.com/gun')
Sync with many peers

Gun({peers: ['http://server1.com/gun', 'http://server2.com/gun']})
Working with modules

Gun({
  // Amazon S3 (comes bundled)
  s3: {
    key: '',
    secret: '',
    bucket: ''
  },

  // simple JSON persistence (bundled)
  // meant for ease of getting started
  // NOT meant for production
  file: 'file/path.json',

  // set your own UUID function
  uuid: function () {...}
})
Start web socket server

    const server = createServer()
    const host = 'localhost'
    const port = 9999
    const gun = Gun({web: server});

    server.listen(port, () => {
        console.info(
            'Gun server started on port ' + port + ' with /gun',
        )
    })
Am I Connected? (Peer counting) 
There's currently no single method provisioned to quickly check whether you're connected, or to how many peers. However, you can retrieve gun's backend list of peers, and then filter the list on specific parameters (which might change as Gun is currently reaching an alchemical state of transmutation).

const opt_peers = gun.back('opt.peers'); // get peers, as configured in the setup
let connectedPeers = _.filter(Object.values(opt_peers), (peer) => {
    return  peer
        && peer.wire
        && peer.wire.readyState === 1
        && peer.wire.OPEN === 1
        && peer.wire.constructor.name === 'WebSocket';
});
The length of connectedPeers corresponds to the connected peers, which you can now use in your UI.

Reconnecting 
Here again, due to convoluted reasons which need high priority addressing, after going offline and then hopping back on, GUN doesn't reliably re-connect to its peers (unless you'd refresh the page/app). In some cases, the peers even get removed from the opt.peers list which we've accessed above to count connected peers.

It's being debated how to approach this most reasonably. In the meantime you can trigger a reconnect by re-adding the peers to GUN's opt list using the following code:

gun.opt({peers: ['http://server1.com/gun', 'http://server2.com/gun']}); // re-add peers to GUN-options

gun.get('key').get('heartbeat').put('heartbeat') // optional: tell GUN to put something small, forcing GUN to establish connection, as GUN is lazy by nature to make it save on data transfer. necessary for receiving missing on() events.
In the case of counting your peers using the previous example, after reconnecting you'll see the peer count go back up.

gun.put(data, callback) 


Save data into gun, syncing it with your connected peers.

It has two parameters, and only the first is required:

the data to save
an optional callback, invoked on each acknowledgment
gun.get('key').put({hello: "world"}, function(ack){})

You do not need to re-save the entire object every time, gun will automatically merge your data into what already exists as a "partial" update.

Allowed types 
.put restricts the input to a specific subset:

objects: partials, circular, and nested
strings
numbers
booleans
null
Other values, like undefined, NaN, Infinity, arrays, will be rejected.

Traditional arrays are dangerous in real-time apps. Use gun.set instead.

Note: when using .put, if any part of the chain does not exist yet, it will implicitly create it as an empty object.

gun.get('something').get('not').get('exist').get('yet').put("Hello World!");
// `.put` will if needed, backwards create a document
// so "Hello World!" has a place to be saved.
Callback(ack) 
ack.err, if there was an error during save.
ack.ok, if there was a success message (none is required though).
The callback is fired for each peer that responds with an error or successful persistence message, including the local cache. Acknowledgement can be slow, but the write propagates across networks as fast as the pipes connecting them.

If the error property is undefined, then the operation succeeded, although the exact values are left up to the module developer.

Examples 
Saving objects

gun.get('key').put({
  property: 'value',
  object: {
    nested: true
  }
})
Saving primitives

// strings
gun.get('person').get('name').put('Alice')

// numbers
gun.get('IoT').get('temperature').put(58.6)

// booleans
gun.get('player').get('alive').put(true)
Using the callback

gun.get('survey').get('submission').put(submission, function(ack){
  if(ack.err){
    return ui.show.error(ack.err)
  }
  ui.show.success(true)
})
Chain context 
gun.put does not change the gun context.

gun.get('key').put(data) /* same context as */ gun.get('key')
Unexpected behavior 
You cannot save primitive values at the root level.

Gun().put("oops"); // error
Gun().get("odd").put("oops"); // error
All data is normalized to a parent node.

Gun().put({foo: 'bar'}); // internally becomes...
Gun().get(randomUUID).put({foo: 'bar'});

Gun().get('user').get('alice').put(data); // internally becomes...
Gun().get('user').put({'alice': data});
// An update to both user and alice happens, not just alice.
You can save a gun chain reference,

var ref = Gun().put({text: 'Hello world!'})
Gun().get('message').get('first').put(ref)
But you cannot save it inline, yet.

var sender = Gun().put({name: 'Tom'})
var msg = Gun().put({
  text: 'Hello world!',
  sender: sender // this will fail
})
// however
msg.get('sender').put(sender) // this will work
Be careful saving deeply nested objects,

Gun().put({
  foo: {
    bar: {
      lol: {
        yay: true
      }
    }
  }
}):
For the most part, gun will handle this perfectly fine. It will attempt to automatically merge every nested object as a partial. However, if it cannot find data (due to a network failure, or a peer it has never spoken with) to merge with it will generate new random UUIDs. You are unlikely to see this in practice, because your apps will probably save data based on user interaction (with previously loaded data). But if you do have this problem, consider giving each one of your sub-objects a deterministic ID.

gun.get(key) 
Where to read data from.



It takes two parameters:

key
callback
gun.get('key').get('property', function(ack){})

You will usually be using gun.on or gun.once to actually retrieve your data, not this callback (it is intended for more low level control, for module and extensions).

Key 
The key is the ID or property name of the data that you saved from earlier (or that will be saved later). It can also be a Lex expression allowing to filter, limit and paginate results.

Note that if you use .put at any depth after a get it first reads the data and then writes, merging the data as a partial update.

gun.get('key').put({property: 'value'})

gun.get('key').on(function(data, key){
  // {property: 'value'}, 'key'
})
Callback(ack) 
ack.put, the raw data.
ack.get, the key, ID, or property name of the data.
The callback is a listener for read errors, not found, and updates. It may be called multiple times for a single request, since gun uses a reactive streaming architecture. Generally, you'll find .not, .on, and .once as more convenient for every day use. Skip to those!

gun.get(key, function(ack){
  // called many times
})
Examples 
Retrieving a key

// retrieve all available documents
gun.get('documents').map().on(ui.show.documents)
Using the callback

gun.get(key, function(ack){
  if(ack.err){
    server.log(error)
  } else
  if(!ack.put){
    // not found
  } else {
    // data!
  }
})
Chain context 
Chaining multiple gets together changes the context of the chain, allowing you to access, traverse, and navigate a graph, node, table, or document.

Note: For users upgrading versions, prior to v0.5.x get used to always return a context from the absolute root of the database. If you want to go back to the root, either save a reference var root = Gun(); or now use .back(-1).

gun.get('user').get('alice') /* same context as */ gun.get('users').get('alice')
Unexpected behavior 
Most callbacks in gun will be called multiple times.

gun.opt(options) 
Change the configuration of the gun database instance.

The options argument is the same object you pass to the constructor. The options's properties replace those in the instance's configuration but options.peers are added to peers known to the gun instance.

Examples 
Create the gun instance.

gun = Gun('http://yourdomain.com/gun')
Change UUID generator:

gun.opt({
  uuid: function () {
    return Math.floor(Math.random() * 4294967296);
  }
});
Add more peers:

gun.opt({peers: ['http://anotherdomain.com/gun']})
/* Now gun syncs with ['http://yourdomain.com/gun', 'http://anotherdomain.com/gun']. */
gun.back(amount) 
Move up to the parent context on the chain.

Every time a new chain is created, a reference to the old context is kept to go back to.

Amount 
The number of times you want to go back up the chain. -1 or Infinity will take you to the root.

Examples 
Moving to a parent context

gun.get('users')
  /* now change the context to alice */
  .get('alice')
  .put(data)
  /* go back up the chain once, to 'users' */
  .back().map(...)
Another example

gun.get('player').get('game').get('score').back(1)
// is the same as...
gun.get('player').get('game')
Chain context 
The context will always be different, returning you to the

gun.get('key').get('property')
/* is not the same as */
gun.get('key').get('property').back()
-- Main API -- 
gun.on(callback, option) 


Subscribe to updates and changes on a node or property in realtime.

Callback(data, key) 
Once initially and whenever the property or node you're focused on changes, this callback is immediately fired with the data as it is at that point in time.

Since gun streams data, the callback will probably be called multiple times as new chunk comes in.

To remove a listener call .off() on the same property or node. This removes all listeners. To remove a specific listener, the best current solution is as follows (more details here):

var ev = null
var listenerHandler = (value, key, _msg, _ev) => {
  ev = _ev
  //...
}
node.on(listenerHandler)
node.get('someValue').put(6) //trigger listener to set ev
ev.off() //remove listener
Note: If data is a node (object), it may have _ meta property on it. If you delete an item, you might get a null tombstone.

Option 
Currently, the only option is to filter out old data, and just be given the changes. If you're listening to a node with 100 fields, and just one changes, you'll instead be passed a node with a single property representing that change rather than the full node every time.

Longhand syntax

// add listener to foo
gun.get('foo').on(callback, {
  change: true
})

// remove listener to foo
gun.get('foo').off()
Shorthand syntax

// add listener to foo
gun.get('foo').on(callback, true)

// remove listener to foo
gun.get('foo').off()
Examples 
Listening for updates on a key

gun.get('users').get(username).on(function(user){
  // update in real-time
  if (user.online) {
    view.show.active(user.name)
  } else {
    view.show.offline(user.name)
  }
})
Listening to updates on a field

gun.get('lights').get('living room').on(function(state, room){
  // update the UI when the living room lights change state
  view.lights[room].show(state)
})
IMPORTANT 
:exclamation: There's a 'bug' when dealing with multiple levels.

gun.get('home').get('lights').on(cb,true);
gun.get('home').get('lights').put({state:'on'})       // BAD fires twice
gun.get('home').get('lights').get('state').put('on')  // GOOD fires once
Chain Context 
gun.on does not change the chain context.

gun.get(key).on(handler) /* is the same as */ gun.get(key)
Unexpected behavior 
Data is only 1 layer deep, a full document is not returned (see the gun.open extension for that), this helps keep things fast.

It will be called many times.

gun.once(callback, option) 


Get the current data without subscribing to updates. Or undefined if it cannot be found.

Option 
wait controls the asynchronous timing (see unexpected behavior, below). gun.get('foo').once(cb, {wait: 0})
Callback(data, key) 
The data is the value for that chain at that given point in time. And the key is the last property name or ID of the node.

Note: If data is a node (object), it may have _ meta property on it. If you delete an item, you might get a null tombstone. If the data cannot be found, undefined may be called back.

Examples 
gun.get('peer').get(userID).get('profile').once(function(profile){
  // render it, but only once. No updates.
  view.show.user(profile)
})
Reading a property

gun.get('IoT').get('temperature').once(function(number){
  view.show.temp(number)
})
Chain Context 
gun.once does not currently change the context of the chain, but it is being discussed for future versions that it will - so try to avoid chaining off of .once for now. This feature is now in experimental mode with v0.6.x, but only if .once() is not passed a callback. A useful example would be gun.get('users').once().map().on(cb) this will tell gun to get the current users in the list and subscribe to each of them, but not any new ones. Please test this behavior and recommend suggestions.

Unexpected behavior 
.once is synchronous and immediate (at extremely high performance) if the data has already been loaded.

.once is asynchronous and on a debounce timeout while data is still being loaded - so it may be called completely out of order compared to other functions. This is intended because gun streams partials of data, so once avoids firing immediately because it may not represent the "complete" data set yet. You can control this timeout with the wait option.

once fires again if you update that node from within it.

Example for once firing again with an update from within:

node.once(function(data, key) {
  node.get('something').put('something')
}
Data is only 1 layer deep, a full document is not returned (see the gun.load extension for that), this helps keep things fast.

gun.set(data, callback) 
Add a unique item to an unordered list.

gun.set works like a mathematical set, where each item in the list is unique. If the item is added twice, it will be merged. This means only objects, for now, are supported.

To remove items from a set, see gun.unset.

Data 
Data should be a gun reference or an object.

var user = gun.get('alice').put({name: "Alice"});
gun.get('users').set(user);
Callback 
The callback is invoked exactly the same as .put, since .set is just a convenience wrapper around .put.

Examples 
var gun = Gun();
var bob = gun.get('bob').put({name: "Bob"});
var dave = gun.get('dave').put({name: "Dave"});

dave.get('friends').set(bob);
bob.get('friends').set(dave);
The "friends" example is perfect, since the set guarantees that you won't have duplicates in your list.

Chain Context 
gun.set changes the chain context, it returns the item reference.

gun.get('friends') /* is not the same as */ gun.get('friends').set(friend)
gun.map(callback) 


Map iterates over each property and item on a node, passing it down the chain, behaving like a forEach on your data. It also subscribes to every item as well and listens for newly inserted items. It accepts one argument:

a callback function that transforms the data as it passes through. If the data is transformed to undefined it gets filtered out of the chain.
the callback gets two arguments (value,key) and will be called once for each key value pair in the objects that are returned from map.
Note: As of v0.6.x the transform function is in experimental mode. Please play with it and report bugs or suggestions on how it could be improved to be more useful.

Disclaimer 
If your data is cyclic and has a lot of self-references, you may receive multiple callbacks with the same result. For example: "Alice is both the president of the company, the wife of Bob, and the friend to the cat." This would return 3 times:

 key: president value: alice,
 key: wife value: alice,
 key: friend value: alice
.map does not exclude or unify nodes, because they exist in different contexts, it is up to you on either UI side or in structuring your data to take this into account.

Examples 
Iterate over an object

/*
  where `stats` are {
    'new customers': 35,
    'returning': 65
  }
*/
gun.get('stats').map().on(function(percent, category) {
  pie.chart(category, percent)
})
The first call to the above will be with (35,'new customers') and the second will be with (65,'returning').

Or forEaching through every user.

/*
{
  user123: "Mark",
  user456: "Dex",
  user789: "Bob"
}
*/
gun.get('users').map().once(function(user, id){
  ui.list.user(user);
});
The above will be called 3 times.

Here's a summary of .map() behavior depending on where it is on the chain:

users.map().on(cb) subscribes to changes on every user and to users as they are added.
users.map().once(cb) gets each user once, including ones that are added over time.
users.once().map().on(cb) gets the user list once, but subscribes to changes on each of those users (not added ones).
users.once().map().once(cb) gets the user list once, gets each of those users only once (not added ones).
Iterate over and only return matching property

/*
{
  user123: "Mark",
  user456: "Dex",
  user789: "Bob"
}
*/
gun.get('users').map(user => user.name === 'Mark'? user : undefined).once(function(user, id){
  ui.list.user(user);
});
Will only return user123: "Mark", as it was the only match.

Chain context 
.map changes the context of the chain to hold many chains simultaneously. Check out this example:

gun.get('users').map().get('name').on(cb);
Everything after the map() will be done for every item in the list, such that you'll get called with each name for every user in the list. This can be combined in really expressive and powerful ways.

gun.get('users').map().get('friends').map().get('pet').on(cb);
This will give you each pet of every friend of every user!

gun.get(key).map() /* is not the same as */ gun.get(key)
-- Extended API -- 
gun.path(key) 


Warning: This extension was removed from core, you probably shouldn't be using it!

Warning: Not included by default! You must include it yourself via require('gun/lib/path.js') or <script src="https://cdn.jsdelivr.net/npm/gun/lib/path.js"></script>!

Path does the same thing as get but has some conveniences built in.

Key 
The key property is the name of the field to move to.

// move to the "themes" field on the settings object
gun.get('settings').path('themes')
Once you've changed the context, you can read, write, and path again from that field. While you can just chain one path after another, it becomes verbose, so there are two shorthand styles:

dot format
array format
Here's dot notation in action:

// verbose
gun.get('settings').path('themes').path('active')

// shorthand
gun.get('settings').path('themes.active')

// which happens to be the the same as
gun.get('settings').get('themes').get('active')
And the array format, which really becomes useful when using variables instead of literal strings:

gun.get('settings').path(['themes', themeName])
Unexpected behavior 
The dot notation can do some strange things if you're not expecting it. Under the hood, everything is changed into a string, including floating point numbers. If you use a decimal in your path, it will split into two paths...

gun.path(30.5)
// interprets to
gun.path(30).path(5)
This can be especially confusing as the chain might never resolve to a value.

Note: For users upgrading from versions prior to v0.5.x, path used to be necessary - now it is purely a convenience wrapper around get.

Examples 
Navigating to a property

/*
  where `user` is {
    name: 'Bob'
  }
*/
gun.get('user').path('name')
Once you've focused on the name property, you can chain other methods like .put or .on to interact with it.

Moving through multiple properties

/*
  where `user` is {
    name: { first: 'bob' }
  }
*/
gun.get('user').path('name').path('first')
// or the shorthand...
gun.get('user').path('name.first')
Chain context 
gun.path creates a new context each time it's called, and is always a result of the previous context.

gun.get('API').path('path').path('chain')
/* is different from */
gun.get('API').path('path')
/* and is different from */
gun.get('API')
gun.not(callback) 
Warning: Not included by default! You must include it yourself via require('gun/lib/not.js') or <script src="https://cdn.jsdelivr.net/npm/gun/lib/not.js"></script>! Handle cases where data can't be found.

If you need to know whether a property or key exists, you can check with .not. It will consult the connected peers and invoke the callback if there's reasonable certainty that none of them have the data available.

Warning: .not has no guarantees, since data could theoretically exist on an unrelated peer that we have no knowledge of. If you only have one server, and data is synced through it, then you have a pretty reasonable assurance that a not found means that the data doesn't exist yet. Just be mindful of how you use it.

Callback(key) 
If there's reason to believe the data doesn't exist, the callback will be invoked. This can be used as a check to prevent implicitly writing data (as described in .put.

Key 
The name of the property or key that could not be found.

Examples 
Providing defaults if they aren't found

// if not found
gun.get('players/3').not(function(key){
  // put in an object and key it
  gun.get(key).put({
    active: false
  });
}).on(handler)
// listen for changes on that key
Setting a property if it isn't found

gun.get('chat').get('enabled').not(function(key){
  this.put(false)
})
Chain context 
.not does not change the context of the chain.

gun.get(key).not(handler) /* is the same as */ gun.get(key)
gun.open(callback) 
Warning: Not included by default! You must include it yourself via require('gun/lib/open.js') or <script src="https://cdn.jsdelivr.net/npm/gun/lib/open.js"></script>!

Open behaves very similarly to gun.on, except it gives you the full depth of a document on every update. It also works with graphs, tables, or other data structures. Think of it as opening up a live connection to a document.

Note: This will automatically load everything it can find on the context. This may sound convenient, but may be unnecessary and excessive - resulting in more bandwidth and slower load times for larger data. It could also result in your entire database being loaded, if your app is highly interconnected.

Callback(data) 
The callback has 1 parameter, and will get called every time an update happens anywhere in the full depth of the data.

data. Unlike most of the API, open does not give you a node. It gives you a copy of your data with all metadata removed. Updates to the callback will return the same data, with changes modified onto it.
Examples 
// include .open
gun.get('person/mark').open(function(mark){
  mark; // {name: "Mark Nadal", pet: {name: "Frizzles", species: "kitty", slave: {...}}}
});

var human = {
  name: "Mark Nadal",
  pet: {
    name: "Frizzles",
    species: "kitty" // for science!
  }
};
human.pet.slave = human;

gun.get('person/mark').put(human);
Chain context 
.open does not change the context.

gun.get('company/acme').open(cb).get('employees').map().once(cb)
Unexpected behavior 
If you do not use a schema with .open(cb) it can only best guess and approximate whether the data is fully loaded or not. As a result, do not assume all the data will be available on the first callback - it may take several calls for things to fully load, so code defensively! By default, it waits 1ms after each piece of data it receives before triggering the callback. You can change the default by passing an option like .open(cb, {wait: 99}) which forces it to wait 99ms before triggering (which is the default gun.once has).

gun.load(cb, opt) 
Warning: Not included by default! You must include it yourself via require('gun/lib/load.js') or <script src="https://cdn.jsdelivr.net/npm/gun/lib/open.js"></script><script src="https://cdn.jsdelivr.net/npm/gun/lib/load.js"></script>!

Loads the full object once. It is the same as open but with the behavior of once.

gun.then(cb) (not official yet) 
Could be buggy until official!

Warning: Not included by default! You must include it yourself via require('gun/lib/then.js') or <script src="https://cdn.jsdelivr.net/npm/gun/lib/then.js"></script>!

Returns a promise for you to use.

Note: a gun chain is not promises! You must include and call .then() to promisify a gun chain!

cb(resolved) 
cb is a function that has 1 parameter.

resolved is the data.

.promise(cb) (not official yet) 
Could be buggy until official!

.then(cb) has a cousin of .promise(cb) which behaves the same way except that resolved is an object with:

resolved = {
  put: data,
  get: key,
  gun: ref // if applicable
}
In case you need more context or metadata.

Chain context 
It is no longer a gun chain, but you can chain promises off of it!

Examples 
Promise.race([
    gun.get('alice').then(), // must be called!
    gun.get('bob').then() // must be called!
])
Or use it with async!

async function get(name) {
     var node = await gun.get(name).then();
     return node;
};
Unexpected behavior 
A gun chain is not already a promise, you must call then() to make it a promise.

gun.bye() 
Warning: Consider this deprecated, or at least no longer supported! You must include it yourself via require('gun/lib/bye.js') or <script src="https://cdn.jsdelivr.net/npm/gun/lib/bye.js"></script>!

bye lets you change data after that browser peer disconnects. This is useful for games and status messages, that if a player leaves you can remove them from the game or set a user's status to "away".

Note: This requires a server side component, and therefore must be included there as well in order for this to work. In the future it should be generalizable to P2P settings, however may not be as reliable.

Chain context 
It returns a special chain context with only 1 method on it of put. It currently does not support chaining, however in the future we hope to make it more chainable. Keep this in mind until then.

Examples 
gun.get('marknadal').get('status').put("I'm online!");

gun.get('marknadal').get('status').bye().put("I'm offline. :(");
Even though this is written entirely in the browser, the put will get called on the server when this browser tab disconnects.

Note: A user might have multiple tabs open on your website, just because they close 1 tab does not mean they are now offline. We recommend you use .bye() for data related to browser tab sessions, and then aggregate those sessions together to determine if the the user as a whole is online or offline.

var player = gun.get('kittycommando1337');

gun.get('game').get('players').set(player);

gun.get('game').get('players').get('kittycommando1337').bye().put(null);
This deletes the player from the game when they go offline or disconnect from the server. It does not delete the player, just whether they are a player of the game or not.

Unexpected behavior 
bye() is in experimental alpha, please report any problems or bugs you have with it. Note again that it does not fire immediately, and it does not get run from the browser. It makes the data change on the server when that browser tab disconnects.

gun.later(cb, seconds) 
Warning: Not included by default! You must include it yourself via require('gun/lib/later.js') or <script src="/gun/lib/open.js"></script><script src="https://cdn.jsdelivr.net/npm/gun/lib/later.js"></script>!

Say you save some data, but want to do something with it later, like expire it or refresh it. Well, then later is for you! You could use this to easily implement a TTL or similar behavior.

cb(data, key) 
data, is a safe snapshot of what you saved, including full depth documents or circular graphs, without any of the metadata.
key is the name of the data.
this is the gun reference that it was called with.
seconds 
Is the number of seconds you want to wait before firing the callback.

Chain context 
It returns itself, as in gun.later() === gun.

Examples 
See a full working example here, at jsbin!

gun.get('foo').put(data).later(function(data, key){
  this.get('session').put(null); // expire data!
}, 2); // 2 seconds!
Unexpected behavior 
Exact timing is not guaranteed! Because it uses setTimeout underneath. Further, after the timeout, it must then open and load the snapshot, this will likely add at least 1ms to the delay. Experimental: If this is problematic, please report it, as we can modify the implementation of later to be more precise.)

If a process/browser has to restart, the timeout will not be called. Experimental: If this behavior is needed, please report it, as it could be added to the implementation.

gun.unset(node) 
Warning: Not included by default! You must include it yourself via require('gun/lib/unset.js') or <script src="https://cdn.jsdelivr.net/npm/gun/lib/unset.js"></script>!

After you save some data in an unordered list, you may need to remove it.

let gun = new Gun();
let machines = gun.get('machines');
let machine = gun.get('machine/tesseract');
machine.put({faces: 24, cells: 8, edges: 32});
// let's add machine to the list of machines;
machines.set(machine);
// now let's remove machine from the list of machines
machines.unset(machine);
-- Gun utils -- 
While running, Gun provides several high-level utility functions for querying and manipulating our components.

Note the capital "G" in Gun, as opposed to an instance variable called gun.

Gun.node.is(data) 
Returns true if data is a gun node, otherwise false.

Gun.node.soul(data) 
Deprecated! It was used to return data's gun ID. Use data._["#"] instead. NOTE! It works only if received data is an object, not a primitive value.

gun.get('test').get('node').put({text:'hello'})
gun.get('test').get('node').once(data=> console.log(data._['#']))  // test/node
Gun.node.ify(json) 
Returns a "gun-ified" variant of the json input by injecting a new gun ID into the metadata field.