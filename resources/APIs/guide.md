
Decentralized Database Mastery: A Unified Guide from Basics to Advanced with gunDB


TLDR;
This introduces the reader to gun by starting with a super basic example and then making that same example increasingly more complex. We’ll cover certificates, database design principles, iteration, awaiting values and much more.

// We start here
const alice = gun.get("members").get("alice");
alice.get("name").put("alice");

// We end here
const user_name = await User.name.chain;
Group.members.iterate(member => {
  member.name.chain.put(user_name, certificate);
}
TIP: If you have read this article before or are familiar with gun, there is a section titled Putting it all together at the end that encapsulates all the concepts of the article…

Introduction
2024 just started (well.. when I began this article it did :), and that made me reflect a bit on what I did in 2023 as a developer. The most impactful thing for me, was learning about gunDB. It is a local, offline-first database that makes it so amazingly easy to create peer to peer applications.

For me, working with and setting up databases has always been this somewhat unpleasant development experience, especially if you just need something that’s quick and dirty — for a small side project.

If you find that setting up an entire backend system just to handle a few database calls is cumbersome, or perhaps you are just starting out with programming and this seems daunting!

Well, if I may, I would like to get you excited about gun in one short code snippet…

The following is a fully working database, that will store a value locally on the device. It will then synchronize that value with all other devices that are listening to it.

 <html>
    <head>
        <script src="https://cdn.jsdelivr.net/npm/gun/gun.js"></script>
        <script src="https://cdn.jsdelivr.net/npm/gun/sea.js"></script>
    </head>

    <body>
        <button onclick="say('hi')">say hi</button>
        <button onclick="say('bye')">say bye</button>
    </body>

    <script>
        const gun = Gun(["https://gun-manhattan.herokuapp.com/gun"]);

        const items = gun.get("test").get("items");

        function say(input){
            items.put(input);
        }

        items.on(res => console.log(res));

    </script>
</html>
If you copy this into a .html file, open it in a browser, open the console and press the button you’ll get a “hi”. If you refresh the page, you will get “hi” again.

If you replace gun.get("test").get("items") with gun.get("test”).get(“paste”)you will get whatever is on this page: https://gun.eco/docs/Hello-World and clicking the button will update the text on that other page!


image from gun.eco/docs/Hello-World
Are you exited yet? Well read on to get the introduction to gun I wished I had when starting out myself…

Installing and core functions
You can install gun via running npm install gun you can then use it with your JavaScript Framework of choice. If I may recommend one — Svelte.js and gun work together like bread and butter!

If you just want to play around with it then you can also install gun the way we did before:

<script src="https://cdn.jsdelivr.net/npm/gun/gun.js"></script>
<script src="https://cdn.jsdelivr.net/npm/gun/sea.js"></script>
gun/gun.js is the core gun code and gun/sea.jsis the code that relates to user and encryption / security — that’s right! We get user authentication for free with gun!

gun.get
You interact with gun via gun.get(“something”). This will give you the “something” node. Currently there is no value there, but you can still get the node. You can string these together like this: gun.get(“person”).get(“info”).get(“names”).get(“first”). Also there is currently no value there either, so not very exiting. Let’s fix that!

gun.put
You can put data onto the graph via gun.put(value). First get the node you want to hold a value, then put data there. Let’s give our person from before a name:
gun.get(“person”).get(“info”).get(“names”).get(“first”).put(“alice”).

gun.on
You can listen to updates via gun.on(callback). This will allow you to receive the data in a callback function. If you don’t want updates, but just want to get the data one time you can also use gun.once.

gun.once
Is similar to on, but it doesn’t get called again if the value changes in the future. It will still wait for there to be an initial value though.

Putting it together
This covers a lot of what you need. Let’s put it all together!
Gun is a graph database, which mean that you should think of data as nodes, that can have connections to each other. If you have an object like this:

{
  name: "alice",
  age: 23,
  father: {name: "bob", age: 57}
}
The way we would represent this in gun is like so:

const alice = gun.get("users").get("alice");
const bob = gun.get("users").get("bob");

alice.get("name").put("alice");
alice.get("age").put(23);
alice.get("father").put(bob);

bob.get("name").put("bob");
bob.get("age").put(57);
Woah! Cool things happened there :I

So far we have only put string values on the graph, but you can put numbers on there too and even references to other nodes!

This means that if multiple things are referencing bob, and bob suddenly is one year older due to the unstoppable passing of time, then we just need to update the value on the bob node, and this will propagate through the system. Very cool!

How are peers actually connected…?
I had many general questions about connectivity and how data is stored when I first started out. Hopefully this is helpful to build some general understanding and intuition of gun…

You can initialize gun like so: const gun = Gun() in which case gun will just run on that one device, i.e. it will not link up to other devices. What we did in the very first code snippet was this: const gun = Gun(["https://.."]). This initializes gun with one or more peer relays. If two instances of gun specify the same peer relay (the same link), they will be able to connect to one another.

This was why clicking a button in a static, local .html file on your computer updated text on the official gun website!

Your computer only stores data that it actively listens to. So we are not filling up our computer with EVERYTHING that exists on the graph. Also, if we have two different peers: https://gun-peer-one.foo.com and https://gun-peer-two.foo.com it is perfectly possible for them to exist completely isolated from each other, and never sync data with each other.

If they ever DO get to know about each other all of a sudden (because a client specifies both peers in its initialization of gun) then data will be merged together. This is not a problem because data is inherently public when we are just storing it via gun.get and gun.put.

Why do we need relays (servers) at all?
This was supposed to be a peer to peer system. Well the job of the relay is just to connect two peers together. This has to do with how the web was build, and that clients (like your computer) can’t really listen to connections (from other clients) directly.

The relay may store some data and in general help the system by being an always online peer, but the idea of gun is not to rely on relays to store your data. The data should be stored by clients (like the users’ of your system) and synchronized between them whenever they are online.

Authentication and write protection
When we do things like gun.get(“foo”).put(“bar”) the data (i.e. “bar”) can be overridden by everyone. This is not always what we want. Lucky for us gun ships with methods for handling users and protected graphs. This is included in the SEA module of gun (In the beginning you will notice that we imported both gun.js and sea.js).

The “~” symbol
A user’s personal graph, that only that user can write to, is created from a special symbol: “~” (the tilde symbol) and a public key like so:

gun.get(`~${public_key}`); // this is a protected node.
If you attempt to write a value to a node of this format, you will get an error. That is because you need to hold the correct private keys that correspond to the public key of the node, for gun to accept your data.

A way to get such public and private keys is via SEA.pair. This function will return an object holding the public and private keys (key pair).

const pair = await SEA.pair();
// returns {pub, epub, priv, epriv}
We then need to tell gun to use the private pair for our write requests. We do this through the gun.user object. This has multiple methods for instance the auth method, that will do what we want.

const user = gun.user();
user.auth(pair, () => {
  // if auth succeded, gun.get will use correct private key.
});
Let’s combine what we know in a more complete example:

<script>
    const gun = Gun([]);
    const user = gun.user();
    const delay = () => new Promise(resolve => setTimeout(() => resolve(), 1000));

    (async () => {
        const pair = await SEA.pair();
        // setup listener
        gun.get(`~${pair.pub}`).get("protected").on(res => console.log(res));
        
        // will fail with: "Signature did not match." error.
        gun.get(`~${pair.pub}`).get("protected").put("hello");

        await delay();

        user.auth(pair, (auth) => {
            // will correctly put data.
            gun.get(`~${pair.pub}`).get("protected").put("hello");
        });

    })();
</script>
Because it is a bit cumbersome to write gun.get(`~${pair.pub]`) all the time, you can also use get via the user object. This will automatically use the right public key:

user.auth(pair, (auth) => {
    // gun.get(`~${pair.pub}`).get("protected").put("hello");
    user.get("protected").put("hello"); // same as above
});
There’s a pit fall here though! If you are not yet authenticated, doing user.get will just fail silently. This makes sense (the “it not working” part, not the “fail silently” part. Last one is a debugging nightmare…) since there is no public key tied to the user, so gun does not know how to construct the get request.

You should also note, that this automatic injection of keys into the get calls does not play nicely with authenticating multiple users! If you find yourself doing this, chances are you want to use a certificate instead — or rethink your database design… We will get into all this later.

Read protection?
If you want to keep people from reading your users’ information, you need to encrypt it. Then share the decryption keys with the relevant parties.

This is not directly related to gun, so I won’t go into details with it here. But let me know if this is something that you are interested in. I might do a section on it. SEA does however provide some nice higher level funcitons for encryption. This is how you might use the users private pair (used for authentication) to also encrypt a message:

const pair = user._.sea // will give you the pair of authenticated user.
user.get('secret').put(await SEA.encrypt("nobody else can read this", pair));
Streams and data iteration
If you put multiple things into a gun node, it is possible to iterate the values using map.

gun.get("foo").get("bar").put("hello from bar!");
gun.get("foo").get("baz").put("hello from baz!");

gun.get("foo").map().once(res => console.log(res));
// hello from bar!
// hello from baz!
If you put data into a node, and that data is structured in the same way, then map can be extremely effective at iterating over different types of values within that data. We could for instance get all the names of all the users pets in one line like so:

gun.get("users").map().get("pets").map().get("name").
  once(res => console.log(res));
// ex. Fido, Spot, ...
OBS: A thing about map, is that it will actually store (in local storage in the browser) all the values it is iterating. If you are connecting to a public relay this might quickly overload your users with data they don’t need.

Array-like data
Gun does not support adding arrays. In essence a gun node is like a mathematical set with no duplicate items. If you try to set the same key twice, gun will merge the value. It will not create two copies.

I say merge because you can actually pass in an object to gun.put. So far we have only been doing primitive values. Personally I try to always write our all gun.get myself, as I fell this is safer, but you could do:
gun.get.("foo").put({a: 1, b: 2}).
This is equivalent to doing:
gun.get.("foo").get("a").put(1);
gun.get("foo").get("b").put(2);

We can quite easily emulate an array by making sure that each key is unique. The following example shows this:

const gun_array = gun.get("array-like");
add_item(gun_array, "item");
add_item(gun_array, "item");

function add_item(node, item){
    const unique_key = crypto.randomUUID(); 
    // '706b7527-1718-4904-9f76-226c46737de6'
    node.get(unique_key).put(item)
}

gun_array.map().once(res => console.log(res));
// item, item
As we shall see in the next section, this might not always be what you want however…

Structuring iterated data
The problem with creating data like in the previous example, is that it can be quite difficult to locate a specific node in the “array” (set).

Let’s say that you have bought an item from a vendor, and now need to retrieve the gun node of that item, but you only know the item information (example coming up)… We would have to loop through the whole list of items and “look for” the relevant item. This is bad.

It would be much more efficient if the item was indexed by something unique, say a timestamp. But it could even be a combination of items.

const item = {name: "coca cola", bought_at: 1705513719345};

// with random index - BAD
gun.get("items").map().once(res => {
    if(res.bought_at === item.bought_at){
        // do something with item...
    }
});

// with timestamp index - GOOD
const item_node = gun.get("items").get(item.bought_at);
// do something with item.
See how much easier that was?! It is also a lot more efficient as the item list increases — and less error prone. If you have the chance to index your items with something non-random, it might make your life a lot easier down the road…

I will quickly mention a pit-fall here. Doing res.bought_at in the random index example is ONLY fine because the value is not nested. If the value is nested, it will not be available in the res object! I will get into the reason and solution for this in the next part.

Data streams and memory leaks
When using .once, the data is supposed to be read out only once. Sometimes this is not the case. When it comes to subscribing and unsubscribing to data, I find that gun can be a bit tricky.

Because of this, I find it useful to wrap the listeners that are iterating items in a stream. By doing this, you can be sure that rouge data does not find its way to your application if the stream is closed — even if the gun listener is not removed. It also centralizes the iteration logic, you can raise errors and in general it is just much nicer to work with.

const stream = new ReadableStream({
    start(controller){
        gun.get("items").map().once(item => {
            controller.enqueue(item)
        });
    }
    cancel(){
        // handle unsubscribing
    }
}

// USAGE EXAMPLES:
// get the stream
const items = stream.getReader();
// end the stream
items.cancel();
// listen to the stream
while(true){
    const {value, done} = await items.read();
    if(done){ break; }
    // do something with value... 
}
Souls, nodes, references and data
In this section we take a closer look at some concepts in gun, gaining a deeper understanding of how gun works internally.

As you know, gun is a graph database. Such a database relies a lot on references. If you ever try to do something that would create a nested object, gun will instead create a new object, and put a reference to that:

gun.get("test").put(
  {a: {b: "hello world!"}}
);
// essentially the same as
const b = gun.get("b-reference").get("b").put("hello world!");
gun.get("test").get("a").put(b);
Now instead of calling the attribute b simply “b-reference” as we did here, gun will give it a soul. That is just a fancy way of saying a computer generated unique id. If you have the soul of the node, you can always look it up by doing:
gun.get("the-soul-of-the-node").once(res => console.log(res);

To read out the soul of a node (if it has one!) you can do:

gun.get("test").once( res => {
  const soul = item._["#"];
  console.log( soul ); // logs soul.
}
Now, not all nodes have souls though. If you are reading a primitive value (such as “hello world!” in the previous example) this does not have a soul!

gun.get("b-reference").get("b").once( 
    res => console.log(res._["#"] ) // error ( res is "hello world!" )
); 
Using once you can get the object values at the node, but if the node that you are calling .once on only stores a soul reference, this is all you will see in the callback!

gun.get("test").once( 
    res => console.log(res); // a: {#: 'b/value'}
);
Well that was disappointing… But it is actually a feature that allow gun to stay performant. This way you are iterating a node (potentially with millions of attributes) but you are only getting the values that are directly set (not nested) on that node. That is amazing.

But what do you do then, if you want to get some values from one node, but also some values from another node? Consider the following more real-world-like example:

gun.get("car").get("type").put({brand: "toyota", year: "2004"});
gun.get("car").get("miles_driven").put(12423);

function get_car(cb) {
    // ???
    cb( {type: {brand, year}, miles_driven} )
}
Let’s first look at two ways we might go about this with our current knowledge about gun, and then a third way that introduces something new :)

Approach number one
Use .get multiple times starting from the same point, i.e. gun.get(“car”), but then branching out:

function get_car(cb) {
    gun.get("car").get("miles_driven").once(miles => {
        gun.get("car").get("type").once(type => {
            const miles_driven = miles;
            const brand = type.brand;
            const year = type.year;

            cb( {type: {brand, year}, miles_driven} )
        });
    });
}

Approach number two
Getting the soul and then making a direct lookup for that soul.

function get_car(cb) {
  gun.get("car").once(car => {
      const miles_driven = car.miles_driven;
      gun.get(car.type["#"]).once(type => { // important line.
          const brand = type.brand;
          const year = type.year;

          cb( {type: {brand, year}, miles_driven} )
      });
  });
}
Approach number three
Using this-context to get gun node instead of values. The following will NOT work with arrow-functions as we loose the this-context because of the way these are bound.

function get_car(cb) {
  gun.get("car").once(function(car){
      const miles_driven = car.miles_driven;
      this.get("type").once(function(type){ // important line.
          const brand = type.brand;
          const year = type.year;

          cb( {type: {brand, year}, miles_driven} )
      });
  });
}
I recommend using the last approach because it preserves the this-context throughout and can be easily incorporated into standardized classes as we will see later. But for now, the three approaches should serve as a way to build understanding of the inner workings of gun.

Hopefully that all makes sense and you will be able to find your way around the gun terminology and avoid the most common pitfalls when it comes to souls, nodes and values!

Certificates and protected shared spaces
As mentioned in the section on authentication and write protection, prepending a public key with the tilde-symbol (~) designates it as a restricted area where only the owner of the matching private key can write data.

One way of creating a shared space is to generate a pair await SEA.pair() and then share that pair with another user (in a secure way).

// on alice's side.
const group = await SEA.pair();
share(group);

// on bob's side.
receive().
  then(group => gun.user().auth(group));
This has one MAJOR disadvantage however. Gun is not that happy about being authenticated with multiple users at the same time. To avoid errors in the long term, you should not authenticate multiple users at the same time!

That means that bob can no longer write to his own graph, if he needs to write something to the group. Signing in and out of multiple users quickly gets out of hand.

To solve this problem we will now look at certificates, that allow a user to write to a specific space on a different users graph!

There are two components to this: 1) Creating certificates and
2) Using certificates.

Using certificates
Using the certificate is pretty easy. Simply pass in the certificate to the .put method like so:

const certificate = // we will get to this...
gun.get(`~${pubkey}`).get("bar").put("baz", null, {opt: {cert: certificate }});
If the certificate is valid for the operation we are trying to do, gun will accept this write, even if we were not authenticated.

Small note… if the certificate is not valid you may get a signature failed error message. In some cases subsequent calls to put will silently break. Be sure to use the correct certificate and see this github issue for more info.

Another gotcha is that the options object that is passed to the function is mutated. For this reason consider doing a structuredClone on the options before passing them to the put-method:

const options = {opt: cert: /* certificate */ }}
gun.get(`~${pubkey}`).get("bar").put(
  "baz", 
  null, 
  structuredClone(options) // ensure that options is not mutated
);

// other gun.get calls using same options.
Creating certificates
You create certificates via the SEA.certify method. The method requires three things as input:
1) public keys of users who will be given access (grantees)
2) the part of the graph for which access will be given (writable spaces)
3) the user who gives the access. (issuer)

Grantees
This one is pretty simple. It is an array of objects that have a single property, pub, that is a string with the public key that will be given access through the certificate.
Writable spaces
This is an array of objects, that define requirements for where the certificate is valid. You can get pretty fancy with this. For advanced examples see the docs.
Issuer
Simply the pair of the user giving access.
Based on this we can construct the code for creating a certificate:

// give one user access ...
const grantees = [{pub: "some-public-key"}] 

// to everything that starts with "some-path, 
// e.g. gun.get(`~${pub}`).get("some-path").put({hello: "world"});
const writable_spaces = [{"*": "some-path"}]

// using a user's pair (if authenticated)
const issuer = gun.user()?._?.sea;

const certificate = await SEA.certify(
  grantees,
  writable_spaces,
  issuer
);

Using this approach we can be authenticated with one user but still write to many other users’ graphs using different certificates. There is nothing secret about the certificates (they are encrypted), so they can even be shared via the graph.

What you would typically do is create a certificate and the store that certificate somewhere on the graph of the user who created it. Everyone can read the encrypted certificate. Only the specified users can use it. And no one can override the certificate (given that this is not allowed by the/a certificate).

Gun nodes are… promises?
The philosophy of gun is that is should be easy to pick up and write a decentralized database in just a few lines of code. Unfortunately this means that gun is written with some bad practices such as monkey patching and can have some unpredictable side effects when used in larger projects.

One such thing is that when you import the SEA library, gun will extend its gun nodes with a .then function. This function is a promise that runs .once under the hood to resolve the promise with the value that resides at this node:

// after importing SEA
gun.get("root").get("some-node").put("hello");
const value = await gun.get("root").get("some-node");
// expected output: "hello"
It is pretty weird that you can use await like this, but it’s possible. A pitfall here is that if you return the gun node from an async function, you will get the value and not the node!

async function test() {
  return gun.get("root").get("some-node");
}
const node = await test();
node.get("test") // .get is not a function error!
Instead you will need to return the node wrapped in an object, as to not accidentally await it:

async function test2() {
  return {node: gun.get("root").get("some-node")};
}
const {node} = await test();
node.get("test") // works!
OBS: Another thing to consider is that if the value you are trying to await does not exist, you will never resolve the promise and your code will hang indefinitely.

To fix this you may do a Promise.race on the gun.then promise along with a timeout promise to ensure that the value resolves (to undefined possibly). More on this later.

Gun options
This section will cover options that you can pass to gun. I am still experimenting with these. Stay tuned!

Gun hooks
We are getting more and more into the weeds. If you are still reading this article, I salute you good sir! Gun hooks are all about hooking into the Gun code, i.e. run some of your code as part of the internal code that gun is running.

You can run two hooks on the main Gun object. Those are: create and opt

You can run seven hooks on the gun instance. Those are: hi, bye, get, put, in, out, and auth.

Say you wanted to run code whenever gun is created or the options are updated. This is done via the “create” and “opt” hooks:

Gun.on("create", function(root){
    console.log("create", root); // called when gun is created
    this.to.next(root);
});

Gun.on("opt", function(root){
    console.log("opt", root) // called whenever options are updated.
    this.to.next(root);
});

const gun = Gun(); // create
const gun.opt({peers: []}) // udpate options
The following code will first run in this order:

opt hooks (as part of creating a gun instance)
then create hook (notice that this is called after options)
then options hook again. (because we are changing options later)
OBS: In gun hooks we must manually call this.to.next if other hooks need to be called down the line, or if messages should be passed on.

The parameter that is passed to the callback function in the hook is the gun instance. So you can tab into more hooks within your hooks :)

Here is a list of what the other hooks do:

Gun.on("create", function(gun) {
    gun.on("hi", function(event){
        console.log(`Hello from ${event.url}`);
        this.next.to(event);
    })

    gun.on("bye", function(event){
        console.log(`Goodbye from ${event.url}`);
        this.next.to(event);
    })

    gun.on("auth", function(event){
        console.log(`Authenticated a user with public key: ${event.sea.pub}`);
        this.next.to(event);
    })

    gun.on("out", function(event){
        console.log("Sending some data", event);
        this.next.to(event);
    })

    gun.on("in", function(event){
        console.log("Receiving some data", event);
        this.next.to(event);
    })

    gun.on("put", function(event){
        console.log("Putting some data", event);
        this.next.to(event);
    })

    gun.on("get", function(event){
        console.log(
            `get data with path, key: ` +
            `(${event.get?.["#"]}, ${event.get?.["."]})`
        );
    })

  this.next.to(event);
})
Putting it ALL together
We now have a solid grasp of how Gun operates and how to utilize its features effectively. Gun can be incredibly powerful when used correctly. However, to prevent unexpected failures or issues with Gun’s functionality, I propose that we construct a blueprint or wrapper class. This wrapper class will encapsulate all the key concepts and functionalities we have discussed up until now, providing a structured and reliable framework for all our Gun implementation.

OBS: The following is code that illustrates thoughts and patterns. It is not guaranteed to work.

Using a singleton
Gun is not too happy about creating multiple instances of itself. It will mutate things and it has side effects. To make sure we only instantiate one instance of gun, we can make use of a singleton pattern. Let’s also throw a reference to SEA in there, so we only need to import Gun and SEA in one file.

import Gun from "gun/gun"; // assumes npm
import SEA from "gun/sea";

export default class MainGun {

    static instance;
    static Gun = Gun;
    static SEA = SEA;

    constructor(options) {
        if(MainGun.instance) {
            return MainGun.instance;
        }
        this.gun = Gun(options);
        MainGun.instance = this;
    }

}
Using an app scope
If you call .map on something, gun will store the entire map in its database (localStorage if in browser). To avoid mapping over irrelevant data, we make use of an app scope, and store all of our data either in user spaces or at that app scope.

// in MainGun
get chain() {
    // the app_scope could be imported from a config file
    // or from an environment variable potentially.
    return this.gun.get(app_scope)
}
When putting anything on the graph we should now start with MainGun.instance.chain.get("some-key")

Defining the database structure
There is a lot of string input to gun get requests. To avoid accidentally misspelling something we can hard code the database structure by defining some base classes that wrap gun nodes and then creating more advanced versions on top of those. Like so:

// A base class
export class GunNode() {
    constructor(chain){ // a gun chain like gun.get("some-key")
        this.chain = chain;
    }

    put(value) {
        this.chain.put(value)
    }
}

// main entry node for our application
export class EntryNode extends GunNode() {
    constructor(chain) {
        super(chain)
    }
    
    info() {
        return new InfoNode(this.chain.get("info"));
    }
}

// A node with some info about our application
export class InfoNode extends GunNode() {
    constructor(chain) {
       super(chain)
    }
    
    version = {
        number: () => new GunNode(this.chain.get("version"));
        name: () => new GunNode(this.chain.get("name"));
    }
    
}
Let’s update the chain attribute in MainGun to give back our own GunNode class:

// in MainGun
get chain() {
    return new EntryNode(this.gun.get(app_scope))
}
Using our new classes we can navigate our graph quite easily by doing:

MainGun.instance.chain.info.version.name.put("first version");
We even get full IntelliSense of our database! We can do more with this than just defining our database though, as we will see…

Using GunNodes in async functions
One thing we already got for free is that we can now await our GunNodes without them being modified or our application potentially hanging indefinitely! This is because we wrap the gun chain in an object, so it does not get awaited (see section on gun nodes as promises).

async function test_bad() {
    return gun.get("test");
}
async function test_fine() {
    return new GunNode(gun.get("test"));
}

const node = await test_fine(); // works fine.
const node = await test_bad(); // potentially hangs forever.
TIP: If we ever do need to access the underlying gun chain we can just call .chain on the GunNode.

Working with iterated data
Lets add some functionality to our GunNode to handle easy iteration for us. Same as what map does, but we want some additional functionality and intellisense.

// in GunNode
constructor(chain, iterates){
    // ...
    this.iterates = iterates ? iterates : GunNode
}

map() {
    return new this.iterates(this.chain.map())
}

// usage, e.g in EntryNode
users() {
    return new GunNode(this.chain.get("users"), UserNode);
}

// usage in code somewhere
MainGun.instance.chain.users.map().name // Map refers to a UserNode now.
Reading data
As mentioned before, data in gun should not really be “read”.. It should be “watched”. Let’s add a watch function to our gun node, that will watch the value on that node.

// in GunNode
watch(initial) {
    const subscribe = (cb) => {
        let ev;
        cb({value: initial, key: "", node: null})
        this.chain.on(function(value, key, message, event){
            ev = event;
            const node = value ? new GunNode(this) : null;
            cb({value, key, node});
        });
        return () => ev?.off() // return unsubscriber
    }
    return {subscribe}
}

// usage
const name_store = MainGun.instance.chain.info.version.name.watch();
const unsubscribe = name_store.subscribe(({value}) => console.log(value));

// in svelte.js simply do $name_store.value to get value.
This has one problem though. If there has not been a call to .on before you call unsubscribe, the ev variable will not be populated and unsubscribing will not be possible. This will result in a memory leak, which would need to be scheduled for clean-up later (after data has been received). To be sure that no data find its way into our application, we could however wrap the .on in a stream (see section on streams). I will not go into the details of that here.

Automatic putting with certificates
For working with certificates we may add some convenience methods somewhere for generation. GunNodes could then hold and pass along certificates like so:

// in GunNode
constructor(chain, {certificate}){
    this.chain = chain;
    this.certificate = certificate // may be undefied.
}
put(value){
    const options = 
        this.certificate ? {opt: cert: this.certificate} : undefined;
    this.chain.put(value, options);
}

// A group node, that is write protected
export GroupNode extends GunNode {
    constructor(chain){
        super(chain);
    }

    name: () => return new GunNode(
        this.chain.get("name"), {certificate: this.certificate} )
}

// Usage
const pair = await MainGun.SEA.pair();
const chain = MainGun.gun.get(`~${pair.pub}`);
const certificate = MainGun.SEA
const group_node = new GroupNode(node);
group_node.certicate = certificate;

// certificate is automatically passed through the chain.
group_node.name.put("a new group") 
Thanks for getting this far. This article is an ongoing project, and I will be updating it every week. Next we will have a look at creating certificates and giving other users access to parts of the graph. Come back later to continue reading :)
