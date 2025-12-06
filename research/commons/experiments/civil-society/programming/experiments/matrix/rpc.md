https://blog.cloudflare.com/capnweb-javascript-rpc-library/

▼
The Cloudflare Blog
magnifier icon
hamburger menu

Cap'n Web: a new RPC system for browsers and web servers
2025-09-22

Kenton Varda
Kenton Varda
Steve Faulkner
Steve Faulkner
12 min read
This post is also available in 简体中文.

Allow us to introduce Cap'n Web, an RPC protocol and implementation in pure TypeScript.

Cap'n Web is a spiritual sibling to Cap'n Proto, an RPC protocol I (Kenton) created a decade ago, but designed to play nice in the web stack. That means:

Like Cap'n Proto, it is an object-capability protocol. ("Cap'n" is short for "capabilities and".) We'll get into this more below, but it's incredibly powerful.

Unlike Cap'n Proto, Cap'n Web has no schemas. In fact, it has almost no boilerplate whatsoever. This means it works more like the JavaScript-native RPC system in Cloudflare Workers.

That said, it integrates nicely with TypeScript.

Also unlike Cap'n Proto, Cap'n Web's underlying serialization is human-readable. In fact, it's just JSON, with a little pre-/post-processing.

It works over HTTP, WebSocket, and postMessage() out-of-the-box, with the ability to extend it to other transports easily.

It works in all major browsers, Cloudflare Workers, Node.js, and other modern JavaScript runtimes.

The whole thing compresses (minify+gzip) to under 10 kB with no dependencies.

It's open source under the MIT license.

Cap'n Web is more expressive than almost every other RPC system, because it implements an object-capability RPC model. That means it:

Supports bidirectional calling. The client can call the server, and the server can also call the client.

Supports passing functions by reference: If you pass a function over RPC, the recipient receives a "stub". When they call the stub, they actually make an RPC back to you, invoking the function where it was created. This is how bidirectional calling happens: the client passes a callback to the server, and then the server can call it later.

Similarly, supports passing objects by reference: If a class extends the special marker type RpcTarget, then instances of that class are passed by reference, with method calls calling back to the location where the object was created.

Supports promise pipelining. When you start an RPC, you get back a promise. Instead of awaiting it, you can immediately use the promise in dependent RPCs, thus performing a chain of calls in a single network round trip.

Supports capability-based security patterns.

In short, Cap'n Web lets you design RPC interfaces the way you'd design regular JavaScript APIs – while still acknowledging and compensating for network latency.

The best part is, Cap'n Web is absolutely trivial to set up.

A client looks like this:

import { newWebSocketRpcSession } from "capnweb";

// One-line setup.
let api = newWebSocketRpcSession("wss://example.com/api");

// Call a method on the server!
let result = await api.hello("World");

console.log(result);
And here's a complete Cloudflare Worker implementing an RPC server:

import { RpcTarget, newWorkersRpcResponse } from "capnweb";

// This is the server implementation.
class MyApiServer extends RpcTarget {
  hello(name) {
    return `Hello, ${name}!`
  }
}

// Standard Workers HTTP handler.
export default {
  fetch(request, env, ctx) {
    // Parse URL for routing.
    let url = new URL(request.url);

    // Serve API at `/api`.
    if (url.pathname === "/api") {
      return newWorkersRpcResponse(request, new MyApiServer());
    }

    // You could serve other endpoints here...
    return new Response("Not found", {status: 404});
  }
}
That's it. That's the app.

You can add more methods to MyApiServer, and call them from the client.

You can have the client pass a callback function to the server, and then the server can just call it.

You can define a TypeScript interface for your API, and easily apply it to the client and server.

It just works.

Why RPC? (And what is RPC anyway?)
Remote Procedure Calls (RPC) are a way of expressing communications between two programs over a network. Without RPC, you might communicate using a protocol like HTTP. With HTTP, though, you must format and parse your communications as an HTTP request and response, perhaps designed in REST style. RPC systems try to make communications look like a regular function call instead, as if you were calling a library rather than a remote service. The RPC system provides a "stub" object on the client side which stands in for the real server-side object. When a method is called on the stub, the RPC system figures out how to serialize and transmit the parameters to the server, invoke the method on the server, and then transmit the return value back.

The merits of RPC have been subject to a great deal of debate. RPC is often accused of committing many of the fallacies of distributed computing.

But this reputation is outdated. When RPC was first invented some 40 years ago, async programming barely existed. We did not have Promises, much less async and await. Early RPC was synchronous: calls would block the calling thread waiting for a reply. At best, latency made the program slow. At worst, network failures would hang or crash the program. No wonder it was deemed "broken".

Things are different today. We have Promise and async and await, and we can throw exceptions on network failures. We even understand how RPCs can be pipelined so that a chain of calls takes only one network round trip. Many large distributed systems you likely use every day are built on RPC. It works.

The fact is, RPC fits the programming model we're used to. Every programmer is trained to think in terms of APIs composed of function calls, not in terms of byte stream protocols nor even REST. Using RPC frees you from the need to constantly translate between mental models, allowing you to move faster.

When should you use Cap'n Web?
Cap'n Web is useful anywhere where you have two JavaScript applications speaking to each other over a network, including client-to-server and microservice-to-microservice scenarios. However, it is particularly well-suited to interactive web applications with real-time collaborative features, as well as modeling interactions over complex security boundaries.

Cap'n Web is still new and experimental, so for now, a willingness to live on the cutting edge may also be required!

Features, features, features…
Here's some more things you can do with Cap'n Web.

HTTP batch mode
Sometimes a WebSocket connection is a bit too heavyweight. What if you just want to make a quick one-time batch of calls, but don't need an ongoing connection?

For that, Cap'n Web supports HTTP batch mode:

import { newHttpBatchRpcSession } from "capnweb";

let batch = newHttpBatchRpcSession("https://example.com/api");

let result = await batch.hello("World");

console.log(result);
(The server is exactly the same as before.)

Note that once you've awaited an RPC in the batch, the batch is done, and all the remote references received through it become broken. To make more calls, you need to start over with a new batch. However, you can make multiple calls in a single batch:

let batch = newHttpBatchRpcSession("https://example.com/api");

// We can call make multiple calls, as long as we await them all at once.
let promise1 = batch.hello("Alice");
let promise2 = batch.hello("Bob");

let [result1, result2] = await Promise.all([promise1, promise2]);

console.log(result1);
console.log(result2);
And that brings us to another feature…

Chained calls (Promise Pipelining)
Here's where things get magical.

In both batch mode and WebSocket mode, you can make a call that depends on the result of another call, without waiting for the first call to finish. In batch mode, that means you can, in a single batch, call a method, then use its result in another call. The entire batch still requires only one network round trip.

For example, say your API is:

class MyApiServer extends RpcTarget {
  getMyName() {
    return "Alice";
  }

  hello(name) {
    return `Hello, ${name}!`
  }
}
You can do:

let namePromise = batch.getMyName();
let result = await batch.hello(namePromise);

console.log(result);
Notice the initial call to getMyName() returned a promise, but we used the promise itself as the input to hello(), without awaiting it first. With Cap'n Web, this just works: The client sends a message to the server saying: "Please insert the result of the first call into the parameters of the second."

Or perhaps the first call returns an object with methods. You can call the methods immediately, without awaiting the first promise, like:

let batch = newHttpBatchRpcSession("https://example.com/api");

// Authencitate the API key, returning a Session object.
let sessionPromise = batch.authenticate(apiKey);

// Get the user's name.
let name = await sessionPromise.whoami();

console.log(name);
This works because the promise returned by a Cap'n Web call is not a regular promise. Instead, it's a JavaScript Proxy object. Any methods you call on it are interpreted as speculative method calls on the eventual result. These calls are sent to the server immediately, telling the server: "When you finish the call I sent earlier, call this method on what it returns."

Did you spot the security?
This last example shows an important security pattern enabled by Cap'n Web's object-capability model.

When we call the authenticate() method, after it has verified the provided API key, it returns an authenticated session object. The client can then make further RPCs on the session object to perform operations that require authorization as that user. The server code might look like this:

class MyApiServer extends RpcTarget {
  authenticate(apiKey) {
    let username = await checkApiKey(apiKey);
    return new AuthenticatedSession(username);
  }
}

class AuthenticatedSession extends RpcTarget {
  constructor(username) {
    super();
    this.username = username;
  }

  whoami() {
    return this.username;
  }

  // ...other methods requiring auth...
}
Here's what makes this work: It is impossible for the client to "forge" a session object. The only way to get one is to call authenticate(), and have it return successfully.

In most RPC systems, it is not possible for one RPC to return a stub pointing at a new RPC object in this way. Instead, all functions are top-level, and can be called by anyone. In such a traditional RPC system, it would be necessary to pass the API key again to every function call, and check it again on the server each time. Or, you'd need to do authorization outside the RPC system entirely.

This is a common pain point for WebSockets in particular. Due to the design of the web APIs for WebSocket, you generally cannot use headers nor cookies to authorize them. Instead, authorization must happen in-band, by sending a message over the WebSocket itself. But this can be annoying for RPC protocols, as it means the authentication message is "special" and changes the state of the connection itself, affecting later calls. This breaks the abstraction.

The authenticate() pattern shown above neatly makes authentication fit naturally into the RPC abstraction. It's even type-safe: you can't possibly forget to authenticate before calling a method requiring auth, because you wouldn't have an object on which to make the call. Speaking of type-safety…

TypeScript
If you use TypeScript, Cap'n Web plays nicely with it. You can declare your RPC API once as a TypeScript interface, implement in on the server, and call it on the client:

// Shared interface declaration:
interface MyApi {
  hello(name: string): Promise<string>;
}

// On the client:
let api: RpcStub<MyApi> = newWebSocketRpcSession("wss://example.com/api");

// On the server:
class MyApiServer extends RpcTarget implements MyApi {
  hello(name) {
    return `Hello, ${name}!`
  }
}
Now you get end-to-end type checking, auto-completed method names, and so on.

Note that, as always with TypeScript, no type checks occur at runtime. The RPC system itself does not prevent a malicious client from calling an RPC with parameters of the wrong type. This is, of course, not a problem unique to Cap'n Web – JSON-based APIs have always had this problem. You may wish to use a runtime type-checking system like Zod to solve this. (Meanwhile, we hope to add type checking based directly on TypeScript types in the future.)

An alternative to GraphQL?
If you’ve used GraphQL before, you might notice some similarities. One benefit of GraphQL was to solve the “waterfall” problem of traditional REST APIs by allowing clients to ask for multiple pieces of data in one query. For example, instead of making three sequential HTTP calls:

GET /user
GET /user/friends
GET /user/friends/photos
…you can write one GraphQL query to fetch it all at once.

That’s a big improvement over REST, but GraphQL comes with its own tradeoffs:

New language and tooling. You have to adopt GraphQL’s schema language, servers, and client libraries. If your team is all-in on JavaScript, that’s a lot of extra machinery.

Limited composability. GraphQL queries are declarative, which makes them great for fetching data, but awkward for chaining operations or mutations. For example, you can’t easily say: “create a user, then immediately use that new user object to make a friend request, all-in-one round trip.”

Different abstraction model. GraphQL doesn’t look or feel like the JavaScript APIs you already know. You’re learning a new mental model rather than extending the one you use every day.

How Cap'n Web goes further
Cap'n Web solves the waterfall problem without introducing a new language or ecosystem. It’s just JavaScript. Because Cap'n Web supports promise pipelining and object references, you can write code that looks like this:

let user = api.createUser({ name: "Alice" });
let friendRequest = await user.sendFriendRequest("Bob");
What happens under the hood? Both calls are pipelined into a single network round trip:

Create the user.

Take the result of that call (a new User object).

Immediately invoke sendFriendRequest() on that object.

All of this is expressed naturally in JavaScript, with no schemas, query languages, or special tooling required. You just call methods and pass objects around, like you would in any other JavaScript code.

In other words, GraphQL gave us a way to flatten REST’s waterfalls. Cap'n Web lets us go even further: it gives you the power to model complex interactions exactly the way you would in a normal program, with no impedance mismatch.

But how do we solve arrays?
With everything we've presented so far, there's a critical missing piece to seriously consider Cap'n Web as an alternative to GraphQL: handling lists. Often, GraphQL is used to say: "Perform this query, and then, for every result, perform this other query." For example: "List the user's friends, and then for each one, fetch their profile photo."

In short, we need an array.map() operation that can be performed without adding a round trip.

Cap'n Proto, historically, has never supported such a thing.

But with Cap'n Web, we've solved it. You can do:

let user = api.authenticate(token);

// Get the user's list of friends (an array).
let friendsPromise = user.listFriends();

// Do a .map() to annotate each friend record with their photo.
// This operates on the *promise* for the friends list, so does not
// add a round trip.
// (wait WHAT!?!?)
let friendsWithPhotos = friendsPromise.map(friend => {
  return {friend, photo: api.getUserPhoto(friend.id))};
}

// Await the friends list with attached photos -- one round trip!
let results = await friendsWithPhotos;
Wait… How!?
.map() takes a callback function, which needs to be applied to each element in the array. As we described earlier, normally when you pass a function to an RPC, the function is passed "by reference", meaning that the remote side receives a stub, where calling that stub makes an RPC back to the client where the function was created.

But that is NOT what is happening here. That would defeat the purpose: we don't want the server to have to round-trip to the client to process every member of the array. We want the server to just apply the transformation server-side.

To that end, .map() is special. It does not send JavaScript code to the server, but it does send something like "code", restricted to a domain-specific, non-Turing-complete language. The "code" is a list of instructions that the server should carry out for each member of the array. In this case, the instructions are:

Invoke api.getUserPhoto(friend.id).

Return an object {friend, photo}, where friend is the original array element and photo is the result of step 1.

But the application code just specified a JavaScript method. How on Earth could we convert this into the narrow DSL?

The answer is record-replay: On the client side, we execute the callback once, passing in a special placeholder value. The parameter behaves like an RPC promise. However, the callback is required to be synchronous, so it cannot actually await this promise. The only thing it can do is use promise pipelining to make pipelined calls. These calls are intercepted by the implementation and recorded as instructions, which can then be sent to the server, where they can be replayed as needed.

And because the recording is based on promise pipelining, which is what the RPC protocol itself is designed to represent, it turns out that the "DSL" used to represent "instructions" for the map function is just the RPC protocol itself. 🤯

Implementation details
JSON-based serialization
Cap'n Web's underlying protocol is based on JSON – but with a preprocessing step to handle special types. Arrays are treated as "escape sequences" that let us encode other values. For example, JSON does not have an encoding for Date objects, but Cap'n Web does. You might see a message that looks like this:

{
  event: "Birthday Week",
  timestamp: ["date", 1758499200000]
}
To encode a literal array, we simply double-wrap it in []:

{
  names: [["Alice", "Bob", "Carol"]]
}
In other words, an array with just one element which is itself an array, evaluates to the inner array literally. An array whose first element is a type name, evaluates to an instance of that type, where the remaining elements are parameters to the type.

Note that only a fixed set of types are supported: essentially, "structured clonable" types, and RPC stub types.

On top of this basic encoding, we define an RPC protocol inspired by Cap'n Proto – but greatly simplified.

RPC protocol
Since Cap'n Web is a symmetric protocol, there is no well-defined "client" or "server" at the protocol level. There are just two parties exchanging messages across a connection. Every kind of interaction can happen in either direction.

In order to make it easier to describe these interactions, I will refer to the two parties as "Alice" and "Bob".

Alice and Bob start the connection by establishing some sort of bidirectional message stream. This may be a WebSocket, but Cap'n Web also allows applications to define their own transports. Each message in the stream is JSON-encoded, as described earlier.

Alice and Bob each maintain some state about the connection. In particular, each maintains an "export table", describing all the pass-by-reference objects they have exposed to the other side, and an "import table", describing the references they have received. Alice's exports correspond to Bob's imports, and vice versa. Each entry in the export table has a signed integer ID, which is used to reference it. You can think of these IDs like file descriptors in a POSIX system. Unlike file descriptors, though, IDs can be negative, and an ID is never reused over the lifetime of a connection.

At the start of the connection, Alice and Bob each populate their export tables with a single entry, numbered zero, representing their "main" interfaces. Typically, when one side is acting as the "server", they will export their main public RPC interface as ID zero, whereas the "client" will export an empty interface. However, this is up to the application: either side can export whatever they want.

From there, new exports are added in two ways:

When Alice sends a message to Bob that contains within it an object or function reference, Alice adds the target object to her export table. IDs assigned in this case are always negative, starting from -1 and counting downwards.

Alice can send a "push" message to Bob to request that Bob add a value to his export table. The "push" message contains an expression which Bob evaluates, exporting the result. Usually, the expression describes a method call on one of Bob's existing exports – this is how an RPC is made. Each "push" is assigned a positive ID on the export table, starting from 1 and counting upwards. Since positive IDs are only assigned as a result of pushes, Alice can predict the ID of each push she makes, and can immediately use that ID in subsequent messages. This is how promise pipelining is achieved.

After sending a push message, Alice can subsequently send a "pull" message, which tells Bob that once he is done evaluating the "push", he should proactively serialize the result and send it back to Alice, as a "resolve" (or "reject") message. However, this is optional: Alice may not actually care to receive the return value of an RPC, if Alice only wants to use it in promise pipelining. In fact, the Cap'n Web implementation will only send a "pull" message if the application has actually awaited the returned promise.

Putting it together, a code sequence like this:

let namePromise = api.getMyName();
let result = await api.hello(namePromise);

console.log(result);
Might produce a message exchange like this:

// Call api.getByName(). `api` is the server's main export, so has export ID 0.
-> ["push", ["pipeline", 0, "getMyName", []]
// Call api.hello(namePromise). `namePromise` refers to the result of the first push,
// so has ID 1.
-> ["push", ["pipeline", 0, "hello", [["pipeline", 1]]]]
// Ask that the result of the second push be proactively serialized and returned.
-> ["pull", 2]
// Server responds.
<- ["resolve", 2, "Hello, Alice!"]
For more details about the protocol, check out the docs.

Try it out!
Cap'n Web is new and still highly experimental. There may be bugs to shake out. But, we're already using it today. Cap'n Web is the basis of the recently-launched "remote bindings" feature in Wrangler, allowing a local test instance of workerd to speak RPC to services in production. We've also begun to experiment with it in various frontend applications – expect more blog posts on this in the future.

In any case, Cap'n Web is open source, and you can start using it in your own projects now.

Check it out on GitHub.

BLOG-2954 2

Cloudflare's connectivity cloud protects entire corporate networks, helps customers build Internet-scale applications efficiently, accelerates any website or Internet application, wards off DDoS attacks, keeps hackers at bay, and can help you on your journey to Zero Trust.

Visit 1.1.1.1 from any device to get started with our free app that makes your Internet faster and safer.

To learn more about our mission to help build a better Internet, start here. If you're looking for a new career direction, check out our open positions.
 Discuss on Hacker News
Birthday WeekOpen SourceJavaScriptCloudflare WorkersDeveloper PlatformDevelopers
Follow on X

Kenton Varda|@kentonvarda
Cloudflare|@cloudflare
Related posts

December 01, 2025 3:00 AM

Why Replicate is joining Cloudflare
Today, we’re excited to announce that Replicate is officially part of Cloudflare. We wanted to share a bit about our journey and why we made this decision. ...

By 
Andreas Jansson, 
Ben Firshman
AI, 
Workers AI, 
Developers, 
Acquisitions 
November 24, 2025 9:00 PM

Partnering with Black Forest Labs to bring FLUX.2 [dev] to Workers AI
FLUX.2 [dev] by Black Forest Labs is now on Workers AI! This advanced open-weight image model offers superior photorealism, multi-reference inputs, and granular control with JSON prompting....

By 
Michelle Chen, 
David Liu
AI, 
Workers AI, 
Developers 
November 17, 2025 11:00 AM

Replicate is joining Cloudflare
Bringing Replicate’s tools into Cloudflare will continue to make our Workers Platform the best place on the Internet to build and deploy any AI or agentic workflow. ...

By 
Rita Kozlov, 
Ben Firshman
Acquisitions, 
Developer Platform, 
Developers, 
AI 
November 12, 2025 11:00 AM

Connecting to production: the architecture of remote bindings
Remote bindings allow you to connect your local Worker code to deployed Cloudflare resources like R2 and D1. Come along on the technical journey of how we built this feature to create a seamless local development experience....

By 
Samuel Macleod, 
Dario Piotrowicz
Cloudflare Workers, 
R2, 
D1 
facebook
X
linkedin
youtube
instagram
© 2025 Cloudflare, Inc. | Privacy Policy | Terms of Use | Report Security Issues |Privacy OptionsCookie Preferences| Trademark