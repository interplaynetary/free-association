# GUN — Decentralized database. Five things I wish I had known

I have been getting into developing applications with gun. It is a really cool decentralized, streaming database. It’s super fun to play around with. For a big project it does require a lot of extra work to get working properly. But all in all, I would recommend trying it out!

If you do start out fiddling with gun beyond the small example code, this article contains what I wish someone would have told me before I got started! So here you go : )

Watch values. Dont read too much.
Gun is a streaming database. It streams in data. You will want to work with this, not against it. Don’t rely too much on reading off values from gun, then doing a bunch of stuff without gun, and trying to put the values back into it…. Maybe better with an example:

// Dont do this
let info = {name: "loading..."};
gun.get("info").get("name").once(name => info.name = name);

// And then later...
info.name = "updated name";

// And then even later...
gun.get("info").get("name").put(info.name);
This might work fine when name is just a single value being read. But weird things will start to happen once you begin building up objects from these values, or passing info around thinking it has some value that you’ve read at a particular point in time, setting values of name in the client code, but also having gun potentially update the value suddenly. This is not the streaming mindset.

Instead subscribe to the name value. If you want to change the value, put the value into gun, and let the value get back to you through your subscription. I am using writable here, which is a svelte.js thing. But it is just something that allow for setting a value, and letting subscribers know about the changes.

const name_store = writable("loading...");
gun.get("info").get("name").once(name => name.store.set(name));

// And then later
name_store.subscribe(name => {
  // will be called with "loading..." 
  // and then again with "updated name".
});

gun.get("info").get("name").put("updated name");
This will be annoying at times, but it will force you into the streaming mindset which will safe you a lot of time in the long run!

// if you really want to read values, there is a pretty neat way.
// This is experimental, and a bit error-prone at times though.
// I recommend being a bit careful with this, but it's there if you need.
const name = await gun.get("info").get("name");
console.log(name) // example: "mark"
Get ready to test… A lot
Go fast and break stuff… That’s the Facebook motto. With gun it’s more like: Go really slow, and probably still break stuff. Gun is quirky, it has side effects and sometimes it will hide an error from you for days. Test your code and functions in isolation. Validate that they actually do work how you think they work.

A trick that might be useful, is to await a node. If there are anything wrong, the promise will hang indefinitely.

const foo = await gun.get("test").get("foo");
console.log(foo) // if this runs, we are good 👍
Create a gun-adapter / wrapper classes
Don’t hard code gun calls directly into your app. Put these into an adapter or wrapper class and use that. This will make sure that you are not doing weird stuff, it will centralize your gun-calls and make it easier to debug.

const user_node = new UserNode(gun.user());
//...
class UserNode {
  constructor(user){
    this.user = user;
  }
  
  get_pets_node(){
    return this.user.get("pets");
  }
  
}
I’m not saying: do exactly this. But the code serves as a general idea.

Leverage node linking
Make sure you connect your nodes in a nice way. If you have the right database structure you can do cool things like getting all the names of all of your friends pets, in just one line!

user.get("friends").map().get("pets").map().get("name").once(
  name => console.log(name)
);
What you absolutely don’t want to start doing is dumping JSON-data or big strings into the database:

user.get("friends").put(
  JSON.stringify({friends: [{name: "john", pets: [{name: "fido"}]}]});
);
Why would you be inclined to do this? Say you want to encrypt the data… In this case, make sure you are encrypting the individual values and keeping the overall database structure!

Use typescript
Gun is a really cool project. I love it a lot. But. Errors. They are really difficult to track down at times.

Like. Yea. Really. Difficult.

Using typescript will save you a lot of debugging hours. Just use it. I know, I wasn’t really sold on it until I started using it either. But it is actually real nice :)

BONUS TIP: Wrap map in streams
Using .map in gun, it is possible to iterate over data and subscribe to changes on each of them. What you may run into is that it can be a challenge to unsubscribe from the map calls again. Consider wrapping .map.once in a ReadableStream. That way, if the stream is closed, you can be sure that data will not find its way into your appliation.

stream = new ReadableStream({
  start(controller){
    gun.get("foo").map().once(value => {
      controller.enqueue(value);
    });
  },
  cancel(){
    // attempt to handle unsubscribing...
  }
});

const reader = stream.getReader();
// ... read data

reader.close(); 
// calling controller.enqueue now will result in:
// "Cannot enqueue a chunk into a closed readable stream" error.
Thanks for reading! Leave a comment with what you think, or if there is anything I missed ❤.

Wow, you made it to the end! Thanks for sticking with me through my musings on gun. If you enjoyed reading this, I’d be thrilled if you hit that follow button — it really means a lot. Cheers to you!