We face an issue that for some reason if i recognize a contact:
- It doesnt show up in my shares of general recognition! Even though it should (even when there is no associated pub key)
- When i add a pubkey to a contact that did not have one prior, it does not (immediately allow us to have mutual-recognition)
    - To be seen whether it works upon reload

We can see in users.svelte.ts that we do publicKeyResoltuion across so many parts of our code including the network streaming layer, calculations.svelte etc.

While this is not elegant, it seems that the issue might actually be from elsewhere, related more to tracing what happens after a pubkey is added to a contact that has already been created!


As for why we dont immediately see the new users that have logged in in the UsersList

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