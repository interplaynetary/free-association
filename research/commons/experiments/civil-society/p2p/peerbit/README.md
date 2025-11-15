# Peerbit Svelte example 

A simple example where each node that starts will send a "Hello" message. Open the app url in multiple tabs to see the messages being sent.

Peerbit related code is found in [`src/db.ts`](https://github.com/dao-xyz/peerbit-examples/blob/master/packages/svelte-example/src/db.ts) 

[`createClient`](https://github.com/dao-xyz/peerbit-examples/blob/93bb2848f118c7ecb69ee7b7ab3e980ca542ad05/packages/svelte-example/src/db.ts#L43) method takes a boolean argument which lets you control whether you want to use a local relay server or not. To launch a local server see https://peerbit.org/#/modules/deploy/server/?id=testing-locally.




## Setup
Run 

```bash 
yarn 
```

in the root of the peerbit-examples repo 


## Developing


Inside this project run

```bash
npm run dev

# or start the server and open the app in a new browser tab
npm run dev -- --open
```

to launch dev server

