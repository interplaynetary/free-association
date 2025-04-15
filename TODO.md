# TODO

- Make reactiveGraph / reactiveComponent etc. use isTransient flags from their imports

- make relay use radix (IndexedDB)

- we can totally have a better way of doing userUtils.ts

- Our dropdown loads all users, but as a result it quickly fills up all our space

- IndexedDB (Radix)

- New nodes are only being added at the root level

A couple issues, zoomin and out shoujld not be possible during deleteMode, and the nodes are not actually deleting, are we properly interacting with rec.ts? Or perhaps we are somehow not properly disabling other event listeners during deleteMode and it is interfering

Trash can becomes invisible on non-root nodes, unlike the other two peers and + add node button

However children are only being added at the root level even if our currentView is not the root! We want to add Chidren to the level we are currently at

Let's make sure that we aerent seeing the hover effect over the text(that we would see for editing text in general) with regards to text that appears in the navigation bar ( I would like the text in the nav bar to be from an interaction persepctive, indigstinguishable from the nav bar itself)



- Spawning a Web Worker:
Can I use GUN inside a Web Worker #
Yes! You just need to fake the window object and then load all the scripts with importScipts(). Web Workers don't have access to localStorage, so you'll have to use indexedDB RAD adapter to be able to persist data in the browser IndexedDB. So you can have a separate thread to handle GUN requests. But mind that a worker will have to load and execute all those scripts at the start and that may create a delay before the first message being processed.

const window = {
  crypto: self.crypto,
  TextEncoder: self.TextEncoder,
  TextDecoder: self.TextDecoder,
  WebSocket: self.WebSocket,
}

importScripts('../js/gun.min.js')
importScripts('../js/sea.js')
importScripts('../js/radix.js')
importScripts('../js/radisk.js')
importScripts('../js/store.js')
importScripts('../js/rindexed.js')

const gun = new window.Gun({ localStorage: false })

onmessage = (e) => {
  gun
    .get(e.data)
    .map()
    .on((d, k) => {
      postMessage({ k, d })
    })
}


///////// Contributor Tags

I added a contributor to Wonder Node  this was the output, what does it tell you, our tags are still not displaying, we should update Recognition.svelte and TreeMap.svelte so that contributors are processed just like how we do children, making a more elegant design:

[ReactiveGraph] Adding node to collection {path: Array(2), id: 'u124', nodeData: {…}}id: "u124"nodeData: {value: true}path: (2) ['recognition', 'contributors'][[Prototype]]: Object
reactiveStores.ts:203 [DEBUG] Subscriber left collection users. Remaining subscribers: 1
reactiveStores.ts:203 [DEBUG] Subscriber left collection users. Remaining subscribers: 0
reactiveStores.ts:207 [DEBUG] No more subscribers, cleaning up Gun subscription for: users
chunk-Q5L3LZAW.js?v=1ca4b971:736 {@: 'hTcbqoHSN', err: 'localStorage max!', $: Gun2, #: '2QnKEvi6I', _: ƒ}
reactiveStores.ts:130 [DEBUG] Collection received item update: {path: 'recognition/contributors', itemKey: 'u124', item: {…}}item: {_: {…}, value: true, _key: 'u124'}itemKey: "u124"path: "recognition/contributors"[[Prototype]]: Object
reactiveStores.ts:158 [DEBUG] Updating existing item: u124
reactiveStores.ts:171 [DEBUG] Updated collection state: {path: 'recognition/contributors', itemCount: 3}itemCount: 3path: "recognition/contributors"[[Prototype]]: Object
reactiveStores.ts:130 [DEBUG] Collection received item update: {path: 'recognition/contributors', itemKey: 'u124', item: {…}}


We need to modify our approach to store contributor data directly in the hierarchical data structure
This prevents the need to create separate stores for each node