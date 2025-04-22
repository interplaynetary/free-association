Ok amazing! Its working! Now the only thing is that when we initially load the page only 1 contributor pill shows up for any given node, but once we zoom in and out from that node, we see immediatelly ALL of the contributor pills! Why is that? Anaylze in depth


zooming into a node then disrupts that node's reactivity:

After zooming in:
Node.svelte:249 Updated name for node 1745334803608 to "Beau"
GunSubscription.ts:118 Error in Gun subscription handler: ReferenceError: Cannot access 'unsub' before initialization
at Array.<anonymous> (rec.ts:547:9)
at set2 (chunk-4ZSOSDTO.js?v=a24f06da:3017:35)
at reactiveStores.ts:77:7
at GunSubscription.ts:116:15
at Set.forEach (<anonymous>)
at Gun2.<anonymous> (GunSubscription.ts:114:25)
at any2 (chunk-Q5L3LZAW.js?v=a24f06da:1151:24)
at chunk-Q5L3LZAW.js?v=a24f06da:1137:21
at chunk-Q5L3LZAW.js?v=a24f06da:576:21
at t (chunk-Q5L3LZAW.js?v=a24f06da:155:34)


Make growth and shrinking visual while it is happening



improve the proportionality of buttons, and make it div based the area underneath the title is area for tags etc. and this is where contributors can be added, scale the button and tags to fit this space.



Ok great! Now for some reason though, when trying to create a node with no children, there is a % of the treemap container thatr can not be filled by nodes, our nodes dont occupz 100% of the space, there is empty space occupying space.



Contributions can't have children and you cant zoom in on them





---

How do we determine what is causing this message:
Warning: You're syncing 1K+ records a second, faster than DOM can update - consider limiting query.
