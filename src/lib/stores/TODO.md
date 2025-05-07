- we don't need to replicate a cache system (for anything except points), Because doing so would create a conflict and necessity of creating a conflict resolution strategy between two sources of truth. Modifications to the cache and modifications to the network.

We obviously don't want that because GUN already has a conflict resolution system and we would be actively fighting against it.

We want to make it so that our derived stores reactively push back into the network whenever the value changes.

When we are not logged in we only want to see public,
When we are logged in we only want to see private, to avoid mixing these.

For some reason adding/removing/renaming nodes/children to public works
but removing nodes or adding(/removing?)/renaming children in user-space does not work properly.
- new nodes only appear when we reload, and dont appear immediately as they do in the public. (they also only appear at all in at the root level - no children appear at al)

When we try to create a child it seems to work in console but not in the interface, the interface shows no children:
[snapshot] selfPoints 
{subscribe: ƒ}
Parent.svelte:31 selfPoints 
Proxy(Object) {subscribe: ƒ}
Parent.svelte:32 childNodes 
[]
Header.svelte:13 pathToRoot 
(2) [{…}, {…}]
0
: 
id
: 
"1746626634893"
name
: 
"hihih"
path
: 
Array(3)
0
: 
"recognition"
1
: 
"children"
2
: 
"1746626634893"
length
: 
3
[[Prototype]]
: 
Array(0)
store
: 
{nameStore: {…}, pointsStore: {…}, parentStore: {…}, childrenStore: {…}, contributorsStore: {…}, …}
[[Prototype]]
: 
Object
1
: 
{path: Array(1), store: {…}, name: '', id: 'recognition'}
length
: 
2
[[Prototype]]
: 
Array(0)
Parent.svelte:179 Adding child node using store with path: recognition/children/1746626634893
Parent.svelte:183 Child node created with ID: 1746626847279
gun.js:542 
{@: '0mDmxFyVK', err: {…}, $: Gun2, #: 'fF8QoCJG4', _: ƒ}
#
: 
"fF8QoCJG4"
$
: 
Gun2 {_: {…}}
@
: 
"0mDmxFyVK"
err
: 
{code: 'ENAMETOOLONG', errno: -36, path: 'radata/~McUmKdO7NtwX1vUrZiNvzgnPVPwgqKGV1PNATxIxTu…1_NSw.J5NEzpx7-FpCtrXYCQ3pzjAuPmVdi7TAmS7MncSXXLY', syscall: 'open'}
out
: 
ƒ universe(msg)
_
: 
ƒ ()
[[Prototype]]
: 
Obj