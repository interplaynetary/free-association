# TODO

- Make reactiveGraph / reactiveComponent etc. use isTransient flags from their imports

- make relay use radix (IndexedDB)

- we can totally have a better way of doing userUtils.ts

- Our dropdown loads all users, but as a result it quickly fills up all our space

- IndexedDB (Radix)

- New nodes are only being added at the root level

A couple issues, zoomin and out shoujld not be possible during deleteMode, and the nodes are not actually deleting, are we properly interacting with rec.ts? Or perhaps we are somehow not properly disabling other event listeners during deleteMode and it is interfering

---

How do we determine what is causing this message:
Warning: You're syncing 1K+ records a second, faster than DOM can update - consider limiting query.

- maybe remove transientGun

- lets create a branch that doesnt even use ReactiveGraph?
- and uses the svelte gun approach outlined in gunSetup

- we need to ensure that when we delete a parent node, we .put() all of its children recursively, otherwise we will flood our network

Aesthetic Fixes:

- Names of contributors not showing up
- rootName only displays after clicking/navigating to it.
