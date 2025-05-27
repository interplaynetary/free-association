# Ideas

## AI

- conversation to value-graph. (Maybe using squiggly) (we now use JSON, this should be way easier)

## Map

- Make markers display the location of capacities, and color the tags according to the depth at which one received access.

## Narrative: Charecters

- People can create charecters that they can play. (accounts) This makes it feel more approachable.

Add filters to inventory UI

Within protocol:
✅ How do we do the calculation chain?
✅ When to recalculate and persist

When change in tree:
✅ persist Tree
✅ recalculate and persist SOGF
✅ then recalculate and persist provider-Shares
✅ then recalculate and persist recipient-Shares

When change in capacities:
✅ Recalculate and persist

✅ // Our capacities should have recipientShares: (with filters applied), these should be stored on the capacity object itself.

// What kind of reactivity is needed for filter conditions?
// When output of filter-conditions changes, we should update a capacities recipientShares

// then people would subscribe to their changes in the recipientShares of a capacity?

but how does the other know when a capacity has changed efficiently, without being notified for all changes to capcities that dont involve us? if we mantain:
user.get(recipients).get(recipientPubKey).put(JSON.stringify(structuredClone(getReceiverShares(recipientPubKey))))

Okay, amazing. So now, what I've found is that mutual fulfillment is reactively changing whenever we add contributors or make changes to how much those contributors have of our general fulfillment. However, in the specific case where we remove a contributor completely from our recognition, this does not actually reactively update the mutual fulfillment bars of others.

Here for example, Charlie is removing gnoman completely as a contributor. And here is the corresponding output that we see in gnomans console.

we immediately receive an update, but i wonder if it contains the corrct data or whether we gnoman's reactive flow to the bar takes it into account, because now no mutual-recognition should be possible:
[NETWORK] Received SOGF update from tW6vbQ1z6XHV4vrf-ocORDAH0FB16p-bwYP3RQUMqJ8.H18lVG2p29xubDI6MoPEXlYridAFJNsDCFKER3KXVzc
state.svelte.ts:1004 [NETWORK] Received share from tW6vbQ1z6XHV4vrf-ocORDAH0FB16p-bwYP3RQUMqJ8.H18lVG2p29xubDI6MoPEXlYridAFJNsDCFKER3KXVzc: 0.6613
state.svelte.ts:1010 [NETWORK] Existing cache entry for tW6vbQ1z6XHV4vrf-ocORDAH0FB16p-bwYP3RQUMqJ8.H18lVG2p29xubDI6MoPEXlYridAFJNsDCFKER3KXVzc: {ourShare: 0.5, theirShare: 0.6613044105752401, timestamp: 1748186883592}
state.svelte.ts:1016 [NETWORK] Updating existing entry for tW6vbQ1z6XHV4vrf-ocORDAH0FB16p-bwYP3RQUMqJ8.H18lVG2p29xubDI6MoPEXlYridAFJNsDCFKER3KXVzc
state.svelte.ts:1032 [NETWORK] Updated cache entry for tW6vbQ1z6XHV4vrf-ocORDAH0FB16p-bwYP3RQUMqJ8.H18lVG2p29xubDI6MoPEXlYridAFJNsDCFKER3KXVzc: {ourShare: 0.5, theirShare: 0.6613044105752401, timestamp: 1748186973605}
state.svelte.ts:45 [MUTUAL-RECOGNITION] 2025-05-25T15:29:33.606Z Recalculating from cache: {lOxe8kXx4v7IbDTGcP_J7MPICC3_diGmbcERRoDjPIk.hbN00piw9Y24ky0EtsnSyhMtHfteZdTJs3WXwJZCI4A: {…}, tW6vbQ1z6XHV4vrf-ocORDAH0FB16p-bwYP3RQUMqJ8.H18lVG2p29xubDI6MoPEXlYridAFJNsDCFKER3KXVzc: {…}}
state.svelte.ts:57 [MUTUAL-RECOGNITION] lOxe8kXx4v7IbDTGcP_J7MPICC3_diGmbcERRoDjPIk.hbN00piw9Y24ky0EtsnSyhMtHfteZdTJs3WXwJZCI4A: our=0.5000, their=0.5010, mutual=0.5000
state.svelte.ts:57 [MUTUAL-RECOGNITION] tW6vbQ1z6XHV4vrf-ocORDAH0FB16p-bwYP3RQUMqJ8.H18lVG2p29xubDI6MoPEXlYridAFJNsDCFKER3KXVzc: our=0.5000, their=0.6613, mutual=0.5000
state.svelte.ts:62 [MUTUAL-RECOGNITION] Final mutual values: {lOxe8kXx4v7IbDTGcP_J7MPICC3_diGmbcERRoDjPIk.hbN00piw9Y24ky0EtsnSyhMtHfteZdTJs3WXwJZCI4A: 0.5, tW6vbQ1z6XHV4vrf-ocORDAH0FB16p-bwYP3RQUMqJ8.H18lVG2p29xubDI6MoPEXlYridAFJNsDCFKER3KXVzc: 0.5}
state.svelte.ts:68 [PROVIDER-SHARES] 2025-05-25T15:29:33.607Z Recalculating from mutual recognition: {lOxe8kXx4v7IbDTGcP_J7MPICC3_diGmbcERRoDjPIk.hbN00piw9Y24ky0EtsnSyhMtHfteZdTJs3WXwJZCI4A: 0.5, tW6vbQ1z6XHV4vrf-ocORDAH0FB16p-bwYP3RQUMqJ8.H18lVG2p29xubDI6MoPEXlYridAFJNsDCFKER3KXVzc: 0.5}
state.svelte.ts:81 [PROVIDER-SHARES] Normalized shares: {lOxe8kXx4v7IbDTGcP_J7MPICC3_diGmbcERRoDjPIk.hbN00piw9Y24ky0EtsnSyhMtHfteZdTJs3WXwJZCI4A: 0.5, tW6vbQ1z6XHV4vrf-ocORDAH0FB16p-bwYP3RQUMqJ8.H18lVG2p29xubDI6MoPEXlYridAFJNsDCFKER3KXVzc: 0.5}
+page.svelte:29 [UI] providerShares changed: {lOxe8kXx4v7IbDTGcP_J7MPICC3_diGmbcERRoDjPIk.hbN00piw9Y24ky0EtsnSyhMtHfteZdTJs3WXwJZCI4A: 0.5, tW6vbQ1z6XHV4vrf-ocORDAH0FB16p-bwYP3RQUMqJ8.H18lVG2p29xubDI6MoPEXlYridAFJNsDCFKER3KXVzc: 0.5}
+page.svelte:45 [UI] Generated provider segments: (2) [{…}, {…}]
state.svelte.ts:977 [PERSIST] Saving recognition cache with 2 entries
state.svelte.ts:1044 [NETWORK] Cache after network update from tW6vbQ1z6XHV4vrf-ocORDAH0FB16p-bwYP3RQUMqJ8.H18lVG2p29xubDI6MoPEXlYridAFJNsDCFKER3KXVzc: {lOxe8kXx4v7IbDTGcP_J7MPICC3_diGmbcERRoDjPIk.hbN00piw9Y24ky0EtsnSyhMtHfteZdTJs3WXwJZCI4A: {…}, tW6vbQ1z6XHV4vrf-ocORDAH0FB16p-bwYP3RQUMqJ8.H18lVG2p29xubDI6MoPEXlYridAFJNsDCFKER3KXVzc: {…}}
state.svelte.ts:1045 [NETWORK] Mutual recognition after update: {lOxe8kXx4v7IbDTGcP_J7MPICC3_diGmbcERRoDjPIk.hbN00piw9Y24ky0EtsnSyhMtHfteZdTJs3WXwJZCI4A: 0.5, tW6vbQ1z6XHV4vrf-ocORDAH0FB16p-bwYP3RQUMqJ8.H18lVG2p29xubDI6MoPEXlYridAFJNsDCFKER3KXVzc: 0.5}
state.svelte.ts:1046 [NETWORK] Provider shares after update: {lOxe8kXx4v7IbDTGcP_J7MPICC3_diGmbcERRoDjPIk.hbN00piw9Y24ky0EtsnSyhMtHfteZdTJs3WXwJZCI4A: 0.5, tW6vbQ1z6XHV4vrf-ocORDAH0FB16p-bwYP3RQUMqJ8.H18lVG2p29xubDI6MoPEXlYridAFJNsDCFKER3KXVzc: 0.5}

I can see the issue! The problem is that when Charlie removes gnoman as a contributor, gnoman receives the updated SOGF data from Charlie (showing `theirShare: 0.6613`), but gnoman's own `ourShare` for Charlie remains at `0.5` because gnoman hasn't recalculated their own tree yet.

The key issue is in this line from the logs:

```
our=0.5000, their=0.6613, mutual=0.5000
```

Gnoman still thinks they're giving Charlie a 50% share (`ourShare: 0.5`), but Charlie has actually removed gnoman completely. The mutual recognition should be 0 because Charlie's new SOGF should show 0% for gnoman.

Let me analyze what's happening:

Search files...
I can see the issue now! The problem is that when Charlie removes gnoman as a contributor, Charlie's SOGF gets recalculated and Charlie's share for gnoman becomes 0. However, the network update that gnoman receives shows `theirShare: 0.6613`, which suggests that Charlie is still giving gnoman a 66% share.

This indicates that either:

1. The SOGF data being sent over the network is stale/incorrect, or
2. There's a timing issue where the old SOGF is being sent before the new calculation completes

Let me examine how the SOGF gets persisted and sent over the network:

Read file: src/lib/state.svelte.ts
Now I can see the issue! The problem is in the **timing and order of operations**. Let me trace what happens when Charlie removes gnoman:

1. **Charlie removes gnoman from their tree**
2. **Charlie's tree recalculation starts** (`recalculateFromTree()`)
3. **Charlie's SOGF gets recalculated** (gnoman should now get 0%)
4. **Charlie's SOGF gets persisted** (`persistSogf()`)
5. **Gun network propagates the update to gnoman**
6. **Gnoman receives the update** but it shows `theirShare: 0.6613`

The issue is that **gnoman is receiving stale SOGF data**. Looking at the logs, gnoman receives `theirShare: 0.6613`, but if Charlie truly removed gnoman completely, Charlie's new SOGF should show 0% for gnoman.

I believe the issue is that gun is merging the objects, but we need to explictly mark deleted contributors as zero instead of just not including them in our maps, our sharesOfGeneralFulfillment is using the contributors writeable which likely does not have the removed contributor, i believe this is the source of the error

I think we need to mantain a writeable of all contributors that have been added, so we can use this for SOGF instead?


------------------
make the MultiLevel Dropdown visualize the tree structure simply and elegantly, similar to how a tree of folders is displayed in consoless

lets adapt 