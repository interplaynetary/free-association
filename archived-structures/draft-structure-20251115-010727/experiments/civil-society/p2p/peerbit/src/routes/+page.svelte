<script lang="ts">
    import { writable } from "svelte/store";
    import { onMount } from "svelte";

    import { createClient, ExampleStore, SimpleDocument } from "../db";

    export const stringList = writable<string[]>([]);

    onMount(async () => {
        const client = await createClient();
        const store = await client.open(new ExampleStore());

        // listen for changes and update messages on the screen
        store.documents.events.addEventListener("change", (evt) => {
            evt.detail.added.forEach((doc) => {
                stringList.update((list) => {
                    return [...list, doc.content];
                });
            });
        });

        // insert some data
        await store.documents.put(
            new SimpleDocument({
                content: "Hello from " + client.libp2p.peerId.toString(),
            }),
        );
    });
</script>

<h1>Peerbit Svelte Example</h1>
<ul>
    {#each $stringList as str}
        <li>{str}</li>
    {/each}
</ul>
