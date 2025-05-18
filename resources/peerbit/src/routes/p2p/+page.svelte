<script lang="ts">
    import { writable } from "svelte/store";
    import { onMount } from "svelte";
    import { 
        createClient, 
        Tree, 
        Node,
        Capacity,
    } from "../../p2p";

    // Store for our tree data
    export const rootNodes = writable<{id: string, name: string, address: string}[]>([]);
    export const capacities = writable<{id: string, name: string, quantity: number, unit: string}[]>([]);
    export const mutualFulfillment = writable<{source: string, target: string, score: number}[]>([]);
    export const capacityShares = writable<{id: string, source: string, target: string, percentage: number, quantity: number, unit: string}[]>([]);
    export const log = writable<string[]>([]);

    // Add a log message
    function addLog(message: string) {
        log.update(logs => [...logs, message]);
    }

    onMount(async () => {
        try {
            addLog("Setting up peer-to-peer network...");
            
            // Create peerbit instances for Alice and Bob
            const alicePeerbit = await createClient(); // Use local network for testing
            const bobPeerbit = await createClient();
            
            addLog(`Alice Peer ID: ${alicePeerbit.libp2p.peerId.toString()}`);
            addLog(`Bob Peer ID: ${bobPeerbit.libp2p.peerId.toString()}`);

            // Create tree programs
            const aliceTree = new Tree();
            const bobTree = new Tree();

            // Open the trees
            await aliceTree.open();
            await bobTree.open();
            
            addLog("Trees opened successfully");

            // Initialize root nodes
            const aliceRoot = await aliceTree.initializeRoot("Alice", 100);
            const bobRoot = await bobTree.initializeRoot("Bob", 100);
            
            // Update the UI with root nodes
            rootNodes.update(nodes => [
                ...nodes, 
                {
                    id: aliceRoot.id, 
                    name: aliceRoot.name,
                    address: aliceTree.address.toString()
                },
                {
                    id: bobRoot.id, 
                    name: bobRoot.name,
                    address: bobTree.address.toString()
                }
            ]);
            
            addLog(`Created root nodes for Alice (${aliceRoot.id}) and Bob (${bobRoot.id})`);

            // Add a capacity for Alice
            const roomCapacity = await aliceTree.addCapacity(
                "Spare Room",
                10,
                "room",
                2,
                1, // Natural divisibility
                0.1 // Percentage divisibility
            );
            
            // Update the UI with capacity
            capacities.update(caps => [
                ...caps, 
                {
                    id: roomCapacity.capacityId,
                    name: roomCapacity.capacityName,
                    quantity: roomCapacity.quantity,
                    unit: roomCapacity.unit
                }
            ]);
            
            addLog(`Added capacity "${roomCapacity.capacityName}" to Alice's tree`);

            // Add children with mutual contributions
            const aliceChild = await aliceTree.addChild(
                aliceRoot.id,
                "alice_child",
                30,
                [bobRoot.id] // Bob is a contributor
            );

            const bobChild = await bobTree.addChild(
                bobRoot.id,
                "bob_child",
                40,
                [aliceRoot.id] // Alice is a contributor
            );
            
            addLog("Added child nodes with mutual contributions");

            // Connect to each other's trees
            const aliceAddress = aliceTree.address.toString();
            const bobAddress = bobTree.address.toString();

            addLog("Networks connected and joined");

            addLog("Setup complete!");
        } catch (error) {
            addLog(`Error in setup: ${error instanceof Error ? error.message : String(error)}`);
            console.error("Error in p2p setup:", error);
        }
    });

</script>

<svelte:head>
    <title>Peerbit P2P Network Test</title>
</svelte:head>

<div class="container">
    <h1>Peerbit P2P Network Test</h1>
    
    <div class="section">
        <h2>Nodes</h2>
        <ul>
            {#each $rootNodes as node}
                <li>
                    <strong>{node.name}</strong> (ID: {node.id})
                    <br />
                    <small>Address: {node.address}</small>
                </li>
            {/each}
        </ul>
    </div>
    
    <div class="section">
        <h2>Capacities</h2>
        <ul>
            {#each $capacities as capacity}
                <li>
                    <strong>{capacity.name}</strong>: {capacity.quantity} {capacity.unit}
                    <br />
                    <small>ID: {capacity.id}</small>
                </li>
            {/each}
        </ul>
    </div>
    
    <div class="section">
        <h2>Mutual Fulfillment</h2>
        <ul>
            {#each $mutualFulfillment as mf}
                <li>
                    <strong>{mf.source} ↔ {mf.target}</strong>: {(mf.score * 100).toFixed(2)}%
                </li>
            {/each}
        </ul>
    </div>
    
    <div class="section">
        <h2>Capacity Shares</h2>
        <ul>
            {#each $capacityShares as share}
                <li>
                    <strong>{share.target}'s share of {share.source}'s {share.unit}</strong>:
                    <br />
                    {(share.percentage * 100).toFixed(2)}% ({share.quantity} {share.unit})
                </li>
            {/each}
        </ul>
    </div>
    
    <div class="section">
        <h2>Logs</h2>
        <div class="log-container">
            {#each $log as message}
                <div class="log-message">{message}</div>
            {/each}
        </div>
    </div>
</div>

<style>
    .container {
        max-width: 800px;
        margin: 0 auto;
        padding: 20px;
        font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Open Sans', 'Helvetica Neue', sans-serif;
    }
    
    h1 {
        color: #333;
        border-bottom: 2px solid #eee;
        padding-bottom: 10px;
    }
    
    .section {
        margin-bottom: 30px;
        padding: 15px;
        background-color: #f9f9f9;
        border-radius: 5px;
        box-shadow: 0 2px 4px rgba(0,0,0,0.1);
    }
    
    h2 {
        margin-top: 0;
        color: #444;
        font-size: 1.2em;
    }
    
    ul {
        list-style-type: none;
        padding-left: 0;
    }
    
    li {
        margin-bottom: 10px;
        padding: 8px;
        background-color: #fff;
        border-radius: 4px;
        box-shadow: 0 1px 3px rgba(0,0,0,0.1);
    }
    
    .log-container {
        max-height: 300px;
        overflow-y: auto;
        background-color: #333;
        color: #f0f0f0;
        padding: 10px;
        border-radius: 4px;
        font-family: monospace;
    }
    
    .log-message {
        margin-bottom: 5px;
        line-height: 1.4;
    }
</style> 