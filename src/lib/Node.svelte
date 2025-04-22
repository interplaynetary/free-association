<script lang="ts">
  import { onMount, onDestroy } from "svelte";
  import {
    type RecognitionStore,
    type RecNode,
    createRec,
  } from "../stores/rec";
  import { NodeStoreHandler } from "./store.svelte";
  import Header from "./Header.svelte";
  import Child from "./Child.svelte";
  import * as d3 from "d3";

  let store: RecognitionStore = createRec();
  let nodeStore: NodeStoreHandler = new NodeStoreHandler(store);

  // Initialize the node store handler with the provided store
  onMount(() => {
    nodeStore.subscribeToStoreData();
  });

  onDestroy(() => {
    // Clean up subscriptions when component is destroyed
    if (nodeStore) {
      nodeStore.destroy();
    }
  });

  const children = $derived.by((): RecNode[] => {
    return nodeStore.hierarchyData?.children ?? [];
  });

  const hierarchyData = $derived.by(() => {
    const data = nodeStore.hierarchyData;
    if (!data) return null;

    // Create hierarchy
    const hierarchy = d3.hierarchy<RecNode>(data, (d) => d?.children ?? []);

    // Sum for sizing
    hierarchy.sum((d) => d?.points ?? 0);

    // Sort by value (optional)
    hierarchy.sort((a, b) => (b?.data?.points ?? 0) - (a?.data?.points ?? 0));

    // Apply treemap with relative sizing (0-1 range)
    const treemap = d3
      .treemap<RecNode>()
      .tile(d3.treemapSquarify)
      .size([1, 1]) // Use 0-1 range for relative sizing
      .padding(0.005) // Relative padding
      .round(false); // Don't round to pixels

    return treemap(hierarchy);
  });

  function handleHeaderClick() {
    console.log("Header clicked");
    // Handle navigation or other actions
  }

  function handleChildClick(childNode: RecNode) {
    console.log("Child clicked:", childNode);
    // Navigate to child or perform other actions
  }
</script>

{#if hierarchyData}
  <div class="node-container">
    <!-- Header -->
    <div class="node-header" on:click={handleHeaderClick}>
      <Header node={hierarchyData} />
    </div>

    <!-- Content area with children -->
    <div class="node-content">
      <svg viewBox="0 0 1 1" preserveAspectRatio="none" class="treemap-svg">
        {#each hierarchyData.children || [] as child (child.data?._key)}
          <g on:click={() => handleChildClick(child)} class="clickable">
            <Child {child} />
          </g>
        {/each}
      </svg>
    </div>
  </div>
{:else}
  <div class="loading">Loading hierarchy data...</div>
{/if}

<style>
  .node-container {
    width: 100%;
    height: 100%;
    display: flex;
    flex-direction: column;
  }

  .node-header {
    width: 100%;
    height: 50px;
    flex-shrink: 0;
  }

  .node-content {
    flex: 1;
    overflow: auto;
    position: relative;
  }

  .treemap-svg {
    width: 100%;
    height: 100%;
    position: absolute;
    top: 0;
    left: 0;
  }

  .loading {
    display: flex;
    align-items: center;
    justify-content: center;
    height: 100%;
    width: 100%;
    color: #888;
  }

  :global(.clickable) {
    cursor: pointer;
  }
</style>
