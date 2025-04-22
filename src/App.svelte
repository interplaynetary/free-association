<script lang="ts">
  import { onMount, onDestroy } from "svelte";
  import { createRec, type RecognitionStore } from "./stores/rec";
  import Node from "./lib/Node.svelte";

  // Path to the recognition data
  let path: string[] = ["recognition"];

  // Store and component references
  let recStore: RecognitionStore;
  let treeMapComponent: any;

  // State
  let loading = true;
  let error: string | null = null;
  let viewportWidth = window.innerWidth;
  let viewportHeight = window.innerHeight;

  // Initialize on mount
  onMount(async () => {
    try {
      loading = true;

      // Create the recognition store directly
      recStore = createRec(path);

      loading = false;
    } catch (err) {
      console.error("Error initializing TreeMap:", err);
      error = err instanceof Error ? err.message : "Unknown error";
      loading = false;
    }
  });

  onDestroy(() => {
    // Clean up TreeMap component
    if (treeMapComponent && typeof treeMapComponent.destroy === "function") {
      treeMapComponent.destroy();
    }
  });

  // Handle window resize
  function handleResize() {
    viewportWidth = window.innerWidth;
    viewportHeight = window.innerHeight;
  }
</script>

<svelte:window on:resize={handleResize} />

<main>
  <div class="container">
    {#if loading}
      <div class="loading">Loading recognition data...</div>
    {:else if error}
      <div class="error">{error}</div>
    {:else if recStore}
      <Node store={recStore} />
    {:else}
      <div class="empty">No recognition data available</div>
    {/if}
  </div>
</main>

<style>
  :global(*) {
    margin: 0;
    padding: 0;
    box-sizing: border-box;
  }

  :global(html, body) {
    margin: 0;
    padding: 0;
    overflow: hidden;
    width: 100vw;
    height: 100vh;
    background-color: #ffffff;
  }

  :global(#app) {
    width: 100vw;
    height: 100vh;
    position: absolute;
    top: 0;
    left: 0;
    margin: 0;
    padding: 0;
    background-color: #888888;
  }

  main {
    width: 100vw;
    height: 100vh;
    margin: 0;
    padding: 0;
    overflow: hidden;
    display: block;
    position: absolute;
    top: 0;
    left: 0;
    background-color: #ffffff;
  }

  .container {
    display: flex;
    flex-direction: column;
    width: 100%;
    height: 100%;
    margin: 0;
    padding: 0;
  }

  .loading,
  .error,
  .empty {
    display: flex;
    align-items: center;
    justify-content: center;
    height: 100%;
    width: 100%;
    font-size: 1.2rem;
    color: #ffffff;
  }

  .error {
    color: #d32f2f;
    padding: 1rem;
  }
</style>
