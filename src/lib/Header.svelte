<script lang="ts">
  import { type RecNode, type RecognitionStore } from "../stores/rec";
  import * as d3 from "d3";
  import { onMount, createEventDispatcher } from "svelte";

  // Event dispatcher for path navigation
  const dispatch = createEventDispatcher<{
    navigateToPath: number;
  }>();

  // Use $props() for Svelte 5 runes mode
  let {
    node = undefined,
    deleteMode = false,
    onAddClick = () => {},
    onDeleteToggle = () => {},
    onZoomOut = () => {},
    store = undefined,
  } = $props<{
    node?: d3.HierarchyRectangularNode<RecNode>;
    deleteMode?: boolean;
    onAddClick?: () => void;
    onDeleteToggle?: () => void;
    onZoomOut?: () => void;
    store?: RecognitionStore;
  }>();

  // Path names state
  let pathNames = $state<string[]>([]);
  let isLoadingPath = $state(false);

  // Determine if this is the root node
  const isRoot = $derived(node && !node.parent);
  const nodeName = $derived(node?.data?.name || "Unnamed");
  const textSegments = $derived(nodeName.split(/(?=[A-Z][^A-Z])/g));

  // Create effect to reload path when store changes
  $effect(() => {
    if (store) {
      loadPathNames();
    }
  });

  // Load the path names from the store
  async function loadPathNames() {
    if (!store) return;

    isLoadingPath = true;

    try {
      // Get the path from the current node to the root
      const pathToRoot = await store.getPathToRoot();

      // Extract names in reverse order (from root to current node)
      const names = pathToRoot
        .map((item: { name: string; id: string }) => item.name || item.id)
        .reverse()
        .filter(Boolean); // Filter out empty names

      pathNames = names;
    } catch (err) {
      console.error("Error resolving path names:", err);
      // Fallback to just showing the node name
      pathNames = [nodeName];
    } finally {
      isLoadingPath = false;
    }
  }

  // Handle button clicks with event stopping
  function handleAddClick(e: MouseEvent) {
    e.stopPropagation();
    onAddClick();
  }

  function handleDeleteToggle(e: MouseEvent) {
    e.stopPropagation();
    onDeleteToggle();
  }

  function handleInventoryClick(e: MouseEvent) {
    e.stopPropagation();
    console.log("Inventory clicked");
  }

  function handlePeerClick(e: MouseEvent) {
    e.stopPropagation();
    console.log("Peer search clicked");
  }

  function handleHomeClick(e: MouseEvent) {
    e.stopPropagation();
    onZoomOut();
  }

  // Handle breadcrumb navigation - dispatch event to parent
  function handlePathClick(e: MouseEvent, index: number) {
    e.stopPropagation();
    console.log(`Navigate to path index: ${index}`);
    dispatch("navigateToPath", index);
  }
</script>

<div
  class="treemap-header clickable"
  class:delete-mode={deleteMode}
  style:border={node?.data?.hasDirectContributionChild
    ? "3px solid #2196f3"
    : "2px solid #fff"}
>
  <div class="header-content">
    <!-- Show breadcrumb path for root node, otherwise show node name -->
    <div class="node-name">
      {#if pathNames.length > 0}
        <div class="breadcrumbs">
          {#if isLoadingPath}
            <div class="loading-path">Loading path...</div>
          {:else}
            {#each pathNames as pathName, i}
              <div
                class="breadcrumb-item"
                class:current={i === pathNames.length - 1}
                onclick={(e) => handlePathClick(e, i)}
              >
                {pathName}
              </div>
              {#if i < pathNames.length - 1}
                <div class="breadcrumb-separator">/</div>
              {/if}
            {/each}
          {/if}
        </div>
      {:else}
        {#each textSegments as segment, i}
          <div class="text-segment">{segment}</div>
        {/each}
      {/if}
    </div>

    <!-- Control buttons for root node -->
    {#if isRoot}
      <div class="header-controls">
        <!-- Inventory button -->
        <button
          class="icon-button inventory-button"
          onclick={handleInventoryClick}
        >
          <span>🎒</span>
        </button>

        <!-- Peer button -->
        <button class="icon-button peer-button" onclick={handlePeerClick}>
          <span>🔍</span>
        </button>

        <!-- Add button -->
        <button class="icon-button add-button" onclick={handleAddClick}>
          <span>➕</span>
        </button>

        <!-- Delete button -->
        <button
          class="icon-button delete-button"
          class:active={deleteMode}
          onclick={handleDeleteToggle}
        >
          <span>🗑️</span>
        </button>
      </div>
    {:else}
      <!-- Home button for non-root nodes -->
      <button class="icon-button home-button" onclick={handleHomeClick}>
        <span>🏠</span>
      </button>
    {/if}
  </div>
</div>

<style>
  .treemap-header {
    width: 100%;
    height: 50px;
    display: flex;
    align-items: center;
    justify-content: space-between;
    padding: 0 15px;
    box-sizing: border-box;
    transition: all 0.2s ease;
    overflow: hidden;
  }

  .treemap-header.delete-mode {
    opacity: 0.95;
  }

  .header-content {
    width: 100%;
    display: flex;
    justify-content: space-between;
    align-items: center;
  }

  .node-name {
    display: flex;
    align-items: center;
    font-weight: bold;
    user-select: none;
    overflow: hidden;
    max-width: calc(100% - 180px);
  }

  .text-segment {
    line-height: 1.2em;
    margin-right: 0.3em;
  }

  /* Breadcrumb styles */
  .breadcrumbs {
    display: flex;
    align-items: center;
    flex-wrap: nowrap;
    overflow-x: auto;
    scrollbar-width: none; /* Firefox */
    -ms-overflow-style: none; /* IE and Edge */
    max-width: 100%;
    padding-bottom: 4px;
  }

  .breadcrumbs::-webkit-scrollbar {
    display: none; /* Chrome, Safari, Opera */
  }

  .breadcrumb-item {
    white-space: nowrap;
    padding: 3px 5px;
    border-radius: 4px;
    cursor: pointer;
    transition: background-color 0.2s;
    font-size: 1.95em;
  }

  .breadcrumb-item:hover {
    background-color: rgba(255, 255, 255, 0.2);
  }

  .breadcrumb-item.current {
    font-weight: bold;
    color: #2196f3;
  }

  .breadcrumb-separator {
    margin: 0 3px;
    color: #888;
  }

  .loading-path {
    color: #888;
    font-style: italic;
    font-size: 1.95em;
  }

  .header-controls {
    display: flex;
    gap: 10px;
    align-items: center;
  }

  .icon-button {
    background: none;
    border: none;
    font-size: 20px;
    padding: 0;
    width: 30px;
    height: 30px;
    display: flex;
    align-items: center;
    justify-content: center;
    transition: transform 0.1s ease;
  }

  .icon-button:hover {
    transform: scale(1.1);
  }

  .icon-button.active span {
    color: #ff4136;
  }

  .delete-button.active span {
    color: #ff4136;
    transform: scale(1.1);
  }

  .home-button {
    margin-left: auto;
  }
</style>
