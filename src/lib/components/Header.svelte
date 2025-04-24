<script lang="ts">
  import {
    type NodeData,
    type RecognitionStore,
  } from "../../stores/rec.svelte";
  import * as d3 from "d3";
  import { onMount, createEventDispatcher, getContext } from "svelte";

  // Event dispatcher for path navigation
  const dispatch = createEventDispatcher<{
    navigateToPath: number;
  }>();

  // Use $props() for Svelte 5 runes mode
  let { node = undefined, store = undefined } = $props<{
    node?: d3.HierarchyNode<NodeData> | null;
    store?: RecognitionStore;
  }>();

  // Get panel context from App
  const { handleInventoryClick, handlePeerClick, handleChartsClick } =
    getContext<{
      handleInventoryClick: () => void;
      handlePeerClick: () => void;
      handleChartsClick: () => void;
    }>("panels");

  // Navigation context - either provided by Parent or we create defaults
  let navigationContext = getContext<{
    deleteMode: boolean;
    toggleDeleteMode: () => void;
    handleAddNode: () => void;
    zoomOutToParent: () => void;
    navigateToPathIndex?: (index: number) => void;
  }>("navigation");

  // If no navigation context, use default no-op functions
  if (!navigationContext) {
    navigationContext = {
      deleteMode: false,
      toggleDeleteMode: () =>
        console.log("Toggle delete mode - not implemented"),
      handleAddNode: () => console.log("Add node - not implemented"),
      zoomOutToParent: () => console.log("Zoom out - not implemented"),
      navigateToPathIndex: (index) =>
        console.log(`Navigate to path index ${index} - not implemented`),
    };
  }

  const {
    deleteMode,
    toggleDeleteMode,
    handleAddNode,
    zoomOutToParent,
    navigateToPathIndex,
  } = navigationContext;

  // Access store properties directly with $ syntax
  let name = $derived(store?.nameStore);
  let hasDirectContributionChild = $derived(store?.hasContributorsStore);

  // Path names state
  let pathNames = $state<string[]>([]);
  let isLoadingPath = $state(false);

  // Determine the effective store based on node/store props
  let effectiveStore = $derived(node?.data?.store || store);

  // Determine if this is the root node
  const isRoot = $derived(node && !node.parent);
  const nodeName = $derived($name || node?.data?.name || "Unnamed");
  const textSegments = $derived(nodeName.split(/(?=[A-Z][^A-Z])/g));

  // Button config - centralized configuration for all header buttons
  const buttons = $derived.by(() => {
    return [
      {
        id: "inventory",
        icon: "🎒",
        title: "View inventory",
        onClick: handleInventoryClick,
        showWhen: () => true, // Root buttons are always shown now
      },
      {
        id: "charts",
        icon: "📊",
        title: "View charts",
        onClick: handleChartsClick,
        showWhen: () => true,
      },
      {
        id: "peer",
        icon: "🔍",
        title: "User login",
        onClick: handlePeerClick,
        showWhen: () => true,
      },
      {
        id: "add",
        icon: "➕",
        title: "Add new node",
        onClick: handleAddNode,
        showWhen: () => true,
      },
      {
        id: "delete",
        icon: "🗑️",
        title: "Toggle delete mode",
        onClick: toggleDeleteMode,
        active: deleteMode,
        showWhen: () => true,
      },
      {
        id: "home",
        icon: "🏠",
        title: "Go to parent",
        onClick: zoomOutToParent,
        showWhen: () => false,
      },
    ];
  });

  // Create effect to reload path when store or node changes
  $effect(() => {
    if (effectiveStore) {
      loadPathNames();
    }
  });

  // Load the path names from the store
  async function loadPathNames() {
    if (!effectiveStore) return;

    isLoadingPath = true;

    try {
      // Get the path from the current node to the root
      const pathToRoot = await effectiveStore.getPathToRoot();

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

  // Generic handler for all button clicks
  function handleButtonClick(e: MouseEvent, handler: () => void) {
    e.stopPropagation();
    handler();
  }

  // Handle breadcrumb navigation - dispatch event to parent
  function handlePathClick(e: MouseEvent, index: number) {
    e.stopPropagation();
    console.log(`Navigate to path index: ${index}`);
    if (navigateToPathIndex) {
      navigateToPathIndex(index);
    } else {
      // Fallback to event dispatch if no context method available
      dispatch("navigateToPath", index);
    }
  }
</script>

<div
  class="treemap-header clickable"
  class:delete-mode={deleteMode}
  style:border={$hasDirectContributionChild
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

    <!-- Control buttons based on configuration -->
    <div class="header-controls">
      {#each buttons.filter((btn) => btn.showWhen()) as button (button.id)}
        <button
          class="icon-button {button.id}-button"
          class:active={button.active}
          onclick={(e) => handleButtonClick(e, button.onClick)}
          title={button.title}
        >
          <span>{button.icon}</span>
        </button>
      {/each}
    </div>
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
