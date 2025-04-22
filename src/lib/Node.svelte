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
  import DropDown from "./DropDown.svelte";
  import * as d3 from "d3";

  // Use $props() instead of export let in runes mode
  let { store: initialStore = createRec() } = $props<{
    store?: RecognitionStore;
  }>();

  // Make store reactive with $state
  let store = $state<RecognitionStore>(initialStore);
  let nodeStore = $state<NodeStoreHandler>(new NodeStoreHandler(store));

  // Track navigation state
  let navigationStack = $state<RecognitionStore[]>([]);
  let isTransitioning = $state(false);

  // DOM references
  let nodeContainer: HTMLDivElement | null = $state(null);
  let contentContainer: HTMLDivElement | null = $state(null);

  // Toast notification
  let toast = $state({
    visible: false,
    message: "",
    type: "info", // "info", "success", "warning", "error"
    timeoutId: null as number | null,
  });

  // Delete mode state
  let deleteMode = $state(false);

  // Contributors dropdown state
  let showUserDropdown = $state(false);
  let dropdownPosition = $state({ x: 0, y: 0 });
  let activeNodeId = $state<string | null>(null);

  // Growth state
  let touchStartTime = $state(0);
  let isTouching = $state(false);
  let activeGrowthNodeId = $state<string | null>(null);
  let isGrowing = $state(false);
  let growthInterval: number | null = $state(null);
  let growthTimeout: number | null = $state(null);

  // Constants for growth
  const GROWTH_DELAY = 300; // ms before growth starts
  const GROWTH_TICK = 50; // ms between growth updates
  const BASE_GROWTH_RATE = 0.5; // points per tick
  const BASE_SHRINK_RATE = -0.5; // points per tick

  // Growth rate based on node size
  const GROWTH_RATE = (node: d3.HierarchyRectangularNode<RecNode>) => {
    const points = node.data?.points || 0;
    return Math.max(
      0.1,
      Math.min(2, BASE_GROWTH_RATE * Math.sqrt(points / 10)),
    );
  };

  // Shrink rate based on node size
  const SHRINK_RATE = (node: d3.HierarchyRectangularNode<RecNode>) => {
    const points = node.data?.points || 0;
    return Math.min(
      -0.1,
      Math.max(-2, BASE_SHRINK_RATE * Math.sqrt(points / 10)),
    );
  };

  // Create effect to update nodeStore when initialStore or store's identity changes
  $effect(() => {
    // Only update if store has actually changed reference
    // This prevents infinite loops of updating the nodeStore
    if (nodeStore.store !== store) {
      updateNodeStore();
    }
  });

  onMount(() => {
    // We don't need to call updateNodeStore here as the effect will handle it
    // Just make sure we're subscribed to data updates
    if (!nodeStore.unsubscribers.length) {
      nodeStore.subscribeToStoreData();
    }

    // Add global event listeners for touch end
    document.addEventListener("mouseup", handleGlobalTouchEnd);
    document.addEventListener("touchend", handleGlobalTouchEnd);
    document.addEventListener("touchcancel", handleGlobalTouchEnd);
  });

  onDestroy(() => {
    // Clean up subscriptions when component is destroyed
    if (nodeStore) {
      nodeStore.destroy();
    }

    // Clear any toast timeout
    if (toast.timeoutId) {
      clearTimeout(toast.timeoutId);
    }

    // Clean up growth timers
    if (growthInterval) {
      clearInterval(growthInterval);
    }
    if (growthTimeout) {
      clearTimeout(growthTimeout);
    }

    // Remove global event listeners
    document.removeEventListener("mouseup", handleGlobalTouchEnd);
    document.removeEventListener("touchend", handleGlobalTouchEnd);
    document.removeEventListener("touchcancel", handleGlobalTouchEnd);
  });

  // Update nodeStore when store changes
  function updateNodeStore() {
    console.log("Updating nodeStore with store:", store.path.join("/"));

    // Clean up old store subscriptions
    if (nodeStore) {
      nodeStore.destroy();
    }

    // Create new nodeStore with current store
    nodeStore = new NodeStoreHandler(store);

    // Subscribe to data updates
    nodeStore.subscribeToStoreData();
  }

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
    //hierarchy.sort((a, b) => (b?.data?.points ?? 0) - (a?.data?.points ?? 0));

    // Check if we have only one child or no children
    const childCount = data.children?.length || 0;
    const shouldUsePadding = childCount > 1;

    // Apply treemap with relative sizing (0-1 range)
    const treemap = d3
      .treemap<RecNode>()
      .tile(d3.treemapSquarify)
      .size([1, 1]) // Use 0-1 range for relative sizing
      .padding(shouldUsePadding ? 0.005 : 0) // Only use padding with multiple nodes
      .round(false); // Don't round to pixels

    return treemap(hierarchy);
  });

  // Method to calculate new node points (10% of total or 100 if no siblings)
  function calculateNewNodePoints(): number {
    // Check if there are any existing child nodes
    if (!hierarchyData?.children || hierarchyData.children.length === 0) {
      // No siblings - set to 100 points (100%)
      return 100;
    }

    // Has siblings - use 10% of total points
    const currentLevelPoints = hierarchyData.children.reduce(
      (sum, node) => sum + (node.data?.points || 0),
      0,
    );

    return Math.max(1, currentLevelPoints * 0.1);
  }

  // Show toast message
  function showToast(
    message: string,
    type: "info" | "success" | "warning" | "error" = "info",
  ) {
    // Clear any existing toast timeout
    if (toast.timeoutId) {
      clearTimeout(toast.timeoutId);
    }

    // Show new toast
    toast = {
      visible: true,
      message,
      type,
      timeoutId: window.setTimeout(() => {
        toast.visible = false;
      }, 3000) as unknown as number,
    };
  }

  // Handle all node-related actions with event delegation
  async function handleNodeAction(event: MouseEvent, nodeId: string) {
    // Only handle clicks, not growth interactions
    const touchDuration = Date.now() - touchStartTime;
    if (touchDuration >= GROWTH_DELAY || isGrowing) {
      return;
    }

    if (deleteMode) {
      // Handle deletion
      if (confirm(`Delete this node?`)) {
        try {
          await store.removeChild(nodeId);
          console.log(`Node ${nodeId} deleted successfully`);
          showToast("Node deleted successfully", "success");
          deleteMode = false; // Turn off delete mode after deletion
        } catch (err) {
          console.error(`Error deleting node ${nodeId}:`, err);
          showToast("Error deleting node", "error");
        }
      }
    } else {
      // Handle zoom in navigation
      zoomIntoNode(nodeId);
    }
  }

  // Handle text editing request from Child component
  async function handleTextEdit(detail: { nodeId: string; newName: string }) {
    const { nodeId, newName } = detail;

    try {
      // Get the appropriate store for this node
      const childStore = store.getChild(nodeId);

      // Update the name in the store
      await childStore.updateName(newName);
      console.log(`Updated name for node ${nodeId} to "${newName}"`);
      showToast(`Node renamed to "${newName}"`, "success");
    } catch (err) {
      console.error(`Error updating name for node ${nodeId}:`, err);
      showToast("Error updating node name", "error");
    }
  }

  // Handle add contributor button click from Child component
  function handleAddContributor(detail: {
    nodeId: string;
    clientX: number;
    clientY: number;
  }) {
    const { nodeId, clientX, clientY } = detail;

    // Show user dropdown for adding contributor
    activeNodeId = nodeId;
    dropdownPosition = { x: clientX, y: clientY };
    showUserDropdown = true;
  }

  // Handle remove contributor request from Child component
  async function handleRemoveContributor(detail: {
    nodeId: string;
    contributorId: string;
  }) {
    const { nodeId, contributorId } = detail;

    try {
      // Get the appropriate store for this node
      const targetStore =
        nodeId === store.path[store.path.length - 1]
          ? store
          : store.getChild(nodeId);

      // Remove the contributor
      await targetStore.removeContributor(contributorId);
      console.log(`Removed contributor ${contributorId} from node ${nodeId}`);
      showToast("Contributor removed successfully", "success");
    } catch (err) {
      console.error("Error removing contributor:", err);
      showToast("Error removing contributor", "error");
    }
  }

  // Handle user selection from dropdown
  function handleUserSelect(detail: { id: string; name: string }) {
    const { id: userId } = detail;

    if (!activeNodeId) {
      console.error("No node selected for adding contributor");
      return;
    }

    // Add the contributor to the node
    addContributorToNode(activeNodeId, userId);

    // Reset dropdown state
    showUserDropdown = false;
    activeNodeId = null;
  }

  // Handle dropdown close
  function handleDropdownClose() {
    showUserDropdown = false;
    activeNodeId = null;
  }

  // Add contributor to node
  async function addContributorToNode(nodeId: string, userId: string) {
    try {
      // Get the appropriate store for this node
      const targetStore =
        nodeId === store.path[store.path.length - 1]
          ? store
          : store.getChild(nodeId);

      // Add the contributor
      await targetStore.addContributor(userId);
      console.log(`Added contributor ${userId} to node ${nodeId}`);
      showToast("Contributor added successfully", "success");
    } catch (err) {
      console.error("Error adding contributor:", err);
      showToast("Error adding contributor", "error");
    }
  }

  // State for node that should be in edit mode
  let nodeToEdit = $state<string | null>(null);

  // Handler for adding a new child node
  async function handleAddNode() {
    try {
      const newNodePoints = calculateNewNodePoints();

      console.log(
        "Adding child node using store with path:",
        store.path.join("/"),
      );

      // Create new node
      const newNodeId = await store.addChild("Undefined", newNodePoints);
      console.log("Child node created with ID:", newNodeId);
      showToast("New node created", "success");

      // Wait for the next render cycle to ensure the DOM has updated
      setTimeout(() => {
        // Set the node to edit mode directly instead of showing modal
        setNodeToEditMode(newNodeId);
      }, 50);
    } catch (err) {
      console.error("Error creating new node:", err);
      showToast("Error creating new node", "error");
    }
  }

  // Set a node to edit mode
  function setNodeToEditMode(nodeId: string) {
    nodeToEdit = nodeId;
    // This flag will be used by the Child component to know when to enter edit mode

    // Auto-clear the edit flag after a short delay if it wasn't handled
    setTimeout(() => {
      if (nodeToEdit === nodeId) {
        nodeToEdit = null;
      }
    }, 1000);
  }

  // Toggle delete mode
  function toggleDeleteMode() {
    deleteMode = !deleteMode;
    console.log(`Delete mode: ${deleteMode ? "activated" : "deactivated"}`);

    // Show toast about delete mode
    showToast(
      deleteMode
        ? "Delete mode activated. Click a node to delete it."
        : "Delete mode deactivated.",
      deleteMode ? "warning" : "info",
    );
  }

  // Start the growth process
  function startGrowth(
    node: d3.HierarchyRectangularNode<RecNode>,
    isShrinking: boolean = false,
  ) {
    // Don't allow growth in delete mode
    if (deleteMode) return;

    // Clear any existing growth state
    if (growthInterval) clearInterval(growthInterval);
    if (growthTimeout) clearTimeout(growthTimeout);
    isGrowing = false;

    // Set the growth start delay
    growthTimeout = window.setTimeout(() => {
      // Only start growing if still touching the same node
      if (isTouching && activeGrowthNodeId === node.data?._key) {
        isGrowing = true;

        growthInterval = window.setInterval(() => {
          // Stop if no longer touching
          if (!isTouching) {
            stopGrowth();
            return;
          }

          // Calculate growth rate
          const rate = isShrinking ? SHRINK_RATE(node) : GROWTH_RATE(node);
          const currentPoints =
            node.data && typeof node.data.points === "number"
              ? node.data.points
              : 0;
          const newPoints = Math.max(1, currentPoints + rate); // Ensure minimum of 1 point

          if (isNaN(newPoints)) {
            console.error("Growth calculation resulted in NaN:", {
              currentPoints,
              rate,
              isShrinking,
            });
            return;
          }

          // Only update if points actually changed
          if (node.data && newPoints !== currentPoints) {
            updateNodePoints(node, newPoints);
          }
        }, GROWTH_TICK);
      }
    }, GROWTH_DELAY);
  }

  // Stop the growth process
  function stopGrowth() {
    isTouching = false;

    // Save final points value if we were growing
    if (isGrowing && activeGrowthNodeId && hierarchyData) {
      const nodeToUpdate = hierarchyData.children?.find(
        (child) => child.data?._key === activeGrowthNodeId,
      );

      if (nodeToUpdate && nodeToUpdate.data && nodeToUpdate.data._key) {
        saveNodePoints(nodeToUpdate.data._key, nodeToUpdate.data.points || 0);
      }
    }

    // Reset growth state
    activeGrowthNodeId = null;
    if (growthTimeout) clearTimeout(growthTimeout);
    if (growthInterval) clearInterval(growthInterval);
    growthInterval = null;
    isGrowing = false;
  }

  // Update node points during growth
  function updateNodePoints(
    node: d3.HierarchyRectangularNode<RecNode>,
    points: number,
  ) {
    if (!node.data || !node.data._key) return;

    // Update the node's points in the hierarchy
    node.data.points = points;

    // Cause a re-layout in the next tick for smooth animation
    setTimeout(() => {
      // This forces a re-evaluation of the hierarchyData derived state
      nodeStore.refreshHierarchyData();
    }, 0);
  }

  // Save final points to store
  async function saveNodePoints(nodeId: string, points: number) {
    try {
      const childStore = store.getChild(nodeId);
      await childStore.updatePoints(points);
      console.log(`Saved points for node ${nodeId}: ${points}`);
    } catch (err) {
      console.error(`Error saving points for node ${nodeId}:`, err);
      showToast("Error saving node points", "error");
    }
  }

  // Handle global touch end (to catch events outside the node)
  function handleGlobalTouchEnd() {
    if (isTouching) {
      stopGrowth();
    }
  }

  // Handle touch/mouse start for growth
  function handleGrowthStart(
    event: MouseEvent | TouchEvent,
    node: d3.HierarchyRectangularNode<RecNode>,
  ) {
    event.preventDefault();
    event.stopPropagation();

    // Set touch state
    isTouching = true;
    touchStartTime = Date.now();
    activeGrowthNodeId = node.data?._key || null;

    // Determine if this is a right-click or two-finger touch for shrinking
    const isShrinking =
      event instanceof MouseEvent
        ? event.button === 2 // right click
        : (event as TouchEvent).touches.length === 2; // two finger touch

    // Start the growth process
    startGrowth(node, isShrinking);
  }

  // Handle touch/mouse end for growth
  function handleGrowthEnd(event: MouseEvent | TouchEvent) {
    event.preventDefault();
    event.stopPropagation();
    stopGrowth();
  }

  // Handle zooming into a node
  async function zoomIntoNode(nodeId: string) {
    if (isTransitioning) return;
    isTransitioning = true;

    try {
      // Save current store to navigation stack
      navigationStack = [...navigationStack, store];

      // Update the store to the child node
      const childStore = store.getChild(nodeId);
      store = childStore;
      console.log(
        `Zoomed into node ${nodeId}, new path: ${store.path.join("/")}`,
      );

      // Update the node store handler
      updateNodeStore();

      showToast(`Zoomed into ${nodeStore.name || "node"}`, "info");
    } catch (err) {
      console.error(`Error zooming into node ${nodeId}:`, err);
      showToast("Error navigating to node", "error");
    } finally {
      isTransitioning = false;
    }
  }

  // Handle zooming out to parent
  async function zoomOutToParent() {
    if (isTransitioning) return;
    isTransitioning = true;

    try {
      // Check if we have a parent to zoom out to
      const parentStore = await store.getParent();
      if (parentStore) {
        // Update the store to the parent
        store = parentStore;
        console.log(`Zoomed out to parent, new path: ${store.path.join("/")}`);

        // Update the node store handler
        updateNodeStore();

        showToast(`Zoomed out to ${nodeStore.name || "parent"}`, "info");
      } else {
        console.log("Already at root level, cannot zoom out further");
        showToast("Already at top level", "info");
      }
    } catch (err) {
      console.error("Error zooming out to parent:", err);
      showToast("Error navigating to parent", "error");
    } finally {
      isTransitioning = false;
    }
  }

  // Handle breadcrumb navigation
  async function navigateToPathIndex(index: number) {
    if (isTransitioning) return;
    isTransitioning = true;

    try {
      if (store) {
        // Get the path to root
        const pathToRoot = await store.getPathToRoot();

        // Calculate the target node from the index
        // pathToRoot is ordered from current to root, so we need to reverse the index
        const targetIndex = pathToRoot.length - 1 - index;

        if (targetIndex >= 0 && targetIndex < pathToRoot.length) {
          const target = pathToRoot[targetIndex];
          store = target.store;
          console.log(
            `Navigated to path index ${index}, new path: ${store.path.join("/")}`,
          );

          // Update the node store handler
          updateNodeStore();

          showToast(`Navigated to ${nodeStore.name || "node"}`, "info");
        }
      }
    } catch (err) {
      console.error(`Error navigating to path index ${index}:`, err);
      showToast("Error during navigation", "error");
    } finally {
      isTransitioning = false;
    }
  }
</script>

{#if hierarchyData}
  <div class="node-container" bind:this={nodeContainer}>
    <!-- Header -->
    <div class="node-header">
      <Header
        node={hierarchyData}
        {deleteMode}
        onAddClick={handleAddNode}
        onDeleteToggle={toggleDeleteMode}
        onZoomOut={zoomOutToParent}
        {store}
        on:navigateToPath={(e) => navigateToPathIndex(e.detail)}
      />
    </div>

    <!-- Content area with children - now using DIVs instead of SVG -->
    <div class="node-content" bind:this={contentContainer}>
      <div
        class="treemap-container"
        oncontextmenu={(e) => e.preventDefault()}
        onclick={(e) => {
          // Skip handling if the click was on an edit field
          if (e.target instanceof Element) {
            const isTextClick = !!e.target.closest(".edit-text-field");
            if (isTextClick) {
              // Let the Child component handle text editing
              return;
            }

            // Only handle clicks directly on div.child-node elements
            const target = e.target.closest(".child-node");
            if (target instanceof HTMLElement) {
              const nodeId = target.dataset.nodeKey;
              if (nodeId) {
                handleNodeAction(e, nodeId);
              }
            }
          }
        }}
      >
        {#each hierarchyData.children || [] as child (child.data?._key)}
          <div
            class="clickable child-node"
            class:deleting={deleteMode}
            class:growing={isGrowing && activeGrowthNodeId === child.data?._key}
            data-node-key={child.data?._key}
            id={`node-${child.data?._key}`}
            style="
              position: absolute;
              left: {child.x0 * 100}%;
              top: {child.y0 * 100}%;
              width: {(child.x1 - child.x0) * 100}%;
              height: {(child.y1 - child.y0) * 100}%;
              transition: {isGrowing ? 'none' : 'all 0.2s ease'};
            "
            onmousedown={(e) => handleGrowthStart(e, child)}
            ontouchstart={(e) => handleGrowthStart(e, child)}
          >
            <Child
              {child}
              addContributor={handleAddContributor}
              removeContributor={handleRemoveContributor}
              onTextEdit={handleTextEdit}
              shouldEdit={nodeToEdit === child.data?._key}
            />
          </div>
        {/each}
      </div>
    </div>

    <!-- Use DropDown component for selecting contributors -->
    {#if showUserDropdown}
      <DropDown
        title="Add Contributor"
        searchPlaceholder="Search contributors..."
        position={dropdownPosition}
        width={280}
        maxHeight={320}
        show={true}
        select={handleUserSelect}
        close={handleDropdownClose}
      />
    {/if}

    <!-- Toast notification using Svelte 5 conditional rendering -->
    {#if toast.visible}
      <div class="toast-container">
        <div class="toast toast-{toast.type}">
          {toast.message}
        </div>
      </div>
    {/if}
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
    position: relative;
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

  .treemap-container {
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
    user-select: none;
    touch-action: none; /* Disable browser handling of gestures */
  }

  :global(.clickable.deleting) {
    cursor: not-allowed;
    opacity: 0.7;
    transition: opacity 0.2s ease;
  }

  :global(.clickable.deleting:hover) {
    opacity: 1;
  }

  :global(.clickable.growing) {
    z-index: 10;
  }

  .toast-container {
    position: absolute;
    bottom: 20px;
    left: 50%;
    transform: translateX(-50%);
    z-index: 1000;
  }

  .toast {
    padding: 10px 20px;
    border-radius: 4px;
    background-color: #333;
    color: white;
    box-shadow: 0 2px 10px rgba(0, 0, 0, 0.2);
    animation: fadeIn 0.3s ease-out;
  }

  .toast-success {
    background-color: #28a745;
  }

  .toast-warning {
    background-color: #ffc107;
    color: #333;
  }

  .toast-error {
    background-color: #dc3545;
  }

  @keyframes fadeIn {
    from {
      opacity: 0;
      transform: translateY(20px);
    }
    to {
      opacity: 1;
      transform: translateY(0);
    }
  }
</style>
