<script lang="ts">
  import { onMount, onDestroy } from "svelte";
  import {
    type NodeData,
    type RecognitionStore,
    createRec,
  } from "../../stores/rec.svelte";
  import Child from "./Child.svelte";
  import DropDown from "./DropDown.svelte";
  import * as d3 from "d3";
  import { derived as derivedStore, type Readable, get } from "svelte/store";
  import { getContext, setContext } from "svelte";

  // Use $props() instead of export let in runes mode
  let { store = createRec() } = $props<{
    store?: RecognitionStore;
  }>();

  // Get toast context from App
  const { showToast } = getContext<{
    showToast: (
      message: string,
      type?: "info" | "success" | "warning" | "error",
    ) => void;
  }>("toast");

  // Local state management that Parent fully controls
  let currentStore = $state<RecognitionStore>(store);
  let navigationStack = $state<RecognitionStore[]>([]);
  let isTransitioning = $state(false);
  let deleteMode = $state(false);

  // Update the store when prop changes
  $effect(() => {
    currentStore = store;
  });

  // Create derived value from store
  let childrenStore = $derived(currentStore?.childrenStore);

  let childNodes: RecognitionStore[] = $derived.by(() => {
    if (!currentStore) return [];
    return $childrenStore.map((child) => {
      return currentStore.getChild(child[0]);
    });
  });

  let packData: Readable<{
    id: string;
    selfPoints: number;
    children: NodeData[];
  }> = $derived.by(() => {
    return derivedStore(
      [currentStore?.pointsStore, ...childNodes.map((c) => c.pointsStore)],
      (points) => {
        const [self, ...children] = points;
        return {
          id: currentStore?.id,
          selfPoints: self,
          children: children.map((c, i) => ({
            id: childNodes[i]?.id,
            store: childNodes[i],
            points: c,
            children: [],
          })),
        };
      },
    );
  });

  const hierarchyData = $derived.by(() => {
    const data = $packData;
    // Create hierarchy
    const hierarchy = d3.hierarchy<NodeData>(
      {
        id: data.id,
        store: currentStore,
        points: data.selfPoints,
        children: data.children,
      },
      (d) => d?.children ?? [],
    );

    // Sum for sizing
    hierarchy.sum((d) => d?.points ?? 0);

    // Check if we have only one child or no children
    const childCount = data.children?.length || 0;
    const shouldUsePadding = childCount > 1;

    // Apply treemap with relative sizing (0-1 range)
    const treemap = d3
      .treemap<NodeData>()
      .tile(d3.treemapSquarify)
      .size([1, 1]) // Use 0-1 range for relative sizing
      .padding(shouldUsePadding ? 0.005 : 0) // Only use padding with multiple nodes
      .round(false); // Don't round to pixels

    return treemap(hierarchy);
  });

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
  const GROWTH_RATE = (node: d3.HierarchyRectangularNode<NodeData>) => {
    const points = node.data?.points || 0;
    return Math.max(
      0.1,
      Math.min(2, BASE_GROWTH_RATE * Math.sqrt(points / 10)),
    );
  };

  // Shrink rate based on node size
  const SHRINK_RATE = (node: d3.HierarchyRectangularNode<NodeData>) => {
    const points = node.data?.points || 0;
    return Math.min(
      -0.1,
      Math.max(-2, BASE_SHRINK_RATE * Math.sqrt(points / 10)),
    );
  };

  onMount(() => {
    // Add global event listeners for touch end
    document.addEventListener("mouseup", handleGlobalTouchEnd);
    document.addEventListener("touchend", handleGlobalTouchEnd);
    document.addEventListener("touchcancel", handleGlobalTouchEnd);
  });

  onDestroy(() => {
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

  // Handle zooming out to parent
  async function zoomOutToParent() {
    if (isTransitioning || !currentStore) return;
    isTransitioning = true;

    try {
      // Check if we have a parent to zoom out to
      const parentStore = await currentStore.getParent();
      if (parentStore) {
        // Update the store to the parent
        currentStore = parentStore;
        console.log(
          `Zoomed out to parent, new path: ${currentStore.path.join("/")}`,
        );
        showToast(`Zoomed out to parent`, "info");
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
    if (isTransitioning || !currentStore) return;
    isTransitioning = true;

    try {
      // Get the path to root
      const pathToRoot = await currentStore.getPathToRoot();

      // Calculate the target node from the index
      // pathToRoot is ordered from current to root, so we need to reverse the index
      const targetIndex = pathToRoot.length - 1 - index;

      if (targetIndex >= 0 && targetIndex < pathToRoot.length) {
        const target = pathToRoot[targetIndex];
        currentStore = target.store;
        console.log(
          `Navigated to path index ${index}, new path: ${currentStore.path.join("/")}`,
        );
        showToast(`Navigated to node`, "info");
      }
    } catch (err) {
      console.error(`Error navigating to path index ${index}:`, err);
      showToast("Error during navigation", "error");
    } finally {
      isTransitioning = false;
    }
  }

  // Handle adding a new node
  async function handleAddNode() {
    if (!currentStore) return;

    try {
      // Calculate new node points
      const newNodePoints = calculateNewNodePoints();

      console.log(
        "Adding child node using store with path:",
        currentStore.path.join("/"),
      );

      // Create new node
      const newNodeId = await currentStore.addChild("Undefined", newNodePoints);
      console.log("Child node created with ID:", newNodeId);
      showToast("New node created", "success");

      // Wait for the next render cycle to ensure the DOM has updated
      setTimeout(() => {
        // Set the node to edit mode
        setNodeToEditMode(newNodeId);
      }, 50);
    } catch (err) {
      console.error("Error creating new node:", err);
      showToast("Error creating new node", "error");
    }
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
          await currentStore.removeChild(nodeId);
          console.log(`Node ${nodeId} deleted successfully`);
          showToast("Node deleted successfully", "success");
        } catch (err) {
          console.error(`Error deleting node ${nodeId}:`, err);
          showToast("Error deleting node", "error");
        }
      }
    } else {
      // Handle zoom in navigation
      zoomIntoChild(nodeId);
    }
  }

  // Handle zooming into a node
  async function zoomIntoChild(nodeId: string) {
    try {
      // Get the child store
      const childStore = currentStore.getChild(nodeId);

      // Update our local currentStore
      currentStore = childStore;

      console.log(
        `Zoomed into node ${nodeId}, new path: ${childStore.path.join("/")}`,
      );
      showToast(`Zoomed into ${get(childStore.nameStore) || "node"}`, "info");
    } catch (err) {
      console.error(`Error zooming into node ${nodeId}:`, err);
      showToast("Error navigating to node", "error");
    }
  }

  // Handle text editing request from Child component
  async function handleTextEdit(detail: { nodeId: string; newName: string }) {
    const { nodeId, newName } = detail;

    try {
      // Get the appropriate store for this node
      const childStore = currentStore.getChild(nodeId);

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
        nodeId === currentStore.path[currentStore.path.length - 1]
          ? currentStore
          : currentStore.getChild(nodeId);

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
        nodeId === currentStore.path[currentStore.path.length - 1]
          ? currentStore
          : currentStore.getChild(nodeId);

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

  // Start the growth process
  function startGrowth(
    node: d3.HierarchyRectangularNode<NodeData>,
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
      if (isTouching && activeGrowthNodeId === node.data?.id) {
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
        (child) => child.data?.id === activeGrowthNodeId,
      );

      if (nodeToUpdate && nodeToUpdate.data && nodeToUpdate.data.id) {
        saveNodePoints(nodeToUpdate.data.id, nodeToUpdate.data.points || 0);
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
    node: d3.HierarchyRectangularNode<NodeData>,
    points: number,
  ) {
    if (!node.data || !node.data.id) return;

    // Update the node's points in the hierarchy
    node.data.points = points;
  }

  // Save final points to store
  async function saveNodePoints(nodeId: string, points: number) {
    try {
      const childStore = currentStore.getChild(nodeId);
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
    node: d3.HierarchyRectangularNode<NodeData>,
  ) {
    event.preventDefault();
    event.stopPropagation();

    // Set touch state
    isTouching = true;
    touchStartTime = Date.now();
    activeGrowthNodeId = node.data?.id || null;

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

  // Set up the navigation context here - Parent is responsible for this
  setContext("navigation", {
    deleteMode,
    toggleDeleteMode,
    handleAddNode,
    zoomOutToParent,
    navigateToPathIndex,
  });
</script>

<div class="node-container">
  <!-- Slot with hierarchyData for the Header -->
  <slot {hierarchyData} />

  <!-- Main treemap content -->
  <div class="app-content">
    <div class="treemap-container">
      {#each hierarchyData?.children || [] as child}
        <div
          class="clickable"
          class:deleting={deleteMode}
          class:growing={isGrowing && activeGrowthNodeId === child.data?.id}
          style="
              position: absolute;
              left: {child.x0 * 100}%;
              top: {child.y0 * 100}%;
              width: {(child.x1 - child.x0) * 100}%;
              height: {(child.y1 - child.y0) * 100}%;
              box-sizing: border-box;
            "
          onclick={(e) => handleNodeAction(e, child.data?.id)}
          onmousedown={(e) => handleGrowthStart(e, child)}
          onmouseup={(e) => handleGrowthEnd(e)}
          ontouchstart={(e) => handleGrowthStart(e, child)}
          ontouchend={(e) => handleGrowthEnd(e)}
          oncontextmenu={(e) => e.preventDefault()}
        >
          <Child
            {child}
            addContributor={handleAddContributor}
            removeContributor={handleRemoveContributor}
            onTextEdit={handleTextEdit}
            shouldEdit={nodeToEdit === child.data?.id}
          />
        </div>
      {/each}
    </div>
  </div>

  <!-- User dropdown for adding contributors -->
  {#if showUserDropdown}
    <DropDown
      position={dropdownPosition}
      show={showUserDropdown}
      title="Select Contributor"
      select={handleUserSelect}
      close={handleDropdownClose}
    />
  {/if}
</div>

<style>
  .node-container {
    width: 100%;
    height: 100%;
    display: flex;
    flex-direction: column;
    position: relative;
  }

  .app-content {
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
</style>
