<script lang="ts">
  import { type RecNode } from "../stores/rec";
  import * as d3 from "d3";

  export let node: d3.HierarchyRectangularNode<RecNode>;

  // Helper function to get color based on name
  function getColorForName(name: string): string {
    if (!name) return "#64748b"; // Default slate color

    // Simple hash function for consistent colors
    let hash = 0;
    for (let i = 0; i < name.length; i++) {
      hash = name.charCodeAt(i) + ((hash << 5) - hash);
    }

    // Generate colors in a pleasant range
    const h = Math.abs(hash) % 360;
    const s = 65 + (Math.abs(hash) % 20); // 65-85% saturation
    const l = 60 + (Math.abs(hash) % 15); // 60-75% lightness

    return `hsl(${h}, ${s}%, ${l}%)`;
  }

  // Determine if this is the root node
  $: isRoot = !node.parent;
  $: nodeName = node.data?.name || "Unnamed";
  $: textSegments = nodeName.split(/(?=[A-Z][^A-Z])/g);
</script>

<div
  class="treemap-header clickable"
  style:background-color={isRoot ? "#fff" : getColorForName(nodeName)}
  style:border={node.data?.hasDirectContributionChild
    ? "3px solid #2196f3"
    : "2px solid #fff"}
>
  <div class="header-content">
    <!-- Node name as segmented text -->
    <div class="node-name">
      {#each textSegments as segment, i}
        <div class="text-segment">{segment}</div>
      {/each}
    </div>

    <!-- Control buttons for root node -->
    {#if isRoot}
      <div class="header-controls">
        <!-- Inventory button -->
        <button class="icon-button inventory-button">
          <span>🎒</span>
        </button>

        <!-- Peer button -->
        <button class="icon-button peer-button">
          <span>🔍</span>
        </button>

        <!-- Add button -->
        <button class="icon-button add-button">
          <span>➕</span>
        </button>

        <!-- Delete button -->
        <button class="icon-button delete-button">
          <span>🗑️</span>
        </button>
      </div>
    {:else if node.data?.isContributorTree}
      <!-- Home button for contributor trees -->
      <button class="icon-button home-button">
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
  }

  .text-segment {
    line-height: 1.2em;
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

  .home-button {
    margin-left: auto;
  }
</style>
