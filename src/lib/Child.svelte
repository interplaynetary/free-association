<script lang="ts">
  import * as d3 from "d3";
  import { type RecNode } from "../stores/rec";
  import TagPill from "./TagPill.svelte";

  // Use $props to get the child node and callbacks
  let {
    child,
    addContributor = (detail: {
      nodeId: string;
      clientX: number;
      clientY: number;
    }) => {},
    removeContributor = (detail: {
      nodeId: string;
      contributorId: string;
    }) => {},
  } = $props<{
    child: d3.HierarchyRectangularNode<RecNode>;
    addContributor?: (detail: {
      nodeId: string;
      clientX: number;
      clientY: number;
    }) => void;
    removeContributor?: (detail: {
      nodeId: string;
      contributorId: string;
    }) => void;
  }>();

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

  // Create unique ID for this node
  const uid = (name: string) => {
    const id = `id-${Math.random().toString(36).substring(2, 9)}`;
    return {
      id,
      href: `#${id}`,
    };
  };

  const leafUid = uid("leaf");
  const clipUid = uid("clip");

  // Prepare text segments
  const name = child.data?.name || "Unnamed";
  const segments = name === "Unnamed" ? [name] : name.split(/(?=[A-Z][^A-Z])/g);

  // Calculate relative size for text scaling
  const nodeWidth = $derived(child.x1 - child.x0);
  const nodeHeight = $derived(child.y1 - child.y0);
  const nodeSizeRatio = $derived(Math.min(nodeWidth, nodeHeight) * 100); // Size as percentage of parent
  const fontSize = $derived(Math.max(0.5, Math.min(2, nodeSizeRatio * 0.1))); // Scale font size (0.5-2rem)

  // Calculate button and tag scaling values - continuous scaling approach
  const buttonSize = $derived(Math.max(8, Math.min(24, nodeSizeRatio * 0.3))); // Scale button size (8-24px)
  const buttonFontSize = $derived(
    Math.max(6, Math.min(16, nodeSizeRatio * 0.25)),
  ); // Scale button font (6-16px)
  const tagScale = $derived(Math.max(0.4, Math.min(1, nodeSizeRatio * 0.02))); // Scale tags (0.4-1)

  // Calculate visibility factor (0-1) for smooth fade-in
  const visibilityFactor = $derived(
    Math.min(1, Math.max(0, (nodeSizeRatio - 5) / 7)),
  );

  // Dynamically decide between full and mini view based on size
  const showFullContributorDetails = $derived(nodeSizeRatio >= 25);

  // Dynamically adjust truncation length based on node size
  const truncateLength = $derived(
    Math.floor(Math.max(3, Math.min(12, nodeSizeRatio * 0.15))),
  );

  // Extract contributor IDs from node data
  const contributorIds = $derived(
    child.data?.contributors
      ? Array.isArray(child.data.contributors)
        ? child.data.contributors
        : Object.keys(child.data.contributors)
      : [],
  );

  // Function to handle add contributor button click
  function handleAddContributorClick(event: MouseEvent) {
    event.stopPropagation();
    const nodeId = child.data?._key;
    if (nodeId) {
      addContributor({
        nodeId,
        clientX: event.clientX,
        clientY: event.clientY,
      });
    }
  }

  // Function to handle remove contributor
  function handleRemoveContributor(contributorId: string) {
    const nodeId = child.data?._key;
    if (nodeId) {
      removeContributor({ nodeId, contributorId });
    }
  }

  // Handle click on tag
  function handleTagClick(userId: string) {
    // For future functionality - could navigate to contributor view
    console.log(`Clicked on contributor: ${userId}`);
  }
</script>

<div
  class="treemap-node"
  style="
    background-color: {getColorForName(child.data?.name || '')};
    border: {child.data?.hasDirectContributionChild
    ? '2px solid #2196f3'
    : '1px solid #fff'};
    width: 100%;
    height: 100%;
    overflow: hidden;
    box-sizing: border-box;
    display: flex;
    flex-direction: column;
  "
>
  <div
    class="node-content"
    style="
      width: 100%;
      height: 100%;
      display: flex;
      flex-direction: column;
      align-items: center;
      justify-content: center;
      padding: 5%;
      box-sizing: border-box;
    "
  >
    <div class="node-body">
      <div
        class="node-text edit-text-field"
        style="
          user-select: none;
          display: flex;
          flex-direction: column;
          align-items: center;
          font-size: {fontSize}rem;
        "
        title={name}
      >
        {#each segments as segment, i}
          <span class="text-segment">
            {segment}
          </span>
        {/each}
      </div>

      <!-- Always show contributors but with opacity based on size -->
      <div
        class="contributor-container"
        style="
          margin-top: {Math.max(1, nodeSizeRatio * 0.05)}px;
          opacity: {visibilityFactor};
          visibility: {visibilityFactor > 0.1 ? 'visible' : 'hidden'};
        "
      >
        <div class="contributor-layout">
          <!-- Add contributor button (positioned to the left) -->
          <div
            class="button-container"
            class:has-tags={contributorIds.length > 0}
          >
            <button
              class="add-contributor-button"
              style="
                width: {buttonSize}px;
                height: {buttonSize}px;
                min-width: {buttonSize}px;
                border-radius: {buttonSize / 2}px;
                font-size: {buttonFontSize}px;
              "
              onclick={handleAddContributorClick}
              title="Add contributor"
            >
              +
            </button>
          </div>

          {#if contributorIds.length > 0}
            <!-- Tag container with better overflow handling -->
            <div class="tag-container" style="transform: scale({tagScale});">
              {#if showFullContributorDetails}
                <!-- Full tag pills for larger nodes -->
                {#each contributorIds as contributorId}
                  <div class="tag-wrapper-item">
                    <TagPill
                      userId={contributorId}
                      {truncateLength}
                      onClick={handleTagClick}
                      onRemove={handleRemoveContributor}
                    />
                  </div>
                {/each}
              {:else}
                <!-- Simplified view for smaller nodes -->
                {#each contributorIds.slice(0, 3) as contributorId, i}
                  <div class="contributor-tag-mini" title={contributorId}></div>
                {/each}
                {#if contributorIds.length > 3}
                  <div class="contributor-more" title="More contributors">
                    +{contributorIds.length - 3}
                  </div>
                {/if}
              {/if}
            </div>
          {/if}
        </div>
      </div>
    </div>
  </div>
</div>

<style>
  .treemap-node {
    transition:
      background-color 0.2s ease,
      border 0.2s ease;
    border-radius: 2px;
    position: relative;
  }

  .node-content {
    text-align: center;
  }

  .node-body {
    display: flex;
    flex-direction: column;
    align-items: center;
    gap: 4px;
    max-width: 100%;
  }

  .node-text {
    color: rgba(0, 0, 0, 0.8);
    text-shadow:
      0px 0px 3px rgba(255, 255, 255, 0.8),
      0px 0px 2px rgba(255, 255, 255, 0.6);
    font-weight: 500;
    overflow: hidden;
    max-width: 100%;
  }

  .text-segment {
    line-height: 1.2;
    padding: 1px 0;
  }

  :global(.edit-text-field) {
    cursor: text;
  }

  .contributor-container {
    width: 100%;
    overflow: visible;
    transition: opacity 0.3s ease;
  }

  .contributor-layout {
    display: flex;
    width: 100%;
    align-items: flex-start;
    justify-content: center;
  }

  .button-container {
    display: flex;
    justify-content: center;
    transition: all 0.2s ease;
  }

  .button-container.has-tags {
    margin-right: 5px;
    justify-content: flex-start;
  }

  .add-contributor-button {
    display: flex;
    align-items: center;
    justify-content: center;
    background: rgba(200, 200, 200, 0.7);
    line-height: 1;
    color: #333;
    cursor: pointer;
    border: none;
    padding: 0;
    transition: all 0.2s ease;
    z-index: 5;
  }

  .add-contributor-button:hover {
    background: rgba(200, 200, 200, 0.9);
    transform: scale(1.1);
  }

  .tag-container {
    display: flex;
    flex-wrap: wrap;
    justify-content: flex-start;
    align-items: center;
    gap: 2px;
    transform-origin: top left;
    min-height: 24px;
    flex: 1;
    width: calc(100% - 30px);
    overflow: hidden;
  }

  .tag-wrapper-item {
    transform-origin: center;
    margin: 2px;
  }

  .contributor-tag-mini {
    width: 10px;
    height: 10px;
    border-radius: 50%;
    background: #e9f0f7;
    border: 1px solid rgba(255, 255, 255, 0.5);
    display: inline-block;
    animation: fadeIn 0.2s ease-out;
    margin: 0 2px;
  }

  .contributor-more {
    width: 14px;
    height: 14px;
    border-radius: 50%;
    background: #d1e1f0;
    color: #3a6b9e;
    display: flex;
    align-items: center;
    justify-content: center;
    font-weight: bold;
    font-size: 9px;
    animation: fadeIn 0.2s ease-out;
    margin: 0 2px;
  }

  @keyframes fadeIn {
    from {
      opacity: 0;
      transform: scale(0.8);
    }
    to {
      opacity: 1;
      transform: scale(1);
    }
  }
</style>
