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
    onTextEdit = (detail: { nodeId: string; newName: string }) => {},
    shouldEdit = false, // New prop to check if this node should enter edit mode
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
    onTextEdit?: (detail: { nodeId: string; newName: string }) => void;
    shouldEdit?: boolean; // Flag to indicate if this node should immediately be in edit mode
  }>();

  // Editing state
  let isEditing = $state(false);
  let editValue = $state("");
  let editInput: HTMLInputElement | null = $state(null);
  let isMobile = $state(false);

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
  const name = $derived(child.data?.name || "Unnamed");
  const segments = $derived(
    name === "Unnamed" ? [name] : name.split(/(?=[A-Z][^A-Z])/g),
  );

  // Calculate relative size for text scaling
  const nodeWidth = $derived(child.x1 - child.x0);
  const nodeHeight = $derived(child.y1 - child.y0);
  const nodeSizeRatio = $derived(Math.min(nodeWidth, nodeHeight) * 100); // Size as percentage of parent
  const fontSize = $derived(Math.max(0.5, Math.min(2, nodeSizeRatio * 0.1))); // Scale font size (0.5-2rem)

  // Determine if this is the only child (occupies 100% of the space)
  const isOnlyChild = $derived(nodeWidth >= 0.999 && nodeHeight >= 0.999);

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

  // Handle text edit start
  function handleTextEditStart(event: MouseEvent | TouchEvent) {
    // Prevent the click from bubbling up to the parent node
    event.stopPropagation();
    event.preventDefault();

    // Only allow editing if we have a valid node ID
    const nodeId = child.data?._key;
    if (!nodeId) return;

    // Set up edit state
    isEditing = true;
    editValue = child.data?.name || "";
  }

  // Handle mobile editing via prompt
  function showMobileEditPrompt() {
    // Create a mobile-friendly dialog with Tailwind classes
    const modal = document.createElement("div");
    modal.className =
      "fixed inset-0 flex items-center justify-center z-50 bg-black bg-opacity-50";

    const modalContent = document.createElement("div");
    modalContent.className =
      "bg-white rounded-lg shadow-lg p-5 w-4/5 max-w-sm mx-auto";

    const label = document.createElement("div");
    label.className = "text-lg font-bold mb-3 text-gray-800";
    label.textContent = "Edit Node Name";

    const input = document.createElement("input");
    input.className =
      "w-full p-2 mb-4 border border-gray-300 rounded focus:outline-none focus:ring-2 focus:ring-blue-500";
    input.value = editValue;

    const buttonContainer = document.createElement("div");
    buttonContainer.className = "flex justify-between";

    const cancelButton = document.createElement("button");
    cancelButton.className =
      "px-4 py-2 bg-gray-200 text-gray-800 rounded hover:bg-gray-300 focus:outline-none";
    cancelButton.textContent = "Cancel";

    const saveButton = document.createElement("button");
    saveButton.className =
      "px-4 py-2 bg-blue-500 text-white rounded hover:bg-blue-600 focus:outline-none";
    saveButton.textContent = "Save";

    buttonContainer.appendChild(cancelButton);
    buttonContainer.appendChild(saveButton);

    modalContent.appendChild(label);
    modalContent.appendChild(input);
    modalContent.appendChild(buttonContainer);
    modal.appendChild(modalContent);
    document.body.appendChild(modal);

    // Focus the input
    setTimeout(() => input.focus(), 100);

    // Handle save action
    const saveEdit = () => {
      const newName = input.value.trim();
      document.body.removeChild(modal);
      if (newName && newName !== name) {
        saveTextEdit(newName);
      }
      isEditing = false;
    };

    // Handle cancel
    const cancelEdit = () => {
      document.body.removeChild(modal);
      isEditing = false;
    };

    // Set up event listeners
    saveButton.addEventListener("click", saveEdit);
    cancelButton.addEventListener("click", cancelEdit);
    modal.addEventListener("click", (e) => {
      if (e.target === modal) cancelEdit();
    });

    // Handle enter key
    input.addEventListener("keydown", (e) => {
      if (e.key === "Enter") {
        e.preventDefault();
        saveEdit();
      } else if (e.key === "Escape") {
        e.preventDefault();
        cancelEdit();
      }
    });
  }

  // Handle text edit save
  function saveTextEdit(newName: string) {
    const nodeId = child.data?._key;
    if (!nodeId) return;

    onTextEdit({ nodeId, newName });
  }

  // Handle keyboard events for the input field
  function handleEditKeydown(event: KeyboardEvent) {
    if (event.key === "Enter") {
      event.preventDefault();
      finishEditing();
    } else if (event.key === "Escape") {
      event.preventDefault();
      isEditing = false;
    }
  }

  // Finish editing and save the result
  function finishEditing() {
    const newName = editValue.trim();
    if (newName && newName !== name) {
      saveTextEdit(newName);
    }
    isEditing = false;
  }

  // Handle click outside to finish editing
  function handleClickOutside(event: MouseEvent) {
    // Only if we're editing
    if (isEditing) {
      // For desktop view, check if the click is outside the input field
      // For mobile view, modal handles its own click outside
      if (editInput && !editInput.contains(event.target as Node)) {
        finishEditing();
      }
    }
  }

  // Set up and clean up event listeners when editing state changes
  $effect(() => {
    if (isEditing) {
      // Add global event listener for clicks outside
      document.addEventListener("mousedown", handleClickOutside);

      // Focus the input when it's available (for desktop view)
      if (editInput) {
        setTimeout(() => {
          editInput?.focus();
          editInput?.select();
        }, 10);
      } else {
        // Show mobile popup for small screens
        showMobileEditPrompt();
      }
    } else {
      // Remove the event listener when not editing
      document.removeEventListener("mousedown", handleClickOutside);
    }

    // Clean up function
    return () => {
      document.removeEventListener("mousedown", handleClickOutside);
    };
  });

  // Check if this node should enter edit mode (either from prop or from being newly created)
  $effect(() => {
    if (shouldEdit && !isEditing && child.data?._key) {
      // Start editing automatically
      isEditing = true;
      editValue = child.data?.name || "";
    }
  });
</script>

<div
  class="treemap-node"
  style="
    background-color: {getColorForName(child.data?.name || '')};
    border: {isOnlyChild
    ? 'none'
    : child.data?.hasDirectContributionChild
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
      padding: {isOnlyChild ? '2%' : '5%'};
      box-sizing: border-box;
    "
  >
    <div class="node-body">
      <!-- Node text with responsive editing based on screen size -->
      <!-- Uses Tailwind's md: breakpoint to show in-place editing on larger screens while smaller screens use modal -->
      {#if isEditing}
        <div
          class="hidden md:block node-text-edit-container"
          style="font-size: {fontSize}rem;"
        >
          <input
            type="text"
            class="node-text-edit-input bg-white/90 border border-gray-300 rounded px-2 py-1 text-center shadow-sm focus:outline-none focus:ring-2 focus:ring-blue-500"
            bind:this={editInput}
            bind:value={editValue}
            onkeydown={handleEditKeydown}
            onblur={finishEditing}
            style="
              font-size: inherit;
              width: 100%;
              max-width: {Math.min(200, nodeSizeRatio * 3)}px;
            "
          />
        </div>
      {:else}
        <div
          class="node-text edit-text-field cursor-text"
          style="
            user-select: none;
            display: flex;
            flex-direction: column;
            align-items: center;
            font-size: {fontSize}rem;
          "
          title={name}
          onclick={handleTextEditStart}
        >
          {#each segments as segment, i}
            <span class="text-segment">
              {segment}
            </span>
          {/each}
        </div>
      {/if}

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

  .node-text-edit-container {
    display: flex;
    justify-content: center;
    align-items: center;
    z-index: 100;
  }

  .node-text-edit-input {
    background: rgba(255, 255, 255, 0.9);
    border: 1px solid rgba(0, 0, 0, 0.2);
    border-radius: 4px;
    padding: 4px;
    text-align: center;
    box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1);
    color: #333;
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
