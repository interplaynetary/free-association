<script lang="ts">
  import { onMount, onDestroy } from "svelte";
  import { getColorForUserId } from "../utils/colorUtils";
  import {
    getUserName,
    onUserNameResolved,
    usersMap,
  } from "../utils/userUtils";

  // Props
  export let userId: string;
  export let truncateLength: number = 10;
  export let removable: boolean = true;

  // Callback props for Svelte 5
  export let onClick: (userId: string) => void = () => {};
  export let onRemove: (userId: string) => void = () => {};

  // State
  let displayName: string = userId;
  let cleanupNameSub: () => void = () => {};

  // Element reference
  let pillElement: HTMLDivElement;

  // Initialize and cleanup
  onMount(() => {
    // Set initial display name
    displayName = getUserName(userId);
    updateTooltip(displayName);

    // Set up name resolution subscription
    cleanupNameSub = onUserNameResolved(userId, (id, resolvedName) => {
      displayName = resolvedName;
      updateTooltip(resolvedName);
    });
  });

  onDestroy(() => {
    if (cleanupNameSub) {
      cleanupNameSub();
    }
  });

  // Function to truncate and format display name
  function getFormattedName(name: string): string {
    return name.length > truncateLength
      ? name.substring(0, truncateLength - 2) + "..."
      : name;
  }

  // Update tooltip text
  function updateTooltip(name: string): void {
    if (pillElement) {
      pillElement.title = removable
        ? `${name}: Click to view tree, click × to remove`
        : `${name}: Click to view tree`;
    }
  }

  // Handle pill click
  function handlePillClick(event: MouseEvent): void {
    // Don't trigger if clicked on remove button
    if ((event.target as HTMLElement).classList.contains("remove-tag")) {
      return;
    }

    onClick(userId);
  }

  // Handle remove button click
  function handleRemoveClick(event: MouseEvent): void {
    event.stopPropagation();
    onRemove(userId);
  }

  // Get the current full name (for logging or debugging)
  export function getFullName(): string {
    return usersMap.has(userId) ? usersMap.get(userId)! : userId;
  }
</script>

<div
  bind:this={pillElement}
  class="tag-pill"
  data-user-id={userId}
  style="background: {getColorForUserId(userId)}"
  on:click={handlePillClick}
>
  <span>{getFormattedName(displayName)}</span>

  {#if removable}
    <span class="remove-tag" on:click={handleRemoveClick}>×</span>
  {/if}
</div>

<style>
  .tag-pill {
    display: flex;
    align-items: center;
    border-radius: 12px;
    padding: 3px 10px;
    margin: 2px;
    height: 22px;
    font-size: 12px;
    white-space: nowrap;
    box-shadow: 0 1px 3px rgba(0, 0, 0, 0.1);
    border: 1px solid rgba(255, 255, 255, 0.2);
    opacity: 0.9;
    cursor: pointer;
    transition: all 0.2s ease;
  }

  .tag-pill:hover {
    opacity: 1;
    transform: translateY(-1px);
    box-shadow: 0 2px 4px rgba(0, 0, 0, 0.2);
  }

  .tag-pill span {
    color: #000;
    margin-right: 5px;
    font-weight: 500;
    text-shadow: 0 1px 0 rgba(255, 255, 255, 0.4);
  }

  .remove-tag {
    color: #000;
    font-size: 14px;
    line-height: 14px;
    opacity: 0.7;
    font-weight: bold;
    margin-left: 2px;
    display: flex;
    align-items: center;
    justify-content: center;
    width: 16px;
    height: 16px;
    border-radius: 50%;
    background: rgba(255, 255, 255, 0.4);
    cursor: pointer;
    transition: all 0.2s ease;
  }

  .remove-tag:hover {
    opacity: 1;
    background: rgba(255, 255, 255, 0.7);
  }
</style>
