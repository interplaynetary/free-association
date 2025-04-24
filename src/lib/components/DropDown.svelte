<script lang="ts">
  import { onMount, onDestroy } from "svelte";
  import { getColorForUserId } from "../utils/colorUtils";
  import {
    loadUsers,
    updateUserProfile,
    usersMap,
    onUserMapChange,
  } from "../utils/userUtils";
  import { derived, writable, readable, get } from "svelte/store";
  import { transientGun } from "../utils/gun/gunSetup";

  // Props using Svelte 5 runes
  let {
    title = "Select User",
    searchPlaceholder = "Search users...",
    position = { x: 0, y: 0 },
    width = 280,
    maxHeight = 320,
    excludeIds = [],
    filterText = "",
    rootId = undefined,
    show = false,
    select = (detail: { id: string; name: string }) => {},
    close = () => {},
  } = $props<{
    title?: string;
    searchPlaceholder?: string;
    position?: { x: number; y: number };
    width?: number;
    maxHeight?: number;
    excludeIds?: string[];
    filterText?: string;
    rootId?: string | undefined;
    show?: boolean;
    select?: (detail: { id: string; name: string }) => void;
    close?: () => void;
  }>();

  // State
  let dropdownContainer = $state<HTMLDivElement | null>(null);
  let searchInput = $state<HTMLInputElement | null>(null);
  let resultsContainer = $state<HTMLDivElement | null>(null);
  let clearSubscriptions = $state<() => void>(() => {});
  let initialized = $state(false);
  let heartbeatInterval = $state<ReturnType<typeof setInterval> | null>(null);

  // Create reactive stores
  const currentFilterTextStore = writable<string>(filterText);
  const usersStore = writable<
    Array<{ id: string; name: string; online?: boolean }>
  >([]);
  const loadingStore = writable<boolean>(true);

  // Add debug store
  const debugInfo = writable({
    mapSize: 0,
    lastUpdateTime: new Date().toISOString(),
  });

  // Effect to update filter text when prop changes
  $effect(() => {
    currentFilterTextStore.set(filterText);
  });

  // Setup heartbeat for online status
  function setupHeartbeat() {
    if (rootId) {
      // Update lastSeen every 30 seconds to indicate user is online
      heartbeatInterval = setInterval(() => {
        transientGun.get("users").get(rootId).put({
          online: true,
          lastSeen: Date.now(),
        });
      }, 30000);
    }
  }

  // Setup reactive user loading system
  function setupUserLoading() {
    // Ensure we clean up previous subscriptions
    clearSubscriptions();

    // Update loading state
    loadingStore.set(true);

    console.log("Setting up user loading in dropdown", {
      filterText: get(currentFilterTextStore),
      excludeIds,
      rootId,
    });

    // Start heartbeat
    setupHeartbeat();

    // Create options for loadUsers
    const options = {
      filterText: get(currentFilterTextStore),
      excludeIds,
      rootId,
    };

    // Set up user loading with reactivity
    const updateUsersCallback = (
      users: Array<{ id: string; name: string }>,
    ) => {
      console.log(
        `Dropdown received ${users.length} users from loadUsers callback`,
      );

      // Check online status for each user
      const enhancedUsers = users.map((user) => ({ ...user, online: false }));
      usersStore.set(enhancedUsers);

      // Load online status
      enhancedUsers.forEach((user, index) => {
        transientGun
          .get("users")
          .get(user.id)
          .once((userData) => {
            if (userData && userData.online === true) {
              usersStore.update((currentUsers) => {
                const updatedUsers = [...currentUsers];
                updatedUsers[index] = { ...updatedUsers[index], online: true };
                return updatedUsers;
              });
            }
          });
      });

      loadingStore.set(false);

      // Update debug info
      debugInfo.update((info) => ({
        ...info,
        mapSize: usersMap.size,
        lastUpdateTime: new Date().toISOString(),
      }));
    };

    // Start the loading process
    const cleanup = loadUsers(updateUsersCallback, options);

    // Subscribe to filter text changes to update results
    const filterSub = currentFilterTextStore.subscribe((newFilter) => {
      // Update loading state
      loadingStore.set(true);

      // Clean up previous subscription
      cleanup();

      // Create new options with updated filter
      const newOptions = {
        filterText: newFilter,
        excludeIds,
        rootId,
      };

      // Start new loading process
      clearSubscriptions = loadUsers(updateUsersCallback, newOptions);
    });

    // Return a cleanup function that handles all subscriptions
    clearSubscriptions = () => {
      cleanup();
      filterSub();
      if (heartbeatInterval) {
        clearInterval(heartbeatInterval);
      }
    };

    // Mark as initialized
    initialized = true;
  }

  // Event handlers
  function handleClose() {
    // Cleanup subscriptions
    clearSubscriptions();

    close();
    show = false;
  }

  function handleSelect(id: string, name: string) {
    select({ id, name });
    handleClose();
  }

  function updateSearchFilter(text: string) {
    currentFilterTextStore.set(text);
  }

  // Initialize the component when shown
  function initialize() {
    if (!initialized && show) {
      setupUserLoading();

      // Focus search input
      if (searchInput) {
        setTimeout(() => searchInput?.focus(), 50);
      }

      adjustPosition();
    }
  }

  // Adjust position to stay in viewport
  function adjustPosition() {
    if (!dropdownContainer) return;

    const rect = dropdownContainer.getBoundingClientRect();
    const viewportWidth = window.innerWidth;
    const viewportHeight = window.innerHeight;

    // Check right edge
    if (position.x + rect.width > viewportWidth - 10) {
      position.x = Math.max(10, viewportWidth - rect.width - 10);
    }

    // Check left edge
    if (position.x < 10) {
      position.x = 10;
    }

    // Check bottom edge
    if (position.y + rect.height > viewportHeight - 10) {
      // If dropdown would go above the top, position it at the top with padding
      if (position.y - rect.height < 10) {
        position.y = 10;
      } else {
        // Otherwise, position above the click
        position.y = position.y - rect.height - 10;
      }
    }
  }

  // Setup click outside listener
  function setupClickOutside() {
    const handleClickOutside = (event: MouseEvent) => {
      if (
        dropdownContainer &&
        !dropdownContainer.contains(event.target as Node)
      ) {
        handleClose();
      }
    };

    document.addEventListener("click", handleClickOutside);
    return () => {
      document.removeEventListener("click", handleClickOutside);
    };
  }

  // Lifecycle
  onMount(() => {
    initialize();

    // Setup click outside handler
    if (show) {
      return setupClickOutside();
    }
  });

  // Effect to run initialize when show changes
  $effect(() => {
    if (show && !initialized) {
      initialize();
    }
  });

  // Effect to adjust position when position or show changes
  $effect(() => {
    if (show && position && dropdownContainer) {
      setTimeout(adjustPosition, 0);
    }
  });

  // Clean up when component is destroyed
  onMount(() => {
    return () => {
      clearSubscriptions();
    };
  });
</script>

{#if show}
  <div
    class="dropdown-container"
    bind:this={dropdownContainer}
    style="
            top: {position.y}px; 
            left: {position.x}px; 
            width: {width}px; 
            max-height: {maxHeight}px;
        "
  >
    <div class="header">
      <input
        type="text"
        placeholder={searchPlaceholder}
        bind:this={searchInput}
        bind:value={$currentFilterTextStore}
        oninput={(e) => updateSearchFilter(e.currentTarget.value)}
      />
      <button class="close-button" onclick={handleClose}>×</button>
    </div>

    <div class="results" bind:this={resultsContainer}>
      {#if $loadingStore && $usersStore.length === 0}
        <div class="message">
          Loading users... <span class="debug"
            >({$debugInfo.mapSize} in cache)</span
          >
        </div>
      {:else if $usersStore.length === 0}
        <div class="message">
          {$currentFilterTextStore
            ? "No matching users found"
            : "No users available"}
          <span class="debug">({$debugInfo.mapSize} in cache)</span>
        </div>
      {:else}
        {#each $usersStore as item (item.id)}
          <div
            class="item"
            data-id={item.id}
            onclick={() => handleSelect(item.id, item.name)}
          >
            <div
              class="color-dot"
              style="background-color: {getColorForUserId(item.id)}"
            ></div>
            <div class="item-name">{item.name || item.id}</div>
            <div
              class="status-indicator"
              class:online={item.online}
              title={item.online ? "Online" : "Offline"}
            ></div>
          </div>
        {/each}
      {/if}
    </div>
  </div>
{/if}

<style>
  .dropdown-container {
    position: fixed;
    background: white;
    border-radius: 8px;
    overflow: hidden;
    display: flex;
    flex-direction: column;
    z-index: 99999;
    box-shadow:
      0 6px 16px rgba(0, 0, 0, 0.12),
      0 3px 6px rgba(0, 0, 0, 0.08);
    opacity: 0;
    animation: fadeIn 150ms forwards;
  }

  @keyframes fadeIn {
    from {
      opacity: 0;
    }
    to {
      opacity: 1;
    }
  }

  .header {
    display: flex;
    align-items: center;
    border-bottom: 1px solid #eee;
    padding: 8px;
    background-color: #f9f9f9;
  }

  input {
    width: 100%;
    padding: 8px 12px;
    border: none;
    border-radius: 4px;
    background: #ffffff;
    box-shadow: inset 0 0 0 1px #e0e0e0;
    flex: 1;
    outline: none;
    font-size: 14px;
    color: #333;
  }

  .close-button {
    padding: 6px 8px;
    margin-left: 8px;
    cursor: pointer;
    color: #666;
    font-weight: bold;
    font-size: 16px;
    line-height: 1;
    border-radius: 4px;
    background: transparent;
    border: none;
    transition: background-color 0.2s;
  }

  .close-button:hover {
    background-color: #f0f0f0;
  }

  .results {
    overflow-y: auto;
    overflow-x: hidden;
    flex: 1;
    max-height: calc(var(--max-height, 320px) - 56px);
    scrollbar-width: thin;
    scrollbar-color: #d0d0d0 #f5f5f5;
    -webkit-overflow-scrolling: touch;
  }

  .results::-webkit-scrollbar {
    width: 6px;
    height: 6px;
  }

  .results::-webkit-scrollbar-track {
    background: #f5f5f5;
  }

  .results::-webkit-scrollbar-thumb {
    background-color: #d0d0d0;
    border-radius: 3px;
  }

  .message {
    padding: 16px;
    text-align: center;
    color: #888;
    font-size: 13px;
  }

  .item {
    padding: 10px 12px;
    cursor: pointer;
    font-size: 14px;
    border-bottom: 1px solid #f0f0f0;
    display: flex;
    align-items: center;
    transition: background 0.2s;
  }

  .item:hover {
    background-color: #f5f7fa;
  }

  .color-dot {
    width: 10px;
    height: 10px;
    border-radius: 50%;
    margin-right: 8px;
  }

  .item-name {
    flex: 1;
    color: #333;
  }

  .status-indicator {
    width: 8px;
    height: 8px;
    border-radius: 50%;
    background-color: #d1d5da;
    margin-left: auto;
  }

  .status-indicator.online {
    background-color: #28a745;
  }

  .debug {
    font-size: 11px;
    color: #999;
    display: inline-block;
    margin-left: 4px;
  }
</style>
