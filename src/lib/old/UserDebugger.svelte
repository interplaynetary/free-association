<script lang="ts">
  import { onMount, onDestroy } from "svelte";
  import {
    usersMap,
    onUserMapChange,
    updateUserProfile,
  } from "../../utils/userUtils";
  import { transientGun } from "../../utils/gun/gunSetup";
  import { writable } from "svelte/store";

  // State
  let mapSize = 0;
  let usersList: Array<{ id: string; name: string }> = [];
  let cleanup: () => void;

  // Create a store for reactive updates
  const infoStore = writable({
    mapSize: 0,
    users: [] as Array<{ id: string; name: string; online?: boolean }>,
  });

  // Function to update info
  function updateInfo() {
    // Convert map to array
    const users = Array.from(usersMap.entries())
      .map(([id, name]) => ({ id, name }))
      .filter((user) => user.name !== user.id) // Filter out unresolved
      .sort((a, b) => a.name.localeCompare(b.name));

    infoStore.set({
      mapSize: usersMap.size,
      users,
    });
  }

  // Create test users directly in the transient users list
  function createTestUser() {
    const userId = `user_${Date.now()}_${Math.floor(Math.random() * 1000)}`;
    const name = `Test User ${Math.floor(Math.random() * 1000)}`;

    console.log(`Creating test user: ${name} (${userId})`);

    updateUserProfile(userId, name);
  }

  // Simulate user coming online/offline
  function toggleUserStatus(userId: string, online: boolean) {
    transientGun.get("users").get(userId).put({
      online: online,
      lastSeen: Date.now(),
    });
  }

  // Clear user cache (for testing)
  function handleClearCache() {
    // Clear only user map, not Gun data
    usersMap.clear();
    updateInfo();
  }

  // Copy debug info to clipboard
  function handleCopyDebugInfo() {
    const debugInfo = {
      mapSize: usersMap.size,
      users: Array.from(usersMap.entries()).map(([id, name]) => ({ id, name })),
    };

    navigator.clipboard
      .writeText(JSON.stringify(debugInfo, null, 2))
      .then(() => alert("Debug info copied to clipboard!"))
      .catch((err) => console.error("Failed to copy debug info:", err));
  }

  // Load online status for users
  function loadOnlineStatus() {
    const usersRef = transientGun.get("users");

    // For each user in our list, check their online status
    infoStore.update((info) => {
      const updatedUsers = [...info.users];

      updatedUsers.forEach((user, index) => {
        usersRef.get(user.id).once((data) => {
          if (data) {
            updatedUsers[index] = {
              ...user,
              online: data.online === true,
            };

            // Update the store
            infoStore.update((current) => ({
              ...current,
              users: updatedUsers,
            }));
          }
        });
      });

      return info;
    });
  }

  onMount(() => {
    // Set up listener for map changes
    cleanup = onUserMapChange(() => {
      updateInfo();
      loadOnlineStatus();
    });

    // Initial update
    updateInfo();
    loadOnlineStatus();
  });

  onDestroy(() => {
    if (cleanup) cleanup();
  });
</script>

<div class="debugger">
  <h3>User Dropdown Debugger</h3>

  <div class="stats">
    <p>Users in cache: <strong>{$infoStore.mapSize}</strong></p>
    <p>Resolved users: <strong>{$infoStore.users.length}</strong></p>
  </div>

  <div class="actions">
    <button onclick={createTestUser}> Create Test User </button>
    <button onclick={handleClearCache}> Clear Cache </button>
    <button onclick={handleCopyDebugInfo}> Copy Debug Info </button>
  </div>

  <div class="user-list">
    <h4>Cached Users</h4>
    {#if $infoStore.users.length === 0}
      <p class="empty">No users in cache</p>
    {:else}
      <ul>
        {#each $infoStore.users as user}
          <li>
            <div class="user-info">
              <strong>{user.name}</strong>
              <span class="user-id">{user.id}</span>
            </div>
            <div class="user-actions">
              <span
                class="status-dot"
                class:online={user.online}
                title={user.online ? "Online" : "Offline"}
              ></span>
              <button
                onclick={() => toggleUserStatus(user.id, true)}
                class="small-btn">Set Online</button
              >
              <button
                onclick={() => toggleUserStatus(user.id, false)}
                class="small-btn">Set Offline</button
              >
            </div>
          </li>
        {/each}
      </ul>
    {/if}
  </div>
</div>

<style>
  .debugger {
    background: #f7f9fc;
    border: 1px solid #e1e4e8;
    border-radius: 6px;
    padding: 16px;
    margin: 20px 0;
    max-width: 800px;
  }

  h3 {
    margin-top: 0;
    color: #24292e;
    font-size: 18px;
    margin-bottom: 12px;
  }

  h4 {
    margin-top: 16px;
    font-size: 16px;
    color: #24292e;
  }

  .stats {
    display: flex;
    gap: 16px;
    margin-bottom: 16px;
  }

  .stats p {
    margin: 0;
    color: #586069;
  }

  .actions {
    display: flex;
    flex-wrap: wrap;
    gap: 8px;
    margin-bottom: 16px;
  }

  button {
    background: #f1f2f3;
    border: 1px solid #d1d5da;
    border-radius: 4px;
    padding: 6px 12px;
    font-size: 14px;
    cursor: pointer;
    transition: background-color 0.2s;
  }

  button:hover {
    background: #e1e4e8;
  }

  .small-btn {
    padding: 2px 6px;
    font-size: 12px;
    margin-left: 4px;
  }

  .user-list {
    margin-top: 16px;
  }

  ul {
    list-style: none;
    padding: 0;
    margin: 0;
    max-height: 300px;
    overflow-y: auto;
    border: 1px solid #e1e4e8;
    border-radius: 4px;
  }

  li {
    padding: 8px 12px;
    border-bottom: 1px solid #e1e4e8;
    display: flex;
    justify-content: space-between;
    align-items: center;
  }

  li:last-child {
    border-bottom: none;
  }

  .user-id {
    font-size: 12px;
    color: #6a737d;
    display: block;
    margin-top: 2px;
  }

  .empty {
    color: #6a737d;
    font-style: italic;
    text-align: center;
    padding: 12px;
  }

  .user-info {
    display: flex;
    flex-direction: column;
  }

  .user-actions {
    display: flex;
    align-items: center;
  }

  .status-dot {
    width: 10px;
    height: 10px;
    border-radius: 50%;
    background-color: #d73a49;
    margin-right: 8px;
  }

  .status-dot.online {
    background-color: #28a745;
  }
</style>
