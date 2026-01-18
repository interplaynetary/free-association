<script lang="ts">
  import { onMount } from 'svelte';

  let registrations: ServiceWorkerRegistration[] = $state([]);
  let logs: string[] = $state([]);
  let loading = $state(true);

  function log(msg: string) {
    logs = [...logs, `${new Date().toLocaleTimeString()} - ${msg}`];
    console.log(`[Reset-SW] ${msg}`);
  }

  async function loadRegistrations() {
    try {
      if (!navigator.serviceWorker) {
        log('Service Workers are not supported in this browser.');
        return;
      }
      registrations = [...(await navigator.serviceWorker.getRegistrations())];
      log(`Found ${registrations.length} active registration(s).`);
    } catch (e) {
      log(`Error loading registrations: ${e}`);
    } finally {
      loading = false;
    }
  }

  async function nuke() {
    if (!confirm('This will unregister ALL service workers and clear ALL caches. Continue?')) return;
    
    log('Starting nuclear cleanup sequence...');

    // 1. Unregister all service workers
    for (const reg of registrations) {
      try {
        const success = await reg.unregister();
        log(`Unregistering ${reg.scope} (Script: ${reg.active?.scriptURL || 'unknown'})... ${success ? 'Success' : 'Failed'}`);
      } catch (e) {
        log(`Error unregistering ${reg.scope}: ${e}`);
      }
    }

    // 2. Clear all caches
    if (window.caches) {
      try {
        const keys = await caches.keys();
        log(`Found ${keys.length} cache(s). Deleting...`);
        await Promise.all(keys.map(async key => {
          const success = await caches.delete(key);
          log(`Deleting cache '${key}'... ${success ? 'Success' : 'Failed'}`);
        }));
      } catch (e) {
        log(`Error clearing caches: ${e}`);
      }
    } else {
        log('Cache API not supported.');
    }

    log('Cleanup finished. You should reload the application now.');
    
    // Refresh list
    await loadRegistrations();
    
    // Suggest reload
    if (confirm('Cleanup complete. Reload page now?')) {
        window.location.reload();
    }
  }

  onMount(() => {
    loadRegistrations();
  });
</script>

<div class="container">
  <h1>Service Worker Reset Tool</h1>
  
  <div class="card">
    <h2>Active Registrations</h2>
    {#if loading}
      <p>Loading...</p>
    {:else if registrations.length === 0}
      <p class="success">No active service workers found. You are clean!</p>
    {:else}
      <ul class="list">
        {#each registrations as reg}
          <li class="item">
            <div>
                <strong>Scope:</strong> {reg.scope}<br>
                <strong>Script:</strong> {reg.active?.scriptURL || reg.installing?.scriptURL || reg.waiting?.scriptURL || 'Unknown'}
            </div>
            <div class="status">
                {#if reg.active} <span class="badge active">Active</span> {/if}
                {#if reg.waiting} <span class="badge waiting">Waiting</span> {/if}
                {#if reg.installing} <span class="badge installing">Installing</span> {/if}
            </div>
          </li>
        {/each}
      </ul>
      
      <button onclick={nuke} class="nuke-btn">
        ⚠️ Unregister All & Clear Caches
      </button>
    {/if}
  </div>

  <div class="card logs">
    <h2>Logs</h2>
    <div class="log-output">
      {#each logs as l}
        <div>{l}</div>
      {/each}
    </div>
  </div>
    
  <div class="links">
      <a href="/">Return to App</a>
  </div>
</div>

<style>
  .container {
    max-width: 600px;
    margin: 40px auto;
    padding: 20px;
    font-family: system-ui, -apple-system, sans-serif;
  }
  h1 {
    color: #333;
    text-align: center;
  }
  .card {
    background: #fff;
    border: 1px solid #ddd;
    border-radius: 8px;
    padding: 20px;
    margin-bottom: 20px;
    box-shadow: 0 2px 4px rgba(0,0,0,0.05);
  }
  .success {
    color: green;
    font-weight: bold;
    text-align: center;
  }
  .list {
    list-style: none;
    padding: 0;
    margin: 0 0 20px 0;
  }
  .item {
    padding: 10px;
    border-bottom: 1px solid #eee;
    font-family: monospace;
    font-size: 0.9em;
    word-break: break-all;
  }
  .item:last-child {
    border-bottom: none;
  }
  .nuke-btn {
    width: 100%;
    padding: 12px;
    background: #d32f2f;
    color: white;
    border: none;
    border-radius: 4px;
    font-size: 1rem;
    cursor: pointer;
    font-weight: bold;
  }
  .nuke-btn:hover {
    background: #b71c1c;
  }
  .logs {
    background: #f5f5f5;
  }
  .log-output {
    font-family: monospace;
    font-size: 0.85em;
    color: #555;
    max-height: 300px;
    overflow-y: auto;
  }
  .badge {
      font-size: 0.7em;
      padding: 2px 6px;
      border-radius: 4px;
      color: white;
      margin-left: 5px;
  }
  .active { background: #2e7d32; }
  .waiting { background: #f57c00; }
  .installing { background: #0288d1; }
  .links { text-align: center; margin-top: 20px; }
  a { color: #007bff; text-decoration: none; }
  a:hover { text-decoration: underline; }
</style>
