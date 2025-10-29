<script lang="ts">
  import {onMount} from "svelte"
  import GenericItem from "./GenericItem.svelte"

  export let type: string
  export let typeStats: any

  let items: any[] = []
  let loading = true
  let error: string | null = null
  let limit = 20
  let offset = 0
  let hasMore = false

  async function loadData() {
    loading = true
    error = null

    try {
      const response = await fetch(
        `/api/relay/${type}/query?limit=${limit}&offset=${offset}`,
        {
          credentials: "include",
        },
      )

      if (!response.ok) {
        throw new Error(`HTTP ${response.status}`)
      }

      const data = await response.json()
      items = data.items
      hasMore = data.hasMore
      loading = false
    } catch (err) {
      error = err instanceof Error ? err.message : "Failed to load data"
      loading = false
    }
  }

  function loadMore() {
    offset += limit
    loadData()
  }

  function reset() {
    offset = 0
    loadData()
  }

  onMount(() => {
    loadData()
  })

  function formatTimestamp(ts: number): string {
    return new Date(ts).toLocaleString()
  }
</script>

<div class="data-viewer">
  <header class="viewer-header">
    <div>
      <h2>📊 {type} Data</h2>
      <p class="subtitle">Viewing stored items from {type} relay</p>
    </div>
    <button class="refresh-btn" on:click={reset}>
      🔄 Refresh
    </button>
  </header>

  <section class="stats-bar">
    <div class="stat-item">
      <span class="stat-label">Total Requests:</span>
      <span class="stat-value">{typeStats.requests.totalRequests.toLocaleString()}</span>
    </div>
    <div class="stat-item">
      <span class="stat-label">Processed:</span>
      <span class="stat-value">{typeStats.processing.totalProcessed.toLocaleString()}</span>
    </div>
    <div class="stat-item">
      <span class="stat-label">Avg Processing Time:</span>
      <span class="stat-value">{Math.round(typeStats.processing.averageProcessingTime)}ms</span>
    </div>
    <div class="stat-item">
      <span class="stat-label">Pending:</span>
      <span class="stat-value">{typeStats.caches.pending || 0}</span>
    </div>
    <div class="stat-item">
      <span class="stat-label">Cached:</span>
      <span class="stat-value">{typeStats.caches.contentHash || 0}</span>
    </div>
  </section>

  {#if loading && items.length === 0}
    <div class="loading-state">
      <div class="spinner"></div>
      <p>Loading {type} data...</p>
    </div>
  {:else if error}
    <div class="error-state">
      <p>⚠️ {error}</p>
      <button on:click={reset}>Try Again</button>
    </div>
  {:else if items.length === 0}
    <div class="empty-state">
      <p>📭 No data found for {type}</p>
      <p class="empty-hint">Items will appear here once they're relayed</p>
    </div>
  {:else}
    <div class="items-grid">
      {#each items as item (item._key)}
        <GenericItem {item} {type} />
      {/each}
    </div>

    {#if hasMore}
      <div class="load-more">
        <button class="load-more-btn" on:click={loadMore} disabled={loading}>
          {loading ? "Loading..." : "Load More"}
        </button>
      </div>
    {/if}
  {/if}
</div>

<style>
  .data-viewer {
    display: flex;
    flex-direction: column;
    gap: 1.5rem;
  }

  .viewer-header {
    display: flex;
    justify-content: space-between;
    align-items: flex-start;
    padding-bottom: 1rem;
    border-bottom: 2px solid #e5e7eb;
  }

  h2 {
    font-size: 1.75rem;
    font-weight: 600;
    color: #111827;
    margin: 0 0 0.5rem 0;
  }

  .subtitle {
    font-size: 0.875rem;
    color: #6b7280;
    margin: 0;
  }

  .refresh-btn {
    background: #3b82f6;
    color: white;
    border: none;
    padding: 0.5rem 1rem;
    border-radius: 6px;
    cursor: pointer;
    font-size: 0.875rem;
    font-weight: 500;
    transition: background 0.2s;
  }

  .refresh-btn:hover {
    background: #2563eb;
  }

  .stats-bar {
    display: flex;
    flex-wrap: wrap;
    gap: 1.5rem;
    background: #f9fafb;
    padding: 1rem 1.5rem;
    border-radius: 8px;
  }

  .stat-item {
    display: flex;
    flex-direction: column;
    gap: 0.25rem;
  }

  .stat-label {
    font-size: 0.75rem;
    color: #6b7280;
    font-weight: 500;
  }

  .stat-value {
    font-size: 1.25rem;
    font-weight: 600;
    color: #111827;
  }

  .loading-state,
  .error-state,
  .empty-state {
    display: flex;
    flex-direction: column;
    align-items: center;
    justify-content: center;
    padding: 4rem 2rem;
    text-align: center;
  }

  .spinner {
    width: 48px;
    height: 48px;
    border: 4px solid #e5e7eb;
    border-top-color: #3b82f6;
    border-radius: 50%;
    animation: spin 0.8s linear infinite;
    margin-bottom: 1rem;
  }

  @keyframes spin {
    to {
      transform: rotate(360deg);
    }
  }

  .error-state p {
    color: #dc2626;
    font-weight: 500;
    margin-bottom: 1rem;
  }

  .error-state button {
    background: #ef4444;
    color: white;
    border: none;
    padding: 0.5rem 1.5rem;
    border-radius: 6px;
    cursor: pointer;
    font-weight: 500;
  }

  .empty-state p {
    color: #6b7280;
    font-size: 1.125rem;
    margin: 0.5rem 0;
  }

  .empty-hint {
    font-size: 0.875rem !important;
    color: #9ca3af !important;
  }

  .items-grid {
    display: grid;
    grid-template-columns: repeat(auto-fill, minmax(350px, 1fr));
    gap: 1.5rem;
  }

  .load-more {
    display: flex;
    justify-content: center;
    padding: 2rem 0;
  }

  .load-more-btn {
    background: white;
    border: 1px solid #d1d5db;
    padding: 0.75rem 2rem;
    border-radius: 8px;
    cursor: pointer;
    font-size: 1rem;
    font-weight: 500;
    transition: all 0.2s;
  }

  .load-more-btn:hover:not(:disabled) {
    border-color: #3b82f6;
    background: #eff6ff;
  }

  .load-more-btn:disabled {
    opacity: 0.5;
    cursor: not-allowed;
  }
</style>

