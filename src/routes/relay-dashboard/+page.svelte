<script lang="ts">
  import {onMount} from "svelte"
  import StatsOverview from "./StatsOverview.svelte"
  import TypeSelector from "./TypeSelector.svelte"
  import DataViewer from "./DataViewer.svelte"

  let stats: any = null
  let selectedType: string | null = null
  let loading = true
  let error: string | null = null
  let refreshInterval: ReturnType<typeof setInterval> | null = null

  async function loadStats() {
    try {
      const response = await fetch("/api/relay/stats", {
        credentials: "include",
      })
      if (!response.ok) {
        throw new Error(`HTTP ${response.status}`)
      }
      stats = await response.json()
      loading = false
    } catch (err) {
      error = err instanceof Error ? err.message : "Failed to load stats"
      loading = false
    }
  }

  function startAutoRefresh() {
    refreshInterval = setInterval(loadStats, 10000) // Every 10 seconds
  }

  function stopAutoRefresh() {
    if (refreshInterval) {
      clearInterval(refreshInterval)
      refreshInterval = null
    }
  }

  onMount(() => {
    loadStats()
    startAutoRefresh()

    return () => {
      stopAutoRefresh()
    }
  })

  function handleTypeSelect(event: CustomEvent<{type: string}>) {
    selectedType = event.detail.type
  }

  function handleBackToOverview() {
    selectedType = null
  }
</script>

<div class="relay-dashboard">
  <header>
    <h1>🔄 Data Relay Dashboard</h1>
    <p class="subtitle">Monitor and explore data from all relay sources</p>
  </header>

  {#if loading}
    <div class="loading">
      <div class="spinner"></div>
      <p>Loading relay statistics...</p>
    </div>
  {:else if error}
    <div class="error-box">
      <h3>⚠️ Error</h3>
      <p>{error}</p>
      <button on:click={loadStats}>Retry</button>
    </div>
  {:else if stats}
    {#if !selectedType}
      <StatsOverview {stats} on:selectType={handleTypeSelect} />
    {:else}
      <button class="back-btn" on:click={handleBackToOverview}>
        ← Back to Overview
      </button>
      <DataViewer type={selectedType} typeStats={stats.byType[selectedType]} />
    {/if}
  {/if}
</div>

<style>
  .relay-dashboard {
    max-width: 1400px;
    margin: 0 auto;
    padding: 2rem;
    font-family: system-ui, -apple-system, sans-serif;
  }

  header {
    margin-bottom: 3rem;
    border-bottom: 2px solid #e5e7eb;
    padding-bottom: 1rem;
  }

  h1 {
    font-size: 2.5rem;
    font-weight: 700;
    color: #111827;
    margin: 0 0 0.5rem 0;
  }

  .subtitle {
    font-size: 1.125rem;
    color: #6b7280;
    margin: 0;
  }

  .loading {
    display: flex;
    flex-direction: column;
    align-items: center;
    gap: 1rem;
    padding: 4rem 0;
  }

  .spinner {
    width: 48px;
    height: 48px;
    border: 4px solid #e5e7eb;
    border-top-color: #3b82f6;
    border-radius: 50%;
    animation: spin 0.8s linear infinite;
  }

  @keyframes spin {
    to {
      transform: rotate(360deg);
    }
  }

  .error-box {
    background: #fee2e2;
    border: 1px solid #ef4444;
    border-radius: 8px;
    padding: 1.5rem;
    margin: 2rem 0;
  }

  .error-box h3 {
    color: #991b1b;
    margin: 0 0 0.5rem 0;
  }

  .error-box p {
    color: #7f1d1d;
    margin: 0 0 1rem 0;
  }

  .error-box button {
    background: #ef4444;
    color: white;
    border: none;
    padding: 0.5rem 1rem;
    border-radius: 6px;
    cursor: pointer;
    font-weight: 500;
  }

  .error-box button:hover {
    background: #dc2626;
  }

  .back-btn {
    background: #f3f4f6;
    border: 1px solid #d1d5db;
    padding: 0.5rem 1rem;
    border-radius: 6px;
    cursor: pointer;
    font-size: 0.875rem;
    font-weight: 500;
    margin-bottom: 1.5rem;
  }

  .back-btn:hover {
    background: #e5e7eb;
  }
</style>

