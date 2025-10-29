<script lang="ts">
  import {createEventDispatcher} from "svelte"

  export let stats: any

  const dispatch = createEventDispatcher()

  function selectType(type: string) {
    dispatch("selectType", {type})
  }

  function formatNumber(num: number): string {
    if (num >= 1000000) return (num / 1000000).toFixed(1) + "M"
    if (num >= 1000) return (num / 1000).toFixed(1) + "K"
    return num.toString()
  }

  function getTypeIcon(type: string): string {
    const icons: Record<string, string> = {
      "rss-feed": "📰",
      twitter: "🐦",
      mastodon: "🦣",
      reddit: "🤖",
      webhook: "🔗",
      "iot-sensor": "📡",
      email: "📧",
      logs: "📝",
      "json-document": "📄",
    }
    return icons[type] || "🔄"
  }

  function getTypeLabel(type: string): string {
    const labels: Record<string, string> = {
      "rss-feed": "RSS Feeds",
      twitter: "Twitter",
      mastodon: "Mastodon",
      reddit: "Reddit",
      webhook: "Webhooks",
      "iot-sensor": "IoT Sensors",
      email: "Email",
      logs: "Logs",
      "json-document": "Documents",
    }
    return labels[type] || type
  }
</script>

<div class="stats-overview">
  <section class="totals">
    <h2>System Overview</h2>
    <div class="stat-cards">
      <div class="stat-card primary">
        <div class="stat-value">{formatNumber(stats.totals.totalRequests)}</div>
        <div class="stat-label">Total Requests</div>
      </div>
      <div class="stat-card success">
        <div class="stat-value">{formatNumber(stats.totals.totalProcessed)}</div>
        <div class="stat-label">Processed</div>
      </div>
      <div class="stat-card warning">
        <div class="stat-value">{formatNumber(stats.totals.totalDuplicates)}</div>
        <div class="stat-label">Duplicates</div>
      </div>
      <div class="stat-card info">
        <div class="stat-value">{formatNumber(stats.totals.totalUnchanged)}</div>
        <div class="stat-label">Unchanged</div>
      </div>
    </div>
  </section>

  <section class="relay-types">
    <h2>Relay Types ({stats.types.length})</h2>
    <div class="type-grid">
      {#each stats.types as type}
        {@const typeStats = stats.byType[type]}
        <button class="type-card" on:click={() => selectType(type)}>
          <div class="type-header">
            <span class="type-icon">{getTypeIcon(type)}</span>
            <span class="type-name">{getTypeLabel(type)}</span>
          </div>
          <div class="type-stats">
            <div class="mini-stat">
              <span class="mini-value">{formatNumber(typeStats.requests.totalRequests)}</span>
              <span class="mini-label">requests</span>
            </div>
            <div class="mini-stat">
              <span class="mini-value">{formatNumber(typeStats.processing.totalProcessed)}</span>
              <span class="mini-label">processed</span>
            </div>
            <div class="mini-stat">
              <span class="mini-value">{typeStats.caches.pending || 0}</span>
              <span class="mini-label">pending</span>
            </div>
          </div>
          <div class="type-footer">
            <span class="view-link">View Details →</span>
          </div>
        </button>
      {/each}
    </div>
  </section>
</div>

<style>
  .stats-overview {
    display: flex;
    flex-direction: column;
    gap: 3rem;
  }

  h2 {
    font-size: 1.5rem;
    font-weight: 600;
    color: #111827;
    margin: 0 0 1.5rem 0;
  }

  .stat-cards {
    display: grid;
    grid-template-columns: repeat(auto-fit, minmax(200px, 1fr));
    gap: 1.5rem;
  }

  .stat-card {
    background: white;
    border-radius: 12px;
    padding: 1.5rem;
    box-shadow: 0 1px 3px rgba(0, 0, 0, 0.1);
    border-left: 4px solid;
  }

  .stat-card.primary {
    border-left-color: #3b82f6;
  }

  .stat-card.success {
    border-left-color: #10b981;
  }

  .stat-card.warning {
    border-left-color: #f59e0b;
  }

  .stat-card.info {
    border-left-color: #8b5cf6;
  }

  .stat-value {
    font-size: 2.5rem;
    font-weight: 700;
    color: #111827;
    margin-bottom: 0.25rem;
  }

  .stat-label {
    font-size: 0.875rem;
    color: #6b7280;
    text-transform: uppercase;
    letter-spacing: 0.05em;
  }

  .type-grid {
    display: grid;
    grid-template-columns: repeat(auto-fill, minmax(280px, 1fr));
    gap: 1.5rem;
  }

  .type-card {
    background: white;
    border: 1px solid #e5e7eb;
    border-radius: 12px;
    padding: 1.5rem;
    cursor: pointer;
    transition: all 0.2s;
    text-align: left;
    width: 100%;
  }

  .type-card:hover {
    border-color: #3b82f6;
    box-shadow: 0 4px 12px rgba(59, 130, 246, 0.15);
    transform: translateY(-2px);
  }

  .type-header {
    display: flex;
    align-items: center;
    gap: 0.75rem;
    margin-bottom: 1rem;
  }

  .type-icon {
    font-size: 2rem;
  }

  .type-name {
    font-size: 1.125rem;
    font-weight: 600;
    color: #111827;
  }

  .type-stats {
    display: flex;
    gap: 1rem;
    margin-bottom: 1rem;
    padding: 0.75rem 0;
    border-top: 1px solid #f3f4f6;
    border-bottom: 1px solid #f3f4f6;
  }

  .mini-stat {
    display: flex;
    flex-direction: column;
    gap: 0.25rem;
  }

  .mini-value {
    font-size: 1.25rem;
    font-weight: 600;
    color: #111827;
  }

  .mini-label {
    font-size: 0.75rem;
    color: #9ca3af;
  }

  .type-footer {
    display: flex;
    justify-content: flex-end;
  }

  .view-link {
    color: #3b82f6;
    font-size: 0.875rem;
    font-weight: 500;
  }

  .type-card:hover .view-link {
    text-decoration: underline;
  }
</style>

