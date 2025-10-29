<script lang="ts">
  export let item: any
  export let type: string

  let expanded = false

  function formatTimestamp(ts: number): string {
    if (!ts) return "N/A"
    return new Date(ts).toLocaleString()
  }

  function getItemTitle(): string {
    // Try common title fields
    if (item.title) return item.title
    if (item.subject) return item.subject
    if (item.name) return item.name
    if (item.message) return truncate(item.message, 60)
    if (item.text) return truncate(item.text, 60)
    if (item.content) return truncate(item.content, 60)
    return `${type} item`
  }

  function getItemTimestamp(): number | null {
    if (item.timestamp) return item.timestamp
    if (item.createdAt) return item.createdAt
    if (item.receivedAt) return item.receivedAt
    return null
  }

  function truncate(text: string, length: number): string {
    if (!text) return ""
    return text.length > length ? text.substring(0, length) + "..." : text
  }

  function getDisplayFields(): Array<{key: string; value: any}> {
    const exclude = ["_", "_key", "title", "timestamp", "createdAt", "receivedAt"]
    return Object.entries(item)
      .filter(([key]) => !exclude.includes(key))
      .map(([key, value]) => ({key, value}))
      .slice(0, expanded ? undefined : 5)
  }

  function formatValue(value: any): string {
    if (value === null || value === undefined) return "N/A"
    if (typeof value === "object") return JSON.stringify(value, null, 2)
    if (typeof value === "boolean") return value ? "✓" : "✗"
    if (typeof value === "number" && value > 1000000000) {
      // Looks like a timestamp
      return formatTimestamp(value)
    }
    return String(value)
  }

  function getTypeColor(type: string): string {
    const colors: Record<string, string> = {
      "rss-feed": "#f59e0b",
      twitter: "#1da1f2",
      mastodon: "#6364ff",
      reddit: "#ff4500",
      webhook: "#10b981",
      "iot-sensor": "#8b5cf6",
      email: "#3b82f6",
      logs: "#6b7280",
      "json-document": "#ec4899",
    }
    return colors[type] || "#9ca3af"
  }
</script>

<div class="generic-item">
  <div class="item-header">
    <span class="type-badge" style="background-color: {getTypeColor(type)}">
      {type}
    </span>
    {#if getItemTimestamp()}
      <span class="timestamp">{formatTimestamp(getItemTimestamp())}</span>
    {/if}
  </div>

  <h3 class="item-title">{getItemTitle()}</h3>

  <div class="item-fields">
    {#each getDisplayFields() as field}
      <div class="field-row">
        <span class="field-key">{field.key}:</span>
        <span class="field-value">{truncate(formatValue(field.value), expanded ? 500 : 100)}</span>
      </div>
    {/each}
  </div>

  {#if Object.keys(item).length > 7}
    <button class="toggle-btn" on:click={() => (expanded = !expanded)}>
      {expanded ? "Show Less" : "Show More"} ↓
    </button>
  {/if}
</div>

<style>
  .generic-item {
    background: white;
    border: 1px solid #e5e7eb;
    border-radius: 12px;
    padding: 1.5rem;
    transition: all 0.2s;
  }

  .generic-item:hover {
    box-shadow: 0 4px 12px rgba(0, 0, 0, 0.1);
    transform: translateY(-2px);
  }

  .item-header {
    display: flex;
    justify-content: space-between;
    align-items: center;
    margin-bottom: 1rem;
  }

  .type-badge {
    font-size: 0.75rem;
    font-weight: 600;
    color: white;
    padding: 0.25rem 0.75rem;
    border-radius: 9999px;
    text-transform: uppercase;
    letter-spacing: 0.05em;
  }

  .timestamp {
    font-size: 0.75rem;
    color: #9ca3af;
  }

  .item-title {
    font-size: 1.125rem;
    font-weight: 600;
    color: #111827;
    margin: 0 0 1rem 0;
    line-height: 1.4;
  }

  .item-fields {
    display: flex;
    flex-direction: column;
    gap: 0.75rem;
  }

  .field-row {
    display: grid;
    grid-template-columns: 120px 1fr;
    gap: 0.5rem;
    font-size: 0.875rem;
  }

  .field-key {
    font-weight: 500;
    color: #6b7280;
    word-break: break-word;
  }

  .field-value {
    color: #111827;
    word-break: break-word;
    white-space: pre-wrap;
  }

  .toggle-btn {
    background: none;
    border: none;
    color: #3b82f6;
    font-size: 0.875rem;
    font-weight: 500;
    cursor: pointer;
    margin-top: 1rem;
    padding: 0.5rem 0;
    width: 100%;
    text-align: center;
  }

  .toggle-btn:hover {
    text-decoration: underline;
  }
</style>

