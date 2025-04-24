<script lang="ts">
  import { onMount, onDestroy } from "svelte";
  import { type RecognitionStore } from "../../stores/rec.svelte";

  // Props using Svelte 5 runes
  let { store } = $props<{
    store: RecognitionStore;
  }>();

  // State tracking
  let capacityEntries = $state<CapacityEntry[]>([]);
  let isInitialized = $state(false);
  let currentNodeId = $state<string | null>(null);
  let inventoryVisible = $state(true); // Default to visible

  // Type definitions
  interface CapacityEntry {
    id: string;
    name: string;
    quantity: number;
    unit: string;
    depth: number;
    chartElement?: HTMLElement;
  }

  // Create a new capacity entry row
  function createCapacityEntry(): CapacityEntry {
    return {
      id: crypto.randomUUID(),
      name: "",
      quantity: 0,
      unit: "",
      depth: 3,
    };
  }

  // Default empty state
  let newCapacityEntries = $state<CapacityEntry[]>([createCapacityEntry()]);

  // Generate a color for distribution chart
  function getDistributionColor(depth: number): string {
    const colors = {
      1: "#ef4444", // Red
      2: "#f97316", // Orange
      3: "#eab308", // Yellow
      4: "#22c55e", // Green
      5: "#3b82f6", // Blue
    };

    return colors[depth as keyof typeof colors] || colors[3];
  }

  // Chart generation functions
  function createDistributionChart(
    depth: number,
    scale: number = 1,
  ): HTMLElement {
    const container = document.createElement("div");
    container.className = "distribution-chart";

    // Create bars representing distribution depth
    for (let i = 1; i <= 5; i++) {
      const bar = document.createElement("div");
      bar.className = `chart-bar ${i <= depth ? "active" : ""}`;
      (bar as HTMLElement).style.backgroundColor =
        i <= depth ? getDistributionColor(i) : "#e5e7eb";
      (bar as HTMLElement).style.height = `${scale * 20}px`;
      bar.setAttribute("data-depth", i.toString());

      // Add click handler to change depth
      bar.addEventListener("click", () => handleDepthChange(container, i));

      container.appendChild(bar);
    }

    return container;
  }

  // Handle depth change in chart
  function handleDepthChange(chartElement: HTMLElement, newDepth: number) {
    // Update visual state
    const bars = chartElement.querySelectorAll(".chart-bar");
    bars.forEach((bar, index) => {
      const i = index + 1;
      bar.classList.toggle("active", i <= newDepth);
      (bar as HTMLElement).style.backgroundColor =
        i <= newDepth ? getDistributionColor(i) : "#e5e7eb";
    });

    // Store the new depth in the element
    chartElement.setAttribute("data-current-depth", newDepth.toString());

    // Find the capacity entry this chart belongs to
    const entryId = chartElement.getAttribute("data-entry-id");

    if (entryId) {
      // Update existing capacity
      const entry = capacityEntries.find((e) => e.id === entryId);
      if (entry) {
        entry.depth = newDepth;
      }
    } else {
      // Update new capacity
      const rowIndex = parseInt(
        chartElement.getAttribute("data-row-index") || "0",
      );
      if (newCapacityEntries[rowIndex]) {
        newCapacityEntries[rowIndex].depth = newDepth;
      }
    }
  }

  // Define the action directive for chart rendering
  function chartAction(
    node: HTMLElement,
    options: {
      depth: number;
      scale?: number;
      entryId?: string;
      rowIndex?: string;
    },
  ) {
    const { depth, scale = 1, entryId, rowIndex } = options;

    const chart = createDistributionChart(depth, scale);

    if (entryId) {
      chart.setAttribute("data-entry-id", entryId);
    }

    if (rowIndex !== undefined) {
      chart.setAttribute("data-row-index", rowIndex);
    }

    node.appendChild(chart);

    return {
      destroy() {
        if (node.contains(chart)) {
          node.removeChild(chart);
        }
      },
    };
  }

  // Handle adding a new capacity row
  function addCapacityRow() {
    newCapacityEntries = [...newCapacityEntries, createCapacityEntry()];
  }

  // Save all capacity changes
  async function saveInventoryChanges() {
    // Process existing entries
    const updatedEntries: CapacityEntry[] = [];

    for (const entry of capacityEntries) {
      if (entry.quantity > 0 && entry.unit) {
        updatedEntries.push(entry);
        // In a real implementation, we would save to backend here
      }
    }

    // Process new entries
    for (const entry of newCapacityEntries) {
      if (entry.name && entry.quantity > 0 && entry.unit) {
        // In a real implementation, we would create and save these entries
        updatedEntries.push(entry);
      }
    }

    // Update our local state
    capacityEntries = updatedEntries;
    newCapacityEntries = [createCapacityEntry()];

    // Show a success message
    alert("Capacity declarations updated successfully.");
  }

  // Initialize with mock data for demo
  function initialize() {
    // In a real implementation, this would fetch data from a store
    capacityEntries = [
      { id: "cap1", name: "John Smith", quantity: 10, unit: "hours", depth: 3 },
      {
        id: "cap2",
        name: "Community Garden",
        quantity: 25,
        unit: "kg",
        depth: 4,
      },
      {
        id: "cap3",
        name: "Support Network",
        quantity: 5,
        unit: "sessions",
        depth: 2,
      },
    ];

    isInitialized = true;
  }

  // Toggle inventory visibility
  function toggleInventory() {
    inventoryVisible = !inventoryVisible;
  }

  // Initialize component
  onMount(() => {
    initialize();

    // In a real implementation, we would subscribe to store changes here
    currentNodeId = store.path[store.path.length - 1];
  });

  onDestroy(() => {
    // Clean up any subscriptions
  });
</script>

<div class="inventory-container">
  {#if inventoryVisible}
    <div class="inventory-panel">
      <div class="inventory-header">
        <h2>Capacity Declarations</h2>
      </div>

      <div class="inventory-content">
        <div class="inventory-form">
          <!-- Existing capacities section -->
          {#if capacityEntries.length > 0}
            <h3 class="section-header">Existing Capacities</h3>
            <div class="capacity-inputs">
              {#each capacityEntries as entry (entry.id)}
                <div class="capacity-input-row" data-capacity-id={entry.id}>
                  <div class="name-container">
                    <span class="capacity-name">{entry.name}</span>
                  </div>

                  <div class="inputs-container">
                    <div class="input-group">
                      <label class="input-label" for={`quantity-${entry.id}`}
                        >Quantity:</label
                      >
                      <input
                        type="number"
                        id={`quantity-${entry.id}`}
                        class="quantity-input"
                        min="0"
                        step="0.01"
                        bind:value={entry.quantity}
                        placeholder="Quantity"
                      />
                    </div>

                    <div class="input-group">
                      <label class="input-label" for={`unit-${entry.id}`}
                        >Units:</label
                      >
                      <input
                        type="text"
                        id={`unit-${entry.id}`}
                        class="unit-input"
                        bind:value={entry.unit}
                        placeholder="Units"
                      />
                    </div>
                  </div>

                  <div class="distribution-chart-container">
                    <span class="depth-indicator">Depth: {entry.depth}</span>
                    <!-- Chart will be rendered with action directive -->
                    <div
                      class="chart-placeholder"
                      use:chartAction={{
                        depth: entry.depth,
                        entryId: entry.id,
                      }}
                    ></div>
                  </div>
                </div>
              {/each}
            </div>
          {/if}

          <!-- New capacity section -->
          <h3 class="section-header">Add New Capacity</h3>
          <div class="capacity-inputs">
            {#each newCapacityEntries as entry, i (i)}
              <div class="capacity-input-row new-capacity-row">
                <div class="name-container">
                  <input
                    type="text"
                    class="capacity-name-input"
                    bind:value={entry.name}
                    placeholder="Capacity Name"
                  />
                </div>

                <div class="inputs-container">
                  <div class="input-group">
                    <label class="input-label" for={`new-quantity-${i}`}
                      >Quantity:</label
                    >
                    <input
                      type="number"
                      id={`new-quantity-${i}`}
                      class="quantity-input"
                      min="0"
                      step="0.01"
                      bind:value={entry.quantity}
                      placeholder="Quantity"
                    />
                  </div>

                  <div class="input-group">
                    <label class="input-label" for={`new-unit-${i}`}
                      >Units:</label
                    >
                    <input
                      type="text"
                      id={`new-unit-${i}`}
                      class="unit-input"
                      bind:value={entry.unit}
                      placeholder="Units"
                    />
                  </div>
                </div>

                <div class="distribution-chart-container">
                  <span class="depth-indicator">Depth: {entry.depth}</span>
                  <!-- Chart will be rendered with action directive -->
                  <div
                    class="chart-placeholder"
                    use:chartAction={{
                      depth: entry.depth,
                      rowIndex: i.toString(),
                    }}
                  ></div>
                </div>
              </div>
            {/each}

            <button
              type="button"
              class="add-capacity-button"
              onclick={addCapacityRow}
            >
              + Add Another Capacity
            </button>
          </div>

          <div class="form-actions">
            <button
              type="button"
              class="update-inventory"
              onclick={saveInventoryChanges}
            >
              Save Changes
            </button>
            <button
              type="button"
              class="cancel-button"
              onclick={toggleInventory}
            >
              Cancel
            </button>
          </div>
        </div>

        <div class="inventory-list">
          <h3 class="section-header">Current Capacity Declarations</h3>

          {#if capacityEntries.length === 0}
            <p class="no-inventory">No capacity declarations found.</p>
          {:else}
            <div class="inventory-header">
              <strong>Current Capacity Declarations</strong>
            </div>

            {#each capacityEntries as entry (entry.id)}
              <div class="inventory-item">
                <span class="item-name">{entry.name}</span>
                <div class="item-details-container">
                  <span class="item-details">{entry.quantity} {entry.unit}</span
                  >
                  <!-- Chart will be rendered with action directive -->
                  <div
                    class="chart-small"
                    use:chartAction={{ depth: entry.depth, scale: 0.8 }}
                  ></div>
                </div>
              </div>
            {/each}
          {/if}
        </div>
      </div>
    </div>
  {/if}
</div>

<style>
  .inventory-container {
    position: relative;
    width: 100%;
    height: 100%;
  }

  .inventory-panel {
    position: absolute;
    top: 0;
    left: 0;
    width: 100%;
    height: 100%;
    background-color: white;
    z-index: 100;
    display: flex;
    flex-direction: column;
  }

  .inventory-header {
    display: flex;
    justify-content: space-between;
    align-items: center;
    padding: 16px;
    background-color: #f8fafc;
    border-bottom: 1px solid #e2e8f0;
  }

  .inventory-header h2 {
    margin: 0;
    font-size: 1.25rem;
    color: #1e293b;
  }

  .inventory-content {
    flex: 1;
    overflow-y: auto;
    padding: 16px;
    display: flex;
    flex-direction: column;
    gap: 24px;
  }

  .section-header {
    font-size: 1.1rem;
    color: #334155;
    margin-bottom: 12px;
    padding-bottom: 4px;
    border-bottom: 1px solid #e2e8f0;
  }

  .capacity-inputs {
    display: flex;
    flex-direction: column;
    gap: 16px;
  }

  .capacity-input-row {
    display: grid;
    grid-template-columns: 1fr 2fr 1fr;
    gap: 12px;
    padding: 12px;
    background-color: #f8fafc;
    border-radius: 6px;
    box-shadow: 0 1px 3px rgba(0, 0, 0, 0.05);
  }

  .name-container {
    display: flex;
    align-items: center;
  }

  .capacity-name {
    font-weight: 500;
    color: #1e293b;
  }

  .capacity-name-input {
    width: 100%;
    padding: 8px 10px;
    border: 1px solid #cbd5e1;
    border-radius: 4px;
    font-size: 0.9rem;
  }

  .inputs-container {
    display: flex;
    gap: 12px;
  }

  .input-group {
    display: flex;
    flex-direction: column;
    flex: 1;
  }

  .input-label {
    font-size: 0.8rem;
    color: #64748b;
    margin-bottom: 4px;
  }

  .quantity-input,
  .unit-input {
    padding: 8px 10px;
    border: 1px solid #cbd5e1;
    border-radius: 4px;
    font-size: 0.9rem;
  }

  .distribution-chart-container {
    display: flex;
    flex-direction: column;
    align-items: center;
    gap: 4px;
  }

  .chart-placeholder {
    height: 30px;
    width: 100%;
    display: flex;
    justify-content: center;
  }

  .depth-indicator {
    font-size: 0.8rem;
    color: #64748b;
  }

  .add-capacity-button {
    background-color: #f8fafc;
    border: 1px dashed #cbd5e1;
    color: #64748b;
    padding: 8px;
    border-radius: 6px;
    cursor: pointer;
    font-size: 0.9rem;
    transition: all 0.2s;
    width: 100%;
  }

  .add-capacity-button:hover {
    background-color: #f1f5f9;
    color: #334155;
  }

  .form-actions {
    display: flex;
    justify-content: flex-end;
    gap: 12px;
    margin-top: 24px;
  }

  .update-inventory {
    padding: 8px 16px;
    background-color: #3b82f6;
    color: white;
    border: none;
    border-radius: 4px;
    font-weight: 500;
    cursor: pointer;
  }

  .update-inventory:hover {
    background-color: #2563eb;
  }

  .cancel-button {
    padding: 8px 16px;
    background-color: #f1f5f9;
    color: #64748b;
    border: 1px solid #cbd5e1;
    border-radius: 4px;
    font-weight: 500;
    cursor: pointer;
  }

  .cancel-button:hover {
    background-color: #e2e8f0;
  }

  .inventory-list {
    margin-top: 24px;
  }

  .inventory-item {
    display: flex;
    justify-content: space-between;
    align-items: center;
    padding: 12px;
    background-color: #f8fafc;
    border-radius: 6px;
    margin-bottom: 8px;
  }

  .item-name {
    font-weight: 500;
    color: #1e293b;
  }

  .item-details-container {
    display: flex;
    align-items: center;
    gap: 12px;
  }

  .item-details {
    color: #64748b;
  }

  .chart-small {
    height: 24px;
  }

  .no-inventory {
    color: #64748b;
    text-align: center;
    padding: 24px;
    font-style: italic;
  }
</style>
