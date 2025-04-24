<script lang="ts">
  import { onMount } from "svelte";

  // Define types for our data structure
  interface RecognitionDataPoint {
    category: string;
    peer: string;
    value: number;
  }

  // Component props using Svelte 5 runes
  let {
    data = [],
    height = null,
    width = 928,
    colorScheme = ["#3498db", "#e74c3c", "#2ecc71", "#f39c12", "#9b59b6"],
  } = $props<{
    data: RecognitionDataPoint[];
    height?: number | null;
    width?: number;
    colorScheme?: string[];
  }>();

  // Reactive state
  let chartContainer: HTMLDivElement | null = $state(null);
  let containerWidth = $state(0);
  let hoveredBar = $state<{
    category: string;
    peer: string;
    value: number;
  } | null>(null);

  // Data state variables
  let groupedData = $state<
    Record<string, { [peer: string]: number; total: number }>
  >({});
  let sortedCategories = $state<string[]>([]);
  let uniquePeers = $state<string[]>([]);
  let percentages = $state<Record<string, { [peer: string]: number }>>({});
  let peerColors = $state<Record<string, string>>({});
  let dataHash = $state("");

  // Create a simple hash of the data to detect changes
  function hashData(inputData: RecognitionDataPoint[]): string {
    return JSON.stringify(inputData);
  }

  // Process the data
  function processData() {
    // Skip processing if data hasn't changed
    const newHash = hashData(data);
    if (newHash === dataHash && Object.keys(groupedData).length > 0) {
      return;
    }
    dataHash = newHash;

    // Get unique categories and peers
    const categories = new Set(
      data.map((d: RecognitionDataPoint) => d.category),
    );
    const peers = new Set(data.map((d: RecognitionDataPoint) => d.peer));

    // Convert to arrays
    const peerArray = Array.from(peers) as string[];
    uniquePeers = peerArray;

    // Initialize result structure
    const result: Record<string, { [peer: string]: number; total: number }> =
      {};

    // Initialize all categories with all peers set to 0
    Array.from(categories).forEach((category) => {
      result[category as string] = { total: 0 };
      uniquePeers.forEach((peer) => {
        result[category as string][peer] = 0;
      });
    });

    // Fill in actual values
    for (const item of data) {
      result[item.category][item.peer] = item.value;
      result[item.category].total += item.value;
    }

    // Update grouped data
    groupedData = result;

    // Sort categories by total value (descending)
    sortedCategories = Object.entries(groupedData)
      .sort((a, b) => b[1].total - a[1].total)
      .map(([category]) => category);

    // Calculate percentages
    const percentageResult: Record<string, { [peer: string]: number }> = {};

    Object.keys(groupedData).forEach((category) => {
      percentageResult[category] = {};
      const totalForCategory = groupedData[category].total;

      uniquePeers.forEach((peer) => {
        if (totalForCategory > 0) {
          percentageResult[category][peer] =
            groupedData[category][peer] / totalForCategory;
        } else {
          percentageResult[category][peer] = 0;
        }
      });
    });

    percentages = percentageResult;

    // Assign colors to peers
    const colors: Record<string, string> = {};
    uniquePeers.forEach((peer, i) => {
      colors[peer] = colorScheme[i % colorScheme.length];
    });
    peerColors = colors;
  }

  // Handle window resize
  function updateContainerWidth() {
    if (chartContainer) {
      containerWidth = chartContainer.getBoundingClientRect().width;
    }
  }

  // Handle mouse enter on bar segment
  function handleMouseEnter(category: string, peer: string, value: number) {
    hoveredBar = { category, peer, value };
  }

  // Handle mouse leave
  function handleMouseLeave() {
    hoveredBar = null;
  }

  // Update width on mount and process data initially
  onMount(() => {
    processData();
    updateContainerWidth();
    window.addEventListener("resize", updateContainerWidth);

    return () => {
      window.removeEventListener("resize", updateContainerWidth);
    };
  });

  // Manually track data changes rather than using $effect
  // Only re-process when data changes, using JSON.stringify for comparison
  let prevDataString = "";
  $effect(() => {
    const dataString = JSON.stringify(data);
    if (dataString !== prevDataString) {
      prevDataString = dataString;
      processData();
    }
  });
</script>

<div class="stacked-bar-chart" bind:this={chartContainer}>
  <h2 class="chart-title">Recognition Distribution</h2>

  <!-- Legend -->
  <div class="legend">
    {#each uniquePeers as peer}
      <div class="legend-item">
        <div
          class="color-box"
          style="background-color: {peerColors[peer]}"
        ></div>
        <div class="peer-name">{peer}</div>
      </div>
    {/each}
  </div>

  <!-- Chart content -->
  <div class="chart-content">
    {#each sortedCategories as category}
      <div class="chart-row">
        <div class="category-label">{category}</div>
        <div class="bar-container">
          {#each uniquePeers as peer}
            {#if percentages[category] && percentages[category][peer] > 0}
              <div
                class="bar-segment"
                style="
                  width: {(percentages[category][peer] * 100).toFixed(1)}%; 
                  background-color: {peerColors[peer]};
                "
                on:mouseenter={() =>
                  handleMouseEnter(category, peer, groupedData[category][peer])}
                on:mouseleave={handleMouseLeave}
              ></div>
            {/if}
          {/each}
        </div>
        <div class="value-label">
          {groupedData[category] ? groupedData[category].total : 0}
        </div>
      </div>
    {/each}
  </div>

  <!-- Tooltip -->
  {#if hoveredBar}
    <div class="tooltip">
      <div class="tooltip-title">{hoveredBar.category} - {hoveredBar.peer}</div>
      <div class="tooltip-content">
        <div class="tooltip-value">Value: {hoveredBar.value}</div>
        <div class="tooltip-percentage">
          {percentages[hoveredBar.category] &&
          percentages[hoveredBar.category][hoveredBar.peer]
            ? (percentages[hoveredBar.category][hoveredBar.peer] * 100).toFixed(
                1,
              )
            : "0"}%
        </div>
      </div>
    </div>
  {/if}
</div>

<style>
  .stacked-bar-chart {
    width: 100%;
    max-width: 100%;
    padding: 20px;
    font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto,
      Helvetica, Arial, sans-serif;
    position: relative;
  }

  .chart-title {
    font-size: 1.4rem;
    margin-bottom: 20px;
    color: #333;
    text-align: center;
  }

  .legend {
    display: flex;
    flex-wrap: wrap;
    gap: 15px;
    margin-bottom: 25px;
    justify-content: center;
  }

  .legend-item {
    display: flex;
    align-items: center;
    gap: 5px;
  }

  .color-box {
    width: 15px;
    height: 15px;
    border-radius: 3px;
  }

  .peer-name {
    font-size: 0.9rem;
    color: #555;
  }

  .chart-content {
    display: flex;
    flex-direction: column;
    gap: 12px;
  }

  .chart-row {
    display: flex;
    align-items: center;
    height: 35px;
  }

  .category-label {
    width: 120px;
    font-weight: 500;
    font-size: 0.95rem;
    color: #333;
    padding-right: 10px;
    text-align: right;
  }

  .bar-container {
    flex: 1;
    height: 24px;
    display: flex;
    background-color: #f0f0f0;
    border-radius: 4px;
    overflow: hidden;
  }

  .bar-segment {
    height: 100%;
    transition: opacity 0.2s ease;
  }

  .bar-segment:hover {
    opacity: 0.8;
    box-shadow: inset 0 0 0 2px rgba(255, 255, 255, 0.5);
  }

  .value-label {
    width: 50px;
    font-size: 0.85rem;
    color: #666;
    padding-left: 10px;
  }

  .tooltip {
    position: absolute;
    top: 50%;
    left: 50%;
    transform: translate(-50%, -50%);
    background-color: rgba(0, 0, 0, 0.85);
    color: white;
    padding: 10px 15px;
    border-radius: 5px;
    font-size: 0.9rem;
    z-index: 10;
    pointer-events: none;
    box-shadow: 0 3px 10px rgba(0, 0, 0, 0.2);
    min-width: 150px;
    text-align: center;
  }

  .tooltip-title {
    font-weight: 600;
    margin-bottom: 5px;
    font-size: 1rem;
  }

  .tooltip-content {
    display: flex;
    justify-content: space-between;
    gap: 10px;
  }

  .tooltip-value,
  .tooltip-percentage {
    font-size: 0.85rem;
    color: rgba(255, 255, 255, 0.9);
  }

  /* Responsive adjustments */
  @media (max-width: 600px) {
    .category-label {
      width: 80px;
      font-size: 0.8rem;
    }

    .value-label {
      width: 40px;
      font-size: 0.8rem;
    }

    .chart-row {
      height: 30px;
    }

    .bar-container {
      height: 20px;
    }
  }
</style>
