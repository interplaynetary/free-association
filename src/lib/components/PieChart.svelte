<script lang="ts">
  /*
   * PieChart.svelte - Svelte component for visualizing mutual fulfillment distributions using D3
   *
   * This component is a direct Svelte adaptation of the original PieChart.ts implementation.
   *
   * Usage:
   * <PieChart bind:this={pieChartComponent} data={dataTreeNode} />
   */
  import * as d3 from "d3";
  import { getUserName } from "../utils/userUtils";
  import { getColorForUserId } from "../utils/colorUtils";
  import { onMount } from "svelte";

  // Props
  export let data: TreeNode;
  export let width: number = 0;
  export let height: number = 0;

  // Container reference with custom interface to allow adding methods
  interface PieChartElement extends HTMLDivElement {
    getElement?: () => HTMLElement | SVGSVGElement;
  }

  let pieContainer: PieChartElement;
  let svg: d3.Selection<SVGSVGElement, unknown, null, undefined>;

  // Expose getElement method for compatibility with the original implementation
  export function getElement(): HTMLElement | SVGSVGElement {
    return svg ? svg.node() : pieContainer;
  }

  // Initialize on mount or when data changes
  $: if (data && pieContainer) {
    renderPieChart();
  }

  onMount(() => {
    if (data && pieContainer) {
      renderPieChart();
    }

    // Make getElement available on the DOM element
    if (pieContainer) {
      pieContainer.getElement = getElement;
    }
  });

  function renderPieChart() {
    // Clean up any existing chart
    if (pieContainer) {
      pieContainer.innerHTML = "";
    }

    // Get dimensions
    if (!width || !height) {
      width = pieContainer.clientWidth;
      height = pieContainer.clientHeight;
    }

    const radius = Math.min(width, height) / 2;
    const ringThickness = radius * 0.8 - radius * 0.4;

    // Get mutualFulfillmentDistribution from root node
    console.log("Creating pie chart for data:", data);

    try {
      // Use the synchronous method to get the distribution
      const mutualFulfillmentDistribution = data.mutualFulfillmentDistribution;
      console.log(
        "mutualFulfillmentDistribution for pie:",
        mutualFulfillmentDistribution,
      );
      console.log(
        "mutualFulfillmentDistribution keys:",
        Array.from(mutualFulfillmentDistribution.keys()),
      );
      console.log(
        "mutualFulfillmentDistribution values:",
        Array.from(mutualFulfillmentDistribution.values()),
      );

      // Create an array of [nodeId, value] pairs for the pie chart
      const pieData: [string, number][] = Array.from(
        mutualFulfillmentDistribution.entries(),
      );
      console.log("Pie chart data entries:", pieData);

      // If there's no data, create a placeholder pie chart
      if (pieData.length === 0) {
        console.log("No mutual fulfillment data found, creating placeholder");
        console.log("Data root node ID:", data.id);
        console.log("Data root node name:", data.name);
        console.log("Data root types:", Array.from(data.types));
        console.log(
          "Data root sharesOfOthersRecognition:",
          data.sharesOfOthersRecognition,
        );
        createPlaceholderPieChart();
        return;
      }

      // Create pie layout
      const pie = d3
        .pie<[string, number]>()
        .value((d) => d[1]) // Use the mutualFulfillmentDistribution value
        .sort(null); // Maintain original order

      // Create arc generator
      const arc = d3
        .arc()
        .innerRadius(radius * 0.4) // Create a donut chart
        .outerRadius(radius * 0.8);

      // Create SVG
      svg = d3
        .select(pieContainer)
        .append("svg")
        .attr("width", width)
        .attr("height", height)
        .attr("viewBox", [-width / 2, -height / 2, width, height])
        .style("font", "12px sans-serif");

      // Create pie segments from mutualFulfillmentDistribution
      const arcs = pie(pieData);

      // Log the calculated arcs for debugging
      console.log("Calculated pie arcs:", arcs);

      // Add segments using the same color scheme as the treemap
      svg
        .selectAll("path")
        .data(arcs)
        .join("path")
        .attr("fill", (d) => {
          // Use the node ID to get a name for the color
          return getColorForUserId(d.data[0]);
        })
        .attr("d", (d) => arc(d as any)) // Type assertion to fix type error
        .append("title")
        .text((d) => {
          const nodeName = getUserName(d.data[0]);
          return `${nodeName}: ${(d.data[1] * 100).toFixed(1)}%`;
        });

      // Add labels with node names
      svg
        .selectAll("text")
        .data(arcs)
        .join("text")
        .attr("transform", (d) => {
          const [x, y] = arc.centroid(d as any);
          return `translate(${x}, ${y})`;
        })
        .attr("dy", "0.35em")
        .attr("text-anchor", "middle")
        .attr("fill", "white")
        .style("font-size", "10px")
        .style("pointer-events", "none")
        .style("text-shadow", "0px 0px 2px rgba(0,0,0,0.8)")
        .text((d) => {
          const nodeName = getUserName(d.data[0]);
          if (nodeName.length < 10) {
            return nodeName;
          }
          // Don't show text for long names
          return "";
        });

      // Add center text
      const centerGroup = svg
        .append("g")
        .attr("text-anchor", "middle")
        .attr("dominant-baseline", "middle");

      // Calculate text size based on inner radius
      const fontSize = radius * 0.12; // 12% of radius

      centerGroup
        .append("text")
        .attr("dy", -fontSize / 1.6) // Center it
        .attr("font-size", fontSize)
        .attr("font-weight", "bold")
        .text("Mutual");

      centerGroup
        .append("text")
        .attr("dy", fontSize / 1.6) // Move down by one full font size
        .attr("font-size", fontSize)
        .attr("font-weight", "bold")
        .text("Fulfillment");
    } catch (error) {
      console.error("Error creating pie chart:", error);
      createPlaceholderPieChart();
    }
  }

  // Helper function to create placeholder pie chart when no data is available
  function createPlaceholderPieChart() {
    // Get dimensions
    if (!width || !height) {
      width = pieContainer.clientWidth;
      height = pieContainer.clientHeight;
    }

    const radius = Math.min(width, height) / 2;

    // Create SVG
    svg = d3
      .select(pieContainer)
      .append("svg")
      .attr("width", width)
      .attr("height", height)
      .attr("viewBox", [-width / 2, -height / 2, width, height])
      .style("font", "12px sans-serif");

    // Add gray empty circle
    svg
      .append("circle")
      .attr("r", radius * 0.8)
      .attr("fill", "#f0f0f0")
      .attr("stroke", "#e0e0e0")
      .attr("stroke-width", 2);

    // Add donut hole
    svg
      .append("circle")
      .attr("r", radius * 0.4)
      .attr("fill", "white");

    // Add center text
    const centerGroup = svg
      .append("g")
      .attr("text-anchor", "middle")
      .attr("dominant-baseline", "middle");

    // Calculate text size based on inner radius
    const fontSize = radius * 0.12;

    centerGroup
      .append("text")
      .attr("dy", -fontSize / 1.6)
      .attr("font-size", fontSize)
      .attr("font-weight", "bold")
      .text("Mutual");

    centerGroup
      .append("text")
      .attr("dy", fontSize / 1.6)
      .attr("font-size", fontSize)
      .attr("font-weight", "bold")
      .text("Fulfillment");

    // Add message about no data
    svg
      .append("text")
      .attr("y", radius * 0.6)
      .attr("text-anchor", "middle")
      .attr("font-size", "12px")
      .attr("fill", "#999");
  }
</script>

<div bind:this={pieContainer} class="pie-chart-container"></div>

<style>
  .pie-chart-container {
    width: 100%;
    height: 100%;
  }
</style>
