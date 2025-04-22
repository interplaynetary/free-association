<script lang="ts">
    import { onMount, afterUpdate } from 'svelte';
    import * as d3 from 'd3';
    
    // Define types for our data structure
    interface RecognitionDataPoint {
      category: string;
      peer: string;
      value: number;
    }
  
    // Component props
    export let data: RecognitionDataPoint[] = [];
    export let width: number = 928;
    export let height: number | null = null; // Will be calculated based on data
    export let marginTop: number = 30;
    export let marginRight: number = 20;
    export let marginBottom: number = 0;
    export let marginLeft: number = 30;
    export let colorScheme: ReadonlyArray<string> = d3.schemeSpectral[10];
    
    // Local variables
    let svg: SVGSVGElement;
    let legendDiv: HTMLDivElement;
    let calculatedHeight: number;
    
    // Function to create/update the chart
    function updateChart() {
      if (!data || data.length === 0 || !svg) return;
  
      // Clear previous content
      d3.select(svg).selectAll('*').remove();
      d3.select(legendDiv).selectAll('*').remove();
      
      // Get unique recognition types
      const peers: string[] = Array.from(new Set(data.map(d => d.peer)));
      
      // Create an index for faster lookups
      const dataByCategoryAndType = d3.group(data, d => d.category, d => d.peer);
      
      // Stack the data
      const series = d3.stack<[string, Map<string, RecognitionDataPoint[]>], string>()
        .keys(peers)
        .value((categoryGroup, key) => {
          const entry = categoryGroup[1].get(key);
          return entry ? entry[0].value : 0;
        })
        .offset(d3.stackOffsetExpand)(dataByCategoryAndType);
      
      // Compute the height from the number of stacks if not provided
      calculatedHeight = height || series[0].length * 25 + marginTop + marginBottom;
      
      // Update the SVG dimensions
      d3.select(svg)
        .attr("width", width)
        .attr("height", calculatedHeight)
        .attr("viewBox", [0, 0, width, calculatedHeight]);
      
      // Prepare the scales for positional and color encodings
      const x = d3.scaleLinear()
        .domain([0, d3.max(series, d => d3.max(d, d => d[1])) || 1])
        .range([marginLeft, width - marginRight]);
      
      // Sort categories by the proportion of self-recognition
      const sortedCategories = Array.from(d3.group(data, d => d.category))
        .map(([category, values]) => {
          const total = d3.sum(values, d => d.value);
          const self = values.find(d => d.peer === "self")?.value || 0;
          return { category, proportion: -self / total };
        })
        .sort((a, b) => a.proportion - b.proportion)
        .map(d => d.category);
      
      const y = d3.scaleBand()
        .domain(sortedCategories)
        .range([marginTop, calculatedHeight - marginBottom])
        .padding(0.08);
      
      const color = d3.scaleOrdinal<string>()
        .domain(series.map(d => d.key))
        .range(colorScheme.slice(0, peers.length))
        .unknown("#ccc");
      
      // A function to format the value in the tooltip
      const formatValue = (x: number): string => isNaN(x) ? "N/A" : x.toLocaleString("en");
      
      // Create a group element
      const g = d3.select(svg).append("g");
      
      // Append a group for each series, and a rect for each element in the series
      g.selectAll<SVGGElement, d3.Series<[string, Map<string, RecognitionDataPoint[]>], string>>(".series")
        .data(series)
        .join("g")
          .attr("fill", d => color(d.key))
        .selectAll<SVGRectElement, d3.SeriesPoint<[string, Map<string, RecognitionDataPoint[]>]>>(".rect")
        .data(D => D.map(d => ({...d, key: D.key})))
        .join("rect")
          .attr("x", d => x(d[0]))
          .attr("y", d => y(d.data[0]) || 0)
          .attr("height", y.bandwidth())
          .attr("width", d => x(d[1]) - x(d[0]))
        .append("title")
          .text(d => {
            const typeData = Array.from(d.data[1].get(d.key as string) || []);
            const value = typeData.length > 0 ? typeData[0].value : 0;
            return `${d.data[0]} ${d.key}\n${formatValue(value)}`;
          });
      
      // Append the horizontal axis
      g.append("g")
        .attr("transform", `translate(0,${marginTop})`)
        .call(d3.axisTop(x).ticks(width / 100, "%"))
        .call(g => g.selectAll(".domain").remove());
      
      // Append the vertical axis
      g.append("g")
        .attr("transform", `translate(${marginLeft},0)`)
        .call(d3.axisLeft(y).tickSizeOuter(0))
        .call(g => g.selectAll(".domain").remove());
      
      // Create a legend
      const legendSvg = d3.select(legendDiv).append("svg")
        .attr("width", 200)
        .attr("height", peers.length * 20 + 10);
        
      const legendGroup = legendSvg.append("g")
        .attr("transform", "translate(10,10)");
        
      peers.forEach((type, i) => {
        const g = legendGroup.append("g")
          .attr("transform", `translate(0,${i * 20})`);
          
        g.append("rect")
          .attr("width", 15)
          .attr("height", 15)
          .attr("fill", color(type));
          
        g.append("text")
          .attr("x", 20)
          .attr("y", 12)
          .text(type);
      });
    }
    
    // Update chart whenever data changes
    $: if (data) {
      afterUpdate(updateChart);
    }
  
    // Initialize the chart when component mounts
    onMount(updateChart);
  </script>
  
  <div class="chart-container">
    <svg bind:this={svg}></svg>
    <div class="legend-container" bind:this={legendDiv}></div>
  </div>
  
  <style>
    .chart-container {
      display: flex;
      flex-direction: column;
      align-items: center;
      width: 100%;
    }
    
    svg {
      max-width: 100%;
      height: auto;
    }
    
    .legend-container {
      align-self: flex-start;
      margin-top: 10px;
    }
  </style>