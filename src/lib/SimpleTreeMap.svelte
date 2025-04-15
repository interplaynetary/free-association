<script lang="ts">
    import { onMount } from 'svelte';
    import * as d3 from 'd3';
    import { type RecognitionStore, type NodeEntry } from '../stores/rec';
    
    // Props
    export let store: RecognitionStore;
    export let width: number = 800;
    export let height: number = 600;
    
    // Internal state
    let svg: d3.Selection<SVGSVGElement, unknown, null, undefined>;
    let container: HTMLDivElement;
    let name = "";
    let children: NodeEntry[] = [];
    
    // Subscribe to store data
    function subscribeToData() {
        if (!store) {
            console.error("SimpleTreeMap: No store provided");
            return;
        }
        
        // Log store properties
        console.log("SimpleTreeMap: Store received", {
            path: store.path,
            stores: {
                name: !!store.nameStore,
                points: !!store.pointsStore,
                children: !!store.childrenStore
            }
        });
        
        // Subscribe to name
        const unsubName = store.nameStore.subscribe(value => {
            console.log("SimpleTreeMap: Name updated", value);
            name = value || "No Name";
            updateVisualization();
        });
        
        // Subscribe to children
        const unsubChildren = store.childrenStore.subscribe(value => {
            console.log("SimpleTreeMap: Children updated", value);
            children = value || [];
            updateVisualization();
        });
        
        return () => {
            unsubName();
            unsubChildren();
        };
    }
    
    // Create visualization
    function updateVisualization() {
        if (!svg || !name) return;
        
        console.log("SimpleTreeMap: Updating visualization", {
            name,
            childCount: children.length
        });
        
        // Clear previous content
        svg.selectAll("*").remove();
        
        // Create root container
        const root = svg.append("g")
            .attr("transform", "translate(10, 10)");
        
        // Add title
        root.append("text")
            .attr("x", width / 2)
            .attr("y", 30)
            .attr("text-anchor", "middle")
            .style("font-size", "24px")
            .text(name);
        
        // Create child boxes
        const boxWidth = (width - 40) / Math.max(1, children.length);
        const boxHeight = height - 100;
        
        children.forEach((child, index) => {
            const [key, data] = child;
            const childName = data.name || "Unnamed";
            const childPoints = data.points || 0;
            
            const childGroup = root.append("g")
                .attr("transform", `translate(${20 + index * boxWidth}, 60)`);
            
            childGroup.append("rect")
                .attr("width", boxWidth - 10)
                .attr("height", boxHeight)
                .attr("fill", d3.interpolateBlues(0.3 + (index * 0.2)))
                .attr("stroke", "#333")
                .attr("rx", 5);
            
            childGroup.append("text")
                .attr("x", (boxWidth - 10) / 2)
                .attr("y", 30)
                .attr("text-anchor", "middle")
                .style("font-size", "16px")
                .style("font-weight", "bold")
                .style("fill", "#fff")
                .text(childName);
            
            childGroup.append("text")
                .attr("x", (boxWidth - 10) / 2)
                .attr("y", 60)
                .attr("text-anchor", "middle")
                .style("font-size", "14px")
                .style("fill", "#fff")
                .text(`${childPoints} points`);
        });
    }
    
    onMount(() => {
        console.log("SimpleTreeMap: Component mounted", { width, height });
        
        // Create SVG
        svg = d3.select(container)
            .append("svg")
            .attr("width", width)
            .attr("height", height)
            .style("border", "1px solid #ccc");
        
        const cleanup = subscribeToData();
        
        return () => {
            if (cleanup) cleanup();
        };
    });
</script>

<div bind:this={container} class="simple-treemap-container">
    <!-- SVG will be added here -->
    {#if !store}
        <div class="error">No store provided</div>
    {/if}
</div>

<style>
    .simple-treemap-container {
        width: 100%;
        height: 100%;
        position: relative;
    }
    
    .error {
        color: red;
        padding: 20px;
        font-size: 18px;
        text-align: center;
    }
</style> 