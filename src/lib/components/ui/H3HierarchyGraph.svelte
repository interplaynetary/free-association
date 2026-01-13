<script lang="ts">
    import * as h3 from 'h3-js';
    import { graphStratify, sugiyama, layeringSimplex, decrossOpt, decrossTwoLayer, coordCenter } from 'd3-dag';
    import * as d3 from 'd3';
    import { h3ToColor } from '$lib/utils/h3Colors';
    
    interface Props {
        selectedCells: Set<string>;
        onHover?: (cell: string) => void;
        onClick?: (cell: string) => void;
    }

    let { selectedCells, onHover, onClick }: Props = $props();

    let svgElement: SVGSVGElement;
    
    // Build DAG layout
    function buildLayout(cells: string[]) {
        // Build hierarchy data
        const data = cells.map(cell => {
            const res = h3.getResolution(cell);
            let parentIds: string[] = [];
            
            // Find nearest ancestor in selection
            for (let r = res - 1; r >= 0; r--) {
                const parent = h3.cellToParent(cell, r);
                if (selectedCells.has(parent)) {
                    parentIds.push(parent);
                    break;
                }
            }
            
            return { id: cell, parentIds };
        });

        // Create DAG
        const stratify = graphStratify();
        const dag = stratify(data);
        
        // Use Simplex layering - it respects the DAG structure which naturally follows H3 hierarchy
        const layout = sugiyama()
            .nodeSize([50, 70])
            .layering(layeringSimplex())
            .decross(decrossTwoLayer())
            .coord(coordCenter());
        
        const { width, height } = layout(dag);
        return { dag, width, height };
    }
    
    // Render DAG
    $effect(() => {
        if (!svgElement || !selectedCells || selectedCells.size === 0) {
            if (svgElement) d3.select(svgElement).selectAll("*").remove();
            return;
        }

        const cells = Array.from(selectedCells);
        
        try {
            const { dag, width, height } = buildLayout(cells);
            
            // Clear and setup SVG
            const svg = d3.select(svgElement);
            svg.selectAll("*").remove();
            svg.attr("width", width)
               .attr("height", height)
               .attr("viewBox", `0 0 ${width} ${height}`);

            const defs = svg.append("defs");
            
            // Create color map
            const colorMap: Record<string, string> = {};
            for (const node of dag.nodes()) {
                colorMap[node.data.id] = h3ToColor(node.data.id);
            }

            // Line generator for edges
            const line = d3.line()
                .curve(d3.curveCatmullRom)
                .x((d: any) => d[0])
                .y((d: any) => d[1]);

            // Draw edges with gradients
            svg.append("g")
                .selectAll("path")
                .data(dag.links())
                .join("path")
                .attr("d", (d: any) => line(d.points))
                .attr("fill", "none")
                .attr("stroke-width", 3)
                .attr("stroke", (d: any) => {
                    const gradId = `grad-${d.source.data.id}-${d.target.data.id}`.replace(/[^a-zA-Z0-9-]/g, '');
                    
                    defs.append("linearGradient")
                        .attr("id", gradId)
                        .attr("gradientUnits", "userSpaceOnUse")
                        .attr("x1", d.source.x)
                        .attr("y1", d.source.y)
                        .attr("x2", d.target.x)
                        .attr("y2", d.target.y)
                        .selectAll("stop")
                        .data([
                            { offset: "0%", color: colorMap[d.source.data.id] },
                            { offset: "100%", color: colorMap[d.target.data.id] }
                        ])
                        .join("stop")
                        .attr("offset", (s: any) => s.offset)
                        .attr("stop-color", (s: any) => s.color);
                    
                    return `url(#${gradId})`;
                });

            // Draw arrow markers
            const arrow = d3.symbol().type(d3.symbolTriangle).size(60);
            svg.append("g")
                .selectAll("path")
                .data(dag.links())
                .join("path")
                .attr("d", arrow as any)
                .attr("transform", (d: any) => {
                    const points = d.points;
                    const end = points[points.length - 1];
                    const start = points[points.length - 2];
                    const angle = Math.atan2(end[1] - start[1], end[0] - start[0]) * 180 / Math.PI + 90;
                    return `translate(${end[0]}, ${end[1]}) rotate(${angle})`;
                })
                .attr("fill", (d: any) => colorMap[d.target.data.id])
                .attr("stroke", "white")
                .attr("stroke-width", 1);

            // Draw nodes
            const nodes = svg.append("g")
                .selectAll("g")
                .data(dag.nodes())
                .join("g")
                .attr("transform", (d: any) => `translate(${d.x}, ${d.y})`)
                .attr("cursor", "pointer")
                .on("click", (event: any, d: any) => {
                    event.stopPropagation();
                    onClick?.(d.data.id);
                })
                .on("mouseenter", (event: any, d: any) => onHover?.(d.data.id));

            // Hexagon shape
            nodes.append("path")
                .attr("d", "M0 -16 L13.86 -8 L13.86 8 L0 16 L-13.86 8 L-13.86 -8 Z")
                .attr("fill", (d: any) => colorMap[d.data.id])
                .attr("stroke", "#fff")
                .attr("stroke-width", 2);
                
        } catch (error) {
            console.error("Failed to render DAG:", error);
            // Clear on error
            d3.select(svgElement).selectAll("*").remove();
        }
    });

</script>

<div class="h3-graph-container overflow-auto max-h-[400px] w-full bg-surface-50-900-token rounded-lg p-4 inner-shadow">
    {#if selectedCells && selectedCells.size > 0}
        <svg bind:this={svgElement} class="block mx-auto"></svg>
    {:else}
        <div class="flex items-center justify-center h-24 text-surface-400 text-sm italic">
            Select cells to view hierarchy
        </div>
    {/if}
</div>
