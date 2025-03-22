import * as d3 from 'd3';
import { getColorForName } from '../utils/colorUtils';
import { TreeNode } from '../models/TreeNode';

/**
 * Creates a stacked bar chart that can be displayed horizontally or vertically
 * similar to the pie chart but for bar representation of proportional data.
 * 
 * @param data - The TreeNode containing the data to visualize
 * @param containerId - The ID of the container element to render the chart in
 * @param orientation - Whether to display the chart 'horizontal' or 'vertical' (default: 'horizontal')
 * @returns The created SVG node
 */
export function createStackedBar(
    data: TreeNode, 
    containerId: string,
    orientation: 'horizontal' | 'vertical' = 'horizontal'
) {
    // Get the container dimensions
    const container = document.getElementById(containerId);
    if (!container) {
        throw new Error(`Could not find container element with id: ${containerId}`);
    }
    
    const width = container.clientWidth;
    const height = container.clientHeight;
    
    // Get mutualFulfillmentDistribution from root node (same as pie chart)
    console.log('Creating stacked bar for data:', data);
    const mutualFulfillmentDistribution = data.mutualFulfillmentDistribution;
    console.log('mutualFulfillmentDistribution for bar:', mutualFulfillmentDistribution);
    
    // Convert to array for D3
    const dataArray = Array.from(mutualFulfillmentDistribution.entries())
        .map(([node, value]) => ({
            name: node.name,
            value: value,
            node: node
        }));
    
    // Create SVG
    const svg = d3.create("svg")
        .attr("width", width)
        .attr("height", height)
        .attr("class", "stacked-bar-chart");
    
    const margin = {
        top: 30,
        right: 30,
        bottom: 50,
        left: 50
    };
    
    const chartWidth = width - margin.left - margin.right;
    const chartHeight = height - margin.top - margin.bottom;
    
    // Create chart group
    const chartGroup = svg.append("g")
        .attr("transform", `translate(${margin.left}, ${margin.top})`);
    
    // Set up the scales
    const xScale = d3.scaleLinear()
        .domain([0, 1]) // For percentages (0-100%)
        .range(orientation === 'horizontal' 
            ? [0, chartWidth] 
            : [0, chartHeight]);
    
    // Set up positioning based on orientation
    let barThickness: number;
    let maxLabelWidth: number;
    
    if (orientation === 'horizontal') {
        barThickness = chartHeight * 0.8; // 80% of height for the bar
        
        // Create the stacked bar (horizontal)
        let currentX = 0;
        
        dataArray.forEach(d => {
            const segmentWidth = xScale(d.value);
            
            chartGroup.append("rect")
                .attr("x", currentX)
                .attr("y", (chartHeight - barThickness) / 2)
                .attr("width", segmentWidth)
                .attr("height", barThickness)
                .attr("fill", getColorForName(d.name))
                .append("title")
                .text(`${d.name}: ${(d.value * 100).toFixed(1)}%`);
                
            // Add a label if there's enough space
            if (segmentWidth > 40) {
                chartGroup.append("text")
                    .attr("x", currentX + segmentWidth / 2)
                    .attr("y", (chartHeight - barThickness) / 2 + barThickness / 2)
                    .attr("text-anchor", "middle")
                    .attr("dominant-baseline", "middle")
                    .attr("fill", "white")
                    .attr("font-size", `${Math.min(16, barThickness * 0.3)}px`)
                    .text(`${(d.value * 100).toFixed(0)}%`);
            }
            
            currentX += segmentWidth;
        });
        
        // Add labels below
        const legend = svg.append("g")
            .attr("class", "legend")
            .attr("transform", `translate(${margin.left}, ${margin.top + chartHeight + 10})`);
        
        // Create color squares with labels
        dataArray.forEach((d, i) => {
            const legendItem = legend.append("g")
                .attr("transform", `translate(${i * (chartWidth / dataArray.length)}, 0)`);
                
            legendItem.append("rect")
                .attr("width", 12)
                .attr("height", 12)
                .attr("fill", getColorForName(d.name));
                
            legendItem.append("text")
                .attr("x", 16)
                .attr("y", 9)
                .attr("font-size", "10px")
                .text(d.name);
        });
    } else {
        // Vertical orientation
        barThickness = chartWidth * 0.8; // 80% of width for the bar
        
        // Create y-scale for the vertical chart
        const yScale = d3.scaleLinear()
            .domain([0, 1])
            .range([chartHeight, 0]);
            
        // Create the stacked bar (vertical)
        let currentY = chartHeight;
        
        dataArray.forEach(d => {
            const segmentHeight = chartHeight - yScale(d.value);
            
            chartGroup.append("rect")
                .attr("x", (chartWidth - barThickness) / 2)
                .attr("y", currentY - segmentHeight)
                .attr("width", barThickness)
                .attr("height", segmentHeight)
                .attr("fill", getColorForName(d.name))
                .append("title")
                .text(`${d.name}: ${(d.value * 100).toFixed(1)}%`);
                
            // Add a label if there's enough space
            if (segmentHeight > 40) {
                chartGroup.append("text")
                    .attr("x", (chartWidth - barThickness) / 2 + barThickness / 2)
                    .attr("y", currentY - segmentHeight / 2)
                    .attr("text-anchor", "middle")
                    .attr("dominant-baseline", "middle")
                    .attr("fill", "white")
                    .attr("font-size", `${Math.min(16, barThickness * 0.3)}px`)
                    .text(`${(d.value * 100).toFixed(0)}%`);
            }
            
            currentY -= segmentHeight;
        });
        
        // Add legend on the right
        const legend = svg.append("g")
            .attr("class", "legend")
            .attr("transform", `translate(${margin.left + chartWidth + 10}, ${margin.top})`);
        
        // Create color squares with labels
        dataArray.forEach((d, i) => {
            const legendItem = legend.append("g")
                .attr("transform", `translate(0, ${i * 20})`);
                
            legendItem.append("rect")
                .attr("width", 12)
                .attr("height", 12)
                .attr("fill", getColorForName(d.name));
                
            legendItem.append("text")
                .attr("x", 16)
                .attr("y", 9)
                .attr("font-size", "10px")
                .text(d.name);
        });
    }
    
    // Add a title
    svg.append("text")
        .attr("x", width / 2)
        .attr("y", margin.top / 2)
        .attr("text-anchor", "middle")
        .attr("font-size", "16px")
        .attr("font-weight", "bold")
        .text("Mutual Fulfillment");
    
    // Return the created SVG node
    return svg.node();
}

/**
 * Creates a component with multiple stacked bars for comparison
 * 
 * @param dataItems - Array of TreeNodes to visualize as separate bars
 * @param containerId - The ID of the container element to render the chart in
 * @param orientation - Whether to display the chart 'horizontal' or 'vertical' (default: 'horizontal')
 * @param labels - Optional array of labels for each bar
 * @returns The created SVG node
 */
export function createMultiStackedBar(
    dataItems: TreeNode[], 
    containerId: string,
    orientation: 'horizontal' | 'vertical' = 'horizontal',
    labels: string[] = []
) {
    // Get the container dimensions
    const container = document.getElementById(containerId);
    if (!container) {
        throw new Error(`Could not find container element with id: ${containerId}`);
    }
    
    const width = container.clientWidth;
    const height = container.clientHeight;
    
    // Process all data items
    const processedData = dataItems.map((data, index) => {
        const distribution = data.mutualFulfillmentDistribution;
        return {
            node: data,
            label: labels[index] || `Bar ${index + 1}`,
            distribution: Array.from(distribution.entries())
                .map(([node, value]) => ({
                    name: node.name,
                    value: value,
                    node: node
                }))
        };
    });
    
    // Create SVG
    const svg = d3.create("svg")
        .attr("width", width)
        .attr("height", height)
        .attr("class", "multi-stacked-bar-chart");
    
    const margin = {
        top: 50,
        right: 100,
        bottom: 50,
        left: 100
    };
    
    const chartWidth = width - margin.left - margin.right;
    const chartHeight = height - margin.top - margin.bottom;
    
    // Create chart group
    const chartGroup = svg.append("g")
        .attr("transform", `translate(${margin.left}, ${margin.top})`);
    
    // Calculate dimensions based on orientation and number of bars
    const barCount = processedData.length;
    const barGap = 20; // Gap between bars
    
    let barThickness: number;
    let barLength: number;
    
    if (orientation === 'horizontal') {
        // Horizontal layout - bars stacked vertically
        barLength = chartWidth;
        barThickness = (chartHeight - (barGap * (barCount - 1))) / barCount;
        
        // Create scales
        const xScale = d3.scaleLinear()
            .domain([0, 1])
            .range([0, barLength]);
        
        // Create each bar
        processedData.forEach((barData, barIndex) => {
            const barY = barIndex * (barThickness + barGap);
            
            // Draw label for this bar
            chartGroup.append("text")
                .attr("x", -10)
                .attr("y", barY + barThickness / 2)
                .attr("text-anchor", "end")
                .attr("dominant-baseline", "middle")
                .attr("font-size", "12px")
                .text(barData.label);
            
            // Draw the segmented bar
            let currentX = 0;
            
            barData.distribution.forEach(segment => {
                const segmentWidth = xScale(segment.value);
                
                chartGroup.append("rect")
                    .attr("x", currentX)
                    .attr("y", barY)
                    .attr("width", segmentWidth)
                    .attr("height", barThickness)
                    .attr("fill", getColorForName(segment.name))
                    .append("title")
                    .text(`${segment.name}: ${(segment.value * 100).toFixed(1)}%`);
                    
                // Add a label if there's enough space
                if (segmentWidth > 40) {
                    chartGroup.append("text")
                        .attr("x", currentX + segmentWidth / 2)
                        .attr("y", barY + barThickness / 2)
                        .attr("text-anchor", "middle")
                        .attr("dominant-baseline", "middle")
                        .attr("fill", "white")
                        .attr("font-size", `${Math.min(12, barThickness * 0.4)}px`)
                        .text(`${(segment.value * 100).toFixed(0)}%`);
                }
                
                currentX += segmentWidth;
            });
        });
        
    } else {
        // Vertical layout - bars next to each other
        barLength = chartHeight;
        barThickness = (chartWidth - (barGap * (barCount - 1))) / barCount;
        
        // Create scales
        const yScale = d3.scaleLinear()
            .domain([0, 1])
            .range([barLength, 0]);
        
        // Create each bar
        processedData.forEach((barData, barIndex) => {
            const barX = barIndex * (barThickness + barGap);
            
            // Draw label for this bar
            chartGroup.append("text")
                .attr("x", barX + barThickness / 2)
                .attr("y", chartHeight + 15)
                .attr("text-anchor", "middle")
                .attr("font-size", "12px")
                .text(barData.label);
            
            // Draw the segmented bar
            let currentY = chartHeight;
            
            barData.distribution.forEach(segment => {
                const segmentHeight = chartHeight - yScale(segment.value);
                
                chartGroup.append("rect")
                    .attr("x", barX)
                    .attr("y", currentY - segmentHeight)
                    .attr("width", barThickness)
                    .attr("height", segmentHeight)
                    .attr("fill", getColorForName(segment.name))
                    .append("title")
                    .text(`${segment.name}: ${(segment.value * 100).toFixed(1)}%`);
                    
                // Add a label if there's enough space
                if (segmentHeight > 40) {
                    chartGroup.append("text")
                        .attr("x", barX + barThickness / 2)
                        .attr("y", currentY - segmentHeight / 2)
                        .attr("text-anchor", "middle")
                        .attr("dominant-baseline", "middle")
                        .attr("fill", "white")
                        .attr("font-size", `${Math.min(12, barThickness * 0.4)}px`)
                        .text(`${(segment.value * 100).toFixed(0)}%`);
                }
                
                currentY -= segmentHeight;
            });
        });
    }
    
    // Create a unified legend
    // Get all unique category names across all data items
    const allCategories = new Set<string>();
    processedData.forEach(barData => {
        barData.distribution.forEach(segment => {
            allCategories.add(segment.name);
        });
    });
    
    // Create the legend
    const legendGroup = svg.append("g")
        .attr("class", "legend")
        .attr("transform", `translate(${width - margin.right + 20}, ${margin.top})`);
    
    Array.from(allCategories).forEach((category, i) => {
        const legendItem = legendGroup.append("g")
            .attr("transform", `translate(0, ${i * 20})`);
            
        legendItem.append("rect")
            .attr("width", 12)
            .attr("height", 12)
            .attr("fill", getColorForName(category));
            
        legendItem.append("text")
            .attr("x", 16)
            .attr("y", 9)
            .attr("font-size", "10px")
            .text(category);
    });
    
    // Add a title
    svg.append("text")
        .attr("x", width / 2)
        .attr("y", margin.top / 2)
        .attr("text-anchor", "middle")
        .attr("font-size", "16px")
        .attr("font-weight", "bold")
        .text("Mutual Fulfillment Comparison");
    
    // Return the created SVG node
    return svg.node();
}