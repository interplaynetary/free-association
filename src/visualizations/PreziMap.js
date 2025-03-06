import * as d3 from 'd3';
import { getColorForName } from '../utils/colorUtils.js';
import { calculateFontSize } from '../utils/fontUtils.js';

export function createPreziMap(data, width, height) {
    // State variables
    let root;
    let svg;
    let g;
    let currentNode;
    
    // Constants for layout
    const margin = { top: 20, right: 120, bottom: 20, left: 120 };
    const innerWidth = width - margin.left - margin.right;
    const innerHeight = height - margin.top - margin.bottom;
    const nodeWidth = 160;
    const nodeHeight = 40;
    const nodeRadius = 5;
    const duration = 750; // Animation duration

    // Create SVG
    svg = d3.create("svg")
        .attr("viewBox", [0, 0, width, height])
        .style("font", "10px sans-serif")
        .style("overflow", "visible");
        
    // Create main group with margin
    g = svg.append("g")
        .attr("transform", `translate(${margin.left},${margin.top})`);
    
    // Create tree layout
    const tree = d3.tree()
        .nodeSize([nodeHeight * 2, nodeWidth * 1.5])
        .separation((a, b) => a.parent === b.parent ? 1 : 1.2);
    
    // Create hierarchy from data
    root = d3.hierarchy(data, d => d.childrenArray);
    
    // Assign initial positions
    root.x0 = innerHeight / 2;
    root.y0 = 0;
    
    // Store the initially expanded nodes
    const expandedNodes = new Set();
    
    // Process the root node to get initial tree state
    processNode(root);
    
    // Initial render
    update(root);
    currentNode = root;
    
    // Function to process a node - collapse nodes beyond a certain depth initially
    function processNode(d, depth = 0) {
        if (d.children) {
            if (depth > 1) {
                d._children = d.children;
                d.children = null;
            } else {
                d.children.forEach(child => processNode(child, depth + 1));
            }
        }
    }
    
    // Update the tree visualization
    function update(source) {
        // Compute the new tree layout
        const treeData = tree(root);
        
        // Get all nodes and links
        const nodes = treeData.descendants();
        const links = treeData.descendants().slice(1);
        
        // Normalize for fixed-depth
        nodes.forEach(d => {
            d.y = d.depth * nodeWidth;
        });
        
        // ****************** Nodes section ******************
        
        // Update the nodes
        const node = g.selectAll(".node")
            .data(nodes, d => d.data.name);
        
        // Enter new nodes at the parent's previous position
        const nodeEnter = node.enter().append("g")
            .attr("class", "node")
            .attr("transform", d => `translate(${source.y0},${source.x0})`)
            .attr("cursor", "pointer")
            .on("click", (event, d) => {
                // Toggle children on click
                if (d.children) {
                    d._children = d.children;
                    d.children = null;
                    expandedNodes.delete(d.data.name);
                } else if (d._children) {
                    d.children = d._children;
                    d._children = null;
                    expandedNodes.add(d.data.name);
                }
                
                // Update current node reference
                currentNode = d;
                
                // Update the tree
                update(d);
            });
        
        // Add node rectangle
        nodeEnter.append("rect")
            .attr("width", nodeWidth - 20)
            .attr("height", nodeHeight)
            .attr("x", -nodeWidth / 2 + 10)
            .attr("y", -nodeHeight / 2)
            .attr("rx", nodeRadius)
            .attr("ry", nodeRadius)
            .attr("fill", d => getColorForName(d.data.name))
            .attr("stroke", d => {
                // Add special outline for nodes with non-contributor children
                return (d.data.hasNonContributorChild) ? "#2196f3" : "#fff";
            })
            .attr("stroke-width", d => {
                // Make stroke wider for nodes with non-contributor children
                return (d.data.hasNonContributorChild) ? "3" : "1";
            })
            .style("paint-order", "stroke")
            .style("stroke-linejoin", "round");
        
        // Add node labels
        nodeEnter.append("text")
            .attr("dy", "0.35em")
            .attr("text-anchor", "middle")
            .text(d => d.data.name)
            .style("fill", "#000")
            .style("font-size", "12px")
            .style("font-weight", d => d.data === data ? "bold" : "normal")
            .style("pointer-events", "none")
            .each(function(d) {
                // Truncate text if too long
                const node = d3.select(this);
                const text = d.data.name;
                let textLength = this.getComputedTextLength();
                const maxWidth = nodeWidth - 30;
                
                if (textLength > maxWidth) {
                    let truncatedText = text;
                    while (textLength > maxWidth && truncatedText.length > 0) {
                        truncatedText = truncatedText.slice(0, -1);
                        node.text(truncatedText + "...");
                        textLength = this.getComputedTextLength();
                    }
                }
            });
        
        // Add expand/collapse indicator
        nodeEnter.append("text")
            .attr("class", "indicator")
            .attr("x", nodeWidth / 2 - 25)
            .attr("y", 0)
            .attr("dy", "0.35em")
            .attr("text-anchor", "middle")
            .text(d => d._children ? "+" : d.children ? "-" : "")
            .style("font-size", "14px")
            .style("font-weight", "bold")
            .style("fill", "#666");
        
        // Add points indicator
        nodeEnter.append("text")
            .attr("class", "points")
            .attr("x", 0)
            .attr("y", nodeHeight / 2 + 12)
            .attr("text-anchor", "middle")
            .text(d => {
                if (d.data.points <= 0) return "";
                
                // Calculate percentage relative to siblings and parent
                let totalSiblingPoints = 0;
                let parentPoints = 0;
                
                if (d.parent) {
                    // Get parent points
                    parentPoints = d.parent.data.points || 0;
                    
                    // Calculate total points from all siblings (including self)
                    if (d.parent.children) {
                        d.parent.children.forEach(sibling => {
                            totalSiblingPoints += sibling.data.points || 0;
                        });
                    } else if (d.parent._children) {
                        d.parent._children.forEach(sibling => {
                            totalSiblingPoints += sibling.data.points || 0;
                        });
                    }
                    
                    // Calculate percentage (avoid division by zero)
                    if (totalSiblingPoints > 0) {
                        const siblingPercentage = Math.round((d.data.points / totalSiblingPoints) * 100);
                        return `${siblingPercentage}%`;
                    }
                }
                
                // Fallback to raw points if can't calculate percentage
                return `${d.data.points} pts`;
            })
            .style("font-size", "10px")
            .style("fill", "#666");
        
        // UPDATE
        const nodeUpdate = nodeEnter.merge(node);
        
        // Transition to the proper position for the nodes
        nodeUpdate.transition()
            .duration(duration)
            .attr("transform", d => `translate(${d.y},${d.x})`);
        
        // Update node attributes and style
        nodeUpdate.select("rect")
            .attr("fill", d => getColorForName(d.data.name))
            .attr("stroke", d => {
                return (d.data.hasNonContributorChild) ? "#2196f3" : "#fff";
            });
        
        nodeUpdate.select(".indicator")
            .text(d => d._children ? "+" : d.children ? "-" : "");
        
        // Remove any exiting nodes
        const nodeExit = node.exit().transition()
            .duration(duration)
            .attr("transform", d => `translate(${source.y},${source.x})`)
            .remove();
        
        // On exit reduce the opacity of text labels
        nodeExit.select("text")
            .style("fill-opacity", 0);
        
        // ****************** Links section ******************
        
        // Update the links
        const link = g.selectAll(".link")
            .data(links, d => d.data.name);
        
        // Enter any new links at the parent's previous position
        const linkEnter = link.enter().insert("path", "g")
            .attr("class", "link")
            .attr("d", d => {
                const o = {x: source.x0, y: source.y0};
                return generateLinkPath(o, o);
            })
            .attr("fill", "none")
            .attr("stroke", "#ccc")
            .attr("stroke-width", 1.5);
        
        // UPDATE
        const linkUpdate = linkEnter.merge(link);
        
        // Transition back to the parent element position
        linkUpdate.transition()
            .duration(duration)
            .attr("d", d => generateLinkPath(d, d.parent));
        
        // Remove any exiting links
        link.exit().transition()
            .duration(duration)
            .attr("d", d => {
                const o = {x: source.x, y: source.y};
                return generateLinkPath(o, o);
            })
            .remove();
        
        // Store the old positions for transition
        nodes.forEach(d => {
            d.x0 = d.x;
            d.y0 = d.y;
        });
    }
    
    // Helper function to generate link path - creates a smooth curved path
    function generateLinkPath(s, d) {
        return `
            M ${s.y} ${s.x}
            C ${(s.y + d.y) / 2} ${s.x},
              ${(s.y + d.y) / 2} ${d.x},
              ${d.y} ${d.x}
        `;
    }
    
    // Reset button to go back to root view
    const resetButton = svg.append("g")
        .attr("class", "reset-button")
        .attr("transform", `translate(${width - 60}, 30)`)
        .attr("cursor", "pointer")
        .style("opacity", 1)
        .on("click", () => {
            // First, collapse all nodes
            const collapseAllNodes = (node) => {
                if (node.children) {
                    node._children = node.children;
                    node.children = null;
                    node._children.forEach(collapseAllNodes);
                } else if (node._children) {
                    node._children.forEach(collapseAllNodes);
                }
            };
            
            // Collapse everything
            collapseAllNodes(root);
            
            // Then expand only the first level (direct children of root)
            if (root._children) {
                root.children = root._children;
                root._children = null;
            }
            
            // Clear the set of expanded nodes and only add first-level nodes
            expandedNodes.clear();
            if (root.children) {
                root.children.forEach(child => {
                    expandedNodes.add(child.data.name);
                });
            }
            
            // Reset the view
            update(root);
            currentNode = root;
            
            // Center the view dynamically on the root node
            // Calculate the centering transform based on root node's position
            const centerX = width / 2 - root.y;
            const centerY = height / 2 - root.x;
            
            g.transition()
             .duration(duration)
             .attr("transform", `translate(${centerX},${centerY})`);
        });
    
    resetButton.append("circle")
        .attr("r", 20)
        .attr("fill", "#f5f5f5")
        .attr("stroke", "#333")
        .attr("stroke-width", 2);
    
    resetButton.append("text")
        .attr("text-anchor", "middle")
        .attr("dominant-baseline", "middle")
        .style("font-size", "12px")
        .text("Reset");
    
    // Add pan and zoom behavior
    const zoom = d3.zoom()
        .scaleExtent([0.5, 2])
        .on("zoom", (event) => {
            g.attr("transform", event.transform);
        });
    
    svg.call(zoom);
    
    // Return the required interface
    return {
        element: svg.node(),
        reset: () => {
            update(root);
            currentNode = root;
        },
        getCurrentView: () => currentNode,
        getCurrentData: () => currentNode.data,
        getRoot: () => root,
        update: (newWidth, newHeight) => {
            // Update dimensions
            svg.attr("viewBox", [0, 0, newWidth, newHeight]);
            resetButton.attr("transform", `translate(${newWidth - 60}, 30)`);
            
            // Update tree layout if needed
            update(currentNode);
        }
    };
}
