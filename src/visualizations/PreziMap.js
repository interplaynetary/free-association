import * as d3 from 'd3';
import { getColorForName } from '../utils/colorUtils.js';
import { calculateFontSize } from '../utils/fontUtils.js';

export function createPreziMap(data, width, height) {
    // State variables
    let root;
    let svg;
    let g;
    let currentNode;
    let lastClickedNode; // Track the last clicked node
    
    // Constants for layout
    const margin = { top: 20, right: 120, bottom: 20, left: 120 };
    const innerWidth = width - margin.left - margin.right;
    const innerHeight = height - margin.top - margin.bottom;
    const nodeWidth = 160; // Base width
    const nodeHeight = 40; // Base height
    const minNodeWidth = 80; // Minimum width for smallest nodes
    const maxNodeWidth = 240; // Maximum width for largest nodes
    const minNodeHeight = 30; // Minimum height for smallest nodes
    const maxNodeHeight = 60; // Maximum height for largest nodes
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
    
    // Create a tooltip div that is hidden by default
    const tooltip = d3.create("div")
        .attr("class", "prezi-map-tooltip")
        .style("position", "absolute")
        .style("visibility", "hidden")
        .style("background-color", "white")
        .style("color", "#333")
        .style("padding", "5px 8px")
        .style("border-radius", "3px")
        .style("border", "1px solid #ddd")
        .style("font-size", "12px")
        .style("pointer-events", "none")
        .style("z-index", "10")
        .style("box-shadow", "0 1px 3px rgba(0,0,0,0.2)");
    
    // Append tooltip to body
    document.body.appendChild(tooltip.node());
    
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
    
    // Add initial centering transform to ensure root node is centered
    const initialCenterX = width / 2 - root.y;
    const initialCenterY = height / 2 - root.x;
    g.attr("transform", `translate(${initialCenterX},${initialCenterY})`);
    
    // Function to process a node - collapse all nodes except the root
    function processNode(d, depth = 0) {
        if (depth === 0) {
            // This is the root node
            if (d.children) {
                // Store its children temporarily
                const children = d.children;
                // Set all children as collapsed
                d._children = children;
                d.children = null;
                
                // Process all children (just to be thorough)
                children.forEach(child => processNode(child, depth + 1));
            }
        } else {
            // For non-root nodes, ensure they're collapsed
            if (d.children) {
                d._children = d.children;
                d.children = null;
                d._children.forEach(child => processNode(child, depth + 1));
            }
        }
    }
    
    // Helper function to calculate node size based on percentage
    function calculateNodeSize(d, dimension, minSize, maxSize) {
        // Default size for root or nodes without points
        if (!d.parent || !d.data.points || d.data.points <= 0) {
            return dimension;
        }
        
        // Calculate total points of siblings
        let totalSiblingPoints = 0;
        const siblings = d.parent.children || d.parent._children || [];
        
        siblings.forEach(sibling => {
            totalSiblingPoints += sibling.data.points || 0;
        });
        
        if (totalSiblingPoints <= 0) return dimension;
        
        // Calculate size based on percentage
        const percentage = d.data.points / totalSiblingPoints;
        return minSize + percentage * (maxSize - minSize);
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
                
                // Remove highlight from previous node
                g.selectAll('.last-clicked-highlight')
                    .classed('search-highlight', false)
                    .classed('last-clicked-highlight', false);
                
                // Update current node reference
                currentNode = d;
                
                // Store as last clicked node
                lastClickedNode = d;
                
                // Add highlight to the current node
                g.selectAll('.node')
                    .filter(node => node === d)
                    .select('rect')
                    .classed('search-highlight', true)
                    .classed('last-clicked-highlight', true);
                
                // Update the tree
                update(d);
            });
        
        // Add node rectangle
        nodeEnter.append("rect")
            .attr("width", d => calculateNodeSize(d, nodeWidth, minNodeWidth, maxNodeWidth) - 20)
            .attr("height", d => calculateNodeSize(d, nodeHeight, minNodeHeight, maxNodeHeight))
            .attr("x", d => -(calculateNodeSize(d, nodeWidth, minNodeWidth, maxNodeWidth) / 2) + 10)
            .attr("y", d => -(calculateNodeSize(d, nodeHeight, minNodeHeight, maxNodeHeight) / 2))
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
                const maxWidth = calculateNodeSize(d, nodeWidth, minNodeWidth, maxNodeWidth) - 30;
                
                if (textLength > maxWidth) {
                    let truncatedText = text;
                    while (textLength > maxWidth && truncatedText.length > 0) {
                        truncatedText = truncatedText.slice(0, -1);
                        node.text(truncatedText + "...");
                        textLength = this.getComputedTextLength();
                    }
                    
                    // Mark this node as truncated for tooltip
                    d.isTruncated = true;
                    d.fullName = text;
                } else {
                    d.isTruncated = false;
                }
            });
            
        // Add tooltip behavior to the node rectangle
        nodeEnter.select("rect")
            .on("mouseover", function(event, d) {
                if (d.isTruncated) {
                    tooltip.style("visibility", "visible")
                           .text(d.fullName);
                           
                    // Position tooltip near mouse pointer
                    tooltip.style("top", (event.pageY - 10) + "px")
                           .style("left", (event.pageX + 10) + "px");
                }
            })
            .on("mousemove", function(event) {
                // Move tooltip with mouse
                tooltip.style("top", (event.pageY - 10) + "px")
                       .style("left", (event.pageX + 10) + "px");
            })
            .on("mouseout", function() {
                tooltip.style("visibility", "hidden");
            });
        
        // Add expand/collapse indicator
        nodeEnter.append("text")
            .attr("class", "indicator")
            .attr("x", d => calculateNodeSize(d, nodeWidth, minNodeWidth, maxNodeWidth) / 2 - 25)
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
            .attr("y", d => calculateNodeSize(d, nodeHeight, minNodeHeight, maxNodeHeight) / 2 + 12)
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
            .attr("width", d => calculateNodeSize(d, nodeWidth, minNodeWidth, maxNodeWidth) - 20)
            .attr("height", d => calculateNodeSize(d, nodeHeight, minNodeHeight, maxNodeHeight))
            .attr("x", d => -(calculateNodeSize(d, nodeWidth, minNodeWidth, maxNodeWidth) / 2) + 10)
            .attr("y", d => -(calculateNodeSize(d, nodeHeight, minNodeHeight, maxNodeHeight) / 2))
            .attr("fill", d => getColorForName(d.data.name))
            .attr("stroke", d => {
                return (d.data.hasNonContributorChild) ? "#2196f3" : "#fff";
            });
        
        nodeUpdate.select(".indicator")
            .attr("x", d => calculateNodeSize(d, nodeWidth, minNodeWidth, maxNodeWidth) / 2 - 25)
            .text(d => d._children ? "+" : d.children ? "-" : "");
        
        nodeUpdate.select(".points")
            .attr("y", d => calculateNodeSize(d, nodeHeight, minNodeHeight, maxNodeHeight) / 2 + 12);
        
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
        
        // Reapply highlight to the last clicked node if it exists
        if (lastClickedNode) {
            g.selectAll('.node')
                .filter(d => d.data.name === lastClickedNode.data.name)
                .select('rect')
                .classed('search-highlight', true)
                .classed('last-clicked-highlight', true);
        }
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
            
            // Also update last clicked node to root
            lastClickedNode = root;
            
            // Clear existing highlights
            g.selectAll('.node rect')
                .classed('search-highlight', false)
                .classed('last-clicked-highlight', false);
                
            // Highlight the root node as the last clicked
            g.selectAll('.node')
                .filter(d => d === root)
                .select('rect')
                .classed('search-highlight', true)
                .classed('last-clicked-highlight', true);
            
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
            lastClickedNode = root;
        },
        getCurrentView: () => currentNode,
        getCurrentData: () => currentNode.data,
        getLastClickedNode: () => lastClickedNode || root, // Return last clicked node or root if none clicked
        getLastClickedData: () => lastClickedNode ? lastClickedNode.data : root.data, // Return data of last clicked node
        getRoot: () => root,
        update: (newWidth, newHeight) => {
            // Update dimensions
            svg.attr("viewBox", [0, 0, newWidth, newHeight]);
            resetButton.attr("transform", `translate(${newWidth - 60}, 30)`);
            
            // Update tree layout if needed
            update(currentNode);
        },
        cleanup: () => {
            // Remove tooltip from DOM when visualization is removed
            const tooltipNode = document.querySelector('.prezi-map-tooltip');
            if (tooltipNode && tooltipNode.parentNode) {
                tooltipNode.parentNode.removeChild(tooltipNode);
            }
        },
        // Highlight nodes that match search results
        highlightNodes: (matchedNodes) => {
            // Clear all highlights (both search and last-clicked)
            g.selectAll('.node rect')
                .classed('search-highlight', false)
                .classed('last-clicked-highlight', false);
            
            // If there are matches
            if (matchedNodes.length > 0) {
                // Find the d3 node for the first match
                const firstMatch = matchedNodes[0];
                
                // Find the node in the d3 hierarchy
                const findNode = (node) => {
                    if (node.data.name === firstMatch.name) {
                        return node;
                    }
                    if (node.children) {
                        for (const child of node.children) {
                            const found = findNode(child);
                            if (found) return found;
                        }
                    }
                    if (node._children) {
                        for (const child of node._children) {
                            const found = findNode(child);
                            if (found) return found;
                        }
                    }
                    return null;
                };
                
                const matchedNode = findNode(root);
                
                if (matchedNode) {
                    // Update lastClickedNode to the first search match
                    lastClickedNode = matchedNode;
                    
                    // Also update currentNode to treat this as the fully selected node
                    currentNode = matchedNode;
                    
                    // Expand parent nodes to make the match visible
                    let parent = matchedNode.parent;
                    while (parent) {
                        if (parent._children) {
                            parent.children = parent._children;
                            parent._children = null;
                            expandedNodes.add(parent.data.name);
                        }
                        parent = parent.parent;
                    }
                    
                    // Update visualization to show expanded nodes
                    update(root);
                    
                    // Re-apply last-clicked highlight for first match
                    g.selectAll('.node')
                        .filter(d => d === matchedNode)
                        .select('rect')
                        .classed('last-clicked-highlight', true);
                    
                    // Center view on the match
                    const centerX = width / 2 - matchedNode.y;
                    const centerY = height / 2 - matchedNode.x;
                    
                    g.transition()
                        .duration(duration)
                        .attr("transform", `translate(${centerX},${centerY})`);
                }
            }
        },
        // Clear search highlights
        clearHighlights: () => {
            // Clear only search highlights, not last clicked highlights
            g.selectAll('.node rect')
                .filter(function() {
                    return !d3.select(this).classed('last-clicked-highlight');
                })
                .classed('search-highlight', false);
        }
    };
}
