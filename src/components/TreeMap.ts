import * as d3 from 'd3';
import { getColorForName } from '../utils/colorUtils';
import { calculateFontSize, name } from '../utils/fontUtils';
import { TreeNode } from '../models/TreeNode';
import { gun } from '../models/Gun';

// TODO:
// Extract various functions into helpers (in particular gun.js related stuff)
// when we navigate into a type, the backbutton no longer works
// we can no longer navigate to a type
// when adding children we no longer automatically zoom-in (commons has working functionality for this)


// Create a type for the treemap return value
type TreemapInstance = {
    element: HTMLElement;
    update: (width: number, height: number) => void;
    destroy: () => void;
    getCurrentView: () => TreeNode;
    getCurrentData: () => TreeNode;
    zoomin: (node: TreeNode) => void;
    zoomout: (node: TreeNode) => void;
};

export function createTreemap(data: TreeNode, width: number, height: number): TreemapInstance {
    // State variables for growth animation
    let growthInterval: number | null = null;
    let growthTimeout: number | null = null;
    const GROWTH_RATE = (d: d3.HierarchyRectangularNode<TreeNode>) => d.data.points * 0.05;
    const GROWTH_TICK = 50;
    const GROWTH_DELAY = 500;
    let isGrowing = false;
    const SHRINK_RATE = (d: d3.HierarchyRectangularNode<TreeNode>) => d.data.points * -0.05; // Negative growth rate for shrinking

    // Helper functions
    const uid = (function() {
        let id = 0;
        return function(prefix: string) {
            const uniqueId = `${prefix}-${++id}`;
            return { id: uniqueId, href: `#${uniqueId}` };
        };
    })();

    // Create scales
    const x = d3.scaleLinear().rangeRound([0, width]);
    const y = d3.scaleLinear().rangeRound([0, height]);

    // Create hierarchy
    let hierarchy = d3.hierarchy<TreeNode>(data, d => Array.from(d.children.values()))
        .sum(d => d.points)
        .each(d => { (d as any).value = d.data.points || 0; });

    // Create treemap layout
    let root = d3.treemap().tile(tile)(hierarchy);
    let currentView = root;

    // Set initial domains
    x.domain([root.x0, root.x1]);
    y.domain([root.y0, root.y1]);

    // Create SVG
    const svg = d3.create("svg")
        .attr("viewBox", [0.5, -50.5, width, height + 50])
        .style("font", "10px sans-serif");

    // Create initial group
    let group = svg.append("g")
        .call(render, root);

    function tile(node: d3.HierarchyRectangularNode<TreeNode>, x0: number, y0: number, x1: number, y1: number) {
        if (!node.children) return;
        
        // Calculate available space
        const availableWidth = x1 - x0;
        const availableHeight = y1 - y0;
        
        // Ensure values match points
        node.children.forEach(child => {
            (child as any).value = child.data.points || 0;
        });
        
        // Create a simpler hierarchy object that matches d3's expectations
        const tempRoot = {
            children: node.children.map(child => ({
                data: child.data,
                value: child.data.points || 0
            }))
        };
        
        // Create hierarchy and apply squarify directly
        const tempHierarchy = d3.hierarchy(tempRoot)
            .sum(d => (d as any).value);
            
        // Apply squarify directly with the available space
        d3.treemapSquarify(tempHierarchy as d3.HierarchyRectangularNode<any>, 0, 0, availableWidth, availableHeight);
        
        // Transfer positions back to our nodes
        node.children.forEach((child, i) => {
            if (tempHierarchy.children && tempHierarchy.children[i]) {
                const tempNode = tempHierarchy.children[i] as d3.HierarchyRectangularNode<any>;
                child.x0 = x0 + tempNode.x0;
                child.x1 = x0 + tempNode.x1;
                child.y0 = y0 + tempNode.y0;
                child.y1 = y0 + tempNode.y1;
            }
        });
    }
  
    function position(group, root) {
        // Update all g elements except the navigation buttons
        group.selectAll("g:not(.home-button):not(.add-button):not(.peer-button)")
            .attr("transform", d => {
                if (!d || typeof d.x0 === 'undefined') return '';
                return d === root ? `translate(0,-50)` : `translate(${x(d.x0)},${y(d.y0)})`;
            });

        // Update home button position (keep it at the left)
        group.selectAll(".home-button")
            .attr("transform", "translate(20, 25)");
            
        // Update peer button position (keep it to the left of the add button)
        group.selectAll(".peer-button")
            .attr("transform", d => {
                const rectWidth = d === root ? width : x(d.x1) - x(d.x0);
                return `translate(${rectWidth - 60}, 25)`;
            });
            
        // Update add button position (keep it at the right)
        group.selectAll(".add-button")
            .attr("transform", d => {
                const rectWidth = d === root ? width : x(d.x1) - x(d.x0);
                return `translate(${rectWidth - 20}, 25)`;
            });

        group.selectAll("rect")
            .attr("width", d => {
                if (!d || typeof d.x0 === 'undefined') return 0;
                return d === root ? width : x(d.x1) - x(d.x0);
            })
            .attr("height", d => {
                if (!d || typeof d.y0 === 'undefined') return 0;
                return d === root ? 50 : y(d.y1) - y(d.y0);
            });

        // Update type tags container position
        group.selectAll(".type-tags-container")
            .style("opacity", () => (window as any).app.isGrowingActive ? 0 : 1) // Hide during growing
            .style("transition", "opacity 0.15s ease") // Add smooth transition
            .attr("transform", d => {
                if (!d || typeof d.x0 === 'undefined') return '';
                const rectWidth = d === root ? width : x(d.x1) - x(d.x0);
                const rectHeight = d === root ? 50 : y(d.y1) - y(d.y0);
                
                // Position centered horizontally, below the text
                // Calculate vertical position based on text lines
                const textLines = d === root ? 1 : 
                                  d.data.name.split(/(?=[A-Z][^A-Z])/g).length;
                
                // Offset from center - move down by half the text height plus padding
                const fontSize = calculateFontSize(d, rectWidth, rectHeight, root, x, y, currentView);
                const fontSizeNumber = typeof fontSize === 'number' ? 
                    fontSize : 12; // Fallback to reasonable default
                const verticalOffset = (textLines * 1.2 * fontSizeNumber / 2) + 10;
                
                return `translate(${rectWidth / 2}, ${rectHeight / 2 + verticalOffset})`;
            });
    }
  
    function render(group: d3.Selection<SVGGElement, unknown, null, undefined>, root: d3.HierarchyRectangularNode<TreeNode>) {
      // First, create groups only for nodes with value or root
      const nodeData = (root.children || []).concat(root);

        const node = group
            .selectAll<SVGGElement, d3.HierarchyRectangularNode<TreeNode>>("g")
            .data(nodeData)
            .join("g")
            .filter(d => d === root || d.data.points > 0);

        node.append("title")
            .text(d => `${name(d)}\n`);

        node.selectAll("text").remove();

        node.append("rect")
            .attr("id", d => {
                (d as any).leafUid = uid("leaf");
                
                // Also add the node's actual ID to make it findable for drag-drop
                if (d === root) return (d as any).leafUid.id;
                if (d.data.id) return `leaf-${d.data.id}`;
                return (d as any).leafUid.id;
            })
            .attr("fill", d => {
                if (d === root) return "#fff";
                return getColorForName(d.data.name);
            })
            .attr("stroke", d => {
                // Only add special outline for nodes with non-contributor children - now in blue
                return (d.data.hasDirectContributionChild) ? "#2196f3" : "#fff";
            })
            .attr("stroke-width", d => {
                // Only make stroke wider for nodes with non-contributor children
                return (d.data.hasDirectContributionChild) ? "3" : "2";
            })

        node.append("clipPath")
            .attr("id", d => {
                (d as any).clipUid = uid("clip");
                return (d as any).clipUid.id;
            })
            .append("use")
            .attr("xlink:href", d => (d as any).leafUid.href);

        node.append("text")
            .attr("clip-path", d => (d as any).clipUid)
            .attr("font-weight", d => d === root ? "bold" : null)
            .style("user-select", "none")
            .style("-webkit-user-select", "none")
            .style("-moz-user-select", "none")
            .style("-ms-user-select", "none")
            .attr("transform", d => {
                const rectWidth = d === root ? width : x(d.x1) - x(d.x0);
                const rectHeight = d === root ? 50 : y(d.y1) - y(d.y0);
                return `translate(${rectWidth / 2},${rectHeight / 2})`;
            })
            .style("text-anchor", "middle")
            .style("dominant-baseline", "middle")
            .style("font-size", d => {
                const rectWidth = d === root ? width : x(d.x1) - x(d.x0);
                const rectHeight = d === root ? 50 : y(d.y1) - y(d.y0);
                return calculateFontSize(d, rectWidth, rectHeight, root, x, y, currentView) + "px";
            })
            .selectAll("tspan")
            .data(d => {
                if (d === root) return [name(d)];
                return d.data.name.split(/(?=[A-Z][^A-Z])/g);
            })
            .join("tspan")
            .attr("x", 0)
            .attr("dy", (d, i, nodes) => {
                if (i === 0) {
                    // Move first line up by half the total height of all lines
                    return `${-(nodes.length - 1) * 1.2 / 2}em`;
                }
                return "1.2em";  // Standard line spacing for subsequent lines
            })
            .text(d => d);
  
      group.call(position, root);

    // Replace type circles container with tag pills container and add tag button
    const typeContainer = node.append("g")
        .attr("class", "type-tags-container")
        .style("opacity", () => (window as any).app.isGrowingActive ? 0 : 1) // Hide during growing
        .style("transition", "opacity 0.15s ease") // Add smooth transition
        .attr("transform", d => {
            const rectWidth = d === root ? width : x(d.x1) - x(d.x0);
            const rectHeight = d === root ? 50 : y(d.y1) - y(d.y0);
            
            // Position centered horizontally, below the text
            // Calculate vertical position based on text lines
            const textLines = d === root ? 1 : 
                              d.data.name.split(/(?=[A-Z][^A-Z])/g).length;
            
            // Offset from center - move down by half the text height plus padding
            const fontSize = calculateFontSize(d, rectWidth, rectHeight, root, x, y, currentView);
            const fontSizeNumber = typeof fontSize === 'number' ? 
                fontSize : 12; // Fallback to reasonable default
            const verticalOffset = (textLines * 1.2 * fontSizeNumber / 2) + 10;
            
            return `translate(${rectWidth / 2}, ${rectHeight / 2 + verticalOffset})`;
        });

    // Add tag pills for each type
    typeContainer.each(function(d: d3.HierarchyRectangularNode<TreeNode>) {
        if (!d || d === root) return; // Skip for root node
        
        const container = d3.select(this);
        
        // Calculate rect dimensions for space check
        const rectWidth = d === root ? width : x(d.x1) - x(d.x0);
        const rectHeight = d === root ? 50 : y(d.y1) - y(d.y0);
        
        // Skip all additions if rect is too small
        if (rectWidth < 60 || rectHeight < 60) return;
        
        // Get type array safely using public getter
        const typesArray = d.data.typesMap ? Array.from(d.data.typesMap.values()) : [];
        
        // Create a tag wrapper to hold all pills in a flex layout
        const tagWrapper = container.append("foreignObject")
            .attr("class", "tag-wrapper")
            .attr("x", -rectWidth / 2 + 10) // Adjust left position with padding
            .attr("y", 0)
            .attr("width", rectWidth - 20) // Full width minus padding
            .attr("height", 60) // Fixed height to contain a couple rows of tags
            .append("xhtml:div")
            .style("display", "flex")
            .style("flex-wrap", "wrap")
            .style("justify-content", "center")
            .style("gap", "4px")
            .style("width", "100%")
            .style("height", "100%")
            .style("overflow", "hidden");
        
        // Add the "Add tag" button
        const addTagButton = tagWrapper.append("div")
            .attr("class", "add-tag-button")
            .style("display", "flex")
            .style("align-items", "center")
            .style("justify-content", "center")
            .style("border-radius", "10px")
            .style("background", "#e0e0e0")
            .style("padding", "2px 8px")
            .style("margin", "2px")
            .style("cursor", "pointer")
            .style("height", "20px")
            .style("font-size", "10px")
            .style("white-space", "nowrap")
            .style("color", "#333")
            .text("+");
        
        // Add existing type pills
        typesArray.forEach((type: TreeNode) => {
            const tagPill = tagWrapper.append("div")
                .attr("class", "tag-pill")
                .attr("data-type-id", type.id)
                .style("display", "flex")
                .style("align-items", "center")
                .style("border-radius", "10px")
                .style("background", getColorForName(type.name))
                .style("padding", "2px 8px")
                .style("margin", "2px")
                .style("height", "20px")
                .style("font-size", "10px")
                .style("white-space", "nowrap");
                
            // Tag name
            tagPill.append("span")
                .style("color", "white")
                .style("margin-right", "4px")
                .style("text-shadow", "0 1px 1px rgba(0,0,0,0.3)")
                .text(() => {
                    // Truncate text if too long
                    const name = type.name;
                    return name.length > 10 ? name.substring(0, 8) + "..." : name;
                });
                
            // X button
            tagPill.append("span")
                .attr("class", "remove-tag")
                .style("cursor", "pointer")
                .style("color", "white")
                .style("font-size", "12px")
                .style("line-height", "10px")
                .style("opacity", "0.8")
                .style("font-weight", "bold")
                .text("×")
                .on("click", (event) => {
                    event.stopPropagation();
                    console.log('Removing type:', type.name, 'from node:', d.data.name);
                    
                    // Remove the type from the node
                    d.data.removeType(type);
                    
                    // Remove tag pill with animation
                    d3.select(event.target.parentNode)
                        .style("transition", "all 0.3s")
                        .style("opacity", "0")
                        .style("transform", "scale(0.8)")
                        .remove();
                });
            
            // Click handler for navigating to type's tree
            tagPill.on("click", (event) => {
                // Don't handle if clicking on X button
                if (event.target.classList.contains("remove-tag")) return;
                
                console.log('Tag clicked, navigating to type:', type.name);
                event.stopPropagation();
                
                // Update current view
                currentView = type;
                
                // Clear existing content and recreate group
                group.selectAll("*").remove();
                
                // Apply treemap layout
                const treemap = d3.treemap().tile(tile);
                root = treemap(hierarchy);
                
                // Reset domains
                x.domain([root.x0, root.x1]);
                y.domain([root.y0, root.y1]);
                
                // Render new view
                render(group, root);
            });
            
            // Add tooltip
            tagPill.attr("title", `${type.name}: Click to view tree, click × to remove`);
        });
        
        // Create search dropdown for add tag button
        addTagButton.on("click", (event) => {
            event.stopPropagation();
            
            // Remove any existing dropdown
            d3.selectAll(".type-search-dropdown").remove();
            
            // Create dropdown container - attach to body instead of container to ensure it's on top
            const dropdownContainer = d3.select("body").append("div")
                .attr("class", "type-search-dropdown")
                .style("position", "fixed") // Use fixed instead of absolute for better positioning
                .style("top", `${event.clientY + 20}px`) // Use clientY instead of pageY for fixed positioning
                .style("left", `${event.clientX - 100}px`)
                .style("width", "200px")
                .style("height", "250px")
                .style("background", "white")
                .style("border", "1px solid #ccc")
                .style("border-radius", "4px")
                .style("box-shadow", "0 4px 8px rgba(0,0,0,0.1)")
                .style("overflow", "hidden")
                .style("display", "flex")
                .style("flex-direction", "column")
                .style("z-index", "99999"); // Even higher z-index to ensure it's above everything
            
            // Store any active gun subscriptions for cleanup
            let gunSubscriptions = [];
            
            // Function to clean up and close dropdown
            function closeDropdown() {
                // Unsubscribe from all Gun listeners
                gunSubscriptions.forEach(unsub => {
                    if (typeof unsub === 'function') {
                        unsub();
                    }
                });
                
                // Remove dropdown element
                dropdownContainer.remove();
                
                // Remove global click handler
                d3.select("body").on("click.dropdown", null);
            }
            
            // Add header with search input and close button
            const headerContainer = dropdownContainer.append("div")
                .style("display", "flex")
                .style("align-items", "center")
                .style("border-bottom", "1px solid #eee")
                .style("padding", "4px");
            
            // Add search input
            const searchInput = headerContainer.append("input")
                .attr("type", "text")
                .attr("placeholder", "Search users...")
                .style("padding", "8px")
                .style("border", "none")
                .style("flex", "1")
                .style("outline", "none")
                .style("font-size", "12px");
            
            // Add close button
            const closeButton = headerContainer.append("div")
                .style("padding", "4px 8px")
                .style("cursor", "pointer")
                .style("color", "#666")
                .style("font-weight", "bold")
                .text("×")
                .on("click", closeDropdown);
            
            // Create results container
            const resultsContainer = dropdownContainer.append("div")
                .style("overflow-y", "auto")
                .style("flex", "1");
                
            // Function to load and filter users
            async function loadUsers(filterText = "") {
                resultsContainer.html(""); // Clear previous results
                
                // Show loading indicator
                const loadingIndicator = resultsContainer.append("div")
                    .attr("class", "loading-indicator")
                    .style("padding", "8px")
                    .style("text-align", "center")
                    .style("color", "#888")
                    .style("font-size", "11px")
                    .text("Loading...");
                
                // Collect users for batched display
                const users: Array<{id: string, name: string}> = [];
                let foundUsers = false;
                let userCount = 0;
                
                // Get users from Gun
                const gunQuery = gun.get('users').map().on(async (userData, userId) => {
                    console.log('[TreeMap] Found user:', userData, userId);
                    const user = await TreeNode.fromId(userId).then(user => {
                        console.log('[TreeMap] Recreated user:', user);
                        console.log('[TreeMap] Recreated Username:', user.name);
                        return user;
                    });
                    if (!user || userId === d.data.app.rootId) return;
                    
                    // Get user name from the users path first, then fall back to userId
                    // This ensures we use the stored name when available
                    const userName = user.name;
                    
                    // Filter by name if search text exists
                    if (filterText && !userName.toLowerCase().includes(filterText.toLowerCase())) {
                        return;
                    }
                    
                    // Check if this user is already a type on the node
                    const isAlreadyType = d.data.typesMap && d.data.typesMap.has(userId);
                    if (isAlreadyType) return;
                    
                    // Add to users collection if not already there
                    if (!users.some(u => u.id === userId)) {
                        users.push({
                            id: userId,  // This is the node ID we'll use to add as a type
                            name: userName
                        });
                        userCount++;
                        foundUsers = true;
                        
                        // Update the display immediately
                        updateUsersList();
                    }
                });
                
                // Add subscription to cleanup list
                gunSubscriptions.push(gunQuery);
                
                // Function to update the displayed list
                function updateUsersList() {
                    // Only update if we have users
                    if (users.length === 0) return;
                    
                    // Remove loading indicator once we have data
                    if (loadingIndicator) {
                        loadingIndicator.remove();
                    }
                    
                    // Clear and recreate the list
                    resultsContainer.html("");
                    
                    if (!foundUsers) {
                        resultsContainer.append("div")
                            .style("padding", "8px")
                            .style("text-align", "center")
                            .style("color", "#888")
                            .style("font-size", "11px")
                            .text(filterText ? "No matching users found" : "No users available");
                        return;
                    }
                    
                    // Sort users by name
                    users.sort((a, b) => a.name.localeCompare(b.name));
                    
                    // Create user items for all found users
                    users.forEach(user => {
                        // Create user item
                        const userItem = resultsContainer.append("div")
                            .attr("class", "user-item")
                            .attr("data-user-id", user.id)
                            .style("padding", "6px 8px")
                            .style("cursor", "pointer")
                            .style("font-size", "12px")
                            .style("border-bottom", "1px solid #f0f0f0")
                            .style("display", "flex")
                            .style("align-items", "center")
                            .style("transition", "background 0.2s");
                        
                        // Add color dot
                        userItem.append("div")
                            .style("width", "10px")
                            .style("height", "10px")
                            .style("background", getColorForName(user.name))
                            .style("border-radius", "50%")
                            .style("margin-right", "6px");
                        
                        // Add name
                        userItem.append("div")
                            .text(user.name);
                        
                        // Hover effect
                        userItem
                            .on("mouseenter", function() {
                                d3.select(this).style("background", "#f5f5f5");
                            })
                            .on("mouseleave", function() {
                                d3.select(this).style("background", "white");
                            });
                        
                        // Click to add as type
                        userItem.on("click", () => {
                            // Add as type
                            d.data.addType(user.id);
                            
                            // Close dropdown
                            closeDropdown();
                            
                            // Refresh tree to show new tag
                            group.selectAll("*").remove();
                            render(group, root);
                        });
                    });
                    
                    // Make sure dropdown is on top after updating
                    if (dropdownContainer.node().parentNode !== document.body) {
                        document.body.appendChild(dropdownContainer.node());
                    }
                    
                    // Re-adjust position if needed after content changes
                    adjustDropdownPosition();
                }
            }
            
            // Initial load of users
            loadUsers();
            
            // Handle search input
            searchInput.on("input", function() {
                loadUsers(this.value);
            });
            
            // Close dropdown when clicking outside
            d3.select("body").on("click.dropdown", () => {
                closeDropdown();
            });
            
            // Prevent clicks inside dropdown from closing it
            dropdownContainer.on("click", (event) => {
                event.stopPropagation();
            });
            
            // Focus search input
            searchInput.node().focus();
            
            // Move the dropdown to the body's end to ensure it's on top of all elements
            document.body.appendChild(dropdownContainer.node());

            // Function to check and adjust dropdown position to keep in viewport
            function adjustDropdownPosition() {
                const dropdown = dropdownContainer.node();
                if (!dropdown) return;
                
                const rect = dropdown.getBoundingClientRect();
                const viewportWidth = window.innerWidth;
                const viewportHeight = window.innerHeight;
                
                // Check right edge
                if (rect.right > viewportWidth) {
                    dropdownContainer.style("left", `${viewportWidth - rect.width - 10}px`);
                }
                
                // Check bottom edge
                if (rect.bottom > viewportHeight) {
                    // If dropdown would go above the top, position it at the top with some padding
                    if (event.clientY - rect.height < 10) {
                        dropdownContainer.style("top", "10px");
                    } else {
                        // Otherwise, position above the click
                        dropdownContainer.style("top", `${event.clientY - rect.height - 10}px`);
                    }
                }
            }
            
            // Adjust position after creation
            setTimeout(adjustDropdownPosition, 0);
        });
    });

    // Add touch state tracking at the top
    let touchStartTime = 0;
    let isTouching = false;
    let activeNode = null; // Track which node we're growing

    // Add function to check if we're in a contributor tree
    function isInContributorTree() {
        let temp = currentView;
        while (temp) {
            if (temp.data === data) {
                return false;
            }
            temp = temp.parent;
        }
        return true;
    }

    node.filter(d => true)
        .attr("cursor", "pointer")
        .on("contextmenu", (event) => {
            event.preventDefault(); // This prevents the context menu from showing up
        })
        .on("mousedown touchstart", (event, d: d3.HierarchyRectangularNode<TreeNode>) => {
            event.preventDefault();
            
            // Always set touch state for navigation purposes
            isTouching = true;
            touchStartTime = Date.now();
            activeNode = d;

            // Only proceed with growth/shrink if not in contributor tree
            if (!isInContributorTree()) {
                // Clear any existing growth state
                if (growthInterval) clearInterval(growthInterval);
                if (growthTimeout) clearTimeout(growthTimeout);
                isGrowing = false;
                
                if (d !== root) {
                    // Determine if this is a right-click or two-finger touch
                    const isShrinking = event.type === 'mousedown' ? 
                        event.button === 2 : // right click
                        event.touches.length === 2; // two finger touch

                    growthTimeout = setTimeout(() => {
                        // Only start growing/shrinking if still touching the same node
                        if (isTouching && activeNode === d) {
                            isGrowing = true;
                            // Set the app's growing active flag
                            (window as any).app.isGrowingActive = true;
                            
                            // Hide type tags immediately when growing starts
                            group.selectAll(".type-tags-container")
                                .style("opacity", 0);
                            
                            growthInterval = setInterval(() => {
                                // Only continue if still touching
                                if (!isTouching) {
                                    clearInterval(growthInterval);
                                    isGrowing = false;
                                    growthInterval = null;
                                    // Clear the app's growing active flag
                                    (window as any).app.isGrowingActive = false;
                                    
                                    // Ensure type tags are shown when click finishes
                                    group.selectAll(".type-tags-container")
                                        .style("opacity", 1);
                                    
                                    return;
                                }
                                
                                // Calculate growth/shrink amount
                                const rate = isShrinking ? SHRINK_RATE(d) : GROWTH_RATE(d);
                                const newPoints = Math.max(0, d.data.points + rate); // Prevent negative points
                                if (isNaN(newPoints)) {
                                    console.error('Growth calculation resulted in NaN:', {
                                        currentPoints: d.data.points,
                                        rate: rate,
                                        isShrinking: isShrinking
                                    });
                                    return;
                                }
                                d.data.points = newPoints;
                                
                                // Apply treemap
                                const treemap = d3.treemap().tile(tile);
                                treemap(hierarchy);
                                
                                // Update visualization including type indicators
                                const nodes = group.selectAll("g")
                                    .filter(node => node !== root);
                                
                                // Existing transitions
                                nodes.transition()
                                    .duration(GROWTH_TICK)
                                    .attr("transform", d => d === root ? 
                                        `translate(0,-50)` : 
                                        `translate(${x(d.x0)},${y(d.y0)})`);
                                
                                // Transition rectangles
                                nodes.select("rect")
                                    .transition()
                                    .duration(GROWTH_TICK)
                                    .attr("width", d => d === root ? 
                                        width : 
                                        Math.max(0, x(d.x1) - x(d.x0)))
                                    .attr("height", d => d === root ? 
                                        50 : 
                                        Math.max(0, y(d.y1) - y(d.y0)));

                                // Update text positions
                                nodes.select("text")
                                    .transition()
                                    .duration(GROWTH_TICK)
                                    .attr("transform", d => {
                                        const rectWidth = d === root ? width : x(d.x1) - x(d.x0);
                                        const rectHeight = d === root ? 50 : y(d.y1) - y(d.y0);
                                        return `translate(${rectWidth / 2},${rectHeight / 2})`;
                                    })
                                    .style("font-size", d => {
                                        const rectWidth = d === root ? width : x(d.x1) - x(d.x0);
                                        const rectHeight = d === root ? 50 : y(d.y1) - y(d.y0);
                                        return calculateFontSize(d, rectWidth, rectHeight, root, x, y, currentView) + "px";
                                    });
                                
                                // Update tag container positions
                                nodes.select(".type-tags-container")
                                    .transition()
                                    .duration(GROWTH_TICK)
                                    .attr("transform", d => {
                                        const rectWidth = d === root ? width : x(d.x1) - x(d.x0);
                                        const rectHeight = d === root ? 50 : y(d.y1) - y(d.y0);
                                        
                                        // Position centered horizontally, below the text
                                        // Calculate vertical position based on text lines
                                        const textLines = d === root ? 1 : 
                                                          d.data.name.split(/(?=[A-Z][^A-Z])/g).length;
                                        
                                        // Offset from center - move down by half the text height plus padding
                                        const fontSize = calculateFontSize(d, rectWidth, rectHeight, root, x, y, currentView);
                                        const fontSizeNumber = typeof fontSize === 'number' ? 
                                            fontSize : 12; // Fallback to reasonable default
                                        const verticalOffset = (textLines * 1.2 * fontSizeNumber / 2) + 10;
                                        
                                        return `translate(${rectWidth / 2}, ${rectHeight / 2 + verticalOffset})`;
                                    });
                                
                                // Update foreign object tag wrappers
                                nodes.select(".tag-wrapper")
                                    .transition()
                                    .duration(GROWTH_TICK)
                                    .attr("x", d => {
                                        const rectWidth = d === root ? width : x(d.x1) - x(d.x0);
                                        return -rectWidth / 2 + 10; // Adjust left position with padding
                                    })
                                    .attr("width", d => {
                                        const rectWidth = d === root ? width : x(d.x1) - x(d.x0);
                                        return rectWidth - 20; // Full width minus padding
                                    });

                                // Update peer button position during growth
                                group.select(".peer-button")
                                    .transition()
                                    .duration(GROWTH_TICK)
                                    .attr("transform", d => {
                                        const rectWidth = d === root ? width : x(d.x1) - x(d.x0);
                                        return `translate(${rectWidth - 60}, 25)`;
                                    });

                                // Update add button position during growth
                                group.select(".add-button")
                                    .transition()
                                    .duration(GROWTH_TICK)
                                    .attr("transform", d => {
                                        const rectWidth = d === root ? width : x(d.x1) - x(d.x0);
                                        return `translate(${rectWidth - 20}, 25)`;
                                    });

                                // console.log("\nFinal values:");
                                // console.log("Node points:", d.data.points);
                                // console.log("Node value:", d.data.value);
                                // console.log("Hierarchy value:", hierarchy.value);
                            }, GROWTH_TICK);
                        }
                    }, GROWTH_DELAY);
                }
            }
        }, { passive: false })
        .on("mouseup touchend touchcancel", (event) => {
            // Only handle if not in contributor tree
            if (!isInContributorTree()) {                
                // Clear all states
                isTouching = false;
                activeNode = null;
                
                // Stop growth
                if (growthTimeout) clearTimeout(growthTimeout);
                if (growthInterval) clearInterval(growthInterval);
                growthInterval = null;
                isGrowing = false;
                
                // Clear the app's growing active flag
                (window as any).app.isGrowingActive = false;
                
                // Ensure type tags are shown when click finishes
                group.selectAll(".type-tags-container")
                    .style("opacity", 1);
            }
        }, { passive: true })
        .on("click touchend", (event, d) => {
            event.preventDefault();
            
            const touchDuration = Date.now() - touchStartTime;
            console.log('Click detected on:', d.data.name);
            console.log('Is root?', d === root);
            console.log('Has parent?', d.parent ? 'yes' : 'no');
            console.log('Touch duration:', touchDuration);
            console.log('Is growing?', isGrowing);
            
            // Allow navigation (zooming) regardless of tree
            if (touchDuration < GROWTH_DELAY && !isGrowing) {
                if (d === root && d.parent) {
                    console.log('Attempting zoom out from:', d.data.name);
                    zoomout(d);
                } else if (d !== root && !d.data.isContribution) {  // Check isContribution directly
                    console.log('Attempting zoom in to:', d.data.name);
                    zoomin(d);
                }
            } else {
                console.log('Navigation blocked because:',
                    touchDuration >= GROWTH_DELAY ? 'touch too long' : 'growing active');
            }
            
            // Clear states only if not in contributor tree
            if (!isInContributorTree()) {
                isTouching = false;
                activeNode = null;
                isGrowing = false;
                
                // Ensure app's growing active flag is cleared
                (window as any).app.isGrowingActive = false;
                
                // Ensure type tags are shown when click finishes
                group.selectAll(".type-tags-container")
                    .style("opacity", 1);
            }
        }, { passive: false });

        // Check if view is empty (no children or all children have 0 points)
        if ((!root.children || root.children.length === 0) && root !== data) {
            group.append("text")
                .attr("class", "helper-text")
                .attr("text-anchor", "middle")
                .attr("dominant-baseline", "middle")
                .attr("x", width / 2)
                .attr("y", height / 2)
                .style("font-size", "24px")
                .style("fill", "#666")
                .style("pointer-events", "none")
                .style("user-select", "none")
                .text("Add Values / Contributors");
        }

        // After creating the node groups and before position call
        node.filter(d => d === root)  // Only for the root navigation rectangle
            .each(function(d) {
                // Check if we're in a contributor tree (no path to original data)
                let temp = d;
                let isContributorTree = true;
                while (temp) {
                    if (temp.data === data) {  // data is the original root passed to createTreemap
                        isContributorTree = false;
                        break;
                    }
                    temp = temp.parent;
                }

                // Add home button only for contributor trees
                if (isContributorTree) {
                    // Add home button
                    d3.select(this)
                        .append("g")
                        .attr("class", "home-button")
                        .attr("transform", "translate(20, 25)")  // 25 is 50% of the 50px height
                        .style("cursor", "pointer")
                        .on("click", (event) => {
                            event.stopPropagation();
                            // Clear existing content
                            group.selectAll("*").remove();
                            
                            /*
                            // COMMENTED: Reset to original data
                            hierarchy = d3.hierarchy(data, d => d.childrenArray)
                                .sum(d => d.data.points)
                                .each(d => { d.value = d.data.points || 0; });
                            */

                            // Apply treemap layout
                            const treemap = d3.treemap().tile(tile);
                            root = treemap(hierarchy);
                            currentView = root;
                            
                            // Reset domains
                            x.domain([root.x0, root.x1]);
                            y.domain([root.y0, root.y1]);
                            
                            // Render new view
                            render(group, root);
                        })
                        .append("text")
                        .attr("fill", "#000")
                        .attr("font-size", "20px")
                        .attr("dominant-baseline", "middle")  // Vertically center the text
                        .text("🏠");  // Unicode home emoji
                }
                
                // Only add navigation buttons on our user's tree views (not contributor trees)
                if (!isContributorTree) {
                    const rectWidth = d === root ? width : x(d.x1) - x(d.x0);
                    
                    // Add peer button (on the left of the plus button)
                    d3.select(this)
                        .append("g")
                        .attr("class", "peer-button")
                        .attr("transform", `translate(${rectWidth - 60}, 25)`)  // Position it to the left of plus button
                        .style("cursor", "pointer")
                        .on("click", (event) => {
                            event.stopPropagation();
                            
                            // Trigger the Peer Options form
                            const peerOptionsTrigger = document.querySelector('.drop-zone[data-form="peerOptions"]');
                            if (peerOptionsTrigger) {
                                // Create and dispatch a click event
                                const clickEvent = new MouseEvent('click', {
                                    bubbles: true,
                                    cancelable: true,
                                    view: window
                                });
                                peerOptionsTrigger.dispatchEvent(clickEvent);
                            } else {
                                // Directly show the peer options form if trigger not found
                                const popup = document.querySelector('.node-popup');
                                const peerOptionsForm = document.querySelector('#peerOptionsForm');
                                
                                if (popup && peerOptionsForm) {
                                    // Hide all forms and show the peer options form
                                    document.querySelectorAll('.popup-form').forEach(form => {
                                        (form as HTMLElement).style.display = 'none';
                                    });
                                    (peerOptionsForm as HTMLElement).style.display = 'block';
                                    
                                    // Show the popup
                                    popup.classList.add('active');
                                }
                            }
                        })
                        .append("text")
                        .attr("fill", "#000")
                        .attr("font-size", "20px")
                        .attr("text-anchor", "middle")  // Center the text horizontally
                        .attr("dominant-baseline", "middle")  // Center the text vertically
                        .text("👥");  // Unicode people emoji for peer
                    
                    // Add plus button for adding values
                    d3.select(this)
                        .append("g")
                        .attr("class", "add-button")
                        .attr("transform", `translate(${rectWidth - 20}, 25)`)
                        .style("cursor", "pointer")
                        .on("click", (event) => {
                            event.stopPropagation();
                            
                            // Trigger the Add Value form
                            const addNodeTrigger = document.querySelector('.drop-zone[data-form="addNode"]');
                            if (addNodeTrigger) {
                                // Create and dispatch a click event
                                const clickEvent = new MouseEvent('click', {
                                    bubbles: true,
                                    cancelable: true,
                                    view: window
                                });
                                addNodeTrigger.dispatchEvent(clickEvent);
                            } else {
                                // Directly show the add node form if trigger not found
                                const popup = document.querySelector('.node-popup');
                                const addNodeForm = document.querySelector('#addNodeForm');
                                
                                if (popup && addNodeForm) {
                                    // Hide all forms and show the add node form
                                    document.querySelectorAll('.popup-form').forEach(form => {
                                        (form as HTMLElement).style.display = 'none';
                                    });
                                    (addNodeForm as HTMLElement).style.display = 'block';
                                    
                                    // Show the popup
                                    popup.classList.add('active');
                                }
                            }
                        })
                        .append("text")
                        .attr("fill", "#000")
                        .attr("font-size", "20px")
                        .attr("text-anchor", "middle")  // Center the text horizontally
                        .attr("dominant-baseline", "middle")  // Center the text vertically
                        .text("➕");  // Unicode plus emoji
                }
            });
    }

    function zoomin(d) {
        console.log('Zooming in to:', d.data.name);
        currentView = d;
        const group0 = group.attr("pointer-events", "none");
        
        // Update domains first
        x.domain([d.x0, d.x1]);
        y.domain([d.y0, d.y1]);
        
        const group1 = group = svg.append("g").call(render, d);
        
        svg.transition()
            .duration(750)
            .call(t => group0.transition(t).remove()
                .call(position, d.parent))
            .call(t => group1.transition(t)
                .attrTween("opacity", () => d3.interpolate(0, 1))
                .call(position, d));
    }
  
    function zoomout(d) {
        console.log('Zooming out from:', d.data.name);
        currentView = d.parent;
        const group0 = group.attr("pointer-events", "none");
        
        // Update domains first
        x.domain([d.parent.x0, d.parent.x1]);
        y.domain([d.parent.y0, d.parent.y1]);
        
        const group1 = group = svg.insert("g", "*").call(render, d.parent);
        
        svg.transition()
            .duration(750)
            .call(t => group0.transition(t).remove()
                .attrTween("opacity", () => d3.interpolate(1, 0))
                .call(position, d))
            .call(t => group1.transition(t)
                .call(position, d.parent));
    }

    // Return public interface with functions to get current state
    return {
        getCurrentView: () => currentView,
        getCurrentData: () => data,
        element: svg.node(),
        getRoot: () => root,
        zoomin,
        zoomout,
        update: (newWidth: number, newHeight: number) => {
            // console.log('Update called with dimensions:', newWidth, newHeight);
            
            // Update scales
            x.rangeRound([0, newWidth]);
            y.rangeRound([0, newHeight]);
            
            // Update SVG viewBox
            svg.attr("viewBox", [0.5, -50.5, newWidth, newHeight + 50]);
            
            // Clear existing content and create new group
            group.remove();  // Remove old group
            group = svg.append("g");  // Create new group
            
            // Update visualization with current view
            group.call(render, currentView);  // Render current view instead of root
        },
        destroy() {
            clearInterval(this.updateInterval);
            clearInterval(this.saveInterval);
            clearInterval(growthInterval);
            clearTimeout(growthTimeout);
        }
    };
}