class Node {
    constructor(name, parent = null, types = []) {
        this.name = name;
        this.parent = parent;
        this.points = 0;
        this.children = new Map();  // Node -> Map(contributor -> points)
        this.totalChildPoints = 0;
        this.isContributor = this.parent ? false : true;

        this.types = types;
        // Map of type -> Set of instances
        this.typeIndex = parent ? parent.getRoot().typeIndex : new Map();
        
        // Add initial types if provided
        if (types.length > 0) {
            types.forEach(type => {
                this.addType(type);
                // If any type is a contributor, this instance can't have children
                if (type.isContributor) {
                    this.isContributor = true;
                }
            });
        }
    }

    // Add this node as an instance of the given type
    addType(type) {
        console.log(`\nAdding type ${type.name} to ${this.name}`);
        
        const root = this.getRoot();
        if (!root.typeIndex.has(type)) {
            root.typeIndex.set(type, new Set());
        }
        root.typeIndex.get(type).add(this);
        
        // Debug verification
        console.log(`After adding type ${type.name} to ${this.name}:`);
        console.log(`- Instances:`, Array.from(root.typeIndex.get(type)).map(i => i.name));
        
        return this;
    }

    

    addChild(name, points = 0, types = []) {
        if (this.parent && this.isContributor) {
            throw new Error(`Node ${this.name} is an instance of a contributor and cannot have children.`);
        }

        const child = new Node(name, this);
        
        // Ensure types are properly added
        if (Array.isArray(types)) {
            types.forEach(type => child.addType(type));
        } else if (types) {  // Handle single type case
            child.addType(types);
        }
        
        this.children.set(child, new Map());
        if (points > 0) {
            child.setPoints(points);
        }
        return child;
    }

    removeChild(name) {
        const child = this.children.get(name);
        if (child) {
            // Update points
            if (child.points > 0) {
                this.totalChildPoints -= child.points;
            }
            // Remove from type index
            this.getRoot().typeIndex.forEach(instances => {
                instances.delete(child);
            });
            // Remove from children
            this.children.delete(name);
        }
        return this;
    }

    getRoot() {
        return this.parent ? this.parent.getRoot() : this;
    }

    // Helper method to get all types in the system
    getTypes() {
        return Array.from(this.getRoot().typeIndex.keys());
    }

    // Helper method to get all instances of a given type
    getInstances(type) {
        return this.getRoot().typeIndex.get(type) || new Set();
    }

    setPoints(points) {
        const diff = points - this.points;
        if (this.parent) {
            this.parent.totalChildPoints += diff;
        }
        this.points = points;
        return this;
    }

    getWeight() {
        if (!this.parent) return 1;
        return this.parent.totalChildPoints === 0 ? 0 :
            (this.points / this.parent.totalChildPoints) * this.parent.getWeight();
    }

    // Calculate how much this node recognizes a given type/node
    shareOfGeneralContribution(node) {
        // Use indexed lookup instead of tree traversal
        const instances = this.getRoot().typeIndex.get(node) || new Set();
        return Array.from(instances).reduce((sum, instance) => {
            // Count only contributor types
            const contributorTypesCount = instance.types.filter(type => type.isContributor).length;
            // Divide the instance's weight by its number of contributor types
            const weightShare = contributorTypesCount > 0 ? 
                instance.getWeight() / contributorTypesCount : 
                instance.getWeight();
            return sum + weightShare;
        }, 0);
    }

    mutualShareOfGeneralContribution(node) {
        console.log(`\nDEBUG: Calculating mutual recognition between ${this.name} and ${node.name}`);
        console.log(`- ${this.name} types:`, this.getTypes().map(t => t.name));
        console.log(`- ${node.name} instances:`, Array.from(this.getInstances(node)).map(i => i.name));
        
        // Calculate recognition in both directions
        const otherShareInMe = this.shareOfGeneralContribution(node);
        console.log(`- ${this.name}'s recognition of ${node.name}: ${otherShareInMe}`);
        
        const myShareInOther = node.shareOfGeneralContribution(this);
        console.log(`- ${node.name}'s recognition of ${this.name}: ${myShareInOther}`);
        
        // Mutual recognition is the minimum of both directions
        const mutual = Math.min(otherShareInMe, myShareInOther);
        console.log(`- Mutual recognition: ${mutual}`);
        
        return mutual;
    }

    // Calculate final mutualGeneralContribution of mutualShareOfGeneralContribution
    mutualGeneralContribution() {
        // Get all types that have instances in the system
        const types = Array.from(this.getRoot().typeIndex.keys())
            .filter(type => this.getInstances(type).size > 0);
        
        // Calculate mutual recognition for each type
        const mutualContributions = types.map(type => ({
            contributor: type,
            value: this.mutualShareOfGeneralContribution(type)
        })).filter(({ value }) => value > 0);  // Only keep positive recognitions

        // Calculate total for normalization
        const total = mutualContributions.reduce((sum, { value }) => sum + value, 0);
        
        // Create normalized mutualGeneralContribution
        console.log('\nCalculating mutualGeneralContribution:');
        const mutualGeneralContribution = new Map(
            mutualContributions.map(({ contributor, value }) => [
                contributor,
                total === 0 ? 0 : value / total
            ])
        );

        // Log final results
        console.log('\nFinal mutualGeneralContribution:');
        mutualGeneralContribution.forEach((share, contributor) => {
            console.log(`${contributor.name}: ${(share * 100).toFixed(1)}%`);
        });
        console.log( 'General mutual Contribution will serve as the bridge between Abstract and Concrete Time distribution and allow us to calculate Concrete Mutual Recognition of Mutual Relation.' );
        return mutualGeneralContribution;
    }
    shareOfMutualGeneralContribution(node){
        // share of communal production/contribution/total-social-product!
        // share in the part of the total product destined for individual consumption (means of subsistence)
        return this.mutualGeneralContribution().get(node);
    }
}

class D3Node extends Node {
    constructor(name, parent = null, types = []) {
        super(name, parent, types);
    }

    // Override addChild to create D3Nodes instead of regular Nodes
    addChild(name, points = 0, types = []) {
        const child = new D3Node(name, this, types);
        this.children.set(name, child);
        
        if (types.length > 0) {
            types.forEach(type => child.addType(type));
        }   
        
        if (points > 0) {
            child.setPoints(points);
        }
        return child;
    }

    // D3 Compatibility Methods
    get value() {
        return this.points;
    }

    get childrenArray() {
        return Array.from(this.children.values());
    }

    get data() {
        return this;
    }

    get hasChildren() {
        return this.children.size > 0;
    }
    
    descendants() {
        const result = [];
        const stack = [this];
        while (stack.length) {
            const node = stack.pop();
            result.push(node);
            stack.push(...node.childrenArray);
        }
        return result;
    }

    ancestors() {
        const result = [];
        let current = this;
        while (current) {
            result.push(current);
            current = current.parent;
        }
        return result;
    }
}

// Create a color scale for names (add this near the top with other utilities)
const nameColors = new Map();
const colorScale = d3.scaleOrdinal()
    .range([
        ...d3.schemePastel1,
        ...d3.schemePastel2,        // 10 colors
        ...d3.schemeSet2,             // 12 colors
        ...d3.schemeSet3,             // 8 colors
    ]);  // Total: 61 unique colors
    
// Helper function to get/set color for a name
function getColorForName(name) {
    if (!nameColors.has(name)) {
        nameColors.set(name, colorScale(name));
    }
    return nameColors.get(name);
}

function TreeMap(data) {
    // Get the container dimensions
    const container = document.getElementById('treemap-container');
    const width = container.clientWidth;
    const height = container.clientHeight;

    // Add state variables at the top
    let growthInterval = null;
    let growthTimeout = null;
    const GROWTH_RATE = (d) => d.data.points * 0.05;
    const GROWTH_TICK = 50;      // Milliseconds between growth
    const GROWTH_DELAY = 500;    // Delay before growth starts
    let isGrowing = false;  // Track if we're in a growth operation

    // Replace Observable's DOM.uid with a custom uid generator
    const uid = (function() {
        let id = 0;
        return function(prefix) {
            const uniqueId = `${prefix}-${++id}`;
            return {
                id: uniqueId,
                href: `#${uniqueId}`
            };
        };
    })();

    // Formatting utilities
    const name = d => d.data.ancestors().reverse().map(d => d.name).join(" / ");

    // Function to calculate font size - extract the logic to ensure consistency
    function calculateFontSize(d, rectWidth, rectHeight, root) {
        // Always use the current view's domain for calculations
        const currentDomain = currentView || root;
        
        // Calculate actual dimensions based on current view
        const actualWidth = Math.abs(x(d.x1) - x(d.x0));
        const actualHeight = Math.abs(y(d.y1) - y(d.y0));
        
        const textContent = d === root ? name(d) : d.data.name;
        const lines = d === root ? [textContent] : textContent.split(/(?=[A-Z][^A-Z])/g);
        const maxLineLength = Math.max(...lines.map(line => line.length));
        
        // Use consistent dimensions for calculations
        const availableWidth = actualWidth * 0.9;
        const availableHeight = (actualHeight * 0.9) / (lines.length * 1.2);
        
        const widthBasedSize = availableWidth / (maxLineLength * 0.6);
        const heightBasedSize = availableHeight * 0.8;
        
        const size = Math.min(widthBasedSize, heightBasedSize);
        
        console.log(`Font size calculation for ${d.data.name}:`, {
            view: currentView ? currentView.data.name : 'root',
            dimensions: { actualWidth, actualHeight },
            result: Math.min(Math.max(size, 8), 24)
        });
        
        return Math.min(Math.max(size, 8), 24);
    }

    function tile(node, x0, y0, x1, y1) {
        if (!node.children) return;
        
        // Calculate available space
        const availableWidth = x1 - x0;
        const availableHeight = y1 - y0;
        
        // Ensure values match points
        node.children.forEach(child => {
            child.value = child.data.points || 0;
        });
        
        // Create a simpler hierarchy object that matches d3's expectations
        const tempRoot = {
            children: node.children.map(child => ({
                data: child.data,
                value: child.value
            }))
        };
        
        // Create hierarchy and apply squarify directly
        const tempHierarchy = d3.hierarchy(tempRoot)
            .sum(d => d.value)
            
        // Apply squarify directly with the available space
        d3.treemapSquarify(tempHierarchy, 0, 0, availableWidth, availableHeight);
        
        // Debug total values
        console.log('Total hierarchy value:', tempHierarchy.value);
        console.log('Available space:', [availableWidth, availableHeight]);
        
        // Transfer positions back to our nodes
        node.children.forEach((child, i) => {
            if (tempHierarchy.children && tempHierarchy.children[i]) {
                const tempNode = tempHierarchy.children[i];
                child.x0 = x0 + tempNode.x0;
                child.x1 = x0 + tempNode.x1;
                child.y0 = y0 + tempNode.y0;
                child.y1 = y0 + tempNode.y1;
                
                // Debug
                console.log(`${child.data.name}: value=${child.value}, width=${child.x1 - child.x0}, height=${child.y1 - child.y0}`);
            }
        });
    }
  
    function position(group, root) {
        group.selectAll("g")
            .attr("transform", d => {
                if (!d || typeof d.x0 === 'undefined') return '';
                return d === root ? `translate(0,-50)` : `translate(${x(d.x0)},${y(d.y0)})`;
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

        // Update type indicators along with other elements
        group.selectAll(".type-indicators")
            .attr("transform", d => {
                if (!d || typeof d.x0 === 'undefined') return '';
                const rectWidth = d === root ? width : x(d.x1) - x(d.x0);
                return `translate(${rectWidth - 10}, 10)`;
            });
    }
  
    // Create the scales first
    const x = d3.scaleLinear().rangeRound([0, width]);
    const y = d3.scaleLinear().rangeRound([0, height]);
  
    // Compute the layout
    const hierarchy = d3.hierarchy(data, d => d.childrenArray)
    .sum(d => {
        // Only use the node's own points, not its children's
        return d.data.points;
    })
    .each(d => {
        // Ensure value is exactly equal to points
        d.value = d.data.points || 0;
    });

    let root = d3.treemap().tile(tile)(hierarchy);
    let currentView = root;

    // Set initial domains
    x.domain([root.x0, root.x1]);
    y.domain([root.y0, root.y1]);
  
    // Create the SVG container with dynamic dimensions
    const svg = d3.create("svg")
        .attr("viewBox", [0.5, -50.5, width, height + 50])
        .style("font", "10px sans-serif");
  
    // Create the initial group
    let group = svg.append("g")
        .call(render, root);
  
    // When zooming in, draw the new nodes on top, and fade them in.
    function zoomin(d) {
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
  
    // When zooming out, draw the old nodes on top, and fade them out.
    function zoomout(d) {
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

      
    function render(group, root) {
      // First, create groups only for nodes with value or root
      const nodeData = (root.children || []).concat(root);

        const node = group
            .selectAll("g")
            .data(nodeData)
            .join("g")
            .filter(d => d === root || d.value > 0);

        node.append("title")
            .text(d => `${name(d)}\n`);

        node.selectAll("text").remove();

        node.append("rect")
            .attr("id", d => (d.leafUid = uid("leaf")).id)
            .attr("fill", d => {
                if (d === root) return "#fff";
                return getColorForName(d.data.name);
            })
            .attr("stroke", "#fff")
            .attr("stroke-width", "5");

        node.append("clipPath")
            .attr("id", d => (d.clipUid = uid("clip")).id)
            .append("use")
            .attr("xlink:href", d => d.leafUid.href);

        node.append("text")
            .attr("clip-path", d => d.clipUid)
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
                return calculateFontSize(d, rectWidth, rectHeight, root) + "px";
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

    // Add type circles container after the rect
    const typeContainer = node.append("g")
        .attr("class", "type-indicators")
        .attr("transform", d => {
            const rectWidth = d === root ? width : x(d.x1) - x(d.x0);
            return `translate(${rectWidth - 10}, 10)`; // Position in top-right corner
        });

    // Add circles for each type
    typeContainer.each(function(d) {
        if (!d.data.types) return;
        
        const container = d3.select(this);
        const circleRadius = 5;
        const spacing = circleRadius * 2.5;
        
        container.selectAll("circle")
            .data(d.data.types)
            .join("circle")
            .attr("cx", (_, i) => -i * spacing)
            .attr("cy", 0)
            .attr("r", circleRadius)
            .attr("fill", type => getColorForName(type.name))
            .attr("stroke", "#fff")
            .attr("stroke-width", "1.5")
            .append("title")
            .text(type => type.name);
    });

    // Add touch state tracking at the top
    let touchStartTime = 0;
    let isTouching = false;
    let activeNode = null; // Track which node we're growing

    node.filter(d => true)
        .attr("cursor", "pointer")
        .on("mousedown touchstart", (event, d) => {
            event.preventDefault();
            
            // Clear any existing growth state
            if (growthInterval) clearInterval(growthInterval);
            if (growthTimeout) clearTimeout(growthTimeout);
            isGrowing = false;
            
            // Set new touch state
            isTouching = true;
            touchStartTime = Date.now();
            activeNode = d;

            if (d !== root) {
                growthTimeout = setTimeout(() => {
                    // Only start growing if still touching the same node
                    if (isTouching && activeNode === d) {
                        isGrowing = true;
                        growthInterval = setInterval(() => {
                            // Only grow if still touching
                            if (!isTouching) {
                                clearInterval(growthInterval);
                                growthInterval = null;
                                isGrowing = false;
                                return;
                            }
                            
                            // Existing growth logic
                            const growthAmount = GROWTH_RATE(d);
                            d.data.setPoints(d.data.points + growthAmount);
                            
                            // Recompute hierarchy ensuring values match points
                            hierarchy.sum(node => node.data.points)
                                .each(node => {
                                    // Force value to exactly match points
                                    node.value = node.data.points || 0;
                                });
                            
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
                                    return calculateFontSize(d, rectWidth, rectHeight, root) + "px";
                                });
                                
                            // Add type indicator updates here
                            nodes.select(".type-indicators")
                                .transition()
                                .duration(GROWTH_TICK)
                                .attr("transform", d => {
                                    const rectWidth = d === root ? width : x(d.x1) - x(d.x0);
                                    return `translate(${rectWidth - 10}, 10)`;
                                });

                            console.log("\nFinal values:");
                            console.log("Node points:", d.data.points);
                            console.log("Node value:", d.value);
                            console.log("Hierarchy value:", hierarchy.value);

                            const pieChart = createPieChart(data);
                            document.getElementById('pie-container').innerHTML = '';
                            document.getElementById('pie-container').appendChild(pieChart);
                            
                            console.log("\nFinal values:");
                            console.log("Node points:", d.data.points);
                            console.log("Node value:", d.value);
                            console.log("Hierarchy value:", hierarchy.value);
                        }, GROWTH_TICK);
                    }
                }, GROWTH_DELAY);
            }
        })
        .on("mouseup mouseleave touchend touchcancel touchleave", (event) => {
            event.preventDefault();
            
            // Clear all states
            isTouching = false;
            activeNode = null;
            
            // Stop growth
            if (growthTimeout) clearTimeout(growthTimeout);
            if (growthInterval) clearInterval(growthInterval);
            growthInterval = null;
            isGrowing = false;
        })
        .on("click touchend", (event, d) => {
            event.preventDefault();
            
            const touchDuration = Date.now() - touchStartTime;
            
            // Handle zoom only on quick taps
            if (touchDuration < GROWTH_DELAY && !isGrowing) {
                if (d === root && d.parent) {
                    zoomout(root);
                } else if (d !== root && !d.data.isContributor) {  // Check isContributor directly
                    console.log('Node:', d.data.name);
                    console.log('Is Contributor:', d.data.isContributor);
                    zoomin(d);
                }
            }
            
            // Clear all states
            isTouching = false;
            activeNode = null;
            isGrowing = false;
        });

        if (root.data.children.size === 0 && root !== data) {  // Check if view is empty and not root
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
    }
      // Add menu bar with cycling text
      const addNodeTexts = ['Add Value', 'Add Goal', 'Add Dependency', 'Add Desire'];
      let currentTextIndex = 0;

      const menuBar = document.createElement('div');
      menuBar.className = 'menu-bar';
      menuBar.style.userSelect = 'none';
      menuBar.style.webkitUserSelect = 'none';
      menuBar.innerHTML = `
          <button class="menu-button cycle-text" data-form="addNode" style="user-select: none; -webkit-user-select: none;">${addNodeTexts[0]}</button>
          <button class="menu-button" data-form="revealQR" style="user-select: none; -webkit-user-select: none;">Add Contributor</button>
      `;
      container.appendChild(menuBar);

      // Add cycling functionality
      const cycleButton = menuBar.querySelector('.cycle-text');
      cycleButton.addEventListener('click', (e) => {
          currentTextIndex = (currentTextIndex + 1) % addNodeTexts.length;
          cycleButton.textContent = addNodeTexts[currentTextIndex];
          
          // Update the form title to match
          const formTitle = document.querySelector('#addNodeForm h2');
          formTitle.textContent = addNodeTexts[currentTextIndex];
      });

      // Add popup forms container
      const popup = document.createElement('div');
      popup.className = 'node-popup';
      popup.innerHTML = `
          <div class="node-popup-content" style="user-select: none; -webkit-user-select: none;">
              <!-- Add Node Form -->
              <form id="addNodeForm" class="popup-form" style="user-select: none; -webkit-user-select: none;">
                  <h2>Add New Node</h2>
                  <div class="form-group">
                      <label for="nodeName" style="user-select: none; -webkit-user-select: none;">Name:</label>
                      <input type="text" id="nodeName" required>
                  </div>
                  <div id="percentageGroup" class="form-group" style="display: none;">
                      <label for="nodePercentage" style="user-select: none; -webkit-user-select: none;">Desired Percentage:</label>
                      <input type="range" id="nodePercentage" min="1" max="100" value="10">
                      <output for="nodePercentage" style="user-select: none; -webkit-user-select: none;">10%</output>
                  </div>
                  <div class="form-buttons">
                      <button type="submit" class="primary" style="user-select: none; -webkit-user-select: none;">Add Node</button>
                      <button type="button" class="cancel" style="user-select: none; -webkit-user-select: none;">Cancel</button>
                  </div>
              </form>

              <!-- Reveal QR Form -->
              <form id="revealQRForm" class="popup-form" style="display: none;">
                  <h2 style="user-select: none; -webkit-user-select: none;">Your Public Key</h2>
                  <div class="qr-display">
                      <div id="qr-code"></div>
                      <div id="public-key-text"></div>
                  </div>
                  <div class="form-buttons">
                      <button type="button" class="copy-key" style="user-select: none; -webkit-user-select: none;">Copy Key</button>
                      <button type="button" class="cancel" style="user-select: none; -webkit-user-select: none;">Close</button>
                  </div>
              </form>
          </div>
      `;
      container.appendChild(popup);

        // Add event listeners for menu and forms
        const menuButtons = menuBar.querySelectorAll('.menu-button');
        const forms = popup.querySelectorAll('.popup-form');
        const cancelButtons = popup.querySelectorAll('.cancel');
        const form = popup.querySelector('#addNodeForm');
        
        menuButtons.forEach(button => {
            button.addEventListener('click', () => {
                const formId = button.dataset.form;
                forms.forEach(form => {
                    form.style.display = form.id === `${formId}Form` ? 'block' : 'none';
                });
                popup.classList.add('active');

                // Show/hide percentage group based on whether current view has children
                if (formId === 'addNode') {
                    const percentageGroup = document.getElementById('percentageGroup');
                    const hasChildren = currentView.data.children.size > 0;
                    percentageGroup.style.display = hasChildren ? 'block' : 'none';
                }

                // Generate QR code when revealing QR form
                if (formId === 'revealQR') {
                    const mockPublicKey = "0x742d35Cc6634C0532925a3b844Bc454e4438f44e";
                    // Clear existing QR code
                    document.getElementById("qr-code").innerHTML = '';
                    
                    // Create QR code with explicit options
                    const qr = new QRCode(document.getElementById("qr-code"), {
                        text: mockPublicKey,
                        width: 200,
                        height: 200,
                        colorDark : "#000000",
                        colorLight : "#ffffff",
                        correctLevel : QRCode.CorrectLevel.H,
                        useSVG: true  // Use SVG instead of Canvas to avoid eval
                    });
                    
                    // Set the public key text content
                    document.getElementById('public-key-text').textContent = mockPublicKey;
                }
            });
        });

        // Update range input display
        const range = document.getElementById('nodePercentage');
        const output = range.nextElementSibling;
        range.addEventListener('input', () => {
            output.textContent = range.value + '%';
        });

        // Add copy key functionality
        const copyButton = popup.querySelector('.copy-key');
        copyButton.addEventListener('click', () => {
            const publicKey = document.getElementById('public-key-text').textContent;
            navigator.clipboard.writeText(publicKey)
                .then(() => alert('Public key copied to clipboard!'));
        });

        // Handle form cancellation
        cancelButtons.forEach(button => {
            button.addEventListener('click', () => {
                popup.classList.remove('active');
                form.reset();
            });
        });
  
      
        form.addEventListener('submit', (e) => {
            e.preventDefault();
            const name = document.getElementById('nodeName').value;
            const hasChildren = currentView.data.children.size > 0;
            const percentage = hasChildren ? 
                Number(document.getElementById('nodePercentage').value) : 
                100;
            
            // Only validate percentage if there are children
            if (hasChildren && (percentage <= 0 || percentage >= 100)) {
                alert('Percentage must be between 1 and 99');
                return;
            }
            
            console.group('Adding New Node');
            console.log('Adding to current view:', currentView.data.name);
            
            // Calculate points needed for this percentage
            const currentTotal = currentView.data.totalChildPoints || currentView.data.points || 100;
            let points;
            
            if (hasChildren) {
                // If there are children, calculate based on percentage
                const newTotal = Math.ceil(currentTotal / (1 - percentage/100));
                points = Math.max(1, newTotal - currentTotal);
            } else {
                // If no children, just use the current total as points
                points = currentTotal;
            }
            
            console.log('Current total:', currentTotal);
            console.log('New total will be:', currentTotal + points);
            console.log('Points needed:', points);
            
            // Add the new node to the current view
            const newChild = currentView.data.addChild(name, points);
            console.log('New child created:', newChild);
            
            // Recompute the entire visualization from scratch
            const container = document.getElementById('treemap-container');
            
            // Restore the zoom level
            let target = root;
            const path = [];
            let temp = currentView;
            while (temp !== root) {
                path.push(temp);
                temp = temp.parent;
            }
            
            // Recompute visualization
            container.innerHTML = ''; 
            const newTreemap = TreeMap(root.data);
            container.appendChild(newTreemap);
            
            // Update pie chart separately to ensure it's not cleared
            const pieChart = createPieChart(root.data);
            document.getElementById('pie-container').innerHTML = '';
            document.getElementById('pie-container').appendChild(pieChart);
        
            // Replay zooms
            path.reverse().forEach(node => {
                zoomin(node);
            });
            
            console.log('Final hierarchy:', root);
            console.groupEnd();
            
            // Close and reset the form
            popup.classList.remove('active');
            form.reset();
        });

    // Add window resize handler
    window.addEventListener('resize', () => {
        const newWidth = container.clientWidth;
        const newHeight = container.clientHeight;
        
        // Update scales
        x.rangeRound([0, newWidth]);
        y.rangeRound([0, newHeight]);
        
        // Update SVG viewBox
        svg.attr("viewBox", [0.5, -50.5, newWidth, newHeight + 50]);
        
        // Recompute the treemap layout
        const newRoot = d3.treemap().tile(tile)(hierarchy);
        
        // Update the visualization
        group.call(render, newRoot);
    });
    
        // Create and append pie chart
    const pieChart = createPieChart(data);
    document.getElementById('pie-container').innerHTML = '';
    document.getElementById('pie-container').appendChild(pieChart);

    return svg.node();  // Return the actual DOM element
}


function createPieChart(data) {
    // Get the container dimensions
    const container = document.getElementById('pie-container');
    const width = container.clientWidth;
    const height = container.clientHeight;
    const radius = Math.min(width, height) / 2;

    // Get mutualGeneralContribution from root node
    const mutualGeneralContribution = data.mutualGeneralContribution();
    console.log('mutualGeneralContribution for pie:', mutualGeneralContribution);

    // Create pie layout
    const pie = d3.pie()
        .value(d => d[1])  // Use the mutualGeneralContribution value
        .sort(null);  // Maintain original order

    // Create arc generator
    const arc = d3.arc()
        .innerRadius(radius * 0.4)  // Create a donut chart
        .outerRadius(radius * 0.8);

    // Create SVG
    const svg = d3.create("svg")
        .attr("width", width)
        .attr("height", height)
        .attr("viewBox", [-width/2, -height/2, width, height])
        .style("font", "12px sans-serif");

    // Create pie segments from mutualGeneralContribution
    const arcs = pie(Array.from(mutualGeneralContribution.entries()));

    // Add segments using the same color scheme as the treemap
    svg.selectAll("path")
        .data(arcs)
        .join("path")
        .attr("fill", d => getColorForName(d.data[0].name))  // Use same color function
        .attr("d", arc)
        .append("title")
        .text(d => `${d.data[0].name}: ${(d.data[1] * 100).toFixed(1)}%`);

    // Add labels
    svg.selectAll("text")
        .data(arcs)
        .join("text")
        .attr("transform", d => {
            const [x, y] = arc.centroid(d);  // This gives us the center of the arc
            return `translate(${x},${y})`;
        })
        .attr("dy", "0.35em")
        .attr("text-anchor", "middle")
        .style("font-size", "15px")  // Adjust the font size here
        .text(d => `${d.data[0].name}`);

    // Add center text
    const centerGroup = svg.append("g")
        .attr("text-anchor", "middle")
        .attr("dominant-baseline", "middle");

    // Calculate text size based on inner radius
    const fontSize = radius * 0.09;  // 20% of radius
    
    /*
    On the other hand, it also serves as a measure of the portion of the common labour borne by each individual, 
    and of his share in the part of the total product destined for individual consumption. 
    The social relations of the individual producers, 
    with regard both to their labour and to its products, 
    are in this case perfectly simple and intelligible, 
    and that with regard not only to production but also to distribution.
    */

    // Communal Shares? Mutual Shares?

    // Can we add a distribute-surplus? button here?

    centerGroup.append("text")
        .attr("dy", -fontSize/1.6)  // Center it
        .attr("font-size", fontSize)
        .attr("font-weight", "bold")
        .text("Mutual");

    centerGroup.append("text")
        .attr("dy", fontSize/1.6)  // Move down by one full font size
        .attr("font-size", fontSize)
        .attr("font-weight", "bold")
        .text("Contribution");

    return svg.node();
}