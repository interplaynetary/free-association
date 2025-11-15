import * as d3 from 'd3';
import { getColorForName } from '../utils/colorUtils.js';
import { calculateFontSize } from '../utils/fontUtils.js';

export function createTreemap(data, width, height) {
	// State variables for growth animation
	let growthInterval = null;
	let growthTimeout = null;
	const GROWTH_RATE = (d) => d.data.points * 0.05;
	const GROWTH_TICK = 50;
	const GROWTH_DELAY = 500;
	let isGrowing = false;
	const SHRINK_RATE = (d) => d.data.points * -0.05; // Negative growth rate for shrinking
	let draggedNode = null; // Track currently dragged node
	let dragTarget = null; // Track the potential drop target
	let mouseDownPosition = null;
	let dragThreshold = 5; // pixels of movement required to start dragging

	// Helper functions
	const uid = (function () {
		let id = 0;
		return function (prefix) {
			const uniqueId = `${prefix}-${++id}`;
			return { id: uniqueId, href: `#${uniqueId}` };
		};
	})();

	const name = (d) =>
		d.data
			.ancestors()
			.reverse()
			.map((d) => d.name)
			.join(' / ');

	// Create scales
	const x = d3.scaleLinear().rangeRound([0, width]);
	const y = d3.scaleLinear().rangeRound([0, height]);

	// Create hierarchy
	let hierarchy = d3
		.hierarchy(data, (d) => d.childrenArray)
		.sum((d) => d.data.points)
		.each((d) => {
			d.value = d.data.points || 0;
		});

	// Create treemap layout
	let root = d3.treemap().tile(tile)(hierarchy);
	let currentView = root;

	// Set initial domains
	x.domain([root.x0, root.x1]);
	y.domain([root.y0, root.y1]);

	// Create SVG
	const svg = d3
		.create('svg')
		.attr('viewBox', [0.5, -50.5, width, height + 50])
		.style('font', '10px sans-serif');

	// Prevent context menu (right-click menu) to allow for right-click interactions
	svg.on('contextmenu', (event) => {
		event.preventDefault();
	});

	// Create initial group
	let group = svg.append('g').call(render, root);

	// Helper to darken a color for the fulfillment indicator
	function darkenColor(color, factor = 0.3) {
		const rgb = d3.rgb(color);
		return d3.rgb(
			Math.max(0, rgb.r - rgb.r * factor),
			Math.max(0, rgb.g - rgb.g * factor),
			Math.max(0, rgb.b - rgb.b * factor)
		);
	}

	function tile(node, x0, y0, x1, y1) {
		if (!node.children) return;

		// Calculate available space
		const availableWidth = x1 - x0;
		const availableHeight = y1 - y0;

		// Ensure values match points
		node.children.forEach((child) => {
			child.value = child.data.points || 0;
		});

		// Create a simpler hierarchy object that matches d3's expectations
		const tempRoot = {
			children: node.children.map((child) => ({
				data: child.data,
				value: child.value
			}))
		};

		// Create hierarchy and apply squarify directly
		const tempHierarchy = d3.hierarchy(tempRoot).sum((d) => d.value);

		// Apply squarify directly with the available space
		d3.treemapSquarify(tempHierarchy, 0, 0, availableWidth, availableHeight);

		// Debug total values
		// console.log('Total hierarchy value:', tempHierarchy.value);
		// console.log('Available space:', [availableWidth, availableHeight]);

		// Transfer positions back to our nodes
		node.children.forEach((child, i) => {
			if (tempHierarchy.children && tempHierarchy.children[i]) {
				const tempNode = tempHierarchy.children[i];
				child.x0 = x0 + tempNode.x0;
				child.x1 = x0 + tempNode.x1;
				child.y0 = y0 + tempNode.y0;
				child.y1 = y0 + tempNode.y1;

				// Debug
				// console.log(`${child.data.name}: value=${child.value}, width=${child.x1 - child.x0}, height=${child.y1 - child.y0}`);
			}
		});
	}

	function position(group, root) {
		// Update all g elements except the home button
		group.selectAll('g:not(.home-button)').attr('transform', (d) => {
			if (!d || typeof d.x0 === 'undefined') return '';
			return d === root ? `translate(0,-50)` : `translate(${x(d.x0)},${y(d.y0)})`;
		});

		group
			.selectAll('rect.node-rect')
			.attr('width', (d) => {
				if (!d || typeof d.x0 === 'undefined') return 0;
				return d === root ? width : x(d.x1) - x(d.x0);
			})
			.attr('height', (d) => {
				if (!d || typeof d.y0 === 'undefined') return 0;
				return d === root ? 50 : y(d.y1) - y(d.y0);
			});

		// Update fulfillment indicator rectangles
		group
			.selectAll('rect.fulfillment-indicator')
			.attr('width', (d) => {
				if (!d || typeof d.x0 === 'undefined' || d === root) return 0;
				const fullWidth = x(d.x1) - x(d.x0);
				// Use the fulfillment percentage to determine width
				const fulfillmentPercentage = d.data.fulfilled;
				return fullWidth * fulfillmentPercentage;
			})
			.attr('height', (d) => {
				if (!d || typeof d.y0 === 'undefined' || d === root) return 0;
				return y(d.y1) - y(d.y0);
			});

		// Update the handle positions to match the indicator widths
		group.selectAll('rect.fulfillment-handle').each(function (d) {
			if (!d || typeof d.x0 === 'undefined' || d === root) return;

			const nodeGroup = d3.select(this.parentNode);
			const indicator = nodeGroup.select('rect.fulfillment-indicator');
			const indicatorWidth = parseFloat(indicator.attr('width'));
			const rectHeight = y(d.y1) - y(d.y0);

			// Update handle position and height, keeping it 75% down the rectangle
			d3.select(this)
				.attr('x', indicatorWidth - 4)
				.attr('y', rectHeight * 0.75) // Position it 75% down the rectangle
				.attr('height', 20);
		});

		// Update type indicators along with other elements
		group.selectAll('.type-indicators').attr('transform', (d) => {
			if (!d || typeof d.x0 === 'undefined') return '';
			const rectWidth = d === root ? width : x(d.x1) - x(d.x0);
			return `translate(${rectWidth - 10}, 10)`;
		});
	}

	function render(group, root) {
		// First, create groups only for nodes with value or root
		let nodeData;

		// Handle different types of children collections (Array or Map)
		if (root.children && Array.isArray(root.children)) {
			// D3 hierarchy nodes have children as arrays
			nodeData = root.children.concat(root);
		} else if (root.children && typeof root.children.values === 'function') {
			// Our data nodes might have children as Maps
			nodeData = Array.from(root.children.values()).concat(root);
		} else if (root.childrenArray) {
			// For contributor trees, we might have childrenArray defined
			nodeData = root.childrenArray.concat(root);
		} else {
			// Fallback to just the root if no children found
			nodeData = [root];
		}

		const node = group
			.selectAll('g')
			.data(nodeData)
			.join('g')
			.filter((d) => d === root || d.value > 0)
			.attr('cursor', 'pointer')
			.on('click', (event, d) => {
				if (d === root) return;
				event.stopPropagation();
				zoomin(d);
			})
			.call(
				d3
					.drag()
					.on('start', dragStarted)
					.on('drag', dragging)
					.on('end', dragEnded)
					.filter((event) => {
						// Only enable drag on left mouse button (not right click which we use for shrinking)
						// AND only if there's actual mouse movement (dx or dy not zero)
						// This prevents drag from interfering with grow/shrink on click-and-hold
						return event.button === 0 && (Math.abs(event.dx) > 3 || Math.abs(event.dy) > 3);
					})
			);

		node.append('title').text((d) => {
			// Format fulfillment as percentage for the tooltip
			const fulfillment = d.data.fulfilled !== undefined ? d.data.fulfilled : 0;
			const fulfillmentText = d === root ? '' : `\nFulfillment: ${Math.round(fulfillment * 100)}%`;
			return `${name(d)}${fulfillmentText}`;
		});

		node.selectAll('text').remove();

		node
			.append('rect')
			.attr('id', (d) => (d.leafUid = uid('leaf')).id)
			.attr('class', 'node-rect') // Add a class for selection
			.attr('fill', (d) => {
				if (d === root) return '#fff';
				return getColorForName(d.data.name);
			})
			.attr('stroke', (d) => {
				// Only add special outline for nodes with non-contribution children - now in blue
				return d.data.hasDirectContributionChild ? '#2196f3' : '#fff';
			})
			.attr('stroke-width', (d) => {
				// Only make stroke wider for nodes with non-contribution children
				return d.data.hasDirectContributionChild ? '2' : '2';
			});

		// Add fulfillment indicator rectangle (darker shade)
		node
			.filter((d) => d !== root) // Don't add to root node
			.append('rect')
			.attr('class', 'fulfillment-indicator')
			.attr('fill', (d) => {
				const baseColor = getColorForName(d.data.name);
				return darkenColor(baseColor);
			})
			.attr('width', (d) => {
				const fullWidth = x(d.x1) - x(d.x0);
				const fulfillmentPercentage = d.data.fulfilled;
				return fullWidth * fulfillmentPercentage;
			})
			.attr('height', (d) => y(d.y1) - y(d.y0))
			.attr('pointer-events', 'all'); // Enable pointer events for draggability

		// Add a handle to the right edge of the fulfillment indicator for dragging
		node
			.filter((d) => {
				// Skip root node and nodes with no children
				if (d === root || d.data.children.size === 0) return false;

				// Add handles to nodes that have at least one direct contribution child
				return d.data.hasDirectContributionChild;
			})
			.each(function (d) {
				const nodeGroup = d3.select(this);
				const rectWidth = x(d.x1) - x(d.x0);
				const rectHeight = y(d.y1) - y(d.y0);

				// Get current fulfillment value
				const currentValue =
					d.data._manualFulfillment !== null && d.data._manualFulfillment !== undefined
						? d.data._manualFulfillment
						: d.data.fulfilled !== undefined
							? d.data.fulfilled
							: 0;

				// Get the fulfillment indicator rectangle
				const indicator = nodeGroup.select('rect.fulfillment-indicator');

				// Calculate current indicator width
				const indicatorWidth = rectWidth * currentValue;

				// Add a handle that appears at the right edge of the indicator
				const handle = nodeGroup
					.append('rect')
					.attr('class', 'fulfillment-handle')
					.attr('x', indicatorWidth - 4) // Position at right edge of indicator
					.attr('y', rectHeight * 0.75) // Position it 75% down the rectangle
					.attr('width', 8) // Make handle thick enough to grab
					.attr('height', 20) // Small centered lip
					.attr('fill', 'rgba(255, 255, 255, 0.7)')
					.attr('stroke', '#fff')
					.attr('stroke-width', 2)
					.attr('cursor', 'ew-resize')
					.attr('rx', 4); // More rounded corners for the lip

				// Add tooltip to handle
				nodeGroup
					.select('title')
					.text(`${name(d)}\nFulfillment: ${Math.round(currentValue * 100)}%`);

				// Make the handle and indicator draggable
				const dragBehavior = d3
					.drag()
					.on('start', function (event) {
						// Stop event propagation to prevent competing with growth/shrink
						event.sourceEvent.stopPropagation();
					})
					.on('drag', function (event) {
						// Stop event propagation to prevent competing with growth/shrink
						event.sourceEvent.stopPropagation();

						// Constrain to rectangle width
						const newWidth = Math.max(0, Math.min(rectWidth, event.x));

						// Calculate fulfillment value (0-1)
						const fulfillmentValue = newWidth / rectWidth;

						// Update indicator width
						indicator.attr('width', newWidth);

						// Update handle position
						handle.attr('x', newWidth - 4);

						// Update the node tooltip
						nodeGroup
							.select('title')
							.text(`${name(d)}\nFulfillment: ${Math.round(fulfillmentValue * 100)}%`);

						// Set the fulfillment value on the node
						d.data.fulfillment = fulfillmentValue;
					})
					.on('end', function (event) {
						// Stop event propagation to prevent competing with growth/shrink
						event.sourceEvent.stopPropagation();

						// After drag ends, update the entire visualization to reflect changes
						if (growthInterval) {
							clearInterval(growthInterval);
							growthInterval = null;
						}
						isGrowing = false;

						// Get the current indicator width
						const currentWidth = parseFloat(indicator.attr('width'));

						// Calculate final fulfillment value
						const finalFulfillmentValue = currentWidth / rectWidth;

						// Ensure the fulfillment value is set properly before recomputing
						d.data.fulfillment = finalFulfillmentValue;

						// Update the tooltip
						nodeGroup
							.select('title')
							.text(`${name(d)}\nFulfillment: ${Math.round(finalFulfillmentValue * 100)}%`);

						// Recompute hierarchy and update
						hierarchy
							.sum((node) => node.data.points)
							.each((node) => {
								node.value = node.data.points || 0;
							});

						// Apply treemap
						const treemap = d3.treemap().tile(tile);
						treemap(hierarchy);

						// Force update the visualization
						position(group, currentView);
					});

				// Add the same growth/shrink handler to the window shade and handle
				// This ensures growth/shrink works even when window shade is at 100%
				let isDragging = false;

				// Simple flag to track dragging state
				indicator.call(
					dragBehavior
						.on('start.flag', () => {
							isDragging = true;
						})
						.on('end.flag', () => {
							isDragging = false;
						})
				);
				handle.call(
					dragBehavior
						.on('start.flag', () => {
							isDragging = true;
						})
						.on('end.flag', () => {
							isDragging = false;
						})
				);

				// Simplified event forwarding - only handle mousedown/touchstart for growth
				indicator.on('mousedown touchstart', (event) => {
					// Only forward if not dragging (important for performance)
					if (!isDragging) {
						// Forward the event to the parent node's handler
						node
							.filter((n) => n === d)
							.each(function () {
								const handler = d3.select(this).on('mousedown touchstart');
								handler.call(this, event, d);
							});
					}
				});

				handle.on('mousedown touchstart', (event) => {
					// Only forward if not dragging (important for performance)
					if (!isDragging) {
						// Forward the event to the parent node's handler
						node
							.filter((n) => n === d)
							.each(function () {
								const handler = d3.select(this).on('mousedown touchstart');
								handler.call(this, event, d);
							});
					}
				});
			});

		node
			.append('clipPath')
			.attr('id', (d) => (d.clipUid = uid('clip')).id)
			.append('use')
			.attr('xlink:href', (d) => d.leafUid.href);

		node
			.append('text')
			.attr('clip-path', (d) => d.clipUid)
			.attr('font-weight', (d) => (d === root ? 'bold' : null))
			.style('user-select', 'none')
			.style('-webkit-user-select', 'none')
			.style('-moz-user-select', 'none')
			.style('-ms-user-select', 'none')
			.attr('transform', (d) => {
				const rectWidth = d === root ? width : x(d.x1) - x(d.x0);
				const rectHeight = d === root ? 50 : y(d.y1) - y(d.y0);
				return `translate(${rectWidth / 2},${rectHeight / 2})`;
			})
			.style('text-anchor', 'middle')
			.style('dominant-baseline', 'middle')
			.style('font-size', (d) => {
				const rectWidth = d === root ? width : x(d.x1) - x(d.x0);
				const rectHeight = d === root ? 50 : y(d.y1) - y(d.y0);
				return calculateFontSize(d, rectWidth, rectHeight, root, x, y, currentView) + 'px';
			})
			.selectAll('tspan')
			.data((d) => {
				if (d === root) return [name(d)];
				return d.data.name.split(/(?=[A-Z][^A-Z])/g);
			})
			.join('tspan')
			.attr('x', 0)
			.attr('dy', (d, i, nodes) => {
				if (i === 0) {
					// Move first line up by half the total height of all lines
					return `${(-(nodes.length - 1) * 1.2) / 2}em`;
				}
				return '1.2em'; // Standard line spacing for subsequent lines
			})
			.text((d) => d);

		group.call(position, root);

		// Add type circles container after the rect
		const typeContainer = node
			.append('g')
			.attr('class', 'type-indicators')
			.attr('transform', (d) => {
				const rectWidth = d === root ? width : x(d.x1) - x(d.x0);
				return `translate(${rectWidth - 15}, 15)`; // Moved slightly more from edge
			});

		// Add circles for each type
		typeContainer.each(function (d) {
			if (!d.data.types) return;

			const container = d3.select(this);
			const circleRadius = 8;
			const spacing = circleRadius * 2.5;

			// Debug the types data
			console.log('Types for node:', d.data.name, d.data.types);

			const circles = container
				.selectAll('circle')
				.data(d.data.types)
				.join('circle')
				.attr('cx', (_, i) => -i * spacing)
				.attr('cy', 0)
				.attr('r', circleRadius)
				.attr('fill', (type) => getColorForName(type.name))
				.attr('stroke', '#fff')
				.attr('stroke-width', '2')
				.attr('cursor', 'pointer')
				.style('pointer-events', 'all'); // Ensure circles receive events

			// Attach click handler separately
			circles.on('click', function (event, type) {
				console.log('Circle clicked!');
				event.stopPropagation();

				console.log('Loading tree for type:', type.name);

				// Update current view
				currentView = type;

				// Clear existing content and recreate group
				group.selectAll('*').remove();

				// Create new hierarchy using childrenArray for D3Node
				hierarchy = d3
					.hierarchy(type, (d) => d.childrenArray)
					.sum((d) => d.points)
					.each((node) => {
						node.value = node.data.points || 0;
					});

				// Apply treemap layout
				const treemap = d3.treemap().tile(tile);
				root = treemap(hierarchy);

				// Reset domains
				x.domain([root.x0, root.x1]);
				y.domain([root.y0, root.y1]);

				// Render new view
				render(group, root);
			});

			circles.append('title').text((type) => `Click to view ${type.name}'s tree`);
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

		node
			.filter((d) => true)
			.attr('cursor', 'pointer')
			.on('mousedown touchstart', (event, d) => {
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
						// Explicitly handle each input type
						let isShrinking = false;

						if (event.type === 'mousedown') {
							// Mouse events: button 0 = left (grow), button 2 = right (shrink)
							if (event.button === 0) {
								isShrinking = false; // Left click = grow
							} else if (event.button === 2) {
								isShrinking = true; // Right click = shrink
							}
						} else if (event.type === 'touchstart') {
							// Touch events: 1 finger = grow, 2 fingers = shrink
							isShrinking = event.touches.length === 2;
						}

						console.log('Mouse event details:');
						console.log('- Type:', event.type);
						console.log('- Button:', event.button, '(0=left, 1=middle, 2=right)');
						console.log('- Shrinking mode:', isShrinking);
						console.log('- Target node:', d.data.name);

						growthTimeout = setTimeout(() => {
							// Only start growing/shrinking if still touching the same node
							if (isTouching && activeNode === d) {
								console.log('Starting growth/shrink after delay:');
								console.log('- Shrinking mode:', isShrinking);
								console.log('- Growth rate:', isShrinking ? SHRINK_RATE(d) : GROWTH_RATE(d));
								isGrowing = true;
								growthInterval = setInterval(() => {
									// Only continue if still touching
									if (!isTouching) {
										clearInterval(growthInterval);
										growthInterval = null;
										isGrowing = false;
										return;
									}

									// Calculate growth/shrink amount - More explicit
									let rate;
									if (isShrinking) {
										rate = SHRINK_RATE(d); // Negative value for shrinking
										console.log(`Shrinking ${d.data.name} by ${rate}`);
									} else {
										rate = GROWTH_RATE(d); // Positive value for growing
										console.log(`Growing ${d.data.name} by ${rate}`);
									}

									const oldPoints = d.data.points;
									const newPoints = Math.max(0, oldPoints + rate); // Prevent negative points
									console.log(`Points changing: ${oldPoints} → ${newPoints}`);

									d.data.setPoints(newPoints);

									// Recompute hierarchy ensuring values match points
									hierarchy
										.sum((node) => node.data.points)
										.each((node) => {
											// Force value to exactly match points
											node.value = node.data.points || 0;
										});

									// Apply treemap
									const treemap = d3.treemap().tile(tile);
									treemap(hierarchy);

									// Update visualization including type indicators
									const nodes = group.selectAll('g').filter((node) => node !== root);

									// Existing transitions
									nodes
										.transition()
										.duration(GROWTH_TICK)
										.attr('transform', (d) =>
											d === root ? `translate(0,-50)` : `translate(${x(d.x0)},${y(d.y0)})`
										);

									// Transition rectangles
									nodes
										.select('rect.node-rect')
										.transition()
										.duration(GROWTH_TICK)
										.attr('width', (d) => (d === root ? width : Math.max(0, x(d.x1) - x(d.x0))))
										.attr('height', (d) => (d === root ? 50 : Math.max(0, y(d.y1) - y(d.y0))));

									// Update fulfillment indicator rectangle
									nodes
										.select('rect.fulfillment-indicator')
										.transition()
										.duration(GROWTH_TICK)
										.attr('width', (d) => {
											if (d === root) return 0;
											const fullWidth = x(d.x1) - x(d.x0);
											const fulfillmentPercentage = d.data.fulfilled;
											return Math.max(0, fullWidth * fulfillmentPercentage);
										})
										.attr('height', (d) => (d === root ? 0 : Math.max(0, y(d.y1) - y(d.y0))));

									// Update text positions
									nodes
										.select('text')
										.transition()
										.duration(GROWTH_TICK)
										.attr('transform', (d) => {
											const rectWidth = d === root ? width : x(d.x1) - x(d.x0);
											const rectHeight = d === root ? 50 : y(d.y1) - y(d.y0);
											return `translate(${rectWidth / 2},${rectHeight / 2})`;
										})
										.style('font-size', (d) => {
											const rectWidth = d === root ? width : x(d.x1) - x(d.x0);
											const rectHeight = d === root ? 50 : y(d.y1) - y(d.y0);
											return (
												calculateFontSize(d, rectWidth, rectHeight, root, x, y, currentView) + 'px'
											);
										});

									// Update handle positions to match fulfillment indicator widths
									nodes.select('rect.fulfillment-handle').each(function (d) {
										if (d === root) return;

										const indicator = d3
											.select(this.parentNode)
											.select('rect.fulfillment-indicator');
										const indicatorWidth = parseFloat(indicator.attr('width'));
										const rectHeight = y(d.y1) - y(d.y0);

										// Update handle position and height, keeping it 75% down the rectangle
										d3.select(this)
											.attr('x', indicatorWidth - 4)
											.attr('y', rectHeight * 0.75) // Position it 75% down the rectangle
											.attr('height', 20);
									});

									// Add type indicator updates here
									nodes
										.select('.type-indicators')
										.transition()
										.duration(GROWTH_TICK)
										.attr('transform', (d) => {
											const rectWidth = d === root ? width : x(d.x1) - x(d.x0);
											return `translate(${rectWidth - 10}, 10)`;
										});

									// console.log("\nFinal values:");
									// console.log("Node points:", d.data.points);
									// console.log("Node value:", d.value);
									// console.log("Hierarchy value:", hierarchy.value);
								}, GROWTH_TICK);
							}
						}, GROWTH_DELAY);
					}
				}
			})
			.on('mouseup touchend touchcancel', (event) => {
				// Only handle if not in contributor tree
				if (!isInContributorTree()) {
					event.preventDefault();

					// Clear all states
					isTouching = false;
					activeNode = null;

					// Stop growth
					if (growthTimeout) clearTimeout(growthTimeout);
					if (growthInterval) clearInterval(growthInterval);
					growthInterval = null;
					isGrowing = false;
				}
			})
			.on('click touchend', (event, d) => {
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
						zoomout(root);
					} else if (d !== root && !d.data.isContribution) {
						// Check isContribution directly
						console.log('Attempting zoom in to:', d.data.name);
						zoomin(d);
					}
				} else {
					console.log(
						'Navigation blocked because:',
						touchDuration >= GROWTH_DELAY ? 'touch too long' : 'growing active'
					);
				}

				// Clear states only if not in contributor tree
				if (!isInContributorTree()) {
					isTouching = false;
					activeNode = null;
					isGrowing = false;
				}
			});

		if (root.data.children.size === 0 && root !== data) {
			// Check if view is empty and not root
			group
				.append('text')
				.attr('class', 'helper-text')
				.attr('text-anchor', 'middle')
				.attr('dominant-baseline', 'middle')
				.attr('x', width / 2)
				.attr('y', height / 2)
				.style('font-size', '24px')
				.style('fill', '#666')
				.style('pointer-events', 'none')
				.style('user-select', 'none')
				.text('Add Values / Contributors');
		}

		// After creating the node groups and before position call
		node
			.filter((d) => d === root) // Only for the root navigation rectangle
			.each(function (d) {
				// Check if we're in a contributor tree (no path to original data)
				let temp = d;
				let isContributorTree = true;
				while (temp) {
					if (temp.data === data) {
						// data is the original root passed to createTreemap
						isContributorTree = false;
						break;
					}
					temp = temp.parent;
				}

				if (isContributorTree) {
					// Add home button
					d3.select(this)
						.append('g')
						.attr('class', 'home-button')
						.attr('transform', 'translate(20, 25)') // 25 is 50% of the 50px height
						.style('cursor', 'pointer')
						.on('click', (event) => {
							event.stopPropagation();
							// Clear existing content
							group.selectAll('*').remove();

							// Reset to original data
							hierarchy = d3
								.hierarchy(data, (d) => d.childrenArray)
								.sum((d) => d.data.points)
								.each((d) => {
									d.value = d.data.points || 0;
								});

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
						.append('text')
						.attr('fill', '#000')
						.attr('font-size', '20px')
						.attr('dominant-baseline', 'middle') // Vertically center the text
						.text('🏠'); // Unicode home emoji
				}
			});
	}

	function zoomin(d) {
		console.log('Zooming in to:', d.data.name);
		currentView = d;
		const group0 = group.attr('pointer-events', 'none');

		// Update domains first
		x.domain([d.x0, d.x1]);
		y.domain([d.y0, d.y1]);

		const group1 = (group = svg.append('g').call(render, d));

		svg
			.transition()
			.duration(750)
			.call((t) => group0.transition(t).remove().call(position, d.parent))
			.call((t) =>
				group1
					.transition(t)
					.attrTween('opacity', () => d3.interpolate(0, 1))
					.call(position, d)
			);
	}

	function zoomout(d) {
		console.log('Zooming out from:', d.data.name);
		currentView = d.parent;
		const group0 = group.attr('pointer-events', 'none');

		// Update domains first
		x.domain([d.parent.x0, d.parent.x1]);
		y.domain([d.parent.y0, d.parent.y1]);

		const group1 = (group = svg.insert('g', '*').call(render, d.parent));

		svg
			.transition()
			.duration(750)
			.call((t) =>
				group0
					.transition(t)
					.remove()
					.attrTween('opacity', () => d3.interpolate(1, 0))
					.call(position, d)
			)
			.call((t) => group1.transition(t).call(position, d.parent));
	}

	// Add drag handlers
	function dragStarted(event, d) {
		// Don't start dragging if growth is already active
		if (isGrowing) {
			console.log('Ignoring drag start because growth is active');
			return;
		}

		// Don't cancel growth immediately - check if there's actual movement first
		console.log('Potential drag detected on node:', d.data.name);

		// Save the dragged node, but don't allow dragging the root
		if (d === root) {
			console.log('Cannot drag root node');
			return;
		}

		// Don't allow dragging in contributor trees
		if (isInContributorTree()) {
			console.log('Cannot drag nodes in contributor trees');
			return;
		}

		// Only start actual dragging if there's real movement
		// We'll check this in the drag event
	}

	function dragging(event, d) {
		// Only continue with dragging if not currently growing
		if (isGrowing) {
			console.log('Ignoring drag because growth is active');
			return;
		}

		// If we haven't set draggedNode yet, this is the first drag event
		if (!draggedNode) {
			console.log('Starting real drag with movement');

			// Now we know it's a real drag, not just a hold - cancel growth
			if (growthTimeout) {
				clearTimeout(growthTimeout);
				growthTimeout = null;
			}
			if (growthInterval) {
				clearInterval(growthInterval);
				growthInterval = null;
			}

			draggedNode = d;

			// Highlight the node being dragged
			d3.select(event.sourceEvent.target.parentNode)
				.select('rect')
				.attr('stroke', '#f39c12')
				.attr('stroke-width', '3');

			// Raise the element being dragged to the front
			d3.select(event.sourceEvent.target.parentNode).raise();
		}

		// For treemap, handle differently - just change opacity and highlight
		d3.select(event.sourceEvent.target.parentNode).attr('opacity', 0.7);

		// Find potential target node under cursor
		// Reset previous target highlight if exists
		if (dragTarget) {
			d3.selectAll('g')
				.filter((n) => n === dragTarget)
				.select('rect')
				.attr('stroke', '#fff');
		}

		// Get all nodes and find one under the cursor (except the dragged node and its children)
		const allNodes = group.selectAll('g').nodes();
		dragTarget = null;

		for (const nodeElem of allNodes) {
			const targetNode = d3.select(nodeElem).datum();
			if (targetNode === draggedNode || targetNode === root) continue;

			// Skip if this node is a descendant of the dragged node (can't parent to own child)
			let isDescendant = false;
			let temp = targetNode;
			while (temp) {
				if (temp === draggedNode) {
					isDescendant = true;
					break;
				}
				if (temp === root) break;
				temp = temp.parent;
			}
			if (isDescendant) continue;

			// Get node rectangle dimensions
			const rectElem = d3.select(nodeElem).select('rect').node();
			if (!rectElem) continue;

			const rectBounds = rectElem.getBoundingClientRect();
			const mouseX = event.sourceEvent.clientX;
			const mouseY = event.sourceEvent.clientY;

			// Check if mouse is inside the target node's bounds
			if (
				mouseX >= rectBounds.left &&
				mouseX <= rectBounds.right &&
				mouseY >= rectBounds.top &&
				mouseY <= rectBounds.bottom
			) {
				dragTarget = targetNode;

				// Highlight potential target
				d3.select(nodeElem).select('rect').attr('stroke', '#27ae60').attr('stroke-width', '3');

				break;
			}
		}
	}

	function dragEnded(event, d) {
		if (!draggedNode) return;

		console.log('Drag ended', draggedNode.data.name);

		// Reset opacity
		d3.select(this).attr('opacity', 1);

		// Remove highlighting
		d3.select(this).select('rect').attr('stroke', '#fff').attr('stroke-width', '1');

		// Remove target highlighting if any
		if (dragTarget) {
			d3.selectAll('g')
				.filter((n) => n === dragTarget)
				.select('rect')
				.attr('stroke', '#fff')
				.attr('stroke-width', '1');
		}

		// If we have a valid target and it's different than the current parent
		if (dragTarget && draggedNode.parent !== dragTarget) {
			// Get the current parent node
			const oldParent = draggedNode.parent;

			console.log(
				`Reparenting node "${draggedNode.data.name}" from "${oldParent.data.name}" to "${dragTarget.data.name}"`
			);

			// 1. Remove the node from its current parent in the data structure
			const nodeName = draggedNode.data.name;
			oldParent.data.children.delete(nodeName);

			// 2. Add it to the new parent
			if (dragTarget.data.children.has(nodeName)) {
				console.log(
					`Warning: Node with name "${nodeName}" already exists as a child of "${dragTarget.data.name}"`
				);
			} else {
				// Add to the new parent's children
				const childNode = draggedNode.data;
				childNode.parent = dragTarget.data;
				dragTarget.data.children.set(nodeName, childNode);

				console.log('Data structure updated:');
				console.log("- Old parent's children:", Array.from(oldParent.data.children.keys()));
				console.log("- New parent's children:", Array.from(dragTarget.data.children.keys()));
			}

			// Update the D3 hierarchy from the modified data
			hierarchy = d3
				.hierarchy(data, (d) => d.childrenArray)
				.sum((d) => d.data.points)
				.each((d) => {
					d.value = d.data.points || 0;
				});

			// Apply the treemap layout
			const treemap = d3.treemap().tile(tile);
			root = treemap(hierarchy);
			currentView = root;

			// Reset the domains
			x.domain([root.x0, root.x1]);
			y.domain([root.y0, root.y1]);

			// Clear and redraw
			svg.selectAll('g').remove();
			group = svg.append('g').call(render, root);
		}

		// Reset drag state
		draggedNode = null;
		dragTarget = null;
	}

	// Return public interface with functions to get current state
	return {
		getCurrentView: () => currentView,
		getCurrentData: () => data,
		element: svg.node(),
		getRoot: () => root,
		zoomin,
		zoomout,
		update: (newWidth, newHeight) => {
			console.log('Update called with dimensions:', newWidth, height);

			// Update scales
			x.rangeRound([0, newWidth]);
			y.rangeRound([0, newHeight]);

			// Update SVG viewBox
			svg.attr('viewBox', [0.5, -50.5, newWidth, newHeight + 50]);

			// Clear existing content and create new group
			group.remove(); // Remove old group
			group = svg.append('g'); // Create new group

			// Update visualization with current view
			group.call(render, currentView); // Render current view instead of root
		},
		// Highlight nodes that match search results
		highlightNodes: (matchedNodes) => {
			// Clear any existing highlights
			svg.selectAll('rect').classed('search-highlight', false);

			// Find all matched nodes in the visualization
			svg
				.selectAll('rect.node-rect') // Use the correct class
				.filter((d) => {
					return matchedNodes.some((node) => node.name === d.data.name);
				})
				.classed('search-highlight', true);

			// If there are matches, zoom to the first match
			if (matchedNodes.length > 0) {
				const firstMatch = matchedNodes[0];

				// Find the node in the hierarchy
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
					return null;
				};

				const matchedNode = findNode(root);

				if (matchedNode) {
					// Zoom directly to the matched node, not its ancestor
					if (matchedNode !== currentView) {
						zoomin(matchedNode);
					}
				}
			}
		},
		// Clear search highlights
		clearHighlights: () => {
			svg.selectAll('rect').classed('search-highlight', false);
		}
	};
}

// Export the update function to allow external updates
export function updateTreemap(root) {
	// Re-render the treemap
	const container = document.getElementById('treemap-container');

	// Clear the container
	container.innerHTML = '';

	// Create a new treemap
	const treemap = createTreemap(root.data, container.clientWidth, container.clientHeight);

	// Append the new treemap to the container
	container.appendChild(treemap);
}
