<script lang="ts">
	import { onMount, onDestroy } from 'svelte';
	import * as d3 from 'd3';
	import { globalState } from '$lib/global.svelte';
	import Child from '$lib/components/Child.svelte';
	import DropDown from '$lib/components/DropDown.svelte';
	import { browser } from '$app/environment';

	// Import centralized system functions
	import { GunUserTree, enterChild, exitToParent } from '$lib/centralized';

	// Import our types
	import type { TreeZipper, Node } from '$lib/centralized/types';

	// Define a type for visualization data
	interface VisualizationNode {
		id: string;
		zipper: TreeZipper | null;
		points: number;
		children: VisualizationNode[];
		nodeName?: string;
		contributors?: string[];
	}

	// Directly use the global state without duplicating it
	let labelIndex = $state(0);
	const nodeLabels = ['Node', 'Value', 'Goal', 'Dependency', 'Desire', 'Contribution'];

	// For tracking changes in d3 visualization
	let updateCounter = $state(0);

	// UI interaction state
	let showUserDropdown = $state(false);
	let dropdownPosition = $state({ x: 0, y: 0 });
	let activeNodeId = $state<string | null>(null);

	// Growth state
	let touchStartTime = $state(0);
	let isTouching = $state(false);
	let activeGrowthNodeId = $state<string | null>(null);
	let isGrowing = $state(false);
	let isShrinkingActive = $state(false);
	let growthInterval = $state<number | null>(null);
	let growthTimeout = $state<number | null>(null);

	// Growth constants
	const GROWTH_DELAY = 350;
	const GROWTH_TICK = 16;
	const BASE_GROWTH_RATE = 0.8;
	const BASE_SHRINK_RATE = -0.8;
	const TAP_THRESHOLD = 250;

	// Get children of current zipper (derived from global state)
	let childZippers = $state<TreeZipper[]>([]);

	$effect(() => {
		if (!globalState.currentZipper) {
			childZippers = [];
			return;
		}

		childZippers = Array.from(globalState.currentZipper.zipperCurrent.nodeChildren.keys())
			.map((id) => {
				if (!globalState.currentZipper) return null;
				return enterChild(id, globalState.currentZipper);
			})
			.filter(Boolean) as TreeZipper[];
	});

	// Format data for d3 to consume
	let packData = $state({ id: '', selfPoints: 0, children: [] as VisualizationNode[] });

	$effect(() => {
		const zipper = globalState.currentZipper;
		if (!zipper) {
			packData = { id: '', selfPoints: 0, children: [] };
			return;
		}

		const children = childZippers.map((child: TreeZipper) => ({
			id: child.zipperCurrent.nodeId,
			zipper: child,
			points: child.zipperCurrent.nodePoints,
			nodeName: child.zipperCurrent.nodeName,
			contributors: Array.from(child.zipperCurrent.nodeContributors),
			children: [] as VisualizationNode[]
		}));

		packData = {
			id: zipper.zipperCurrent.nodeId,
			selfPoints: zipper.zipperCurrent.nodePoints,
			children
		};
	});

	// Create hierarchy for d3
	let hierarchyData: d3.HierarchyRectangularNode<VisualizationNode> | null = $state(null);

	$effect(() => {
		const data = packData;
		// Force rerender when updateCounter changes
		updateCounter;

		// Create hierarchy
		const rootNode: VisualizationNode = {
			id: data.id,
			zipper: globalState.currentZipper,
			points: data.selfPoints,
			children: data.children
		};

		const hierarchy = d3.hierarchy<VisualizationNode>(rootNode, (d) => d.children);

		// Sum for sizing
		hierarchy.sum((d) => d.points);

		// Sort by value for better layout
		hierarchy.sort((a, b) => b.value! - a.value!);

		// Custom tile function to ensure nodes fill their container
		function customTile(
			node: d3.HierarchyRectangularNode<VisualizationNode>,
			x0: number,
			y0: number,
			x1: number,
			y1: number
		) {
			// Apply the standard binary tiling algorithm to a normalized space
			d3.treemapBinary(node, 0, 0, 1, 1);

			// Scale the output to fit the current viewport
			for (const child of node.children || []) {
				child.x0 = x0 + child.x0 * (x1 - x0);
				child.x1 = x0 + child.x1 * (x1 - x0);
				child.y0 = y0 + child.y0 * (y1 - y0);
				child.y1 = y0 + child.y1 * (y1 - y0);
			}
		}

		// Apply treemap with custom tiling
		const treemap = d3
			.treemap<VisualizationNode>()
			.tile(customTile)
			.size([1, 1])
			.padding(0.005)
			.round(false);

		hierarchyData = treemap(hierarchy);
	});

	onMount(() => {
		// Set up event listeners
		document.addEventListener('mouseup', handleGlobalTouchEnd);
		document.addEventListener('touchend', handleGlobalTouchEnd);
		document.addEventListener('touchcancel', handleGlobalTouchEnd);

		// Start label cycling interval
		const interval = setInterval(() => {
			labelIndex = (labelIndex + 1) % nodeLabels.length;
		}, 4000);

		return () => {
			// Clean up timers and event listeners
			if (growthInterval !== null) clearInterval(growthInterval);
			if (growthTimeout !== null) clearTimeout(growthTimeout);
			clearInterval(interval);
			document.removeEventListener('mouseup', handleGlobalTouchEnd);
			document.removeEventListener('touchend', handleGlobalTouchEnd);
			document.removeEventListener('touchcancel', handleGlobalTouchEnd);
		};
	});

	// Navigation functions
	function zoomInto(nodeId: string) {
		console.log('[UI FLOW] zoomInto called for node:', nodeId);
		// Use the centralized navigation function
		globalState.zoomInto(nodeId);
		console.log('[UI FLOW] Navigation handled by global state');
	}

	async function handleAddNode() {
		console.log('[UI FLOW] handleAddNode started');
		if (!globalState.currentZipper) {
			console.log('[UI FLOW] No currentZipper, aborting');
			return;
		}

		// Calculate initial points for new node
		const calculateNewNodePoints = (): number => {
			// No siblings - set to 100 points
			if (!hierarchyData?.children || hierarchyData.children.length === 0) {
				console.log('[UI FLOW] No siblings, setting new node to 100 points');
				return 100;
			}

			// Has siblings - use 10% of total points
			const currentLevelPoints = hierarchyData.children.reduce(
				(sum: number, node: any) => sum + (node.data?.points || 0),
				0
			);
			const points = Math.max(1, currentLevelPoints * 0.1);
			console.log('[UI FLOW] Calculated points based on siblings:', points);
			return points;
		};

		const newPoints = calculateNewNodePoints();
		const newNodeId = `node_${Date.now()}_${Math.random().toString(36).substring(2, 9)}`; // Use GunUserTree's ID format
		const newNodeName = 'New Node'; // Default name for better UX

		console.log('[UI FLOW] Creating new node:', {
			id: newNodeId,
			points: newPoints,
			name: newNodeName
		});
		console.log('[UI FLOW] Current parent:', globalState.currentZipper.zipperCurrent.nodeId);
		console.log('[UI FLOW] Current path:', globalState.currentPath);

		try {
			// Use the centralized addNode function with a default name
			console.log('[UI FLOW] Calling globalState.addNode');
			const success = await globalState.addNode(newNodeId, newPoints, newNodeName);

			if (!success) {
				console.log('[UI FLOW] addNode returned false');
				globalState.showToast('Error creating node', 'error');
				return;
			}

			console.log('[UI FLOW] addNode successful');
			globalState.showToast('New node created', 'success');

			// Set node to edit mode
			console.log('[UI FLOW] Setting node to edit mode:', newNodeId);
			setTimeout(() => {
				globalState.setNodeToEditMode(newNodeId);
				console.log('[UI FLOW] Node edit mode set');
			}, 50);
		} catch (err) {
			console.error('[UI FLOW] Error in handleAddNode:', err);
			globalState.showToast('Error creating node', 'error');
		}
	}

	// Assign to global handle for toolbar access
	$effect(() => {
		globalState.handleAddNode = handleAddNode;
	});

	// Text editing handler
	function handleTextEdit(detail: { nodeId: string; newName: string }) {
		const { nodeId, newName } = detail;

		try {
			if (!globalState.currentZipper) return;

			// We'll use the global state's updateNode function
			globalState
				.updateNode(nodeId, { name: newName })
				.then((success) => {
					if (success) {
						globalState.showToast(`Node renamed to "${newName}"`, 'success');
					} else {
						globalState.showToast('Error updating node name', 'error');
					}
				})
				.catch((err) => {
					console.error(`Error updating name for node ${nodeId}:`, err);
					globalState.showToast('Error updating node name', 'error');
				});
		} catch (err) {
			console.error(`Error updating name for node ${nodeId}:`, err);
			globalState.showToast('Error updating node name', 'error');
		}
	}

	// Contributor management
	function handleAddContributor(detail: { nodeId: string; clientX: number; clientY: number }) {
		const { nodeId, clientX, clientY } = detail;

		// Show user dropdown
		activeNodeId = nodeId;
		dropdownPosition = { x: clientX, y: clientY };
		showUserDropdown = true;
	}

	function handleRemoveContributor(detail: { nodeId: string; contributorId: string }) {
		const { nodeId, contributorId } = detail;

		try {
			if (!globalState.currentZipper) return;

			// Find the child zipper to get current contributors
			const childZipper = enterChild(nodeId, globalState.currentZipper);
			if (!childZipper) return;

			// Get current contributors and remove the specified one
			const contributors = Array.from(childZipper.zipperCurrent.nodeContributors);
			const updatedContributors = contributors.filter((id) => id !== contributorId);

			// Use global state's updateNode function
			globalState
				.updateNode(nodeId, { contributors: updatedContributors })
				.then((success) => {
					if (success) {
						globalState.showToast('Contributor removed successfully', 'success');
					} else {
						globalState.showToast('Error removing contributor', 'error');
					}
				})
				.catch((err) => {
					console.error('Error removing contributor:', err);
					globalState.showToast('Error removing contributor', 'error');
				});
		} catch (err) {
			console.error('Error removing contributor:', err);
			globalState.showToast('Error removing contributor', 'error');
		}
	}

	function handleUserSelect(detail: { id: string; name: string }) {
		const { id: userId } = detail;

		if (!activeNodeId) {
			console.error('No node selected for adding contributor');
			return;
		}

		// Add the contributor
		addContributorToNode(activeNodeId, userId);

		// Reset dropdown
		showUserDropdown = false;
		activeNodeId = null;
	}

	function handleDropdownClose() {
		showUserDropdown = false;
		activeNodeId = null;
	}

	function addContributorToNode(nodeId: string, userId: string) {
		try {
			if (!globalState.currentZipper) return;

			// Find the child zipper to get current contributors
			const childZipper = enterChild(nodeId, globalState.currentZipper);
			if (!childZipper) return;

			// Get current contributors and add the new one
			const contributors = Array.from(childZipper.zipperCurrent.nodeContributors);
			if (!contributors.includes(userId)) {
				contributors.push(userId);
			}

			// Use global state's updateNode function
			globalState
				.updateNode(nodeId, { contributors })
				.then((success) => {
					if (success) {
						globalState.showToast('Contributor added successfully', 'success');
					} else {
						globalState.showToast('Error adding contributor', 'error');
					}
				})
				.catch((err) => {
					console.error('Error adding contributor:', err);
					globalState.showToast('Error adding contributor', 'error');
				});
		} catch (err) {
			console.error('Error adding contributor:', err);
			globalState.showToast('Error adding contributor', 'error');
		}
	}

	// Growth handlers
	function startGrowth(node: d3.HierarchyRectangularNode<VisualizationNode>, isShrinking = false) {
		// Don't allow growth in delete mode
		if (globalState.deleteMode) return;

		// Clear existing growth state
		if (growthInterval !== null) clearInterval(growthInterval);
		if (growthTimeout !== null) clearTimeout(growthTimeout);
		isGrowing = false;
		isShrinkingActive = false;

		// Only run timeouts in browser environment
		if (!browser) return;

		// Set growth delay
		growthTimeout = window.setTimeout(() => {
			// Only start growing if still touching same node
			if (isTouching && activeGrowthNodeId === node.data.id) {
				isGrowing = true;
				isShrinkingActive = isShrinking;

				growthInterval = window.setInterval(() => {
					// Stop if no longer touching
					if (!isTouching) {
						stopGrowth();
						return;
					}

					// Calculate growth rate based on current size
					let rate;
					if (isShrinking) {
						// More aggressive shrinking for larger nodes
						rate = Math.min(
							-0.2,
							Math.max(-4, BASE_SHRINK_RATE * Math.sqrt(node.data.points / 10))
						);
					} else {
						// More aggressive growth for smaller nodes
						rate = Math.max(0.2, Math.min(4, BASE_GROWTH_RATE * Math.sqrt(node.data.points / 10)));
					}

					const currentPoints = node.data.points;
					const newPoints = Math.max(1, currentPoints + rate);

					if (isNaN(newPoints)) {
						console.error('Growth calculation resulted in NaN:', {
							currentPoints,
							rate,
							isShrinking
						});
						return;
					}

					// Always update to ensure animation runs smoothly
					updateNodePoints(node, newPoints);
				}, GROWTH_TICK);
			}
		}, GROWTH_DELAY);
	}

	function stopGrowth() {
		isTouching = false;

		// Save final points if growing
		if (isGrowing && activeGrowthNodeId && hierarchyData) {
			const nodeToUpdate = hierarchyData.children?.find(
				(child: d3.HierarchyRectangularNode<VisualizationNode>) =>
					child.data.id === activeGrowthNodeId
			);

			if (nodeToUpdate && nodeToUpdate.data && nodeToUpdate.data.id) {
				saveNodePoints(nodeToUpdate.data.id, nodeToUpdate.data.points);
			}
		}

		// Reset growth state
		activeGrowthNodeId = null;
		if (growthTimeout !== null) clearTimeout(growthTimeout);
		if (growthInterval !== null) clearInterval(growthInterval);
		growthInterval = null;
		isGrowing = false;
		isShrinkingActive = false;
	}

	function updateNodePoints(node: d3.HierarchyRectangularNode<VisualizationNode>, points: number) {
		// Update node's points in hierarchy
		node.data.points = points;

		// Trigger a reactive update
		updateCounter++;
	}

	function saveNodePoints(nodeId: string, points: number) {
		try {
			if (!globalState.currentZipper) return;

			// Use global state's updateNode function
			globalState
				.updateNode(nodeId, { points })
				.then((success) => {
					if (success) {
						console.log(`Saved points for node ${nodeId}: ${points}`);
					} else {
						globalState.showToast('Error saving node points', 'error');
					}
				})
				.catch((err) => {
					console.error(`Error saving points for node ${nodeId}:`, err);
					globalState.showToast('Error saving node points', 'error');
				});
		} catch (err) {
			console.error(`Error saving points for node ${nodeId}:`, err);
			globalState.showToast('Error saving node points', 'error');
		}
	}

	function handleGlobalTouchEnd() {
		if (isTouching) {
			stopGrowth();
		}
	}

	function handleGrowthStart(
		event: MouseEvent | TouchEvent,
		node: d3.HierarchyRectangularNode<VisualizationNode>
	) {
		// Set touch state
		isTouching = true;
		touchStartTime = Date.now();
		activeGrowthNodeId = node.data.id;

		// Determine if shrinking (right-click or two-finger touch)
		const isShrinking =
			event instanceof MouseEvent ? event.button === 2 : (event as TouchEvent).touches.length === 2;

		// Start growth with delay
		startGrowth(node, isShrinking);
	}

	function handleGrowthEnd(event: MouseEvent | TouchEvent) {
		// Check if the click target is a text edit field, node-text element, or contributor element
		const target = event.target as HTMLElement;
		if (
			target &&
			(target.closest('.node-text') ||
				target.closest('.node-text-edit-container') ||
				target.closest('.add-contributor-button') ||
				target.closest('.tag-container') ||
				target.closest('.contributor-container'))
		) {
			// Click originated from text or contributor elements, don't navigate
			return;
		}

		// Check if this was a short tap (for navigation) or long press (for growth)
		const touchDuration = Date.now() - touchStartTime;
		const wasGrowthEvent = touchDuration >= TAP_THRESHOLD || isGrowing;

		// Get the nodeId before stopping growth
		const nodeId = activeGrowthNodeId;

		// Always stop growth
		stopGrowth();

		// For short taps, trigger navigation or deletion
		if (!wasGrowthEvent && nodeId) {
			if (globalState.deleteMode) {
				// Handle deletion
				handleNodeDeletion(nodeId);
			} else {
				// This was a tap, not a hold - navigate into the node
				zoomInto(nodeId);
			}
		}
	}

	function handleNodeDeletion(nodeId: string) {
		if (confirm(`Delete this node?`)) {
			console.log('[UI FLOW] Deleting node:', nodeId);

			// Use the centralized deletion method
			globalState
				.deleteNode(nodeId)
				.then((success) => {
					if (success) {
						console.log('[UI FLOW] Node deleted successfully');
						globalState.showToast('Node deleted successfully', 'success');
					} else {
						console.error('[UI FLOW] Failed to delete node');
						globalState.showToast('Failed to delete node', 'error');
					}
				})
				.catch((err) => {
					console.error('[UI FLOW] Error deleting node:', err);
					globalState.showToast('Error deleting node', 'error');
				});
		}
	}
</script>

<div class="node-container">
	<!-- Main treemap content -->
	<div class="app-content">
		<div class="treemap-container">
			{#if hierarchyData && hierarchyData.children && hierarchyData.children.length > 0}
				{#each hierarchyData.children as child}
					<div
						class="clickable"
						class:deleting={globalState.deleteMode}
						class:growing={isGrowing && activeGrowthNodeId === child.data.id && !isShrinkingActive}
						class:shrinking={isGrowing && activeGrowthNodeId === child.data.id && isShrinkingActive}
						style="
							position: absolute;
							left: {child.x0 * 100}%;
							top: {child.y0 * 100}%;
							width: {(child.x1 - child.x0) * 100}%;
							height: {(child.y1 - child.y0) * 100}%;
							box-sizing: border-box;
						"
						onmousedown={(e) => handleGrowthStart(e, child)}
						onmouseup={(e) => handleGrowthEnd(e)}
						ontouchstart={(e) => handleGrowthStart(e, child)}
						ontouchend={(e) => handleGrowthEnd(e)}
						oncontextmenu={(e) => e.preventDefault()}
					>
						<Child
							node={{
								id: child.data.id,
								name: child.data.nodeName || 'Unnamed',
								points: child.data.points,
								contributors: child.data.contributors || []
							}}
							dimensions={{
								x0: child.x0,
								y0: child.y0,
								x1: child.x1,
								y1: child.y1
							}}
							addContributor={handleAddContributor}
							removeContributor={handleRemoveContributor}
							onTextEdit={handleTextEdit}
							shouldEdit={globalState.nodeToEdit === child.data.id}
						/>
					</div>
				{/each}
			{:else}
				<div class="empty-state">
					<div class="add-node-prompt">
						<button class="add-node-button" onclick={handleAddNode}>
							<span class="add-icon">➕</span>
							<span class="add-text">{nodeLabels[labelIndex]}</span>
						</button>
					</div>
				</div>
			{/if}
		</div>
	</div>

	<!-- User dropdown for adding contributors -->
	{#if showUserDropdown}
		<DropDown
			position={dropdownPosition}
			show={showUserDropdown}
			title="Select Contributor"
			select={handleUserSelect}
			close={handleDropdownClose}
		/>
	{/if}
</div>

<style>
	.node-container {
		width: 100%;
		height: 100%;
		display: flex;
		flex-direction: column;
		position: relative;
	}

	.path-navigation {
		padding: 8px;
		display: flex;
		align-items: center;
		gap: 8px;
		border-bottom: 1px solid #eee;
	}

	.path {
		display: flex;
		align-items: center;
		flex-wrap: nowrap;
		overflow-x: auto;
		scrollbar-width: none; /* Firefox */
		-ms-overflow-style: none; /* IE and Edge */
		max-width: 100%;
		padding: 4px 0;
	}

	.path::-webkit-scrollbar {
		display: none; /* Chrome, Safari, Opera */
	}

	.path-item {
		white-space: nowrap;
		padding: 2px 4px;
		border-radius: 4px;
		cursor: pointer;
		transition: background-color 0.2s;
		color: #666;
		font-size: 14px;
	}

	.path-item:hover {
		background-color: rgba(0, 0, 0, 0.05);
		color: #333;
	}

	.path-item.current {
		font-weight: bold;
		color: #2196f3;
	}

	.path-separator {
		margin: 0 4px;
		color: #999;
	}

	.app-content {
		flex: 1;
		overflow: auto;
		position: relative;
	}

	.treemap-container {
		width: 100%;
		height: 100%;
		position: absolute;
		top: 0;
		left: 0;
	}

	:global(.clickable) {
		cursor: pointer;
		user-select: none;
		touch-action: none;
		transition:
			left 0.12s linear,
			top 0.12s linear,
			width 0.12s linear,
			height 0.12s linear;
	}

	:global(.clickable.deleting) {
		cursor: not-allowed;
		opacity: 0.7;
		transition: opacity 0.2s ease;
	}

	:global(.clickable.deleting:hover) {
		opacity: 1;
	}

	:global(.clickable.growing) {
		z-index: 10;
		box-shadow: 0 0 12px rgba(0, 100, 255, 0.5);
		border: 2px solid rgba(0, 100, 255, 0.7);
	}

	:global(.clickable.shrinking) {
		z-index: 10;
		box-shadow: 0 0 12px rgba(255, 60, 60, 0.5);
		border: 2px solid rgba(255, 60, 60, 0.7);
	}

	.empty-state {
		display: flex;
		justify-content: center;
		align-items: center;
		width: 100%;
		height: 100%;
		background-color: #f9f9f9;
		border-radius: 8px;
	}

	.add-node-prompt {
		display: flex;
		flex-direction: column;
		align-items: center;
		text-align: center;
		padding: 24px;
	}

	.add-node-button {
		display: flex;
		align-items: center;
		gap: 8px;
		padding: 12px 20px;
		border: none;
		border-radius: 8px;
		background-color: #2196f3;
		color: white;
		font-size: 16px;
		font-weight: 500;
		cursor: pointer;
		transition: all 0.2s ease;
		box-shadow: 0 2px 6px rgba(33, 150, 243, 0.3);
	}

	.add-node-button:hover {
		background-color: #1976d2;
		transform: translateY(-2px);
		box-shadow: 0 4px 8px rgba(33, 150, 243, 0.4);
		animation: none;
	}

	.add-icon {
		font-size: 20px;
	}

	.help-text {
		margin-top: 12px;
		color: #666;
		font-size: 14px;
	}
</style>
