<script lang="ts">
	import { onMount, onDestroy } from 'svelte';
	import { derived, writable, type Writable } from 'svelte/store';
	import * as d3 from 'd3';
	import { globalState } from '$lib/global.svelte';
	import Child from '$lib/components/Child.svelte';
	import DropDown from '$lib/components/DropDown.svelte';
	import { browser } from '$app/environment';

	// Import centralized system functions
	import {
		createRootNode,
		addChild,
		makePoints,
		enterChild,
		exitToParent,
		modifyNode,
		addToForest,
		getPoints,
		createExampleForest
	} from '$lib/centralized';

	// Import our types
	import type { TreeZipper, Node, Points, Forest } from '$lib/centralized/types';

	// Define a type for visualization data
	interface VisualizationNode {
		id: string;
		zipper: TreeZipper | null;
		points: number;
		children: VisualizationNode[];
		nodeName?: string;
		contributors?: string[];
	}

	// Current state of our navigation in the tree
	let currentZipper: TreeZipper | null = $state(null);
	let forest: Forest = $state(new Map());
	let path: string[] = $state([]);

	// Store for change tracking
	const zipperStore: Writable<TreeZipper | null> = writable(null);

	// Update store when currentZipper changes
	$effect(() => {
		if (currentZipper) {
			zipperStore.set(currentZipper);
		}
	});

	// Get children of current zipper
	let childrenIds = $state<string[]>([]);
	$effect(() => {
		if (!currentZipper) {
			childrenIds = [];
		} else {
			childrenIds = Array.from(currentZipper.zipperCurrent.nodeChildren.keys());
		}
	});

	// Get child zippers for all children
	let childZippers = $state<TreeZipper[]>([]);
	$effect(() => {
		if (!currentZipper) {
			childZippers = [];
		} else {
			childZippers = childrenIds
				.map((id: string) => {
					const child = enterChild(id, currentZipper!);
					return child;
				})
				.filter(Boolean) as TreeZipper[];
		}
	});

	// Format data for d3 to consume
	const packData = derived(zipperStore, ($zipper) => {
		if (!$zipper) return { id: '', selfPoints: 0, children: [] as VisualizationNode[] };

		const children = childZippers.map((child: TreeZipper) => ({
			id: child.zipperCurrent.nodeId,
			zipper: child,
			points: getPoints(child.zipperCurrent.nodePoints),
			nodeName: child.zipperCurrent.nodeName,
			contributors: Array.from(child.zipperCurrent.nodeContributors),
			children: [] as VisualizationNode[]
		}));

		return {
			id: $zipper.zipperCurrent.nodeId,
			selfPoints: getPoints($zipper.zipperCurrent.nodePoints),
			children
		};
	});

	// Create hierarchy for d3
	let hierarchyData: d3.HierarchyRectangularNode<VisualizationNode> | null = $state(null);
	// Track updates to trigger rerenders
	let updateCounter = $state(0);

	$effect(() => {
		const data = $packData;
		// Force rerender when updateCounter changes
		updateCounter;

		// Create hierarchy
		const rootNode: VisualizationNode = {
			id: data.id,
			zipper: currentZipper,
			points: data.selfPoints,
			children: data.children
		};

		const hierarchy = d3.hierarchy<VisualizationNode>(rootNode, (d) => d.children);

		// Sum for sizing
		hierarchy.sum((d) => d.points);

		// Apply treemap with relative sizing
		const childCount = data.children?.length || 0;
		const shouldUsePadding = childCount > 1;

		const treemap = d3
			.treemap<VisualizationNode>()
			.tile(d3.treemapSquarify)
			.size([1, 1])
			.padding(shouldUsePadding ? 0.005 : 0)
			.round(false);

		hierarchyData = treemap(hierarchy);
	});

	// UI interaction state
	let showUserDropdown = $state(false);
	let dropdownPosition = $state({ x: 0, y: 0 });
	let activeNodeId = $state<string | null>(null);
	let deleteMode = $state(false);

	// Derive deleteMode from globalState
	$effect(() => {
		deleteMode = globalState.deleteMode;
	});

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

	onMount(() => {
		// Initialize with example forest from our centralized system
		const { forest: initialForest } = createExampleForest();
		forest = new Map(initialForest);

		// Set initial zipper to the first node in the forest
		const entries = Array.from(forest.entries());
		if (entries.length > 0) {
			const [rootId, rootZipper] = entries[0];
			currentZipper = { ...rootZipper };
			path = [rootId];
		}

		// Set up event listeners
		document.addEventListener('mouseup', handleGlobalTouchEnd);
		document.addEventListener('touchend', handleGlobalTouchEnd);
		document.addEventListener('touchcancel', handleGlobalTouchEnd);
	});

	onDestroy(() => {
		// Clean up timers and event listeners
		if (growthInterval !== null) clearInterval(growthInterval);
		if (growthTimeout !== null) clearTimeout(growthTimeout);

		document.removeEventListener('mouseup', handleGlobalTouchEnd);
		document.removeEventListener('touchend', handleGlobalTouchEnd);
		document.removeEventListener('touchcancel', handleGlobalTouchEnd);
	});

	// Navigation functions
	function zoomInto(nodeId: string) {
		if (!currentZipper) return;

		const childZipper = enterChild(nodeId, currentZipper);
		if (childZipper) {
			// Update current zipper (immutably)
			currentZipper = { ...childZipper };

			// Update path
			path = [...path, nodeId];
		}
	}

	function zoomOut() {
		if (!currentZipper) return;

		const parentZipper = exitToParent(currentZipper);
		if (parentZipper) {
			// Update current zipper (immutably)
			currentZipper = { ...parentZipper };

			// Update path
			path = path.slice(0, -1);
		}
	}

	async function handleAddNode() {
		if (!currentZipper) return;

		// Calculate initial points for new node
		const calculateNewNodePoints = (): number => {
			// No siblings - set to 100 points
			if (!hierarchyData?.children || hierarchyData.children.length === 0) {
				return 100;
			}

			// Has siblings - use 10% of total points
			const currentLevelPoints = hierarchyData.children.reduce(
				(sum: number, node: any) => sum + (node.data?.points || 0),
				0
			);

			return Math.max(1, currentLevelPoints * 0.1);
		};

		const newPoints = calculateNewNodePoints();
		const newNodeId = `node-${Date.now()}`; // Unique ID

		try {
			// Add child to current zipper
			const updatedZipper = addChild(
				newNodeId,
				makePoints(newPoints),
				[], // No contributors initially
				null, // No manual fulfillment
				currentZipper
			);

			// Update our zipper immutably
			currentZipper = { ...updatedZipper };

			// Update forest with the root zipper
			if (path.length > 0) {
				const rootId = path[0];
				const rootZipper = forest.get(rootId);
				if (rootZipper) {
					forest = addToForest(forest, rootZipper);
				}
			}

			globalState.showToast('New node created', 'success');

			// Set node to edit mode
			setTimeout(() => {
				globalState.setNodeToEditMode(newNodeId);
			}, 50);
		} catch (err) {
			console.error('Error adding node:', err);
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
			if (!currentZipper) return;

			// Find the child zipper
			const childZipper = enterChild(nodeId, currentZipper);
			if (!childZipper) return;

			// Update node name
			const updatedChildZipper = modifyNode(
				(node) => ({
					...node,
					nodeName: newName
				}),
				childZipper
			);

			// Update parent with modified child
			const updatedChildren = new Map(currentZipper.zipperCurrent.nodeChildren);
			updatedChildren.set(nodeId, updatedChildZipper.zipperCurrent);

			// Update the current zipper
			const updatedZipper = modifyNode(
				(node) => ({
					...node,
					nodeChildren: updatedChildren
				}),
				currentZipper
			);

			// Apply updates immutably
			currentZipper = { ...updatedZipper };

			// Update forest
			if (path.length > 0) {
				const rootId = path[0];
				const rootZipper = forest.get(rootId);
				if (rootZipper) {
					forest = addToForest(forest, rootZipper);
				}
			}

			globalState.showToast(`Node renamed to "${newName}"`, 'success');
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
			if (!currentZipper) return;

			// Find the child zipper
			const childZipper = enterChild(nodeId, currentZipper);
			if (!childZipper) return;

			// Remove contributor
			const updatedContributors = new Set(childZipper.zipperCurrent.nodeContributors);
			updatedContributors.delete(contributorId);

			// Update node
			const updatedChildZipper = modifyNode(
				(node) => ({
					...node,
					nodeContributors: updatedContributors
				}),
				childZipper
			);

			// Update parent with modified child
			const updatedChildren = new Map(currentZipper.zipperCurrent.nodeChildren);
			updatedChildren.set(nodeId, updatedChildZipper.zipperCurrent);

			// Update current zipper
			const updatedZipper = modifyNode(
				(node) => ({
					...node,
					nodeChildren: updatedChildren
				}),
				currentZipper
			);

			// Apply updates immutably
			currentZipper = { ...updatedZipper };

			// Update forest
			if (path.length > 0) {
				const rootId = path[0];
				const rootZipper = forest.get(rootId);
				if (rootZipper) {
					forest = addToForest(forest, rootZipper);
				}
			}

			globalState.showToast('Contributor removed successfully', 'success');
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
			if (!currentZipper) return;

			// Find the child zipper
			const childZipper = enterChild(nodeId, currentZipper);
			if (!childZipper) return;

			// Add contributor
			const updatedContributors = new Set(childZipper.zipperCurrent.nodeContributors);
			updatedContributors.add(userId);

			// Update node
			const updatedChildZipper = modifyNode(
				(node) => ({
					...node,
					nodeContributors: updatedContributors
				}),
				childZipper
			);

			// Update parent with modified child
			const updatedChildren = new Map(currentZipper.zipperCurrent.nodeChildren);
			updatedChildren.set(nodeId, updatedChildZipper.zipperCurrent);

			// Update current zipper
			const updatedZipper = modifyNode(
				(node) => ({
					...node,
					nodeChildren: updatedChildren
				}),
				currentZipper
			);

			// Apply updates immutably
			currentZipper = { ...updatedZipper };

			// Update forest
			if (path.length > 0) {
				const rootId = path[0];
				const rootZipper = forest.get(rootId);
				if (rootZipper) {
					forest = addToForest(forest, rootZipper);
				}
			}

			globalState.showToast('Contributor added successfully', 'success');
		} catch (err) {
			console.error('Error adding contributor:', err);
			globalState.showToast('Error adding contributor', 'error');
		}
	}

	// Growth handlers
	function startGrowth(node: d3.HierarchyRectangularNode<VisualizationNode>, isShrinking = false) {
		// Don't allow growth in delete mode
		if (deleteMode) return;

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
			if (!currentZipper) return;

			// Find the child zipper
			const childZipper = enterChild(nodeId, currentZipper);
			if (!childZipper) return;

			// Update points
			const updatedChildZipper = modifyNode(
				(node) => ({
					...node,
					nodePoints: makePoints(points)
				}),
				childZipper
			);

			// Update parent with modified child
			const updatedChildren = new Map(currentZipper.zipperCurrent.nodeChildren);
			updatedChildren.set(nodeId, updatedChildZipper.zipperCurrent);

			// Update current zipper
			const updatedZipper = modifyNode(
				(node) => ({
					...node,
					nodeChildren: updatedChildren
				}),
				currentZipper
			);

			// Apply updates immutably
			currentZipper = { ...updatedZipper };

			// Update forest
			if (path.length > 0) {
				const rootId = path[0];
				const rootZipper = forest.get(rootId);
				if (rootZipper) {
					forest = addToForest(forest, rootZipper);
				}
			}

			console.log(`Saved points for node ${nodeId}: ${points}`);
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
		event.preventDefault();
		event.stopPropagation();

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
		event.preventDefault();

		// Check if this was a short tap (for navigation) or long press (for growth)
		const touchDuration = Date.now() - touchStartTime;
		const wasGrowthEvent = touchDuration >= TAP_THRESHOLD || isGrowing;

		// Get the nodeId before stopping growth
		const nodeId = activeGrowthNodeId;

		// Always stop growth
		stopGrowth();

		// For short taps, trigger navigation or deletion
		if (!wasGrowthEvent && nodeId) {
			if (deleteMode) {
				// Handle deletion
				if (confirm(`Delete this node?`)) {
					try {
						if (!currentZipper) return;

						// Create an updated children map without the deleted node
						const updatedChildren = new Map(currentZipper.zipperCurrent.nodeChildren);
						updatedChildren.delete(nodeId);

						// Update the current node
						const updatedZipper = modifyNode(
							(node) => ({
								...node,
								nodeChildren: updatedChildren
							}),
							currentZipper
						);

						// Update our zipper immutably
						currentZipper = { ...updatedZipper };

						// Update forest with the modified zipper
						if (path.length > 0) {
							const rootId = path[0];
							const rootZipper = forest.get(rootId);
							if (rootZipper) {
								forest = addToForest(forest, rootZipper);
							}
						}

						globalState.showToast('Node deleted successfully', 'success');
					} catch (err) {
						console.error(`Error deleting node ${nodeId}:`, err);
						globalState.showToast('Error deleting node', 'error');
					}
				}
			} else {
				// This was a tap, not a hold - navigate into the node
				zoomInto(nodeId);
			}
		}
	}
</script>

<div class="node-container">
	<!-- Path navigation -->
	<div class="path-navigation">
		<button onclick={() => zoomOut()} disabled={path.length <= 1}> ← Back </button>
		<span class="path">
			{path.join(' / ')}
		</span>
	</div>

	<!-- Main treemap content -->
	<div class="app-content">
		<div class="treemap-container">
			{#if hierarchyData && hierarchyData.children}
				{#each hierarchyData.children as child}
					<div
						class="clickable"
						class:deleting={deleteMode}
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
		font-size: 14px;
		color: #666;
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
</style>
