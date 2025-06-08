<script lang="ts">
	import { onMount, onDestroy } from 'svelte';
	import * as d3 from 'd3';
	import { username, userpub, userTree, createUsersDataProvider } from '$lib/state.svelte';
	import { currentPath, globalState } from '$lib/global.svelte';
	import { type Node, type NonRootNode, type RootNode } from '$lib/schema';
	import {
		findNodeById,
		getParentNode,
		updateNodeById,
		updatePoints,
		updateName,
		updateManualFulfillment,
		fulfilled,
		addChild,
		deleteSubtree,
		addContributors
	} from '$lib/protocol';
	import Child from '$lib/components/Child.svelte';
	import BottomSheet from '$lib/components/BottomSheet.svelte';
	import { browser } from '$app/environment';

	// Define a type for visualization data
	interface VisualizationNode {
		id: string;
		points: number;
		children: VisualizationNode[];
		nodeName?: string;
		contributors?: string[];
		fulfillment?: number;
	}

	// UI state
	let labelIndex = $state(0);
	const nodeLabels = ['Node', 'Value', 'Goal', 'Dependency', 'Desire', 'Contribution'];

	// For tracking changes in d3 visualization
	let updateCounter = $state(0);

	// Function to manually trigger a UI update
	function triggerUpdate() {
		updateCounter++;
		console.log('[UI FLOW] Triggered update', updateCounter);
	}

	// UI interaction state
	let showUserDropdown = $state(false);
	let dropdownPosition = $state({ x: 0, y: 0 });
	let activeNodeId = $state<string | null>(null);

	// Create users data provider for the dropdown
	let usersDataProvider = createUsersDataProvider([]);

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
	const BASE_GROWTH_RATE = 0.03;
	const BASE_SHRINK_RATE = -0.03;
	const TAP_THRESHOLD = 250;

	// Reactive store subscriptions
	const tree = $derived($userTree);
	const path = $derived($currentPath);
	const pub = $derived($userpub);

	// Helper function to get current node ID
	const currentNodeId = $derived.by(() => {
		// If the path is empty but we have a user pub, use that as the root
		if (path.length === 0 && pub) {
			return pub;
		}

		return path.length > 0 ? path[path.length - 1] : null;
	});

	// Get the current node
	const currentNode = $derived.by(() => {
		if (!tree && currentNodeId && pub && currentNodeId === pub) {
			// Initial load - wait for tree
			console.log('[UI FLOW] Waiting for tree to be loaded');
			return null;
		}

		if (!tree || !currentNodeId) return null;
		return findNodeById(tree, currentNodeId);
	});

	// Get child nodes
	const childNodes = $derived.by(() => {
		return currentNode ? currentNode.children : [];
	});

	// Format data for d3 to consume
	const packData = $derived.by(() => {
		if (!currentNode) {
			return { id: '', selfPoints: 0, children: [] };
		}

		const mappedChildren = childNodes.map((child) => ({
			id: child.id,
			points: child.type === 'NonRootNode' ? (child as NonRootNode).points : 0,
			nodeName: child.name,
			contributors: child.type === 'NonRootNode' ? (child as NonRootNode).contributor_ids : [],
			children: [] as VisualizationNode[]
		}));

		return {
			id: currentNode.id,
			selfPoints: currentNode.type === 'NonRootNode' ? (currentNode as NonRootNode).points : 0,
			children: mappedChildren
		};
	});

	// Create hierarchy for d3
	const hierarchyData = $derived.by(() => {
		// Force rerender when updateCounter changes
		updateCounter;

		const data = packData;

		// Create hierarchy
		const rootNode: VisualizationNode = {
			id: data.id,
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

		return treemap(hierarchy);
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

		// Setup a subscription to the userTree store to trigger updates
		const unsubscribe = userTree.subscribe((value) => {
			if (value) {
				console.log('[UI FLOW] userTree updated, triggering UI update');
				triggerUpdate();
			}
		});

		return () => {
			// Clean up timers and event listeners
			if (growthInterval !== null) clearInterval(growthInterval);
			if (growthTimeout !== null) clearTimeout(growthTimeout);
			clearInterval(interval);
			document.removeEventListener('mouseup', handleGlobalTouchEnd);
			document.removeEventListener('touchend', handleGlobalTouchEnd);
			document.removeEventListener('touchcancel', handleGlobalTouchEnd);
			unsubscribe();
		};
	});

	// Navigation functions
	function zoomInto(nodeId: string) {
		console.log('[UI FLOW] zoomInto called for node:', nodeId);
		globalState.zoomInto(nodeId);
		console.log('[UI FLOW] Navigation handled');
	}

	async function handleAddNode() {
		console.log('[UI FLOW] handleAddNode started');
		if (!currentNodeId) {
			console.log('[UI FLOW] No currentNodeId, aborting');
			return;
		}

		// Calculate initial points for new node
		const calculateNewNodePoints = (): number => {
			// No siblings - set to 100 points
			if (!hierarchyData?.children || hierarchyData.children.length === 0) {
				console.log('[UI FLOW] No siblings, setting new node to 100 points');
				return 100;
			}

			// Has siblings - use 20% of total points
			const currentLevelPoints = hierarchyData.children.reduce(
				(sum: number, node: any) => sum + (node.data?.points || 0),
				0
			);
			const points = Math.max(1, currentLevelPoints * 0.2);
			console.log('[UI FLOW] Calculated points based on siblings:', points);
			return points;
		};

		const newPoints = calculateNewNodePoints();
		const newNodeId = `node_${Date.now()}_${Math.random().toString(36).substring(2, 9)}`;
		const newNodeName = 'New Node'; // Default name for better UX

		console.log('[UI FLOW] Creating new node:', {
			id: newNodeId,
			points: newPoints,
			name: newNodeName
		});
		console.log('[UI FLOW] Current parent:', currentNodeId);
		console.log('[UI FLOW] Current path:', path);

		try {
			// Get the tree and current node
			if (!tree) {
				globalState.showToast('Error creating node: No tree found', 'error');
				return;
			}

			// Find the current node
			if (!currentNode) {
				globalState.showToast('Error creating node: Current node not found', 'error');
				return;
			}

			// Create a deep clone of the current tree to ensure reactivity
			const updatedTree = structuredClone(tree);

			// Find the current node in our cloned tree
			const updatedCurrentNode = findNodeById(updatedTree, currentNodeId);
			if (!updatedCurrentNode) {
				globalState.showToast(
					'Error creating node: Current node not found in updated tree',
					'error'
				);
				return;
			}

			// Add child to current node in our cloned tree
			addChild(updatedCurrentNode, newNodeId, newNodeName, newPoints);

			// Update the store with the new tree to trigger reactivity
			userTree.set(updatedTree);

			// Force update counter to trigger redraw
			triggerUpdate();

			console.log('[UI FLOW] addNode successful');
			globalState.showToast('New node created', 'success');

			// Set node to edit mode
			console.log('[UI FLOW] Setting node to edit mode:', newNodeId);
			setTimeout(() => {
				globalState.nodeToEdit = newNodeId;
				console.log('[UI FLOW] Node edit mode set');
				console.log('userTree Parent.275', $userTree);
			}, 50);
		} catch (err) {
			console.error('[UI FLOW] Error in handleAddNode:', err);
			globalState.showToast('Error creating node', 'error');
		}
	}

	// Text editing handler
	function handleTextEdit(detail: { nodeId: string; newName: string }) {
		const { nodeId, newName } = detail;

		try {
			if (!tree) return;

			// Create a clone of the tree to ensure reactivity
			const updatedTree = structuredClone(tree);

			// Update the node name in memory
			updateNodeById(updatedTree, nodeId, (node) => {
				updateName(node, newName);
			});

			// Update store to trigger reactivity
			userTree.set(updatedTree);

			// Force update
			triggerUpdate();

			// Clear edit mode
			globalState.nodeToEdit = '';

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
			if (!tree) return;

			// Create a clone of the tree to ensure reactivity
			const updatedTree = structuredClone(tree);

			// Find the node to update
			const node = findNodeById(updatedTree, nodeId);
			if (!node || node.type !== 'NonRootNode') return;

			// Get current contributor IDs and filter out the one to remove
			const updatedContributors = (node as NonRootNode).contributor_ids.filter(
				(id: string) => id !== contributorId
			);

			// Update the node's contributors
			(node as NonRootNode).contributor_ids = updatedContributors;

			// Update the store to trigger reactivity
			userTree.set(updatedTree);

			// Force update
			triggerUpdate();

			globalState.showToast('Contributor removed successfully', 'success');
		} catch (err) {
			console.error('Error removing contributor:', err);
			globalState.showToast('Error removing contributor', 'error');
		}
	}

	function handleUserSelect(detail: { id: string; name: string; metadata?: any }) {
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
			if (!tree) return;

			// Create a clone of the tree to ensure reactivity
			const updatedTree = structuredClone(tree);

			// Find the node to update
			const node = findNodeById(updatedTree, nodeId);
			if (!node || node.type !== 'NonRootNode') return;

			// Check if contributor already exists
			const hasContributor = (node as NonRootNode).contributor_ids.includes(userId);

			if (!hasContributor) {
				// Get current contributors and add the new one
				const currentContributors = [...(node as NonRootNode).contributor_ids];
				currentContributors.push(userId);

				// Use the protocol function to properly add contributors AND clear children
				// This ensures the node becomes a proper leaf contribution node
				addContributors(node, currentContributors);

				// Update the store to trigger reactivity
				userTree.set(updatedTree);

				// Force update
				triggerUpdate();

				globalState.showToast('Contributor added successfully', 'success');
			}
		} catch (err) {
			console.error('Error adding contributor:', err);
			globalState.showToast('Error adding contributor', 'error');
		}
	}

	// Calculate node fulfillment directly using protocol
	function getNodeFulfillment(nodeId: string): number {
		if (!tree) return 0;

		const node = findNodeById(tree, nodeId);
		if (!node) return 0;

		return fulfilled(node, tree);
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
					const currentPoints = node.data.points;
					if (isShrinking) {
						// For consistent relative change, use current points * constant rate
						// This means it takes the same time to halve in size regardless of starting size
						rate = currentPoints * BASE_SHRINK_RATE;
					} else {
						// For consistent relative change, use current points * constant rate
						// This means it takes the same time to double in size regardless of starting size
						rate = currentPoints * BASE_GROWTH_RATE;
					}

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
			if (!tree) return;

			// Create a clone of the tree to ensure reactivity
			const updatedTree = structuredClone(tree);

			// Find the node and directly update it
			const node = findNodeById(updatedTree, nodeId);
			if (!node || node.type !== 'NonRootNode') return;

			// Update points directly using protocol
			updatePoints(node as NonRootNode, points);

			// Update the store to trigger reactivity
			userTree.set(updatedTree);

			// Force update
			triggerUpdate();
		} catch (err) {
			console.error(`Error saving points for node ${nodeId}:`, err);
			globalState.showToast('Error saving node points', 'error');
		}
	}

	function handleGlobalTouchEnd() {
		if (isTouching) {
			stopGrowth();
			resetInteractionState();
		}
	}

	// Track active interaction to prevent duplicate events
	let activePointerId: number | null = $state(null);
	let interactionHandled = $state(false);
	let isMultiTouch = $state(false);

	// Handle touch start to detect multi-touch gestures
	function handleTouchStart(
		event: TouchEvent,
		node: d3.HierarchyRectangularNode<VisualizationNode>
	) {
		// Detect multi-touch for shrinking
		isMultiTouch = event.touches.length === 2;
		console.log(
			'[DEBUG] TouchStart - touches:',
			event.touches.length,
			'isMultiTouch:',
			isMultiTouch
		);

		// Let the pointer event handle the main logic
		// This is just for multi-touch detection
	}

	// Handle touch end to reset multi-touch state
	function handleTouchEnd(event: TouchEvent) {
		// Reset multi-touch state when fingers are lifted
		if (event.touches.length < 2) {
			isMultiTouch = false;
		}
	}

	function handleGrowthStart(
		event: PointerEvent,
		node: d3.HierarchyRectangularNode<VisualizationNode>
	) {
		console.log(
			'[DEBUG] PointerDown - isPrimary:',
			event.isPrimary,
			'button:',
			event.button,
			'isMultiTouch:',
			isMultiTouch
		);

		// Only handle primary pointer to prevent duplicates
		if (!event.isPrimary) return;

		// Prevent duplicate handling for the same interaction
		if (interactionHandled) return;

		// Set interaction tracking
		activePointerId = event.pointerId;
		interactionHandled = true;

		// Set touch state
		isTouching = true;
		touchStartTime = Date.now();
		activeGrowthNodeId = node.data.id;

		// For touch events, delay processing to allow multi-touch detection
		if (event.pointerType === 'touch') {
			// Small delay to let touch events fire and detect multi-touch
			setTimeout(() => {
				if (!interactionHandled) return; // Check if interaction was cancelled

				// Determine if shrinking (right-click or multi-touch)
				const isShrinking = event.button === 2 || isMultiTouch;
				console.log(
					'[DEBUG] Delayed processing - isShrinking:',
					isShrinking,
					'button:',
					event.button,
					'isMultiTouch:',
					isMultiTouch
				);

				// Start growth with delay
				startGrowth(node, isShrinking);
			}, 50); // 50ms delay to allow touch events to fire
		} else {
			// For mouse events, process immediately
			const isShrinking = event.button === 2 || isMultiTouch;
			console.log(
				'[DEBUG] Immediate processing - isShrinking:',
				isShrinking,
				'button:',
				event.button,
				'isMultiTouch:',
				isMultiTouch
			);

			// Start growth with delay
			startGrowth(node, isShrinking);
		}
	}

	function handleGrowthEnd(event: PointerEvent) {
		// Only handle if this is our active pointer
		if (!event.isPrimary || event.pointerId !== activePointerId) return;

		// Check if the click target is a text edit field, node-text element, or contributor element
		const target = event.target as HTMLElement;
		if (
			target &&
			(target.closest('.node-text') ||
				target.closest('.node-text-edit-container') ||
				target.closest('.add-contributor-button') ||
				target.closest('.tag-container') ||
				target.closest('.contributor-container') ||
				target.closest('.node-title') ||
				target.closest('.node-title-area') ||
				target.closest('.title-segment') ||
				target.closest('.node-edit-input') ||
				target.closest('.contributors-area'))
		) {
			// Click originated from text or contributor elements, don't navigate
			resetInteractionState();
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

		// Reset interaction state
		resetInteractionState();
	}

	// Helper function to reset interaction tracking
	function resetInteractionState() {
		activePointerId = null;
		interactionHandled = false;
		isMultiTouch = false;
	}

	// Reset interaction state when navigation occurs
	$effect(() => {
		// Watch for path changes to reset interaction state
		path;
		resetInteractionState();
	});

	function handleNodeDeletion(nodeId: string) {
		if (confirm(`Delete this node?`)) {
			try {
				if (!tree) return;

				// Create a clone of the tree to ensure reactivity
				const updatedTree = structuredClone(tree);

				// Find the parent node first
				const parentNode = getParentNode(updatedTree, nodeId);
				if (!parentNode) {
					console.error('Cannot find parent node');
					return;
				}

				// Don't allow deleting root node
				if (nodeId === path[0]) {
					console.error('Cannot delete root node');
					globalState.showToast('Cannot delete root node', 'error');
					return;
				}

				// Check if we're trying to delete the current node
				const isCurrentNode = currentNodeId === nodeId;
				if (isCurrentNode) {
					// If we're on the node to delete, navigate up to parent first
					if (path.length <= 1) {
						console.error('Cannot delete current node with no parent');
						return;
					}

					// Navigate to parent
					globalState.zoomOut();
				}

				// Find the node to delete
				const nodeToDelete = findNodeById(updatedTree, nodeId);
				if (!nodeToDelete) {
					globalState.showToast('Node not found', 'error');
					return;
				}

				// Delete the node from its parent
				const childIndex = parentNode.children.findIndex((child) => child.id === nodeId);
				if (childIndex !== -1) {
					parentNode.children.splice(childIndex, 1);
				}

				// Update the store
				userTree.set(updatedTree);

				// Force update
				triggerUpdate();

				globalState.showToast('Node deleted successfully', 'success');
			} catch (err) {
				console.error(`Error in deletion process: ${err}`);
				globalState.showToast('Error deleting node', 'error');
			}
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
						onpointerdown={(e) => handleGrowthStart(e, child)}
						onpointerup={(e) => handleGrowthEnd(e)}
						ontouchstart={(e) => handleTouchStart(e, child)}
						ontouchend={(e) => handleTouchEnd(e)}
						oncontextmenu={(e) => e.preventDefault()}
					>
						<Child
							node={{
								id: child.data.id,
								name: child.data.nodeName || 'Unnamed',
								points: child.data.points,
								contributors: child.data.contributors || [],
								fulfillment: getNodeFulfillment(child.data.id)
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
		<BottomSheet
			show={showUserDropdown}
			title="Select Contributor"
			searchPlaceholder="Search users..."
			dataProvider={usersDataProvider}
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
