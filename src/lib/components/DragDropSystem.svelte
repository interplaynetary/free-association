<script lang="ts">
	import { reorderNode, reorderNodeAtIndex, wouldCreateCycle } from '$lib/protocol';
	import type { Node } from '$lib/schema';
	
	// Props
	let {
		tree,
		onReorderComplete
	} = $props<{
		tree: Node;
		onReorderComplete?: () => void;
	}>();
	
	// Drag state
	let draggedNodeId = $state<string | null>(null);
	let draggedElement = $state<HTMLElement | null>(null);
	let ghostElement = $state<HTMLElement | null>(null);
	let dragOffset = $state({ x: 0, y: 0 });
	let isDragging = $state(false);
	
	// Drop zones
	let dropTargets = $state(new Set<string>());
	let activeDropTarget = $state<string | null>(null);
	
	// Enable drag on an element
	function enableDrag(element: HTMLElement, nodeId: string) {
		element.draggable = true;
		
		element.addEventListener('dragstart', (e) => handleDragStart(e, nodeId));
		element.addEventListener('dragend', handleDragEnd);
		
		// Add visual indicator that element is draggable
		element.style.cursor = 'grab';
		element.addEventListener('mousedown', () => {
			element.style.cursor = 'grabbing';
		});
		element.addEventListener('mouseup', () => {
			element.style.cursor = 'grab';
		});
	}
	
	// Enable drop on an element
	function enableDrop(element: HTMLElement, nodeId: string) {
		dropTargets.add(nodeId);
		
		element.addEventListener('dragover', (e) => handleDragOver(e, nodeId));
		element.addEventListener('drop', (e) => handleDrop(e, nodeId));
		element.addEventListener('dragenter', (e) => handleDragEnter(e, nodeId));
		element.addEventListener('dragleave', (e) => handleDragLeave(e, nodeId));
		
		// Add visual indicator for drop zones
		element.style.outline = 'none';
	}
	
	function handleDragStart(e: DragEvent, nodeId: string) {
		draggedNodeId = nodeId;
		draggedElement = e.target as HTMLElement;
		isDragging = true;
		
		// Store drag offset for ghost positioning
		const rect = draggedElement.getBoundingClientRect();
		dragOffset = {
			x: e.clientX - rect.left,
			y: e.clientY - rect.top
		};
		
		// Create ghost element
		createGhostElement();
		
		// Set drag data
		e.dataTransfer!.effectAllowed = 'move';
		e.dataTransfer!.setData('text/plain', nodeId);
		
		// Make original element semi-transparent
		draggedElement.style.opacity = '0.5';
		
		console.log('Drag started for node:', nodeId);
	}
	
	function handleDragEnd(e: DragEvent) {
		// Restore original element
		if (draggedElement) {
			draggedElement.style.opacity = '1';
			draggedElement.style.cursor = 'grab';
		}
		
		// Remove ghost element
		removeGhostElement();
		
		// Reset state
		draggedNodeId = null;
		draggedElement = null;
		activeDropTarget = null;
		isDragging = false;
		
		console.log('Drag ended');
	}
	
	function handleDragOver(e: DragEvent, targetNodeId: string) {
		// Prevent default to allow drop
		e.preventDefault();
		
		// Don't allow dropping on self or descendants
		if (draggedNodeId === targetNodeId || checkWouldCreateCycle(targetNodeId)) {
			e.dataTransfer!.dropEffect = 'none';
			return;
		}
		
		e.dataTransfer!.dropEffect = 'move';
		activeDropTarget = targetNodeId;
	}
	
	function handleDragEnter(e: DragEvent, targetNodeId: string) {
		e.preventDefault();
		
		if (draggedNodeId === targetNodeId || checkWouldCreateCycle(targetNodeId)) {
			return;
		}
		
		// Add drop zone visual feedback
		const element = e.target as HTMLElement;
		element.style.outline = '2px dashed #4CAF50';
		element.style.backgroundColor = 'rgba(76, 175, 80, 0.1)';
	}
	
	function handleDragLeave(e: DragEvent, targetNodeId: string) {
		// Remove drop zone visual feedback
		const element = e.target as HTMLElement;
		element.style.outline = 'none';
		element.style.backgroundColor = '';
	}
	
	function handleDrop(e: DragEvent, targetNodeId: string) {
		e.preventDefault();
		
		const sourceNodeId = e.dataTransfer!.getData('text/plain');
		
		// Validate drop
		if (!sourceNodeId || sourceNodeId === targetNodeId || checkWouldCreateCycle(targetNodeId)) {
			console.log('Invalid drop operation');
			return;
		}
		
		// Perform the reorder
		const success = reorderNode(tree, sourceNodeId, targetNodeId);
		
		if (success) {
			console.log(`Successfully moved node ${sourceNodeId} to ${targetNodeId}`);
			onReorderComplete?.();
		} else {
			console.error(`Failed to move node ${sourceNodeId} to ${targetNodeId}`);
		}
		
		// Remove drop zone visual feedback
		const element = e.target as HTMLElement;
		element.style.outline = 'none';
		element.style.backgroundColor = '';
	}
	
	function checkWouldCreateCycle(targetNodeId: string): boolean {
		if (!draggedNodeId || !tree) return false;
		
		return wouldCreateCycle(tree, draggedNodeId, targetNodeId);
	}
	
	function createGhostElement() {
		if (!draggedElement) return;
		
		ghostElement = draggedElement.cloneNode(true) as HTMLElement;
		ghostElement.style.position = 'fixed';
		ghostElement.style.pointerEvents = 'none';
		ghostElement.style.zIndex = '10000';
		ghostElement.style.opacity = '0.8';
		ghostElement.style.transform = 'scale(0.9)';
		ghostElement.style.border = '2px solid #2196F3';
		ghostElement.style.borderRadius = '8px';
		
		document.body.appendChild(ghostElement);
		
		// Position ghost element at cursor
		document.addEventListener('dragover', updateGhostPosition);
	}
	
	function removeGhostElement() {
		if (ghostElement) {
			document.body.removeChild(ghostElement);
			ghostElement = null;
		}
		document.removeEventListener('dragover', updateGhostPosition);
	}
	
	function updateGhostPosition(e: DragEvent) {
		if (!ghostElement) return;
		
		ghostElement.style.left = `${e.clientX - dragOffset.x}px`;
		ghostElement.style.top = `${e.clientY - dragOffset.y}px`;
	}
	
	// Public API for components to use
	export function makeDraggable(element: HTMLElement, nodeId: string) {
		enableDrag(element, nodeId);
	}
	
	export function makeDroppable(element: HTMLElement, nodeId: string) {
		enableDrop(element, nodeId);
	}
</script>

<!-- Drag feedback overlay -->
{#if isDragging}
	<div class="drag-overlay">
		<div class="drag-instructions">
			Drop on a node to move here
		</div>
	</div>
{/if}

<style>
	.drag-overlay {
		position: fixed;
		top: 0;
		left: 0;
		width: 100vw;
		height: 100vh;
		background: rgba(0, 0, 0, 0.1);
		z-index: 9999;
		pointer-events: none;
		display: flex;
		justify-content: center;
		align-items: flex-start;
		padding-top: 20px;
	}
	
	.drag-instructions {
		background: rgba(33, 150, 243, 0.9);
		color: white;
		padding: 8px 16px;
		border-radius: 20px;
		font-size: 14px;
		font-weight: 500;
		box-shadow: 0 4px 12px rgba(0, 0, 0, 0.3);
	}
</style>