<script lang="ts">
	import {
		getColorForNameHash,
		getColorForUserId,
		getDarkerColorForNameHash
	} from '$lib/utils/colorUtils';
	import { getUserName } from '$lib/state/users.svelte';
	import { globalState } from '$lib/global.svelte';
	import { pie, arc } from 'd3-shape';

	// Define interface for node data
	interface NodeData {
		id: string;
		name: string;
		points: number;
		contributors: string[];
		antiContributors?: string[];
		fulfillment?: number;
		manualFulfillment?: number; // Add manual fulfillment to interface
		hasChildren?: boolean; // Flag to indicate if this node has children
	}

	// Define interface for node dimensions
	interface Dimensions {
		x0: number;
		y0: number;
		x1: number;
		y1: number;
	}

	// Use $props to get the node data and callbacks
	let {
		node,
		dimensions,
		addContributor = (detail: { nodeId: string; clientX: number; clientY: number }) => {},
		addAntiContributor = (detail: { nodeId: string; clientX: number; clientY: number }) => {},
		removeContributor = (detail: { nodeId: string; contributorId: string }) => {},
		onTextEdit = (detail: { nodeId: string; newName: string }) => {},
		onManualFulfillmentChange = (detail: {
			nodeId: string;
			value: number;
			showNotification?: boolean;
		}) => {} // Add callback for manual fulfillment
	} = $props<{
		node: NodeData;
		dimensions: Dimensions;
		addContributor?: (detail: { nodeId: string; clientX: number; clientY: number }) => void;
		addAntiContributor?: (detail: { nodeId: string; clientX: number; clientY: number }) => void;
		removeContributor?: (detail: { nodeId: string; contributorId: string }) => void;
		onTextEdit?: (detail: { nodeId: string; newName: string }) => void;
		onManualFulfillmentChange?: (detail: {
			nodeId: string;
			value: number;
			showNotification?: boolean;
		}) => void; // Add callback type
	}>();

	// shouldEdit prop is no longer used for auto-edit functionality

	// Editing state - now synced with global state
	let isEditing = $state(false);
	let editValue = $state('');
	let editInput: HTMLInputElement | null = $state(null);

	// Sync local editing state with global state
	$effect(() => {
		const isThisNodeBeingEdited = globalState.editMode && globalState.editingNodeId === node.id;
		if (isThisNodeBeingEdited !== isEditing) {
			isEditing = isThisNodeBeingEdited;
			if (isEditing) {
				editValue = node.name || '';
			}
		}
	});

	// Handle auto-edit signal from globalState.nodeToEdit
	$effect(() => {
		if (globalState.nodeToEdit === node.id) {
			console.log('[DEBUG CHILD] Auto-edit signal received for node:', node.id);
			// Try to enter edit mode (this will also enable textEditMode if needed)
			const canEdit = globalState.enterEditMode(node.id);
			if (canEdit) {
				console.log('[DEBUG CHILD] Auto-edit successful for node:', node.name);
			} else {
				console.log('[DEBUG CHILD] Auto-edit failed for node:', node.name);
			}
		}
	});

	// Calculate relative size for text scaling
	const nodeWidth = $derived(dimensions.x1 - dimensions.x0);
	const nodeHeight = $derived(dimensions.y1 - dimensions.y0);
	const nodeSizeRatio = $derived(Math.min(nodeWidth, nodeHeight) * 100); // Size as percentage of parent

	// Prepare text segments for multi-line display using natural word breaks
	const segments = $derived(
		(() => {
			if (node.name === 'Unnamed') return [node.name];

			// First try natural word breaks (spaces)
			const words = node.name.split(/\s+/);
			if (words.length > 1) {
				return words;
			}

			// For single words, only split on capital letters if the word is very long
			if (node.name.length > 12) {
				return node.name.split(/(?=[A-Z][^A-Z])/g);
			}

			// Keep short single words intact
			return [node.name];
		})()
	);

	// Clean Svelte 5 reactive font sizing - no DOM manipulation needed!
	const fontSize = $derived(() => {
		// Calculate available space based on container dimensions
		const containerWidth = nodeWidth * 400; // Convert to pixels (assuming 400px base)
		const containerHeight = nodeHeight * 400;

		// Account for UI elements in contribution nodes
		const maxWidth = containerWidth * 0.85; // Breathing room

		let maxHeight;
		if (node.hasChildren) {
			// Parent nodes can use generous space (no buttons)
			maxHeight = containerHeight * 0.8;
		} else {
			// Leaf nodes must be conservative (buttons at 80%)
			maxHeight = containerHeight * 0.5;
		}

		// Detect content type
		const allText = segments.join('');
		const isEmojiOnly = /^[\p{Emoji}\s]*$/u.test(allText.trim());
		const isShortText = allText.length <= 3;
		const isSingleSegment = segments.length === 1;

		let calculatedSize;
		if (isEmojiOnly || isShortText) {
			// Emojis and short text: use container-based sizing
			calculatedSize = Math.min(maxWidth * 0.8, maxHeight * 0.8);
		} else if (isSingleSegment && allText.length <= 8) {
			// Short single words
			calculatedSize = Math.min(maxWidth * 0.6, maxHeight * 0.6);
		} else {
			// Multi-line text: calculate based on content
			const longestLineLength = Math.max(...segments.map((s: string) => s.length));
			const widthBasedSize = maxWidth / (longestLineLength * 0.6);
			const heightBasedSize = maxHeight / (segments.length * 1.4);
			calculatedSize = Math.min(widthBasedSize, heightBasedSize);
		}

		// Clamp to reasonable bounds
		return Math.max(4, Math.min(300, calculatedSize));
	});

	// Determine if this is the only child (occupies 100% of the space)
	const isOnlyChild = $derived(nodeWidth >= 0.999 && nodeHeight >= 0.999);

	// Unified spacing calculations
	const adaptivePadding = $derived(Math.max(1, Math.min(8, nodeSizeRatio * 0.1)));

	// Button scaling
	const contributorSize = $derived(Math.max(12, Math.min(40, nodeSizeRatio * 0.5)));

	// Calculate visibility factor (0-1) for smooth fade-in
	const visibilityFactor = $derived(Math.min(1, Math.max(0, (nodeSizeRatio - 5) / 7)));

	// === SIMPLIFIED ELEMENT SIZING ===
	// Use container-relative sizing for all elements

	// Simple element sizing based on node scale
	const elementScale = $derived(Math.min(nodeWidth, nodeHeight));
	const sliderHeight = $derived(Math.max(0.05, Math.min(0.15, elementScale * 0.12))); // 12% of node scale
	const buttonHeight = $derived(Math.max(0.08, Math.min(0.22, elementScale * 0.18))); // 18% of node scale

	// Use fixed proportional widths instead of text-based calculations
	const sliderWidth = $derived(Math.max(0.4, Math.min(0.8, elementScale * 0.7))); // 70% of container scale
	const buttonContainerWidth = $derived(Math.max(0.85, Math.min(0.95, nodeWidth * 0.9))); // 90% of node width, capped at 95%

	// CSS-style positioning: Give title more breathing room
	// Title is at 50%, so position elements further away for better spacing
	const sliderTopPercent = 20; // Upper fifth (more space from title)
	const buttonsTopPercent = 80; // Lower fifth (more space from title)

	// Convert to percentages for CSS
	const sliderHeightPercent = $derived(sliderHeight * 100);
	const sliderWidthPercent = $derived(sliderWidth * 100);
	const buttonHeightPercent = $derived(buttonHeight * 100);
	const buttonContainerWidthPercent = $derived(buttonContainerWidth * 100);

	// Legacy compatibility
	const buttonSizePercent = $derived(buttonHeightPercent);

	// Simplified percentage indicator font size
	const percentageIndicatorFontSize = $derived(() => {
		// Scale with slider height for proportional look
		return Math.max(0.3, Math.min(0.7, sliderHeight * 0.5));
	});

	// Check if node has contributors or anti-contributors for styling
	const hasContributors = $derived(node.contributors.length > 0);
	const hasAntiContributors = $derived((node.antiContributors || []).length > 0);
	const hasAnyContributors = $derived(hasContributors || hasAntiContributors);

	// Create pie chart data for contributors
	const pieData = $derived(() => {
		if (!hasContributors) return [];

		// Simple data array - each contributor gets value 1 (equal segments)
		const data = node.contributors.map((id: string) => ({ id, value: 1 }));

		// Create pie generator following D3 docs exactly
		const pieGenerator = pie<any>()
			.value((d: any) => d.value)
			.sort(null);

		return pieGenerator(data);
	});

	// Create pie chart data for anti-contributors
	const antiPieData = $derived(() => {
		if (!hasAntiContributors) return [];

		// Simple data array - each anti-contributor gets value 1 (equal segments)
		const data = (node.antiContributors || []).map((id: string) => ({ id, value: 1 }));

		// Create pie generator following D3 docs exactly
		const pieGenerator = pie<any>()
			.value((d: any) => d.value)
			.sort(null);

		return pieGenerator(data);
	});

	// Simple arc generator
	const arcPath = $derived(() => {
		const radius = 20; // Fixed radius for simplicity
		return arc<any>().innerRadius(0).outerRadius(radius);
	});

	// Load contributor names
	let contributorNames = $state<Record<string, string>>({});

	$effect(() => {
		// Load names for all contributors
		node.contributors.forEach(async (contributorId: string) => {
			if (!contributorNames[contributorId]) {
				try {
					const name = await getUserName(contributorId);
					contributorNames = {
						...contributorNames,
						[contributorId]: name
					};
				} catch (error) {
					console.error('Failed to load contributor name:', error);
				}
			}
		});

		// Load names for all anti-contributors
		(node.antiContributors || []).forEach(async (contributorId: string) => {
			if (!contributorNames[contributorId]) {
				try {
					const name = await getUserName(contributorId);
					contributorNames = {
						...contributorNames,
						[contributorId]: name
					};
				} catch (error) {
					console.error('Failed to load anti-contributor name:', error);
				}
			}
		});
	});

	// State for tracking hovered segment
	let hoveredSegment = $state<string | null>(null);

	// Note: Fulfillment is now controlled by the full-width slider

	// Get current slider value from node data (including ephemeral values during drag)
	const currentSliderValue = $derived(() => {
		// Access counter to ensure reactivity when ephemeral values change
		globalState.ephemeralFulfillmentCounter;

		// Use ephemeral value during drag for slider position
		const ephemeralValue = globalState.ephemeralFulfillmentValues.get(node.id);
		if (ephemeralValue !== undefined) {
			return Math.round(ephemeralValue);
		}

		// Fall back to actual values
		return node.manualFulfillment !== undefined ? Math.round(node.manualFulfillment * 100) : 100;
	});

	// Calculate fulfillment percentage for the visual rectangle
	const fulfillmentPercentage = $derived(() => {
		if (node.hasChildren) {
			// Parent nodes: Show calculated fulfillment only (no manual override)
			const result = Math.max(0, Math.min(100, (node.fulfillment || 0) * 100));
			console.log(`[FULFILLMENT-RECT] Parent node ${node.id}: ${result}%`);
			return result;
		} else {
			// Child nodes with slider: Track slider position in real-time (including during drag)
			// Access counter to ensure reactivity when ephemeral values change
			const counter = globalState.ephemeralFulfillmentCounter;

			// Check for ephemeral value first (during slider drag)
			const ephemeralValue = globalState.ephemeralFulfillmentValues.get(node.id);
			if (ephemeralValue !== undefined) {
				console.log(
					`[FULFILLMENT-RECT] Child node ${node.id}: Using ephemeral ${ephemeralValue}% (counter: ${counter})`
				);
				return ephemeralValue;
			}

			// Otherwise use manual fulfillment or fall back to calculated fulfillment
			let result;
			if (node.manualFulfillment !== undefined) {
				result = Math.max(0, Math.min(100, node.manualFulfillment * 100));
				console.log(
					`[FULFILLMENT-RECT] Child node ${node.id}: Using manual ${result}% (counter: ${counter})`
				);
			} else {
				result = Math.max(0, Math.min(100, (node.fulfillment || 0) * 100));
				console.log(
					`[FULFILLMENT-RECT] Child node ${node.id}: Using calculated ${result}% (counter: ${counter})`
				);
			}
			return result;
		}
	});

	// Function to handle add contributor button click (mouse or touch)
	function handleAddContributorClick(event: MouseEvent | TouchEvent) {
		// In recompose mode, don't handle contributor actions - let event bubble up
		if (globalState.recomposeMode) {
			return; // Let parent handle recompose
		}

		// In delete mode, buttons are hidden so this shouldn't be called

		// Don't allow adding contributors when editing
		if (globalState.editMode) {
			globalState.showToast('Cannot add contributors while editing', 'warning');
			return;
		}

		event.preventDefault();
		event.stopPropagation();
		event.stopImmediatePropagation();

		const nodeId = node.id;
		if (nodeId) {
			// Get coordinates from either mouse or touch event
			let clientX: number, clientY: number;

			if (event.type === 'touchstart' && 'touches' in event) {
				// Touch event - get coordinates from the first touch point
				const touch = event.touches[0];
				clientX = touch.clientX;
				clientY = touch.clientY;
			} else {
				// Mouse event - get coordinates directly
				clientX = (event as MouseEvent).clientX;
				clientY = (event as MouseEvent).clientY;
			}

			addContributor({
				nodeId,
				clientX,
				clientY
			});
		}
	}

	// Function to handle add anti-contributor button click (mouse or touch)
	function handleAddAntiContributorClick(event: MouseEvent | TouchEvent) {
		// In recompose mode, don't handle anti-contributor actions - let event bubble up
		if (globalState.recomposeMode) {
			return; // Let parent handle recompose
		}

		// In delete mode, buttons are hidden so this shouldn't be called

		// Don't allow adding anti-contributors when editing
		if (globalState.editMode) {
			globalState.showToast('Cannot add anti-contributors while editing', 'warning');
			return;
		}

		event.preventDefault();
		event.stopPropagation();
		event.stopImmediatePropagation();

		const nodeId = node.id;
		if (nodeId) {
			// Get coordinates from either mouse or touch event
			let clientX: number, clientY: number;

			if (event.type === 'touchstart' && 'touches' in event) {
				// Touch event - get coordinates from the first touch point
				const touch = event.touches[0];
				clientX = touch.clientX;
				clientY = touch.clientY;
			} else {
				// Mouse event - get coordinates directly
				clientX = (event as MouseEvent).clientX;
				clientY = (event as MouseEvent).clientY;
			}

			addAntiContributor({
				nodeId,
				clientX,
				clientY
			});
		}
	}

	// Track if this was triggered by user interaction (important for iOS)
	let userTriggeredEdit = $state(false);

	// Track if we're actually dragging the slider thumb (not just clicking the track)
	let isDraggingSliderThumb = $state(false);

	// Unified text edit handler that works across all devices and interaction types
	function handleTextEditActivation(event: Event) {
		console.log('[DEBUG CHILD] Text edit activation:', {
			type: event.type,
			target: event.target,
			nodeId: node.id,
			nodeName: node.name,
			editMode: globalState.editMode,
			deleteMode: globalState.deleteMode,
			recomposeMode: globalState.recomposeMode,
			textEditMode: globalState.textEditMode,
			isTrusted: event.isTrusted
		});

		// In delete or recompose mode, don't handle text editing - let the event bubble up to parent
		if (globalState.deleteMode || globalState.recomposeMode) {
			console.log('[DEBUG CHILD] In delete/recompose mode - allowing event to bubble to parent');
			// Don't call preventDefault or stopPropagation - let the event bubble up naturally
			return;
		}

		// If text edit mode is disabled, don't handle text editing - let the event bubble up to parent
		if (!globalState.textEditMode) {
			console.log('[DEBUG CHILD] Text edit mode disabled - allowing event to bubble to parent');
			// Don't call preventDefault or stopPropagation - let the event bubble up naturally
			return;
		}

		// Mark as user-triggered for iOS compatibility
		userTriggeredEdit = event.isTrusted;

		// Stop propagation to prevent parent node interactions
		event.stopPropagation();

		// For touch events, also prevent default to avoid iOS text selection conflicts
		if (event.type === 'touchend') {
			event.preventDefault();
		}

		// Only allow editing if we have a valid node ID
		if (!node.id) {
			console.log('[DEBUG CHILD] No node ID, cannot edit');
			return;
		}

		// Try to enter edit mode through global state
		const canEdit = globalState.enterEditMode(node.id);
		if (!canEdit) {
			console.log('[DEBUG CHILD] Cannot edit - global state prevented, reason:', {
				editMode: globalState.editMode,
				deleteMode: globalState.deleteMode,
				recomposeMode: globalState.recomposeMode
			});
			return;
		}

		console.log(
			'[DEBUG CHILD] Edit mode activated successfully for node:',
			node.name,
			'userTriggered:',
			userTriggeredEdit
		);
	}

	// Handle text edit save
	function saveTextEdit(newName: string) {
		const nodeId = node.id;
		if (!nodeId) return;

		onTextEdit({ nodeId, newName });
	}

	// Handle keyboard events for the input field
	function handleEditKeydown(event: KeyboardEvent) {
		if (event.key === 'Enter') {
			event.preventDefault();
			finishEditing();
		} else if (event.key === 'Escape') {
			event.preventDefault();
			finishEditing();
		}
	}

	// Finish editing and save the result
	function finishEditing() {
		const newName = editValue.trim();
		if (newName && newName !== node.name) {
			saveTextEdit(newName);
		}

		// Exit edit mode through global state
		globalState.exitEditMode();
	}

	// Handle click/touch outside to finish editing
	function handleOutsideInteraction(event: MouseEvent | TouchEvent) {
		// Only if we're editing this specific node
		if (isEditing && globalState.editingNodeId === node.id) {
			// Check if the interaction is outside the input field
			if (editInput && !editInput.contains(event.target as Node)) {
				// For mouse events, check for scrollbar clicks
				if (event.type === 'mousedown') {
					const mouseEvent = event as MouseEvent;
					const clickedElement = mouseEvent.target as Element;
					const isScrollbarClick =
						mouseEvent.offsetX > clickedElement.clientWidth ||
						mouseEvent.offsetY > clickedElement.clientHeight;

					if (isScrollbarClick) {
						return;
					}
				}

				finishEditing();
			}
		}
	}

	// Global event handlers for slider dragging state
	function handleGlobalMouseUp() {
		if (isDraggingSliderThumb) {
			console.log('[SLIDER] Global mouse up - resetting drag state');
			isDraggingSliderThumb = false;
		}
	}

	function handleGlobalTouchEnd() {
		if (isDraggingSliderThumb) {
			console.log('[SLIDER] Global touch end - resetting drag state');
			isDraggingSliderThumb = false;
		}
	}

	// Set up and clean up event listeners when editing state changes
	$effect(() => {
		console.log('[DEBUG CHILD] $effect triggered, isEditing:', isEditing, 'editInput:', editInput);

		if (isEditing) {
			// Add global event listeners for both mouse and touch interactions outside
			document.addEventListener('mousedown', handleOutsideInteraction);
			document.addEventListener('touchstart', handleOutsideInteraction);

			// iOS-compatible focus handling with user interaction detection
			const focusInput = () => {
				if (editInput) {
					console.log('[DEBUG CHILD] Focusing input, userTriggered:', userTriggeredEdit);
					try {
						// For iOS, we need to ensure the input is visible first
						editInput.style.opacity = '1';
						editInput.style.pointerEvents = 'auto';

						if (userTriggeredEdit) {
							// User-triggered edit - iOS will allow keyboard
							editInput.focus();
							setTimeout(() => {
								if (editInput && document.activeElement === editInput) {
									editInput.select();
									console.log('[DEBUG CHILD] User-triggered: Input focused and text selected');
								}
							}, 50);
						} else {
							// Programmatic edit (auto-edit) - iOS might not show keyboard
							// Try to focus anyway, but with longer delays
							setTimeout(() => {
								if (editInput) {
									editInput.focus();
									console.log('[DEBUG CHILD] Programmatic: Input focused');

									setTimeout(() => {
										if (editInput) {
											editInput.select();
											console.log('[DEBUG CHILD] Programmatic: Text selected');

											// For auto-edit on iOS, show a hint if keyboard didn't appear
											setTimeout(() => {
												if (
													editInput &&
													document.activeElement === editInput &&
													window.innerHeight === window.visualViewport?.height
												) {
													console.log(
														'[DEBUG CHILD] iOS keyboard may not have appeared - adding click handler'
													);
													// The input is focused but keyboard didn't appear - this is common on iOS for programmatic focus
												}
											}, 300);
										}
									}, 100);
								}
							}, 200);
						}
					} catch (error) {
						console.warn('[DEBUG CHILD] Focus error:', error);
					}
				}
			};

			if (editInput) {
				// Input is ready, focus with appropriate timing
				focusInput();
			} else {
				// Input not ready, wait for DOM update
				setTimeout(focusInput, 150);
			}
		} else {
			// Remove the event listeners when not editing
			document.removeEventListener('mousedown', handleOutsideInteraction);
			document.removeEventListener('touchstart', handleOutsideInteraction);
		}

		// Clean up function
		return () => {
			document.removeEventListener('mousedown', handleOutsideInteraction);
			document.removeEventListener('touchstart', handleOutsideInteraction);
		};
	});

	// Set up global listeners for slider dragging state
	$effect(() => {
		// Always add these listeners to handle slider drag state cleanup
		document.addEventListener('mouseup', handleGlobalMouseUp);
		document.addEventListener('touchend', handleGlobalTouchEnd);
		document.addEventListener('touchcancel', handleGlobalTouchEnd);

		// Clean up function
		return () => {
			document.removeEventListener('mouseup', handleGlobalMouseUp);
			document.removeEventListener('touchend', handleGlobalTouchEnd);
			document.removeEventListener('touchcancel', handleGlobalTouchEnd);
		};
	});

	// Auto-edit functionality removed - users must manually tap to edit
</script>

<div
	class="treemap-node"
	style="
    background-color: {getColorForNameHash(node.name)};
    border: {isOnlyChild ? 'none' : '1px solid #fff'};
    width: 100%;
    height: 100%;
    overflow: hidden;
    box-sizing: border-box;
  "
>
	<!-- Fulfillment Rectangle - positioned behind content but above background -->
	<div
		class="fulfillment-rectangle"
		style="
			position: absolute;
			top: 0;
			left: 0;
			width: {fulfillmentPercentage()}%;
			height: 100%;
			background-color: {getDarkerColorForNameHash(node.name)};
			z-index: 1;
			pointer-events: none;
		"
	>
		<!-- Note: Fulfillment is now controlled by the full-width slider positioned above the title -->
	</div>

	<div
		class="unified-node-content"
		style="
      width: 100%;
      height: 100%;
      display: flex;
      align-items: center;
      justify-content: center;
      padding: {adaptivePadding}px;
      box-sizing: border-box;
      position: relative;
      z-index: 2;
    "
	>
		<!-- Title - always centered in the container -->
		<div
			class="node-title-area"
			style="
        display: flex;
        align-items: center;
        justify-content: center;
        position: absolute;
        top: 50%;
        left: 50%;
        transform: translate(-50%, -50%);
        max-width: 95%;
        z-index: 2;
        pointer-events: {globalState.textEditMode ? 'auto' : 'none'};
      "
		>
			{#if isEditing}
				<input
					type="text"
					class="node-edit-input"
					bind:this={editInput}
					bind:value={editValue}
					onkeydown={handleEditKeydown}
					onblur={finishEditing}
					style="
						width: 100%;
						max-width: {Math.min(200, nodeSizeRatio * 3)}px;
						font-size: {fontSize()}px;
					"
					autocomplete="off"
					autocorrect="off"
					spellcheck="false"
				/>
			{:else if globalState.textEditMode}
				<div
					class="node-title"
					title={node.name}
					role="button"
					tabindex={0}
					style="font-size: {fontSize()}px;"
					onclick={handleTextEditActivation}
					ontouchend={handleTextEditActivation}
					onkeydown={(e) => {
						if (e.key === 'Enter' || e.key === ' ') {
							e.preventDefault();
							handleTextEditActivation(e);
						}
					}}
				>
					{#each segments as segment}
						<span class="title-segment">{segment}</span>
					{/each}
				</div>
			{:else}
				<div
					class="node-title non-interactive"
					title={node.name}
					style="font-size: {fontSize()}px;"
				>
					{#each segments as segment}
						<span class="title-segment">{segment}</span>
					{/each}
				</div>
			{/if}
		</div>

		<!-- Manual Fulfillment Slider - full width, positioned above title -->
		{#if hasAnyContributors && !node.hasChildren && visibilityFactor > 0.1 && !globalState.deleteMode && !globalState.recomposeMode}
			<div
				class="manual-fulfillment-slider"
				style="
					position: absolute;
					top: {sliderTopPercent}%;
					left: 0;
					width: 100%;
					height: {sliderHeightPercent}%;
					z-index: 3;
					opacity: {visibilityFactor};
					display: flex;
					align-items: center;
					padding: 0;
					margin: 0;
					box-sizing: border-box;
				"
				role="slider"
				tabindex="0"
				aria-valuenow={currentSliderValue()}
				aria-valuemin="0"
				aria-valuemax="100"
				aria-label="Manual fulfillment percentage"
				onkeydown={(e) => {
					e.stopPropagation();
					let newValue = currentSliderValue();

					switch (e.key) {
						case 'ArrowRight':
						case 'ArrowUp':
							e.preventDefault();
							newValue = Math.min(100, currentSliderValue() + 5);
							break;
						case 'ArrowLeft':
						case 'ArrowDown':
							e.preventDefault();
							newValue = Math.max(0, currentSliderValue() - 5);
							break;
						case 'Home':
							e.preventDefault();
							newValue = 0;
							break;
						case 'End':
							e.preventDefault();
							newValue = 100;
							break;
						case 'PageUp':
							e.preventDefault();
							newValue = Math.min(100, currentSliderValue() + 10);
							break;
						case 'PageDown':
							e.preventDefault();
							newValue = Math.max(0, currentSliderValue() - 10);
							break;
					}

					if (newValue !== currentSliderValue()) {
						const protocolValue = newValue / 100;
						onManualFulfillmentChange({
							nodeId: node.id,
							value: protocolValue,
							showNotification: true
						});
					}
				}}
			>
				<input
					type="range"
					min="0"
					max="100"
					value={currentSliderValue()}
					oninput={(event) => {
						// Only update fulfillment if we're actually dragging the thumb
						if (!isDraggingSliderThumb) {
							console.log(`[SLIDER-INPUT] Ignoring input - not dragging thumb, resetting value`);
							// Reset the slider to its current value to prevent unwanted changes
							event.currentTarget.value = currentSliderValue().toString();
							return;
						}

						const newValue = parseFloat(event.currentTarget.value);
						console.log(`[SLIDER-INPUT] Moving slider to ${newValue}% (ephemeral during drag)`);
						console.log(
							`[SLIDER-INPUT] Counter before: ${globalState.ephemeralFulfillmentCounter}`
						);
						// Update ephemeral value for visual feedback during drag - don't persist yet
						globalState.updateEphemeralFulfillment(node.id, newValue);
						console.log(`[SLIDER-INPUT] Counter after: ${globalState.ephemeralFulfillmentCounter}`);
						console.log(`[SLIDER-INPUT] Rectangle should now be: ${newValue}%`);
					}}
					onchange={(event) => {
						// Only persist changes if we were actually dragging the thumb
						if (!isDraggingSliderThumb) {
							console.log(`[SLIDER-CHANGE] Ignoring change - not dragging thumb, resetting value`);
							// Reset the slider to its current value to prevent unwanted changes
							event.currentTarget.value = currentSliderValue().toString();
							return;
						}

						const newValue = parseFloat(event.currentTarget.value);
						const protocolValue = newValue / 100; // Convert to 0-1 range for protocol
						console.log(
							`[SLIDER-CHANGE] Slider drag ended at ${newValue}%, persisting value: ${protocolValue}`
						);
						// Clear ephemeral value and persist the final result
						globalState.clearEphemeralFulfillment(node.id);
						// Now persist the actual value with notification
						onManualFulfillmentChange({
							nodeId: node.id,
							value: protocolValue,
							showNotification: true
						});

						// Reset dragging state
						isDraggingSliderThumb = false;
					}}
					onmousedown={(event) => {
						// Only allow dragging from the thumb, not track clicks
						const target = event.currentTarget;
						const rect = target.getBoundingClientRect();
						const clickX = event.clientX - rect.left;
						const thumbPosition = (currentSliderValue() / 100) * rect.width;
						const thumbSize = 20; // Match CSS thumb width

						// If click is not near the thumb, prevent slider behavior and forward to parent
						if (Math.abs(clickX - thumbPosition) > thumbSize / 2) {
							// Prevent the slider from changing value
							event.preventDefault();
							event.stopPropagation();

							// Manually forward the event to the parent clickable div for normal interactions
							const parentClickable = target.closest('.clickable');
							if (parentClickable) {
								// Create a synthetic pointer event that bypasses our slider exclusions
								const syntheticEvent = new PointerEvent('pointerdown', {
									bubbles: true,
									cancelable: true,
									clientX: event.clientX,
									clientY: event.clientY,
									button: event.button,
									buttons: event.buttons,
									pointerId: 1,
									isPrimary: true,
									pointerType: 'mouse'
								});

								// Mark this as a forwarded event to bypass slider exclusions
								(syntheticEvent as any).isForwardedFromSlider = true;

								// Dispatch directly on the parent, bypassing slider exclusions
								setTimeout(() => {
									parentClickable.dispatchEvent(syntheticEvent);
								}, 0);
							}
							return;
						}
						// Only when clicking the thumb, set dragging state and prevent parent interactions
						isDraggingSliderThumb = true;
						// Don't prevent default for thumb clicks - let slider work normally
						event.stopPropagation();
						event.stopImmediatePropagation();
					}}
					ontouchstart={(event) => {
						// Only allow dragging from the thumb, not track touches
						const target = event.currentTarget;
						const rect = target.getBoundingClientRect();
						const touch = event.touches[0];
						const touchX = touch.clientX - rect.left;
						const thumbPosition = (currentSliderValue() / 100) * rect.width;
						const thumbSize = 20; // Match CSS thumb width

						// If touch is not near the thumb, prevent slider behavior and forward to parent
						if (Math.abs(touchX - thumbPosition) > thumbSize / 2) {
							// Prevent the slider from changing value
							event.preventDefault();
							event.stopPropagation();

							// Manually forward the event to the parent clickable div for normal interactions
							const parentClickable = target.closest('.clickable');
							if (parentClickable) {
								// Create a synthetic pointer event that bypasses our slider exclusions
								const syntheticEvent = new PointerEvent('pointerdown', {
									bubbles: true,
									cancelable: true,
									clientX: touch.clientX,
									clientY: touch.clientY,
									button: 0,
									buttons: 1,
									pointerId: 1,
									isPrimary: true,
									pointerType: 'touch'
								});

								// Mark this as a forwarded event to bypass slider exclusions
								(syntheticEvent as any).isForwardedFromSlider = true;

								// Dispatch directly on the parent, bypassing slider exclusions
								setTimeout(() => {
									parentClickable.dispatchEvent(syntheticEvent);
								}, 0);
							}
							return;
						}
						// Only when touching the thumb, set dragging state and prevent parent interactions
						isDraggingSliderThumb = true;
						// Don't prevent default for thumb touches - let slider work normally
						event.stopPropagation();
						event.stopImmediatePropagation();
					}}
					onmousemove={(event) => {
						// Prevent parent interactions during thumb dragging
						if (isDraggingSliderThumb) {
							event.stopPropagation();
							event.stopImmediatePropagation();
						}
					}}
					ontouchmove={(event) => {
						// Prevent parent interactions during thumb dragging
						if (isDraggingSliderThumb) {
							event.stopPropagation();
							event.stopImmediatePropagation();
						}
					}}
					onmouseup={(event) => {
						// Prevent parent interactions when releasing thumb
						if (isDraggingSliderThumb) {
							event.stopPropagation();
							event.stopImmediatePropagation();
						}
					}}
					ontouchend={(event) => {
						// Prevent parent interactions when releasing thumb
						if (isDraggingSliderThumb) {
							event.stopPropagation();
							event.stopImmediatePropagation();
						}
					}}
					class="fulfillment-slider"
					style="width: 100%;"
				/>
			</div>
		{/if}

		<!-- Contributor Buttons Container - positioned below title -->
		{#if visibilityFactor > 0.1 && !node.hasChildren && !globalState.deleteMode}
			<div
				class="contributor-buttons-container"
				style="
					position: absolute;
					top: {buttonsTopPercent}%;
					left: 50%;
					transform: translate(-50%, -50%);
					width: {buttonContainerWidthPercent}%;
					height: {buttonHeightPercent}%;
					opacity: {visibilityFactor};
					display: flex;
					flex-direction: row;
					justify-content: center;
					align-items: center;
					gap: {Math.max(2, buttonHeight * 100 * 0.1)}px;
					pointer-events: none;
				"
			>
				<!-- Anti-Contributor Button: Always show on leaf nodes (left side) -->
				{#if hasAntiContributors}
					<!-- Pie Chart for Anti-Contributors -->
					<svg
						class="add-anti-contributor-button pie-chart"
						style="
							width: 47.5%;
							height: 100%;
							cursor: pointer;
							flex-shrink: 0;
							pointer-events: auto;
						"
						viewBox="-25 -25 50 50"
						role="button"
						tabindex="0"
						aria-label="Add anti-contributor"
						onclick={handleAddAntiContributorClick}
						ontouchstart={handleAddAntiContributorClick}
						onpointerdown={(e) => {
							e.preventDefault();
							e.stopPropagation();
							e.stopImmediatePropagation();
						}}
						onmousedown={(e) => {
							e.preventDefault();
							e.stopPropagation();
							e.stopImmediatePropagation();
						}}
						onkeydown={(e) => {
							if (e.key === 'Enter' || e.key === ' ') {
								e.preventDefault();
								// Create a synthetic MouseEvent for consistency
								const syntheticEvent = new MouseEvent('click', {
									bubbles: true,
									cancelable: true
								});
								handleAddAntiContributorClick(syntheticEvent);
							}
						}}
					>
						{#each antiPieData() as segment, i}
							<path
								d={arcPath()(segment)}
								fill={getColorForUserId(segment.data.id)}
								role="presentation"
								onmouseenter={() => (hoveredSegment = segment.data.id)}
								onmouseleave={() => (hoveredSegment = null)}
								style="opacity: {hoveredSegment === segment.data.id ? 0.8 : 1};"
							/>
						{/each}

						<!-- Minus sign in center for adding more anti-contributors -->
						<circle
							cx={0}
							cy={0}
							r={6}
							fill="rgba(255, 255, 255, 0.9)"
							stroke="rgba(0, 0, 0, 0.3)"
							stroke-width="1"
						/>
						<text
							x={0}
							y={0}
							text-anchor="middle"
							dominant-baseline="middle"
							font-size="8"
							fill="#333"
							font-weight="bold"
						>
							−
						</text>
					</svg>
				{:else}
					<!-- Simple button when no anti-contributors -->
					<button
						class="add-anti-contributor-button"
						style="
							width: 47.5%;
							height: 100%;
							font-size: {Math.max(12, buttonSizePercent * 0.8)}px;
							flex-shrink: 0;
							pointer-events: auto;
						"
						onclick={handleAddAntiContributorClick}
						ontouchstart={handleAddAntiContributorClick}
						onpointerdown={(e) => {
							e.preventDefault();
							e.stopPropagation();
							e.stopImmediatePropagation();
						}}
						onmousedown={(e) => {
							e.preventDefault();
							e.stopPropagation();
							e.stopImmediatePropagation();
						}}
						title="Add anti-contributor"
					>
						−
					</button>
				{/if}

				<!-- Contributor Button: Always show on leaf nodes (right side) -->
				{#if hasContributors}
					<!-- Pie Chart for Contributors -->
					<svg
						class="add-contributor-button pie-chart"
						style="
							width: 47.5%;
							height: 100%;
							cursor: pointer;
							flex-shrink: 0;
							pointer-events: auto;
						"
						viewBox="-25 -25 50 50"
						role="button"
						tabindex="0"
						aria-label="Add contributor"
						onclick={handleAddContributorClick}
						ontouchstart={handleAddContributorClick}
						onpointerdown={(e) => {
							e.preventDefault();
							e.stopPropagation();
							e.stopImmediatePropagation();
						}}
						onmousedown={(e) => {
							e.preventDefault();
							e.stopPropagation();
							e.stopImmediatePropagation();
						}}
						onkeydown={(e) => {
							if (e.key === 'Enter' || e.key === ' ') {
								e.preventDefault();
								// Create a synthetic MouseEvent for consistency
								const syntheticEvent = new MouseEvent('click', {
									bubbles: true,
									cancelable: true
								});
								handleAddContributorClick(syntheticEvent);
							}
						}}
					>
						{#each pieData() as segment, i}
							<path
								d={arcPath()(segment)}
								fill={getColorForUserId(segment.data.id)}
								role="presentation"
								onmouseenter={() => (hoveredSegment = segment.data.id)}
								onmouseleave={() => (hoveredSegment = null)}
								style="opacity: {hoveredSegment === segment.data.id ? 0.8 : 1};"
							/>
						{/each}

						<!-- Plus sign in center for adding more contributors -->
						<circle
							cx={0}
							cy={0}
							r={6}
							fill="rgba(255, 255, 255, 0.9)"
							stroke="rgba(0, 0, 0, 0.3)"
							stroke-width="1"
						/>
						<text
							x={0}
							y={0}
							text-anchor="middle"
							dominant-baseline="middle"
							font-size="8"
							fill="#333"
							font-weight="bold"
						>
							+
						</text>
					</svg>
				{:else}
					<!-- Simple button when no contributors -->
					<button
						class="add-contributor-button"
						style="
							width: 47.5%;
							height: 100%;
							font-size: {Math.max(12, buttonSizePercent * 0.8)}px;
							flex-shrink: 0;
							pointer-events: auto;
						"
						onclick={handleAddContributorClick}
						ontouchstart={handleAddContributorClick}
						onpointerdown={(e) => {
							e.preventDefault();
							e.stopPropagation();
							e.stopImmediatePropagation();
						}}
						onmousedown={(e) => {
							e.preventDefault();
							e.stopPropagation();
							e.stopImmediatePropagation();
						}}
						title="Add contributor"
					>
						+
					</button>
				{/if}
			</div>

			<!-- Tooltips for hovered segments -->
			{#if hoveredSegment && !globalState.deleteMode}
				<div
					class="contributor-tooltip"
					style="
						position: absolute;
						top: {buttonsTopPercent + 8}%;
						left: 50%;
						transform: translateX(-50%);
						z-index: 1000;
					"
				>
					{contributorNames[hoveredSegment] || hoveredSegment}
					{#if (node.antiContributors || []).includes(hoveredSegment)}
						<span class="tooltip-type">(anti-contributor)</span>
					{:else}
						<span class="tooltip-type">(contributor)</span>
					{/if}
				</div>
			{/if}
		{/if}
	</div>
</div>

<style>
	.treemap-node {
		transition:
			background-color 0.2s ease,
			border 0.2s ease;
		border-radius: 6px;
		position: relative;
		/* Enable container queries for responsive text */
		container-type: size;
	}

	.unified-node-content {
		text-align: center;
	}

	.node-title-area {
		/* Cross-device text interaction */
		user-select: none; /* Prevent text selection during normal interaction */
		-webkit-user-select: none;
		-moz-user-select: none;
		-ms-user-select: none;
		/* Clean touch feedback */
		-webkit-tap-highlight-color: rgba(0, 0, 0, 0.05);
		touch-action: manipulation;
		/* pointer-events is set dynamically via inline style based on textEditMode */
	}

	/* In delete mode, allow events to pass through to parent */
	:global(.deleting) .node-title-area {
		pointer-events: none;
	}

	.node-title {
		color: rgba(0, 0, 0, 0.8);
		text-shadow:
			0px 0px 3px rgba(255, 255, 255, 0.8),
			0px 0px 2px rgba(255, 255, 255, 0.6);
		font-weight: 500;
		cursor: pointer;
		/* Constrain to container */
		width: 100%;
		max-width: 100%;
		overflow: visible; /* Changed from hidden to prevent clipping */
		text-align: center;
		padding: 2px; /* Add small padding to prevent edge clipping */
		line-height: 1.1;
		/* Font size set dynamically via inline style */
		/* Cross-device interaction */
		user-select: none;
		-webkit-user-select: none;
		-moz-user-select: none;
		-ms-user-select: none;
		-webkit-tap-highlight-color: rgba(0, 0, 0, 0.05);
		touch-action: manipulation;
		/* Smooth interaction feedback */
		transition: opacity 0.1s ease;
	}

	.node-title:hover {
		opacity: 0.8;
	}

	.node-title:active {
		opacity: 0.6;
	}

	/* Non-interactive title styling - no visual feedback */
	.node-title.non-interactive {
		cursor: default;
		-webkit-tap-highlight-color: transparent;
	}

	.node-title.non-interactive:hover {
		opacity: 1; /* No hover effect */
	}

	.node-title.non-interactive:active {
		opacity: 1; /* No active effect */
	}

	.title-segment {
		line-height: 1.2; /* Balanced: prevents clipping but not too much spacing */
		display: block;
	}

	.node-edit-input {
		background: rgba(255, 255, 255, 0.98);
		border: 2px solid rgba(33, 150, 243, 0.5);
		border-radius: 6px;
		padding: 8px 12px;
		text-align: center;
		box-shadow: 0 4px 12px rgba(0, 0, 0, 0.15);
		color: #333;
		outline: none;
		font-weight: 500;
		/* Font size set dynamically via inline style to match title */
		/* Cross-device text editing */
		touch-action: manipulation;
		user-select: text;
		-webkit-user-select: text;
		-moz-user-select: text;
		-ms-user-select: text;
		/* iOS-specific improvements */
		-webkit-appearance: none;
		appearance: none;
		-webkit-border-radius: 6px;
		/* iOS keyboard compatibility */
		-webkit-user-modify: read-write-plaintext-only;
		-webkit-tap-highlight-color: transparent;
		/* Ensure proper layering */
		position: relative;
		z-index: 100;
		/* Smooth transitions */
		transition: all 0.2s ease;
	}

	.node-edit-input:focus {
		border-color: #2196f3;
		box-shadow: 0 0 0 2px rgba(33, 150, 243, 0.2);
	}

	.add-contributor-button {
		display: flex;
		align-items: center;
		justify-content: center;
		background: rgba(255, 255, 255, 0.7);
		color: #555;
		cursor: pointer;
		transition: all 0.2s ease;
		font-family: Arial, sans-serif;
		font-weight: normal;
		line-height: 1;
	}

	.add-contributor-button:hover {
		background: rgba(255, 255, 255, 0.9);
		color: #333;
	}

	.add-anti-contributor-button {
		display: flex;
		align-items: center;
		justify-content: center;
		background: rgba(255, 220, 220, 0.7);
		color: #555;
		cursor: pointer;
		transition: all 0.2s ease;
		font-family: Arial, sans-serif;
		font-weight: normal;
		line-height: 1;
	}

	.add-anti-contributor-button:hover {
		background: rgba(255, 220, 220, 0.9);
		color: #333;
	}

	.pie-chart {
		border-radius: 4px;
		transition: opacity 0.2s ease;
	}

	.pie-chart:hover {
		opacity: 0.9;
	}

	.pie-chart path {
		transition: opacity 0.2s ease;
	}

	.pie-chart path:hover {
		opacity: 0.8;
	}

	.contributor-buttons-container {
		/* Ensure flexbox layout works properly */
		min-height: 20px;
	}

	.contributor-tooltip {
		background: rgba(0, 0, 0, 0.8);
		color: white;
		padding: 4px 8px;
		border-radius: 4px;
		font-size: 12px;
		pointer-events: none;
		white-space: nowrap;
	}

	.contributor-tooltip .tooltip-type {
		opacity: 0.7;
		font-size: 10px;
		margin-left: 4px;
	}

	.contributor-tooltip::after {
		content: '';
		position: absolute;
		top: 100%;
		left: 50%;
		transform: translateX(-50%);
		border: 4px solid transparent;
		border-top-color: rgba(0, 0, 0, 0.8);
	}

	.manual-fulfillment-slider {
		box-sizing: border-box;
		/* Flex layout and styling handled inline for dynamic sizing */
	}

	.fulfillment-slider {
		width: 100%;
		height: 20px;
		-webkit-appearance: none;
		appearance: none;
		background: transparent;
		outline: none;
		cursor: default; /* Changed from ew-resize to default for track area */
		transition: all 0.2s ease;
		pointer-events: auto;
		z-index: 10;
		position: relative;
		margin: 0;
		padding: 0;
	}

	/* Hide the track completely */
	.fulfillment-slider::-webkit-slider-track {
		background: transparent;
		border: none;
		height: 0;
	}

	.fulfillment-slider::-moz-range-track {
		background: transparent;
		border: none;
		height: 0;
	}

	.fulfillment-slider::-webkit-slider-thumb {
		-webkit-appearance: none;
		appearance: none;
		width: 20px;
		height: 20px;
		background: rgba(0, 0, 0, 0.6);
		cursor: ew-resize; /* Keep resize cursor on thumb */
		border-radius: 2px;
		box-shadow:
			0 2px 6px rgba(0, 0, 0, 0.3),
			0 0 0 2px rgba(255, 255, 255, 0.8);
		transition: all 0.2s ease;
		position: relative;
	}

	.fulfillment-slider::-webkit-slider-thumb:hover {
		background: rgba(0, 0, 0, 0.8);
		transform: scaleY(1.2);
		box-shadow:
			0 3px 8px rgba(0, 0, 0, 0.4),
			0 0 0 2px rgba(255, 255, 255, 1);
	}

	.fulfillment-slider::-moz-range-thumb {
		width: 20px;
		height: 20px;
		background: rgba(0, 0, 0, 0.6);
		cursor: ew-resize; /* Keep resize cursor on thumb */
		border-radius: 2px;
		box-shadow:
			0 2px 6px rgba(0, 0, 0, 0.3),
			0 0 0 2px rgba(255, 255, 255, 0.8);
		border: none;
		transition: all 0.2s ease;
	}

	.fulfillment-slider::-moz-range-thumb:hover {
		background: rgba(0, 0, 0, 0.8);
		transform: scaleY(1.2);
		box-shadow:
			0 3px 8px rgba(0, 0, 0, 0.4),
			0 0 0 2px rgba(255, 255, 255, 1);
	}

	.fulfillment-rectangle {
		/* Container for the fulfillment visualization */
		position: relative;
	}

	/* Note: Old fulfillment lip styles removed - now using full-width slider approach */
</style>
