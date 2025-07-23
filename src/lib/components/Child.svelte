<script lang="ts">
	import { getColorForNameHash, getColorForUserId } from '$lib/utils/colorUtils';
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
		onManualFulfillmentChange = (detail: { nodeId: string; value: number }) => {}, // Add callback for manual fulfillment
		shouldEdit = false // Flag to indicate if this node should immediately be in edit mode
	} = $props<{
		node: NodeData;
		dimensions: Dimensions;
		addContributor?: (detail: { nodeId: string; clientX: number; clientY: number }) => void;
		addAntiContributor?: (detail: { nodeId: string; clientX: number; clientY: number }) => void;
		removeContributor?: (detail: { nodeId: string; contributorId: string }) => void;
		onTextEdit?: (detail: { nodeId: string; newName: string }) => void;
		onManualFulfillmentChange?: (detail: { nodeId: string; value: number }) => void; // Add callback type
		shouldEdit?: boolean;
	}>();

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

	// Calculate relative size for text scaling
	const nodeWidth = $derived(dimensions.x1 - dimensions.x0);
	const nodeHeight = $derived(dimensions.y1 - dimensions.y0);
	const nodeSizeRatio = $derived(Math.min(nodeWidth, nodeHeight) * 100); // Size as percentage of parent

	// Prepare text segments first
	const segments = $derived(
		node.name === 'Unnamed' ? [node.name] : node.name.split(/(?=[A-Z][^A-Z])/g)
	);

	// Calculate font size based on actual text content and container constraints
	const longestSegment = $derived(
		segments.reduce(
			(longest: string, segment: string) => (segment.length > longest.length ? segment : longest),
			''
		)
	);

	const fontSize = $derived(() => {
		// More aggressive approach - use most of the available space
		const availableWidth = nodeWidth * 0.9 * 400; // 90% of node width
		const availableHeight = nodeHeight * 0.9 * 400; // 90% of node height

		// More realistic character width
		const charWidth = 0.55; // Closer to typical character width

		// Calculate max font size using most of available space
		const maxFontSizeForWidth = (availableWidth * 0.95) / (longestSegment.length * charWidth * 16); // 95% of available width

		// Calculate height with tighter line spacing
		const lineHeight = 1.15; // Tighter spacing
		const maxFontSizeForHeight = (availableHeight * 0.95) / (segments.length * lineHeight * 16); // 95% of available height

		// Take the more restrictive constraint
		const calculatedSize = Math.min(maxFontSizeForWidth, maxFontSizeForHeight);

		// Apply minimal safety margin
		const safeSize = calculatedSize * 0.95; // Only 5% safety margin
		return Math.max(0.4, Math.min(3.5, safeSize));
	});

	// Determine if this is the only child (occupies 100% of the space)
	const isOnlyChild = $derived(nodeWidth >= 0.999 && nodeHeight >= 0.999);

	// Unified spacing calculations
	const adaptivePadding = $derived(Math.max(1, Math.min(8, nodeSizeRatio * 0.1)));

	// Button scaling
	const contributorSize = $derived(Math.max(12, Math.min(40, nodeSizeRatio * 0.5)));

	// Calculate visibility factor (0-1) for smooth fade-in
	const visibilityFactor = $derived(Math.min(1, Math.max(0, (nodeSizeRatio - 5) / 7)));

	// Calculate button positioning - place it between title end and container edge
	const titleWidthPercent = $derived(
		((longestSegment.length * fontSize() * 0.6) / (nodeWidth * 100)) * 100
	); // Title width as percentage
	const titleEndPercent = $derived(50 + titleWidthPercent / 2); // Where title ends
	const buttonCenterPercent = $derived(titleEndPercent + (100 - titleEndPercent) / 2); // Middle of remaining space
	const availableSpacePercent = $derived(100 - titleEndPercent); // Space available for button
	const buttonSizePercent = $derived(Math.min(availableSpacePercent * 0.6, 15)); // Button size as percentage, max 15%

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

	// Get current slider value from node data
	const currentSliderValue = $derived(
		node.manualFulfillment !== undefined ? Math.round(node.manualFulfillment * 100) : 100
	);

	// Function to handle add contributor button click (mouse or touch)
	function handleAddContributorClick(event: MouseEvent | TouchEvent) {
		// Don't allow adding contributors in delete mode
		if (globalState.deleteMode) {
			globalState.showToast('Cannot add contributors in delete mode', 'warning');
			return;
		}

		// Don't allow adding contributors when editing
		if (globalState.editMode) {
			globalState.showToast('Cannot add contributors while editing', 'warning');
			return;
		}

		// Don't allow adding contributors in recompose mode
		if (globalState.recomposeMode) {
			globalState.showToast('Cannot add contributors in recompose mode', 'warning');
			return;
		}

		event.stopPropagation();
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
		// Don't allow adding anti-contributors in delete mode
		if (globalState.deleteMode) {
			globalState.showToast('Cannot add anti-contributors in delete mode', 'warning');
			return;
		}

		// Don't allow adding anti-contributors when editing
		if (globalState.editMode) {
			globalState.showToast('Cannot add anti-contributors while editing', 'warning');
			return;
		}

		// Don't allow adding anti-contributors in recompose mode
		if (globalState.recomposeMode) {
			globalState.showToast('Cannot add anti-contributors in recompose mode', 'warning');
			return;
		}

		event.stopPropagation();
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

	// Selective handler for text selection prevention - only for specific events
	function preventAllInterference(event: Event) {
		console.log('[DEBUG CHILD] preventAllInterference called, event type:', event.type);

		// Only prevent drag start events, not scroll-related events
		if (event.type === 'dragstart') {
			// Clear selections first
			document.getSelection()?.removeAllRanges();

			// Only prevent drag events, allow other events to bubble
			event.preventDefault();
			event.stopPropagation();
			return false;
		}

		// For other events, just clear selections but allow normal behavior
		document.getSelection()?.removeAllRanges();
		return true;
	}

	// Handle edit initiation with selective interference prevention
	function handleTextEditStart(event: Event) {
		console.log(
			'[DEBUG CHILD] handleTextEditStart called, event type:',
			event.type,
			'target:',
			event.target
		);

		// Prevent event propagation to parent but preserve user gesture
		event.stopPropagation();

		// Clear selections IMMEDIATELY before any other processing
		document.getSelection()?.removeAllRanges();

		// For pointer/mouse events, prevent default to stop text selection
		// But allow touch events to preserve gesture for mobile keyboard and scrolling
		if (event.type !== 'touchstart' && event.type !== 'touchmove') {
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
			// Global state prevented editing (e.g., in delete mode)
			return;
		}

		// Set up edit state - this will be synced via the effect above
		console.log('[DEBUG CHILD] Edit mode entered successfully');
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

	// Set up and clean up event listeners when editing state changes
	$effect(() => {
		console.log('[DEBUG CHILD] $effect triggered, isEditing:', isEditing, 'editInput:', editInput);

		if (isEditing) {
			// Add global event listeners for both mouse and touch interactions outside
			document.addEventListener('mousedown', handleOutsideInteraction);
			document.addEventListener('touchstart', handleOutsideInteraction);

			// Focus the input when it's available (works for both desktop and mobile)
			if (editInput) {
				console.log('[DEBUG CHILD] editInput exists, focusing immediately');
				setTimeout(() => {
					console.log('[DEBUG CHILD] Attempting to focus input, editInput:', editInput);
					editInput?.focus();
					// Select text only if not on mobile to avoid virtual keyboard issues
					if (
						!/Android|webOS|iPhone|iPad|iPod|BlackBerry|IEMobile|Opera Mini/i.test(
							navigator.userAgent
						)
					) {
						editInput?.select();
					}
					console.log(
						'[DEBUG CHILD] Focus/select called, document.activeElement:',
						document.activeElement
					);
				}, 10);
			} else {
				console.log('[DEBUG CHILD] editInput not ready, setting up retry');
				// Input not ready yet, try again with longer delay
				setTimeout(() => {
					console.log('[DEBUG CHILD] Retry - editInput:', editInput);
					if (editInput) {
						console.log('[DEBUG CHILD] Retry focusing input');
						editInput.focus();
						// Select text only if not on mobile to avoid virtual keyboard issues
						if (
							!/Android|webOS|iPhone|iPad|iPod|BlackBerry|IEMobile|Opera Mini/i.test(
								navigator.userAgent
							)
						) {
							editInput.select();
						}
						console.log(
							'[DEBUG CHILD] Retry focus/select called, document.activeElement:',
							document.activeElement
						);
					} else {
						console.log('[DEBUG CHILD] Input still not ready after retry');
					}
				}, 100);
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

	// Check if this node should enter edit mode (either from prop or from being newly created)
	$effect(() => {
		if (shouldEdit && !isEditing && node.id) {
			// Try to enter edit mode through global state
			const canEdit = globalState.enterEditMode(node.id);
			if (canEdit) {
				// Edit state will be synced via the effect above
				console.log('[DEBUG CHILD] Auto-entering edit mode for new node');
			}
		}
	});
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
        max-width: 90%;
        z-index: 2;
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
          font-size: {fontSize()}rem;
          width: 100%;
          max-width: {Math.min(200, nodeSizeRatio * 3)}px;
        "
					autocomplete="off"
					autocorrect="off"
					spellcheck="false"
				/>
			{:else}
				<div
					class="node-title"
					style="font-size: {fontSize()}rem;"
					title={node.name}
					onmousedown={handleTextEditStart}
					ontouchstart={handleTextEditStart}
					onpointerdown={handleTextEditStart}
					onselectstart={preventAllInterference}
					ondragstart={preventAllInterference}
					oncontextmenu={preventAllInterference}
					ondblclick={preventAllInterference}
				>
					{#each segments as segment}
						<span class="title-segment">{segment}</span>
					{/each}
				</div>
			{/if}
		</div>

		<!-- Manual Fulfillment Slider - only for contribution nodes -->
		{#if hasContributors && !node.hasChildren && visibilityFactor > 0.1}
			<div
				class="manual-fulfillment-slider"
				style="
					position: absolute;
					top: 70%;
					left: 50%;
					transform: translateX(-50%);
					width: 70%;
					max-width: 120px;
					z-index: 3;
					opacity: {visibilityFactor};
				"
				onmousedown={(e) => e.stopPropagation()}
				ontouchstart={(e) => e.stopPropagation()}
				onpointerdown={(e) => e.stopPropagation()}
				onclick={(e) => e.stopPropagation()}
			>
				<input
					type="range"
					min="0"
					max="100"
					value={currentSliderValue}
					oninput={(event) => {
						const newValue = parseFloat(event.currentTarget.value);
						const protocolValue = newValue / 100; // Convert to 0-1 range for protocol
						console.log(
							`[SLIDER-INPUT] Moving slider to ${newValue}%, protocol value: ${protocolValue}`
						);
						onManualFulfillmentChange({ nodeId: node.id, value: protocolValue });
					}}
					class="fulfillment-slider"
				/>
				<div
					class="slider-value"
					style="
						font-size: {Math.max(8, Math.min(14, buttonSizePercent * 0.3))}px;
						margin-top: {Math.max(2, buttonSizePercent * 0.15)}px;
					"
				>
					{currentSliderValue}%
				</div>
			</div>
		{/if}

		<!-- Contributor Buttons Container - centered and consistent -->
		{#if visibilityFactor > 0.1 && !node.hasChildren}
			<div
				class="contributor-buttons-container"
				style="
					position: absolute;
					top: 50%;
					left: {buttonCenterPercent}%;
					transform: translate(-50%, -50%);
					width: {buttonSizePercent}%;
					height: {hasAnyContributors ? buttonSizePercent * 2.2 : buttonSizePercent}%;
					opacity: {visibilityFactor};
					display: flex;
					flex-direction: column;
					justify-content: center;
					align-items: center;
					gap: {Math.max(2, buttonSizePercent * 0.1)}px;
				"
				onmousedown={(e) => e.stopPropagation()}
				ontouchstart={(e) => e.stopPropagation()}
				onpointerdown={(e) => e.stopPropagation()}
				onclick={(e) => e.stopPropagation()}
			>
				<!-- Contributor Button (always present) -->
				{#if hasContributors}
					<!-- Pie Chart for Contributors -->
					<svg
						class="add-contributor-button pie-chart"
						style="
							width: 100%;
							height: {hasAnyContributors ? '45%' : '100%'};
							cursor: pointer;
							flex-shrink: 0;
						"
						viewBox="-25 -25 50 50"
						onclick={handleAddContributorClick}
						ontouchstart={handleAddContributorClick}
					>
						{#each pieData() as segment, i}
							<path
								d={arcPath()(segment)}
								fill={getColorForUserId(segment.data.id)}
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
							width: 100%;
							height: {hasAnyContributors ? '45%' : '100%'};
							font-size: {Math.max(12, buttonSizePercent * 0.8)}px;
							flex-shrink: 0;
						"
						onclick={handleAddContributorClick}
						ontouchstart={handleAddContributorClick}
						title="Add contributor"
					>
						+
					</button>
				{/if}

				<!-- Anti-Contributor Button -->
				{#if hasAntiContributors}
					<!-- Pie Chart for Anti-Contributors -->
					<svg
						class="add-anti-contributor-button pie-chart"
						style="
							width: 100%;
							height: 45%;
							cursor: pointer;
							flex-shrink: 0;
						"
						viewBox="-25 -25 50 50"
						onclick={handleAddAntiContributorClick}
						ontouchstart={handleAddAntiContributorClick}
					>
						{#each antiPieData() as segment, i}
							<path
								d={arcPath()(segment)}
								fill={getColorForUserId(segment.data.id)}
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
							width: 100%;
							height: 45%;
							font-size: {Math.max(12, buttonSizePercent * 0.8)}px;
							flex-shrink: 0;
						"
						onclick={handleAddAntiContributorClick}
						ontouchstart={handleAddAntiContributorClick}
						title="Add anti-contributor"
					>
						−
					</button>
				{/if}
			</div>

			<!-- Tooltips for hovered segments -->
			{#if hoveredSegment}
				<div
					class="contributor-tooltip"
					style="
						position: absolute;
						top: {50 - (hasAnyContributors ? buttonSizePercent * 1.1 : buttonSizePercent / 2) - 5}%;
						left: {buttonCenterPercent}%;
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
		border-radius: 2px;
		position: relative;
	}

	.unified-node-content {
		text-align: center;
	}

	.node-title-area {
		/* Comprehensive text selection prevention */
		user-select: none;
		-webkit-user-select: none;
		-moz-user-select: none;
		-ms-user-select: none;
		/* Touch interaction control */
		-webkit-touch-callout: none;
		-webkit-tap-highlight-color: transparent;
		touch-action: manipulation;
		/* Prevent drag operations */
		-webkit-user-drag: none;
		pointer-events: auto;
	}

	.node-title {
		color: rgba(0, 0, 0, 0.8);
		text-shadow:
			0px 0px 3px rgba(255, 255, 255, 0.8),
			0px 0px 2px rgba(255, 255, 255, 0.6);
		font-weight: 500;
		cursor: text;
		/* Advanced text fitting */
		word-break: break-word;
		hyphens: auto;
		overflow-wrap: break-word;
		word-wrap: break-word;
		/* Constrain to container */
		width: 100%;
		max-width: 100%;
		overflow: hidden;
		text-align: center;
		/* Font size will be set via inline style */
		line-height: 1.1;
		/* Inherit all interference prevention from parent */
		user-select: none;
		-webkit-user-select: none;
		-moz-user-select: none;
		-ms-user-select: none;
		-webkit-touch-callout: none;
		-webkit-tap-highlight-color: transparent;
		touch-action: manipulation;
		-webkit-user-drag: none;
	}

	.title-segment {
		line-height: 1.1;
		display: block;
	}

	.node-edit-input {
		background: rgba(255, 255, 255, 0.9);
		border: 1px solid rgba(0, 0, 0, 0.2);
		border-radius: 4px;
		padding: 4px 8px;
		text-align: center;
		box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1);
		color: #333;
		outline: none;
		/* Ensure proper touch interaction */
		touch-action: manipulation;
		user-select: text;
		-webkit-user-select: text;
		-moz-user-select: text;
		-ms-user-select: text;
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
		display: flex;
		flex-direction: column;
		align-items: center;
		gap: 4px;
	}

	.fulfillment-slider {
		width: 100%;
		height: 6px;
		-webkit-appearance: none;
		background: rgba(255, 255, 255, 0.3);
		outline: none;
		border-radius: 3px;
		cursor: pointer;
		transition: all 0.2s ease;
		pointer-events: auto;
		z-index: 10;
		position: relative;
	}

	.fulfillment-slider:hover {
		background: rgba(255, 255, 255, 0.5);
	}

	.fulfillment-slider::-webkit-slider-thumb {
		-webkit-appearance: none;
		appearance: none;
		width: 16px;
		height: 16px;
		background: #2196f3;
		cursor: pointer;
		border-radius: 50%;
		box-shadow: 0 2px 4px rgba(0, 0, 0, 0.2);
		transition: all 0.2s ease;
	}

	.fulfillment-slider::-webkit-slider-thumb:hover {
		background: #1976d2;
		transform: scale(1.1);
	}

	.fulfillment-slider::-moz-range-thumb {
		width: 16px;
		height: 16px;
		background: #2196f3;
		cursor: pointer;
		border-radius: 50%;
		box-shadow: 0 2px 4px rgba(0, 0, 0, 0.2);
		border: none;
		transition: all 0.2s ease;
	}

	.fulfillment-slider::-moz-range-thumb:hover {
		background: #1976d2;
		transform: scale(1.1);
	}

	.slider-value {
		background: rgba(0, 0, 0, 0.8);
		color: white;
		padding: 2px 6px;
		border-radius: 3px;
		font-weight: 500;
		white-space: nowrap;
		text-shadow: none;
	}
</style>
