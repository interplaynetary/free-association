<script lang="ts">
	import TagPill from './TagPill.svelte';
	import { getColorForNameHash, getColorForUserId } from '$lib/utils/colorUtils';

	// Define interface for node data
	interface NodeData {
		id: string;
		name: string;
		points: number;
		contributors: string[];
		fulfillment?: number;
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
		removeContributor = (detail: { nodeId: string; contributorId: string }) => {},
		onTextEdit = (detail: { nodeId: string; newName: string }) => {},
		shouldEdit = false // Flag to indicate if this node should immediately be in edit mode
	} = $props<{
		node: NodeData;
		dimensions: Dimensions;
		addContributor?: (detail: { nodeId: string; clientX: number; clientY: number }) => void;
		removeContributor?: (detail: { nodeId: string; contributorId: string }) => void;
		onTextEdit?: (detail: { nodeId: string; newName: string }) => void;
		shouldEdit?: boolean;
	}>();

	// Editing state
	let isEditing = $state(false);
	let editValue = $state('');
	let editInput: HTMLInputElement | null = $state(null);

	// Prepare text segments
	const segments = $derived(
		node.name === 'Unnamed' ? [node.name] : node.name.split(/(?=[A-Z][^A-Z])/g)
	);

	// Calculate relative size for text scaling
	const nodeWidth = $derived(dimensions.x1 - dimensions.x0);
	const nodeHeight = $derived(dimensions.y1 - dimensions.y0);
	const nodeSizeRatio = $derived(Math.min(nodeWidth, nodeHeight) * 100); // Size as percentage of parent
	const fontSize = $derived(Math.max(0.5, Math.min(2, nodeSizeRatio * 0.1))); // Scale font size (0.5-2rem)

	// Determine if this is the only child (occupies 100% of the space)
	const isOnlyChild = $derived(nodeWidth >= 0.999 && nodeHeight >= 0.999);

	// Calculate available space and scaling factors
	const hasContributors = $derived(node.contributors.length > 0);
	const minButtonWidth = 33.33; // Minimum button width (1/3 of container)
	const totalElements = $derived(node.contributors.length + 1); // +1 for the add button

	// Calculate element widths ensuring button never goes below minimum
	const buttonWidth = $derived(
		hasContributors
			? Math.max(minButtonWidth, 100 / totalElements) // Never smaller than 1/3
			: 100 // Full width when no contributors
	);

	// Remaining width for tags
	const remainingWidth = $derived(hasContributors ? 100 - buttonWidth : 0);

	// Scale for tags based on available width and number of contributors
	const tagScale = $derived(
		hasContributors
			? Math.min(1.2, remainingWidth / (node.contributors.length * 33.33)) // Scale down if too many tags
			: 1
	);

	// Button font size remains proportional to its container
	const buttonFontSize = $derived(Math.min(buttonWidth * 0.3, 32)); // Adjusted scaling factor and max size

	// Calculate visibility factor (0-1) for smooth fade-in
	const visibilityFactor = $derived(Math.min(1, Math.max(0, (nodeSizeRatio - 5) / 7)));

	// Dynamically decide between full and mini view based on size
	const showFullContributorDetails = $derived(nodeSizeRatio >= 25);

	// Dynamically adjust truncation length based on node size
	const truncateLength = $derived(Math.floor(Math.max(3, Math.min(12, nodeSizeRatio * 0.15))));

	// Unified spacing calculations
	const adaptivePadding = $derived(Math.max(1, Math.min(8, nodeSizeRatio * 0.1)));
	const adaptiveGap = $derived(Math.max(2, Math.min(8, nodeSizeRatio * 0.08)));

	// Button scaling
	const contributorSize = $derived(Math.max(12, Math.min(40, nodeSizeRatio * 0.5)));

	// Calculate actual space requirements
	const titleHeight = $derived(segments.length * fontSize * 1.1); // Actual title height in rem
	const titleHeightPx = $derived(titleHeight * 16); // Convert rem to pixels (assuming 16px base)
	const containerHeightPx = $derived(nodeHeight * 100); // Container height in pixels

	// Calculate where title ends (50% center + half title height)
	const titleBottomPercent = $derived(50 + (titleHeightPx / containerHeightPx) * 50);

	// Space available below title (from title bottom to container bottom, minus padding)
	const availableSpaceBelow = $derived(
		100 - titleBottomPercent - (adaptivePadding / containerHeightPx) * 100
	);

	// Space needed for contributors (button size + gap, converted to percentage)
	const spaceNeededForContributors = $derived(
		((contributorSize + adaptiveGap * 2) / containerHeightPx) * 100
	);

	// Use vertical layout if there's enough space below, otherwise horizontal
	const useVerticalLayout = $derived(availableSpaceBelow >= spaceNeededForContributors);

	// Function to handle add contributor button click
	function handleAddContributorClick(event: MouseEvent) {
		event.stopPropagation();
		const nodeId = node.id;
		if (nodeId) {
			addContributor({
				nodeId,
				clientX: event.clientX,
				clientY: event.clientY
			});
		}
	}

	// Function to handle remove contributor
	function handleRemoveContributor(contributorId: string) {
		const nodeId = node.id;
		if (nodeId) {
			removeContributor({ nodeId, contributorId });
		}
	}

	// Handle click on tag
	function handleTagClick(userId: string, event?: MouseEvent) {
		// Prevent default behavior if event is passed
		if (event) {
			event.stopPropagation();
			event.preventDefault();
		}

		// For future functionality - could navigate to contributor view
		console.log(`Clicked on contributor: ${userId}`);
	}

	// Handle text edit start
	function handleTextEditStart(event: MouseEvent | TouchEvent) {
		// More aggressive event stopping to prevent navigation and text selection
		event.stopPropagation();
		event.preventDefault();

		// Also stop immediate propagation to prevent any other handlers
		if ('stopImmediatePropagation' in event) {
			event.stopImmediatePropagation();
		}

		// For touch events, also prevent default to stop text selection menus
		if (event.type === 'touchstart') {
			// Add a small delay for touch events to ensure proper handling
			setTimeout(() => {
				startEdit();
			}, 10);
		} else {
			startEdit();
		}

		function startEdit() {
			// Only allow editing if we have a valid node ID
			const nodeId = node.id;
			if (!nodeId) return;

			// Set up edit state
			isEditing = true;
			editValue = node.name || '';
		}
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
			isEditing = false;
			// Let the global escape handler in the layout take care of navigation
		}
	}

	// Finish editing and save the result
	function finishEditing() {
		const newName = editValue.trim();
		if (newName && newName !== node.name) {
			saveTextEdit(newName);
		}
		isEditing = false;
	}

	// Handle click outside to finish editing
	function handleClickOutside(event: MouseEvent) {
		// Only if we're editing
		if (isEditing) {
			// For desktop view, check if the click is outside the input field
			// For mobile view, modal handles its own click outside
			if (editInput && !editInput.contains(event.target as Node)) {
				finishEditing();
			}
		}
	}

	// Set up and clean up event listeners when editing state changes
	$effect(() => {
		if (isEditing) {
			// Add global event listener for clicks outside
			document.addEventListener('mousedown', handleClickOutside);

			// Focus the input when it's available (for desktop view)
			if (editInput) {
				setTimeout(() => {
					editInput?.focus();
					editInput?.select();
				}, 10);
			}
		} else {
			// Remove the event listener when not editing
			document.removeEventListener('mousedown', handleClickOutside);
		}

		// Clean up function
		return () => {
			document.removeEventListener('mousedown', handleClickOutside);
		};
	});

	// Check if this node should enter edit mode (either from prop or from being newly created)
	$effect(() => {
		if (shouldEdit && !isEditing && node.id) {
			// Start editing automatically
			isEditing = true;
			editValue = node.name || '';
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
          font-size: {fontSize}rem;
          width: 100%;
          max-width: {Math.min(200, nodeSizeRatio * 3)}px;
        "
				/>
			{:else}
				<div
					class="node-title"
					style="
          font-size: {fontSize}rem;
          text-align: center;
          max-width: 100%;
        "
					title={node.name}
					onmousedown={handleTextEditStart}
					ontouchstart={handleTextEditStart}
				>
					{#each segments as segment}
						<span class="title-segment">{segment}</span>
					{/each}
				</div>
			{/if}
		</div>

		<!-- Contributors - positioned to the right of title -->
		{#if visibilityFactor > 0.1}
			<div
				class="contributors-area"
				style="
          position: absolute;
          opacity: {visibilityFactor};
          z-index: 1;
          top: 50%; 
          left: 70%; 
          transform: translateY(-50%);
        "
			>
				<div
					class="contributors-layout"
					style="
            display: flex;
            align-items: center;
            justify-content: center;
            gap: {Math.max(2, adaptiveGap * 0.5)}px;
            flex-wrap: wrap;
            flex-direction: column;
          "
				>
					<!-- Add button -->
					<button
						class="add-contributor-button"
						style="
            width: {contributorSize}px;
            height: {contributorSize}px;
            font-size: {contributorSize * 0.5}px;
          "
						onclick={handleAddContributorClick}
						title="Add contributor"
					>
						+
					</button>

					<!-- Contributors -->
					{#if hasContributors}
						{#if showFullContributorDetails}
							{#each node.contributors as contributorId}
								<TagPill
									userId={contributorId}
									{truncateLength}
									onClick={(id, e) => handleTagClick(id, e)}
									onRemove={handleRemoveContributor}
								/>
							{/each}
						{:else}
							{#each node.contributors as contributorId}
								<div
									class="mini-contributor"
									title={contributorId}
									style="
                  background: {getColorForUserId(contributorId)};
                  width: {Math.max(6, contributorSize * 0.4)}px;
                  height: {Math.max(6, contributorSize * 0.4)}px;
                "
								></div>
							{/each}
							{#if node.contributors.length > 3}
								<div
									class="more-contributors"
									title="More contributors"
									style="
                    width: {Math.max(8, contributorSize * 0.5)}px;
                    height: {Math.max(8, contributorSize * 0.5)}px;
                    font-size: {Math.max(6, contributorSize * 0.3)}px;
                  "
								>
									+{node.contributors.length - 3}
								</div>
							{/if}
						{/if}
					{/if}
				</div>
			</div>
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

	.content-cluster {
		transition: all 0.2s ease;
	}

	.node-title-area {
		user-select: none;
		-webkit-user-select: none;
		-moz-user-select: none;
		-ms-user-select: none;
		-webkit-touch-callout: none;
		-webkit-tap-highlight-color: transparent;
	}

	.node-title {
		color: rgba(0, 0, 0, 0.8);
		text-shadow:
			0px 0px 3px rgba(255, 255, 255, 0.8),
			0px 0px 2px rgba(255, 255, 255, 0.6);
		font-weight: 500;
		cursor: text;
		word-break: break-word;
		hyphens: auto;
		user-select: none;
		-webkit-user-select: none;
		-moz-user-select: none;
		-ms-user-select: none;
		-webkit-touch-callout: none;
		-webkit-tap-highlight-color: transparent;
		touch-action: manipulation;
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
	}

	.node-edit-input:focus {
		border-color: #2196f3;
		box-shadow: 0 0 0 2px rgba(33, 150, 243, 0.2);
	}

	.contributors-area {
		transition: opacity 0.3s ease;
	}

	.add-contributor-button {
		display: flex;
		align-items: center;
		justify-content: center;
		background: rgba(200, 200, 200, 0.7);
		border: none;
		border-radius: 50%;
		color: #333;
		cursor: pointer;
		transition: all 0.3s ease;
		font-family: Arial, sans-serif;
		line-height: 0;
		flex-shrink: 0;
	}

	.add-contributor-button:hover {
		background: rgba(200, 200, 200, 0.9);
		transform: scale(1.025);
	}

	.mini-contributor {
		border-radius: 50%;
		animation: fadeIn 0.2s ease-out;
		flex-shrink: 0;
	}

	.more-contributors {
		border-radius: 50%;
		background: #d1e1f0;
		color: #3a6b9e;
		display: flex;
		align-items: center;
		justify-content: center;
		font-weight: bold;
		animation: fadeIn 0.2s ease-out;
		flex-shrink: 0;
	}

	@keyframes fadeIn {
		from {
			opacity: 0;
			transform: scale(0.8);
		}
		to {
			opacity: 1;
			transform: scale(1);
		}
	}
</style>
