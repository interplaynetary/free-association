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
		// Prevent the click from bubbling up to the parent node
		event.stopPropagation();
		event.preventDefault();

		// Only allow editing if we have a valid node ID
		const nodeId = node.id;
		if (!nodeId) return;

		// Set up edit state
		isEditing = true;
		editValue = node.name || '';
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
    display: flex;
    flex-direction: column;
  "
>
	<div
		class="node-content"
		style="
      width: 100%;
      height: 100%;
      display: flex;
      flex-direction: column;
      box-sizing: border-box;
      padding: {isOnlyChild ? '2%' : '5%'};
    "
	>
		<!-- Empty space above title -->
		<div style="flex: 1;"></div>

		<!-- Title section with auto height -->
		<div class="node-body" style="flex: 0 0 auto;">
			<!-- Node text with responsive editing based on screen size -->
			{#if isEditing}
				<div class="node-text-edit-container hidden md:block" style="font-size: {fontSize}rem;">
					<input
						type="text"
						class="node-text-edit-input rounded border border-gray-300 bg-white/90 px-2 py-1 text-center shadow-sm focus:ring-2 focus:ring-blue-500 focus:outline-none"
						bind:this={editInput}
						bind:value={editValue}
						onkeydown={handleEditKeydown}
						onblur={finishEditing}
						style="
              font-size: inherit;
              width: 100%;
              max-width: {Math.min(200, nodeSizeRatio * 3)}px;
            "
					/>
				</div>
			{:else}
				<div
					class="node-text edit-text-field cursor-text"
					style="
            user-select: none;
            display: flex;
            flex-direction: column;
            align-items: center;
            font-size: {fontSize}rem;
          "
					title={node.name}
					onclick={handleTextEditStart}
				>
					{#each segments as segment, i}
						<span
							class="text-segment"
							onclick={(e) => {
								e.stopPropagation();
								handleTextEditStart(e);
							}}
						>
							{segment}
						</span>
					{/each}
				</div>
			{/if}
		</div>

		<!-- Contributors section takes remaining space -->
		<div
			class="contributor-container"
			style="
        flex: 1;
        display: flex;
        min-height: 0;
        opacity: {visibilityFactor};
        visibility: {visibilityFactor > 0.1 ? 'visible' : 'hidden'};
        margin-top: {Math.max(4, nodeSizeRatio * 0.05)}px;
    "
		>
			<div
				class="contributor-layout"
				class:solo={!hasContributors}
				style="
					flex: 1;
					display: flex;
					align-items: stretch;
					gap: 8px;
					min-height: 0;
				"
			>
				<div
					class="button-container"
					class:has-tags={hasContributors}
					style="
						width: {buttonWidth}%;
						display: flex;
						align-items: stretch;
						min-height: 0;
					"
				>
					<button
						class="add-contributor-button"
						style="
							width: 100%;
							height: 100%;
							aspect-ratio: 1;
							border-radius: 4px;
							font-size: {buttonFontSize}px;
						"
						onclick={handleAddContributorClick}
						title="Add contributor"
					>
						+
					</button>
				</div>

				{#if hasContributors}
					<div
						class="tag-container"
						style="
							width: {remainingWidth}%;
							display: flex;
							flex-wrap: wrap;
							align-items: center;
							align-content: center;
							min-height: 0;
							min-width: 0;
							max-width: {remainingWidth}%;
							height: 100%;
							gap: 8px;
						"
					>
						{#if showFullContributorDetails}
							{#each node.contributors as contributorId}
								<div class="tag-wrapper-item">
									<TagPill
										userId={contributorId}
										{truncateLength}
										onClick={(id, e) => handleTagClick(id, e)}
										onRemove={handleRemoveContributor}
									/>
								</div>
							{/each}
						{:else}
							{#each node.contributors.slice(0, 3) as contributorId, i}
								<div
									class="contributor-tag-mini"
									title={contributorId}
									style="background: {getColorForUserId(contributorId)};"
								></div>
							{/each}
							{#if node.contributors.length > 3}
								<div class="contributor-more" title="More contributors">
									+{node.contributors.length - 3}
								</div>
							{/if}
						{/if}
					</div>
				{/if}
			</div>
		</div>
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

	.node-content {
		text-align: center;
	}

	.node-body {
		display: flex;
		flex-direction: column;
		align-items: center;
		gap: 4px;
		max-width: 100%;
		padding: 5%;
	}

	.node-text {
		color: rgba(0, 0, 0, 0.8);
		text-shadow:
			0px 0px 3px rgba(255, 255, 255, 0.8),
			0px 0px 2px rgba(255, 255, 255, 0.6);
		font-weight: 500;
		overflow: hidden;
		max-width: 100%;
	}

	.text-segment {
		line-height: 1.2;
		padding: 1px 0;
	}

	:global(.edit-text-field) {
		cursor: text;
	}

	.node-text-edit-container {
		display: flex;
		justify-content: center;
		align-items: center;
		z-index: 100;
	}

	.node-text-edit-input {
		background: rgba(255, 255, 255, 0.9);
		border: 1px solid rgba(0, 0, 0, 0.2);
		border-radius: 4px;
		padding: 4px;
		text-align: center;
		box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1);
		color: #333;
	}

	.contributor-container {
		width: 100%;
		overflow: visible;
		transition: opacity 0.3s ease;
	}

	.contributor-layout {
		display: flex;
		width: 100%;
	}

	.contributor-layout.solo {
		justify-content: center;
	}

	.button-container {
		transition: all 0.3s ease;
	}

	.add-contributor-button {
		display: flex;
		align-items: center;
		justify-content: center;
		background: rgba(200, 200, 200, 0.7);
		line-height: 0; /* Ensure the line height doesn't affect centering */
		color: #333;
		cursor: pointer;
		border: none;
		padding: 0;
		transition: all 0.3s ease;
		z-index: 5;
		font-family: Arial, sans-serif; /* Consistent font for the plus sign */
	}

	.add-contributor-button:hover {
		background: rgba(200, 200, 200, 0.9);
		transform: scale(1.025);
	}

	.tag-container {
		display: flex;
		overflow: hidden;
	}

	.tag-wrapper-item {
		display: flex;
		align-items: center;
		min-width: 0;
	}

	.tag-wrapper-item :global(div) {
		min-width: 0;
	}

	.contributor-tag-mini {
		width: 10px;
		height: 10px;
		border-radius: 50%;
		display: inline-block;
		animation: fadeIn 0.2s ease-out;
		margin: 0 2px;
	}

	.contributor-more {
		width: 14px;
		height: 14px;
		border-radius: 50%;
		background: #d1e1f0;
		color: #3a6b9e;
		display: flex;
		align-items: center;
		justify-content: center;
		font-weight: bold;
		font-size: 9px;
		animation: fadeIn 0.2s ease-out;
		margin: 0 2px;
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
