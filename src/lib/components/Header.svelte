<script lang="ts">
	import { globalState } from '$lib/global.svelte';

	// Get current zipper and path from global state
	let currentZipper = $derived(globalState.currentZipper);
	let currentPath = $derived(globalState.currentPath);

	// Get current node name
	let currentNodeName = $derived(
		!currentZipper
			? 'Loading...'
			: currentZipper.zipperCurrent.nodeName || currentZipper.zipperCurrent.nodeId
	);

	// Extract path names from the path IDs
	let pathNames = $derived(() => {
		if (!currentZipper || !currentPath.length) return [] as string[];

		// We'll build the path names by going up the tree
		const names: string[] = [];
		let zipper = currentZipper;

		// Add the current node name
		names.unshift(currentNodeName);

		// Go up the tree for each path element (except the current one)
		for (let i = 0; i < currentPath.length - 1; i++) {
			const parentZipper = zipper.zipperContext;
			if (!parentZipper) break;

			// Get parent name
			const parentName = parentZipper.ctxParent.nodeName || parentZipper.ctxParent.nodeId;
			names.unshift(parentName);

			// Continue up the tree
			zipper = {
				zipperCurrent: parentZipper.ctxParent,
				zipperContext: i > 0 ? parentZipper.ctxAncestors[0] : null
			};
		}

		return names;
	});

	// Check if current node has direct contribution children
	let hasDirectContributionChild = $derived(() => {
		if (!currentZipper) return false;

		// Check if any child has contributors
		for (const child of currentZipper.zipperCurrent.nodeChildren.values()) {
			if (child.nodeContributors.size > 0) return true;
		}

		return false;
	});
</script>

<div class="header-content">
	<div class="node-name">
		<div class="breadcrumbs">
			{#if pathNames().length === 0}
				<div class="loading-path">Loading...</div>
			{:else}
				{#each pathNames() as segment, index}
					{#if index > 0}
						<div class="breadcrumb-separator">/</div>
					{/if}
					<a
						href="/"
						class="breadcrumb-item"
						class:current={index === pathNames().length - 1}
						onclick={(e) => globalState.navigateToPathIndex(index)}
						tabindex="0"
						aria-label={segment}
					>
						{segment}
					</a>
				{/each}
			{/if}
		</div>
	</div>
	<div class="header-controls">
		<a href="/inventory" class="icon-button inventory-button" title="View inventory"
			><span>📊</span></a
		>
		<a href="/login" class="icon-button peer-button" title="User login"><span>🔍</span></a>
		<button class="icon-button add-button" title="Add new node" onclick={globalState.handleAddNode}
			><span>➕</span></button
		>
		<button
			class="icon-button delete-button"
			title="Toggle delete mode"
			onclick={globalState.toggleDeleteMode}><span>🗑️</span></button
		>
	</div>
</div>

<style>
	.header-content {
		width: 100%;
		display: flex;
		justify-content: space-between;
		align-items: center;
	}

	.node-name {
		display: flex;
		align-items: center;
		font-weight: bold;
		user-select: none;
		overflow: hidden;
		max-width: calc(100% - 180px);
	}

	/* Breadcrumb styles */
	.breadcrumbs {
		display: flex;
		align-items: center;
		flex-wrap: nowrap;
		overflow-x: auto;
		scrollbar-width: none; /* Firefox */
		-ms-overflow-style: none; /* IE and Edge */
		max-width: 100%;
		padding-bottom: 4px;
	}

	.breadcrumbs::-webkit-scrollbar {
		display: none; /* Chrome, Safari, Opera */
	}

	.breadcrumb-item {
		white-space: nowrap;
		padding: 3px 5px;
		border-radius: 4px;
		cursor: pointer;
		transition: background-color 0.2s;
		font-size: 1.95em;
	}

	.breadcrumb-item:hover {
		background-color: rgba(255, 255, 255, 0.2);
	}

	.breadcrumb-item.current {
		font-weight: bold;
		color: #2196f3;
	}

	.breadcrumb-separator {
		margin: 0 5px;
		color: #888;
		font-size: 1.8em;
	}

	.loading-path {
		color: #888;
		font-style: italic;
		font-size: 1.95em;
	}

	.header-controls {
		display: flex;
		gap: 10px;
		align-items: center;
	}

	.icon-button {
		background: none;
		border: none;
		font-size: 20px;
		padding: 0;
		width: 30px;
		height: 30px;
		display: flex;
		align-items: center;
		justify-content: center;
		transition: transform 0.1s ease;
	}

	.icon-button:hover {
		transform: scale(1.1);
	}
</style>
