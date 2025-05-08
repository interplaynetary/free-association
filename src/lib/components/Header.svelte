<script lang="ts">
	import { globalState } from '$lib/global.svelte';
	import { enterChild, exitToParent } from '$lib/centralized';

	// Get current zipper, path and pathInfo from global state
	let currentZipper = $derived(globalState.currentZipper);
	let currentPath = $derived(globalState.currentPath);
	let pathInfo = $derived(globalState.pathInfo);

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
			{#if pathInfo.length === 0}
				<div class="loading-path">Loading...</div>
			{:else}
				{#each pathInfo as segment, index}
					{#if index > 0}
						<div class="breadcrumb-separator">/</div>
					{/if}
					<a
						href={`/${segment.id}${segment.name ? ':' + segment.name.replace(/\s+/g, '-').toLowerCase() : ''}`}
						class="breadcrumb-item"
						class:current={index === pathInfo.length - 1}
						onclick={(e) => {
							globalState.navigateToPathIndex(index);
						}}
						tabindex="0"
						aria-label={segment.name}
					>
						{segment.name}
					</a>
				{/each}
			{/if}
		</div>
	</div>
	<!--
	<div class="debug-info">
		<span class="debug-path">Path: {currentPath.join(' → ')}</span>
	</div> -->

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

	.debug-info {
		font-size: 0.7rem;
		color: #999;
		text-align: center;
		position: absolute;
		top: 48px;
		left: 50%;
		transform: translateX(-50%);
		background: rgba(0, 0, 0, 0.05);
		padding: 2px 6px;
		border-radius: 4px;
		max-width: 80%;
		white-space: nowrap;
		overflow: hidden;
		text-overflow: ellipsis;
	}

	.debug-path {
		overflow: hidden;
		text-overflow: ellipsis;
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
