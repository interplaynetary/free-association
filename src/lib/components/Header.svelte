<script lang="ts">
	import { globalState } from '$lib/global.svelte';

	const store = $derived(globalState.recStore);
	let name = $derived(store.nameStore);
	let hasDirectContributionChild = $derived(store.hasContributorsStore);

	// Load the path names from the store
	async function loadPathNames() {
		let pathNames: string[] = [];
		// Get the path from the current node to the root
		const pathToRoot = await store.getPathToRoot();
		console.log('pathToRoot', pathToRoot);

		// Extract names in reverse order (from root to current node)
		const names = pathToRoot
			.map((item: { name: string; id: string }) => item.name || item.id)
			.reverse()
			.filter(Boolean); // Filter out empty names

		pathNames = names;
		return pathNames;
	}

	let path = $derived.by(() => {
		name;
		return loadPathNames();
	});
</script>

<div class="header-content">
	<div class="node-name">
		<div class="breadcrumbs">
			{#await path}
				<div class="loading-path">Loading...</div>
			{:then path}
				{#each path as segment, index}
					{#if index > 0}
						<div class="breadcrumb-separator">/</div>
					{/if}
					<a
						href="/"
						class="breadcrumb-item"
						class:current={index === path.length - 1}
						onclick={() => globalState.navigateToPathInIndex(index)}
						tabindex="0"
						aria-label={segment}
					>
						{segment}
					</a>
				{/each}
			{/await}
		</div>
	</div>
	<div class="header-controls">
		<a href="/inventory" class="icon-button inventory-button" title="View inventory"
			><span>📊</span></a
		>
		<a href="/login" class="icon-button peer-button" title="User login"><span>🔍</span></a>
		<button
			class="icon-button add-button"
			title="Add new node"
			onclick={() => globalState.handleAddNode()}><span>➕</span></button
		>
		<button
			class="icon-button delete-button"
			title="Toggle delete mode"
			onclick={() => globalState.toggleDeleteMode()}><span>🗑️</span></button
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
