<script lang="ts">
	import { onMount } from 'svelte';
	import * as d3 from 'd3';
	import { currentPath } from '$lib/global.svelte';
	import { type CollectiveNode, type CollectiveTree } from '$lib/schema';
	import { findCollectiveNodeById } from '$lib/collective.svelte';
	import Child from '$lib/components/collective/Child.svelte';

	// Props for the collective tree
	let { collectiveTree, onNavigate = (nodeId: string) => {} } = $props<{
		collectiveTree: CollectiveTree;
		onNavigate?: (nodeId: string) => void;
	}>();

	// Simple visualization data structure
	interface VisualizationNode {
		id: string;
		weight: number;
		children: VisualizationNode[];
		nodeName: string;
		contributors: string[];
		hasChildren: boolean;
		contributorWeights: Record<string, number>;
	}

	// Reactive path for navigation
	const path = $derived($currentPath);

	// Current node ID from path
	const currentNodeId = $derived.by(() => {
		if (path.length === 0) {
			return collectiveTree.root.id;
		}
		return path[path.length - 1];
	});

	// Get current node from collective tree
	const currentNode = $derived.by(() => {
		if (!collectiveTree || !currentNodeId) return null;
		return findCollectiveNodeById(collectiveTree.root, currentNodeId);
	});

	// Get child nodes
	const childNodes = $derived.by(() => {
		return currentNode ? currentNode.children : [];
	});

	// Format data for d3 visualization
	const packData = $derived.by(() => {
		if (!currentNode) {
			return { id: '', weight: 0, children: [] };
		}

		const mappedChildren = childNodes.map((child) => ({
			id: child.id,
			weight: child.type === 'CollectiveNonRootNode' ? child.weight_percentage : 1.0,
			nodeName: child.name,
			contributors:
				child.type === 'CollectiveNonRootNode'
					? child.contributor_ids
					: collectiveTree.contributors,
			contributorWeights:
				child.type === 'CollectiveNonRootNode'
					? child.source_contributors
					: collectiveTree.root.contributor_weights,
			children: [] as VisualizationNode[],
			hasChildren: child.children.length > 0
		}));

		return {
			id: currentNode.id,
			weight: currentNode.type === 'CollectiveNonRootNode' ? currentNode.weight_percentage : 1.0,
			children: mappedChildren
		};
	});

	// Create d3 hierarchy for layout
	const hierarchyData = $derived.by(() => {
		const data = packData;

		const rootNode: VisualizationNode = {
			id: data.id,
			weight: 0,
			children: data.children,
			nodeName: 'Root',
			contributors: [],
			hasChildren: data.children.length > 0,
			contributorWeights: {}
		};

		const hierarchy = d3.hierarchy<VisualizationNode>(rootNode, (d) => d.children);
		hierarchy.sum((d) => d.weight || 0.1);
		hierarchy.sort((a, b) => b.value! - a.value!);

		// Apply treemap layout
		const treemap = d3
			.treemap<VisualizationNode>()
			.size([1, 1])
			.paddingInner(0.003)
			.paddingOuter(0.002)
			.round(false);

		return treemap(hierarchy);
	});

	// Simple navigation function
	function zoomInto(nodeId: string) {
		onNavigate(nodeId);
	}
</script>

<div class="collective-container">
	<div class="treemap-container">
		{#if hierarchyData && hierarchyData.children && hierarchyData.children.length > 0}
			{#each hierarchyData.children as child}
				<div
					class="collective-node"
					data-node-id={child.data.id}
					style="
							position: absolute;
							left: {child.x0 * 100}%;
							top: {child.y0 * 100}%;
							width: {(child.x1 - child.x0) * 100}%;
							height: {(child.y1 - child.y0) * 100}%;
						cursor: pointer;
					"
					role="button"
					tabindex="0"
					onclick={() => zoomInto(child.data.id)}
					onkeydown={(e) => {
						if (e.key === 'Enter' || e.key === ' ') {
							e.preventDefault();
							zoomInto(child.data.id);
						}
					}}
				>
					<Child
						node={{
							id: child.data.id,
							name: child.data.nodeName,
							weight: child.data.weight,
							contributors: child.data.contributors,
							contributorWeights: child.data.contributorWeights,
							hasChildren: child.data.hasChildren
						}}
						dimensions={{
							x0: child.x0,
							y0: child.y0,
							x1: child.x1,
							y1: child.y1
						}}
					/>
				</div>
			{/each}
		{:else}
			<div class="empty-state">
				<span class="info-text">No collective data available</span>
			</div>
		{/if}
	</div>
</div>

<style>
	.collective-container {
		width: 100%;
		height: 100%;
		position: relative;
	}

	.treemap-container {
		width: 100%;
		height: 100%;
		position: absolute;
		top: 0;
		left: 0;
	}

	.collective-node {
		user-select: none;
		transition: opacity 0.2s ease;
	}

	.collective-node:hover {
		opacity: 0.8;
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

	.info-text {
		color: #666;
		font-size: 16px;
		font-weight: 500;
	}
</style>
