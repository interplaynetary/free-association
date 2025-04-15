<script lang="ts">
  import { onMount, onDestroy } from 'svelte';
  import { get } from 'svelte/store';
  import { createRec, type RecognitionStore, type NodeEntry } from '../stores/rec';
  import TreeMap from './TreeMap.svelte';
  import { TreeNode } from '../models/TreeNode';
  
  // Core props
  export let path: string[] = ['recognition'];
  export let height = '100vh';
  export let showDebug = false; // Add option to hide debug info
  
  // Internal state
  let recStore: RecognitionStore;
  let loading = true;
  let error: string | null = null;
  let treeMapComponent: any;
  let treeMapData: TreeNode;
  
  // Viewport dimensions
  let viewportWidth = 0;
  let viewportHeight = 0;
  
  // Subscriptions to clean up
  let unsubscribeChildren: (() => void) | undefined;
  let unsubscribeContributors: (() => void) | undefined;
  let unsubscribeTags: (() => void) | undefined;
  let unsubscribeFulfillment: (() => void) | undefined;
  
  onMount(async () => {
    try {
      loading = true;
      
      // Set initial viewport dimensions
      viewportWidth = window.innerWidth;
      viewportHeight = window.innerHeight;
      
      // Get the recognition store directly
      recStore = createRec(path);
      
      // Initialize the tree data
      await updateTreeMapData();
      
      // Set up subscriptions to keep TreeMap data updated
      unsubscribeChildren = recStore.childrenStore.subscribe(() => updateTreeMapData());
      unsubscribeContributors = recStore.contributorsStore.subscribe(() => updateTreeMapData());
      unsubscribeTags = recStore.tagsStore.subscribe(() => updateTreeMapData());
      unsubscribeFulfillment = recStore.fulfillmentStore.subscribe(() => updateTreeMapData());
      
      loading = false;
    } catch (err) {
      console.error('Error initializing recognition component:', err);
      error = err instanceof Error ? err.message : 'Unknown error';
      loading = false;
    }
  });
  
  onDestroy(() => {
    // Clean up subscriptions
    if (unsubscribeChildren) unsubscribeChildren();
    if (unsubscribeContributors) unsubscribeContributors();
    if (unsubscribeTags) unsubscribeTags();
    if (unsubscribeFulfillment) unsubscribeFulfillment();
    
    // Clean up TreeMap
    if (treeMapComponent && typeof treeMapComponent.destroy === 'function') {
      treeMapComponent.destroy();
    }
    
    // Remove resize listener
    window.removeEventListener('resize', handleResize);
  });
  
  async function updateTreeMapData() {
    console.log('Recognition: Updating TreeMap data');
    
    if (!recStore) return;
    
    try {
      // Get data from store
      const name = get(recStore.nameStore) || 'Recognition';
      const points = get(recStore.pointsStore) || 0;
      const children = get(recStore.childrenStore) || [];
      const contributors = get(recStore.contributorsStore) || [];
      const tags = get(recStore.tagsStore) || [];
      const fulfillment = get(recStore.fulfillmentStore) || 0;
      const isContribution = get(recStore.isContributionStore) || false;
      
      // Create the root node using fromRecStore
      const rootData = {
        name,
        points,
        isContribution,
        fulfilled: fulfillment
      };
      
      const root = TreeNode.fromRecStore(rootData, recStore.path[recStore.path.length - 1]);
      
      // Add children using fromRecStore for each child
      children.forEach(([childId, childData]) => {
        if (!childData || !childData.name) return;
        
        const childNode = TreeNode.fromRecStore(childData, childId);
        root.addChild(childId, childNode);
      });
      
      // Add contributors
      contributors.forEach(([contributorId, contributorData]) => {
        if (!contributorData) return;
        
        // Create a minimal node for contributors
        const contributorNode = new TreeNode(contributorData.name || contributorId);
        contributorNode.id = contributorId;
        
        if (contributorData.metadata) {
          contributorNode.metadata = { ...contributorData.metadata };
        }
        
        root.addContributor(contributorId, contributorNode);
      });
      
      // Add tags
      tags.forEach(([tag]) => {
        root.addTag(tag);
      });
      
      console.log('Recognition: TreeMap data created', { 
        root: root, 
        childCount: root.children.size,
        contributorCount: root.contributors.size,
        tagCount: root.tags.length
      });
      
      treeMapData = root;
    } catch (err) {
      console.error('Error updating TreeMap data:', err);
    }
  }
  
  function handleTreeMapNodeClick(node: TreeNode) {
    if (!node.id || node === treeMapData) return;
    
    // Navigate to child by updating path
    const newPath = [...path, 'children', node.id];
    path = newPath;
  }
  
  // Handle window resize
  function handleResize() {
    viewportWidth = window.innerWidth;
    viewportHeight = window.innerHeight;
  }
  
  // Set up resize listener
  onMount(() => {
    window.addEventListener('resize', handleResize);
  });
</script>

<svelte:window on:resize={handleResize} />

<div class="recognition-container" style="height: {height};">
  {#if loading}
    <div class="loading">Loading recognition data...</div>
  {:else if error}
    <div class="error">{error}</div>
  {:else if recStore && treeMapData}
    {#if showDebug}
      <div class="debug-info">
        <div>Store: {recStore.path.join('/')}</div>
        <div>Children: {treeMapData.children.size}</div>
        <div>Contributors: {treeMapData.contributors.size}</div>
        <div>Tags: {treeMapData.tags.length}</div>
        <div>Fulfillment: {Math.round(treeMapData.calculateFulfillment() * 100)}%</div>
      </div>
    {/if}
    <TreeMap 
      bind:this={treeMapComponent}
      data={treeMapData}
      store={recStore}
      width={viewportWidth} 
      height={viewportHeight}
      on:nodeClick={event => handleTreeMapNodeClick(event.detail)}
    />
  {:else}
    <div class="empty">No recognition data available</div>
  {/if}
</div>

<style>
  :global(body) {
    margin: 0;
    padding: 0;
    overflow: hidden;
  }
  
  .recognition-container {
    position: fixed;
    top: 0;
    left: 0;
    width: 100vw;
    height: 100vh;
    overflow: hidden;
    margin: 0;
    padding: 0;
  }
  
  .loading, .error, .empty {
    display: flex;
    align-items: center;
    justify-content: center;
    height: 100%;
    width: 100%;
    font-size: 1.2rem;
    color: #666;
  }
  
  .error {
    color: #d32f2f;
    padding: 1rem;
  }
  
  .debug-info {
    position: absolute;
    top: 0;
    left: 0;
    background: rgba(0,0,0,0.7);
    color: white;
    padding: 5px 10px;
    font-size: 12px;
    z-index: 100;
  }
  
  :global(.recognition-container svg) {
    width: 100vw !important;
    height: 100vh !important;
    display: block;
  }
</style>
