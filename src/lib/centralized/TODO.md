Currently our Forest is just simulating global look up of user-ids in gun

GunNode is only useful for adding to a given user's tree at the moment

For anything in calculations where we have a Forest, we should just look up the
gun.soul to find the user (including ourselves!)

still the delete functionality is not working! we get a toast that says it was successful but it doesnt dissapear visually and when i reload the page it is still there, clearly indicating it was not deleted from Gun either!

is our Tree.ts correclty using GunNode.ts for deletion? removeChild? removeSubtree?

We have addChild in tree.ts but we dont have removeChild (which should actually recursively delete the whole subtree of the child and its children etc. from the bottom up to the child

// Can we simplify tree.ts somehow and make it more elgant?

// we should mantain a contact list of all the contributors we have ever added. // Contact management // The dropdown should be based on this (not the public)

Currently:
we are doing localStorage of userData outside of gunSetup, and setting currentUser in sloppy way. This should be handled in gunSetup

Our deletion of nodes is not working, and we need to reimplement the fix we found for this (somewher in chat history)

Soul fix for contributor reloading! (perhaps we have also solved inconsistent state?)

There sue with contributors not persisting has now been fixed. Let me explain what was happening and how the solution works:
What was causing the issue
Reference handling: When you save contributors in Gun using put(), Gun sometimes creates a reference (soul) to the actual data rather than storing it inline. This is normal behavior for complex objects.
Incorrect reference resolution: Previously, the code was trying to resolve these references by constructing a path like [...nodePath, 'nodeContributors'] which doesn't correctly access the referenced data since that's not where Gun actually stored it.
The fix
I modified the code in two places (both in fetchNode and in loadNodeRecursive) to:
// Use Gun directly with the soul to get the data
const contributorsData = await new Promise<any>((resolve) => {
gun.get(soul).once((data: any) => resolve(data));
});

This approach directly uses the soul reference to load the actual contributor data from Gun's database. The soul is the unique identifier that Gun uses to locate data, regardless of where it's hierarchically organized.
By using gun.get(soul) we're going straight to the source rather than trying to traverse a path that might not exist.

After analyzing tree.ts, I can confirm that it doesn't properly support the deletion of child nodes or subtrees. Let's look at the issue:
Current Implementation Analysis
The current deletion implementation in Parent.svelte only removes the child reference from the parent's nodeChildren Map and saves the parent node. This approach has a critical flaw:
It updates the parent's references to children by removing the link to the deleted node
But it doesn't actually delete the child node data from Gun's database
This explains why the child disappears from the UI (because the parent no longer references it), but reappears when you reload (because the node data still exists in Gun, and is rediscovered during the full tree load).
Problematic Code in handleNodeDeletion

// Current approach:
const updatedChildren = new Map(currentNode.nodeChildren);
updatedChildren.delete(nodeId);

// Only updates the parent, doesn't delete the child node data
Tree.saveNode(updatedZipper)

Missing Functionality in tree.ts
The Tree class in tree.ts lacks a proper deleteNode method. There's no code path that actually removes the node data from Gun's database.
How Gun Deletion Works
In Gun, to delete data properly, you need to:
Remove references to the node from its parent(s)
Set the actual node data to null to mark it for deletion

Solution
We need to implement a proper deleteNode method in Tree. Here's how we can add it:

    /**
     * Delete a node and its children from Gun database
     * @param nodeId ID of the node to delete
     * @returns Promise resolving to true if deletion was successful
     */
    static async deleteNode(nodeId: string): Promise<boolean> {
    	console.log('[GUN FLOW] deleteNode started:', nodeId);

    	// Ensure user is authenticated
    	if (!user.is?.pub) {
    		console.error('[GUN FLOW] User not authenticated');
    		throw new Error('User must be authenticated to delete a node');
    	}

    	try {
    		// Don't allow deleting the root node
    		if (nodeId === user.is.pub) {
    			console.error('[GUN FLOW] Cannot delete root node');
    			return false;
    		}

    		// Get node reference
    		const nodePath = ['tree', 'nodes', nodeId];
    		const nodeRef = new GunNode(nodePath);

    		// Get node data to check for children
    		const nodeData = await nodeRef.once();

    		if (!nodeData) {
    			console.error('[GUN FLOW] Node not found:', nodeId);
    			return false;
    		}

    		// First, recursively delete all children
    		// Get children references
    		const childrenRef = new GunNode([...nodePath, 'children']);
    		const children = await childrenRef.once();

    		if (children && typeof children === 'object') {
    			// Process each child
    			const childPromises = Object.keys(children)
    				.filter(key => key !== '_') // Skip Gun metadata
    				.map(async (childId) => {
    					try {
    						// Extract the actual node ID from the reference
    						const childData = children[childId];
    						let actualChildId = childId;

    						if (childData && typeof childData === 'object' && childData['#']) {
    							const parts = childData['#'].split('/');
    							actualChildId = parts.length > 0 ? parts[parts.length - 1] : childId;
    						}

    						// Recursively delete this child
    						console.log(`[GUN FLOW] Deleting child ${actualChildId} of node ${nodeId}`);
    						await Tree.deleteNode(actualChildId);
    					} catch (error) {
    						console.error(`[GUN FLOW] Error deleting child ${childId}:`, error);
    					}
    				});

    			// Wait for all child deletions to complete
    			await Promise.all(childPromises);
    		}

    		// Now delete the node itself
    		console.log(`[GUN FLOW] Deleting node data for ${nodeId}`);

    		// 1. Delete children references
    		await childrenRef.put(null);

    		// 2. Delete the node data
    		await nodeRef.put(null);

    		console.log(`[GUN FLOW] Node ${nodeId} deleted successfully`);
    		return true;
    	} catch (error) {
    		console.error('[GUN FLOW] Error deleting node:', error);
    		return false;
    	}
    }

}

Now let's update the handleNodeDeletion function in Parent.svelte to use our new deleteNode method:

    // Delete a node and its children
    deleteNode: async (nodeId: string): Promise<boolean> => {
    	console.log('[FLOW] deleteNode started:', nodeId);
    	if (!globalState.currentZipper) return false;

    	try {
    		// Don't allow deleting root node
    		if (nodeId === globalState.currentPath[0]) {
    			console.error('[FLOW] Cannot delete root node');
    			globalState.showToast('Cannot delete root node', 'error');
    			return false;
    		}

    		// Check if we're trying to delete the current node
    		const isCurrentNode = globalState.currentZipper.zipperCurrent.nodeId === nodeId;

    		if (isCurrentNode) {
    			// If we're on the node to delete, navigate up to parent first
    			if (globalState.currentPath.length <= 1) {
    				console.error('[FLOW] Cannot delete current node with no parent');
    				return false;
    			}

    			// Navigate to parent
    			console.log('[FLOW] Navigating to parent before deleting current node');
    			globalState.zoomOut();
    		}

    		// Find the parent of the node to delete
    		let parentZipper = null;
    		let parentNodeId = '';

    		if (globalState.currentPath.length > 1) {
    			// If we have a path, try to find parent using the path
    			const pathIndex = globalState.currentPath.indexOf(nodeId);
    			if (pathIndex > 0) {
    				parentNodeId = globalState.currentPath[pathIndex - 1];
    				// Get parent from forest if possible
    				parentZipper = globalState.currentForest.get(parentNodeId);
    			}
    		}

    		// If we couldn't find parent in the path, assume current node is parent
    		if (!parentZipper) {
    			parentZipper = globalState.currentZipper;
    			parentNodeId = parentZipper.zipperCurrent.nodeId;
    		}

    		// Verify the node exists in parent's children
    		if (!parentZipper.zipperCurrent.nodeChildren.has(nodeId)) {
    			console.error(`[FLOW] Node ${nodeId} not found in parent's children`);
    			return false;
    		}

    		// 1. Update parent in memory by removing the child reference
    		const updatedChildren = new Map(parentZipper.zipperCurrent.nodeChildren);
    		updatedChildren.delete(nodeId);

    		const updatedParentNode = {
    			...parentZipper.zipperCurrent,
    			nodeChildren: updatedChildren
    		};

    		const updatedParentZipper = {
    			...parentZipper,
    			zipperCurrent: updatedParentNode
    		};

    		// 2. Save the updated parent to Gun
    		console.log(`[FLOW] Updating parent node ${parentNodeId} to remove child reference`);
    		await Tree.saveNode(updatedParentZipper);

    		// 3. Delete the node and all its children from Gun
    		console.log(`[FLOW] Deleting node ${nodeId} and its children from Gun`);
    		const deleteResult = await Tree.deleteNode(nodeId);

    		if (!deleteResult) {
    			console.error(`[FLOW] Failed to delete node ${nodeId} from Gun`);
    			return false;
    		}

    		// 4. Update the in-memory state
    		// If we're viewing the deleted node's parent, update the current zipper
    		if (globalState.currentZipper.zipperCurrent.nodeId === parentNodeId) {
    			globalState.currentZipper = updatedParentZipper;
    		}

    		// 5. Update the forest if parent is root
    		const rootId = globalState.currentPath[0];
    		if (rootId === parentNodeId) {
    			globalState.currentForest.set(rootId, updatedParentZipper);
    		}

    		// 6. Force an update of path info if needed
    		globalState.updatePathInfo();

    		console.log(`[FLOW] Node ${nodeId} and its children deleted successfully`);
    		return true;
    	} catch (err) {
    		console.error('[FLOW] Error in deleteNode:', err);
    		return false;
    	}
    }

    Finally, let's update the handleNodeDeletion in Parent.svelte to use our new global state method:

The Issue
The problem lies in how Gun handles object updates. When we're removing a contributor, Gun needs to be told explicitly to update the entire contributorsObj object, not just the property being removed.
Let's look at the flow of removing a contributor:
In Parent.svelte, the handleRemoveContributor function filters out the contributor ID from the array
This is passed to globalState.updateNode which correctly converts the array to a Set and saves it
In Tree.saveNode, we properly recreate the contributorsObj from the Set
The nodeContributors object is sent to Gun via nodeRef.put()
However, there's a subtle issue with how Gun handles object updates:
When the contributorsObj is saved:
If the previous version had a soul reference (#), Gun might be updating only the changed properties
When a contributor is removed, we're not explicitly telling Gun to remove that property from the referenced object

The Solution
Let's modify the saveNode method in tree.ts to ensure that when contributors are updated, Gun properly removes properties for deleted contributors:
Now let's also add additional logging to help debug contributor removal from the TagPill component:
The issue with contributor removal persistence has been fixed! Here's a summary of what we did:
Identified the Root Cause:
When contributors are stored as a soul reference in Gun (which we fixed in the earlier fix), removal of contributors wasn't updating the referenced object properly.
Gun was attempting to update only the changed properties, but wasn't correctly removing properties that no longer existed.
Implemented the Solution:
Modified saveNode to check if the node's contributors are stored as a reference.
If they are, we clear the entire reference object with put(null) before saving the new contributors.
This ensures that removed contributors are properly deleted from the database.
Added proper error handling and logging to track the process.
Enhanced Debugging:
Added additional logging to the TagPill component to better track contributor removal events.

TODO:

- Meditate
- Eat Food
- Evaluate current state of code
- make calculations simpler and remove caching of those values, like we do in haskell, use haskell as reference
- make our Gunsetup login way simpler using svelte example from gun.eco
- Can we make our existing tree.ts more elegant? Where do we repeat ourselves, or have conflicting ways of doing things
- Can we find more elegant abstractions for writing to and saving a node?
- Let's implement manual .put() of everything that is not being saved in the gun object? Or does guide, have a better way of storing collections than our :true method? I believe they used dates somehow

- Lets move the
- Lets try to implement it in Peerbit
