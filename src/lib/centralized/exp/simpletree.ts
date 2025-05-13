import Gun from 'gun';

// Initialize Gun
const gun = Gun();

// Tree node interface
interface TreeNode {
	id: string;
	name: string;
	data?: any;
}

/**
 * Create a new tree with a root node
 * @param rootName The name of the root node
 * @param rootData Optional data for the root node
 * @returns The ID of the root node
 */
export function createTree(rootName: string, rootData: any = {}): string {
	const rootId = `tree_${Date.now()}`;

	// Create the root node
	gun.get(rootId).put({
		id: rootId,
		name: rootName,
		data: rootData
	});

	return rootId;
}

/**
 * Add a child node to a parent node
 * @param parentId The ID of the parent node
 * @param childName The name of the child node
 * @param childData Optional data for the child node
 * @returns The ID of the new child node
 */
export function addChild(parentId: string, childName: string, childData: any = {}): string {
	const childId = `node_${Date.now()}_${Math.floor(Math.random() * 1000)}`;

	// Create the child node
	gun.get(childId).put({
		id: childId,
		name: childName,
		data: childData
	});

	// Link child to parent by setting the child's unique ID as a property on the parent's children node
	gun.get(parentId).get('children').get(childId).put(true);

	return childId;
}

/**
 * Get a node by its ID
 * @param nodeId The ID of the node to get
 * @returns A promise that resolves to the node data
 */
export function getNode(nodeId: string): Promise<TreeNode> {
	return new Promise((resolve) => {
		gun.get(nodeId).once((node: TreeNode) => {
			resolve(node);
		});
	});
}

/**
 * Get all children of a node
 * @param nodeId The ID of the parent node
 * @returns A promise that resolves to an array of child nodes
 */
export function getChildren(nodeId: string): Promise<TreeNode[]> {
	return new Promise((resolve) => {
		const children: TreeNode[] = [];

		// Use map() to iterate over all child IDs in the set
		gun
			.get(nodeId)
			.get('children')
			.map()
			.once((value, key) => {
				if (value === true && key) {
					gun.get(key).once((child: TreeNode) => {
						if (child) {
							children.push(child);
						}
					});
				}
			});

		// Gun doesn't have a built-in way to know when all children are loaded
		// This is a simple timeout-based approach
		setTimeout(() => {
			resolve(children);
		}, 100);
	});
}

/**
 * Update a node's data
 * @param nodeId The ID of the node to update
 * @param data The new data to merge with existing data
 */
export function updateNode(nodeId: string, data: any): void {
	gun.get(nodeId).once((node: TreeNode) => {
		const updatedData = { ...node.data, ...data };
		gun.get(nodeId).put({ data: updatedData });
	});
}

/**
 * Delete a node and all its children
 * @param nodeId The ID of the node to delete
 */
export function deleteNode(nodeId: string): void {
	// First recursively delete all children
	getChildren(nodeId).then((children) => {
		children.forEach((child) => {
			deleteNode(child.id);
		});

		// Remove all child references
		gun
			.get(nodeId)
			.get('children')
			.map()
			.once((val, key) => {
				if (key) {
					gun.get(nodeId).get('children').get(key).put(null);
				}
			});

		// Then null out the node itself (Gun doesn't fully delete data)
		gun.get(nodeId).put(null);
	});
}

/**
 * Example usage:
 *
 * // Create a new tree with a root node
 * const rootId = createTree('Root Node', { description: 'This is the root' });
 *
 * // Add children to the root
 * const childId1 = addChild(rootId, 'Child 1', { value: 42 });
 * const childId2 = addChild(rootId, 'Child 2', { value: 84 });
 *
 * // Add grandchildren
 * const grandchildId = addChild(childId1, 'Grandchild', { value: 21 });
 *
 * // Get a node and its children
 * getNode(rootId).then(rootNode => console.log('Root node:', rootNode));
 * getChildren(rootId).then(children => console.log('Root children:', children));
 */
