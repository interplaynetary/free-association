// TypeScript implementation of the Haskell free-association model

// Tree structure for hierarchical recognition model
class Node {
  nodeId: string;
  nodeName: string;
  points: number;
  nodeParent: Node | null;
  nodeChildren: Map<string, Node>;
  nodeTypes: Set<string>; // Types are referenced by their ID
  nodeManualFulfillment: number | null;

  constructor(
    id: string,
    name: string,
    points: number,
    parent: Node | null,
    types: string[],
    manualFulfillment: number | null
  ) {
    this.nodeId = id;
    this.nodeName = name;
    this.points = points;
    this.nodeParent = parent;
    this.nodeChildren = new Map<string, Node>();
    this.nodeTypes = new Set<string>(types);
    this.nodeManualFulfillment = this.validateManualFulfillment(manualFulfillment);
  }

  // Ensure manual fulfillment is between 0.0 and 1.0
  private validateManualFulfillment(value: number | null): number | null {
    if (value === null) return null;
    if (value < 0.0) return 0.0;
    if (value > 1.0) return 1.0;
    return value;
  }
}

// Type alias for TypeIndex to track instances of each type
type TypeIndex = Map<string, Set<Node>>;

// Check if a node is a contributor (root node)
function isContributor(node: Node): boolean {
  return node.nodeParent === null;
}

// === Standardized Tree Operations ===

// Apply a function to each node in the tree
function mapTree(f: (node: Node) => Node, node: Node): Node {
  const updatedNode = f(node);
  const updatedChildren = new Map<string, Node>();
  
  updatedNode.nodeChildren.forEach((child, key) => {
    updatedChildren.set(key, mapTree(f, child));
  });
  
  updatedNode.nodeChildren = updatedChildren;
  return updatedNode;
}

// Fold a tree from the bottom up
function foldTree<T>(f: (node: Node, acc: T) => T, initial: T, node: Node): T {
  // Start with the accumulated value
  let accumulator = initial;
  
  // Apply the fold to each child node, starting with our initial value
  for (const child of node.nodeChildren.values()) {
    accumulator = foldTree(f, accumulator, child);
  }
  
  // Finally, apply f to this node and the accumulated result from children
  return f(node, accumulator);
}

// Filter children of a node
function filterNodeChildren(pred: (node: Node) => boolean, node: Node): Node[] {
  return Array.from(node.nodeChildren.values()).filter(pred);
}

// Test if any child satisfies a predicate
function anyNodeChild(pred: (node: Node) => boolean, node: Node): boolean {
  return Array.from(node.nodeChildren.values()).some(pred);
}

// Sum a value over the children of a node
function sumOverChildren(f: (node: Node) => number, node: Node): number {
  return Array.from(node.nodeChildren.values()).reduce((sum, child) => sum + f(child), 0);
}

// === End of Standardized Tree Operations ===

// Safely get instances of a type from TypeIndex
function getInstances(typeIndex: TypeIndex, typeId: string): Set<Node> {
  return typeIndex.get(typeId) || new Set<Node>();
}

// Safely get a type node from a type ID, returns undefined to handle empty sets
function getTypeNode(typeIndex: TypeIndex, typeId: string): Node | undefined {
  const instances = getInstances(typeIndex, typeId);
  return instances.size > 0 ? Array.from(instances)[0] : undefined;
}

// Check if a node is a contribution (has contributor types and a parent)
function isContribution(typeIndex: TypeIndex, tree: Node): boolean {
  if (tree.nodeTypes.size === 0 || tree.nodeParent === null) return false;
  
  // Check if any of the node's types has a contributor
  return Array.from(tree.nodeTypes).some(typeId => {
    const typeInstances = getInstances(typeIndex, typeId);
    return Array.from(typeInstances).some(isContributor);
  });
}

// Add a node to the type index
function addNodeToTypeIndex(node: Node, typeId: string, typeIndex: TypeIndex): TypeIndex {
  if (!typeIndex.has(typeId)) {
    typeIndex.set(typeId, new Set<Node>());
  }
  typeIndex.get(typeId)!.add(node);
  return typeIndex;
}

// Remove a node from a specific type in the index
function removeNodeFromType(node: Node, typeId: string, typeIndex: TypeIndex): TypeIndex {
  const instances = typeIndex.get(typeId);
  if (instances) {
    instances.delete(node);
  }
  return typeIndex;
}

// Add a child to a tree node
function addChild(
  typeIndex: TypeIndex,
  parent: Node,
  childId: string,
  childName: string,
  childPoints: number,
  childTypes: string[],
  childManualFulfillment: number | null
): { result: [Node, TypeIndex] | null; error: string | null } {
  // Check if parent is a contribution (has a parent and is a contribution type)
  if (parent.nodeParent !== null && isContribution(typeIndex, parent)) {
    return {
      result: null,
      error: `Node ${parent.nodeName} is an instance of a contributor/contribution and cannot have children.`
    };
  }

  // Validate manual fulfillment
  const validatedManualFulfillment = childManualFulfillment === null ? null
    : childManualFulfillment < 0.0 ? 0.0
    : childManualFulfillment > 1.0 ? 1.0
    : childManualFulfillment;

  // Create the child node
  const child = new Node(
    childId,
    childName,
    childPoints,
    parent,
    childTypes,
    validatedManualFulfillment
  );

  // Add child to parent
  parent.nodeChildren.set(childId, child);

  // Update type index with child's types
  let updatedTypeIndex = typeIndex;
  for (const typeId of childTypes) {
    updatedTypeIndex = addNodeToTypeIndex(child, typeId, updatedTypeIndex);
  }

  return { result: [parent, updatedTypeIndex], error: null };
}

// Remove a child from a tree node
function removeChild(
  typeIndex: TypeIndex,
  parent: Node,
  childId: string
): [Node, TypeIndex] {
  // If child doesn't exist, return unchanged parent and typeIndex
  if (!parent.nodeChildren.has(childId)) {
    return [parent, typeIndex];
  }

  const child = parent.nodeChildren.get(childId)!;
  
  // Remove child from parent
  parent.nodeChildren.delete(childId);
  
  // Update type index by removing child from all its types
  let updatedTypeIndex = typeIndex;
  for (const typeId of child.nodeTypes) {
    updatedTypeIndex = removeNodeFromType(child, typeId, updatedTypeIndex);
  }
  
  return [parent, updatedTypeIndex];
}

// Add a type to a node
function addType(
  typeIndex: TypeIndex,
  node: Node,
  typeId: string
): [Node, TypeIndex] {
  // If node already has this type, return unchanged
  if (node.nodeTypes.has(typeId)) {
    return [node, typeIndex];
  }
  
  // Add type to node
  node.nodeTypes.add(typeId);
  
  // Update type index
  const updatedTypeIndex = addNodeToTypeIndex(node, typeId, typeIndex);
  
  return [node, updatedTypeIndex];
}

// Remove a type from a node
function removeType(
  typeIndex: TypeIndex,
  node: Node,
  typeId: string
): [Node, TypeIndex] {
  // If node doesn't have this type, return unchanged
  if (!node.nodeTypes.has(typeId)) {
    return [node, typeIndex];
  }
  
  // Remove type from node
  node.nodeTypes.delete(typeId);
  
  // Update type index
  const updatedTypeIndex = removeNodeFromType(node, typeId, typeIndex);
  
  return [node, updatedTypeIndex];
}

// Get a node's total child points
function totalChildPoints(node: Node): number {
  return Array.from(node.nodeChildren.values())
    .reduce((sum, child) => sum + child.points, 0);
}

// Calculate a node's weight (proportional importance in the tree)
function weight(node: Node): number {
  if (node.nodeParent === null) {
    return 1.0; // Root node has weight 1.0
  }
  
  const parent = node.nodeParent;
  const parentTotalPoints = totalChildPoints(parent);
  
  if (parentTotalPoints === 0) {
    return 0.0;
  } else {
    return (node.points / parentTotalPoints) * weight(parent);
  }
}

// Calculate a node's share of its parent's total points
function shareOfParent(node: Node): number {
  if (node.nodeParent === null) {
    return 1.0; // Root node has 100% share
  }
  
  const parent = node.nodeParent;
  const parentTotalPoints = totalChildPoints(parent);
  
  if (parentTotalPoints === 0) {
    return 0.0;
  } else {
    return node.points / parentTotalPoints;
  }
}

// Check if a node has direct contribution children
function hasDirectContributionChild(typeIndex: TypeIndex, node: Node): boolean {
  return anyNodeChild(child => isContribution(typeIndex, child), node);
}

// Check if a node has non-contribution children
function hasNonContributionChild(typeIndex: TypeIndex, node: Node): boolean {
  return anyNodeChild(child => !isContribution(typeIndex, child), node);
}

// Calculate the proportion of total child points from contribution children
function contributionChildrenWeight(typeIndex: TypeIndex, node: Node): number {
  const contributionPoints = filterNodeChildren(
    child => isContribution(typeIndex, child),
    node
  ).reduce((sum, child) => sum + child.points, 0);
  
  const totalPoints = totalChildPoints(node);
  
  if (totalPoints === 0) {
    return 0.0;
  } else {
    return contributionPoints / totalPoints;
  }
}

// Helper function for calculating fulfillment
function childrenFulfillment(
  typeIndex: TypeIndex,
  pred: (node: Node) => boolean,
  node: Node
): number {
  return filterNodeChildren(pred, node).reduce(
    (sum, child) => sum + fulfilled(typeIndex, child) * shareOfParent(child),
    0
  );
}

// Calculate the fulfillment from contribution children
function contributionChildrenFulfillment(typeIndex: TypeIndex, node: Node): number {
  return childrenFulfillment(typeIndex, child => isContribution(typeIndex, child), node);
}

// Calculate the fulfillment from non-contribution children
function nonContributionChildrenFulfillment(typeIndex: TypeIndex, node: Node): number {
  return childrenFulfillment(typeIndex, child => !isContribution(typeIndex, child), node);
}

// Calculate the fulfillment of a node (core recursive function)
function fulfilled(typeIndex: TypeIndex, node: Node): number {
  // Leaf nodes
  if (node.nodeChildren.size === 0) {
    return isContribution(typeIndex, node) ? 1.0 : 0.0;
  }
  
  // Nodes with manual fulfillment and contributor children
  if (
    node.nodeManualFulfillment !== null &&
    hasDirectContributionChild(typeIndex, node)
  ) {
    const manualFulfillment = Math.min(1.0, Math.max(0.0, node.nodeManualFulfillment));
    
    if (!hasNonContributionChild(typeIndex, node)) {
      return manualFulfillment;
    } else {
      const contribWeight = contributionChildrenWeight(typeIndex, node);
      const nonContribFulfillment = nonContributionChildrenFulfillment(typeIndex, node);
      return manualFulfillment * contribWeight + nonContribFulfillment * (1.0 - contribWeight);
    }
  }
  
  // Default case: weighted sum of all children's fulfillment
  return sumOverChildren(
    child => fulfilled(typeIndex, child) * shareOfParent(child),
    node
  );
}

// Calculate the desire (unfulfilled need) of a node
function desire(typeIndex: TypeIndex, node: Node): number {
  return 1.0 - fulfilled(typeIndex, node);
}

// Get all ancestors of a node
function ancestors(node: Node): Node[] {
  if (node.nodeParent === null) {
    return [];
  }
  return [node.nodeParent, ...ancestors(node.nodeParent)];
}

// Calculate a node's share of general fulfillment from another type
function shareOfGeneralFulfillment(
  typeIndex: TypeIndex,
  node: Node,
  typeNode: Node
): number {
  // Get all instances of the type
  const typeInstances = getInstances(typeIndex, typeNode.nodeId);
  const nodeAncestors = ancestors(node);

  // Filter instances that are either the node itself or have the node as ancestor
  const relevantInstances = Array.from(typeInstances).filter(instance =>
    instance === node || nodeAncestors.some(ancestor => ancestor === instance)
  );

  // Calculate instance share for each relevant instance
  return relevantInstances.reduce((sum, instance) => {
    const contributorTypes = Array.from(instance.nodeTypes).filter(typeId => {
      const typeInstances = getInstances(typeIndex, typeId);
      return Array.from(typeInstances).some(isContributor);
    });
    
    const contributorTypesCount = contributorTypes.length;
    const fWeight = fulfilled(typeIndex, instance) * weight(instance);
    
    return sum + (contributorTypesCount > 0
      ? fWeight / contributorTypesCount
      : fWeight);
  }, 0);
}

// Calculate mutual fulfillment between two nodes
function mutualFulfillment(
  typeIndex: TypeIndex,
  node1: Node,
  node2: Node
): number {
  return Math.min(
    shareOfGeneralFulfillment(typeIndex, node1, node2),
    shareOfGeneralFulfillment(typeIndex, node2, node1)
  );
}

// Calculate mutual fulfillment distribution
function mutualFulfillmentDistribution(
  typeIndex: TypeIndex,
  node: Node
): Map<Node, number> {
  // Get valid type IDs (those with instances)
  const validTypeIds = Array.from(typeIndex.keys()).filter(
    typeId => getInstances(typeIndex, typeId).size > 0
  );
  
  // Calculate mutual fulfillment for each type node
  const typesWithValues: [Node, number][] = [];
  
  for (const typeId of validTypeIds) {
    const typeNode = getTypeNode(typeIndex, typeId);
    if (typeNode) {
      const mf = mutualFulfillment(typeIndex, node, typeNode);
      if (mf > 0) {
        typesWithValues.push([typeNode, mf]);
      }
    }
  }
  
  // Calculate total for normalization
  const total = typesWithValues.reduce((sum, [_, value]) => sum + value, 0);
  
  // Create normalized distribution
  const result = new Map<Node, number>();
  for (const [typeNode, value] of typesWithValues) {
    result.set(typeNode, total > 0 ? value / total : 0);
  }
  
  return result;
}

// Example function to create and use a tree
function exampleTree(): [Node, TypeIndex] {
  // Create root node
  const root = new Node(
    "root",
    "Root",
    0,
    null,
    [],
    null
  );
  
  // Create type nodes (like Alice, Bob, etc.)
  const alice = new Node(
    "alice",
    "Alice",
    0,
    null,
    [],
    null
  );
  
  const bob = new Node(
    "bob",
    "Bob",
    0,
    null,
    [],
    null
  );
  
  // Setup type index with contributors
  let typeIndex: TypeIndex = new Map();
  typeIndex = addNodeToTypeIndex(alice, "alice", typeIndex);
  typeIndex = addNodeToTypeIndex(bob, "bob", typeIndex);
  
  // Add children to root with different recognitions
  const need1Result = addChild(
    typeIndex,
    root,
    "need1",
    "Need 1",
    50,
    ["alice"],
    null
  );
  
  if (need1Result.error) {
    throw new Error(need1Result.error);
  }
  
  const [rootWithChild1, typeIndex1] = need1Result.result!;
  
  const need2Result = addChild(
    typeIndex1,
    rootWithChild1,
    "need2",
    "Need 2",
    30,
    ["bob"],
    null
  );
  
  if (need2Result.error) {
    throw new Error(need2Result.error);
  }
  
  const [finalRoot, finalTypeIndex] = need2Result.result!;
  
  return [finalRoot, finalTypeIndex];
}

// Main function to demonstrate tree-based calculations
function main(): void {
  console.log("Tree-Based Recognition and Fulfillment:");
  
  const [tree, typeIndex] = exampleTree();
  
  const need1 = tree.nodeChildren.get("need1");
  const need2 = tree.nodeChildren.get("need2");
  
  if (!need1 || !need2) {
    throw new Error("Needs not found");
  }
  
  console.log(`Need 1 Fulfillment: ${fulfilled(typeIndex, need1)}`);
  console.log(`Need 2 Fulfillment: ${fulfilled(typeIndex, need2)}`);
  console.log(`Root Total Fulfillment: ${fulfilled(typeIndex, tree)}`);
}

// Export all functions and classes for external use
export {
  Node,
  isContributor,
  mapTree,
  foldTree,
  filterNodeChildren,
  anyNodeChild,
  sumOverChildren,
  getInstances,
  getTypeNode,
  isContribution,
  addNodeToTypeIndex,
  removeNodeFromType,
  addChild,
  removeChild,
  addType,
  removeType,
  totalChildPoints,
  weight,
  shareOfParent,
  hasDirectContributionChild,
  hasNonContributionChild,
  contributionChildrenWeight,
  childrenFulfillment,
  contributionChildrenFulfillment,
  nonContributionChildrenFulfillment,
  fulfilled,
  desire,
  ancestors,
  shareOfGeneralFulfillment,
  mutualFulfillment,
  mutualFulfillmentDistribution,
  exampleTree,
  main
};

// Export type separately using 'export type'
export type { TypeIndex };
