// ====== ENHANCED TYPE SYSTEM ======
type EntityID = string;

interface BaseEntity {
  id: EntityID;
  type: 'Individual' | 'Collective';
  version: number; // For cache invalidation
}

interface IndividualNode extends BaseEntity {
  type: 'Individual';
  capacities: CapacitiesCollection;
  // ... other individual properties
}

interface Collective extends BaseEntity {
  type: 'Collective';
  members: EntityID[];
  weightMap: Map<EntityID, number>;
  structureHash: string; // For cache validation
}

type Entity = IndividualNode | Collective;
type Forest = Map<EntityID, Entity>;

// ====== OPTIMIZED CACHING SYSTEM ======
class RecognitionCache {
  private mfCache = new Map<string, number>();
  private weightCache = new Map<string, Map<EntityID, number>>();
  private structureCache = new Map<EntityID, string>();

  getMF(a: EntityID, b: EntityID): number | null {
    return this.mfCache.get(`${a}|${b}`) ?? null;
  }

  setMF(a: EntityID, b: EntityID, value: number): void {
    this.mfCache.set(`${a}|${b}`, value);
    this.mfCache.set(`${b}|${a}`, value); // Mutual recognition is symmetric
  }

  // Cache invalidation based on entity changes
  invalidate(entityId: EntityID): void {
    // Efficient invalidation using entity ID prefixes
    for (const key of this.mfCache.keys()) {
      if (key.startsWith(`${entityId}|`) || key.endsWith(`|${entityId}`)) {
        this.mfCache.delete(key);
      }
    }
    this.weightCache.delete(entityId);
  }
}

// ====== OPTIMIZED MUTUAL FULFILLMENT CALCULATION ======
function optimizedMutualFulfillment(
  forest: Forest,
  a: Entity,
  b: Entity,
  cache: RecognitionCache
): number {
  // Identity case
  if (a.id === b.id) return 1.0;
  
  // Check cache
  const cached = cache.getMF(a.id, b.id);
  if (cached !== null) return cached;
  
  // Base case: Both individuals
  if (a.type === 'Individual' && b.type === 'Individual') {
    const value = calculateBaseMF(a, b);
    cache.setMF(a.id, b.id, value);
    return value;
  }
  
  // Collective handling with dynamic programming
  let result = 0;
  if (a.type === 'Collective') {
    const [weights] = getCollectiveWeights(a, forest, cache);
    for (const memberId of a.members) {
      const member = forest.get(memberId);
      if (!member) continue;
      result += weights.get(memberId)! * optimizedMutualFulfillment(forest, member, b, cache);
    }
  } 
  // Similar logic for b as collective
  
  cache.setMF(a.id, b.id, result);
  return result;
}

// ====== COLLECTIVE TREE MERGING OPTIMIZATION ======
function mergeTreesOptimized(
  config: TreeMergeConfig,
  cache: RecognitionCache
): TreeMergeResult {
  // Pre-calculate all pairwise MF values
  const contributorIds = Object.keys(config.contributor_trees);
  const mfMatrix: number[][] = [];
  
  for (let i = 0; i < contributorIds.length; i++) {
    mfMatrix[i] = [];
    for (let j = 0; j <= i; j++) {
      const mf = optimizedMutualFulfillment(
        new Map(Object.entries(config.contributor_trees)),
        config.contributor_trees[contributorIds[i]],
        config.contributor_trees[contributorIds[j]],
        cache
      );
      mfMatrix[i][j] = mf;
      mfMatrix[j][i] = mf; // Symmetric
    }
  }

  // Calculate weights using precomputed matrix
  const weights: Record<string, number> = {};
  for (let i = 0; i < contributorIds.length; i++) {
    let totalRecognition = 0;
    for (let j = 0; j < contributorIds.length; j++) {
      if (i !== j) totalRecognition += mfMatrix[i][j];
    }
    weights[contributorIds[i]] = totalRecognition;
  }

  // Normalization and tree merging continues...
}

// ====== MATHEMATICAL CONSISTENCY CHECKS ======
function verifyTreeProperties(tree: CollectiveTree): VerificationResult {
  const errors: string[] = [];
  
  // 1. Conservation of Recognition Check
  const totalWeight = Object.values(tree.root.contributor_weights)
    .reduce((sum, w) => sum + w, 0);
  if (Math.abs(totalWeight - 1) > 1e-6) {
    errors.push(`Root weight sum invalid: ${totalWeight}`);
  }
  
  // 2. Path Consistency Verification
  const leafNodes = findLeafNodes(tree.root);
  for (const leaf of leafNodes) {
    const path = getPathToCollectiveNode(tree.root, leaf.id)!;
    let computedWeight = 1;
    
    for (const nodeId of path) {
      const node = findCollectiveNodeById(tree.root, nodeId)!;
      if (node.type === 'CollectiveNonRootNode') {
        computedWeight *= node.weight_percentage;
      }
    }
    
    if (Math.abs(leaf.weight_percentage - computedWeight) > 1e-6) {
      errors.push(`Path consistency broken at ${leaf.id}`);
    }
  }
  
  return {
    isValid: errors.length === 0,
    errors
  };
}

// ====== PERFORMANCE CRITICAL OPERATIONS ======
function calculateRecognitionPressure(
  collectiveTree: CollectiveTree,
  targetNodeId: string
): RecognitionPressureResult {
  // Use iterative path processing instead of recursion
  const path = getPathToCollectiveNode(collectiveTree.root, targetNodeId) || [];
  const pressureMap = new Map<EntityID, number>();
  
  // Initialize with root weights
  for (const contributorId of collectiveTree.contributors) {
    pressureMap.set(contributorId, 
      collectiveTree.root.contributor_weights[contributorId] || 0
    );
  }
  
  // Process path iteratively
  for (const nodeId of path.slice(1)) { // Skip root
    const node = findCollectiveNodeById(collectiveTree.root, nodeId)!;
    const newPressures = new Map<EntityID, number>();
    
    for (const [contributorId, currentPressure] of pressureMap) {
      if (node.type === 'CollectiveNonRootNode') {
        const contribution = node.source_contributors[contributorId] || 0;
        newPressures.set(contributorId, currentPressure * contribution * node.weight_percentage);
      }
    }
    
    // Update pressure map for next level
    pressureMap.clear();
    newPressures.forEach((v, k) => pressureMap.set(k, v));
  }
  
  // Convert to result format
  return { /* ... */ };
}

