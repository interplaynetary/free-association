import { GunNode } from "../utils/gun/GunNode";
import { GunSubscription } from "../utils/gun/GunSubscription";
import { 
  createGunStore, 
  deriveFromGun, 
  aggregateCollection,
  asyncDerivedStore,
  type GunStore, 
  type CollectionItem
} from "../utils/svelte/reactiveStores";
import { derived, get, type Readable } from "svelte/store";
import { type Proportion, DistributionMap, asProportion } from "../utils/proportions";
import { ReactiveGraph, safeGet, withErrorHandling, StoreRegistry } from "../utils/reactive/ReactiveGraph";

// ===== IMPROVED TYPE DEFINITIONS =====

/**
 * Improved typing for child nodes with specific properties
 */
export interface ChildNode extends CollectionItem {
  _key?: string;
  name?: string;
  points?: number; 
  parent?: any;
  isContribution?: boolean;
  fulfilled?: number;
  manualFulfillment?: number;
  [key: string]: any;
}

export type ChildEntry = [string, ChildNode];

/**
 * Enhanced recognition data interface with more specific types
 */
export interface RecognitionData {
  name?: string;
  points?: number;
  parent?: any; // Gun reference
  manualFulfillment?: number;
  children?: Record<string, ChildNode>;
  contributors?: Record<string, any>;
}

export interface ContributorData extends RecognitionData {
  sharesOfGeneralFulfillment?: Record<string, number>;
  socialDistributions?: Record<string, Record<string, number>>;
}

// Base interface for all reactive recognition stores
export interface BaseRecognitionStores<T> {
  // Basic data stores
  dataStore: Readable<T>;
  nameStore: Readable<string>;
  pointsStore: Readable<number>;
  
  // Methods to update data
  updateName: (name: string) => Promise<void>;
  updatePoints: (points: number) => Promise<void>;
  
  // Access to underlying models
  node: GunNode<T>;
  path: string[];
}

// Interface for recognition stores with improved typing
export interface EnhancedRecognitionStores extends BaseRecognitionStores<RecognitionData> {
  // Collection stores
  contributorsStore: Readable<ChildEntry[]>;
  childrenStore: Readable<ChildEntry[]>;
  
  // Derived data stores
  parentStore: Readable<any | null>;
  isContributionStore: Readable<boolean>;
  isContributorStore: Readable<boolean>;
  totalChildPointsStore: Readable<number>;
  shareOfParentStore: Readable<number>;
  shareOfRootStore: Readable<number>;
  hasDirectContributionChildStore: Readable<boolean>;
  hasNonContributionChildStore: Readable<boolean>;
  contributionChildrenWeightStore: Readable<number>;
  contributionChildrenFulfillmentStore: Readable<number>;
  nonContributionChildrenFulfillmentStore: Readable<number>;
  fulfilledStore: Readable<number>;
  desireStore: Readable<number>;
  
  // Methods to modify related nodes
  addChild: (name: string, points?: number, contributors?: string[], manualFulfillment?: number) => Promise<string>;
  removeChild: (childId: string) => Promise<void>;
  addContributor: (contributorId: string) => Promise<void>;
  removeContributor: (contributorId: string) => Promise<void>;
}

// Interface for contributor stores with improved typing
export interface EnhancedContributorStores extends EnhancedRecognitionStores {
  // Additional contributor-specific stores with better typing
  sharesOfGeneralFulfillmentStore: Readable<Map<string, number>>;
  socialDistributionsStore: Readable<Map<number, DistributionMap<string>>>;
  
  // Methods for contributors with better error handling
  updateSharesOfGeneralFulfillment: () => Promise<Map<string, number>>;
  calculateSocialDistribution: (depth?: number) => Promise<DistributionMap<string>>;
  mutualRecognition: (otherContributorId: string) => Promise<Proportion>;
}

/**
 * Helper to get a child property store with proper caching
 */
function getChildPropertyStore<T>(
  graph: ReactiveGraph,
  parentPath: string[],
  childId: string,
  property: string, 
  defaultValue?: T
): Readable<T> {
  const childPath = [...parentPath, 'children', childId, property];
  return graph.getNodeStore<T>(childPath, defaultValue);
}

/**
 * Enhanced helper to get multiple child properties at once
 */
function getChildProperties<T extends Record<string, any>>(
  graph: ReactiveGraph,
  parentPath: string[],
  childId: string,
  properties: (keyof T)[]
): { [K in keyof T]: Readable<T[K]> } {
  const result = {} as { [K in keyof T]: Readable<T[K]> };
  
  for (const prop of properties) {
    const childPath = [...parentPath, 'children', childId, prop as string];
    result[prop] = graph.getNodeStore(childPath);
  }
  
  return result;
}

// ===== ENHANCED RECOGNITION STORE IMPLEMENTATION =====

/**
 * Create a recognition data model, strictly focusing on data access
 */
function createRecognitionDataModel(path: string[]) {
  const graph = new ReactiveGraph();
  
  // Core data stores
  const dataStore = graph.getNodeStore<RecognitionData>(path);
  const node = new GunNode<RecognitionData>(path);
  
  // Basic property stores
  const nameStore = graph.getNodeStore<string>([...path, 'name'], '');
  const pointsStore = graph.getNodeStore<number>([...path, 'points'], 0);
  const parentStore = graph.getNodeStore<any | null>([...path, 'parent'], null);
  const manualFulfillmentStore = graph.getNodeStore<number>([...path, 'manualFulfillment'], 0);
  
  // Collection stores
  const childrenStore = graph.getCollectionStore<ChildNode>([...path, 'children']);
  const contributorsStore = graph.getCollectionStore<ChildNode>([...path, 'contributors']);
  
  // Extract values from Gun objects
  const nameValueStore = derived(nameStore, $name => {
    if ($name && typeof $name === 'object' && 'value' in ($name as any)) {
      return ($name as any).value || '';
    }
    return typeof $name === 'string' ? $name : '';
  });
  
  const pointsValueStore = derived(pointsStore, $points => {
    if ($points && typeof $points === 'object' && 'value' in ($points as any)) {
      return Number(($points as any).value) || 0;
    }
    return typeof $points === 'number' ? $points : 0;
  });
  
  // Core data modification methods
  const updateName = async (name: string): Promise<void> => {
    return withErrorHandling(
      async () => {
        await node.get('name').put(name as unknown as Partial<any>);
      },
      undefined,
      `Error updating name for ${path.join('/')}`
    );
  };
  
  const updatePoints = async (points: number): Promise<void> => {
    return withErrorHandling(
      async () => {
        await node.get('points').put(points as unknown as Partial<any>);
      },
      undefined,
      `Error updating points for ${path.join('/')}`
    );
  };
  
  const addChild = async (
    name: string,
    points: number = 0,
    contributors: string[] = [],
    manualFulfillment?: number
  ): Promise<string> => {
    return withErrorHandling(
      async () => {
        // Generate a unique ID for the child node
        const childId = Date.now().toString();
        
        // Create the child node
        const childNode = node.get('children').get(childId);
        
        // Set up the basic properties
        await childNode.put({
          name,
          points,
          parent: node.getChain(),
          ...(manualFulfillment !== undefined ? { manualFulfillment } : {})
        });
        
        // Add contributors if provided
        for (const contributorId of contributors) {
          await childNode.get('contributors').get(contributorId).put(true as unknown as Partial<any>);
        }
        
        return childId;
      },
      '',
      `Error adding child to ${path.join('/')}`
    );
  };
  
  const removeChild = async (childId: string): Promise<void> => {
    return withErrorHandling(
      async () => {
        await node.get('children').get(childId).put(null as any);
      },
      undefined,
      `Error removing child ${childId} from ${path.join('/')}`
    );
  };
  
  const addContributor = async (contributorId: string): Promise<void> => {
    return withErrorHandling(
      async () => {
        await node.get('contributors').get(contributorId).put({ value: true });
      },
      undefined,
      `Error adding contributor ${contributorId} to ${path.join('/')}`
    );
  };
  
  const removeContributor = async (contributorId: string): Promise<void> => {
    return withErrorHandling(
      async () => {
        await node.get('contributors').get(contributorId).put(null as any);
      },
      undefined,
      `Error removing contributor ${contributorId} from ${path.join('/')}`
    );
  };
  
  return {
    // Base data
    dataStore,
    nameStore: nameValueStore,
    pointsStore: pointsValueStore,
    parentStore,
    manualFulfillmentStore,
    
    // Collections
    childrenStore,
    contributorsStore,
    
    // Methods
    updateName,
    updatePoints,
    addChild,
    removeChild,
    addContributor,
    removeContributor,
    
    // Model access
    node,
    path,
    graph
  };
}

/**
 * Create enhanced recognition UI state from a data model
 */
function createRecognitionUIState(dataModel: ReturnType<typeof createRecognitionDataModel>) {
  const { 
    parentStore, 
    contributorsStore, 
    childrenStore,
    pointsStore,
    manualFulfillmentStore,
    path,
    graph
  } = dataModel;
  
  // Derived state for UI
  const isContributorStore = derived(parentStore, $parent => $parent === null);
  
  const isContributionStore = derived(
    [parentStore, contributorsStore],
    ([$parent, $contributors]) => Boolean($parent && $contributors.length > 0)
  );
  
  const totalChildPointsStore = aggregateCollection(
    childrenStore as any,
    (children: ChildEntry[]) => children.reduce((sum, [_, child]) => sum + (Number(child.points) || 0), 0)
  );
  
  const shareOfParentStore = deriveFromGun(
    [pointsStore, totalChildPointsStore, parentStore],
    ([points, totalChildPoints, parent]) => {
      if (!parent) return 1; // This is root
      return totalChildPoints === 0 ? 0 : points / totalChildPoints;
    }
  );
  
  // Create a shareOfRoot store that updates when relevant values change
  const shareOfRootStore = asyncDerivedStore(
    [shareOfParentStore, parentStore],
    async (values) => {
      const shareOfParent = values[0] as number;
      const parent = values[1] as any;
      
      if (!parent) return 1;
      
      const parentPath = Array.isArray(parent) ? parent : [];
      if (parentPath.length === 0) return shareOfParent;
      
      try {
        // Use cached store for parent share of root
        const parentShareStore = graph.getNodeStore<number>(parentPath, 1);
        const parentShareOfRoot = safeGet(parentShareStore, 1);
        
        return shareOfParent * parentShareOfRoot;
      } catch (err) {
        console.error('Error getting parent share of root:', err);
        return shareOfParent;
      }
    },
    1
  );
  
  // Async derived stores using the improved pattern
  const hasDirectContributionChildStore = asyncDerivedStore(
    [childrenStore],
    async (values) => {
      const children = values[0] as ChildEntry[];
      for (const [childId, _] of children) {
        // Get cached store for child's isContribution property
        const isContributionStore = getChildPropertyStore<boolean>(graph, path, childId, 'isContribution', false);
        const isContribution = safeGet(isContributionStore, false);
        
        if (isContribution) {
          return true;
        }
      }
      return false;
    },
    false
  );
  
  const hasNonContributionChildStore = asyncDerivedStore(
    [childrenStore],
    async (values) => {
      const children = values[0] as ChildEntry[];
      for (const [childId, _] of children) {
        // Get cached store for child's isContribution property
        const isContributionStore = getChildPropertyStore<boolean>(graph, path, childId, 'isContribution', false);
        const isContribution = safeGet(isContributionStore, false);
        
        if (!isContribution) {
          return true;
        }
      }
      return false;
    },
    false
  );
  
  // Use helper functions for child stores to reduce duplication
  function getChildFulfillment(
    children: ChildEntry[], 
    totalChildPoints: number, 
    filterFn: (isContribution: boolean) => boolean
  ): number {
    let fulfillmentSum = 0;
    
    for (const [childId, child] of children) {
      // Get all properties we need at once
      const props = getChildProperties<{ isContribution: boolean, fulfilled: number }>(
        graph,
        path, 
        childId, 
        ['isContribution', 'fulfilled']
      );
      
      const isContribution = safeGet(props.isContribution, false);
      
      if (filterFn(isContribution)) {
        const fulfilled = safeGet(props.fulfilled, 0);
        const points = Number(child.points) || 0;
        const shareOfParent = totalChildPoints === 0 ? 0 : points / totalChildPoints;
        
        fulfillmentSum += fulfilled * shareOfParent;
      }
    }
    
    return fulfillmentSum;
  }
  
  const contributionChildrenWeightStore = asyncDerivedStore(
    [childrenStore, totalChildPointsStore],
    async (values) => {
      const children = values[0] as ChildEntry[];
      const totalChildPoints = values[1] as number;
      
      let contributionPoints = 0;
      
      for (const [childId, child] of children) {
        const points = Number(child.points) || 0;
        const isContributionStore = getChildPropertyStore<boolean>(graph, path, childId, 'isContribution', false);
        const isContribution = safeGet(isContributionStore, false);
        
        if (isContribution) {
          contributionPoints += points;
        }
      }
      
      return totalChildPoints === 0 ? 0 : contributionPoints / totalChildPoints;
    },
    0
  );
  
  const contributionChildrenFulfillmentStore = asyncDerivedStore(
    [childrenStore, totalChildPointsStore],
    async (values) => {
      const children = values[0] as ChildEntry[];
      const totalChildPoints = values[1] as number;
      
      return getChildFulfillment(
        children, 
        totalChildPoints, 
        (isContribution) => isContribution
      );
    },
    0
  );
  
  const nonContributionChildrenFulfillmentStore = asyncDerivedStore(
    [childrenStore, totalChildPointsStore],
    async (values) => {
      const children = values[0] as ChildEntry[];
      const totalChildPoints = values[1] as number;
      
      return getChildFulfillment(
        children, 
        totalChildPoints, 
        (isContribution) => !isContribution
      );
    },
    0
  );

  // Calculate the fulfillment combining different sources
  const fulfilledStore = derived(
    [
      contributionChildrenFulfillmentStore,
      contributionChildrenWeightStore,
      nonContributionChildrenFulfillmentStore,
      manualFulfillmentStore
    ],
    ([
      contributionChildrenFulfillment,
      contributionChildrenWeight,
      nonContributionChildrenFulfillment,
      manualFulfillment
    ]) => {
      // Apply contribution fulfillment only to its portion
      let fulfilled = contributionChildrenFulfillment * (contributionChildrenWeight as number);
      
      // Add non-contribution fulfillment for its portion
      fulfilled += nonContributionChildrenFulfillment * (1 - (contributionChildrenWeight as number));
      
      // Add manual fulfillment
      fulfilled += (manualFulfillment as number) || 0;
      
      // Cap at 1
      return Math.min(1, fulfilled);
    }
  );
  
  // Add desire store which is a user-configurable value representing desire/interest in the node
  const desireStore = graph.getNodeStore<number>([...path, 'desire'], 0);
  
  return {
    ...dataModel,
    
    // Reactive UI state
    isContributorStore,
    isContributionStore,
    totalChildPointsStore,
    shareOfParentStore,
    shareOfRootStore,
    hasDirectContributionChildStore,
    hasNonContributionChildStore,
    contributionChildrenWeightStore,
    contributionChildrenFulfillmentStore,
    nonContributionChildrenFulfillmentStore,
    fulfilledStore,
    desireStore,
    
    // Utility for child property access
    getChildProperties: <T extends Record<string, any>>(childId: string, propertyNames: string[]) => 
      getChildProperties<T>(graph, path, childId, propertyNames),
    getChildPropertyStore: <T>(childId: string, propertyName: string, defaultValue?: T) => 
      getChildPropertyStore<T>(graph, path, childId, propertyName, defaultValue)
  };
}

// ===== CONTRIBUTOR STORES =====

/**
 * Create enhanced contributor stores with social distribution and mutual recognition
 */
function createContributorStores(dataModel: ReturnType<typeof createRecognitionDataModel>) {
  const { node, path, contributorsStore, graph } = dataModel;
  
  // Cache for social distribution calculations to prevent redundant calculations
  const socialDistributionCache = new Map<string, Map<number, any>>();
  
  /**
   * Calculate mutual recognition between two contributors
   * @param contributors Array of contributor IDs
   * @returns Mutual recognition matrix
   */
  const calculateMutualRecognition = async (contributors: string[]): Promise<Record<string, Record<string, number>>> => {
    return withErrorHandling(
      async () => {
        const result: Record<string, Record<string, number>> = {};
        
        // Initialize result structure
        for (const contributorId of contributors) {
          result[contributorId] = {};
          for (const otherId of contributors) {
            result[contributorId][otherId] = 0;
          }
        }
        
        // Calculate mutual recognition for each pair
        for (const contributorId of contributors) {
          for (const otherId of contributors) {
            if (contributorId === otherId) continue;
            
            // Get contributor's shares
            const sharesStore = graph.getNodeStore<Record<string, number>>([...path, 'contributors', contributorId, 'shares']);
            const shares = safeGet(sharesStore, {});
            
            // Update mutual recognition value
            result[contributorId][otherId] = shares[otherId] || 0;
          }
        }
        
        return result;
      },
      {} as Record<string, Record<string, number>>,
      `Error calculating mutual recognition for ${path.join('/')}`
    );
  };
  
  /**
   * Calculate social distribution from a contributor
   * @param contributorId Contributor ID to start from
   * @param depth Depth of social distribution calculation
   * @returns Social distribution object
   */
  const calculateSocialDistribution = async (contributorId: string, depth: number = 2): Promise<Record<string, number>> => {
    // Input validation
    if (depth < 0) {
      throw new Error('Depth must be non-negative');
    }
    
    if (depth === 0) {
      return { [contributorId]: 1 };
    }
    
    // Check cache
    if (!socialDistributionCache.has(contributorId)) {
      socialDistributionCache.set(contributorId, new Map());
    }
    
    const cacheForContributor = socialDistributionCache.get(contributorId)!;
    if (cacheForContributor.has(depth)) {
      return cacheForContributor.get(depth);
    }
    
    return withErrorHandling(
      async () => {
        // Get contributor's shares
        const sharesStore = graph.getNodeStore<Record<string, number>>([...path, 'contributors', contributorId, 'shares'], {});
        const shares = safeGet(sharesStore, {});
        const result: Record<string, number> = { [contributorId]: 0 };
        
        // Base case: no shares
        if (Object.keys(shares).length === 0) {
          result[contributorId] = 1;
          cacheForContributor.set(depth, result);
          return result;
        }
        
        // Recursive case: distribute based on shares
        for (const [otherId, share] of Object.entries(shares)) {
          // Get nested distribution
          const nestedDistribution = await calculateSocialDistribution(otherId, depth - 1);
          
          // Combine distributions with weights
          for (const [nestedId, nestedShare] of Object.entries(nestedDistribution)) {
            result[nestedId] = (result[nestedId] || 0) + nestedShare * share;
          }
        }
        
        // Store in cache
        cacheForContributor.set(depth, result);
        return result;
      },
      { [contributorId]: 1 } as Record<string, number>,
      `Error calculating social distribution for ${contributorId} at depth ${depth}`
    );
  };
  
  /**
   * Update a contributor's shares in other contributors
   * @param contributorId Contributor ID to update
   * @param shares New shares object
   */
  const updateShares = async (contributorId: string, shares: Record<string, number>): Promise<void> => {
    return withErrorHandling(
      async () => {
        await node.get('contributors').get(contributorId).get('shares').put(shares as unknown as Partial<any>);
        
        // Clear cache for this contributor
        socialDistributionCache.delete(contributorId);
      },
      undefined,
      `Error updating shares for ${contributorId}`
    );
  };
  
  // Create a store for contributors list
  const contributorIdsStore = derived(contributorsStore, ($contributors) => {
    return $contributors.map(([id, _]) => id);
  });
  
  // MutualRecognition store
  const mutualRecognitionStore = asyncDerivedStore(
    [contributorIdsStore],
    async (values) => {
      const contributorIds = values[0] as string[];
      return await calculateMutualRecognition(contributorIds);
    },
    {} as Record<string, Record<string, number>>
  );
  
  return {
    // Data model operations
    ...dataModel,
    
    // Contributor-specific functionality
    contributorIdsStore,
    mutualRecognitionStore,
    calculateSocialDistribution,
    updateShares,
    
    // Clear cache
    clearSocialDistributionCache: () => {
      socialDistributionCache.clear();
    }
  };
}

// ===== FACTORY FUNCTION =====

/**
 * Create the enhanced recognition stores system
 * This is the main factory function that combines all the sub-components
 */
export function createEnhancedRecognitionStores(
  path: string[] = ['recognition']
): EnhancedRecognitionStores {
  // Create the base data model
  const dataModel = createRecognitionDataModel(path);
  
  // Create UI state
  const uiState = createRecognitionUIState(dataModel);
  
  // Create contributor stores
  const contributorStores = createContributorStores(dataModel);
  
  // Return the combined store system
  return {
    ...uiState,
    ...contributorStores
  };
}
