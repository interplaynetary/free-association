import { derived, type Readable } from "svelte/store";
import {
  ReactiveGraph,
  withErrorHandling,
  safeGet,
} from "../utils/reactive/ReactiveGraph";
import { GunNode } from "../utils/gun/GunNode";
import {
  type Proportion,
  DistributionMap,
  asProportion,
} from "../utils/proportions";
import type { CollectionItem } from "../utils/svelte/reactiveStores";
import { transientGun } from "../utils/gun/gunSetup";

// ===== TYPE DEFINITIONS =====

// can we add the actual reference (soul) to addContributorData, same for addTag?

// What role does the contributor Store play? and why dont we just use the childrenStore (and add contributors onto the RecNodes directly)
// could we simply store contributors in the RecNode?

// we dont want to add contributors to the store
// we want to add contributors to the node that we selected

// We dont want to contributors of the store, but to contributors of the children

/**
 * Child node interface with specific properties
 */
export interface RecNode extends CollectionItem {
  _key?: string;
  name?: string;
  points?: number;
  parent?: any;
  isContribution?: boolean;
  fulfilled?: number;
  manualFulfillment?: number;
  contributors?: Record<string, any>;
  children?: RecNode[];
  [key: string]: any;
}

export type NodeEntry = [string, RecNode];

/**
 * Root recognition data interface
 */
export interface RecognitionData {
  name?: string;
  points?: number;
  parent?: any;
  manualFulfillment?: number;
  children?: Record<string, RecNode>;
  contributors?: Record<string, any>;
  tags?: Record<string, any>;
  metadata?: Record<string, any>;
}

/**
 * Contributor-specific data interface
 */
export interface ContributorData extends RecognitionData {
  sharesOfFulfillment?: Record<string, number>;
  socialDistributions?: Record<string, Record<string, number>>;
  preferences?: Record<string, number>;
}

/**
 * Base store interface with common functionality
 */
export interface RecognitionStoreBase {
  // Data stores
  nameStore: Readable<string>;
  pointsStore: Readable<number>;

  // Methods
  updateName: (name: string) => Promise<void>;
  updatePoints: (points: number) => Promise<void>;
  updateProperty: <P>(property: string, value: P) => Promise<void>;

  // Path information
  path: string[];
}

/**
 * Main recognition store interface
 */
export interface RecognitionStore extends RecognitionStoreBase {
  // Collection stores
  childrenStore: Readable<NodeEntry[]>;
  contributorsStore: Readable<NodeEntry[]>;
  tagsStore: Readable<[string, any][]>;

  // Derived data
  parentStore: Readable<any | null>;
  isContributionStore: Readable<boolean>;
  hasContributorsStore: Readable<boolean>;
  totalChildPointsStore: Readable<number>;
  shareOfParentStore: Readable<number>;
  fulfillmentStore: Readable<number>;
  contributionChildrenStore: Readable<NodeEntry[]>;
  nonContributionChildrenStore: Readable<NodeEntry[]>;

  // Operations
  addChild: (
    name: string,
    points?: number,
    isContribution?: boolean
  ) => Promise<string>;
  removeChild: (childId: string) => Promise<void>;
  addContributor: (contributorId: string, nodeId?: string) => Promise<void>;
  removeContributor: (contributorId: string, nodeId?: string) => Promise<void>;
  addTag: (tag: string) => Promise<void>;
  removeTag: (tag: string) => Promise<void>;

  // Traversal
  getChild: (childId: string) => RecognitionStore;
  getParent: () => Promise<RecognitionStore | null>;
  getRoot: () => Promise<RecognitionStore>;
  getContributor: (contributorId: string) => RecognitionStore;

  // Path traversal - returns path from node to root
  getPathToRoot: () => Promise<
    Array<{
      path: string[];
      store: RecognitionStore;
      name: string;
      id: string;
    }>
  >;

  // State management
  refreshState: () => Promise<void>;
  onStateChange: (callback: (state: RecognitionData) => void) => () => void;
}

/**
 * Contributor-specific store interface
 */
export interface ContributorStore extends RecognitionStore {
  // Contributor-specific stores
  preferencesStore: Readable<[string, any][]>;
  sharesOfFulfillmentStore: Readable<Record<string, number>>;
  socialDistributionStore: Readable<DistributionMap<string>>;

  // Contributor-specific methods
  updateShareOfFulfillment: (targetId: string, value: number) => Promise<void>;
  calculateSocialDistribution: (
    depth?: number
  ) => Promise<DistributionMap<string>>;
  getMutualRecognition: (otherContributorId: string) => Promise<Proportion>;
}

// ===== SINGLETON GRAPH MANAGEMENT =====

/**
 * Singleton to manage callbacks for state changes
 * This is a lightweight wrapper around ReactiveGraph that adds callback management
 */
class RecognitionSystem {
  private static instance: RecognitionSystem;
  private graph: ReactiveGraph;
  private callbacks: Map<string, Set<(state: any) => void>> = new Map();

  private constructor() {
    this.graph = new ReactiveGraph();
    // Enable debug mode in development
    if (process.env.NODE_ENV === "development") {
      this.graph.setDebugMode(false);
    }
  }

  public static getInstance(): RecognitionSystem {
    if (!RecognitionSystem.instance) {
      RecognitionSystem.instance = new RecognitionSystem();
    }
    return RecognitionSystem.instance;
  }

  public getGraph(): ReactiveGraph {
    return this.graph;
  }

  public registerCallback(
    path: string,
    callback: (state: any) => void
  ): () => void {
    if (!this.callbacks.has(path)) {
      this.callbacks.set(path, new Set());
    }

    this.callbacks.get(path)!.add(callback);

    return () => {
      const callbacks = this.callbacks.get(path);
      if (callbacks) {
        callbacks.delete(callback);
        if (callbacks.size === 0) {
          this.callbacks.delete(path);
        }
      }
    };
  }

  public notifyCallbacks(path: string, data: any): void {
    const callbacks = this.callbacks.get(path);
    if (callbacks) {
      for (const callback of callbacks) {
        callback(data);
      }
    }
  }

  public clearCache(): void {
    // Let ReactiveGraph's StoreRegistry handle the caching
    this.graph.getNodeStore(["_clearCache_"]).subscribe(() => {});
    this.callbacks.clear();
  }
}

// ===== IMPLEMENTATION FUNCTIONS =====

/**
 * Internal implementation of recognition store
 */
function createRecognitionStoreImpl(path: string[]): RecognitionStore {
  const system = RecognitionSystem.getInstance();
  const graph = system.getGraph();
  const pathKey = path.join("/");

  // Basic data stores using ReactiveGraph's built-in store methods
  const nameStore = graph.getNodePropertyStore<string>(path, "name", "");
  const pointsStore = graph.getNodePropertyStore<number>(path, "points", 0);
  const parentStore = graph.getNodePropertyStore<any>(path, "parent", null);
  const manualFulfillmentStore = graph.getNodePropertyStore<number>(
    path,
    "manualFulfillment",
    0
  );

  // Collection stores
  const childrenStore = graph.getCollectionStore<RecNode>([
    ...path,
    "children",
  ]);
  const contributorsStore = graph.getCollectionStore<RecNode>([
    ...path,
    "contributors",
  ]);
  const tagsStore = graph.getCollectionStore<any>([...path, "tags"]);

  // Derived stores using ReactiveGraph's createDerivedStore method
  const isContributionStore = graph.createDerivedStore<boolean>(
    ["parentStore", "contributorsStore"],
    [parentStore, contributorsStore],
    ([$parent, $contributors]) => Boolean($parent && $contributors.length > 0),
    `${pathKey}/isContribution`
  );

  const hasContributorsStore = graph.createDerivedStore<boolean>(
    ["contributorsStore"],
    [contributorsStore],
    ([$contributors]) => $contributors.length > 0,
    `${pathKey}/hasContributors`
  );

  const totalChildPointsStore = graph.createDerivedStore<number>(
    ["childrenStore"],
    [childrenStore],
    ([$children]) =>
      $children.reduce(
        (sum: number, [_, child]: [string, RecNode]) =>
          sum + (Number(child.points) || 0),
        0
      ),
    `${pathKey}/totalChildPoints`
  );

  const shareOfParentStore = graph.createDerivedStore<number>(
    ["pointsStore", "parentStore", "totalChildPointsStore"],
    [pointsStore, parentStore, totalChildPointsStore],
    ([$points, $parent, $totalChildPoints]) => {
      if (!$parent) return 1; // This is root
      return $totalChildPoints === 0 ? 0 : $points / $totalChildPoints;
    },
    `${pathKey}/shareOfParent`
  );

  // Filter collections
  const contributionChildrenStore = graph.createDerivedStore<NodeEntry[]>(
    ["childrenStore"],
    [childrenStore],
    ([$children]) =>
      $children.filter(([_, child]: [string, RecNode]) =>
        Boolean(child.isContribution)
      ) as NodeEntry[],
    `${pathKey}/contributionChildren`
  );

  const nonContributionChildrenStore = graph.createDerivedStore<NodeEntry[]>(
    ["childrenStore"],
    [childrenStore],
    ([$children]) =>
      $children.filter(
        ([_, child]: [string, RecNode]) => !Boolean(child.isContribution)
      ) as NodeEntry[],
    `${pathKey}/nonContributionChildren`
  );

  const materializedStore = derived(
    [nameStore, pointsStore, childrenStore, contributorsStore],
    ([$name, $points, $children, $contributors]) => {
      return {
        name: $name,
        points: $points,
        children: $children,
        contributors: $contributors,
      };
    }
  );

  // Complex derived state for fulfillment calculation
  const fulfillmentStore = graph.createDerivedStore<number>(
    [
      "contributionChildrenStore",
      "nonContributionChildrenStore",
      "manualFulfillmentStore",
      "totalChildPointsStore",
    ],
    [
      contributionChildrenStore,
      nonContributionChildrenStore,
      manualFulfillmentStore,
      totalChildPointsStore,
    ],
    ([
      $contributionChildren,
      $nonContributionChildren,
      $manualFulfillment,
      $totalChildPoints,
    ]) => {
      // Start with manual fulfillment
      let fulfillment = $manualFulfillment || 0;

      // Add contribution from children
      if ($totalChildPoints > 0) {
        let contributionFulfillment = 0;
        let nonContributionFulfillment = 0;

        for (const [_, child] of $contributionChildren as [string, RecNode][]) {
          const childFulfillment = child.fulfilled || 0;
          const childWeight = (child.points || 0) / $totalChildPoints;
          contributionFulfillment += childFulfillment * childWeight;
        }

        for (const [_, child] of $nonContributionChildren as [
          string,
          RecNode
        ][]) {
          const childFulfillment = child.fulfilled || 0;
          const childWeight = (child.points || 0) / $totalChildPoints;
          nonContributionFulfillment += childFulfillment * childWeight;
        }

        const contributionWeight =
          ($contributionChildren as [string, RecNode][]).reduce(
            (sum: number, [_, child]: [string, RecNode]) =>
              sum + (Number(child.points) || 0),
            0
          ) / $totalChildPoints;

        fulfillment += contributionFulfillment * contributionWeight;
        fulfillment += nonContributionFulfillment * (1 - contributionWeight);
      }

      return Math.min(1, Math.max(0, fulfillment));
    },
    `${pathKey}/fulfillment`
  );


  // Data store to notify callbacks
  const dataStore = graph.getNodeStore<RecognitionData>(path);
  dataStore.subscribe((state) => {
    system.notifyCallbacks(pathKey, state);
  });

  // State change notification through system
  const onStateChange = (callback: (state: RecognitionData) => void) => {
    return system.registerCallback(pathKey, callback);
  };

  // Data modification methods
  const updateName = async (name: string): Promise<void> => {
    return withErrorHandling(
      async () => {
        await graph.updateNodeProperty(path, "name", name);
      },
      undefined,
      `Error updating name for ${pathKey}`
    );
  };

  const updatePoints = async (points: number): Promise<void> => {
    return withErrorHandling(
      async () => {
        await graph.updateNodeProperty(path, "points", points);
      },
      undefined,
      `Error updating points for ${pathKey}`
    );
  };

  const updateProperty = async <P>(
    property: string,
    value: P
  ): Promise<void> => {
    return withErrorHandling(
      async () => {
        await graph.updateNodeProperty(path, property, value);
      },
      undefined,
      `Error updating ${property} for ${pathKey}`
    );
  };

  // Collection operations using graph API
  const addChild = async (
    name: string,
    points: number = 0,
    isContribution: boolean = false
  ): Promise<string> => {
    return withErrorHandling(
      async () => {
        return await graph.addNode(path, "children", {
          name,
          points,
          parent: { "#": pathKey },
          isContribution,
        });
      },
      "",
      `Error adding child to ${pathKey}`
    );
  };

  const removeChild = async (childId: string): Promise<void> => {
    return withErrorHandling(
      async () => {
        const childPath = [...path, "children"];
        const nodeRef = new GunNode(childPath);
        await nodeRef.get(childId).put(null as any);
      },
      undefined,
      `Error removing child ${childId} from ${pathKey}`
    );
  };

  const addContributor = async (contributorId: string): Promise<void> => {
    return withErrorHandling(
      async () => {
        await graph.addNode(
          path,
          "contributors",
          { value: true },
          contributorId
        );
      },
      undefined,
      `Error adding contributor ${contributorId} to ${pathKey}`
    );
  };

  const removeContributor = async (contributorId: string): Promise<void> => {
    return withErrorHandling(
      async () => {
        const contributorPath = [...path, "contributors"];
        const nodeRef = new GunNode(contributorPath);
        await nodeRef.get(contributorId).put(null as any);
      },
      undefined,
      `Error removing contributor ${contributorId} from ${pathKey}`
    );
  };

  const addTag = async (tag: string): Promise<void> => {
    return withErrorHandling(
      async () => {
        await graph.addNode(path, "tags", { value: true }, tag);
      },
      undefined,
      `Error adding tag ${tag} to ${pathKey}`
    );
  };

  const removeTag = async (tag: string): Promise<void> => {
    return withErrorHandling(
      async () => {
        const tagPath = [...path, "tags"];
        const nodeRef = new GunNode(tagPath);
        await nodeRef.get(tag).put(null as any);
      },
      undefined,
      `Error removing tag ${tag} from ${pathKey}`
    );
  };

  // Navigation methods
  const getChild = (childId: string): RecognitionStore => {
    return createRecognitionStoreImpl([...path, "children", childId]);
  };
  
  const getContributor = (contributorId: string): RecognitionStore => {
    return createRecognitionStoreImpl([...path, "contributors", contributorId]);
  };

  const getParent = async (): Promise<RecognitionStore | null> => {
    return withErrorHandling(
      async () => {
        const parent = safeGet(parentStore, null);
        if (!parent) return null;

        // Extract path from parent reference
        const parentPath =
          typeof parent === "string"
            ? parent.split("/")
            : parent["#"]
            ? parent["#"].split("/")
            : null;

        if (!parentPath) return null;

        return createRecognitionStoreImpl(parentPath);
      },
      null,
      `Error getting parent for ${pathKey}`
    );
  };

  const getRoot = async (): Promise<RecognitionStore> => {
    return withErrorHandling(
      async () => {
        let current: RecognitionStore = createRecognitionStoreImpl(path);
        let parent = await current.getParent();

        // Traverse up to root
        while (parent !== null) {
          current = parent;
          parent = await current.getParent();
        }

        return current;
      },
      createRecognitionStoreImpl(path),
      `Error getting root for ${pathKey}`
    );
  };

  // Get base data helpers
  const getStoreSnapshot = async <T>(store: Readable<T>): Promise<T> => {
    return new Promise((resolve) => {
      const unsub = store.subscribe((value) => {
        resolve(value);
        unsub();
      });
    });
  };

  // Path traversal implementation
  const getPathToRoot = async (): Promise<
    Array<{
      path: string[];
      store: RecognitionStore;
      name: string;
      id: string;
    }>
  > => {
    try {
      // Create the result array, starting with the current node
      const result: Array<{
        path: string[];
        store: RecognitionStore;
        name: string;
        id: string;
      }> = [];

      console.log("getPathToRoot", result);
      // Add current node to the path
      const currentName = await getStoreSnapshot(nameStore);
      const currentId = path[path.length - 1] || "root";

      result.push({
        path: [...path],
        store: createRecognitionStoreImpl(path),
        name: currentName || "",
        id: currentId,
      });

      // Recursively add parents until we reach the root
      let currentStore: RecognitionStore | null =
        createRecognitionStoreImpl(path);
      let parentStore = await currentStore.getParent();

      while (parentStore !== null) {
        const parentPath = parentStore.path;
        const parentName = await getStoreSnapshot(parentStore.nameStore);
        const parentId = parentPath[parentPath.length - 1] || "root";

        result.push({
          path: parentPath,
          store: parentStore,
          name: parentName || "",
          id: parentId,
        });

        // Move up to the next parent
        currentStore = parentStore;
        parentStore = await currentStore.getParent();
      }

      return result;
    } catch (err) {
      console.error(`Error getting path to root for ${pathKey}`, err);
      // Return fallback with just the current node
      const currentNodeName = safeGet(nameStore, "") || "";
      const currentNodeId = path[path.length - 1] || "root";

      return [
        {
          path: [...path],
          store: createRecognitionStoreImpl(path),
          name: currentNodeName,
          id: currentNodeId,
        },
      ];
    }
  };

  // Force refresh state
  const refreshState = async (): Promise<void> => {
    return withErrorHandling(
      async () => {
        const data = safeGet(dataStore, {} as RecognitionData);
        system.notifyCallbacks(pathKey, data);
      },
      undefined,
      `Error refreshing state for ${pathKey}`
    );
  };

  return {
    // Basic stores
    nameStore,
    pointsStore,
    parentStore,

    // Collection stores
    childrenStore,
    contributorsStore,
    tagsStore,

    // Derived stores
    isContributionStore,
    hasContributorsStore,
    totalChildPointsStore,
    shareOfParentStore,
    fulfillmentStore,
    contributionChildrenStore,
    nonContributionChildrenStore,

    // Methods
    updateName,
    updatePoints,
    updateProperty,
    addChild,
    removeChild,
    addContributor,
    removeContributor,
    addTag,
    removeTag,

    // Navigation
    getChild,
    getParent,
    getRoot,
    getContributor,

    getPathToRoot,

    // State management
    refreshState,
    onStateChange,

    // Path information
    path,
  };
}

/**
 * Internal implementation of contributor store
 */
function createContributorStoreImpl(path: string[]): ContributorStore {
  // Get base recognition store
  const baseStore = createRecognitionStoreImpl(path) as RecognitionStore;
  const system = RecognitionSystem.getInstance();
  const graph = system.getGraph();
  const pathKey = path.join("/");

  // Contributor-specific stores
  const preferencesStore = graph.getCollectionStore<any>([
    ...path,
    "preferences",
  ]);
  const sharesOfFulfillmentStore = graph.getNodePropertyStore<
    Record<string, number>
  >(path, "sharesOfFulfillment", {});

  // Social distribution cache - stored in closure, not duplicating the graph's cache
  const socialDistributionCache: Map<
    number,
    Promise<DistributionMap<string>>
  > = new Map();

  // Create a derived store for social distribution
  const socialDistributionStore = graph.createDerivedStore<
    DistributionMap<string>
  >(
    ["sharesOfFulfillment"],
    [sharesOfFulfillmentStore],
    ([$sharesOfFulfillment]) => {
      const distribution = new DistributionMap<string>();

      if ($sharesOfFulfillment) {
        for (const [targetId, share] of Object.entries($sharesOfFulfillment)) {
          distribution.set(targetId, Number(share));
        }
      }

      return distribution;
    },
    `${pathKey}/socialDistribution`
  );

  // Contributor-specific methods
  const updatePreference = async (
    targetId: string,
    value: number
  ): Promise<void> => {
    return withErrorHandling(
      async () => {
        await graph.updateNodeProperty(
          [...path, "preferences", targetId],
          "value",
          value
        );
      },
      undefined,
      `Error updating preference for ${targetId}`
    );
  };

  const updateShareOfFulfillment = async (
    targetId: string,
    value: number
  ): Promise<void> => {
    return withErrorHandling(
      async () => {
        // Get current shares
        const shares = safeGet(sharesOfFulfillmentStore, {});

        // Update the value
        const updatedShares = { ...shares, [targetId]: value };

        // Save back using graph's updateNodeProperty
        await graph.updateNodeProperty(
          path,
          "sharesOfFulfillment",
          updatedShares
        );

        // Clear cache
        socialDistributionCache.clear();
      },
      undefined,
      `Error updating fulfillment share for ${targetId}`
    );
  };

  const calculateSocialDistribution = async (
    depth: number = 2
  ): Promise<DistributionMap<string>> => {
    // Check cache first
    if (socialDistributionCache.has(depth)) {
      try {
        return await socialDistributionCache.get(depth)!;
      } catch (err) {
        // If cached promise rejection, clear and recalculate
        socialDistributionCache.delete(depth);
      }
    }

    // Create a promise for the calculation and store in cache
    const calculationPromise = (async () => {
      try {
        // Base case
        if (depth <= 0) {
          const selfDistribution = new DistributionMap<string>();
          selfDistribution.set(path[path.length - 1], 1);
          return selfDistribution;
        }

        // Get shares
        const shares = safeGet(sharesOfFulfillmentStore, {});

        // If no shares, return self
        if (Object.keys(shares).length === 0) {
          const selfDistribution = new DistributionMap<string>();
          selfDistribution.set(path[path.length - 1], 1);
          return selfDistribution;
        }

        // Create distribution map
        const distribution = new DistributionMap<string>();

        // Process each share using BFS to avoid stack overflow for deep graphs
        const processQueue: Array<[string, number, number]> = [];

        // Add initial shares to queue as [targetId, weight, remainingDepth]
        for (const [targetId, share] of Object.entries(shares)) {
          processQueue.push([targetId, Number(share), depth - 1]);
        }

        // Process queue
        while (processQueue.length > 0) {
          const [targetId, weight, remainingDepth] = processQueue.shift()!;

          if (remainingDepth <= 0) {
            // At max depth, add weight directly to target
            const currentValue = distribution.get(targetId) || 0;
            distribution.set(targetId, currentValue + weight);
            continue;
          }

          // Get target contributor's shares
          const targetStore = createContributorStoreImpl([
            "contributors",
            targetId,
          ]);
          const targetShares = safeGet(
            targetStore.sharesOfFulfillmentStore,
            {}
          );

          if (Object.keys(targetShares).length === 0) {
            // If no further shares, add weight to target
            const currentValue = distribution.get(targetId) || 0;
            distribution.set(targetId, currentValue + weight);
          } else {
            // Distribute weight according to shares
            for (const [nextId, nextShare] of Object.entries(targetShares)) {
              const nextWeight = weight * Number(nextShare);
              processQueue.push([nextId, nextWeight, remainingDepth - 1]);
            }
          }
        }

        // Normalize
        distribution.normalize();
        return distribution;
      } catch (err) {
        console.error(
          `Error calculating social distribution for ${pathKey}:`,
          err
        );
        const fallback = new DistributionMap<string>();
        fallback.set(path[path.length - 1], 1);
        return fallback;
      }
    })();

    // Store in cache and return
    socialDistributionCache.set(depth, calculationPromise);
    return calculationPromise;
  };

  const getMutualRecognition = async (
    otherContributorId: string
  ): Promise<Proportion> => {
    return withErrorHandling(
      async () => {
        // Get our shares for the other contributor
        const ourShares = safeGet(sharesOfFulfillmentStore, {});
        const ourShareToOther = ourShares[otherContributorId] || 0;

        // Get other contributor's shares for us
        const otherStore = createContributorStoreImpl([
          "contributors",
          otherContributorId,
        ]);
        const otherShares = safeGet(otherStore.sharesOfFulfillmentStore, {});
        const otherShareToUs = otherShares[path[path.length - 1]] || 0;

        // Create proportion - use minimum as the mutual recognition value
        return asProportion(Math.min(ourShareToOther, otherShareToUs));
      },
      asProportion(0),
      `Error getting mutual recognition between ${pathKey} and ${otherContributorId}`
    );
  };

  return {
    ...baseStore,

    // Contributor-specific stores
    preferencesStore,
    sharesOfFulfillmentStore,
    socialDistributionStore,

    // Contributor-specific methods
    updateShareOfFulfillment,
    calculateSocialDistribution,
    getMutualRecognition,
  };
}

// ===== PUBLIC API =====

/**
 * Factory function to create a recognition store
 */
export function createRec(path: string[] = ["recognition"]): RecognitionStore {
  return createRecognitionStoreImpl(path);
}

/**
 * Factory function to create a contributor store
 */
export function createContributor(contributorId: string): ContributorStore {
  return createContributorStoreImpl(["contributors", contributorId]);
}

/**
 * Clear all caches - useful for testing or state resets
 */
export function clearRecognitionCaches(): void {
  RecognitionSystem.getInstance().clearCache();
}

/**
 * Set debug mode for the recognition system
 */
export function setRecognitionDebugMode(enabled: boolean): void {
  RecognitionSystem.getInstance().getGraph().setDebugMode(enabled);
}
