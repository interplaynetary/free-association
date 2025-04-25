
import type { Readable } from "svelte/store";
import type { DistributionMap, Proportion } from "./proportions";
import type { CollectionItem } from "../lib/utils/svelte/reactiveStores";

// ===== TYPE DEFINITIONS =====

export type NodeData = {
    id: string;
    store: RecognitionStore;
    points: number;
    children: NodeData[];
  };
  
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
  
    // Get the ID of this node (last segment of the path or a specific ID)
    readonly id: string;
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
  