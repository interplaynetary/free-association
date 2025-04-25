

import type { CollectionItem } from "../lib/utils/svelte/reactiveStores";
import type { RecognitionStore } from "../stores/rec.svelte";

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
  