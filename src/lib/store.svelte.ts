import {
  type RecNode,
  type RecognitionStore,
  type NodeEntry,
} from "../stores/rec";

export class NodeStoreHandler {
  store: RecognitionStore;

  // Track subscriptions to store data
  private unsubscribers = $state<(() => void)[]>([]);

  // State properties
  public name = $state("");
  public points = $state(0);
  public contributors = $state<NodeEntry[]>([]);
  public childrenData = $state<NodeEntry[]>([]);

  // Derived hierarchy data
  public hierarchyData = $derived(this.getHierarchyData());

  constructor(store: RecognitionStore) {
    this.store = store;
    this.subscribeToStoreData();
  }

  // Helper to get store value synchronously without subscription side effects
  private get<T>(store: {
    subscribe: (callback: (value: T) => void) => () => void;
  }): T | undefined {
    let value: T | undefined;
    const unsubscribe = store.subscribe((v) => {
      value = v;
    });
    unsubscribe();
    return value;
  }

  subscribeToStoreData() {
    // Clean up previous subscriptions first
    this.destroy();

    // Only subscribe if we have a valid store
    if (!this.store) {
      console.warn("No store provided to NodeStoreHandler");
      return;
    }

    // Pre-fetch data to avoid recursive subscription triggers
    this.childrenData = this.get(this.store.childrenStore) || [];
    this.name = this.get(this.store.nameStore) || "";
    this.points = this.get(this.store.pointsStore) || 0;
    this.contributors = this.get(this.store.contributorsStore) || [];

    // Set up data subscriptions with careful update triggers
    this.unsubscribers = [
      // Children subscription
      this.store.childrenStore.subscribe((newChildrenData) => {
        if (
          JSON.stringify(this.childrenData) !== JSON.stringify(newChildrenData)
        ) {
          this.childrenData = newChildrenData;
          // Child subscriptions are handled automatically via derived data
        }
      }),

      // Name subscription
      this.store.nameStore.subscribe((newName) => {
        if (this.name !== newName) {
          this.name = newName || "";
        }
      }),

      // Points subscription
      this.store.pointsStore.subscribe((newPoints) => {
        if (this.points !== newPoints) {
          this.points = typeof newPoints === "number" ? newPoints : 0;
        }
      }),

      // Contributors subscription
      this.store.contributorsStore.subscribe((newContributors) => {
        if (
          JSON.stringify(this.contributors) !== JSON.stringify(newContributors)
        ) {
          this.contributors = newContributors;
        }
      }),
    ];
  }

  // Clean up all subscriptions
  destroy() {
    this.unsubscribers.forEach((unsub) => unsub?.());
    this.unsubscribers = [];
  }

  getHierarchyData() {
    // Collect all necessary data upfront to avoid triggering reactive updates during processing
    const nodeDataMap = new Map();
    const hierarchyData: RecNode = {
      name: this.name,
      points: this.points,
      _key: this.store.path[this.store.path.length - 1] || "root",
      contributors: this.contributors.map(([id]) => id), // Use the already fetched data
      children: this.childrenData.map(([id, data]) => {
        // Get child store but don't subscribe
        const childStore = this.store.getChild(id);

        // Get contributors - use the map if available to avoid redundant fetches
        let childContributorIds;
        if (nodeDataMap.has(id)) {
          childContributorIds = nodeDataMap.get(id);
        } else {
          // Fetch synchronously without subscription
          const childContributors =
            this.get(childStore.contributorsStore) || [];
          childContributorIds = childContributors.map(([cid]) => cid);
          nodeDataMap.set(id, childContributorIds);
        }

        return {
          ...data,
          _key: id,
          // Ensure points is a number
          points: typeof data.points === "number" ? data.points : 0,
          // Add contributors directly to the node structure
          contributors: childContributorIds,
        } satisfies RecNode;
      }),
    };

    return hierarchyData;
  }
}
