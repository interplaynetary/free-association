import {
  type RecNode,
  type RecognitionStore,
  type NodeEntry,
} from "../stores/rec";

export class NodeStoreHandler {
  store: RecognitionStore;

  // Single array to track all dynamic subscriptions
  private nodeSubscriptions: (() => void)[] = [];

  // Track subscriptions to store data
  private unsubscribeChildren: (() => void) | undefined;
  private unsubscribeName: (() => void) | undefined;
  private unsubscribePoints: (() => void) | undefined;
  private unsubscribeContributors: (() => void) | undefined;

  public name: string = $state("");
  public points: number = $state(0);
  public contributors: NodeEntry[] = $state([]);
  public childrenData: NodeEntry[] = $state([]);

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
    console.log("subscribeToStoreData");
    // Clean up previous subscriptions first
    if (this.unsubscribeChildren) this.unsubscribeChildren();
    if (this.unsubscribeName) this.unsubscribeName();
    if (this.unsubscribePoints) this.unsubscribePoints();
    if (this.unsubscribeContributors) this.unsubscribeContributors();

    // Clean up node subscriptions
    this.nodeSubscriptions.forEach((unsub) => unsub());
    this.nodeSubscriptions = [];

    // Only subscribe if we have a valid store
    if (!this.store) {
      console.warn("No store provided to TreeMap");
      return;
    }

    // Pre-fetch data to avoid recursive subscription triggers
    // This gets the current state before setting up new subscriptions
    this.childrenData = this.get(this.store.childrenStore) || [];
    this.name = this.get(this.store.nameStore) || "";
    this.points = this.get(this.store.pointsStore) || 0;
    this.contributors = this.get(this.store.contributorsStore) || [];

    // Now, set up data subscriptions with careful update triggers
    this.unsubscribeChildren = this.store.childrenStore.subscribe(
      (newChildrenData) => {
        // Only update if data actually changed
        if (
          JSON.stringify(this.childrenData) !== JSON.stringify(newChildrenData)
        ) {
          this.childrenData = newChildrenData;
          // Setup unified data subscriptions without triggering updates
          this.setupNodeSubscriptions();
          // Schedule a single tree data update
          this.requestDataUpdate();
        }
      }
    );

    this.unsubscribeName = this.store.nameStore.subscribe((newName) => {
      console.log("subscriptionName", newName);
      if (this.name !== newName) {
        this.name = newName || "";
        this.requestDataUpdate();
      }
    });

    this.unsubscribePoints = this.store.pointsStore.subscribe((newPoints) => {
      console.log("subscriptionPoints", newPoints);
      if (this.points !== newPoints) {
        this.points = typeof newPoints === "number" ? newPoints : 0;
        this.requestDataUpdate();
      }
    });

    // Subscribe to contributors for the current node
    this.unsubscribeContributors = this.store.contributorsStore.subscribe(
      (newContributors) => {
        console.log("subscriptionContributors", newContributors);
        // Only update if contributors actually changed
        if (
          JSON.stringify(this.contributors) !== JSON.stringify(newContributors)
        ) {
          this.contributors = newContributors;
          // Update hierarchy data if it exists without triggering a full rebuild
        }
      }
    );

    // Initial update of visualization (only once after all data is available)
    this.requestDataUpdate();
  }

  // Setup subscriptions for all nodes in a unified way
  setupNodeSubscriptions() {
    // Clean up existing subscriptions
    this.nodeSubscriptions.forEach((unsub) => unsub());
    this.nodeSubscriptions = [];

    // Subscribe to current node and all children
    if (this.childrenData && this.childrenData.length) {
      this.childrenData.forEach(([childId]) => {
        const childStore = this.store.getChild(childId);

        // Pre-fetch current contributors to avoid triggering updates
        const currentContributors =
          this.get(childStore.contributorsStore) || [];

        // Subscribe to child contributors
        const contributorSub = childStore.contributorsStore.subscribe(
          (contributors) => {
            // Skip initial update to avoid recursion (first call after subscription)
            if (
              JSON.stringify(contributors) ===
              JSON.stringify(currentContributors)
            ) {
              return;
            }
          }
        );

        // Store subscription for cleanup
        this.nodeSubscriptions.push(contributorSub);
      });
    }
  }

  requestDataUpdate() {
    console.log("requestDataUpdate");
    // Schedule a single tree data update
  }

  // Clean up all subscriptions
  destroy() {
    if (this.unsubscribeChildren) this.unsubscribeChildren();
    if (this.unsubscribeName) this.unsubscribeName();
    if (this.unsubscribePoints) this.unsubscribePoints();
    if (this.unsubscribeContributors) this.unsubscribeContributors();

    this.nodeSubscriptions.forEach((unsub) => unsub());
    this.nodeSubscriptions = [];
  }

  hierarchyData: RecNode | undefined = $derived(this.getHierarchyData());

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
