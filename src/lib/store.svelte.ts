import {
  type RecNode,
  type RecognitionStore,
  type NodeEntry,
} from "../stores/rec";

export class NodeStoreHandler {
  store: RecognitionStore;

  // Track subscriptions to store data
  public unsubscribers = $state<(() => void)[]>([]);

  // State properties
  public name = $state("");
  public points = $state(0);
  public contributors = $state<NodeEntry[]>([]);
  public childrenData = $state<NodeEntry[]>([]);
  public childContributors = $state<Map<string, NodeEntry[]>>(new Map());

  // Derived hierarchy data
  public hierarchyData = $derived(this.computeHierarchyData());

  constructor(store: RecognitionStore) {
    this.store = store;
    // Don't automatically subscribe - let caller handle this
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

    // Pre-populate child contributors data
    this.childContributors.clear();
    for (const [childId, _] of this.childrenData) {
      const childStore = this.store.getChild(childId);
      const childContributors = this.get(childStore.contributorsStore) || [];
      if (childContributors.length > 0) {
        this.childContributors.set(childId, childContributors);
      }
    }

    // Set up data subscriptions with careful update triggers
    this.unsubscribers = [
      // Children subscription
      this.store.childrenStore.subscribe((newChildrenData) => {
        if (
          JSON.stringify(this.childrenData) !== JSON.stringify(newChildrenData)
        ) {
          this.childrenData = newChildrenData;
          console.log(
            performance.now(),
            "childrenData updated",
            newChildrenData
          );

          // We need to manage subscriptions to each child's contributors
          // Update our subscriptions when children change
          this.subscribeToChildContributors(newChildrenData);
        }
      }),

      // Name subscription
      this.store.nameStore.subscribe((newName) => {
        if (this.name !== newName) {
          this.name = newName || "";
          /*console.log(
            performance.now(),
            "name updated",
            newName
          );*/
        }
      }),

      // Points subscription
      this.store.pointsStore.subscribe((newPoints) => {
        if (this.points !== newPoints) {
          this.points = typeof newPoints === "number" ? newPoints : 0;
          /*console.log(
            performance.now(),
            "points updated",
            newPoints
          );*/
        }
      }),

      // Contributors subscription
      this.store.contributorsStore.subscribe((newContributors) => {
        if (
          JSON.stringify(this.contributors) !== JSON.stringify(newContributors)
        ) {
          this.contributors = newContributors;
          console.log("contributors updated", newContributors);
        }
      }),
    ];

    // Initialize subscriptions to all child contributors
    this.subscribeToChildContributors(this.childrenData);
  }

  // Separate method to handle child contributor subscriptions
  private childContributorUnsubscribers = $state<Map<string, () => void>>(
    new Map()
  );

  private subscribeToChildContributors(childrenData: NodeEntry[]) {
    // Clean up any existing child contributor subscriptions
    for (const unsub of this.childContributorUnsubscribers.values()) {
      unsub();
    }
    this.childContributorUnsubscribers.clear();

    // Clear existing child contributors data
    this.childContributors.clear();

    // Create new subscriptions for each child
    for (const [childId, _] of childrenData) {
      const childStore = this.store.getChild(childId);
      const unsubscribe = childStore.contributorsStore.subscribe(
        (newContributors) => {
          // Store the child's contributors data
          this.childContributors.set(childId, newContributors);

          // When a child's contributors change, log but don't immediately refresh
          console.log(
            "childStore contributors updated",
            childId,
            newContributors
          );

          // Only trigger refresh if this isn't an empty update
          // This reduces overhead during rapid growth operations
          if (newContributors.length > 0) {
            // Use debounced refresh to avoid disrupting growth operations
            this.debouncedRefreshHierarchy();
          }
        }
      );

      // Store the unsubscribe function for cleanup
      this.childContributorUnsubscribers.set(childId, unsubscribe);
    }
  }

  // Debounce timer for hierarchy refresh
  private refreshDebounceTimer: number | null = null;

  // Debounced refresh to avoid too many updates
  private debouncedRefreshHierarchy() {
    // Clear existing timer
    if (this.refreshDebounceTimer !== null) {
      clearTimeout(this.refreshDebounceTimer);
    }

    // Set new timer
    this.refreshDebounceTimer = window.setTimeout(() => {
      this.refreshHierarchyData();
      this.refreshDebounceTimer = null;
    }, 150); // 150ms debounce time
  }

  // Force a refresh of the hierarchy data
  refreshHierarchyData() {
    // Check if we need to trigger a refresh at all
    // This helps prevent disrupting growth operations
    if (document.querySelector(".growing")) {
      console.log("Skipping hierarchy refresh during active growth");
      return;
    }

    // With our reactive approach, we just need to trigger a change in one of the
    // state properties that hierarchyData depends on.
    // We can use a small trick to trigger reactivity without changing actual data

    // Create a new map with the same data to trigger reactivity
    const newMap = new Map<string, NodeEntry[]>();
    for (const [key, value] of this.childContributors.entries()) {
      newMap.set(key, value);
    }
    this.childContributors = newMap;
  }

  // Method to compute the hierarchy data reactively
  private computeHierarchyData(): RecNode {
    const path = this.store?.path || [];
    const rootKey = path[path.length - 1] || "root";

    // Build hierarchy data using reactive properties
    const hierarchyData: RecNode = {
      name: this.name,
      points: this.points,
      _key: rootKey,
      contributors: this.contributors.map(([id]) => id),
      children: this.childrenData.map(([id, data]) => {
        // Get contributors for this child
        const childContributorIds = this.childContributors.has(id)
          ? this.childContributors.get(id)?.map(([cid]) => cid) || []
          : [];

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

    // Log hierarchy data occasionally for debugging
    if (Math.random() < 0.05) {
      // Only log ~5% of the time to reduce noise
      console.log("Computed hierarchyData:", {
        name: hierarchyData.name,
        childrenCount: hierarchyData.children?.length || 0,
        childrenWithContributors:
          hierarchyData.children?.filter(
            (child) => (child.contributors?.length || 0) > 0
          ).length || 0,
      });
    }

    return hierarchyData;
  }

  // This method is now only used for backward compatibility or manual refresh
  getHierarchyData(): RecNode {
    // Just return the computed hierarchy data
    return this.computeHierarchyData();
  }

  // Clean up all subscriptions
  destroy() {
    this.unsubscribers.forEach((unsub) => unsub?.());
    this.unsubscribers = [];

    // Also clean up child contributor subscriptions
    for (const unsub of this.childContributorUnsubscribers.values()) {
      unsub();
    }
    this.childContributorUnsubscribers.clear();
  }

  // Force a full data refresh by re-fetching all child data
  forceDataRefresh() {
    console.log("Forcing full data refresh");

    // Clear debounce timer if active
    if (this.refreshDebounceTimer !== null) {
      clearTimeout(this.refreshDebounceTimer);
      this.refreshDebounceTimer = null;
    }

    // Re-fetch all child data directly from the store
    // This bypasses any caching issues
    if (this.store) {
      // Get fresh data from all children
      for (const [childId, _] of this.childrenData) {
        const childStore = this.store.getChild(childId);

        // Synchronously get fresh data
        const freshContributors = this.get(childStore.contributorsStore) || [];

        // Update our cache
        this.childContributors.set(childId, freshContributors);
      }

      // Trigger refresh with the fresh data
      this.refreshHierarchyData();
    }
  }
}
