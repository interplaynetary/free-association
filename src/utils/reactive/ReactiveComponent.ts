import { ReactiveGraph, safeGet, withErrorHandling } from './ReactiveGraph';
import type { TraversalOptions } from './ReactiveGraph';
import { writable, derived, get, type Readable, type Writable } from 'svelte/store';
import type { ComponentType, SvelteComponent } from 'svelte';

/**
 * Types of relationships a component might have with other nodes in the graph
 */
export enum ComponentRelationshipType {
  PARENT = 'parent',           // Direct parent in hierarchy
  CHILD = 'child',             // Direct child in hierarchy
  SIBLING = 'sibling',         // Same level component
  REFERENCE = 'reference',     // Referenced entity
  ORIGIN = 'origin',           // Source of data
  CONTRIBUTOR = 'contributor', // Contributing entity
  EXTENSION = 'extension',     // Extending functionality
  DEPENDENT = 'dependent',     // Depends on this component
  CUSTOM = 'custom'            // Custom relationship with specific semantics
}

/**
 * Definition of a relationship a component has with other nodes in the graph
 */
export interface ComponentRelationship {
  /** Type of relationship */
  type: ComponentRelationshipType | string;
  /** Path to related node relative to component's base path */
  relativePath: string[];
  /** Traversal options when resolving this relationship */
  traversalOptions?: TraversalOptions;
  /** Whether changes to this relationship should trigger component reactivity */
  reactive?: boolean;
  /** Depth of reactivity (how many levels of nested properties should trigger updates) */
  reactiveDepth?: number;
  /** Custom data transformation for this relationship */
  transform?: (data: any) => any;
  /** Whether to automatically traverse this relationship when component initializes */
  autoTraverse?: boolean;
  /** Metadata about the relationship */
  metadata?: Record<string, any>;
}

/**
 * Configuration for graph-aware components
 */
export interface ComponentGraphConfig {
  /** Default base path if none is provided */
  defaultPath?: string[];
  /** Relationships this component has with other nodes */
  relationships?: ComponentRelationship[];
  /** Whether component should activate on initialization */
  autoActivate?: boolean;
  /** Default depth for traversal operations */
  defaultTraversalDepth?: number;
  /** Debounce time for reactivity in milliseconds */
  debounceMs?: number;
  /** Whether to cache relationship traversal results */
  cacheRelationships?: boolean;
  /** How long relationship cache entries should live (ms) */
  cacheTtlMs?: number;
  /** Whether to use transient storage (won't persist to localStorage) */
  transient?: boolean;
}

/**
 * Properties for a graph position
 */
export interface GraphPosition {
  /** Full path to this position */
  path: string[];
  /** Depth in the graph */
  depth: number;
  /** Parent path if any */
  parentPath?: string[];
  /** Name/key of this node in parent */
  key?: string;
}

/**
 * Full context of a component's graph awareness
 */
export interface ComponentGraphContext {
  /** Component's position in the graph */
  position: GraphPosition;
  /** Graph-derived properties for the component */
  derivedProps: Record<string, any>;
  /** Relationships with other graph nodes */
  relationships: Record<string, any>;
  /** Whether component has fully initialized its graph awareness */
  initialized: boolean;
  /** Errors encountered during graph operations */
  errors: Record<string, string>;
  /** Current traversal state for async operations */
  traversalState: 'idle' | 'traversing' | 'error';
  /** Custom metadata for this component instance */
  metadata: Record<string, any>;
}

/**
 * Properties that graph-aware components can access
 */
export interface GraphAwareProps {
  /** Base path in the graph where this component is positioned */
  path?: string[];
  /** Graph configuration overrides for this specific instance */
  graphConfig?: Partial<ComponentGraphConfig>;
  /** Initial data if available */
  initialData?: any;
  /** Whether to subscribe to graph updates */
  connected?: boolean;
  /** Custom react-to paths beyond configured relationships */
  watchPaths?: string[][];
  /** Whether to use transient storage (won't persist to localStorage) */
  transient?: boolean;
}

/**
 * Main API for reactive component's graph awareness
 */
export interface ReactiveComponentApi {
  /** The current context as a readable store */
  context: Readable<ComponentGraphContext>;
  /** Initialize or reinitialize the component's graph awareness */
  initialize: (path?: string[]) => Promise<void>;
  /** Get a reactive store for a specific data property */
  getDataStore: <T>(propertyName: string, defaultValue?: T) => Readable<T>;
  /** Get a reactive store for a related node */
  getRelationshipStore: <T>(relationshipName: string, defaultValue?: T) => Readable<T>;
  /** Get a reactive store for a specific path relative to the component */
  getRelativePathStore: <T>(relativePath: string[], defaultValue?: T) => Readable<T>;
  /** Traverse to a new path relative to the component's base path */
  traverseTo: (relativePath: string[]) => Promise<string[]>;
  /** Modify data at the component's path */
  updateData: <T>(propertyName: string, value: T) => Promise<void>;
  /** Update relationship data */
  updateRelationship: (relationshipName: string, data: any) => Promise<void>;
  /** Connect a new relationship dynamically */
  addRelationship: (name: string, relationship: ComponentRelationship) => void;
  /** Remove a relationship */
  removeRelationship: (name: string) => void;
  /** Get full path from relative path */
  resolveRelativePath: (relativePath: string[]) => string[];
  /** Get the underlying ReactiveGraph instance */
  getGraph: () => ReactiveGraph;
  /** Disconnect and clean up all subscriptions */
  destroy: () => void;
}

/**
 * Main implementation of reactive component
 */
export class ReactiveComponent implements ReactiveComponentApi {
  private graph: ReactiveGraph;
  private config: ComponentGraphConfig;
  private state: Writable<ComponentGraphContext>;
  private basePath: string[] = [];
  private subscriptions: Array<() => void> = [];
  private relationshipCache: Map<string, { data: any, timestamp: number }> = new Map();
  private debouncers: Map<string, NodeJS.Timeout> = new Map();
  private connected: boolean = false;

  /**
   * Create a new reactive component
   */
  constructor(graph: ReactiveGraph, config: ComponentGraphConfig = {}) {
    this.graph = graph;
    this.config = {
      defaultPath: [],
      relationships: [],
      autoActivate: true,
      defaultTraversalDepth: 1,
      debounceMs: 200,
      cacheRelationships: true,
      cacheTtlMs: 5 * 60 * 1000, // 5 minutes
      transient: false,
      ...config
    };

    // Apply transient setting to the graph if specified
    if (this.config.transient) {
      this.graph.setTransient(this.config.transient);
    }

    // Initialize empty state
    const initialState: ComponentGraphContext = {
      position: {
        path: [],
        depth: 0
      },
      derivedProps: {},
      relationships: {},
      initialized: false,
      errors: {},
      traversalState: 'idle',
      metadata: {}
    };

    this.state = writable(initialState);

    // Create relationship name lookup map for easier access
    const relationshipMap = new Map<string, ComponentRelationship>();
    if (this.config.relationships) {
      for (const relationship of this.config.relationships) {
        const name = this.getRelationshipName(relationship);
        relationshipMap.set(name, relationship);
      }
    }
  }

  /**
   * Toggle transient mode on or off
   * @param transient Whether to use transient storage
   * @returns This instance for chaining
   */
  public setTransient(transient: boolean): this {
    this.config.transient = transient;
    this.graph.setTransient(transient);
    return this;
  }

  /**
   * Check if this component is using transient storage
   */
  public isTransient(): boolean {
    return !!this.config.transient;
  }

  /**
   * Generate a consistent name for a relationship if none is explicitly provided
   */
  private getRelationshipName(relationship: ComponentRelationship): string {
    return relationship.type + ':' + relationship.relativePath.join('/');
  }

  /**
   * Initialize the component at a specific path
   */
  public async initialize(path?: string[]): Promise<void> {
    // Clean up any existing subscriptions
    this.cleanup();

    // Set the base path
    this.basePath = path || this.config.defaultPath || [];

    // Update position info
    this.state.update(state => {
      state.position = {
        path: [...this.basePath],
        depth: this.basePath.length,
        parentPath: this.basePath.length > 0 ? this.basePath.slice(0, -1) : undefined,
        key: this.basePath.length > 0 ? this.basePath[this.basePath.length - 1] : undefined
      };
      state.initialized = false;
      state.traversalState = 'traversing';
      return state;
    });

    try {
      // Set up subscriptions for the base node
      await this.setupBaseSubscriptions();

      // Set up relationships
      if (this.config.relationships) {
        await this.setupRelationships();
      }

      // Mark as initialized
      this.state.update(state => {
        state.initialized = true;
        state.traversalState = 'idle';
        return state;
      });

      // Set as connected if auto-activate is true
      if (this.config.autoActivate) {
        this.connected = true;
      }
    } catch (err: any) {
      this.state.update(state => {
        state.traversalState = 'error';
        state.errors.initialization = err.message || 'Initialization failed';
        return state;
      });
    }
  }

  /**
   * Set up subscriptions to the base node properties
   */
  private async setupBaseSubscriptions(): Promise<void> {
    const nodeStore = this.graph.getNodeStore(this.basePath);

    // Subscribe to the node store to get base data
    const unsubscribe = nodeStore.subscribe(data => {
      if (!this.connected) return;

      this.state.update(state => {
        state.derivedProps = { ...(data as Record<string, any>) };
        return state;
      });
    });

    this.subscriptions.push(unsubscribe);
  }

  /**
   * Set up all configured relationships
   */
  private async setupRelationships(): Promise<void> {
    if (!this.config.relationships) return;

    for (const relationship of this.config.relationships) {
      await this.setupRelationship(relationship);
    }
  }

  /**
   * Set up a single relationship
   */
  private async setupRelationship(relationship: ComponentRelationship): Promise<void> {
    const relationshipName = this.getRelationshipName(relationship);
    const fullPath = this.resolveRelativePath(relationship.relativePath);

    try {
      // If the relationship should be auto-traversed, fetch immediately
      if (relationship.autoTraverse) {
        const relatedData = await this.fetchRelationshipData(relationship);

        // Update state with relationship data
        this.state.update(state => {
          state.relationships[relationshipName] = relatedData;
          return state;
        });

        // Cache the result if caching is enabled
        if (this.config.cacheRelationships) {
          this.relationshipCache.set(relationshipName, {
            data: relatedData,
            timestamp: Date.now()
          });
        }
      }

      // If the relationship is reactive, set up a subscription
      if (relationship.reactive) {
        // Determine what to subscribe to based on relationship type
        let storeToSubscribe: Readable<any>;

        if (relationship.type === ComponentRelationshipType.CHILD ||
            relationship.type === ComponentRelationshipType.CONTRIBUTOR) {
          // For collection-type relationships
          storeToSubscribe = this.graph.getCollectionStore(fullPath);
        } else {
          // For regular node relationships
          storeToSubscribe = this.graph.getNodeStore(fullPath);
        }

        // Set up the subscription
        const unsubscribe = storeToSubscribe.subscribe(data => {
          if (!this.connected) return;

          // Apply transformation if provided
          const transformedData = relationship.transform ? relationship.transform(data) : data;

          // Debounce updates for this relationship
          if (this.debouncers.has(relationshipName)) {
            clearTimeout(this.debouncers.get(relationshipName)!);
          }

          this.debouncers.set(relationshipName, setTimeout(() => {
            this.state.update(state => {
              state.relationships[relationshipName] = transformedData;
              return state;
            });

            // Update cache
            if (this.config.cacheRelationships) {
              this.relationshipCache.set(relationshipName, {
                data: transformedData,
                timestamp: Date.now()
              });
            }

            this.debouncers.delete(relationshipName);
          }, this.config.debounceMs));
        });

        this.subscriptions.push(unsubscribe);
      }
    } catch (err: any) {
      // Record the error but continue with other relationships
      this.state.update(state => {
        state.errors[relationshipName] = err.message || 'Failed to set up relationship';
        return state;
      });
    }
  }

  /**
   * Fetch data for a relationship
   */
  private async fetchRelationshipData(relationship: ComponentRelationship): Promise<any> {
    const relationshipName = this.getRelationshipName(relationship);
    const fullPath = this.resolveRelativePath(relationship.relativePath);

    // Check cache first if enabled
    if (this.config.cacheRelationships) {
      const cached = this.relationshipCache.get(relationshipName);
      if (cached && (Date.now() - cached.timestamp) < (this.config.cacheTtlMs || 0)) {
        return cached.data;
      }
    }

    // Determine how to fetch the data based on relationship type
    if (relationship.type === ComponentRelationshipType.CHILD ||
        relationship.type === ComponentRelationshipType.CONTRIBUTOR) {
      // For collection-type relationships, use getCollectionStore
      const collectionStore = this.graph.getCollectionStore(fullPath);
      const data = safeGet(collectionStore, []);
      return relationship.transform ? relationship.transform(data) : data;
    } else {
      // For regular node relationships, use getNodeStore
      const nodeStore = this.graph.getNodeStore(fullPath);
      const data = safeGet(nodeStore, null);
      return relationship.transform ? relationship.transform(data) : data;
    }
  }

  /**
   * The current component context as a readable store
   */
  public get context(): Readable<ComponentGraphContext> {
    return { subscribe: this.state.subscribe };
  }

  /**
   * Get a reactive store for a specific property at the component's path
   */
  public getDataStore<T>(propertyName: string, defaultValue?: T): Readable<T> {
    return this.graph.getNodePropertyStore<T>(
      this.basePath,
      propertyName,
      defaultValue as T
    );
  }

  /**
   * Get a reactive store for a relationship
   */
  public getRelationshipStore<T>(relationshipName: string, defaultValue?: T): Readable<T> {
    // Create a derived store based on the context's relationships
    return derived(this.state, $state => {
      const relationshipData = $state.relationships[relationshipName];
      return relationshipData !== undefined ? relationshipData : defaultValue;
    });
  }

  /**
   * Get a reactive store for a path relative to this component
   */
  public getRelativePathStore<T>(relativePath: string[], defaultValue?: T): Readable<T> {
    const fullPath = this.resolveRelativePath(relativePath);
    return this.graph.getNodeStore<T>(fullPath, defaultValue);
  }

  /**
   * Traverse to a new path relative to the component's base path
   */
  public async traverseTo(relativePath: string[]): Promise<string[]> {
    const fullPath = this.resolveRelativePath(relativePath);
    
    this.state.update(state => {
      state.traversalState = 'traversing';
      return state;
    });

    try {
      // Use the graph's traversal capabilities
      const result = await this.graph.traversePath(this.basePath, relativePath);
      
      this.state.update(state => {
        state.traversalState = 'idle';
        return state;
      });
      
      return result;
    } catch (err: any) {
      this.state.update(state => {
        state.traversalState = 'error';
        state.errors.traversal = err.message || 'Traversal failed';
        return state;
      });
      
      return this.basePath;
    }
  }

  /**
   * Update data at the component's path
   */
  public async updateData<T>(propertyName: string, value: T): Promise<void> {
    try {
      await this.graph.updateNodeProperty(this.basePath, propertyName, value);
    } catch (err: any) {
      this.state.update(state => {
        state.errors.dataUpdate = err.message || 'Update failed';
        return state;
      });
    }
  }

  /**
   * Update a relationship's data
   */
  public async updateRelationship(relationshipName: string, data: any): Promise<void> {
    // Find the relationship config
    const relationship = this.config.relationships?.find(
      r => this.getRelationshipName(r) === relationshipName
    );

    if (!relationship) {
      throw new Error(`Relationship ${relationshipName} not found`);
    }

    const fullPath = this.resolveRelativePath(relationship.relativePath);

    try {
      if (Array.isArray(data)) {
        // Handle collection updates
        // For each item in the collection, update it
        for (const item of data) {
          if (typeof item === 'object' && item.id) {
            await this.graph.updateNodeProperty(
              [...fullPath, item.id],
              'value',
              item
            );
          }
        }
      } else if (typeof data === 'object') {
        // Update each property individually
        for (const [key, value] of Object.entries(data)) {
          await this.graph.updateNodeProperty(fullPath, key, value);
        }
      } else {
        // Simple value update
        await this.graph.updateNodeProperty(fullPath, 'value', data);
      }

      // Invalidate cache for this relationship
      if (this.config.cacheRelationships) {
        this.relationshipCache.delete(relationshipName);
      }
    } catch (err: any) {
      this.state.update(state => {
        state.errors.relationshipUpdate = err.message || 'Relationship update failed';
        return state;
      });
    }
  }

  /**
   * Add a new relationship dynamically
   */
  public addRelationship(name: string, relationship: ComponentRelationship): void {
    // Add to config
    this.config.relationships = [...(this.config.relationships || []), relationship];
    
    // Set up the new relationship
    this.setupRelationship(relationship).catch(err => {
      this.state.update(state => {
        state.errors[name] = err.message || 'Failed to add relationship';
        return state;
      });
    });
  }

  /**
   * Remove a relationship
   */
  public removeRelationship(name: string): void {
    if (!this.config.relationships) return;

    // Find and remove the relationship
    const index = this.config.relationships.findIndex(
      r => this.getRelationshipName(r) === name
    );

    if (index >= 0) {
      this.config.relationships.splice(index, 1);
      
      // Remove from state
      this.state.update(state => {
        delete state.relationships[name];
        delete state.errors[name];
        return state;
      });
      
      // Remove from cache
      if (this.config.cacheRelationships) {
        this.relationshipCache.delete(name);
      }
    }
  }

  /**
   * Resolve a relative path to a full path
   */
  public resolveRelativePath(relativePath: string[]): string[] {
    return [...this.basePath, ...relativePath];
  }

  /**
   * Get the underlying ReactiveGraph instance
   */
  public getGraph(): ReactiveGraph {
    return this.graph;
  }

  /**
   * Clean up all subscriptions
   */
  private cleanup(): void {
    // Cancel all subscriptions
    for (const unsubscribe of this.subscriptions) {
      unsubscribe();
    }
    this.subscriptions = [];

    // Clear all debouncers
    for (const timerId of this.debouncers.values()) {
      clearTimeout(timerId);
    }
    this.debouncers.clear();
  }

  /**
   * Destroy this component and clean up all resources
   */
  public destroy(): void {
    this.connected = false;
    this.cleanup();
    this.relationshipCache.clear();
  }
}

/**
 * Options for creating a graph-aware component
 */
export interface GraphAwareOptions extends ComponentGraphConfig {
  /** Whether to pass the entire api to the component */
  exposeFullApi?: boolean;
  /** What specific parts of the context to pass to the component */
  expose?: Array<keyof ComponentGraphContext>;
  /** Custom prop mapping */
  propMapping?: Record<string, string>;
}

// Simplified type definitions for Svelte integration - these will be replaced
// with proper type integration in the actual Svelte implementation
type SvelteComponentProps = Record<string, any>;
type EnhancedSvelteComponent = {
  component: any;
  props: SvelteComponentProps;
};

/**
 * Create a higher-order function that wraps a component with graph awareness
 */
export function withGraphAwareness(
  graph: ReactiveGraph,
  options: GraphAwareOptions = {}
) {
  return function graphAwareHOC(Component: any) {
    return function createGraphAwareComponent(props: SvelteComponentProps & GraphAwareProps): EnhancedSvelteComponent {
      const componentOptions: ComponentGraphConfig = {
        ...options,
        ...(props.graphConfig || {})
      };

      // Handle transient setting from props (both direct and via graphConfig)
      const transient = props.transient ?? props.graphConfig?.transient ?? options.transient;
      if (transient !== undefined) {
        graph.setTransient(transient);
        componentOptions.transient = transient;
      }

      // Create the reactive component
      const reactiveComponent = new ReactiveComponent(graph, componentOptions);
      
      // Initialize with the provided path
      if (props.path) {
        reactiveComponent.initialize(props.path);
      } else if (componentOptions.defaultPath) {
        reactiveComponent.initialize(componentOptions.defaultPath);
      }

      // Create derived props for the component
      const derivedProps = derived(reactiveComponent.context, $context => {
        const result: Record<string, any> = {};
        
        // Include specific parts of the context based on options
        if (options.expose) {
          for (const key of options.expose) {
            result[key] = $context[key as keyof ComponentGraphContext];
          }
        }
        
        // Include all derived props from the graph
        result.derivedProps = $context.derivedProps;
        
        // Include relationships data
        result.relationships = $context.relationships;
        
        // Apply custom prop mapping if provided
        if (options.propMapping) {
          for (const [from, to] of Object.entries(options.propMapping)) {
            // Handle nested paths with dot notation
            const parts = from.split('.');
            let value: any = $context;
            
            for (const part of parts) {
              if (value === undefined || value === null) break;
              value = value[part];
            }
            
            result[to] = value;
          }
        }
        
        // Expose the full API if requested
        if (options.exposeFullApi) {
          result.graphApi = reactiveComponent;
        }
        
        return result;
      });

      // Clean up when component is destroyed
      onDestroy(() => {
        reactiveComponent.destroy();
      });

      // Combine the original props with the derived props
      return {
        component: Component,
        props: {
          ...props,
          ...get(derivedProps)
        }
      };
    };
  };
}

// Create a Svelte action for graph awareness
export function graphAware(
  node: HTMLElement, 
  params: {
    graph: ReactiveGraph,
    path: string[],
    config?: ComponentGraphConfig,
    transient?: boolean
  }
) {
  const { graph, path, config, transient } = params;
  
  // Set transient mode if provided
  if (transient !== undefined) {
    graph.setTransient(transient);
  }
  
  const component = new ReactiveComponent(graph, {
    ...config,
    transient
  });
  
  // Initialize the component with the provided path
  component.initialize(path);
  
  // Store the component instance on the DOM node for access in component code
  (node as any).__graphAware = component;
  
  return {
    // Handle updates to parameters
    update(newParams: typeof params) {
      if (!arraysEqual(newParams.path, path)) {
        component.initialize(newParams.path);
      }
      
      // Update transient setting if changed
      if (newParams.transient !== transient) {
        component.setTransient(!!newParams.transient);
      }
    },
    // Clean up when element is removed
    destroy() {
      component.destroy();
      delete (node as any).__graphAware;
    }
  };
}

// Helper to compare arrays
function arraysEqual(a: any[], b: any[]): boolean {
  if (a.length !== b.length) return false;
  for (let i = 0; i < a.length; i++) {
    if (a[i] !== b[i]) return false;
  }
  return true;
}

// Helper function for component destruction
function onDestroy(fn: () => void): void {
  // This would be implemented by the framework (Svelte in this case)
  // For TypeScript compilation, we provide an empty implementation
}

// Create a factory function for reactive components
export function createReactiveComponent(
  graph: ReactiveGraph,
  config: ComponentGraphConfig = {}
): ReactiveComponentApi {
  return new ReactiveComponent(graph, config);
}
