
import { z, ZodSchema } from 'zod';

/**
 * Generic Aggregation Module
 * 
 * Provides generic Graph Traversal and Aggregation capabilities.
 * Decoupled from specific "Potential" or "Economic" models.
 * 
 * Core concepts:
 * - Node: A graph node with an ID and topological relationships (parents/children).
 * - Extractor: Determines WHAT to aggregate (e.g., "capacity", "tags", "votes").
 * - Reducer: Determines HOW to aggregate (e.g., "sum", "union", "average").
 */

export type NodeId = string;

/**
 * Generic Node Interface
 * Represents any entity in the graph (Individual, Group, Resource, Task).
 */
export interface Node {
    id: NodeId;
    /** Upward relationships (e.g. memberOf, partOf) - stored as attributes */
    // memberOf?: Set<NodeId>; 
    /** Arbitrary attributes */
    [key: string]: any;
}

/**
 * Function to retrieve a node by its ID.
 * Abstracts the storage mechanism (Map, DB, etc).
 */
export type NodeGetter = (id: NodeId) => Node | undefined;

/**
 * Function to extract a value of type T from a node.
 * Returns undefined if the node doesn't have the value.
 */
/**
 * Function to extract a value of type T from a node.
 * Returns undefined if the node doesn't have the value.
 */
export type Extractor<T> = (node: Node) => T | undefined;

/**
 * Function to combine values.
 * Identical to Array.reduce signature.
 */
export type Reducer<T, R> = (accumulator: R, value: T) => R;

/**
 * Function to extract related node IDs (topology).
 */
export type RelationExtractor = (node: Node) => Set<NodeId> | undefined;

// ============================================================================
// CORE AGGREGATION FUNCTIONS
// ============================================================================

/**
 * Generic Upward Aggregation
 * 
 * Aggregates values extracted from nodes related by `getParents`.
 * 
 * @param node The starting node
 * @param getNode Function to resolve Node IDs
 * @param getParents Function to extract parent IDs from a node (Topology)
 * @param extractor Function to extract value T from a parent node
 * @param reducer Function to combine values into result R
 * @param initialValue Starting value for the reducer
 */
export function aggregateUp<T, R>(
    node: Node,
    getNode: NodeGetter,
    getParents: RelationExtractor,
    extractor: Extractor<T>,
    reducer: Reducer<T, R>,
    initialValue: R
): R {
    const parents = getParents(node);
    if (!parents || parents.size === 0) {
        return initialValue;
    }

    let acc = initialValue;

    for (const parentId of parents) {
        const parent = getNode(parentId);
        if (!parent) continue;

        const value = extractor(parent);
        if (value !== undefined) {
            acc = reducer(acc, value);
        }
    }

    return acc;
}

/**
 * Generic Downward Aggregation
 * 
 * Aggregates values extracted from children nodes.
 * 
 * @param node The starting node (the parent)
 * @param getAllNodes Function to retrieve all possible child candidates
 * @param getParents Function to extract parent IDs from a candidate child (Topology definition)
 * @param extractor Function to extract value T from a child node
 * @param reducer Function to combine values into result R
 * @param initialValue Starting value for the reducer
 */
export function aggregateDown<T, R>(
    node: Node,
    getAllNodes: () => Node[],
    getParents: RelationExtractor,
    extractor: Extractor<T>,
    reducer: Reducer<T, R>,
    initialValue: R
): R {
    let acc = initialValue;

    const allNodes = getAllNodes();
    // Optimization: If we had a reverse index, we wouldn't scan allNodes.
    // Scan nodes that list 'node.id' in their parents set.
    const children = allNodes.filter(candidate =>
        getParents(candidate)?.has(node.id)
    );

    for (const child of children) {
        const value = extractor(child);
        if (value !== undefined) {
            acc = reducer(acc, value);
        }
    }

    return acc;
}


// ============================================================================
// RECURSIVE / DEEP AGGREGATION (Optional Extensions)
// ============================================================================

/**
 * Deep Upward Aggregation (Recursive)
 * Traverses up the hierarchy until roots are reached.
 * Warning: Needs cycle detection in general graphs.
 */
export function aggregateUpDeep<T, R>(
    node: Node,
    getNode: NodeGetter,
    extractor: Extractor<T>,
    reducer: Reducer<T, R>,
    initialValue: R,
    visited: Set<NodeId> = new Set()
): R {
    if (visited.has(node.id)) return initialValue;
    visited.add(node.id);

    // 1. Extract from self (optional semantics, but usually "Deep" implies inclusive)
    // Actually, usually "Upward" means "From Context". 
    // Let's keep it strictly strictly parents to match non-deep semantics?
    // Or make it "Aggregate Ancestors".

    // Let's stick to the definition: Aggregate values OF ANCESTORS.
    if (!node.memberOf || node.memberOf.size === 0) {
        return initialValue;
    }

    let acc = initialValue;

    for (const parentId of node.memberOf) {
        const parent = getNode(parentId);
        if (!parent) continue;

        // Extract from parent
        const value = extractor(parent);
        if (value !== undefined) {
            acc = reducer(acc, value);
        }

        // Recurse
        // Note: This logic depends on whether we want to aggregate [Parent Value] + [Grandparent Value]
        // or if we just want to traverse.
        // Simple recursion:
        acc = aggregateUpDeep(parent, getNode, extractor, reducer, acc, visited);
    }

    return acc;
}


// ============================================================================
// COMMON REDUCERS
// ============================================================================

export const Reducers = {
    sum: (acc: number, val: number) => acc + val,

    concat: <T>(acc: T[], val: T | T[]) => {
        if (Array.isArray(val)) return acc.concat(val);
        return [...acc, val];
    },

    union: <T>(acc: Set<T>, val: Set<T> | T[]) => {
        const newSet = new Set(acc);
        for (const item of val) newSet.add(item);
        return newSet;
    },

    average: (acc: { sum: number; count: number }, val: number) => ({
        sum: acc.sum + val,
        count: acc.count + 1
    })
};

// ============================================================================
// EXAMPLE ADAPTERS (Backwards Compatibility / Helpers)
// ============================================================================

// Legacy Interface shim if needed, or consumers should adapt.
// We will define a specific "Potential Extractor" helper.

export interface Potential {
    type: string;
    magnitude: number;
}

export const Extractors = {
    /** Extracts all potentials of a specific type */
    potentialsByType: (targetType: string) => (node: Node): number[] | undefined => {
        if (!node.potentials || !Array.isArray(node.potentials)) return undefined;
        return (node.potentials as Potential[])
            .filter(p => p.type === targetType)
            .map(p => p.magnitude);
    },


    /** Extracts a specific attribute by key */
    attribute: (key: string) => (node: Node) => node[key],

    /** Extracts a relationship set (topology) by key */
    relations: (key: string) => (node: Node): Set<NodeId> | undefined => {
        const val = node[key];
        if (val instanceof Set) return val as Set<NodeId>;
        if (Array.isArray(val)) return new Set(val);
        return undefined;
    },

    /**
     * Extracts and validates data using a Zod schema.
     * If validation fails, returns undefined (skipping the node).
     */
    schema: <T>(schema: ZodSchema<T>) => (node: Node): T | undefined => {
        const result = schema.safeParse(node);
        return result.success ? result.data : undefined;
    }
};

// ============================================================================
// FLUENT API (ELEGANCE LAYER)
// ============================================================================

export class Graph {
    private nodeMap: Map<NodeId, Node>;

    constructor(nodes: Node[]) {
        this.nodeMap = new Map(nodes.map(n => [n.id, n]));
    }

    get(id: NodeId): Node | undefined {
        return this.nodeMap.get(id);
    }

    from(id: NodeId) {
        return new Traversal(this.get(id), (id) => this.get(id));
    }
}

export class Traversal {
    constructor(
        private startNode: Node | undefined,
        private lookup: NodeGetter
    ) { }

    /**
     * Aggregates values from ancestors (Upward).
     */
    aggregateUp<T, R>(
        relation: string,
        extractor: Extractor<T>,
        reducer: Reducer<T, R>,
        initial: R
    ): R {
        if (!this.startNode) return initial;
        return aggregateUp(
            this.startNode,
            this.lookup,
            Extractors.relations(relation),
            extractor,
            reducer,
            initial
        );
    }
}