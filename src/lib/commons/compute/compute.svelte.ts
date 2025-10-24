/**
 * Reactive Computation Runtime
 * 
 * Executes declarative computation graphs defined via ReactiveComputationGraphSchema.
 * 
 * Features:
 * - Variable binding (values, subscriptions, local state, derived)
 * - Automatic subscription management
 * - Dependency-aware computation ordering
 * - Output persistence (Holster, local state, memory)
 * - Reactive re-computation on input changes
 * 
 * Architecture:
 * 1. Parse computation graph
 * 2. Set up variable bindings (create stores for subscriptions)
 * 3. Register computation functions
 * 4. Execute computations in topological order
 * 5. Persist outputs according to bindings
 */

import { writable, derived, get, type Readable, type Writable } from 'svelte/store';
import type {
	ReactiveComputationGraph,
	VariableBinding,
	OutputBinding,
	Computation,
	ComputationProvenance,
	InputProvenance,
	OutputProvenance
} from './schema';
import { createStore, type HolsterStore } from '../utils/store.svelte';
import { getSchema } from './node-store.svelte';
import { holsterUser } from '$lib/state/holster.svelte';
import { holsterUserPub as myPubKey } from '$lib/state/holster.svelte';
import { getMyITCStamp, incrementMyITCStamp } from '../algorithm-v2.svelte';
import { 
	getProgramHash, 
	prefixHolsterPath, 
	buildProgramDataPath,
	registerProgram,
	hashContent,
	hashComputation,
	createDeterministicHash,
	createProvenanceSignature,
	buildProvenancePath
} from './program-hash.svelte';

// ═══════════════════════════════════════════════════════════════════
// COMPUTATION FUNCTION REGISTRY
// ═══════════════════════════════════════════════════════════════════

/**
 * Registry of named computation functions
 * Maps function names to actual implementations
 */
const computationFunctionRegistry = new Map<string, Function>();

/**
 * Register a computation function
 * 
 * @param name - Function name (used in compute_fn field)
 * @param fn - The actual function to execute
 */
export function registerComputationFunction(name: string, fn: Function): void {
	computationFunctionRegistry.set(name, fn);
	console.log(`[REACTIVE-COMPUTE] Registered function: ${name}`);
}

/**
 * Get a registered computation function
 */
export function getComputationFunction(name: string): Function | null {
	return computationFunctionRegistry.get(name) || null;
}

/**
 * List all registered functions
 */
export function listComputationFunctions(): string[] {
	return Array.from(computationFunctionRegistry.keys());
}

// ═══════════════════════════════════════════════════════════════════
// VARIABLE RESOLUTION
// ═══════════════════════════════════════════════════════════════════

/**
 * Resolved variable - stores + subscription management
 */
interface ResolvedVariable {
	type: 'value' | 'subscription' | 'fetch' | 'local' | 'derived';
	store: Readable<any>;
	cleanup?: () => void;
}

/**
 * Resolve a variable binding to a readable store
 * 
 * @param programHash - Program hash to prefix holster paths
 */
async function resolveVariableBinding(
	binding: VariableBinding,
	localState: any,
	derivedResults: Map<string, Map<string, any>>,
	programHash?: string
): Promise<ResolvedVariable> {
	switch (binding.type) {
		case 'value': {
			// Static value - wrap in store
			const store = writable(binding.value);
			return { type: 'value', store };
		}
		
		case 'subscription': {
			// Holster subscription - create store
			const schema = getSchema(binding.schema_type);
			if (!schema) {
				console.error(`[REACTIVE-COMPUTE] Unknown schema type: ${binding.schema_type}`);
				const fallback = writable(binding.default_value ?? null);
				return { type: 'subscription', store: fallback };
			}
			
			// Prefix holster path with program hash
			const holsterPath = programHash 
				? prefixHolsterPath(programHash, binding.holster_path)
				: binding.holster_path;
			
			const holsterStore = createStore({
				holsterPath,
				schema: schema,
				persistDebounce: 0
			});
			
			if (!holsterStore) {
				console.error(`[REACTIVE-COMPUTE] Failed to create store for ${holsterPath}`);
				const fallback = writable(binding.default_value ?? null);
				return { type: 'subscription', store: fallback };
			}
			
			// Initialize store
			holsterStore.initialize();
			
			// Subscribe to user if specified
			if (binding.subscribe_to_user) {
				holsterStore.subscribeToUser(binding.subscribe_to_user, (data) => {
					console.log(`[REACTIVE-COMPUTE] Received data from ${binding.subscribe_to_user?.slice(0, 20)}...`);
				});
			}
			
			// Cleanup function
			const cleanup = () => holsterStore.cleanup();
			
			return { type: 'subscription', store: holsterStore, cleanup };
		}
		
		case 'fetch': {
			// One-time Holster fetch (non-reactive, uses .get())
			const schema = getSchema(binding.schema_type);
			if (!schema) {
				console.error(`[REACTIVE-COMPUTE] Unknown schema type: ${binding.schema_type}`);
				const fallback = writable(binding.default_value ?? null);
				return { type: 'fetch', store: fallback };
			}
			
			// Prefix holster path with program hash
			const holsterPath = programHash 
				? prefixHolsterPath(programHash, binding.holster_path)
				: binding.holster_path;
			
			// Fetch data using Holster.get()
			const fetchData = async (): Promise<any> => {
				if (!holsterUser.is) {
					console.warn('[REACTIVE-COMPUTE] Not authenticated, cannot fetch');
					return binding.default_value ?? null;
				}
				
				return new Promise((resolve) => {
					// Build Holster path
					let holsterRef = binding.fetch_from_user
						? holsterUser.get([binding.fetch_from_user, holsterPath])
						: holsterUser.get(holsterPath);
					
					// Set timeout for wait period
					const timeoutMs = binding.wait_ms ?? 100;
					const timeout = setTimeout(() => {
						console.log(`[REACTIVE-COMPUTE] Fetch timeout after ${timeoutMs}ms: ${holsterPath}`);
						resolve(binding.default_value ?? null);
					}, timeoutMs);
					
					// Perform one-time get
					holsterRef.get((data: any) => {
						clearTimeout(timeout);
						
						if (!data || data === null) {
							console.log(`[REACTIVE-COMPUTE] Fetch returned null: ${holsterPath}`);
							resolve(binding.default_value ?? null);
							return;
						}
						
						// Remove Holster metadata
						const { _updatedAt, ...actualData } = data;
						
						// Validate with schema
						const validation = schema.safeParse(actualData);
						if (validation.success) {
							console.log(`[REACTIVE-COMPUTE] Fetched data from ${holsterPath}`);
							resolve(validation.data);
						} else {
							console.warn(`[REACTIVE-COMPUTE] Fetch validation failed:`, validation.error);
							resolve(binding.default_value ?? null);
						}
					});
				});
			};
			
			// Fetch immediately and wait for result
			const value = await fetchData();
			const store = writable(value);
			
			return { type: 'fetch', store };
		}
		
		case 'local': {
			// Read from local state path
			const pathParts = binding.state_path.split('.');
			let value = localState;
			
			for (const part of pathParts) {
				value = value?.[part];
			}
			
			if (value === undefined) {
				value = binding.default_value ?? null;
			}
			
			const store = writable(value);
			return { type: 'local', store };
		}
		
		case 'derived': {
			// Reference derived computation result
			const computationResults = derivedResults.get(binding.computation_id);
			const value = computationResults?.get(binding.output_key) ?? binding.default_value ?? null;
			
			const store = writable(value);
			return { type: 'derived', store };
		}
	}
}

// ═══════════════════════════════════════════════════════════════════
// OUTPUT HANDLING
// ═══════════════════════════════════════════════════════════════════

/**
 * Handle output binding - persist computation result
 * 
 * @param programHash - Program hash to prefix holster paths
 */
async function handleOutputBinding(
	outputKey: string,
	value: any,
	binding: OutputBinding,
	localState: any,
	programHash?: string
): Promise<void> {
	switch (binding.type) {
		case 'holster': {
			// Persist to Holster
			const schema = binding.schema_type ? getSchema(binding.schema_type) : null;
			if (!schema) {
				console.warn(`[REACTIVE-COMPUTE] No schema for output ${outputKey}, using 'Any'`);
			}
			
			// Prefix holster path with program hash
			const holsterPath = programHash 
				? prefixHolsterPath(programHash, binding.holster_path)
				: binding.holster_path;
			
			const store = createStore({
				holsterPath,
				schema: schema || getSchema('Any')!,
				persistDebounce: binding.persist_debounce_ms ?? 0
			});
			
			if (store) {
				store.initialize();
				store.set(value);
				console.log(`[REACTIVE-COMPUTE] Persisted ${outputKey} to ${holsterPath}`);
			}
			break;
		}
		
		case 'local': {
			// Store in local state
			const pathParts = binding.state_path.split('.');
			let current = localState;
			
			for (let i = 0; i < pathParts.length - 1; i++) {
				const part = pathParts[i];
				if (!current[part]) {
					current[part] = {};
				}
				current = current[part];
			}
			
			current[pathParts[pathParts.length - 1]] = value;
			console.log(`[REACTIVE-COMPUTE] Stored ${outputKey} to local state: ${binding.state_path}`);
			break;
		}
		
		case 'memory': {
			// Keep in memory only (handled by caller)
			console.log(`[REACTIVE-COMPUTE] Keeping ${outputKey} in memory only`);
			break;
		}
	}
}

// ═══════════════════════════════════════════════════════════════════
// COMPUTATION EXECUTION
// ═══════════════════════════════════════════════════════════════════

/**
 * Execute a single computation
 * 
 * @param programHash - Program hash to prefix holster paths
 */
async function executeComputation(
	computation: Computation,
	variables: Map<string, ResolvedVariable>,
	localState: any,
	derivedResults: Map<string, Map<string, any>>,
	programHash?: string
): Promise<Map<string, any>> {
	console.log(`[REACTIVE-COMPUTE] Executing computation: ${computation.id}`);
	
	// Get computation function
	const fn = getComputationFunction(computation.compute_fn);
	if (!fn) {
		console.error(`[REACTIVE-COMPUTE] Unknown computation function: ${computation.compute_fn}`);
		return new Map();
	}
	
	// Resolve inputs
	const inputValues: Record<string, any> = {};
	
	for (const [inputName, inputBinding] of Object.entries(computation.inputs)) {
		// Check if it's a variable reference
		const variable = variables.get(inputName);
		if (variable) {
			inputValues[inputName] = get(variable.store);
		} else {
			// Treat as inline binding
			const resolved = await resolveVariableBinding(inputBinding, localState, derivedResults, programHash);
			inputValues[inputName] = get(resolved.store);
			
			// Cleanup inline binding
			resolved.cleanup?.();
		}
	}
	
	// Resolve local bindings (variables scoped to this computation)
	const localBindings: Record<string, any> = {};
	if (computation.local_bindings) {
		for (const [localName, localBinding] of Object.entries(computation.local_bindings)) {
			const resolved = await resolveVariableBinding(localBinding, localState, derivedResults, programHash);
			localBindings[localName] = get(resolved.store);
			
			// Cleanup local binding
			resolved.cleanup?.();
		}
	}
	
	// Merge inputs and local bindings
	const allInputs = { ...inputValues, ...localBindings };
	
	// Execute computation
	let result: any;
	try {
		result = await fn(allInputs);
	} catch (error) {
		console.error(`[REACTIVE-COMPUTE] Error executing ${computation.id}:`, error);
		return new Map();
	}
	
	// Store results in memory
	const outputResults = new Map<string, any>();
	for (const [outputKey, outputBinding] of Object.entries(computation.outputs)) {
		const value = result[outputKey];
		outputResults.set(outputKey, value);
		
		// Handle output binding
		await handleOutputBinding(outputKey, value, outputBinding, localState, programHash);
	}
	
	return outputResults;
}

/**
 * Topological sort of computations based on dependencies
 */
function topologicalSort(computations: Computation[]): Computation[] {
	const sorted: Computation[] = [];
	const visited = new Set<string>();
	const visiting = new Set<string>();
	
	const visit = (comp: Computation) => {
		if (visited.has(comp.id)) return;
		if (visiting.has(comp.id)) {
			throw new Error(`Circular dependency detected: ${comp.id}`);
		}
		
		visiting.add(comp.id);
		
		// Visit dependencies first
		if (comp.depends_on) {
			for (const depId of comp.depends_on) {
				const dep = computations.find(c => c.id === depId);
				if (dep) {
					visit(dep);
				}
			}
		}
		
		// Check for implicit dependencies (derived inputs)
		for (const inputBinding of Object.values(comp.inputs)) {
			if (inputBinding.type === 'derived') {
				const dep = computations.find(c => c.id === inputBinding.computation_id);
				if (dep) {
					visit(dep);
				}
			}
		}
		
		visiting.delete(comp.id);
		visited.add(comp.id);
		sorted.push(comp);
	};
	
	for (const comp of computations) {
		visit(comp);
	}
	
	return sorted;
}

// ═══════════════════════════════════════════════════════════════════
// COMPUTATION GRAPH RUNTIME
// ═══════════════════════════════════════════════════════════════════

/**
 * Reactive Computation Graph Runtime
 * 
 * Manages execution of a declarative computation graph
 */
export class ComputationGraphRuntime {
	private graph: ReactiveComputationGraph;
	private programHash: string;
	private variables = new Map<string, ResolvedVariable>();
	private derivedResults = new Map<string, Map<string, any>>();
	private localState: any = {};
	private isRunning = false;
	private debounceTimeouts = new Map<string, ReturnType<typeof setTimeout>>();
	private subscriptionUnsubscribers: (() => void)[] = [];
	private reactiveMode = false;
	
	// Provenance tracking
	private provenanceLog = new Map<string, ComputationProvenance>();
	private enableProvenance = true; // Track provenance by default
	
	constructor(graph: ReactiveComputationGraph, options?: { enableProvenance?: boolean }) {
		this.graph = graph;
		
		// Get or compute program hash
		this.programHash = getProgramHash(graph);
		
		// Register program in global registry
		registerProgram(graph);
		
		// Configure provenance tracking
		if (options?.enableProvenance !== undefined) {
			this.enableProvenance = options.enableProvenance;
		}
		
		console.log(`[REACTIVE-COMPUTE] Created runtime for program: ${graph.id} (hash: ${this.programHash})`);
		console.log(`[REACTIVE-COMPUTE] Provenance tracking: ${this.enableProvenance ? 'enabled' : 'disabled'}`);
	}
	
	/**
	 * Initialize the runtime
	 * - Set up variable bindings
	 * - Create subscriptions
	 */
	async initialize(): Promise<void> {
		console.log(`[REACTIVE-COMPUTE] Initializing graph: ${this.graph.id}`);
		console.log(`[REACTIVE-COMPUTE] Program hash: ${this.programHash}`);
		console.log(`[REACTIVE-COMPUTE] All data will be namespaced under: ${this.programHash}/*`);
		
		// Resolve all variables
		for (const [varName, binding] of Object.entries(this.graph.variables)) {
			const resolved = await resolveVariableBinding(binding, this.localState, this.derivedResults, this.programHash);
			this.variables.set(varName, resolved);
			console.log(`[REACTIVE-COMPUTE] Resolved variable: ${varName} (${binding.type})`);
		}
		
		console.log(`[REACTIVE-COMPUTE] Initialized ${this.variables.size} variables`);
	}
	
	/**
	 * Execute all computations in dependency order
	 * 
	 * @param computationId - Optional: Execute only this computation (and dependencies)
	 */
	async execute(computationId?: string): Promise<void> {
		if (this.isRunning) {
			console.warn('[REACTIVE-COMPUTE] Computation already running, skipping');
			return;
		}
		
		this.isRunning = true;
		
		try {
			// Get computations to execute
			let computationsToExecute = this.graph.computations;
			
			if (computationId) {
				// Execute only specific computation (and dependencies)
				const computation = computationsToExecute.find(c => c.id === computationId);
				if (!computation) {
					console.error(`[REACTIVE-COMPUTE] Computation not found: ${computationId}`);
					return;
				}
				console.log(`[REACTIVE-COMPUTE] Executing computation: ${computationId}`);
				computationsToExecute = [computation];
			} else {
				console.log(`[REACTIVE-COMPUTE] Executing ${this.graph.computations.length} computations`);
			}
			
			// Sort computations topologically
			const sorted = topologicalSort(computationsToExecute);
			
			// Execute in order
			for (const computation of sorted) {
				if (!computation.enabled) {
					console.log(`[REACTIVE-COMPUTE] Skipping disabled computation: ${computation.id}`);
					continue;
				}
				
				// Handle debounce
				if (computation.debounce_ms && computation.debounce_ms > 0) {
					await this.executeWithDebounce(computation);
				} else {
					await this.executeComputationInternal(computation);
				}
			}
			
			console.log('[REACTIVE-COMPUTE] Execution complete');
		} catch (error) {
			console.error('[REACTIVE-COMPUTE] Execution error:', error);
		} finally {
			this.isRunning = false;
		}
	}
	
	/**
	 * Execute a single computation (internal helper)
	 */
	private async executeComputationInternal(computation: Computation): Promise<void> {
		// Increment ITC stamp before execution (peer causality)
		if (this.enableProvenance) {
			incrementMyITCStamp();
		}
		
		// Track input provenance
		const inputProvenance = new Map<string, InputProvenance>();
		
		if (this.enableProvenance) {
			// Collect input data and hash it
			for (const [name, binding] of Object.entries(computation.inputs)) {
				let value: any;
				let source: 'value' | 'subscription' | 'fetch' | 'local' | 'derived' = binding.type;
				let path: string | undefined;
				
				// Get the actual value
				if (binding.type === 'local') {
					value = this.localState[binding.state_path];
				} else if (binding.type === 'derived') {
					const derivedData = this.derivedResults.get(binding.computation_id);
					value = derivedData?.get(binding.output_key);
					source = 'derived';
				} else {
					const variable = this.variables.get(name);
					value = variable ? get(variable.store) : undefined;
				}
				
				// Extract path if applicable
				if (binding.type === 'subscription') {
					path = binding.holster_path;
				} else if (binding.type === 'fetch') {
					path = binding.holster_path;
				}
				
				// Create input provenance
				inputProvenance.set(name, {
					source,
					path,
					contentHash: hashContent(value),
					provenance: undefined // TODO: Track parent provenance for derived inputs
				});
			}
		}
		
		// Execute computation
				const results = await executeComputation(
					computation,
					this.variables,
					this.localState,
			this.derivedResults,
			this.programHash
				);
				
				// Store derived results
				this.derivedResults.set(computation.id, results);
		
		// Create provenance record
		if (this.enableProvenance) {
			const myPub = get(myPubKey);
			const currentITC = getMyITCStamp();
			
			// Hash outputs
			const outputProvenance = new Map<string, OutputProvenance>();
			for (const [name, binding] of Object.entries(computation.outputs)) {
				const outputValue = results.get(name);
				outputProvenance.set(name, {
					path: binding.type === 'holster' ? binding.holster_path : '',
					contentHash: hashContent(outputValue)
				});
			}
			
			// Create deterministic hash
			const computationHash = hashComputation(computation);
			const inputHashes = Object.fromEntries(
				Array.from(inputProvenance.entries()).map(([name, prov]) => [name, prov.contentHash])
			);
			const deterministicHash = createDeterministicHash(computationHash, inputHashes);
			
			// Create provenance ID (unique identifier for this execution)
			const provenanceId = `${this.programHash}_${computation.id}_${Date.now()}`;
			
			// Create full provenance record (V2: using ITC stamps)
			const provenance: ComputationProvenance = {
				id: provenanceId,
				itcStamp: currentITC,
				executedBy: (myPub || 'unknown') as string,
				timestamp: Date.now(),
				programHash: this.programHash,
				computationId: computation.id,
				computationHash,
				inputs: Object.fromEntries(inputProvenance),
				outputs: Object.fromEntries(outputProvenance),
				deterministicHash,
				parents: [] // TODO: Track parent provenance IDs
			};
			
			// Store provenance
			this.provenanceLog.set(computation.id, provenance);
			
		console.log(`[PROVENANCE] Created provenance for ${computation.id}`);
		console.log(`[PROVENANCE]   Deterministic hash: ${deterministicHash}`);
		console.log(`[PROVENANCE]   ITC stamp: ${JSON.stringify(currentITC)}`);
		
		// Store outputs with hybrid storage strategy
		for (const [name, binding] of Object.entries(computation.outputs)) {
			if (binding.type === 'holster') {
				const outputValue = results.get(name);
				const timestamp = Date.now();
				const provenanceSig = createProvenanceSignature(provenance);
				
				// Prepare data with provenance metadata
				const dataWithProvenance = {
					data: outputValue,
					_provenance: provenance,
					_updatedAt: timestamp
				};
				
				// 1. Store at CANONICAL path (latest, easy to query)
				const canonicalPath = prefixHolsterPath(this.programHash, binding.holster_path);
				
				if (myPub) {
					// Cross-user: use array format [pubkey, path]
					holsterUser.get([myPub, canonicalPath]).put(dataWithProvenance);
					console.log(`[PROVENANCE] Canonical: ~${myPub}/${canonicalPath}`);
				} else {
					// Current user: use string path
					holsterUser.get(canonicalPath).put(dataWithProvenance);
					console.log(`[PROVENANCE] Canonical: ${canonicalPath}`);
				}
				
				// 2. Store at VERSIONED path (immutable history)
				const versionPath = `${canonicalPath}/_versions/${provenanceSig}`;
				const immutableData = {
					...dataWithProvenance,
					_immutable: true
				};
				
				if (myPub) {
					holsterUser.get([myPub, versionPath]).put(immutableData);
					console.log(`[PROVENANCE] Version: ~${myPub}/${versionPath}`);
				} else {
					holsterUser.get(versionPath).put(immutableData);
					console.log(`[PROVENANCE] Version: ${versionPath}`);
				}
				
				// 3. Update LATEST index (pointer to current version)
				const latestIndexPath = `${this.programHash}/_index/latest/${binding.holster_path}`;
				
				if (myPub) {
					holsterUser.get([myPub, latestIndexPath]).put(provenanceSig);
				} else {
					holsterUser.get(latestIndexPath).put(provenanceSig);
				}
				
				// 4. Update COMPUTATIONS index (all outputs of this computation)
				const compIndexPath = `${this.programHash}/_index/computations/${computation.id}`;
				
				if (myPub) {
					holsterUser.get([myPub, compIndexPath]).set(binding.holster_path);
				} else {
					holsterUser.get(compIndexPath).set(binding.holster_path);
				}
				
				// 5. Update LINEAGE index (provenance lineage for traversal)
				const lineageIndexPath = `${this.programHash}/_index/lineage/${provenance.id}`;
				const lineageData = {
					computationId: computation.id,
					programHash: this.programHash,
					inputs: Object.keys(provenance.inputs),
					outputs: [binding.holster_path],
					timestamp: timestamp,
					itcStamp: currentITC
				};
				
				if (myPub) {
					holsterUser.get([myPub, lineageIndexPath]).put(lineageData);
				} else {
					holsterUser.get(lineageIndexPath).put(lineageData);
				}
				
				console.log(`[PROVENANCE] ✅ Hybrid storage complete for ${binding.holster_path}`);
				console.log(`[PROVENANCE]   - Canonical path (latest)`);
				console.log(`[PROVENANCE]   - Versioned path (immutable)`);
				console.log(`[PROVENANCE]   - Latest index updated`);
				console.log(`[PROVENANCE]   - Computation index updated`);
				console.log(`[PROVENANCE]   - Lineage index updated`);
			}
		}
	}
	}
	
	/**
	 * Execute a computation with debounce
	 */
	private async executeWithDebounce(computation: Computation): Promise<void> {
		// Clear existing timeout
		const existingTimeout = this.debounceTimeouts.get(computation.id);
		if (existingTimeout) {
			clearTimeout(existingTimeout);
		}
		
		// Set new timeout
		return new Promise((resolve) => {
			const timeout = setTimeout(async () => {
				await this.executeComputationInternal(computation);
				this.debounceTimeouts.delete(computation.id);
				resolve();
			}, computation.debounce_ms);
			
			this.debounceTimeouts.set(computation.id, timeout);
		});
	}
	
	/**
	 * Enable reactive mode - automatically re-execute when inputs change
	 */
	enableReactivity(): void {
		if (this.reactiveMode) {
			console.warn('[REACTIVE-COMPUTE] Reactivity already enabled');
			return;
		}
		
		console.log('[REACTIVE-COMPUTE] Enabling reactivity');
		this.reactiveMode = true;
		
		// Subscribe to all subscription-type variables
		for (const [varName, variable] of this.variables) {
			if (variable.type === 'subscription') {
				const unsubscribe = variable.store.subscribe((value) => {
					console.log(`[REACTIVE-COMPUTE] Variable changed: ${varName}, triggering re-execution`);
					
					// Find computations that depend on this variable
					const affectedComputations = this.graph.computations.filter(comp => {
						return Object.keys(comp.inputs).includes(varName);
					});
					
					// Re-execute affected computations
					for (const comp of affectedComputations) {
						if (comp.enabled) {
							this.execute(comp.id).catch(err => {
								console.error(`[REACTIVE-COMPUTE] Error re-executing ${comp.id}:`, err);
							});
						}
					}
				});
				
				this.subscriptionUnsubscribers.push(unsubscribe);
			}
		}
		
		console.log(`[REACTIVE-COMPUTE] Watching ${this.subscriptionUnsubscribers.length} variables`);
	}
	
	/**
	 * Disable reactive mode
	 */
	disableReactivity(): void {
		if (!this.reactiveMode) {
			return;
		}
		
		console.log('[REACTIVE-COMPUTE] Disabling reactivity');
		this.reactiveMode = false;
		
		// Unsubscribe from all variables
		for (const unsubscribe of this.subscriptionUnsubscribers) {
			unsubscribe();
		}
		this.subscriptionUnsubscribers = [];
	}
	
	/**
	 * Get current value of a variable
	 */
	getVariable(name: string): any {
		const variable = this.variables.get(name);
		return variable ? get(variable.store) : null;
	}
	
	/**
	 * Get result of a computation
	 */
	getComputationResult(computationId: string, outputKey: string): any {
		return this.derivedResults.get(computationId)?.get(outputKey) ?? null;
	}
	
	/**
	 * Get all results from a computation
	 */
	getComputationResults(computationId: string): Record<string, any> {
		const results = this.derivedResults.get(computationId);
		return results ? Object.fromEntries(results.entries()) : {};
	}
	
	/**
	 * Set a variable value (for 'value' type variables)
	 */
	setVariable(name: string, value: any): void {
		const variable = this.variables.get(name);
		if (variable && variable.type === 'value') {
			(variable.store as any).set(value);
			console.log(`[REACTIVE-COMPUTE] Updated variable: ${name}`);
		} else {
			console.warn(`[REACTIVE-COMPUTE] Cannot set non-value variable: ${name}`);
		}
	}
	
	/**
	 * Update local state
	 */
	updateLocalState(updates: Record<string, any>): void {
		Object.assign(this.localState, updates);
		console.log('[REACTIVE-COMPUTE] Updated local state');
	}
	
	/**
	 * Cleanup - unsubscribe and cleanup stores
	 */
	async cleanup(): Promise<void> {
		console.log('[REACTIVE-COMPUTE] Cleaning up runtime');
		
		// Disable reactivity
		this.disableReactivity();
		
		// Clear all debounce timeouts
		for (const timeout of this.debounceTimeouts.values()) {
			clearTimeout(timeout);
		}
		this.debounceTimeouts.clear();
		
		// Cleanup all variable subscriptions
		for (const [name, variable] of this.variables) {
			variable.cleanup?.();
		}
		
		this.variables.clear();
		this.derivedResults.clear();
		this.localState = {};
		
		console.log('[REACTIVE-COMPUTE] Cleanup complete');
	}
	
	/**
	 * Check if runtime is in reactive mode
	 */
	isReactive(): boolean {
		return this.reactiveMode;
	}
	
	/**
	 * Get the program hash
	 */
	getProgramHash(): string {
		return this.programHash;
	}
	
	// ═══════════════════════════════════════════════════════════════════
	// PROVENANCE QUERY METHODS
	// ═══════════════════════════════════════════════════════════════════
	
	/**
	 * Get provenance record for a computation
	 */
	getProvenance(computationId: string): ComputationProvenance | undefined {
		return this.provenanceLog.get(computationId);
	}
	
	/**
	 * Get latest data from canonical path
	 * 
	 * This is the recommended way to query computation results in RDL programs.
	 * Returns data with provenance metadata.
	 */
	async getLatest(holsterPath: string, fromUser?: string): Promise<any | null> {
		const canonicalPath = prefixHolsterPath(this.programHash, holsterPath);
		
		try {
			if (fromUser) {
				// Query specific user's data
				return await new Promise((resolve) => {
					holsterUser.get([fromUser, canonicalPath]).once((data: any) => {
						resolve(data || null);
					});
				});
			} else {
				// Query current user's data
				return await new Promise((resolve) => {
					holsterUser.get(canonicalPath).once((data: any) => {
						resolve(data || null);
					});
				});
			}
		} catch (error) {
			console.error(`[PROVENANCE-QUERY] Error fetching latest from ${holsterPath}:`, error);
			return null;
		}
	}
	
	/**
	 * Get all versions of data from version history
	 * 
	 * Returns array of all versions with their provenance.
	 */
	async getAllVersions(holsterPath: string, fromUser?: string): Promise<any[]> {
		const canonicalPath = prefixHolsterPath(this.programHash, holsterPath);
		const versionPath = `${canonicalPath}/_versions`;
		
		try {
			return await new Promise((resolve) => {
				const versions: any[] = [];
				
				const ref = fromUser 
					? holsterUser.get([fromUser, versionPath])
					: holsterUser.get(versionPath);
				
				ref.map().once((data: any, key: string) => {
					if (data && key !== '_') {
						versions.push({ ...data, _versionKey: key });
					}
				});
				
				// Give Gun time to collect all versions
				setTimeout(() => resolve(versions), 100);
			});
		} catch (error) {
			console.error(`[PROVENANCE-QUERY] Error fetching versions from ${holsterPath}:`, error);
			return [];
		}
	}
	
	/**
	 * Get specific version by provenance signature
	 */
	async getVersion(
		holsterPath: string,
		provenanceSignature: string,
		fromUser?: string
	): Promise<any | null> {
		const canonicalPath = prefixHolsterPath(this.programHash, holsterPath);
		const versionPath = `${canonicalPath}/_versions/${provenanceSignature}`;
		
		try {
			return await new Promise((resolve) => {
				const ref = fromUser
					? holsterUser.get([fromUser, versionPath])
					: holsterUser.get(versionPath);
				
				ref.once((data: any) => {
					resolve(data || null);
				});
			});
		} catch (error) {
			console.error(`[PROVENANCE-QUERY] Error fetching version ${provenanceSignature}:`, error);
			return null;
		}
	}
	
	/**
	 * Get all outputs of a specific computation
	 * 
	 * Uses the computations index for fast lookup.
	 */
	async getComputationOutputs(
		computationId: string,
		fromUser?: string
	): Promise<string[]> {
		const indexPath = `${this.programHash}/_index/computations/${computationId}`;
		
		try {
			return await new Promise((resolve) => {
				const outputs: string[] = [];
				
				const ref = fromUser
					? holsterUser.get([fromUser, indexPath])
					: holsterUser.get(indexPath);
				
				ref.map().once((path: string) => {
					if (path && typeof path === 'string') {
						outputs.push(path);
					}
				});
				
				setTimeout(() => resolve(outputs), 100);
			});
		} catch (error) {
			console.error(`[PROVENANCE-QUERY] Error fetching computation outputs:`, error);
			return [];
		}
	}
	
	/**
	 * Get lineage information for a provenance ID
	 * 
	 * Returns information about inputs and outputs of a computation execution.
	 */
	async getLineage(provenanceId: string, fromUser?: string): Promise<any | null> {
		const lineagePath = `${this.programHash}/_index/lineage/${provenanceId}`;
		
		try {
			return await new Promise((resolve) => {
				const ref = fromUser
					? holsterUser.get([fromUser, lineagePath])
					: holsterUser.get(lineagePath);
				
				ref.once((data: any) => {
					resolve(data || null);
				});
			});
		} catch (error) {
			console.error(`[PROVENANCE-QUERY] Error fetching lineage:`, error);
			return null;
		}
	}
	
	/**
	 * Trace computation lineage upstream (what produced this?)
	 * 
	 * Follows the chain of computations that led to a result.
	 */
	async traceLineageUpstream(
		provenanceId: string,
		maxDepth: number = 10,
		fromUser?: string
	): Promise<any[]> {
		const chain: any[] = [];
		let currentId = provenanceId;
		let depth = 0;
		
		while (currentId && depth < maxDepth) {
			const lineage = await this.getLineage(currentId, fromUser);
			if (!lineage) break;
			
			chain.push(lineage);
			
			// Get parent provenance ID from inputs
			// (Would need to store parent IDs in lineage data)
			// For now, stop at first level
			break;
		}
		
		return chain;
	}
	
	/**
	 * Get all provenance records
	 */
	getAllProvenance(): Map<string, ComputationProvenance> {
		return new Map(this.provenanceLog);
	}
	
	/**
	 * Verify a computation result against its provenance
	 * 
	 * Checks:
	 * - Deterministic hash matches (computation + inputs → expected result)
	 * - Output hash matches claimed result
	 * 
	 * Returns true if verification passes
	 */
	async verifyComputationResult(
		computationId: string,
		claimedResult: any,
		provenance: ComputationProvenance
	): Promise<boolean> {
		console.log(`[PROVENANCE-VERIFY] Verifying computation: ${computationId}`);
		
		// 1. Verify deterministic hash
		const expectedHash = createDeterministicHash(
			provenance.computationHash,
			Object.fromEntries(
				Object.entries(provenance.inputs).map(([name, inp]) => [name, inp.contentHash])
			)
		);
		
		if (expectedHash !== provenance.deterministicHash) {
			console.error('[PROVENANCE-VERIFY] ❌ Deterministic hash mismatch');
			console.error(`  Expected: ${expectedHash}`);
			console.error(`  Got: ${provenance.deterministicHash}`);
			return false;
		}
		
		console.log(`[PROVENANCE-VERIFY] ✅ Deterministic hash valid: ${expectedHash}`);
		
		// 2. Find computation in graph
		const computation = this.graph.computations.find(c => c.id === computationId);
		if (!computation) {
			console.error(`[PROVENANCE-VERIFY] ❌ Computation not found: ${computationId}`);
			return false;
		}
		
		// 3. Verify computation hash matches
		const currentComputationHash = hashComputation(computation);
		if (currentComputationHash !== provenance.computationHash) {
			console.warn('[PROVENANCE-VERIFY] ⚠️  Computation definition has changed');
			console.warn(`  Current hash: ${currentComputationHash}`);
			console.warn(`  Provenance hash: ${provenance.computationHash}`);
			// This might be ok - just means different version
			// Continue verification...
		}
		
		// 4. Verify output hash
		const resultHash = hashContent(claimedResult);
		
		// Find the first output (or could check all outputs)
		const firstOutput = Object.values(provenance.outputs)[0];
		if (!firstOutput) {
			console.error('[PROVENANCE-VERIFY] ❌ No outputs in provenance');
			return false;
		}
		
		const expectedOutputHash = firstOutput.contentHash;
		
		if (resultHash !== expectedOutputHash) {
			console.error('[PROVENANCE-VERIFY] ❌ Output hash mismatch');
			console.error(`  Expected: ${expectedOutputHash}`);
			console.error(`  Got: ${resultHash}`);
			return false;
		}
		
		console.log(`[PROVENANCE-VERIFY] ✅ Output hash valid: ${resultHash}`);
		console.log(`[PROVENANCE-VERIFY] ✅ Verification passed for ${computationId}`);
		
		return true;
	}
	
	/**
	 * Re-execute a computation and compare with provenance
	 * 
	 * WARNING: This requires having the same inputs available
	 */
	async reExecuteAndVerify(
		computationId: string,
		provenance: ComputationProvenance
	): Promise<boolean> {
		console.log(`[PROVENANCE-VERIFY] Re-executing ${computationId} for verification`);
		
		// Find computation
		const computation = this.graph.computations.find(c => c.id === computationId);
		if (!computation) {
			console.error(`[PROVENANCE-VERIFY] ❌ Computation not found`);
			return false;
		}
		
		// TODO: Set up inputs to match provenance.inputs
		// This is complex because we'd need to restore the exact input state
		// For now, just verify hashes
		
		console.warn('[PROVENANCE-VERIFY] Re-execution not yet implemented, using hash verification');
		
		// Get current result from derived results
		const currentResult = this.derivedResults.get(computationId);
		if (!currentResult) {
			console.error('[PROVENANCE-VERIFY] ❌ No current result available');
			return false;
		}
		
		// Verify first output
		const firstOutputName = Object.keys(provenance.outputs)[0];
		const firstOutputValue = currentResult.get(firstOutputName);
		
		return await this.verifyComputationResult(computationId, firstOutputValue, provenance);
	}
}

// ═══════════════════════════════════════════════════════════════════
// CONVENIENCE FUNCTIONS
// ═══════════════════════════════════════════════════════════════════

/**
 * Create and execute a computation graph
 * 
 * Convenience function that initializes and executes in one call
 */
export async function executeComputationGraph(
	graph: ReactiveComputationGraph
): Promise<ComputationGraphRuntime> {
	const runtime = new ComputationGraphRuntime(graph);
	await runtime.initialize();
	await runtime.execute();
	return runtime;
}

/**
 * Create a reactive computation that auto-runs when inputs change
 * 
 * Returns a cleanup function to stop reactivity
 */
export async function createReactiveComputation(
	graph: ReactiveComputationGraph
): Promise<{ runtime: ComputationGraphRuntime; cleanup: () => void }> {
	const runtime = new ComputationGraphRuntime(graph);
	await runtime.initialize();
	await runtime.execute();
	
	// Enable reactivity - auto-recompute when subscription variables change
	runtime.enableReactivity();
	
	const cleanup = async () => {
		await runtime.cleanup();
	};
	
	return { runtime, cleanup };
}

