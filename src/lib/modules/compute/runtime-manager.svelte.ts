/**
 * Compute Runtime Manager
 * 
 * High-level wrapper around ComputationGraphRuntime that provides:
 * - Automatic program registration in /programs/registry/
 * - Automatic program activation in /programs/active/
 * - Space-aware lifecycle management
 * - Integration with user space structure
 * 
 * This is Phase 2 of the unified user space architecture migration.
 * 
 * Usage:
 * ```typescript
 * const manager = new ComputeRuntimeManager(myComputationGraph, {
 *   enableProvenance: true,
 *   enableLineageTracking: true
 * });
 * 
 * await manager.start();  // Auto-registers, activates, and executes
 * // ...
 * await manager.stop();   // Cleanup
 * ```
 */

import { get } from 'svelte/store';
import { ComputationGraphRuntime } from './compute.svelte';
import type { ReactiveComputationGraph } from './schema';
import { 
	getProgramHash,
	registerProgram as registerProgramGlobal
} from './program-hash.svelte';
import {
	registerProgram as registerProgramInSpace,
	activateProgram,
	deactivateProgram,
	getProgram,
	type ProgramRegistryEntry
} from './kernel.svelte';
import { holsterUserPub as myPubKey } from '$lib/network/holster.svelte';

// ═══════════════════════════════════════════════════════════════════
// TYPES
// ═══════════════════════════════════════════════════════════════════

export interface RuntimeManagerOptions {
	/** Enable provenance tracking (default: true) */
	enableProvenance?: boolean;
	
	/** Enable lineage tracking for computation chains (default: true) */
	enableLineageTracking?: boolean;
	
	/** Auto-activate program after registration (default: true) */
	autoActivate?: boolean;
	
	/** Enable reactive mode (auto-recompute on changes) (default: false) */
	enableReactivity?: boolean;
	
	/** Program version (default: graph.version or '1.0.0') */
	version?: string;
	
	/** Program description (default: graph.description or '') */
	description?: string;
}

export interface RuntimeManagerStatus {
	/** Is the runtime currently running? */
	isRunning: boolean;
	
	/** Is the program registered in the registry? */
	isRegistered: boolean;
	
	/** Is the program activated? */
	isActive: boolean;
	
	/** Is reactive mode enabled? */
	isReactive: boolean;
	
	/** Program hash */
	programHash: string;
	
	/** Program ID */
	programId: string;
}

// ═══════════════════════════════════════════════════════════════════
// COMPUTE RUNTIME MANAGER CLASS
// ═══════════════════════════════════════════════════════════════════

export class ComputeRuntimeManager {
	private graph: ReactiveComputationGraph;
	private runtime: ComputationGraphRuntime;
	private programHash: string;
	private options: Required<RuntimeManagerOptions>;
	private isRunning = false;
	private isRegistered = false;
	private isActive = false;
	
	constructor(
		graph: ReactiveComputationGraph,
		options: RuntimeManagerOptions = {}
	) {
		this.graph = graph;
		
		// Compute program hash
		this.programHash = this.graph.program_hash || getProgramHash(graph);
		
		// Set defaults for options
		this.options = {
			enableProvenance: options.enableProvenance ?? true,
			enableLineageTracking: options.enableLineageTracking ?? true,
			autoActivate: options.autoActivate ?? true,
			enableReactivity: options.enableReactivity ?? false,
			version: options.version || graph.version || '1.0.0',
			description: options.description || graph.description || ''
		};
		
		// Create runtime instance
		this.runtime = new ComputationGraphRuntime(graph, {
			enableProvenance: this.options.enableProvenance
		});
		
		console.log(`[RUNTIME-MANAGER] Created manager for program: ${graph.id}`);
		console.log(`[RUNTIME-MANAGER] Program hash: ${this.programHash}`);
		console.log(`[RUNTIME-MANAGER] Options:`, this.options);
	}
	
	// ───────────────────────────────────────────────────────────────────
	// LIFECYCLE METHODS
	// ───────────────────────────────────────────────────────────────────
	
	/**
	 * Start the runtime manager
	 * 
	 * This will:
	 * 1. Register the program in /programs/registry/
	 * 2. Activate the program in /programs/active/ (if autoActivate)
	 * 3. Initialize the runtime
	 * 4. Execute the computation graph
	 * 5. Enable reactivity (if enableReactivity)
	 */
	async start(): Promise<void> {
		if (this.isRunning) {
			console.warn('[RUNTIME-MANAGER] Already running');
			return;
		}
		
		console.log('[RUNTIME-MANAGER] Starting...');
		
		try {
			// Step 1: Register program in user space
			await this.registerInSpace();
			
			// Step 2: Activate program (if enabled)
			if (this.options.autoActivate) {
				await this.activate();
			}
			
			// Step 3: Initialize runtime
			console.log('[RUNTIME-MANAGER] Initializing runtime...');
			await this.runtime.initialize();
			
			// Step 4: Execute computations
			console.log('[RUNTIME-MANAGER] Executing computations...');
			await this.runtime.execute();
			
			// Step 5: Enable reactivity (if enabled)
			if (this.options.enableReactivity) {
				console.log('[RUNTIME-MANAGER] Enabling reactivity...');
				this.runtime.enableReactivity();
			}
			
			this.isRunning = true;
			console.log('[RUNTIME-MANAGER] ✅ Started successfully');
			console.log(`[RUNTIME-MANAGER] Program available at: /programs/registry/${this.programHash}`);
			
		} catch (error) {
			console.error('[RUNTIME-MANAGER] ❌ Failed to start:', error);
			throw error;
		}
	}
	
	/**
	 * Stop the runtime manager
	 * 
	 * This will:
	 * 1. Disable reactivity
	 * 2. Cleanup the runtime
	 * 3. Optionally deactivate the program
	 */
	async stop(deactivate: boolean = false): Promise<void> {
		if (!this.isRunning) {
			console.warn('[RUNTIME-MANAGER] Not running');
			return;
		}
		
		console.log('[RUNTIME-MANAGER] Stopping...');
		
		try {
			// Cleanup runtime
			await this.runtime.cleanup();
			
			// Optionally deactivate
			if (deactivate && this.isActive) {
				await this.deactivate();
			}
			
			this.isRunning = false;
			console.log('[RUNTIME-MANAGER] ✅ Stopped successfully');
			
		} catch (error) {
			console.error('[RUNTIME-MANAGER] ❌ Error during stop:', error);
			throw error;
		}
	}
	
	/**
	 * Restart the runtime manager
	 */
	async restart(): Promise<void> {
		console.log('[RUNTIME-MANAGER] Restarting...');
		await this.stop(false); // Don't deactivate
		await this.start();
	}
	
	// ───────────────────────────────────────────────────────────────────
	// PROGRAM REGISTRATION & ACTIVATION
	// ───────────────────────────────────────────────────────────────────
	
	/**
	 * Register program in user space
	 * 
	 * Writes to:
	 * - /programs/registry/{programHash}/definition
	 * - /programs/registry/{programHash}/metadata
	 * - /programs/registry/{programHash}/status
	 */
	private async registerInSpace(): Promise<void> {
		console.log('[RUNTIME-MANAGER] Registering program in user space...');
		
		try {
			// Register in global program registry (for hash lookup)
			registerProgramGlobal(this.graph);
			
			// Create registry entry
			const entry: ProgramRegistryEntry = {
				definition: this.graph,
				metadata: {
					version: this.options.version,
					description: this.options.description,
					created_at: Date.now(),
					updated_at: Date.now()
				},
				status: {
					active: false, // Will be set to true if activated
					enabled: true
				}
			};
			
			// Register in user space
			await registerProgramInSpace(this.programHash, entry);
			
			this.isRegistered = true;
			console.log('[RUNTIME-MANAGER] ✅ Program registered');
			console.log(`[RUNTIME-MANAGER]   Path: /programs/registry/${this.programHash}`);
			
		} catch (error) {
			console.error('[RUNTIME-MANAGER] ❌ Failed to register program:', error);
			throw error;
		}
	}
	
	/**
	 * Activate the program
	 * 
	 * Writes to:
	 * - /programs/active/{programHash} → reference to registry
	 */
	async activate(): Promise<void> {
		if (this.isActive) {
			console.warn('[RUNTIME-MANAGER] Program already active');
			return;
		}
		
		console.log('[RUNTIME-MANAGER] Activating program...');
		
		try {
			await activateProgram(this.programHash);
			this.isActive = true;
			console.log('[RUNTIME-MANAGER] ✅ Program activated');
			console.log(`[RUNTIME-MANAGER]   Path: /programs/active/${this.programHash}`);
			
		} catch (error) {
			console.error('[RUNTIME-MANAGER] ❌ Failed to activate program:', error);
			throw error;
		}
	}
	
	/**
	 * Deactivate the program
	 * 
	 * Writes to:
	 * - /programs/inactive/{programHash} → reference to registry
	 */
	async deactivate(): Promise<void> {
		if (!this.isActive) {
			console.warn('[RUNTIME-MANAGER] Program not active');
			return;
		}
		
		console.log('[RUNTIME-MANAGER] Deactivating program...');
		
		try {
			await deactivateProgram(this.programHash);
			this.isActive = false;
			console.log('[RUNTIME-MANAGER] ✅ Program deactivated');
			console.log(`[RUNTIME-MANAGER]   Path: /programs/inactive/${this.programHash}`);
			
		} catch (error) {
			console.error('[RUNTIME-MANAGER] ❌ Failed to deactivate program:', error);
			throw error;
		}
	}
	
	// ───────────────────────────────────────────────────────────────────
	// RUNTIME ACCESS & CONTROL
	// ───────────────────────────────────────────────────────────────────
	
	/**
	 * Execute a specific computation (or all if no ID provided)
	 */
	async execute(computationId?: string): Promise<void> {
		if (!this.isRunning) {
			throw new Error('Runtime not running. Call start() first.');
		}
		
		await this.runtime.execute(computationId);
	}
	
	/**
	 * Enable reactive mode
	 */
	enableReactivity(): void {
		if (!this.isRunning) {
			throw new Error('Runtime not running. Call start() first.');
		}
		
		this.runtime.enableReactivity();
		this.options.enableReactivity = true;
	}
	
	/**
	 * Disable reactive mode
	 */
	disableReactivity(): void {
		if (!this.isRunning) {
			return;
		}
		
		this.runtime.disableReactivity();
		this.options.enableReactivity = false;
	}
	
	/**
	 * Get a variable value
	 */
	getVariable(name: string): any {
		return this.runtime.getVariable(name);
	}
	
	/**
	 * Set a variable value (for 'value' type variables)
	 */
	setVariable(name: string, value: any): void {
		this.runtime.setVariable(name, value);
	}
	
	/**
	 * Get computation result
	 */
	getComputationResult(computationId: string, outputKey: string): any {
		return this.runtime.getComputationResult(computationId, outputKey);
	}
	
	/**
	 * Get all results from a computation
	 */
	getComputationResults(computationId: string): Record<string, any> {
		return this.runtime.getComputationResults(computationId);
	}
	
	/**
	 * Update local state
	 */
	updateLocalState(updates: Record<string, any>): void {
		this.runtime.updateLocalState(updates);
	}
	
	// ───────────────────────────────────────────────────────────────────
	// STATUS & DIAGNOSTICS
	// ───────────────────────────────────────────────────────────────────
	
	/**
	 * Get current status
	 */
	getStatus(): RuntimeManagerStatus {
		return {
			isRunning: this.isRunning,
			isRegistered: this.isRegistered,
			isActive: this.isActive,
			isReactive: this.runtime.isReactive(),
			programHash: this.programHash,
			programId: this.graph.id
		};
	}
	
	/**
	 * Get program hash
	 */
	getProgramHash(): string {
		return this.programHash;
	}
	
	/**
	 * Get program ID
	 */
	getProgramId(): string {
		return this.graph.id;
	}
	
	/**
	 * Get program graph
	 */
	getGraph(): ReactiveComputationGraph {
		return this.graph;
	}
	
	/**
	 * Get runtime instance (for advanced usage)
	 */
	getRuntime(): ComputationGraphRuntime {
		return this.runtime;
	}
	
	/**
	 * Get program from registry
	 */
	getRegistryEntry(): ProgramRegistryEntry | null {
		return getProgram(this.programHash);
	}
	
	// ───────────────────────────────────────────────────────────────────
	// PROVENANCE METHODS
	// ───────────────────────────────────────────────────────────────────
	
	/**
	 * Get provenance for a computation
	 */
	getProvenance(computationId: string) {
		return this.runtime.getProvenance(computationId);
	}
	
	/**
	 * Get all provenance records
	 */
	getAllProvenance() {
		return this.runtime.getAllProvenance();
	}
	
	/**
	 * Get latest data from canonical path
	 */
	async getLatest(holsterPath: string, fromUser?: string): Promise<any | null> {
		return await this.runtime.getLatest(holsterPath, fromUser);
	}
	
	/**
	 * Get all versions of data
	 */
	async getAllVersions(holsterPath: string, fromUser?: string): Promise<any[]> {
		return await this.runtime.getAllVersions(holsterPath, fromUser);
	}
	
	/**
	 * Get specific version by provenance signature
	 */
	async getVersion(
		holsterPath: string,
		provenanceSignature: string,
		fromUser?: string
	): Promise<any | null> {
		return await this.runtime.getVersion(holsterPath, provenanceSignature, fromUser);
	}
	
	/**
	 * Verify a computation result
	 */
	async verifyComputationResult(
		computationId: string,
		claimedResult: any,
		provenance: any
	): Promise<boolean> {
		return await this.runtime.verifyComputationResult(computationId, claimedResult, provenance);
	}
}

// ═══════════════════════════════════════════════════════════════════
// CONVENIENCE FUNCTIONS
// ═══════════════════════════════════════════════════════════════════

/**
 * Deploy a program with sensible defaults
 * 
 * This is the recommended way to deploy a reactive computation program.
 * 
 * @param graph - The computation graph to deploy
 * @param options - Optional configuration
 * @returns A running ComputeRuntimeManager instance
 * 
 * @example
 * ```typescript
 * const manager = await deployProgram({
 *   id: 'my_program',
 *   variables: { ... },
 *   computations: [ ... ]
 * });
 * 
 * // Program is now registered, activated, and running!
 * ```
 */
export async function deployProgram(
	graph: ReactiveComputationGraph,
	options: RuntimeManagerOptions = {}
): Promise<ComputeRuntimeManager> {
	const manager = new ComputeRuntimeManager(graph, {
		enableProvenance: true,
		enableLineageTracking: true,
		autoActivate: true,
		enableReactivity: false,
		...options
	});
	
	await manager.start();
	
	return manager;
}

/**
 * Deploy a reactive program (auto-recomputes on input changes)
 * 
 * @param graph - The computation graph to deploy
 * @param options - Optional configuration
 * @returns A running ComputeRuntimeManager instance with reactivity enabled
 * 
 * @example
 * ```typescript
 * const manager = await deployReactiveProgram({
 *   id: 'allocation_v2',
 *   variables: {
 *     myCommitment: {
 *       type: 'subscription',
 *       holster_path: 'allocation/commitment',
 *       schema_type: 'Commitment'
 *     }
 *   },
 *   computations: [ ... ]
 * });
 * 
 * // Program automatically recomputes when myCommitment changes!
 * ```
 */
export async function deployReactiveProgram(
	graph: ReactiveComputationGraph,
	options: RuntimeManagerOptions = {}
): Promise<ComputeRuntimeManager> {
	const manager = new ComputeRuntimeManager(graph, {
		enableProvenance: true,
		enableLineageTracking: true,
		autoActivate: true,
		enableReactivity: true,
		...options
	});
	
	await manager.start();
	
	return manager;
}

/**
 * List all active programs
 * 
 * Returns program hashes for all currently active programs.
 * These can be used to query the registry for full program details.
 */
export async function listActivePrograms(): Promise<string[]> {
	// TODO: Query /programs/active/ from Holster
	// For now, return empty array
	console.warn('[RUNTIME-MANAGER] listActivePrograms() not yet implemented');
	return [];
}

/**
 * List all registered programs
 * 
 * Returns program hashes for all registered programs.
 */
export async function listRegisteredPrograms(): Promise<string[]> {
	// TODO: Query /programs/registry/ from Holster
	// For now, return empty array
	console.warn('[RUNTIME-MANAGER] listRegisteredPrograms() not yet implemented');
	return [];
}

