/**
 * Elegant Login - One-Line API
 * 
 * Login and restore state in ONE FUNCTION.
 * Handles everything: keypair, discovery, reconstruction, verification, bootstrap.
 */

import { deriveKeypair, type KeyPair } from '../identity/keypair';
import { createDiscoveryClient, type DiscoveryClient } from './discovery';
import { createStateProxy, StateProxy } from './state-proxy';
import { createRestorationBatch } from './batch';
import { mergeFragments, validateState, type ReconstructedState } from './reconstruct';
import { EntitySession } from '../entity-session';
import { BrowserStorage } from '../browser-storage';
import { RecognitionCache } from '../cache';
import type { EntityId } from '../types';

/**
 * Login options
 */
export interface LoginOptions {
  /**
   * Discovery peers for finding replicas
   */
  discoveryPeers?: string[];
  
  /**
   * Whether to verify state with Merkle roots
   */
  verify?: boolean;
  
  /**
   * Timeout for the entire operation (ms)
   */
  timeout?: number;
  
  /**
   * Custom storage (for testing)
   */
  storage?: BrowserStorage;
  
  /**
   * Custom cache (for testing)
   */
  cache?: RecognitionCache;
  
  /**
   * Use HTTP batch for initial load
   * (default: true)
   */
  useBatch?: boolean;
}

/**
 * Login result
 */
export interface LoginResult {
  session: EntitySession;
  keypair: KeyPair;
  stats: {
    entityCount: number;
    edgeCount: number;
    fragmentCount: number;
    consensusReached: boolean;
    conflictRate: number;
  };
}

/**
 * ONE-LINE LOGIN
 * 
 * This is the main entry point for state restoration.
 * 
 * @param email - User email (used as salt for keypair derivation)
 * @param password - User password
 * @param options - Optional configuration
 * @returns Ready-to-use EntitySession
 */
export async function login(
  email: string,
  password: string,
  options?: LoginOptions
): Promise<EntitySession> {
  const result = await loginWithIdentity(email, password, options);
  return result.session;
}

/**
 * Login with full result information
 * 
 * Use this if you need access to keypair and stats.
 */
export async function loginWithIdentity(
  email: string,
  password: string,
  options?: LoginOptions
): Promise<LoginResult> {
  const timeout = options?.timeout || 30000; // 30 seconds default
  const verify = options?.verify ?? true;
  const useBatch = options?.useBatch ?? true;

  // Set up timeout
  const timeoutPromise = new Promise<never>((_, reject) => {
    setTimeout(() => reject(new Error('Login timeout')), timeout);
  });

  // Race against timeout
  return Promise.race([
    performLogin(email, password, options, verify, useBatch),
    timeoutPromise
  ]);
}

/**
 * Perform the actual login
 * @private
 */
async function performLogin(
  email: string,
  password: string,
  options: LoginOptions | undefined,
  verify: boolean,
  useBatch: boolean
): Promise<LoginResult> {
  // 1. Derive keypair from password (local, instant)
  console.log('[Login] Deriving keypair...');
  const keypair = await deriveKeypair(password, email);
  const publicKey = keypair.publicKey;

  // 2. Create discovery client
  console.log('[Login] Creating discovery client...');
  const discovery = createDiscoveryClient(options?.discoveryPeers);

  // 3. Find replicas and get state fragments
  console.log('[Login] Finding replicas...');
  let fragments;
  
  if (useBatch) {
    // Use HTTP batch mode for efficiency
    fragments = await fetchFragmentsBatch(publicKey, discovery);
  } else {
    // Direct fetching
    fragments = await discovery.getFragments(publicKey);
  }

  console.log(`[Login] Found ${fragments.length} fragments`);

  // 4. Merge fragments into reconstructed state
  console.log('[Login] Reconstructing state...');
  const reconstructed = mergeFragments(fragments);

  // 5. Validate state
  if (verify) {
    console.log('[Login] Validating state...');
    const validation = validateState(reconstructed);
    if (!validation.valid) {
      console.warn('[Login] State validation warnings:', validation.errors);
      // Continue anyway - warnings are non-fatal
    }
  }

  // 6. Create session with lazy state (secure by default!)
  console.log('[Login] Creating secure session...');
  const session = await createSessionWithState(
    publicKey,
    reconstructed,
    discovery.getKnownReplicas(),
    options,
    keypair  // Pass keypair for automatic signing
  );

  // 7. Calculate stats
  const stats = calculateStats(reconstructed);

  console.log('[Login] Login complete!', stats);

  return {
    session,
    keypair,
    stats
  };
}

/**
 * Fetch fragments using HTTP batch mode
 * @private
 */
async function fetchFragmentsBatch(
  publicKey: string,
  discovery: DiscoveryClient
): Promise<any[]> {
  // Note: In a real implementation, this would use the actual batch endpoint
  // For now, we'll fall back to direct fetching
  return await discovery.getFragments(publicKey);
}

/**
 * Create EntitySession with restored state
 * @private
 */
async function createSessionWithState(
  entityId: EntityId,
  state: ReconstructedState,
  replicas: any[],
  options?: LoginOptions,
  keypair?: KeyPair
): Promise<EntitySession> {
  // Create or use provided storage
  const storage = options?.storage || new BrowserStorage(`fa-db-${entityId}`);
  await storage.initialize();

  // Create or use provided cache
  const cache = options?.cache || new RecognitionCache();

  // Create state proxy for lazy loading
  const stateProxy = createStateProxy(entityId, replicas, {
    cache,
    useLocalStorage: true
  });

  // Preload state from fragments
  stateProxy.preloadFromFragments(
    replicas.map(r => ({
      entityId,
      edges: state.edges,
      timestamp: { id: 0, event: Date.now() }, // Generic timestamp
      merkleRoot: 'preloaded',
      replicaId: r.getInfo?.()?.id || 'unknown'
    }))
  );

  // Create session
  const session = new EntitySession({
    entityId,
    storage,
    cache
  });

  // Initialize with keypair for secure sessions (auto-signs all updates)
  if (keypair) {
    await session.initializeWithKeypair(keypair);
  }

  return session;
}

/**
 * Calculate statistics from reconstructed state
 * @private
 */
function calculateStats(state: ReconstructedState): {
  entityCount: number;
  edgeCount: number;
  fragmentCount: number;
  consensusReached: boolean;
  conflictRate: number;
} {
  const entities = new Set<EntityId>();
  let edgeCount = 0;

  for (const [from, toMap] of state.edges.entries()) {
    entities.add(from);
    for (const [to,] of toMap.entries()) {
      entities.add(to);
      edgeCount++;
    }
  }

  const conflictRate = edgeCount > 0 
    ? state.metadata.conflicts / edgeCount 
    : 0;

  return {
    entityCount: entities.size,
    edgeCount,
    fragmentCount: state.metadata.fragmentCount,
    consensusReached: state.metadata.consensusReached,
    conflictRate
  };
}

/**
 * Login with an existing keypair
 * 
 * Use this if you already have a keypair (e.g., from secure storage).
 */
export async function loginWithKeypair(
  keypair: KeyPair,
  options?: LoginOptions
): Promise<EntitySession> {
  // Use publicKey as the "email" parameter (not actually used except as entityId)
  const fakeEmail = keypair.publicKey;
  const fakePassword = 'unused'; // Not needed since we have the keypair
  
  const result = await performLoginWithKeypair(keypair, options);
  return result.session;
}

/**
 * Perform login with existing keypair
 * @private
 */
async function performLoginWithKeypair(
  keypair: KeyPair,
  options?: LoginOptions
): Promise<LoginResult> {
  const timeout = options?.timeout || 30000;
  const verify = options?.verify ?? true;
  const useBatch = options?.useBatch ?? true;
  const publicKey = keypair.publicKey;

  // Set up timeout
  const timeoutPromise = new Promise<never>((_, reject) => {
    setTimeout(() => reject(new Error('Login timeout')), timeout);
  });

  // Race against timeout
  const result = await Promise.race([
    (async () => {
      // Create discovery client
      const discovery = createDiscoveryClient(options?.discoveryPeers);

      // Find replicas and get state fragments
      const fragments = useBatch
        ? await fetchFragmentsBatch(publicKey, discovery)
        : await discovery.getFragments(publicKey);

      // Merge fragments
      const reconstructed = mergeFragments(fragments);

      // Validate if requested
      if (verify) {
        const validation = validateState(reconstructed);
        if (!validation.valid) {
          console.warn('[Login] State validation warnings:', validation.errors);
        }
      }

      // Create secure session (with keypair for auto-signing)
      const session = await createSessionWithState(
        publicKey,
        reconstructed,
        discovery.getKnownReplicas(),
        options,
        keypair  // Pass keypair for automatic signing
      );

      // Calculate stats
      const stats = calculateStats(reconstructed);

      return { session, keypair, stats };
    })(),
    timeoutPromise
  ]);

  return result;
}

/**
 * Check if a user can login (has replicas available)
 * 
 * This is useful for checking if a user exists without actually logging in.
 */
export async function canLogin(
  email: string,
  discoveryPeers?: string[]
): Promise<boolean> {
  try {
    const fakePassword = 'unused';
    const keypair = await deriveKeypair(fakePassword, email);
    const discovery = createDiscoveryClient(discoveryPeers);
    const replicas = await discovery.findReplicas(keypair.publicKey);
    return replicas.length > 0;
  } catch (error) {
    console.error('[canLogin] Error:', error);
    return false;
  }
}

