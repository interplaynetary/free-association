/**
 * Factory Functions - DRY Object Creation
 * 
 * Elegant one-line creation with automatic initialization.
 * Follows the Cap'n Web philosophy: simple, consistent, obvious.
 */

import { BrowserStorage } from './browser-storage';
import { RecognitionCache, CacheConfig, DEFAULT_CACHE_CONFIG } from './cache';
import { ITClock } from './clock/itc-adapter';
import { EntitySession, EntitySessionConfig } from './entity-session';
import { EntityId } from './types';
import { KeyPair, deriveKeypair, generateKeypair } from './identity/keypair';

/**
 * Create and initialize browser storage
 * 
 * @example
 * const storage = await createStorage('alice');
 */
export async function createStorage(entityId: EntityId): Promise<BrowserStorage> {
  const storage = new BrowserStorage(`fa-db-${entityId}`);
  await storage.initialize();
  return storage;
}

/**
 * Create recognition cache with optional config
 * 
 * @example
 * const cache = createCache();
 * const customCache = createCache({ ttl: 5000, maxSize: 500 });
 */
export function createCache(config?: Partial<CacheConfig>): RecognitionCache {
  return new RecognitionCache(config ? { ...DEFAULT_CACHE_CONFIG, ...config } : undefined);
}

/**
 * Create a new ITC clock (seeded)
 * 
 * @example
 * const clock = createClock();
 */
export function createClock(): ITClock {
  return ITClock.seed();
}

/**
 * Create a new ITC clock from an existing stamp
 * 
 * @example
 * const clock = createClockFrom(existingStamp);
 */
export function createClockFrom(stamp: any): ITClock {
  return new ITClock(stamp);
}

/**
 * Generate a new random keypair
 * 
 * @example
 * const keypair = await createKeypair();
 */
export async function createKeypair(): Promise<KeyPair> {
  return await generateKeypair();
}

/**
 * Derive a keypair from password (deterministic)
 * 
 * @example
 * const keypair = await createKeypairFrom('password', 'alice@example.com');
 */
export async function createKeypairFrom(
  password: string,
  salt: string,
  iterations: number = 100000
): Promise<KeyPair> {
  return await deriveKeypair(password, salt, iterations);
}

/**
 * Create a fully initialized EntitySession
 * 
 * This is the recommended way to create sessions.
 * Automatically creates and initializes all required components.
 * 
 * @example
 * // Simple
 * const session = await createSession('alice');
 * 
 * // With options
 * const session = await createSession('alice', {
 *   autoSync: true,
 *   maxAllocation: 0.8
 * });
 */
export async function createSession(
  entityId: EntityId,
  config?: Partial<EntitySessionConfig>
): Promise<EntitySession> {
  const session = new EntitySession(
    config ? { entityId, ...config } : entityId
  );
  await session.initialize();
  return session;
}

/**
 * Create a session with custom storage and cache
 * 
 * Useful when you want full control over the session components.
 * 
 * @example
 * const storage = await createStorage('alice');
 * const cache = createCache({ ttl: 10000 });
 * const session = await createSessionWith(entityId, storage, cache);
 */
export async function createSessionWith(
  entityId: EntityId,
  storage: BrowserStorage,
  cache: RecognitionCache,
  config?: Partial<EntitySessionConfig>
): Promise<EntitySession> {
  const session = new EntitySession({
    entityId,
    storage,
    cache,
    ...config
  });
  await session.initialize();
  return session;
}

/**
 * Quick session creation for testing
 * 
 * Creates a session with minimal setup, in-memory only.
 * 
 * @example
 * const testSession = await createTestSession('alice');
 */
export async function createTestSession(entityId: EntityId): Promise<EntitySession> {
  const session = new EntitySession(entityId);
  await session.initialize();
  return session;
}

