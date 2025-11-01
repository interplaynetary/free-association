/**
 * Holster Database Helper Utilities
 * 
 * Provides type-safe wrappers around Gun/Holster database operations
 * to eliminate repeated promise-wrapping patterns across routes.
 */

import { error } from '@sveltejs/kit';
import { user, holster } from './core';
import type { AccountData, InviteCode } from '$lib/server/schemas/holster';

/**
 * Generic get operation for Gun/Holster
 */
export async function holsterGet<T = any>(path: string[]): Promise<T | null> {
  return new Promise((resolve) => {
    let chain: any = user;
    
    for (const segment of path) {
      if (segment === 'get' || segment === 'next') continue;
      chain = chain.get ? chain.get(segment) : chain.next(segment);
    }
    
    chain.once((data: T | null) => {
      resolve(data);
    });
  });
}

/**
 * Get operation with explicit next() navigation
 */
export async function holsterNext<T = any>(collection: string, key: string): Promise<T | null> {
  return new Promise((resolve) => {
    user.get(collection).next(key, (data: T | null) => {
      resolve(data);
    });
  });
}

/**
 * Put operation for Gun/Holster with error handling
 */
export async function holsterPut(
  path: string[], 
  data: any,
  throwOnError: boolean = true
): Promise<void> {
  return new Promise((resolve, reject) => {
    let chain: any = user;
    
    for (const segment of path) {
      if (segment === 'get' || segment === 'next' || segment === 'put') continue;
      chain = chain.get ? chain.get(segment) : chain.next(segment);
    }
    
    chain.put(data, (err: any) => {
      if (err) {
        console.error('[Holster Put Error]', path.join('/'), err);
        if (throwOnError) {
          reject(err);
        } else {
          resolve();
        }
      } else {
        resolve();
      }
    });
  });
}

/**
 * Put operation with explicit next() navigation
 */
export async function holsterNextPut(
  collection: string,
  key: string,
  data: any,
  throwOnError: boolean = true
): Promise<void> {
  return new Promise((resolve, reject) => {
    user.get(collection).next(key).put(data, (err: any) => {
      if (err) {
        console.error('[Holster Put Error]', collection, key, err);
        if (throwOnError) {
          reject(err);
        } else {
          resolve();
        }
      } else {
        resolve();
      }
    });
  });
}

/**
 * Get account by code with proper typing
 */
export async function getAccountByCode(code: string): Promise<AccountData | null> {
  if (!user.is) {
    throw error(500, 'Host error: User not authenticated');
  }
  
  const account = await holsterNext<AccountData>('accounts', code);
  return account;
}

/**
 * Get account and verify it exists
 */
export async function getAccountByCodeOrFail(code: string): Promise<AccountData> {
  const account = await getAccountByCode(code);
  
  if (!account || !(account as any).epub) {
    throw error(404, 'Account not found');
  }
  
  return account;
}

/**
 * Update account data
 */
export async function updateAccount(
  code: string, 
  data: Partial<AccountData>
): Promise<void> {
  if (!user.is) {
    throw error(500, 'Host error: User not authenticated');
  }
  
  try {
    await holsterNextPut('accounts', code, data);
  } catch (err) {
    console.error('Failed to update account:', err);
    throw error(500, 'Failed to update account');
  }
}

/**
 * Get data from a collection as an array
 */
export async function holsterGetArray<T = any>(
  collection: string,
  filter?: (item: T) => boolean
): Promise<T[]> {
  return new Promise((resolve) => {
    user.get(collection).once((data: any) => {
      if (!data) {
        resolve([]);
        return;
      }
      
      const items: T[] = [];
      for (const [key, value] of Object.entries(data)) {
        if (key !== '_' && value && typeof value === 'object') {
          const item = { ...value, _key: key } as T;
          if (!filter || filter(item)) {
            items.push(item);
          }
        }
      }
      
      resolve(items);
    });
  });
}

/**
 * Decrypt data using Holster SEA
 */
export async function holsterDecrypt<T = any>(
  encrypted: any,
  secret: any
): Promise<T | null> {
  try {
    const decrypted = await holster.SEA.decrypt(encrypted, secret);
    return decrypted as T;
  } catch (err) {
    console.error('[Holster Decrypt Error]', err);
    return null;
  }
}

/**
 * Encrypt data using Holster SEA
 */
export async function holsterEncrypt(
  data: any,
  secret: any
): Promise<any> {
  try {
    return await holster.SEA.encrypt(data, secret);
  } catch (err) {
    console.error('[Holster Encrypt Error]', err);
    throw error(500, 'Encryption failed');
  }
}

/**
 * Verify signed data
 */
export async function holsterVerify<T = any>(
  signed: any,
  publicKey: any
): Promise<T | null> {
  try {
    const verified = await holster.SEA.verify(signed, publicKey);
    return verified as T;
  } catch (err) {
    console.error('[Holster Verify Error]', err);
    return null;
  }
}

/**
 * Check if user is authenticated
 */
export function ensureAuthenticated(): void {
  if (!user.is) {
    throw error(500, 'Host error: User not authenticated');
  }
}

/**
 * Delete data from a path
 */
export async function holsterDelete(
  collection: string,
  key: string
): Promise<void> {
  return holsterNextPut(collection, key, null, false);
}

/**
 * Subscribe to realtime updates (returns unsubscribe function)
 */
export function holsterSubscribe<T = any>(
  collection: string,
  callback: (data: T) => void
): () => void {
  const handler = (data: T) => callback(data);
  user.get(collection).on(handler);
  
  // Return unsubscribe function
  return () => {
    user.get(collection).off(handler);
  };
}

