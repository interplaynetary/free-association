/**
 * Tests for RecognitionCache
 */

import { describe, it, expect, beforeEach, afterEach } from 'vitest';
import { RecognitionCache } from '../cache';

describe('RecognitionCache', () => {
  let cache: RecognitionCache;

  beforeEach(() => {
    cache = new RecognitionCache({
      maxSize: 100,
      defaultTTL: 1000, // 1 second for testing
      cleanupInterval: 500
    });
  });

  afterEach(() => {
    cache.destroy();
  });

  describe('Basic Operations', () => {
    it('should store and retrieve values', () => {
      cache.set('mr:alice:bob', 0.5);
      expect(cache.get('mr:alice:bob')).toBe(0.5);
    });

    it('should return null for missing keys', () => {
      expect(cache.get('mr:alice:charlie')).toBeNull();
    });

    it('should check if key exists', () => {
      cache.set('mr:alice:bob', 0.5);
      expect(cache.has('mr:alice:bob')).toBe(true);
      expect(cache.has('mr:alice:charlie')).toBe(false);
    });

    it('should delete keys', () => {
      cache.set('mr:alice:bob', 0.5);
      expect(cache.delete('mr:alice:bob')).toBe(true);
      expect(cache.get('mr:alice:bob')).toBeNull();
    });

    it('should clear all entries', () => {
      cache.set('mr:alice:bob', 0.5);
      cache.set('mr:bob:charlie', 0.3);
      cache.clear();
      expect(cache.get('mr:alice:bob')).toBeNull();
      expect(cache.get('mr:bob:charlie')).toBeNull();
    });
  });

  describe('TTL Expiration', () => {
    it('should expire entries after TTL', async () => {
      cache.set('mr:alice:bob', 0.5, 100); // 100ms TTL
      
      // Should exist immediately
      expect(cache.get('mr:alice:bob')).toBe(0.5);
      
      // Wait for expiration
      await new Promise(resolve => setTimeout(resolve, 150));
      
      // Should be expired
      expect(cache.get('mr:alice:bob')).toBeNull();
    });

    it('should respect custom TTL', async () => {
      cache.set('mr:alice:bob', 0.5, 200); // Custom 200ms TTL
      
      // Should exist after 100ms
      await new Promise(resolve => setTimeout(resolve, 100));
      expect(cache.get('mr:alice:bob')).toBe(0.5);
      
      // Should expire after 200ms
      await new Promise(resolve => setTimeout(resolve, 150));
      expect(cache.get('mr:alice:bob')).toBeNull();
    });
  });

  describe('LRU Eviction', () => {
    it('should evict LRU entry when cache is full', () => {
      const smallCache = new RecognitionCache({ maxSize: 3, defaultTTL: 10000 });
      
      // Fill cache
      smallCache.set('mr:a:b', 1);
      smallCache.set('mr:c:d', 2);
      smallCache.set('mr:e:f', 3);
      
      // Access first entry multiple times
      smallCache.get('mr:a:b');
      smallCache.get('mr:a:b');
      
      // Add new entry - should evict least used (mr:c:d or mr:e:f)
      smallCache.set('mr:g:h', 4);
      
      // Most used should still exist
      expect(smallCache.get('mr:a:b')).toBe(1);
      
      // New entry should exist
      expect(smallCache.get('mr:g:h')).toBe(4);
      
      smallCache.destroy();
    });
  });

  describe('Invalidation', () => {
    it('should invalidate entries by entity', () => {
      cache.set('mr:alice:bob', 0.5);
      cache.set('mr:alice:charlie', 0.3);
      cache.set('mr:bob:charlie', 0.4);
      
      const invalidated = cache.invalidateEntity('alice');
      
      expect(invalidated).toBe(2); // Two entries with alice
      expect(cache.get('mr:alice:bob')).toBeNull();
      expect(cache.get('mr:alice:charlie')).toBeNull();
      expect(cache.get('mr:bob:charlie')).toBe(0.4); // Unaffected
    });

    it('should invalidate all MR entries', () => {
      cache.set('mr:alice:bob', 0.5);
      cache.set('tmr:alice:universe1', 1.5);
      cache.set('mr:bob:charlie', 0.3);
      
      const invalidated = cache.invalidateMR();
      
      expect(invalidated).toBe(2); // Only MR entries
      expect(cache.get('mr:alice:bob')).toBeNull();
      expect(cache.get('mr:bob:charlie')).toBeNull();
      expect(cache.get('tmr:alice:universe1')).toBe(1.5); // TMR unaffected
    });

    it('should invalidate entries by pattern', () => {
      cache.set('mr:alice:bob', 0.5);
      cache.set('mrs:alice:universe', { distribution: {} });
      cache.set('mrd:alice:collective', 1.5);
      
      const invalidated = cache.invalidatePattern(/^mrs:/);
      
      expect(invalidated).toBe(1); // Only MRS entry
      expect(cache.get('mrs:alice:universe')).toBeNull();
      expect(cache.get('mr:alice:bob')).toBe(0.5);
    });
  });

  describe('Statistics', () => {
    it('should provide cache statistics', () => {
      cache.set('mr:alice:bob', 0.5);
      cache.set('mr:bob:charlie', 0.3);
      
      // Access first entry twice
      cache.get('mr:alice:bob');
      cache.get('mr:alice:bob');
      
      const stats = cache.getStats();
      
      expect(stats.size).toBe(2);
      expect(stats.maxSize).toBe(100);
      expect(stats.hitRate).toBeGreaterThan(0);
      expect(stats.memoryEstimate).toBeGreaterThan(0);
    });
  });

  describe('Cleanup', () => {
    it('should clean up expired entries', async () => {
      cache.set('mr:alice:bob', 0.5, 100);
      cache.set('mr:bob:charlie', 0.3, 100);
      cache.set('mr:charlie:diana', 0.4, 10000); // Long TTL
      
      // Wait for expiration
      await new Promise(resolve => setTimeout(resolve, 150));
      
      const removed = cache.cleanup();
      
      expect(removed).toBe(2);
      expect(cache.get('mr:alice:bob')).toBeNull();
      expect(cache.get('mr:bob:charlie')).toBeNull();
      expect(cache.get('mr:charlie:diana')).toBe(0.4); // Should still exist
    });
  });
});

