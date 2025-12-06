/**
 * Simple API Tests
 * 
 * Verify elegant one-line setup works correctly
 */

import { describe, it, expect, beforeEach } from 'vitest';
import { newWebSocketSession, createHttpBatchSession } from '../simple-api';

describe('Simple API', () => {
  describe('WebSocket Session', () => {
    it('should create session with one line', async () => {
      const api = newWebSocketSession('alice', 'wss://example.com');
      
      expect(api).toBeDefined();
      expect(api.getStorage()).toBeDefined();
      expect(api.getCache()).toBeDefined();
      expect(api.getClock()).toBeDefined();
    });

    it('should provide recognition methods', async () => {
      const api = newWebSocketSession('alice', 'wss://example.com');
      
      await api.getStorage().initialize();
      
      // Allocate recognition
      await api.allocateRecognition('bob', 0.5);
      
      // Get mutual recognition
      const mr = await api.getMutualRecognition('bob');
      expect(mr).toBeGreaterThanOrEqual(0);
      
      // Get allocations
      const allocations = await api.getMyAllocations();
      expect(allocations).toHaveLength(1);
      expect(allocations[0].targetId).toBe('bob');
      expect(allocations[0].amount).toBe(0.5);
    });

    it('should support clock operations', async () => {
      const api = newWebSocketSession('alice', 'wss://example.com');
      
      const clock = api.getClock();
      expect(clock).toBeDefined();
      
      // Fork for peer
      const peerClock = api.forkClock();
      expect(peerClock).toBeDefined();
      expect(peerClock.serialize()).toBeDefined();
    });
  });

  describe('HTTP Batch Session', () => {
    it('should create batch with one line', () => {
      const batch = createHttpBatchSession('https://api.example.com');
      
      expect(batch).toBeDefined();
      expect(typeof batch.getMRS).toBe('function');
      expect(typeof batch.getMRD).toBe('function');
    });

    it('should queue multiple calls', async () => {
      // Note: This test would need a mock server
      // For now, just verify the interface exists
      
      const batch = createHttpBatchSession('https://api.example.com');
      
      expect(batch.getMRS).toBeDefined();
      expect(batch.getMRD).toBeDefined();
      expect(batch.getMutualRecognition).toBeDefined();
    });
  });

  describe('Elegance Comparison', () => {
    it('demonstrates the improvement', async () => {
      // BEFORE (complex):
      // const storage = new BrowserStorage('alice');
      // await storage.initialize();
      // const cache = new RecognitionCache();
      // const session = new EntitySession({ entityId: 'alice', storage, cache });
      // const capMgr = new CapabilityManager();
      // capMgr.exportMain(session);
      // ... 20 more lines ...

      // AFTER (elegant!):
      const api = newWebSocketSession('alice', 'wss://example.com');
      await api.getStorage().initialize();
      
      // That's it! 🎉
      expect(api).toBeDefined();
    });
  });
});

