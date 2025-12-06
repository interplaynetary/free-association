/**
 * ITC Integration Tests
 * 
 * Verify that ITC clocks work correctly in RPC context
 */

import { describe, it, expect } from 'vitest';
import { ITClock } from '../clock';

describe('ITC Integration', () => {
  it('should create seed clock', () => {
    const clock = ITClock.seed();
    expect(clock).toBeDefined();
    expect(clock.serialize()).toBeDefined();
  });

  it('should increment clock', () => {
    const clock = ITClock.seed();
    const stamp1 = clock.serialize();
    
    clock.increment();
    const stamp2 = clock.serialize();
    
    // After increment, clock should have progressed
    expect(clock.happensAfter(stamp1)).toBe(true);
  });

  it('should fork clock for new peer', () => {
    const alice = ITClock.seed();
    const aliceStamp1 = alice.serialize();
    
    // Fork for Bob
    const bob = alice.fork();
    
    // Both clocks exist
    expect(alice.serialize()).toBeDefined();
    expect(bob.serialize()).toBeDefined();
    
    // They start at same causality
    expect(alice.isConcurrent(bob.serialize())).toBe(true);
    
    // After separate increments, they're concurrent
    alice.increment();
    bob.increment();
    
    expect(alice.isConcurrent(bob.serialize())).toBe(true);
  });

  it('should merge clocks correctly', () => {
    const alice = ITClock.seed();
    const bob = alice.fork();
    
    // Alice increments
    alice.increment();
    const aliceStamp = alice.serialize();
    
    // Bob increments
    bob.increment();
    const bobStamp = bob.serialize();
    
    // They're concurrent
    expect(alice.isConcurrent(bobStamp)).toBe(true);
    
    // Alice merges Bob's stamp
    alice.merge(bobStamp);
    
    // Now Alice's clock includes both updates
    expect(alice.happensAfter(aliceStamp)).toBe(true);
    expect(alice.happensAfter(bobStamp)).toBe(true);
  });

  it('should compare causality correctly', () => {
    const clock = ITClock.seed();
    const stamp1 = clock.serialize();
    
    clock.increment();
    const stamp2 = clock.serialize();
    
    clock.increment();
    const stamp3 = clock.serialize();
    
    expect(clock.compare(stamp1)).toBe('after');
    expect(clock.compare(stamp2)).toBe('after');
    expect(clock.compare(stamp3)).toBe('equal');
  });

  it('should handle fork-join pattern', () => {
    // Main clock
    const main = ITClock.seed();
    main.increment();
    
    // Fork for worker
    const worker = main.fork();
    
    // Worker does work
    worker.increment();
    worker.increment();
    const workerStamp = worker.serialize();
    
    // Main continues
    main.increment();
    
    // Merge worker back
    main.merge(workerStamp);
    
    // Main now has both histories
    expect(main.happensAfter(workerStamp)).toBe(true);
  });

  it('should demonstrate space efficiency', () => {
    // ITC: Space is O(log n) - doesn't grow with participants
    const clock = ITClock.seed();
    
    // Create many forks
    const forks: ITClock[] = [];
    for (let i = 0; i < 100; i++) {
      forks.push(clock.fork());
    }
    
    // Clock still compact!
    const stampSize = JSON.stringify(clock.serialize()).length;
    
    // Vector clock would be: { entity1: 1, entity2: 1, ..., entity100: 1 }
    // ITC is much smaller!
    expect(stampSize).toBeLessThan(1000);  // Small and elegant
  });
});

