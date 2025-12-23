/**
 * Comprehensive tests for Interval Tree Clocks (ITC)
 * 
 * Tests cover:
 * - Basic operations (seed, event, fork, join, peek)
 * - Causal ordering (leq, equals, concurrent)
 * - Normalization and optimization
 * - Complex distributed scenarios
 * - Edge cases and error conditions
 * - Class-based API
 * - String representation
 */

import { describe, it, expect, test } from 'vitest';
import * as ITC from './itc';
import type { Stamp, Id, Event } from './itc';

// ═══════════════════════════════════════════════════════════════════
// BASIC OPERATIONS
// ═══════════════════════════════════════════════════════════════════

describe('ITC Basic Operations', () => {
	describe('seed()', () => {
		it('creates a seed stamp with full ownership', () => {
			const stamp = ITC.seed();
			expect(stamp.id).toBe(1);
			expect(stamp.event).toBe(0);
		});
		
		it('creates unique stamps each time', () => {
			const s1 = ITC.seed();
			const s2 = ITC.seed();
			expect(s1).not.toBe(s2);
			expect(s1).toEqual(s2);
		});
	});
	
	describe('event()', () => {
		it('increments event on seed stamp', () => {
			const s0 = ITC.seed();
			const s1 = ITC.event(s0);
			
			expect(s1.event).toBe(1);
			expect(s1.id).toBe(1);
		});
		
		it('increments multiple times', () => {
			let stamp = ITC.seed();
			
			for (let i = 1; i <= 10; i++) {
				stamp = ITC.event(stamp);
				expect(stamp.event).toBe(i);
			}
		});
		
		it('preserves id component', () => {
			const s0 = ITC.seed();
			const s1 = ITC.event(s0);
			expect(s1.id).toBe(s0.id);
		});
		
		it('is idempotent when called on same stamp', () => {
			const s0 = ITC.seed();
			const s1 = ITC.event(s0);
			const s2 = ITC.event(s0);
			
			expect(s1.event).toBe(s2.event);
		});
	});
	
	describe('fork()', () => {
		it('splits a seed stamp into two stamps', () => {
			const s0 = ITC.seed();
			const [s1, s2] = ITC.fork(s0);
			
			expect(s1.event).toBe(0);
			expect(s2.event).toBe(0);
			expect(s1.id).not.toBe(s2.id);
		});
		
		it('creates stamps with distinct ids', () => {
			const s0 = ITC.seed();
			const [s1, s2] = ITC.fork(s0);
			
			expect(typeof s1.id).toBe('object');
			expect(typeof s2.id).toBe('object');
			
			if (typeof s1.id === 'object' && typeof s2.id === 'object') {
				expect(s1.id.l).toBe(1);
				expect(s1.id.r).toBe(0);
				expect(s2.id.l).toBe(0);
				expect(s2.id.r).toBe(1);
			}
		});
		
		it('preserves event on both stamps', () => {
			let stamp = ITC.seed();
			stamp = ITC.event(stamp);
			stamp = ITC.event(stamp);
			
			const [s1, s2] = ITC.fork(stamp);
			expect(s1.event).toBe(stamp.event);
			expect(s2.event).toBe(stamp.event);
		});
		
		it('allows nested forks', () => {
			const s0 = ITC.seed();
			const [s1, s2] = ITC.fork(s0);
			const [s3, s4] = ITC.fork(s1);
			const [s5, s6] = ITC.fork(s2);
			
			// All should have distinct ids but same event
			expect(s3.event).toBe(0);
			expect(s4.event).toBe(0);
			expect(s5.event).toBe(0);
			expect(s6.event).toBe(0);
		});
		
		it('can fork null id stamps', () => {
			const s0 = ITC.seed();
			const [s1, s2] = ITC.fork({ id: 0, event: 5 });
			
			expect(s1.id).toBe(0);
			expect(s2.id).toBe(0);
			expect(s1.event).toBe(5);
			expect(s2.event).toBe(5);
		});
	});
	
	describe('join()', () => {
		it('joins two stamps into one', () => {
			const s0 = ITC.seed();
			const [s1, s2] = ITC.fork(s0);
			const s3 = ITC.join(s1, s2);
			
			expect(s3.id).toBe(1); // Full ownership restored
		});
		
		it('takes maximum event', () => {
			const s0 = ITC.seed();
			const [s1, s2] = ITC.fork(s0);
			
			const s1_e = ITC.event(s1);
			const s2_e = ITC.event(ITC.event(s2));
			
			const s3 = ITC.join(s1_e, s2_e);
			
			// After forking, events become tree structures
			// The joined event should subsume both input events
			expect(ITC.leq(s1_e, s3)).toBe(true);
			expect(ITC.leq(s2_e, s3)).toBe(true);
			
			// And should be greater than both
			expect(ITC.leq(s3, s1_e)).toBe(false);
			expect(ITC.leq(s3, s2_e)).toBe(false);
		});
		
		it('is commutative', () => {
			const s0 = ITC.seed();
			const [s1, s2] = ITC.fork(s0);
			
			const s1_e = ITC.event(s1);
			const s2_e = ITC.event(s2);
			
			const j1 = ITC.join(s1_e, s2_e);
			const j2 = ITC.join(s2_e, s1_e);
			
			expect(j1.id).toEqual(j2.id);
			expect(j1.event).toEqual(j2.event);
		});
		
		it('is associative', () => {
			const s0 = ITC.seed();
			const [s1, s2] = ITC.fork(s0);
			const [s3, s4] = ITC.fork(s1);
			
			// (s3 join s4) join s2
			const j1 = ITC.join(ITC.join(s3, s4), s2);
			
			// s3 join (s4 join s2)
			const j2 = ITC.join(s3, ITC.join(s4, s2));
			
			expect(j1.id).toEqual(j2.id);
			expect(j1.event).toEqual(j2.event);
		});
		
		it('handles joining with null id stamps', () => {
			const s1 = ITC.seed();
			const s1_e = ITC.event(s1);
			const s2 = { id: 0 as Id, event: 5 as Event };
			
			const joined = ITC.join(s1_e, s2);
			expect(joined.id).toBe(1);
			expect(joined.event).toBe(5);
		});
	});
	
	describe('peek()', () => {
		it('creates stamp with null id', () => {
			const s0 = ITC.seed();
			const s1 = ITC.event(s0);
			const p = ITC.peek(s1);
			
			expect(p.id).toBe(0);
			expect(p.event).toBe(s1.event);
		});
		
		it('preserves event component', () => {
			let stamp = ITC.seed();
			for (let i = 0; i < 5; i++) {
				stamp = ITC.event(stamp);
			}
			
			const peeked = ITC.peek(stamp);
			expect(peeked.event).toBe(stamp.event);
		});
		
		it('can be used for message passing', () => {
			const s0 = ITC.seed();
			const [s1, s2] = ITC.fork(s0);
			
			const s1_e = ITC.event(s1);
			const message = ITC.peek(s1_e);
			
			// Receiver can join message to update their knowledge
			const s2_updated = ITC.join(s2, message);
			
			expect(ITC.leq(s1_e, s2_updated)).toBe(true);
		});
	});
});

// ═══════════════════════════════════════════════════════════════════
// CAUSAL ORDERING
// ═══════════════════════════════════════════════════════════════════

describe('ITC Causal Ordering', () => {
	describe('leq()', () => {
		it('reflexive: stamp <= stamp', () => {
			const s0 = ITC.seed();
			expect(ITC.leq(s0, s0)).toBe(true);
			
			const s1 = ITC.event(s0);
			expect(ITC.leq(s1, s1)).toBe(true);
		});
		
		it('transitive: if a <= b and b <= c, then a <= c', () => {
			let s0 = ITC.seed();
			let s1 = ITC.event(s0);
			let s2 = ITC.event(s1);
			
			expect(ITC.leq(s0, s1)).toBe(true);
			expect(ITC.leq(s1, s2)).toBe(true);
			expect(ITC.leq(s0, s2)).toBe(true);
		});
		
		it('antisymmetric: if a <= b and b <= a, then a = b', () => {
			const s0 = ITC.seed();
			const s1 = ITC.event(s0);
			const s2 = ITC.clone(s1);
			
			expect(ITC.leq(s1, s2)).toBe(true);
			expect(ITC.leq(s2, s1)).toBe(true);
			expect(ITC.equals(s1, s2)).toBe(true);
		});
		
		it('event increments are ordered', () => {
			let s0 = ITC.seed();
			let s1 = ITC.event(s0);
			let s2 = ITC.event(s1);
			let s3 = ITC.event(s2);
			
			expect(ITC.leq(s0, s1)).toBe(true);
			expect(ITC.leq(s1, s2)).toBe(true);
			expect(ITC.leq(s2, s3)).toBe(true);
			expect(ITC.leq(s0, s3)).toBe(true);
			
			// Not the other way
			expect(ITC.leq(s3, s0)).toBe(false);
			expect(ITC.leq(s2, s1)).toBe(false);
		});
		
		it('forked stamps are not ordered (concurrent)', () => {
			const s0 = ITC.seed();
			const [s1, s2] = ITC.fork(s0);
			
			const s1_e = ITC.event(s1);
			const s2_e = ITC.event(s2);
			
			expect(ITC.leq(s1_e, s2_e)).toBe(false);
			expect(ITC.leq(s2_e, s1_e)).toBe(false);
		});
		
		it('joined stamps subsume both inputs', () => {
			const s0 = ITC.seed();
			const [s1, s2] = ITC.fork(s0);
			
			const s1_e = ITC.event(s1);
			const s2_e = ITC.event(s2);
			
			const joined = ITC.join(s1_e, s2_e);
			
			expect(ITC.leq(s1_e, joined)).toBe(true);
			expect(ITC.leq(s2_e, joined)).toBe(true);
		});
		
		it('works with complex tree events', () => {
			const s0 = ITC.seed();
			const [s1, s2] = ITC.fork(s0);
			const [s3, s4] = ITC.fork(s1);
			
			const s3_e = ITC.event(s3);
			const s4_e = ITC.event(s4);
			
			const s12 = ITC.join(s3_e, s4_e);
			
			expect(ITC.leq(s3_e, s12)).toBe(true);
			expect(ITC.leq(s4_e, s12)).toBe(true);
		});
	});
	
	describe('equals()', () => {
		it('returns true for identical stamps', () => {
			const s1 = ITC.seed();
			const s2 = ITC.seed();
			
			expect(ITC.equals(s1, s2)).toBe(true);
		});
		
		it('returns true for cloned stamps', () => {
			const s1 = ITC.event(ITC.seed());
			const s2 = ITC.clone(s1);
			
			expect(ITC.equals(s1, s2)).toBe(true);
		});
		
		it('returns false for different stamps', () => {
			const s1 = ITC.seed();
			const s2 = ITC.event(s1);
			
			expect(ITC.equals(s1, s2)).toBe(false);
		});
		
		it('is symmetric', () => {
			const s1 = ITC.seed();
			const s2 = ITC.event(s1);
			
			expect(ITC.equals(s1, s2)).toBe(ITC.equals(s2, s1));
		});
	});
	
	describe('concurrent()', () => {
		it('returns false for ordered stamps', () => {
			const s1 = ITC.seed();
			const s2 = ITC.event(s1);
			
			expect(ITC.concurrent(s1, s2)).toBe(false);
		});
		
		it('returns true for forked stamps with events', () => {
			const s0 = ITC.seed();
			const [s1, s2] = ITC.fork(s0);
			
			const s1_e = ITC.event(s1);
			const s2_e = ITC.event(s2);
			
			expect(ITC.concurrent(s1_e, s2_e)).toBe(true);
		});
		
		it('returns false after join', () => {
			const s0 = ITC.seed();
			const [s1, s2] = ITC.fork(s0);
			
			const s1_e = ITC.event(s1);
			const s2_e = ITC.event(s2);
			const joined = ITC.join(s1_e, s2_e);
			
			expect(ITC.concurrent(s1_e, joined)).toBe(false);
			expect(ITC.concurrent(s2_e, joined)).toBe(false);
		});
		
		it('is symmetric', () => {
			const s0 = ITC.seed();
			const [s1, s2] = ITC.fork(s0);
			
			const s1_e = ITC.event(s1);
			const s2_e = ITC.event(s2);
			
			expect(ITC.concurrent(s1_e, s2_e)).toBe(ITC.concurrent(s2_e, s1_e));
		});
	});
});

// ═══════════════════════════════════════════════════════════════════
// COMPLEX DISTRIBUTED SCENARIOS
// ═══════════════════════════════════════════════════════════════════

describe('ITC Complex Scenarios', () => {
	it('handles diamond pattern (fork-event-join)', () => {
		// Create initial stamp
		const s0 = ITC.seed();
		
		// Fork into two replicas
		const [s1, s2] = ITC.fork(s0);
		
		// Each replica does an event
		const s1_e = ITC.event(s1);
		const s2_e = ITC.event(s2);
		
		// Replicas should be concurrent
		expect(ITC.concurrent(s1_e, s2_e)).toBe(true);
		
		// Join back together
		const merged = ITC.join(s1_e, s2_e);
		
		// Merged should subsume both
		expect(ITC.leq(s1_e, merged)).toBe(true);
		expect(ITC.leq(s2_e, merged)).toBe(true);
		
		// Event on merged should be greater than both
		const merged_e = ITC.event(merged);
		expect(ITC.leq(s1_e, merged_e)).toBe(true);
		expect(ITC.leq(s2_e, merged_e)).toBe(true);
	});
	
	it('handles multiple sequential forks', () => {
		const s0 = ITC.seed();
		const [s1, s2] = ITC.fork(s0);
		const [s3, s4] = ITC.fork(s1);
		const [s5, s6] = ITC.fork(s2);
		
		// All should have same event initially
		expect(s3.event).toBe(0);
		expect(s4.event).toBe(0);
		expect(s5.event).toBe(0);
		expect(s6.event).toBe(0);
		
		// Do events on leaf nodes
		const s3_e = ITC.event(s3);
		const s4_e = ITC.event(s4);
		const s5_e = ITC.event(s5);
		const s6_e = ITC.event(s6);
		
		// All leaf events should be concurrent
		expect(ITC.concurrent(s3_e, s4_e)).toBe(true);
		expect(ITC.concurrent(s3_e, s5_e)).toBe(true);
		expect(ITC.concurrent(s5_e, s6_e)).toBe(true);
		
		// Join them back
		const left = ITC.join(s3_e, s4_e);
		const right = ITC.join(s5_e, s6_e);
		const final = ITC.join(left, right);
		
		// Final should have full ownership
		expect(final.id).toBe(1);
		
		// Final should subsume all leaf stamps
		expect(ITC.leq(s3_e, final)).toBe(true);
		expect(ITC.leq(s4_e, final)).toBe(true);
		expect(ITC.leq(s5_e, final)).toBe(true);
		expect(ITC.leq(s6_e, final)).toBe(true);
	});
	
	it('handles message passing between replicas', () => {
		// Two replicas
		const s0 = ITC.seed();
		const [r1, r2] = ITC.fork(s0);
		
		// Replica 1 does some events
		let r1_current = ITC.event(r1);
		r1_current = ITC.event(r1_current);
		r1_current = ITC.event(r1_current);
		
		// Send message to replica 2
		const message = ITC.peek(r1_current);
		
		// Replica 2 receives and updates
		let r2_current = ITC.join(r2, message);
		
		// R2 should now know about R1's events
		expect(ITC.leq(message, r2_current)).toBe(true);
		
		// R2 does its own events
		r2_current = ITC.event(r2_current);
		r2_current = ITC.event(r2_current);
		
		// Send message back to R1
		const reply = ITC.peek(r2_current);
		r1_current = ITC.join(r1_current, reply);
		
		// Both replicas should now be synchronized
		const r1_peek = ITC.peek(r1_current);
		const r2_peek = ITC.peek(r2_current);
		
		expect(ITC.equals(r1_peek, r2_peek)).toBe(true);
	});
	
	it('handles dynamic system with replicas joining and leaving', () => {
		// Start with one replica
		const system = ITC.seed();
		
		// Replica 1 joins
		const [r1, rest1] = ITC.fork(system);
		let r1_current = ITC.event(r1);
		
		// Replica 2 joins
		const [r2, rest2] = ITC.fork(rest1);
		let r2_current = ITC.event(r2);
		
		// Replica 3 joins
		const [r3, rest3] = ITC.fork(rest2);
		let r3_current = ITC.event(r3);
		
		// Each does independent work
		r1_current = ITC.event(r1_current);
		r2_current = ITC.event(r2_current);
		r3_current = ITC.event(r3_current);
		
		// All should be concurrent
		expect(ITC.concurrent(r1_current, r2_current)).toBe(true);
		expect(ITC.concurrent(r2_current, r3_current)).toBe(true);
		expect(ITC.concurrent(r1_current, r3_current)).toBe(true);
		
		// Merge all back together
		const merged = ITC.join(ITC.join(r1_current, r2_current), ITC.join(r3_current, rest3));
		
		// Should have full ownership
		expect(merged.id).toBe(1);
		
		// Should subsume all replicas
		expect(ITC.leq(r1_current, merged)).toBe(true);
		expect(ITC.leq(r2_current, merged)).toBe(true);
		expect(ITC.leq(r3_current, merged)).toBe(true);
	});
	
	it('maintains causality with deep fork tree', () => {
		// Create deep fork structure
		const s0 = ITC.seed();
		
		// Level 1
		const [s1, s2] = ITC.fork(s0);
		
		// Level 2
		const [s3, s4] = ITC.fork(s1);
		const [s5, s6] = ITC.fork(s2);
		
		// Level 3
		const [s7, s8] = ITC.fork(s3);
		const [s9, s10] = ITC.fork(s4);
		
		// Do events at various levels
		const s7_e = ITC.event(s7);
		const s8_e = ITC.event(s8);
		const s9_e = ITC.event(s9);
		const s5_e = ITC.event(s5);
		
		// Verify concurrency
		expect(ITC.concurrent(s7_e, s8_e)).toBe(true);
		expect(ITC.concurrent(s7_e, s9_e)).toBe(true);
		expect(ITC.concurrent(s5_e, s7_e)).toBe(true);
		
		// Partial merge
		const left_branch = ITC.join(s7_e, s8_e);
		
		// Verify partial ordering
		expect(ITC.leq(s7_e, left_branch)).toBe(true);
		expect(ITC.leq(s8_e, left_branch)).toBe(true);
		expect(ITC.concurrent(left_branch, s9_e)).toBe(true);
	});
});

// ═══════════════════════════════════════════════════════════════════
// UTILITY FUNCTIONS
// ═══════════════════════════════════════════════════════════════════

describe('ITC Utility Functions', () => {
	describe('clone()', () => {
		it('creates deep copy of stamp', () => {
			const s1 = ITC.seed();
			const s2 = ITC.clone(s1);
			
			expect(s1).not.toBe(s2);
			expect(s1).toEqual(s2);
		});
		
		it('clones complex stamps', () => {
			const s0 = ITC.seed();
			const [s1, s2] = ITC.fork(s0);
			const s1_e = ITC.event(s1);
			
			const cloned = ITC.clone(s1_e);
			
			expect(cloned).not.toBe(s1_e);
			expect(cloned).toEqual(s1_e);
			expect(ITC.equals(cloned, s1_e)).toBe(true);
		});
		
		it('cloned stamp behaves identically', () => {
			const s1 = ITC.seed();
			const s2 = ITC.clone(s1);
			
			const s1_e = ITC.event(s1);
			const s2_e = ITC.event(s2);
			
			expect(ITC.equals(s1_e, s2_e)).toBe(true);
		});
	});
	
	describe('toString()', () => {
		it('converts seed to string', () => {
			const s = ITC.seed();
			const str = ITC.toString(s);
			
			expect(typeof str).toBe('string');
			expect(str).toContain('[');
			expect(str).toContain(']');
		});
		
		it('converts stamp after event to string', () => {
			const s = ITC.event(ITC.seed());
			const str = ITC.toString(s);
			
			expect(typeof str).toBe('string');
			expect(str.length).toBeGreaterThan(0);
		});
		
		it('converts forked stamps to string', () => {
			const [s1, s2] = ITC.fork(ITC.seed());
			
			const str1 = ITC.toString(s1);
			const str2 = ITC.toString(s2);
			
			expect(str1).not.toBe(str2);
			expect(typeof str1).toBe('string');
			expect(typeof str2).toBe('string');
		});
		
		it('converts complex stamps to string', () => {
			const s0 = ITC.seed();
			const [s1, s2] = ITC.fork(s0);
			const s1_e = ITC.event(s1);
			const s2_e = ITC.event(ITC.event(s2));
			const joined = ITC.join(s1_e, s2_e);
			
			const str = ITC.toString(joined);
			expect(typeof str).toBe('string');
			expect(str.length).toBeGreaterThan(0);
		});
	});
});

// ═══════════════════════════════════════════════════════════════════
// CLASS-BASED API
// ═══════════════════════════════════════════════════════════════════

describe('ITC Class-Based API', () => {
	describe('StampClass constructor', () => {
		it('creates seed stamp by default', () => {
			const s = new ITC.StampClass();
			const stamp = s.getStamp();
			
			expect(stamp.id).toBe(1);
			expect(stamp.event).toBe(0);
		});
		
		it('accepts initial stamp', () => {
			const initial = ITC.seed();
			const s = new ITC.StampClass(initial);
			
			expect(s.getStamp()).toEqual(initial);
		});
	});
	
	describe('StampClass.event()', () => {
		it('increments event (mutating)', () => {
			const s = new ITC.StampClass();
			s.event();
			
			expect(s.getStamp().event).toBe(1);
		});
		
		it('can be called multiple times', () => {
			const s = new ITC.StampClass();
			
			for (let i = 1; i <= 5; i++) {
				s.event();
				expect(s.getStamp().event).toBe(i);
			}
		});
	});
	
	describe('StampClass.fork()', () => {
		it('returns new stamp and mutates original', () => {
			const s1 = new ITC.StampClass();
			const s2 = s1.fork();
			
			expect(s2).toBeInstanceOf(ITC.StampClass);
			expect(s1).not.toBe(s2);
			
			const stamp1 = s1.getStamp();
			const stamp2 = s2.getStamp();
			
			expect(stamp1.id).not.toBe(1);
			expect(stamp2.id).not.toBe(1);
		});
		
		it('allows independent events on forked stamps', () => {
			const s1 = new ITC.StampClass();
			const s2 = s1.fork();
			
			s1.event();
			s2.event();
			s2.event();
			
			expect(s1.getStamp().event).not.toBe(s2.getStamp().event);
		});
	});
	
	describe('StampClass.join()', () => {
		it('joins two stamps (mutating)', () => {
			const s1 = new ITC.StampClass();
			const s2 = s1.fork();
			
			s1.event();
			s2.event();
			s2.event();
			
			s1.join(s2);
			
			const stamp = s1.getStamp();
			expect(stamp.id).toBe(1); // Full ownership restored
		});
	});
	
	describe('StampClass.peek()', () => {
		it('creates stamp with null id', () => {
			const s1 = new ITC.StampClass();
			s1.event();
			s1.event();
			
			const peeked = s1.peek();
			
			expect(peeked.getStamp().id).toBe(0);
			expect(peeked.getStamp().event).toBe(s1.getStamp().event);
		});
	});
	
	describe('StampClass.leq()', () => {
		it('checks causal ordering', () => {
			const s1 = new ITC.StampClass();
			const s2 = s1.clone();
			
			s2.event();
			
			expect(s1.leq(s2)).toBe(true);
			expect(s2.leq(s1)).toBe(false);
		});
	});
	
	describe('StampClass.equals()', () => {
		it('checks equality', () => {
			const s1 = new ITC.StampClass();
			const s2 = s1.clone();
			
			expect(s1.equals(s2)).toBe(true);
			
			s2.event();
			expect(s1.equals(s2)).toBe(false);
		});
	});
	
	describe('StampClass.concurrent()', () => {
		it('checks concurrency', () => {
			const s1 = new ITC.StampClass();
			const s2 = s1.fork();
			
			s1.event();
			s2.event();
			
			expect(s1.concurrent(s2)).toBe(true);
			expect(s2.concurrent(s1)).toBe(true);
		});
	});
	
	describe('StampClass.clone()', () => {
		it('creates independent copy', () => {
			const s1 = new ITC.StampClass();
			s1.event();
			
			const s2 = s1.clone();
			
			expect(s1).not.toBe(s2);
			expect(s1.equals(s2)).toBe(true);
			
			s2.event();
			expect(s1.equals(s2)).toBe(false);
		});
	});
	
	describe('StampClass.toString()', () => {
		it('converts to string', () => {
			const s = new ITC.StampClass();
			s.event();
			
			const str = s.toString();
			expect(typeof str).toBe('string');
			expect(str.length).toBeGreaterThan(0);
		});
	});
	
	describe('StampClass static methods', () => {
		it('seed() creates new stamp', () => {
			const s = ITC.StampClass.seed();
			expect(s).toBeInstanceOf(ITC.StampClass);
			expect(s.getStamp()).toEqual(ITC.seed());
		});
		
		it('event() returns new stamp (functional)', () => {
			const s1 = new ITC.StampClass();
			const s2 = ITC.StampClass.event(s1);
			
			expect(s1.getStamp().event).toBe(0);
			expect(s2.getStamp().event).toBe(1);
		});
		
		it('fork() returns array of two stamps (functional)', () => {
			const s1 = new ITC.StampClass();
			const [s2, s3] = ITC.StampClass.fork(s1);
			
			expect(s1.getStamp()).toEqual(ITC.seed()); // Original unchanged
			expect(s2).toBeInstanceOf(ITC.StampClass);
			expect(s3).toBeInstanceOf(ITC.StampClass);
		});
		
		it('join() returns new stamp (functional)', () => {
			const s1 = new ITC.StampClass();
			const s2 = s1.fork();
			
			s1.event();
			s2.event();
			
			const original_s1 = s1.clone();
			const joined = ITC.StampClass.join(s1, s2);
			
			expect(s1.equals(original_s1)).toBe(true); // Original unchanged
			expect(joined.getStamp().id).toBe(1);
		});
		
		it('peek() returns new stamp (functional)', () => {
			const s1 = new ITC.StampClass();
			s1.event();
			
			const peeked = ITC.StampClass.peek(s1);
			
			expect(s1.getStamp().id).toBe(1);
			expect(peeked.getStamp().id).toBe(0);
		});
	});
});

// ═══════════════════════════════════════════════════════════════════
// EDGE CASES AND ERROR CONDITIONS
// ═══════════════════════════════════════════════════════════════════

describe('ITC Edge Cases', () => {
	it('handles many sequential events', () => {
		let stamp = ITC.seed();
		
		for (let i = 1; i <= 100; i++) {
			stamp = ITC.event(stamp);
		}
		
		expect(stamp.event).toBe(100);
	});
	
	it('handles deep fork hierarchy', () => {
		let stamps = [ITC.seed()];
		
		// Create 8 leaf stamps through 3 levels of forks
		for (let level = 0; level < 3; level++) {
			const newStamps: Stamp[] = [];
			for (const stamp of stamps) {
				const [s1, s2] = ITC.fork(stamp);
				newStamps.push(s1, s2);
			}
			stamps = newStamps;
		}
		
		expect(stamps.length).toBe(8);
		
		// All should have event 0
		stamps.forEach(s => expect(s.event).toBe(0));
		
		// Do events on all
		stamps = stamps.map(s => ITC.event(s));
		
		// Join them all back
		let result = stamps[0];
		for (let i = 1; i < stamps.length; i++) {
			result = ITC.join(result, stamps[i]);
		}
		
		// Should have full ownership
		expect(result.id).toBe(1);
	});
	
	it('handles multiple joins of same stamp', () => {
		const s1 = ITC.seed();
		const s2 = ITC.clone(s1);
		
		const joined = ITC.join(s1, s2);
		
		expect(joined.id).toBe(1);
		expect(joined.event).toBe(0);
	});
	
	it('handles join with self (idempotent)', () => {
		const s1 = ITC.seed();
		const joined = ITC.join(s1, s1);
		
		expect(joined.id).toBe(1);
		expect(joined.event).toBe(0);
	});
	
	it('preserves ordering through multiple join/fork cycles', () => {
		let s = ITC.seed();
		
		// Do some events
		s = ITC.event(s);
		s = ITC.event(s);
		
		// Fork and rejoin multiple times
		for (let i = 0; i < 5; i++) {
			const [s1, s2] = ITC.fork(s);
			const s1_e = ITC.event(s1);
			const s2_e = ITC.event(s2);
			s = ITC.join(s1_e, s2_e);
		}
		
		// Should still maintain causality
		const initial = { id: 1 as Id, event: 2 as Event };
		expect(ITC.leq(initial, s)).toBe(true);
	});
	
	it('handles null id stamps correctly', () => {
		const nullStamp = { id: 0 as Id, event: 5 as Event };
		const seed = ITC.seed();
		
		const joined = ITC.join(nullStamp, seed);
		
		expect(joined.id).toBe(1);
		expect(joined.event).toBe(5);
	});
	
	it('peek preserves all event information', () => {
		const s0 = ITC.seed();
		const [s1, s2] = ITC.fork(s0);
		
		const s1_e = ITC.event(ITC.event(s1));
		const s2_e = ITC.event(s2);
		
		const joined = ITC.join(s1_e, s2_e);
		const peeked = ITC.peek(joined);
		
		// Peeked should have same event structure
		expect(peeked.event).toEqual(joined.event);
	});
	
	it('equals is reflexive, symmetric, and transitive', () => {
		const s1 = ITC.seed();
		const s2 = ITC.clone(s1);
		const s3 = ITC.clone(s1);
		
		// Reflexive
		expect(ITC.equals(s1, s1)).toBe(true);
		
		// Symmetric
		expect(ITC.equals(s1, s2)).toBe(ITC.equals(s2, s1));
		
		// Transitive
		if (ITC.equals(s1, s2) && ITC.equals(s2, s3)) {
			expect(ITC.equals(s1, s3)).toBe(true);
		}
	});
	
	it('concurrent is symmetric and irreflexive', () => {
		const s0 = ITC.seed();
		const [s1, s2] = ITC.fork(s0);
		
		const s1_e = ITC.event(s1);
		const s2_e = ITC.event(s2);
		
		// Symmetric
		expect(ITC.concurrent(s1_e, s2_e)).toBe(ITC.concurrent(s2_e, s1_e));
		
		// Irreflexive
		expect(ITC.concurrent(s1_e, s1_e)).toBe(false);
	});
});

// ═══════════════════════════════════════════════════════════════════
// PROPERTY-BASED TESTS
// ═══════════════════════════════════════════════════════════════════

describe('ITC Properties', () => {
	it('fork-join identity: join(fork(s)) ~= s', () => {
		const s = ITC.seed();
		const [s1, s2] = ITC.fork(s);
		const joined = ITC.join(s1, s2);
		
		expect(joined.id).toBe(s.id);
		expect(joined.event).toBe(s.event);
		expect(ITC.equals(joined, s)).toBe(true);
	});
	
	it('event monotonicity: event(s) > s', () => {
		const s = ITC.seed();
		const s_e = ITC.event(s);
		
		expect(ITC.leq(s, s_e)).toBe(true);
		expect(ITC.leq(s_e, s)).toBe(false);
	});
	
	it('join commutativity: join(a, b) = join(b, a)', () => {
		const s0 = ITC.seed();
		const [s1, s2] = ITC.fork(s0);
		
		const s1_e = ITC.event(s1);
		const s2_e = ITC.event(ITC.event(s2));
		
		const j1 = ITC.join(s1_e, s2_e);
		const j2 = ITC.join(s2_e, s1_e);
		
		expect(ITC.equals(j1, j2)).toBe(true);
	});
	
	it('join associativity: join(join(a,b),c) = join(a,join(b,c))', () => {
		const s0 = ITC.seed();
		const [s1, rest] = ITC.fork(s0);
		const [s2, s3] = ITC.fork(rest);
		
		const s1_e = ITC.event(s1);
		const s2_e = ITC.event(s2);
		const s3_e = ITC.event(s3);
		
		const j1 = ITC.join(ITC.join(s1_e, s2_e), s3_e);
		const j2 = ITC.join(s1_e, ITC.join(s2_e, s3_e));
		
		expect(ITC.equals(j1, j2)).toBe(true);
	});
	
	it('join idempotency: join(s, s) = s', () => {
		const s = ITC.event(ITC.seed());
		const joined = ITC.join(s, s);
		
		expect(ITC.equals(joined, s)).toBe(true);
	});
	
	it('join subsumption: s1 <= join(s1, s2)', () => {
		const s0 = ITC.seed();
		const [s1, s2] = ITC.fork(s0);
		
		const s1_e = ITC.event(s1);
		const s2_e = ITC.event(s2);
		
		const joined = ITC.join(s1_e, s2_e);
		
		expect(ITC.leq(s1_e, joined)).toBe(true);
		expect(ITC.leq(s2_e, joined)).toBe(true);
	});
	
	it('peek preserves causality', () => {
		let s = ITC.seed();
		s = ITC.event(s);
		s = ITC.event(s);
		
		const peeked = ITC.peek(s);
		
		// Peeked should be causally equivalent
		expect(ITC.leq(s, peeked)).toBe(true);
		expect(ITC.leq(peeked, s)).toBe(true);
	});
	
	it('fork creates concurrent stamps', () => {
		const s = ITC.seed();
		const [s1, s2] = ITC.fork(s);
		
		// After events, they should be concurrent
		const s1_e = ITC.event(s1);
		const s2_e = ITC.event(s2);
		
		expect(ITC.concurrent(s1_e, s2_e)).toBe(true);
	});
	
	it('transitivity: a <= b && b <= c => a <= c', () => {
		let a = ITC.seed();
		let b = ITC.event(a);
		let c = ITC.event(b);
		
		expect(ITC.leq(a, b)).toBe(true);
		expect(ITC.leq(b, c)).toBe(true);
		expect(ITC.leq(a, c)).toBe(true);
	});
	
	it('antisymmetry: a <= b && b <= a => a = b', () => {
		const a = ITC.event(ITC.seed());
		const b = ITC.clone(a);
		
		expect(ITC.leq(a, b)).toBe(true);
		expect(ITC.leq(b, a)).toBe(true);
		expect(ITC.equals(a, b)).toBe(true);
	});
});

// ═══════════════════════════════════════════════════════════════════
// REAL-WORLD SCENARIOS
// ═══════════════════════════════════════════════════════════════════

describe('ITC Real-World Scenarios', () => {
	it('simulates collaborative text editing', () => {
		// Two users editing a document
		const document = ITC.seed();
		const [user1, user2] = ITC.fork(document);
		
		// User 1 makes edits
		let u1_version = ITC.event(user1); // Edit 1
		u1_version = ITC.event(u1_version); // Edit 2
		
		// User 2 makes concurrent edits
		let u2_version = ITC.event(user2); // Edit 1
		
		// Check they're concurrent
		expect(ITC.concurrent(u1_version, u2_version)).toBe(true);
		
		// Users sync
		const synced = ITC.join(u1_version, u2_version);
		
		// Both users update to synced version
		u1_version = synced;
		u2_version = synced;
		
		// Now they should be equal
		expect(ITC.equals(u1_version, u2_version)).toBe(true);
		
		// More edits after sync
		u1_version = ITC.event(u1_version);
		
		// User 1's new version should be ahead
		expect(ITC.leq(u2_version, u1_version)).toBe(true);
		expect(ITC.leq(u1_version, u2_version)).toBe(false);
	});
	
	it('simulates distributed database with 3 replicas', () => {
		// Initialize database
		const db = ITC.seed();
		
		// Create 3 replicas
		const [r1, rest] = ITC.fork(db);
		const [r2, r3] = ITC.fork(rest);
		
		// Each replica processes writes independently
		let r1_state = ITC.event(r1); // Write A
		r1_state = ITC.event(r1_state); // Write B
		
		let r2_state = ITC.event(r2); // Write C
		
		let r3_state = ITC.event(r3); // Write D
		r3_state = ITC.event(r3_state); // Write E
		r3_state = ITC.event(r3_state); // Write F
		
		// All should be concurrent
		expect(ITC.concurrent(r1_state, r2_state)).toBe(true);
		expect(ITC.concurrent(r2_state, r3_state)).toBe(true);
		
		// Replica 1 and 2 sync
		r1_state = ITC.join(r1_state, r2_state);
		r2_state = ITC.clone(r1_state);
		
		// After sync, r1 and r2 should be equal
		expect(ITC.equals(r1_state, r2_state)).toBe(true);
		
		// But still concurrent with r3
		expect(ITC.concurrent(r1_state, r3_state)).toBe(true);
		
		// Final sync with r3
		const final_state = ITC.join(ITC.join(r1_state, r2_state), r3_state);
		
		// Final state should subsume all
		expect(ITC.leq(r1_state, final_state)).toBe(true);
		expect(ITC.leq(r2_state, final_state)).toBe(true);
		expect(ITC.leq(r3_state, final_state)).toBe(true);
	});
	
	it('simulates mobile offline-first app with sync', () => {
		// Server state
		let server = ITC.seed();
		
		// Mobile client goes offline
		const [client, server_after_fork] = ITC.fork(server);
		server = server_after_fork;
		
		// Client makes offline changes
		let client_state = ITC.event(client);
		client_state = ITC.event(client_state);
		client_state = ITC.event(client_state);
		
		// Server receives changes from other clients
		server = ITC.event(server);
		server = ITC.event(server);
		
		// Client comes online and syncs
		const sync_message = ITC.peek(client_state);
		server = ITC.join(server, sync_message);
		
		// Server should now know about client changes
		expect(ITC.leq(client_state, server)).toBe(true);
		
		// Client gets server updates
		const server_message = ITC.peek(server);
		client_state = ITC.join(client_state, server_message);
		
		// Both should now be in sync
		expect(ITC.equals(ITC.peek(client_state), ITC.peek(server))).toBe(true);
	});
});

