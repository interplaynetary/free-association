/**
 * Interval Tree Clocks (ITC) Tests
 * 
 * Tests the TypeScript implementation of ITC against the reference behavior
 */

import { describe, test, expect } from 'bun:test';
import {
	seed,
	event,
	fork,
	join,
	peek,
	leq,
	equals,
	concurrent,
	clone,
	toString,
	StampClass,
	type Stamp
} from '../utils/itc';

// ═══════════════════════════════════════════════════════════════════
// BASIC OPERATIONS
// ═══════════════════════════════════════════════════════════════════

describe('ITC - Basic Operations', () => {
	
	test('seed creates initial stamp', () => {
		const s = seed();
		expect(s.id).toBe(1);
		expect(s.event).toBe(0);
	});
	
	test('event increments event component', () => {
		const s = seed();
		const s1 = event(s);
		
		expect(s1.id).toBe(1);
		expect(s1.event).toBe(1);
	});
	
	test('fork creates two stamps with distinct ids', () => {
		const s = seed();
		const [s1, s2] = fork(s);
		
		// Both should have same event
		expect(s1.event).toBe(0);
		expect(s2.event).toBe(0);
		
		// Ids should be different
		expect(s1.id).not.toBe(s2.id);
		expect(typeof s1.id).toBe('object');
		expect(typeof s2.id).toBe('object');
	});
	
	test('peek creates stamp with null id', () => {
		const s = seed();
		const s1 = event(s);
		const p = peek(s1);
		
		expect(p.id).toBe(0);
		expect(p.event).toBe(1);
	});
	
	test('join merges two stamps', () => {
		const s = seed();
		const [s1, s2] = fork(s);
		
		const s1e = event(s1);
		const s2e = event(s2);
		
		const merged = join(s1e, s2e);
		
		// Merged stamp should have full id
		expect(merged.id).toBe(1);
		// And should reflect both events (may be normalized to number or remain object)
		expect(merged.event).toBeDefined();
		// Both original stamps should be <= merged
		expect(leq(s1e, merged)).toBe(true);
		expect(leq(s2e, merged)).toBe(true);
	});
});

// ═══════════════════════════════════════════════════════════════════
// CAUSALITY TRACKING
// ═══════════════════════════════════════════════════════════════════

describe('ITC - Causality Tracking', () => {
	
	test('leq: same stamps are equal', () => {
		const s = seed();
		expect(leq(s, s)).toBe(true);
	});
	
	test('leq: event creates causal dependency', () => {
		const s1 = seed();
		const s2 = event(s1);
		
		expect(leq(s1, s2)).toBe(true);
		expect(leq(s2, s1)).toBe(false);
	});
	
	test('equals: checks bidirectional leq', () => {
		const s1 = seed();
		const s2 = clone(s1);
		
		expect(equals(s1, s2)).toBe(true);
		
		const s3 = event(s1);
		expect(equals(s1, s3)).toBe(false);
	});
	
	test('concurrent: forked stamps with events are concurrent', () => {
		const s = seed();
		const [s1, s2] = fork(s);
		
		const s1e = event(s1);
		const s2e = event(s2);
		
		expect(concurrent(s1e, s2e)).toBe(true);
		expect(leq(s1e, s2e)).toBe(false);
		expect(leq(s2e, s1e)).toBe(false);
	});
});

// ═══════════════════════════════════════════════════════════════════
// DEMO RUN (from ITC paper)
// ═══════════════════════════════════════════════════════════════════

describe('ITC - Demo Run (Functional API)', () => {
	
	test('complete demo scenario', () => {
		// a: seed
		let seedStamp = seed();
		expect(seedStamp.id).toBe(1);
		expect(seedStamp.event).toBe(0);
		
		// b: fork
		let [a, b] = fork(seedStamp);
		expect(equals(a, b)).toBe(true); // Same event, different ids
		
		// c: event on both
		a = event(a);
		b = event(b);
		expect(concurrent(a, b)).toBe(true); // Concurrent events
		
		// d: fork a, event b
		let [aNew, c] = fork(a);
		a = aNew;
		b = event(b);
		
		// e: event a, join b and c
		a = event(a);
		b = join(b, c);
		
		// f: fork b
		let [bNew, cNew] = fork(b);
		b = bNew;
		c = cNew;
		
		// g: join a and b
		a = join(a, b);
		
		// h: event a
		a = event(a);
		
		// a should be greater than c
		expect(leq(c, a)).toBe(true);
		expect(leq(a, c)).toBe(false);
		
		console.log('Final a:', toString(a));
		console.log('Final c:', toString(c));
	});
});

describe('ITC - Demo Run (Class API)', () => {
	
	test('complete demo scenario with classes', () => {
		// a: seed
		let a = new StampClass();
		
		// b: fork
		let b = a.fork();
		expect(a.equals(b)).toBe(true);
		
		// c: event on both
		a.event();
		b.event();
		expect(a.concurrent(b)).toBe(true);
		
		// d: fork a, event b
		let c = a.fork();
		b.event();
		
		// e: event a, join b and c
		a.event();
		b.join(c);
		
		// f: fork b
		c = b.fork();
		
		// g: join a and b
		a.join(b);
		
		// h: event a
		a.event();
		
		// a should be greater than c
		expect(c.leq(a)).toBe(true);
		expect(a.leq(c)).toBe(false);
		
		console.log('Final a:', a.toString());
		console.log('Final c:', c.toString());
	});
});

// ═══════════════════════════════════════════════════════════════════
// VERSION VECTOR SIMULATION
// ═══════════════════════════════════════════════════════════════════

describe('ITC - Simulating Version Vectors', () => {
	
	test('create 4 replicas', () => {
		const seedStamp = seed();
		
		const b = fork(seedStamp)[1];
		const c = fork(seedStamp)[1];
		const [cNew, d] = fork(c);
		
		// All should be equal initially
		expect(equals(seedStamp, b)).toBe(true);
		expect(equals(seedStamp, cNew)).toBe(true);
		expect(equals(seedStamp, d)).toBe(true);
	});
	
	test('replica updates create ordering', () => {
		const a = seed();
		const [aMod, b] = fork(a);
		const [cMod, d] = fork(aMod);
		
		// Update replica b
		const bUpdated = event(b);
		
		// b is now greater than all others
		expect(leq(a, bUpdated)).toBe(true);
		expect(leq(d, bUpdated)).toBe(true);
		
		// Update replica d (different partition)
		const dUpdated = event(d);
		
		// b and d should be concurrent (different id partitions)
		expect(concurrent(bUpdated, dUpdated)).toBe(true);
	});
	
	test('merge updates via join', () => {
		const a = seed();
		const [aLeft, aRight] = fork(a);
		let b = aRight;
		let d = fork(aLeft)[1];
		
		b = event(b);
		d = event(d);
		
		// They should be concurrent (different partitions)
		expect(concurrent(b, d)).toBe(true);
		
		// Merge via join
		const merged = join(b, d);
		
		// Merged reflects both events
		expect(leq(b, merged)).toBe(true);
		expect(leq(d, merged)).toBe(true);
	});
	
	test('merge updates via peek exchange', () => {
		const a = seed();
		const [aLeft, aRight] = fork(a);
		let b = aRight;
		let d = fork(aLeft)[1];
		
		b = event(b);
		d = event(d);
		
		// They should be concurrent
		expect(concurrent(b, d)).toBe(true);
		
		// Exchange events via peek (note: must peek d before updating b)
		const dPeek = peek(d);
		const bPeek = peek(b);
		b = join(b, dPeek);
		d = join(d, bPeek);
		
		// Now they should be equivalent (both have seen both events)
		expect(equals(b, d)).toBe(true);
	});
});

// ═══════════════════════════════════════════════════════════════════
// COMPLEX SCENARIOS
// ═══════════════════════════════════════════════════════════════════

describe('ITC - Complex Scenarios', () => {
	
	test('multiple forks and joins', () => {
		let s = seed();
		
		const [s1, s2] = fork(s);
		const [s3, s4] = fork(s1);
		const [s5, s6] = fork(s2);
		
		// 6 replicas now exist
		// Do events on different replicas
		const s3e = event(s3);
		const s5e = event(s5);
		const s6e = event(s6);
		
		// s3e and s5e are concurrent
		expect(concurrent(s3e, s5e)).toBe(true);
		
		// Join s3e and s5e
		const merged = join(s3e, s5e);
		
		// merged > s3e and merged > s5e
		expect(leq(s3e, merged)).toBe(true);
		expect(leq(s5e, merged)).toBe(true);
		
		// merged and s6e are concurrent
		expect(concurrent(merged, s6e)).toBe(true);
	});
	
	test('chain of events', () => {
		let s = seed();
		
		// Create a causal chain
		for (let i = 0; i < 10; i++) {
			s = event(s);
		}
		
		// After 10 events
		const s10 = s;
		
		// Create another stamp
		const s0 = seed();
		
		// s0 < s10
		expect(leq(s0, s10)).toBe(true);
		expect(leq(s10, s0)).toBe(false);
	});
	
	test('fork tree', () => {
		const root = seed();
		
		// Create binary tree of forks
		const [l1, r1] = fork(root);
		const [l2, r2] = fork(l1);
		const [l3, r3] = fork(r1);
		
		// All leaves should be equal to root (no events yet)
		expect(equals(l2, root)).toBe(true);
		expect(equals(r2, root)).toBe(true);
		expect(equals(l3, root)).toBe(true);
		expect(equals(r3, root)).toBe(true);
		
		// Add event to one leaf
		const l2e = event(l2);
		
		// Now l2e > all others
		expect(leq(r2, l2e)).toBe(true);
		expect(leq(l3, l2e)).toBe(true);
		expect(leq(r3, l2e)).toBe(true);
	});
});

// ═══════════════════════════════════════════════════════════════════
// EDGE CASES
// ═══════════════════════════════════════════════════════════════════

describe('ITC - Edge Cases', () => {
	
	test('clone creates independent copy', () => {
		const s1 = seed();
		const s2 = clone(s1);
		
		expect(equals(s1, s2)).toBe(true);
		
		// Modify s1
		const s1e = event(s1);
		
		// s2 should be unchanged
		expect(equals(s2, seed())).toBe(true);
		expect(leq(s2, s1e)).toBe(true);
		expect(leq(s1e, s2)).toBe(false);
	});
	
	test('join with self is idempotent', () => {
		const s = event(seed());
		const sJoined = join(s, s);
		
		expect(equals(s, sJoined)).toBe(true);
	});
	
	test('peek has null id', () => {
		const s = event(event(seed()));
		const p = peek(s);
		
		expect(p.id).toBe(0);
		expect(p.event).toBe(s.event);
		
		// Join with peek doesn't change id
		const s2 = seed();
		const s2joined = join(s2, p);
		
		expect(s2joined.id).toBe(s2.id);
	});
	
	test('multiple events on same stamp', () => {
		let s = seed();
		
		for (let i = 0; i < 100; i++) {
			s = event(s);
		}
		
		// Should still be valid
		expect(s.id).toBe(1);
		expect(typeof s.event === 'number' || typeof s.event === 'object').toBe(true);
	});
});

// ═══════════════════════════════════════════════════════════════════
// STRING REPRESENTATION
// ═══════════════════════════════════════════════════════════════════

describe('ITC - String Representation', () => {
	
	test('seed string representation', () => {
		const s = seed();
		const str = toString(s);
		
		expect(str).toContain('1'); // id
		expect(str).toContain('0'); // event
	});
	
	test('after event string representation', () => {
		const s = event(seed());
		const str = toString(s);
		
		expect(str).toBeDefined();
		expect(typeof str).toBe('string');
	});
	
	test('after fork string representation', () => {
		const [s1, s2] = fork(seed());
		const str1 = toString(s1);
		const str2 = toString(s2);
		
		expect(str1).toBeDefined();
		expect(str2).toBeDefined();
		expect(str1).not.toBe(str2);
	});
});

// ═══════════════════════════════════════════════════════════════════
// STATIC CLASS METHODS
// ═══════════════════════════════════════════════════════════════════

describe('ITC - Static Class Methods', () => {
	
	test('static event', () => {
		const s1 = StampClass.seed();
		const s2 = StampClass.event(s1);
		
		// s1 should be unchanged
		expect(s1.leq(s2)).toBe(true);
		expect(s2.leq(s1)).toBe(false);
	});
	
	test('static fork', () => {
		const s = StampClass.seed();
		const [s1, s2] = StampClass.fork(s);
		
		expect(s1.equals(s2)).toBe(true);
		expect(s1).not.toBe(s2); // Different objects
	});
	
	test('static join', () => {
		const s = StampClass.seed();
		const [s1, s2] = StampClass.fork(s);
		
		s1.event();
		s2.event();
		
		const merged = StampClass.join(s1, s2);
		
		expect(s1.leq(merged)).toBe(true);
		expect(s2.leq(merged)).toBe(true);
	});
	
	test('static peek', () => {
		const s = StampClass.seed();
		s.event();
		
		const p = StampClass.peek(s);
		
		expect(p.getStamp().id).toBe(0);
	});
});

// ═══════════════════════════════════════════════════════════════════
// SUMMARY
// ═══════════════════════════════════════════════════════════════════

console.log('\n✅ ITC (Interval Tree Clocks) Test Suite');
console.log('   - Basic operations (5 tests)');
console.log('   - Causality tracking (4 tests)');
console.log('   - Demo run functional (1 test)');
console.log('   - Demo run class-based (1 test)');
console.log('   - Version vector simulation (4 tests)');
console.log('   - Complex scenarios (3 tests)');
console.log('   - Edge cases (4 tests)');
console.log('   - String representation (3 tests)');
console.log('   - Static class methods (4 tests)');
console.log('   Total: 29 tests');
console.log('   Focus: Causality tracking in dynamic distributed systems');

