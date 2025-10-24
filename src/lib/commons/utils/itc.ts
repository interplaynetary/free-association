/**
 * Interval Tree Clocks (ITC) - TypeScript Implementation
 * 
 * Translated from the reference Erlang implementation by Paulo Sergio Almeida.
 * 
 * Interval Tree Clocks can be used in scenarios with a dynamic number of 
 * participants, allowing completely decentralized creation of processes/replicas 
 * without need for global identifiers.
 * 
 * Original: http://gsd.di.uminho.pt/members/cbm/ps/itc2012.pdf
 * Reference implementation: https://github.com/ricardobcl/Interval-Tree-Clocks
 * 
 * @author Original: Paulo Sergio Almeida <psa@di.uminho.pt>
 * @author TypeScript translation: 2025
 */

// ═══════════════════════════════════════════════════════════════════
// TYPES
// ═══════════════════════════════════════════════════════════════════

/**
 * Id component of a Stamp
 * Can be:
 * - 0 (null id)
 * - 1 (full id)
 * - {l, r} (split id with left and right components)
 */
export type Id = 0 | 1 | { l: Id; r: Id };

/**
 * Event component of a Stamp
 * Can be:
 * - N (integer counter)
 * - {n, l, r} (tree node with base counter and left/right subtrees)
 */
export type Event = number | { n: number; l: Event; r: Event };

/**
 * Stamp is a pair of Id and Event
 */
export interface Stamp {
	id: Id;
	event: Event;
}

// ═══════════════════════════════════════════════════════════════════
// CORE API
// ═══════════════════════════════════════════════════════════════════

/**
 * Create a seed stamp with full ownership
 * Returns: {1, 0}
 */
export function seed(): Stamp {
	return { id: 1, event: 0 };
}

/**
 * Increment the event component of a stamp
 */
export function event(stamp: Stamp): Stamp {
	const { id, event: e } = stamp;
	const filled = fill(id, e);
	const newEvent = filled === e ? grow(id, e)[1] : filled;
	return { id, event: newEvent };
}

/**
 * Split a stamp into two stamps with distinct ids but same event
 * Returns: [stamp1, stamp2]
 */
export function fork(stamp: Stamp): [Stamp, Stamp] {
	const { id, event: e } = stamp;
	const [id1, id2] = split(id);
	return [
		{ id: id1, event: e },
		{ id: id2, event: e }
	];
}

/**
 * Merge two stamps into one
 */
export function join(stamp1: Stamp, stamp2: Stamp): Stamp {
	const id = sumId(stamp1.id, stamp2.id);
	const event = joinEvent(stamp1.event, stamp2.event);
	return { id, event };
}

/**
 * Create a stamp with null id but same event (for message passing)
 */
export function peek(stamp: Stamp): Stamp {
	return { id: 0, event: stamp.event };
}

/**
 * Check if stamp1 causally precedes or is equal to stamp2
 */
export function leq(stamp1: Stamp, stamp2: Stamp): boolean {
	return leqEvent(stamp1.event, stamp2.event);
}

// ═══════════════════════════════════════════════════════════════════
// EVENT COMPARISON
// ═══════════════════════════════════════════════════════════════════

function leqEvent(e1: Event, e2: Event): boolean {
	// Both are tree nodes
	if (typeof e1 === 'object' && typeof e2 === 'object') {
		return (
			e1.n <= e2.n &&
			leqEvent(lift(e1.n, e1.l), lift(e2.n, e2.l)) &&
			leqEvent(lift(e1.n, e1.r), lift(e2.n, e2.r))
		);
	}
	
	// e1 is tree node, e2 is integer
	if (typeof e1 === 'object' && typeof e2 === 'number') {
		return (
			e1.n <= e2 &&
			leqEvent(lift(e1.n, e1.l), e2) &&
			leqEvent(lift(e1.n, e1.r), e2)
		);
	}
	
	// e1 is integer, e2 is tree node
	if (typeof e1 === 'number' && typeof e2 === 'object') {
		return e1 <= e2.n;
	}
	
	// Both are integers
	return e1 <= e2;
}

// ═══════════════════════════════════════════════════════════════════
// NORMALIZATION
// ═══════════════════════════════════════════════════════════════════

function normId(id: Id): Id {
	if (typeof id === 'object' && id.l === 0 && id.r === 0) return 0;
	if (typeof id === 'object' && id.l === 1 && id.r === 1) return 1;
	return id;
}

function normEvent(event: Event): Event {
	if (typeof event === 'number') return event;
	
	const { n, l, r } = event;
	
	// Both children are same integer
	if (typeof l === 'number' && typeof r === 'number' && l === r) {
		return n + l;
	}
	
	// Normalize tree node
	const m = Math.min(base(l), base(r));
	return { n: n + m, l: drop(m, l), r: drop(m, r) };
}

// ═══════════════════════════════════════════════════════════════════
// ID OPERATIONS
// ═══════════════════════════════════════════════════════════════════

function sumId(id1: Id, id2: Id): Id {
	if (id1 === 0) return id2;
	if (id2 === 0) return id1;
	if (id1 === 1 && id2 === 1) return 1;
	
	// Both are objects
	if (typeof id1 === 'object' && typeof id2 === 'object') {
		return normId({
			l: sumId(id1.l, id2.l),
			r: sumId(id1.r, id2.r)
		});
	}
	
	// One is 1, other is object - this means full coverage
	if (id1 === 1 || id2 === 1) return 1;
	
	// Should not reach here for valid inputs
	throw new Error(`Invalid id sum operation: id1=${JSON.stringify(id1)}, id2=${JSON.stringify(id2)}`);
}

function split(id: Id): [Id, Id] {
	if (id === 0) return [0, 0];
	if (id === 1) return [{ l: 1, r: 0 }, { l: 0, r: 1 }];
	
	// id is {l, r}
	const { l, r } = id as { l: Id; r: Id };
	
	if (l === 0) {
		const [r1, r2] = split(r);
		return [{ l: 0, r: r1 }, { l: 0, r: r2 }];
	}
	
	if (r === 0) {
		const [l1, l2] = split(l);
		return [{ l: l1, r: 0 }, { l: l2, r: 0 }];
	}
	
	// Both l and r are non-zero
	return [{ l, r: 0 }, { l: 0, r }];
}

// ═══════════════════════════════════════════════════════════════════
// EVENT OPERATIONS
// ═══════════════════════════════════════════════════════════════════

function joinEvent(e1: Event, e2: Event): Event {
	// Ensure e1.n <= e2.n (swap if needed)
	if (typeof e1 === 'object' && typeof e2 === 'object' && e1.n > e2.n) {
		return joinEvent(e2, e1);
	}
	
	// Both are tree nodes
	if (typeof e1 === 'object' && typeof e2 === 'object') {
		const d = e2.n - e1.n;
		return normEvent({
			n: e1.n,
			l: joinEvent(e1.l, lift(d, e2.l)),
			r: joinEvent(e1.r, lift(d, e2.r))
		});
	}
	
	// e1 is integer, e2 is tree node
	if (typeof e1 === 'number' && typeof e2 === 'object') {
		return joinEvent({ n: e1, l: 0, r: 0 }, e2);
	}
	
	// e1 is tree node, e2 is integer
	if (typeof e1 === 'object' && typeof e2 === 'number') {
		return joinEvent(e1, { n: e2, l: 0, r: 0 });
	}
	
	// Both are integers
	return Math.max(e1 as number, e2 as number);
}

function fill(id: Id, event: Event): Event {
	if (id === 0) return event;
	
	if (id === 1 && typeof event === 'object') {
		return height(event);
	}
	
	if (typeof event === 'number') return event;
	
	const { n, l: el, r: er } = event;
	
	// id is {1, r}
	if (typeof id === 'object' && id.l === 1) {
		const er1 = fill(id.r, er);
		const d = Math.max(height(el), base(er1));
		return normEvent({ n, l: d, r: er1 });
	}
	
	// id is {l, 1}
	if (typeof id === 'object' && id.r === 1) {
		const el1 = fill(id.l, el);
		const d = Math.max(height(er), base(el1));
		return normEvent({ n, l: el1, r: d });
	}
	
	// id is {l, r} where both are non-trivial
	if (typeof id === 'object') {
		return normEvent({
			n,
			l: fill(id.l, el),
			r: fill(id.r, er)
		});
	}
	
	return event;
}

function grow(id: Id, event: Event): [number, Event] {
	// id is 1, event is integer
	if (id === 1 && typeof event === 'number') {
		return [0, event + 1];
	}
	
	// id is {0, i}, event is tree node
	if (typeof id === 'object' && id.l === 0 && typeof event === 'object') {
		const [h, e1] = grow(id.r, event.r);
		return [h + 1, { n: event.n, l: event.l, r: e1 }];
	}
	
	// id is {i, 0}, event is tree node
	if (typeof id === 'object' && id.r === 0 && typeof event === 'object') {
		const [h, e1] = grow(id.l, event.l);
		return [h + 1, { n: event.n, l: e1, r: event.r }];
	}
	
	// id is {il, ir}, event is tree node
	if (
		typeof id === 'object' &&
		id.l !== 0 &&
		id.r !== 0 &&
		typeof event === 'object'
	) {
		const [hl, el] = grow(id.l, event.l);
		const [hr, er] = grow(id.r, event.r);
		
		if (hl < hr) {
			return [hl + 1, { n: event.n, l: el, r: event.r }];
		} else {
			return [hr + 1, { n: event.n, l: event.l, r: er }];
		}
	}
	
	// id is non-trivial, event is integer
	if (typeof event === 'number') {
		const [h, e] = grow(id, { n: event, l: 0, r: 0 });
		return [h + 1000, e];
	}
	
	throw new Error('Invalid grow operation');
}

// ═══════════════════════════════════════════════════════════════════
// HELPER FUNCTIONS
// ═══════════════════════════════════════════════════════════════════

function height(event: Event): number {
	if (typeof event === 'number') return event;
	return event.n + Math.max(height(event.l), height(event.r));
}

function base(event: Event): number {
	if (typeof event === 'number') return event;
	return event.n;
}

function lift(m: number, event: Event): Event {
	if (typeof event === 'number') return event + m;
	return { n: event.n + m, l: event.l, r: event.r };
}

function drop(m: number, event: Event): Event {
	if (typeof event === 'number') {
		if (m > event) throw new Error('Cannot drop more than event value');
		return event - m;
	}
	
	if (m > event.n) throw new Error('Cannot drop more than base value');
	return { n: event.n - m, l: event.l, r: event.r };
}

// ═══════════════════════════════════════════════════════════════════
// STRING REPRESENTATION
// ═══════════════════════════════════════════════════════════════════

/**
 * Convert stamp to string representation
 */
export function toString(stamp: Stamp): string {
	return `[${idToString(stamp.id)}, ${eventToString(stamp.event)}]`;
}

function idToString(id: Id): string {
	if (id === 0) return '0';
	if (id === 1) return '1';
	
	const { l, r } = id;
	
	if (l === 0) return 'R' + idToString(r);
	if (r === 0) return 'L' + idToString(l);
	
	return `(L${idToString(l)}+R${idToString(r)})`;
}

function eventToString(event: Event): string {
	if (typeof event === 'number') {
		return event.toString();
	}
	
	const { n, l, r } = event;
	const nStr = n > 0 ? n.toString() : '';
	
	if (typeof l === 'number' && l === 0 && typeof r !== 'number') {
		return nStr + 'R' + eventToString(r);
	}
	
	if (typeof r === 'number' && r === 0 && typeof l !== 'number') {
		return nStr + 'L' + eventToString(l);
	}
	
	if ((typeof l !== 'number' || l !== 0) && (typeof r !== 'number' || r !== 0)) {
		return `${nStr}(L${eventToString(l)}+R${eventToString(r)})`;
	}
	
	return nStr;
}

// ═══════════════════════════════════════════════════════════════════
// UTILITY FUNCTIONS
// ═══════════════════════════════════════════════════════════════════

/**
 * Check if two stamps are equal
 */
export function equals(stamp1: Stamp, stamp2: Stamp): boolean {
	return leq(stamp1, stamp2) && leq(stamp2, stamp1);
}

/**
 * Check if two stamps are concurrent (neither precedes the other)
 */
export function concurrent(stamp1: Stamp, stamp2: Stamp): boolean {
	return !leq(stamp1, stamp2) && !leq(stamp2, stamp1);
}

/**
 * Clone a stamp (deep copy)
 */
export function clone(stamp: Stamp): Stamp {
	return {
		id: cloneId(stamp.id),
		event: cloneEvent(stamp.event)
	};
}

function cloneId(id: Id): Id {
	if (typeof id === 'number') return id;
	return { l: cloneId(id.l), r: cloneId(id.r) };
}

function cloneEvent(event: Event): Event {
	if (typeof event === 'number') return event;
	return { n: event.n, l: cloneEvent(event.l), r: cloneEvent(event.r) };
}

// ═══════════════════════════════════════════════════════════════════
// CLASS-BASED API (JAVA-like style)
// ═══════════════════════════════════════════════════════════════════

/**
 * Stamp class for object-oriented style usage
 * Similar to the Java implementation
 */
export class StampClass {
	private stamp: Stamp;
	
	/**
	 * Create a new seed stamp
	 */
	constructor(stamp?: Stamp) {
		this.stamp = stamp || seed();
	}
	
	/**
	 * Get the underlying stamp
	 */
	getStamp(): Stamp {
		return this.stamp;
	}
	
	/**
	 * Increment the event component (mutating)
	 */
	event(): void {
		this.stamp = event(this.stamp);
	}
	
	/**
	 * Fork this stamp into two (returns new stamp, mutates this one)
	 */
	fork(): StampClass {
		const [s1, s2] = fork(this.stamp);
		this.stamp = s1;
		return new StampClass(s2);
	}
	
	/**
	 * Join with another stamp (mutating)
	 */
	join(other: StampClass): void {
		this.stamp = join(this.stamp, other.stamp);
	}
	
	/**
	 * Create a peek stamp (non-mutating)
	 */
	peek(): StampClass {
		return new StampClass(peek(this.stamp));
	}
	
	/**
	 * Check if this stamp precedes or equals another
	 */
	leq(other: StampClass): boolean {
		return leq(this.stamp, other.stamp);
	}
	
	/**
	 * Check if equal to another stamp
	 */
	equals(other: StampClass): boolean {
		return equals(this.stamp, other.stamp);
	}
	
	/**
	 * Check if concurrent with another stamp
	 */
	concurrent(other: StampClass): boolean {
		return concurrent(this.stamp, other.stamp);
	}
	
	/**
	 * Clone this stamp
	 */
	clone(): StampClass {
		return new StampClass(clone(this.stamp));
	}
	
	/**
	 * String representation
	 */
	toString(): string {
		return toString(this.stamp);
	}
	
	// ═══════════════════════════════════════════════════════════════
	// STATIC METHODS (Functional style)
	// ═══════════════════════════════════════════════════════════════
	
	/**
	 * Create a seed stamp
	 */
	static seed(): StampClass {
		return new StampClass();
	}
	
	/**
	 * Event (functional - returns new stamp)
	 */
	static event(stamp: StampClass): StampClass {
		return new StampClass(event(stamp.stamp));
	}
	
	/**
	 * Fork (functional - returns array of two stamps)
	 */
	static fork(stamp: StampClass): [StampClass, StampClass] {
		const [s1, s2] = fork(stamp.stamp);
		return [new StampClass(s1), new StampClass(s2)];
	}
	
	/**
	 * Join (functional - returns new stamp)
	 */
	static join(stamp1: StampClass, stamp2: StampClass): StampClass {
		return new StampClass(join(stamp1.stamp, stamp2.stamp));
	}
	
	/**
	 * Peek (functional - returns new stamp)
	 */
	static peek(stamp: StampClass): StampClass {
		return new StampClass(peek(stamp.stamp));
	}
}

// ═══════════════════════════════════════════════════════════════════
// EXPORTS
// ═══════════════════════════════════════════════════════════════════

export default {
	// Core API (functional)
	seed,
	event,
	fork,
	join,
	peek,
	leq,
	
	// Utilities
	equals,
	concurrent,
	clone,
	toString,
	
	// Class-based API
	Stamp: StampClass
};

