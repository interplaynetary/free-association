/**
 * H3 Spatial Indexing - Index Data Structures (Phase 2)
 * 
 * Three-tier index architecture for efficient space-time slot matching:
 * 1. TypeIndex: Exact match on type_id (highest selectivity)
 * 2. SpatialIndex: H3-based geographic indexing
 * 3. TemporalIndex: Hybrid calendar (month buckets + recurrence rules)
 * 
 * Query complexity: O(1 + k×(M+R) + K) vs O(N×M) brute force
 */

import * as h3 from 'h3-js';
import type { BaseSlot, Contact } from './resources.js';
import {
	computeH3Index,
	ensureH3Index,
	getCellsInRadius,
	REMOTE_H3_INDEX,
	DEFAULT_SEARCH_RADIUS_KM
} from './spatial.js';

// ═══════════════════════════════════════════════════════════════════
// TEMPORAL INDEX (Tier 3)
// ═══════════════════════════════════════════════════════════════════

/**
 * Temporal index using hybrid calendar approach
 * - One-time slots: Sparse month buckets
 * - Recurring slots: Compact recurrence rules
 * - Timeless slots: Always available (no start_date or recurrence)
 */
export class TemporalIndex {
	// One-time slots indexed by month (YYYY-MM)
	private oneTimeSlots: Map<string, BaseSlot[]>;

	// Recurring slots indexed by recurrence pattern
	private recurringSlots: Map<string, BaseSlot[]>;

	// Timeless slots (no time constraints)
	private timelessSlots: BaseSlot[];

	constructor() {
		this.oneTimeSlots = new Map();
		this.recurringSlots = new Map();
		this.timelessSlots = [];
	}

	/**
	 * Insert a slot into the temporal index
	 */
	insert(slot: BaseSlot): void {
		if (this.isRecurring(slot)) {
			// Recurring slot: index by recurrence pattern
			const recurrenceKey = this.getRecurrenceKey(slot);
			const existing = this.recurringSlots.get(recurrenceKey) ?? [];
			existing.push(slot);
			this.recurringSlots.set(recurrenceKey, existing);
		} else {
			const monthKey = this.getMonthKey(slot);
			if (monthKey) {
				// One-time slot: index by month
				const existing = this.oneTimeSlots.get(monthKey) ?? [];
				existing.push(slot);
				this.oneTimeSlots.set(monthKey, existing);
			} else {
				// Timeless slot
				this.timelessSlots.push(slot);
			}
		}
	}

	/**
	 * Query slots overlapping with a time range
	 */
	query(timeRange: { start: Date; end: Date }): BaseSlot[] {
		const results: BaseSlot[] = [];

		// Query one-time slots
		const months = this.getMonthsInRange(timeRange);
		for (const month of months) {
			const slots = this.oneTimeSlots.get(month) ?? [];
			results.push(...slots);
		}

		// Query recurring slots (all recurring slots potentially match)
		for (const slots of this.recurringSlots.values()) {
			results.push(...slots);
		}

		// Query timeless slots
		results.push(...this.timelessSlots);

		return results;
	}

	/**
	 * Remove a slot from the index
	 */
	remove(slot: BaseSlot): void {
		if (this.isRecurring(slot)) {
			const recurrenceKey = this.getRecurrenceKey(slot);
			const existing = this.recurringSlots.get(recurrenceKey) ?? [];
			const filtered = existing.filter(s => s.id !== slot.id);
			if (filtered.length > 0) {
				this.recurringSlots.set(recurrenceKey, filtered);
			} else {
				this.recurringSlots.delete(recurrenceKey);
			}
		} else {
			const monthKey = this.getMonthKey(slot);
			if (monthKey) {
				const existing = this.oneTimeSlots.get(monthKey) ?? [];
				const filtered = existing.filter(s => s.id !== slot.id);
				if (filtered.length > 0) {
					this.oneTimeSlots.set(monthKey, filtered);
				} else {
					this.oneTimeSlots.delete(monthKey);
				}
			} else {
				// Remove from timeless
				this.timelessSlots = this.timelessSlots.filter(s => s.id !== slot.id);
			}
		}
	}

	/**
	 * Get all slots in this temporal index
	 */
	getAllSlots(): BaseSlot[] {
		const results: BaseSlot[] = [];
		for (const slots of this.oneTimeSlots.values()) {
			results.push(...slots);
		}
		for (const slots of this.recurringSlots.values()) {
			results.push(...slots);
		}
		results.push(...this.timelessSlots);
		return results;
	}

	/**
	 * Clear all slots
	 */
	clear(): void {
		this.oneTimeSlots.clear();
		this.recurringSlots.clear();
		this.timelessSlots = [];
	}

	/**
	 * Get size (number of slots)
	 */
	get size(): number {
		let count = 0;
		for (const slots of this.oneTimeSlots.values()) {
			count += slots.length;
		}
		for (const slots of this.recurringSlots.values()) {
			count += slots.length;
		}
		count += this.timelessSlots.length;
		return count;
	}

	// Private helper methods

	private isRecurring(slot: BaseSlot): boolean {
		return !!(slot.recurrence);
	}

	private getMonthKey(slot: BaseSlot): string | null {
		if (!slot.start_date) return null;
		// Extract YYYY-MM from start_date
		return slot.start_date.substring(0, 7);
	}

	private getRecurrenceKey(slot: BaseSlot): string {
		// Simple key: just the recurrence type
		// Could be enhanced to include day_schedules, etc.
		return slot.recurrence || 'none';
	}

	private getMonthsInRange(timeRange: { start: Date; end: Date }): string[] {
		const months: string[] = [];
		const current = new Date(timeRange.start);
		const end = new Date(timeRange.end);

		while (current <= end) {
			const year = current.getFullYear();
			const month = String(current.getMonth() + 1).padStart(2, '0');
			months.push(`${year}-${month}`);
			current.setMonth(current.getMonth() + 1);
		}

		return months;
	}
}

// ═══════════════════════════════════════════════════════════════════
// SPATIAL INDEX (Tier 2)
// ═══════════════════════════════════════════════════════════════════

/**
 * Spatial index using H3 hexagonal grid
 */
export class SpatialIndex {
	// Map H3 cell ID → temporal index
	private cells: Map<string, TemporalIndex>;

	constructor() {
		this.cells = new Map();
	}

	/**
	 * Insert a slot into the spatial index
	 */
	insert(slot: BaseSlot): void {
		// Ensure slot has H3 index
		ensureH3Index(slot);
		const h3Index = slot.h3_index!;

		// Get or create temporal index for this cell
		let temporalIndex = this.cells.get(h3Index);
		if (!temporalIndex) {
			temporalIndex = new TemporalIndex();
			this.cells.set(h3Index, temporalIndex);
		}

		// Insert into temporal index
		temporalIndex.insert(slot);
	}

	/**
	 * Query slots within radius of a center cell
	 */
	query(centerCell: string, radiusKm: number): BaseSlot[] {
		// Get all cells covering the radius
		const coveringCells = getCellsInRadius(centerCell, radiusKm);

		// Query each cell's temporal index
		const results: BaseSlot[] = [];
		for (const cellId of coveringCells) {
			const temporalIndex = this.cells.get(cellId);
			if (temporalIndex) {
				results.push(...temporalIndex.getAllSlots());
			}
		}

		// Also include remote slots (they match everything)
		// valid for spatial queries (unless we are already querying the remote index)
		if (centerCell !== REMOTE_H3_INDEX) {
			const remoteIndex = this.cells.get(REMOTE_H3_INDEX);
			if (remoteIndex) {
				results.push(...remoteIndex.getAllSlots());
			}
		}

		return results;
	}

	/**
	 * Query slots within radius and time range
	 */
	querySpaceTime(
		centerCell: string,
		radiusKm: number,
		timeRange: { start: Date; end: Date }
	): BaseSlot[] {
		// Get all cells covering the radius
		const coveringCells = getCellsInRadius(centerCell, radiusKm);

		// Query each cell's temporal index
		const results: BaseSlot[] = [];
		for (const cellId of coveringCells) {
			const temporalIndex = this.cells.get(cellId);
			if (temporalIndex) {
				results.push(...temporalIndex.query(timeRange));
			}
		}

		// Also include remote slots
		if (centerCell !== REMOTE_H3_INDEX) {
			const remoteIndex = this.cells.get(REMOTE_H3_INDEX);
			if (remoteIndex) {
				results.push(...remoteIndex.query(timeRange));
			}
		}

		return results;
	}

	/**
	 * Remove a slot from the index
	 */
	remove(slot: BaseSlot): void {
		if (!slot.h3_index) return;

		const temporalIndex = this.cells.get(slot.h3_index);
		if (temporalIndex) {
			temporalIndex.remove(slot);

			// Clean up empty temporal indexes
			if (temporalIndex.size === 0) {
				this.cells.delete(slot.h3_index);
			}
		}
	}

	/**
	 * Get all slots in this spatial index
	 */
	getAllSlots(): BaseSlot[] {
		const results: BaseSlot[] = [];
		for (const temporalIndex of this.cells.values()) {
			results.push(...temporalIndex.getAllSlots());
		}
		return results;
	}

	/**
	 * Clear all slots
	 */
	clear(): void {
		this.cells.clear();
	}

	/**
	 * Get size (number of slots)
	 */
	get size(): number {
		let count = 0;
		for (const temporalIndex of this.cells.values()) {
			count += temporalIndex.size;
		}
		return count;
	}

	/**
	 * Get number of occupied cells
	 */
	get cellCount(): number {
		return this.cells.size;
	}
}

// ═══════════════════════════════════════════════════════════════════
// TYPE INDEX (Tier 1)
// ═══════════════════════════════════════════════════════════════════

/**
 * Type index - exact match on type_id (highest selectivity)
 */
export class TypeIndex {
	// Map type_id → spatial index
	private indexes: Map<string, SpatialIndex>;

	constructor() {
		this.indexes = new Map();
	}

	/**
	 * Insert a slot into the type index
	 */
	insert(slot: BaseSlot): void {
		// Get or create spatial index for this type
		let spatialIndex = this.indexes.get(slot.type_id);
		if (!spatialIndex) {
			spatialIndex = new SpatialIndex();
			this.indexes.set(slot.type_id, spatialIndex);
		}

		// Insert into spatial index
		spatialIndex.insert(slot);
	}

	/**
	 * Query slots by type
	 */
	query(typeId: string): BaseSlot[] {
		const spatialIndex = this.indexes.get(typeId);
		return spatialIndex ? spatialIndex.getAllSlots() : [];
	}

	/**
	 * Query slots by type and space
	 */
	queryTypeSpace(
		typeId: string,
		centerCell: string,
		radiusKm: number
	): BaseSlot[] {
		const spatialIndex = this.indexes.get(typeId);
		return spatialIndex ? spatialIndex.query(centerCell, radiusKm) : [];
	}

	/**
	 * Query slots by type, space, and time
	 */
	queryTypeSpaceTime(
		typeId: string,
		centerCell: string,
		radiusKm: number,
		timeRange: { start: Date; end: Date }
	): BaseSlot[] {
		const spatialIndex = this.indexes.get(typeId);
		return spatialIndex ? spatialIndex.querySpaceTime(centerCell, radiusKm, timeRange) : [];
	}

	/**
	 * Remove a slot from the index
	 */
	remove(slot: BaseSlot): void {
		const spatialIndex = this.indexes.get(slot.type_id);
		if (spatialIndex) {
			spatialIndex.remove(slot);

			// Clean up empty spatial indexes
			if (spatialIndex.size === 0) {
				this.indexes.delete(slot.type_id);
			}
		}
	}

	/**
	 * Get all slots in this type index
	 */
	getAllSlots(): BaseSlot[] {
		const results: BaseSlot[] = [];
		for (const spatialIndex of this.indexes.values()) {
			results.push(...spatialIndex.getAllSlots());
		}
		return results;
	}

	/**
	 * Clear all slots
	 */
	clear(): void {
		this.indexes.clear();
	}

	/**
	 * Get size (number of slots)
	 */
	get size(): number {
		let count = 0;
		for (const spatialIndex of this.indexes.values()) {
			count += spatialIndex.size;
		}
		return count;
	}

	/**
	 * Get number of types
	 */
	get typeCount(): number {
		return this.indexes.size;
	}
}

// ═══════════════════════════════════════════════════════════════════
// SKILL INDEX (Option 2 - Inverted Index)
// ═══════════════════════════════════════════════════════════════════

/**
 * Skill index - Maps Skill ID to Contacts who possess it
 * Used for efficient intersection filtering
 */
export class SkillIndex {
	// Map skill_id → Set of contact_ids
	private index: Map<string, Set<string>>;

	constructor() {
		this.index = new Map();
	}

	/**
	 * Index a contact's skills
	 */
	insert(contact: Contact): void {
		for (const skill of contact.skills) {
			let contactSet = this.index.get(skill.id);
			if (!contactSet) {
				contactSet = new Set();
				this.index.set(skill.id, contactSet);
			}
			contactSet.add(contact.contact_id);
		}
	}

	/**
	 * Remove a contact from the index
	 */
	remove(contact: Contact): void {
		for (const skill of contact.skills) {
			const contactSet = this.index.get(skill.id);
			if (contactSet) {
				contactSet.delete(contact.contact_id);
				if (contactSet.size === 0) {
					this.index.delete(skill.id);
				}
			}
		}
	}

	/**
	 * Get contacts that possess a specific skill
	 */
	query(skillId: string): Set<string> {
		return this.index.get(skillId) ?? new Set();
	}

	/**
	 * Get number of indexed skills
	 */
	get size(): number {
		return this.index.size;
	}

	clear(): void {
		this.index.clear();
	}
}

// ═══════════════════════════════════════════════════════════════════
// COMPOSITE SLOT INDEX (Main API)
// ═══════════════════════════════════════════════════════════════════

/**
 * Composite slot index - main API for indexed slot queries
 * 
 * Three-tier architecture:
 * TypeIndex → SpatialIndex → TemporalIndex → Slots[]
 */
export class SlotIndex {
	private typeIndex: TypeIndex;
	private skillIndex?: SkillIndex;

	constructor() {
		this.typeIndex = new TypeIndex();
	}

	/**
	 * Populate the separate SkillIndex with known contacts
	 * Enables "Option 2" filtering (Inverted Index)
	 */
	withContacts(contacts: Contact[]): SlotIndex {
		if (!this.skillIndex) {
			this.skillIndex = new SkillIndex();
		} else {
			this.skillIndex.clear();
		}

		for (const contact of contacts) {
			this.skillIndex.insert(contact);
		}
		return this;
	}

	/**
	 * Build index from array of slots
	 */
	build(slots: BaseSlot[]): void {
		this.clear();
		for (const slot of slots) {
			this.insert(slot);
		}
	}

	/**
	 * Insert a single slot
	 */
	insert(slot: BaseSlot): void {
		this.typeIndex.insert(slot);
	}

	/**
	 * Remove a single slot
	 */
	remove(slot: BaseSlot): void {
		this.typeIndex.remove(slot);
	}

	/**
	 * Query for slots matching a "probe" slot
	 * 
	 * Returns candidate slots that pass:
	 * 1. Type filter (exact match)
	 * 2. Spatial filter (within radius)
	 * 
	 * @param probeSlot - The slot to find matches for check (Need or Availability)
	 * @returns Array of candidate slots (Need or Availability)
	 */
	query(probeSlot: BaseSlot): BaseSlot[] {
		// Ensure probe slot has H3 index
		ensureH3Index(probeSlot);

		// 1. Type filter (O(1))
		const typeId = probeSlot.type_id;
		const h3Index = probeSlot.h3_index!;
		const radiusKm = probeSlot.search_radius_km ?? DEFAULT_SEARCH_RADIUS_KM;

		// 2. Spatial filter (O(k) where k = covering cells)
		let candidates = this.typeIndex.queryTypeSpace(typeId, h3Index, radiusKm);

		// 3. Skill Filter (Option 2 - Inverted Index Intersection)
		// If the probe slot requires skills, and we have a SkillIndex,
		// filter candidates to only those offered by qualified contacts.
		if (this.skillIndex && probeSlot.required_skills && probeSlot.required_skills.length > 0) {
			// Find set of contacts that have ALL required skills
			let qualifiedContacts: Set<string> | null = null;

			for (const reqSkill of probeSlot.required_skills) {
				const providers = this.skillIndex.query(reqSkill.id);

				if (qualifiedContacts === null) {
					// First skill - initialize set
					qualifiedContacts = new Set(providers);
				} else {
					// Subsequent skills - intersect
					for (const contactId of qualifiedContacts) {
						if (!providers.has(contactId)) {
							qualifiedContacts.delete(contactId);
						}
					}
				}

				// Optimization: If intersection becomes empty, no matches possible
				if (qualifiedContacts && qualifiedContacts.size === 0) {
					break;
				}
			}

			// Filter candidates by qualified offered_by
			if (qualifiedContacts && qualifiedContacts.size > 0) {
				candidates = candidates.filter(slot =>
					slot.offered_by && qualifiedContacts!.has(slot.offered_by)
				);
			} else {
				// Requirements exist but no one qualifies -> 0 matches
				return [];
			}
		}

		// 4. Temporal filter (TODO in Phase 4)
		// For now, we return broad spatial (and optionally skill-filtered) matches

		return candidates;
	}

	/**
	 * Get all slots in the index
	 */
	getAllSlots(): BaseSlot[] {
		return this.typeIndex.getAllSlots();
	}

	/**
	 * Clear the index
	 */
	clear(): void {
		this.typeIndex.clear();
	}

	/**
	 * Get index statistics
	 */
	getStats(): {
		totalSlots: number;
		typeCount: number;
		avgSlotsPerType: number;
	} {
		const totalSlots = this.typeIndex.size;
		const typeCount = this.typeIndex.typeCount;
		const avgSlotsPerType = typeCount > 0 ? totalSlots / typeCount : 0;

		return {
			totalSlots,
			typeCount,
			avgSlotsPerType
		};
	}
}

// ═══════════════════════════════════════════════════════════════════
// CONVENIENCE FUNCTIONS
// ═══════════════════════════════════════════════════════════════════

/**
 * Build a slot index from an array of slots
 */
export function buildSlotIndex(slots: BaseSlot[]): SlotIndex {
	const index = new SlotIndex();
	index.build(slots);
	return index;
}

/**
 * Find matching slots for a probe using the index
 * (Broad Phase only - caller must verify fine-grained compatibility)
 */
export function findMatchesIndexed(
	probeSlot: BaseSlot,
	index: SlotIndex
): BaseSlot[] {
	return index.query(probeSlot);
}
