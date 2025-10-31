/**
 * Provenance DAG Management
 * 
 * This module manages the Merkle-DAG of provenance events:
 * - Event storage and retrieval (Holster)
 * - Head tracking (latest events per author)
 * - Parent indexing (reverse edges for traversal)
 * - DAG traversal (BFS/DFS)
 * - Lineage queries
 * 
 * Storage Structure (in Holster user space):
 * ~{myPubKey}/provenance/
 *   events/{eventId}         -> EventStoreEntry
 *   heads/{authorId}         -> HeadIndexEntry
 *   parents/{parentId}       -> ParentIndexEntry
 */

import { holster, holsterUser, holsterUserPub } from '$lib/network/holster.svelte';
import { get } from 'svelte/store';
import type {
	ProvenanceEvent,
	EventStoreEntry,
	HeadIndexEntry,
	ParentIndexEntry,
	Hash
} from './provenance-event-schema';
import {
	parseProvenanceEvent,
	parseEventStoreEntry
} from './provenance-event-schema';

// ═══════════════════════════════════════════════════════════════════
// PATH HELPERS
// ═══════════════════════════════════════════════════════════════════

/**
 * Build Holster paths for provenance DAG storage
 */
export const ProvenancePaths = {
	/** Event storage path */
	event: (pubKey: string, eventId: Hash) => 
		`~${pubKey}/provenance/events/${eventId}`,
	
	/** Head index path (latest events per author) */
	head: (pubKey: string, authorId: string) => 
		`~${pubKey}/provenance/heads/${authorId}`,
	
	/** Parent index path (reverse edges) */
	parent: (pubKey: string, parentId: Hash) => 
		`~${pubKey}/provenance/parents/${parentId}`,
	
	/** All events path (for listing) */
	allEvents: (pubKey: string) => 
		`~${pubKey}/provenance/events`,
	
	/** All heads path (for listing) */
	allHeads: (pubKey: string) => 
		`~${pubKey}/provenance/heads`
};

// ═══════════════════════════════════════════════════════════════════
// EVENT STORAGE
// ═══════════════════════════════════════════════════════════════════

/**
 * Store a provenance event in Holster
 * 
 * Writes to:
 * - /provenance/events/{eventId} (event store entry)
 * - /provenance/heads/{authorId} (update head index)
 * - /provenance/parents/{parentId} (update parent indexes)
 * 
 * @param event - Provenance event to store
 * @param verified - Whether the event has been verified
 * @returns Promise that resolves when storage is complete
 */
export async function storeEvent(
	event: ProvenanceEvent,
	verified: boolean = false
): Promise<void> {
	const myPub = get(holsterUserPub);
	if (!myPub) {
		throw new Error('User not authenticated - cannot store events');
	}
	
	try {
		// Create store entry
		const storeEntry: EventStoreEntry = {
			event,
			stored_at: Date.now(),
			verified
		};
		
		// Store event
		await new Promise<void>((resolve, reject) => {
			holster
				.get(ProvenancePaths.event(myPub, event.id))
				.put(storeEntry, (err: any) => {
					if (err) {
						console.error('[PROVENANCE-DAG] Error storing event:', err);
						reject(err);
					} else {
						resolve();
					}
				});
		});
		
		// Update head index
		await updateHeadIndex(event.author, event.id);
		
		// Update parent indexes
		for (const parentId of event.parents) {
			await updateParentIndex(parentId, event.id);
		}
		
		console.log(`[PROVENANCE-DAG] Stored event: ${event.id.substring(0, 16)}...`);
		
	} catch (error) {
		console.error('[PROVENANCE-DAG] Failed to store event:', error);
		throw error;
	}
}

/**
 * Retrieve a provenance event from Holster
 * 
 * @param eventId - Event ID to retrieve
 * @param fromUser - Optional: retrieve from another user's space
 * @returns Promise resolving to event or null if not found
 */
export async function getEvent(
	eventId: Hash,
	fromUser?: string
): Promise<ProvenanceEvent | null> {
	const pubKey = fromUser || get(holsterUserPub);
	if (!pubKey) {
		throw new Error('No public key available');
	}
	
	return new Promise((resolve) => {
		holster
			.get(ProvenancePaths.event(pubKey, eventId))
			.once((data: any) => {
				if (!data) {
					resolve(null);
					return;
				}
				
				const entry = parseEventStoreEntry(data);
				if (!entry) {
					console.warn('[PROVENANCE-DAG] Invalid event store entry:', eventId);
					resolve(null);
					return;
				}
				
				resolve(entry.event);
			});
	});
}

/**
 * Check if an event exists in storage
 * 
 * @param eventId - Event ID to check
 * @param fromUser - Optional: check in another user's space
 * @returns Promise resolving to true if event exists
 */
export async function hasEvent(
	eventId: Hash,
	fromUser?: string
): Promise<boolean> {
	const event = await getEvent(eventId, fromUser);
	return event !== null;
}

/**
 * Get multiple events in batch
 * 
 * @param eventIds - Array of event IDs
 * @param fromUser - Optional: retrieve from another user's space
 * @returns Promise resolving to map of eventId -> event (nulls excluded)
 */
export async function getEventsBatch(
	eventIds: Hash[],
	fromUser?: string
): Promise<Map<Hash, ProvenanceEvent>> {
	const events = new Map<Hash, ProvenanceEvent>();
	
	for (const eventId of eventIds) {
		const event = await getEvent(eventId, fromUser);
		if (event) {
			events.set(eventId, event);
		}
	}
	
	return events;
}

// ═══════════════════════════════════════════════════════════════════
// HEAD INDEX (Latest Events Per Author)
// ═══════════════════════════════════════════════════════════════════

/**
 * Update head index when a new event is stored
 * 
 * @param authorId - Author's public key
 * @param eventId - New event ID
 */
async function updateHeadIndex(authorId: string, eventId: Hash): Promise<void> {
	const myPub = get(holsterUserPub);
	if (!myPub) return;
	
	return new Promise((resolve, reject) => {
		const path = ProvenancePaths.head(myPub, authorId);
		
		// Get current head entry
		holster.get(path).once((data: any) => {
			const currentEntry = data ? parseHeadEntry(data) : null;
			
			// Create or update head entry
			const headEntry: HeadIndexEntry = {
				author: authorId,
				event_ids: currentEntry 
					? [...new Set([...currentEntry.event_ids, eventId])] 
					: [eventId],
				updated_at: Date.now()
			};
			
			// Store updated head entry
			holster.get(path).put(headEntry, (err: any) => {
				if (err) {
					console.error('[PROVENANCE-DAG] Error updating head index:', err);
					reject(err);
				} else {
					resolve();
				}
			});
		});
	});
}

/**
 * Get head events for an author
 * 
 * @param authorId - Author's public key
 * @param fromUser - Optional: query another user's index
 * @returns Promise resolving to array of head event IDs
 */
export async function getHeadEvents(
	authorId: string,
	fromUser?: string
): Promise<Hash[]> {
	const pubKey = fromUser || get(holsterUserPub);
	if (!pubKey) return [];
	
	return new Promise((resolve) => {
		holster
			.get(ProvenancePaths.head(pubKey, authorId))
			.once((data: any) => {
				if (!data) {
					resolve([]);
					return;
				}
				
				const entry = parseHeadEntry(data);
				resolve(entry ? entry.event_ids : []);
			});
	});
}

function parseHeadEntry(data: any): HeadIndexEntry | null {
	try {
		return data as HeadIndexEntry;
	} catch {
		return null;
	}
}

// ═══════════════════════════════════════════════════════════════════
// PARENT INDEX (Reverse Edges for Traversal)
// ═══════════════════════════════════════════════════════════════════

/**
 * Update parent index when a new event is stored
 * 
 * @param parentId - Parent event ID
 * @param childId - Child event ID (the new event)
 */
async function updateParentIndex(parentId: Hash, childId: Hash): Promise<void> {
	const myPub = get(holsterUserPub);
	if (!myPub) return;
	
	return new Promise((resolve, reject) => {
		const path = ProvenancePaths.parent(myPub, parentId);
		
		// Get current parent entry
		holster.get(path).once((data: any) => {
			const currentEntry = data ? parseParentEntry(data) : null;
			
			// Create or update parent entry
			const parentEntry: ParentIndexEntry = {
				parent_id: parentId,
				child_ids: currentEntry 
					? [...new Set([...currentEntry.child_ids, childId])] 
					: [childId],
				updated_at: Date.now()
			};
			
			// Store updated parent entry
			holster.get(path).put(parentEntry, (err: any) => {
				if (err) {
					console.error('[PROVENANCE-DAG] Error updating parent index:', err);
					reject(err);
				} else {
					resolve();
				}
			});
		});
	});
}

/**
 * Get children of a parent event
 * 
 * @param parentId - Parent event ID
 * @param fromUser - Optional: query another user's index
 * @returns Promise resolving to array of child event IDs
 */
export async function getChildren(
	parentId: Hash,
	fromUser?: string
): Promise<Hash[]> {
	const pubKey = fromUser || get(holsterUserPub);
	if (!pubKey) return [];
	
	return new Promise((resolve) => {
		holster
			.get(ProvenancePaths.parent(pubKey, parentId))
			.once((data: any) => {
				if (!data) {
					resolve([]);
					return;
				}
				
				const entry = parseParentEntry(data);
				resolve(entry ? entry.child_ids : []);
			});
	});
}

function parseParentEntry(data: any): ParentIndexEntry | null {
	try {
		return data as ParentIndexEntry;
	} catch {
		return null;
	}
}

// ═══════════════════════════════════════════════════════════════════
// DAG TRAVERSAL
// ═══════════════════════════════════════════════════════════════════

/**
 * Traverse DAG from target event to roots (backward traversal)
 * 
 * Uses BFS to collect all ancestor events.
 * 
 * @param targetId - Starting event ID
 * @param maxDepth - Maximum traversal depth (default: Infinity)
 * @param fromUser - Optional: traverse another user's DAG
 * @returns Promise resolving to array of events (target to roots)
 */
export async function traverseToRoots(
	targetId: Hash,
	maxDepth: number = Infinity,
	fromUser?: string
): Promise<ProvenanceEvent[]> {
	const visited = new Set<Hash>();
	const events: ProvenanceEvent[] = [];
	const queue: Array<{ id: Hash; depth: number }> = [{ id: targetId, depth: 0 }];
	
	while (queue.length > 0) {
		const { id, depth } = queue.shift()!;
		
		// Skip if already visited or max depth reached
		if (visited.has(id) || depth > maxDepth) {
			continue;
		}
		
		visited.add(id);
		
		// Get event
		const event = await getEvent(id, fromUser);
		if (!event) {
			console.warn('[PROVENANCE-DAG] Event not found during traversal:', id);
			continue;
		}
		
		events.push(event);
		
		// Add parents to queue
		for (const parentId of event.parents) {
			if (!visited.has(parentId)) {
				queue.push({ id: parentId, depth: depth + 1 });
			}
		}
	}
	
	return events;
}

/**
 * Traverse DAG from root event to leaves (forward traversal)
 * 
 * Uses BFS with parent index to collect all descendant events.
 * 
 * @param rootId - Starting event ID
 * @param maxDepth - Maximum traversal depth (default: Infinity)
 * @param fromUser - Optional: traverse another user's DAG
 * @returns Promise resolving to array of events (root to leaves)
 */
export async function traverseToLeaves(
	rootId: Hash,
	maxDepth: number = Infinity,
	fromUser?: string
): Promise<ProvenanceEvent[]> {
	const visited = new Set<Hash>();
	const events: ProvenanceEvent[] = [];
	const queue: Array<{ id: Hash; depth: number }> = [{ id: rootId, depth: 0 }];
	
	while (queue.length > 0) {
		const { id, depth } = queue.shift()!;
		
		// Skip if already visited or max depth reached
		if (visited.has(id) || depth > maxDepth) {
			continue;
		}
		
		visited.add(id);
		
		// Get event
		const event = await getEvent(id, fromUser);
		if (!event) {
			console.warn('[PROVENANCE-DAG] Event not found during traversal:', id);
			continue;
		}
		
		events.push(event);
		
		// Add children to queue
		const children = await getChildren(id, fromUser);
		for (const childId of children) {
			if (!visited.has(childId)) {
				queue.push({ id: childId, depth: depth + 1 });
			}
		}
	}
	
	return events;
}

/**
 * Find path between two events
 * 
 * @param fromId - Starting event ID
 * @param toId - Target event ID
 * @param fromUser - Optional: search in another user's DAG
 * @returns Promise resolving to array of events (path from -> to) or null if no path
 */
export async function findPath(
	fromId: Hash,
	toId: Hash,
	fromUser?: string
): Promise<ProvenanceEvent[] | null> {
	const visited = new Set<Hash>();
	const queue: Array<{ id: Hash; path: Hash[] }> = [{ id: fromId, path: [fromId] }];
	
	while (queue.length > 0) {
		const { id, path } = queue.shift()!;
		
		// Found target
		if (id === toId) {
			// Reconstruct events from path
			const events = await getEventsBatch(path, fromUser);
			return path.map(eventId => events.get(eventId)!).filter(e => e);
		}
		
		// Skip if already visited
		if (visited.has(id)) {
			continue;
		}
		
		visited.add(id);
		
		// Get event
		const event = await getEvent(id, fromUser);
		if (!event) continue;
		
		// Check parents (backward)
		for (const parentId of event.parents) {
			if (!visited.has(parentId)) {
				queue.push({ id: parentId, path: [...path, parentId] });
			}
		}
		
		// Check children (forward)
		const children = await getChildren(id, fromUser);
		for (const childId of children) {
			if (!visited.has(childId)) {
				queue.push({ id: childId, path: [...path, childId] });
			}
		}
	}
	
	return null;
}

// ═══════════════════════════════════════════════════════════════════
// LINEAGE QUERIES
// ═══════════════════════════════════════════════════════════════════

/**
 * Get all root events (events with no parents)
 * 
 * @param fromUser - Optional: query another user's DAG
 * @returns Promise resolving to array of root events
 */
export async function getRootEvents(fromUser?: string): Promise<ProvenanceEvent[]> {
	const pubKey = fromUser || get(holsterUserPub);
	if (!pubKey) return [];
	
	// This is inefficient - would need an index for roots
	// For now, we traverse all events and filter
	// TODO: Add root index for efficient queries
	
	console.warn('[PROVENANCE-DAG] getRootEvents() requires full scan - performance issue');
	return [];
}

/**
 * Get all leaf events (events with no children)
 * 
 * @param fromUser - Optional: query another user's DAG
 * @returns Promise resolving to array of leaf events
 */
export async function getLeafEvents(fromUser?: string): Promise<ProvenanceEvent[]> {
	const pubKey = fromUser || get(holsterUserPub);
	if (!pubKey) return [];
	
	// This is inefficient - would need to scan all events
	// TODO: Add leaf index or use head index
	
	console.warn('[PROVENANCE-DAG] getLeafEvents() requires full scan - performance issue');
	return [];
}

/**
 * Count events in DAG
 * 
 * @param fromUser - Optional: count in another user's DAG
 * @returns Promise resolving to event count
 */
export async function countEvents(fromUser?: string): Promise<number> {
	// TODO: Implement efficient counting
	console.warn('[PROVENANCE-DAG] countEvents() not yet implemented');
	return 0;
}

// ═══════════════════════════════════════════════════════════════════
// UTILITY FUNCTIONS
// ═══════════════════════════════════════════════════════════════════

/**
 * Delete an event (use with caution - breaks DAG integrity!)
 * 
 * @param eventId - Event ID to delete
 * @returns Promise that resolves when deletion is complete
 */
export async function deleteEvent(eventId: Hash): Promise<void> {
	const myPub = get(holsterUserPub);
	if (!myPub) {
		throw new Error('User not authenticated');
	}
	
	console.warn('[PROVENANCE-DAG] Deleting event - this breaks DAG integrity:', eventId);
	
	return new Promise((resolve, reject) => {
		holster.get(ProvenancePaths.event(myPub, eventId)).put(null, (err: any) => {
			if (err) {
				reject(err);
			} else {
				resolve();
			}
		});
	});
}

/**
 * Export DAG to JSON (for backup/analysis)
 * 
 * @param fromUser - Optional: export from another user's DAG
 * @returns Promise resolving to JSON representation
 */
export async function exportDAG(fromUser?: string): Promise<any> {
	// TODO: Implement DAG export
	console.warn('[PROVENANCE-DAG] exportDAG() not yet implemented');
	return { events: [], heads: [], parents: [] };
}

