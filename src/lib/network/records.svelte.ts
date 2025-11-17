/**
 * Coalition Records System
 * 
 * Manages secretariat records using Holster distributed storage.
 * Records are stored per-user and synchronized across the network.
 * 
 * Storage structure:
 * - User's records: user().get('coalition').get('records').get(recordId)
 * - Record index: user().get('coalition').get('record_index') - array of record IDs
 */

import { writable, derived, get } from 'svelte/store';
import type { Readable, Writable } from 'svelte/store';
import { holster, holsterUser, isAuthenticated } from './holster';
import { createVersionedStore, type VersionedStore } from '../utils/primitives/v-store.svelte';
import type { 
	Record as CoalitionRecord, 
	UUID,
	MembershipUpdateRecord,
	RegistryEntryRecord,
	ContactInfoRecord,
	RecognitionDistributionRecord,
	StateDeclarationRecord,
	CapacityOfferRecord,
	ProposalRecord,
	StatementRecord,
	PositionRecord,
	SupportExpressionRecord,
	DecisionOutcomeRecord,
	ProtocolAdoptionRecord,
	InvitationRecord,
	InvitationResponseRecord,
	AssemblyMinutesRecord,
	AllocationDecisionRecord,
	SubscriptionRecord,
	SubscriptionUpdateRecord,
	DerivationRuleRecord,
	FilterDefinitionRecord,
	ComputedResultRecord,
	RecordAmendmentRecord,
	FrameworkVersionRecord,
	ValidationReportRecord,
	DisputeRecord,
	DisputeResolutionRecord,
	RecordSchema
} from '$lib/coalition/record';
import { validateRecord } from '$lib/coalition/record';

// ═══════════════════════════════════════════════════════════════════
// STATE STORES
// ═══════════════════════════════════════════════════════════════════

/**
 * My records (issued by me)
 */
export const myRecords = writable<Map<UUID, CoalitionRecord>>(new Map());

/**
 * Network records (from subscribed users)
 * Key: pubKey -> recordId -> CoalitionRecord
 */
export const networkRecords = writable<Map<string, Map<UUID, CoalitionRecord>>>(new Map());

/**
 * Subscribed participants (for record syncing)
 */
export const subscribedRecordParticipants = writable<Set<string>>(new Set());

/**
 * Record loading states
 */
export const recordLoadingStates = writable<Map<string, boolean>>(new Map());

// ═══════════════════════════════════════════════════════════════════
// VERSIONED STORE (for advanced use cases)
// ═══════════════════════════════════════════════════════════════════

/**
 * Versioned store for records with fine-grained reactivity
 * Tracks changes at the record level with ITC causality
 */
export const recordStore = createVersionedStore<CoalitionRecord, UUID>({
	fields: {
		status: (r) => r.status,
		data: (r) => r.data,
		decision_timestamp: (r) => r.decision_timestamp
	},
	timestampExtractor: (r) => new Date(r.timestamp).getTime(),
	enableLogging: true
});

// ═══════════════════════════════════════════════════════════════════
// INITIALIZATION
// ═══════════════════════════════════════════════════════════════════

let myRecordsUnsubscribe: (() => void) | null = null;
let networkUnsubscribes: Map<string, (() => void)> = new Map();

/**
 * Initialize my records (listen to my own record space)
 */
export function initializeMyRecords(): void {
	if (!isAuthenticated()) {
		console.warn('[RECORDS] Cannot initialize - not authenticated');
		return;
	}

	const authState = holsterUser.is;
	if (!authState) return;

	console.log('[RECORDS] Initializing my records...');

	// Subscribe to my record index
	const recordIndexRef = holster.user().get('coalition').get('record_index');
	
	recordIndexRef.next().get((data: any, key: string) => {
		if (!data || typeof data !== 'object') return;
		
		// data is expected to be an array of record IDs
		const recordIds = Array.isArray(data) ? data : (data.ids || []);
		
		console.log(`[RECORDS] Found ${recordIds.length} records in index`);
		
		// Load each record
		recordIds.forEach((recordId: string) => {
			loadMyRecord(recordId);
		});
	});
}

/**
 * Load a specific record from my user space
 */
function loadMyRecord(recordId: UUID): void {
	const recordRef = holster.user().get('coalition').get('records').get(recordId);
	
	recordRef.next().get((data: any, key: string) => {
		if (!data || typeof data !== 'object') return;
		
		try {
			// Validate record
			const record = validateRecord(data);
			
			// Update stores
			myRecords.update(map => {
				const newMap = new Map(map);
				newMap.set(recordId, record);
				return newMap;
			});
			
			// Update versioned store
			recordStore.update(recordId, record);
			
			console.log(`[RECORDS] ✅ Loaded my record: ${recordId} (${record.type})`);
		} catch (error) {
			console.error(`[RECORDS] ❌ Invalid record ${recordId}:`, error);
		}
	});
}

/**
 * Subscribe to another participant's records
 */
export function subscribeToParticipantRecords(pubKey: string): void {
	if (!isAuthenticated()) {
		console.warn('[RECORDS] Cannot subscribe - not authenticated');
		return;
	}

	// Check if already subscribed
	if (networkUnsubscribes.has(pubKey)) {
		console.log(`[RECORDS] Already subscribed to ${pubKey.slice(0, 20)}...`);
		return;
	}

	console.log(`[RECORDS] 📡 Subscribing to records from ${pubKey.slice(0, 20)}...`);

	// Add to subscribed set
	subscribedRecordParticipants.update(set => {
		const newSet = new Set(set);
		newSet.add(pubKey);
		return newSet;
	});

	// Set loading state
	recordLoadingStates.update(map => {
		const newMap = new Map(map);
		newMap.set(pubKey, true);
		return newMap;
	});

	// Subscribe to their record index
	const recordIndexRef = holster.user(pubKey).get('coalition').get('record_index');
	
	const unsubIndex = recordIndexRef.next().get((data: any, key: string) => {
		if (!data || typeof data !== 'object') return;
		
		const recordIds = Array.isArray(data) ? data : (data.ids || []);
		
		console.log(`[RECORDS] Found ${recordIds.length} records from ${pubKey.slice(0, 20)}...`);
		
		// Load each record
		recordIds.forEach((recordId: string) => {
			loadNetworkRecord(pubKey, recordId);
		});

		// Clear loading state
		recordLoadingStates.update(map => {
			const newMap = new Map(map);
			newMap.set(pubKey, false);
			return newMap;
		});
	});

	// Store unsubscribe function
	networkUnsubscribes.set(pubKey, unsubIndex);
}

/**
 * Load a specific record from another participant
 */
function loadNetworkRecord(pubKey: string, recordId: UUID): void {
	const recordRef = holster.user(pubKey).get('coalition').get('records').get(recordId);
	
	recordRef.next().get((data: any, key: string) => {
		if (!data || typeof data !== 'object') return;
		
		try {
			// Validate record
			const record = validateRecord(data);
			
			// Verify issuer matches pubKey (security check)
			if (record.issuer !== pubKey) {
				console.warn(`[RECORDS] ⚠️  Issuer mismatch for record ${recordId}: expected ${pubKey.slice(0, 20)}, got ${record.issuer.slice(0, 20)}`);
				// Still store it but log the warning
			}
			
			// Update network records store
			networkRecords.update(map => {
				const newMap = new Map(map);
				if (!newMap.has(pubKey)) {
					newMap.set(pubKey, new Map());
				}
				newMap.get(pubKey)!.set(recordId, record);
				return newMap;
			});
			
			console.log(`[RECORDS] ✅ Loaded network record: ${recordId} from ${pubKey.slice(0, 20)}... (${record.type})`);
		} catch (error) {
			console.error(`[RECORDS] ❌ Invalid record ${recordId} from ${pubKey.slice(0, 20)}...:`, error);
		}
	});
}

/**
 * Unsubscribe from a participant's records
 */
export function unsubscribeFromParticipantRecords(pubKey: string): void {
	const unsubscribe = networkUnsubscribes.get(pubKey);
	if (unsubscribe) {
		unsubscribe();
		networkUnsubscribes.delete(pubKey);
	}

	// Remove from subscribed set
	subscribedRecordParticipants.update(set => {
		const newSet = new Set(set);
		newSet.delete(pubKey);
		return newSet;
	});

	// Remove records
	networkRecords.update(map => {
		const newMap = new Map(map);
		newMap.delete(pubKey);
		return newMap;
	});

	console.log(`[RECORDS] Unsubscribed from ${pubKey.slice(0, 20)}...`);
}

// ═══════════════════════════════════════════════════════════════════
// RECORD CREATION & MANAGEMENT
// ═══════════════════════════════════════════════════════════════════

/**
 * Issue a new record (stores it in my user space)
 */
export async function issueRecord(record: CoalitionRecord): Promise<void> {
	if (!isAuthenticated()) {
		throw new Error('Cannot issue record - not authenticated');
	}

	const authState = holsterUser.is;
	if (!authState) {
		throw new Error('No authenticated user');
	}

	// Validate record
	try {
		validateRecord(record);
	} catch (error) {
		throw new Error(`Invalid record: ${error}`);
	}

	// Ensure issuer matches current user
	if (record.issuer !== authState.pub) {
		throw new Error('Record issuer must match authenticated user');
	}

	console.log(`[RECORDS] 📝 Issuing record ${record.id} (${record.type})...`);

	// Store record
	const recordRef = holster.user().get('coalition').get('records').get(record.id);
	await new Promise<void>((resolve, reject) => {
		recordRef.put(record, (ack: any) => {
			if (ack.err) {
				reject(new Error(ack.err));
			} else {
				resolve();
			}
		});
	});

	// Update record index
	const currentRecords = get(myRecords);
	const recordIds = Array.from(currentRecords.keys());
	recordIds.push(record.id);

	const indexRef = holster.user().get('coalition').get('record_index');
	await new Promise<void>((resolve, reject) => {
		indexRef.put({ ids: recordIds, updated: Date.now() }, (ack: any) => {
			if (ack.err) {
				reject(new Error(ack.err));
			} else {
				resolve();
			}
		});
	});

	// Update local store
	myRecords.update(map => {
		const newMap = new Map(map);
		newMap.set(record.id, record);
		return newMap;
	});

	// Update versioned store
	recordStore.update(record.id, record);

	console.log(`[RECORDS] ✅ Record issued: ${record.id}`);
}

/**
 * Update an existing record's status (e.g., pending -> adopted)
 */
export async function updateRecordStatus(
	recordId: UUID,
	newStatus: 'pending' | 'adopted' | 'rejected',
	decisionTimestamp?: string
): Promise<void> {
	if (!isAuthenticated()) {
		throw new Error('Cannot update record - not authenticated');
	}

	const currentRecords = get(myRecords);
	const record = currentRecords.get(recordId);
	
	if (!record) {
		throw new Error(`Record ${recordId} not found`);
	}

	const updatedRecord = {
		...record,
		status: newStatus,
		decision_timestamp: decisionTimestamp || new Date().toISOString()
	};

	// Validate updated record
	validateRecord(updatedRecord);

	// Store updated record
	const recordRef = holster.user().get('coalition').get('records').get(recordId);
	await new Promise<void>((resolve, reject) => {
		recordRef.put(updatedRecord, (ack: any) => {
			if (ack.err) {
				reject(new Error(ack.err));
			} else {
				resolve();
			}
		});
	});

	// Update local store
	myRecords.update(map => {
		const newMap = new Map(map);
		newMap.set(recordId, updatedRecord);
		return newMap;
	});

	// Update versioned store
	recordStore.update(recordId, updatedRecord);

	console.log(`[RECORDS] ✅ Record status updated: ${recordId} -> ${newStatus}`);
}

// ═══════════════════════════════════════════════════════════════════
// DERIVED STORES & QUERIES
// ═══════════════════════════════════════════════════════════════════

/**
 * Get all records for a specific participant (my records or network)
 */
export function getRecordsForParticipant(pubKey: string): Readable<CoalitionRecord[]> {
	return derived(
		[myRecords, networkRecords],
		([$myRecords, $networkRecords]) => {
			const authState = holsterUser.is;
			if (authState?.pub === pubKey) {
				// Return my records
				return Array.from($myRecords.values());
			} else {
				// Return network records
				const participantRecords = $networkRecords.get(pubKey);
				return participantRecords ? Array.from(participantRecords.values()) : [];
			}
		}
	);
}

/**
 * Get records by type
 */
export function getRecordsByType(pubKey: string, recordType: CoalitionRecord['type']): Readable<CoalitionRecord[]> {
	return derived(
		getRecordsForParticipant(pubKey),
		($records) => $records.filter(r => r.type === recordType)
	);
}

/**
 * Get records by status
 */
export function getRecordsByStatus(
	pubKey: string,
	status: 'pending' | 'adopted' | 'rejected'
): Readable<CoalitionRecord[]> {
	return derived(
		getRecordsForParticipant(pubKey),
		($records) => $records.filter(r => r.status === status)
	);
}

/**
 * Get all records across all participants (for aggregation)
 */
export const allRecords = derived(
	[myRecords, networkRecords],
	([$myRecords, $networkRecords]) => {
		const all: CoalitionRecord[] = [];
		
		// Add my records
		all.push(...Array.from($myRecords.values()));
		
		// Add network records
		for (const participantRecords of $networkRecords.values()) {
			all.push(...Array.from(participantRecords.values()));
		}
		
		return all;
	}
);

/**
 * Get record statistics
 */
export const recordStats = derived(
	[myRecords, networkRecords],
	([$myRecords, $networkRecords]) => {
		const stats = {
			myRecordsCount: $myRecords.size,
			networkParticipantsCount: $networkRecords.size,
			totalNetworkRecords: 0,
			byType: {} as Record<string, number>,
			byStatus: {
				pending: 0,
				adopted: 0,
				rejected: 0
			}
		};

		// Count network records
		for (const participantRecords of $networkRecords.values()) {
			stats.totalNetworkRecords += participantRecords.size;
		}

		// Aggregate by type and status
		const allRecordsArray = Array.from($myRecords.values());
		for (const participantRecords of $networkRecords.values()) {
			allRecordsArray.push(...Array.from(participantRecords.values()));
		}

		for (const record of allRecordsArray) {
			// Count by type
			stats.byType[record.type] = (stats.byType[record.type] || 0) + 1;
			
			// Count by status
			stats.byStatus[record.status]++;
		}

		return stats;
	}
);

// ═══════════════════════════════════════════════════════════════════
// CLEANUP
// ═══════════════════════════════════════════════════════════════════

export function cleanupRecords(): void {
	// Unsubscribe from all network participants
	for (const unsubscribe of networkUnsubscribes.values()) {
		unsubscribe();
	}
	networkUnsubscribes.clear();

	// Clear stores
	myRecords.set(new Map());
	networkRecords.set(new Map());
	subscribedRecordParticipants.set(new Set());
	recordLoadingStates.set(new Map());

	console.log('[RECORDS] 🧹 Cleaned up record stores');
}

