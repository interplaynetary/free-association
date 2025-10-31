/**
 * CLI Data Storage Layer
 * 
 * Persists capacities, needs, and commitments to local filesystem
 * for CLI usage (separate from P2P Holster storage)
 */

import fs from 'fs';
import path from 'path';
import os from 'os';
import type { Commitment, AvailabilitySlot, NeedSlot } from '../../schemas';

const CLI_DATA_DIR = path.join(os.homedir(), '.playnet');
const COMMITMENTS_FILE = path.join(CLI_DATA_DIR, 'commitments.json');
const CONFIG_FILE = path.join(CLI_DATA_DIR, 'config.json');

// Ensure data directory exists
export function ensureDataDir(): void {
	if (!fs.existsSync(CLI_DATA_DIR)) {
		fs.mkdirSync(CLI_DATA_DIR, { recursive: true });
	}
}

// ════════════════════════════════════════════════════════════════
// COMMITMENT STORAGE
// ════════════════════════════════════════════════════════════════

export interface StoredCommitments {
	[pubKey: string]: Commitment;
}

export function loadCommitments(): StoredCommitments {
	ensureDataDir();
	
	if (!fs.existsSync(COMMITMENTS_FILE)) {
		return {};
	}
	
	try {
		const data = fs.readFileSync(COMMITMENTS_FILE, 'utf-8');
		return JSON.parse(data);
	} catch (error) {
		console.error('Error loading commitments:', error);
		return {};
	}
}

export function saveCommitments(commitments: StoredCommitments): void {
	ensureDataDir();
	
	try {
		fs.writeFileSync(COMMITMENTS_FILE, JSON.stringify(commitments, null, 2));
	} catch (error) {
		console.error('Error saving commitments:', error);
		throw error;
	}
}

export function getMyCommitment(myPubKey: string): Commitment | null {
	const commitments = loadCommitments();
	return commitments[myPubKey] || null;
}

export function updateMyCommitment(myPubKey: string, commitment: Commitment): void {
	const commitments = loadCommitments();
	commitments[myPubKey] = commitment;
	saveCommitments(commitments);
}

// ════════════════════════════════════════════════════════════════
// CAPACITY OPERATIONS
// ════════════════════════════════════════════════════════════════

export function listMyCapacities(myPubKey: string): AvailabilitySlot[] {
	const commitment = getMyCommitment(myPubKey);
	return commitment?.capacity_slots || [];
}

export function addCapacity(myPubKey: string, capacity: AvailabilitySlot): void {
	let commitment = getMyCommitment(myPubKey);
	
	if (!commitment) {
		commitment = {
			capacity_slots: [],
			need_slots: [],
			global_recognition_weights: {},
			itcStamp: { id: 1, event: 0 },
			timestamp: Date.now()
		};
	}
	
	if (!commitment.capacity_slots) {
		commitment.capacity_slots = [];
	}
	
	commitment.capacity_slots.push(capacity);
	commitment.timestamp = Date.now();
	
	updateMyCommitment(myPubKey, commitment);
}

export function getCapacity(myPubKey: string, capacityId: string): AvailabilitySlot | null {
	const capacities = listMyCapacities(myPubKey);
	return capacities.find(c => c.id === capacityId) || null;
}

export function updateCapacity(myPubKey: string, capacityId: string, updates: Partial<AvailabilitySlot>): void {
	const commitment = getMyCommitment(myPubKey);
	if (!commitment || !commitment.capacity_slots) return;
	
	const index = commitment.capacity_slots.findIndex(c => c.id === capacityId);
	if (index === -1) return;
	
	commitment.capacity_slots[index] = {
		...commitment.capacity_slots[index],
		...updates
	};
	commitment.timestamp = Date.now();
	
	updateMyCommitment(myPubKey, commitment);
}

export function removeCapacity(myPubKey: string, capacityId: string): void {
	const commitment = getMyCommitment(myPubKey);
	if (!commitment || !commitment.capacity_slots) return;
	
	commitment.capacity_slots = commitment.capacity_slots.filter(c => c.id !== capacityId);
	commitment.timestamp = Date.now();
	
	updateMyCommitment(myPubKey, commitment);
}

// ════════════════════════════════════════════════════════════════
// NEED OPERATIONS
// ════════════════════════════════════════════════════════════════

export function listMyNeeds(myPubKey: string): NeedSlot[] {
	const commitment = getMyCommitment(myPubKey);
	return commitment?.need_slots || [];
}

export function addNeed(myPubKey: string, need: NeedSlot): void {
	let commitment = getMyCommitment(myPubKey);
	
	if (!commitment) {
		commitment = {
			capacity_slots: [],
			need_slots: [],
			global_recognition_weights: {},
			itcStamp: { id: 1, event: 0 },
			timestamp: Date.now()
		};
	}
	
	if (!commitment.need_slots) {
		commitment.need_slots = [];
	}
	
	commitment.need_slots.push(need);
	commitment.timestamp = Date.now();
	
	updateMyCommitment(myPubKey, commitment);
}

export function getNeed(myPubKey: string, needId: string): NeedSlot | null {
	const needs = listMyNeeds(myPubKey);
	return needs.find(n => n.id === needId) || null;
}

export function updateNeed(myPubKey: string, needId: string, updates: Partial<NeedSlot>): void {
	const commitment = getMyCommitment(myPubKey);
	if (!commitment || !commitment.need_slots) return;
	
	const index = commitment.need_slots.findIndex(n => n.id === needId);
	if (index === -1) return;
	
	commitment.need_slots[index] = {
		...commitment.need_slots[index],
		...updates
	};
	commitment.timestamp = Date.now();
	
	updateMyCommitment(myPubKey, commitment);
}

export function removeNeed(myPubKey: string, needId: string): void {
	const commitment = getMyCommitment(myPubKey);
	if (!commitment || !commitment.need_slots) return;
	
	commitment.need_slots = commitment.need_slots.filter(n => n.id !== needId);
	commitment.timestamp = Date.now();
	
	updateMyCommitment(myPubKey, commitment);
}

// ════════════════════════════════════════════════════════════════
// CONFIG OPERATIONS
// ════════════════════════════════════════════════════════════════

export interface CLIConfig {
	myPubKey: string;
	myName: string;
}

export function loadConfig(): CLIConfig | null {
	ensureDataDir();
	
	if (!fs.existsSync(CONFIG_FILE)) {
		return null;
	}
	
	try {
		const data = fs.readFileSync(CONFIG_FILE, 'utf-8');
		return JSON.parse(data);
	} catch (error) {
		console.error('Error loading config:', error);
		return null;
	}
}

export function saveConfig(config: CLIConfig): void {
	ensureDataDir();
	
	try {
		fs.writeFileSync(CONFIG_FILE, JSON.stringify(config, null, 2));
	} catch (error) {
		console.error('Error saving config:', error);
		throw error;
	}
}

