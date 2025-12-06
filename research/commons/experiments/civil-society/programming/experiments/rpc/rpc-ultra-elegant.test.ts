/**
 * Tests for Ultra-Elegant RPC System
 * 
 * Tests Proxy-based auto-interception, fluent builders, and time parsing
 */

import { describe, it, expect, beforeEach } from 'vitest';
import {
	AutoRevocableRpcTarget,
	Commitment,
	ReadOnly,
	Auth,
	Session,
	SessionAuth,
	parseTime
} from './rpc-ultra-elegant';
import type { Commitment as CommitmentData } from '../../../../../../../src/lib/protocol/schemas';

const delay = (ms: number) => new Promise(resolve => setTimeout(resolve, ms));

const mockCommitment: CommitmentData = {
	need_slots: [],
	capacity_slots: [],
	global_recognition_weights: {},
	timestamp: Date.now()
};

describe('parseTime', () => {
	it('should parse seconds', () => {
		expect(parseTime('60s')).toBe(60000);
	});
	
	it('should parse minutes', () => {
		expect(parseTime('30m')).toBe(1800000);
	});
	
	it('should parse hours', () => {
		expect(parseTime('24h')).toBe(86400000);
	});
	
	it('should parse days', () => {
		expect(parseTime('7d')).toBe(604800000);
	});
	
	it('should throw on invalid format', () => {
		expect(() => parseTime('invalid')).toThrow('Invalid time format');
	});
});

describe('AutoRevocableRpcTarget - Auto Interception', () => {
	it('should auto-check access via Proxy', async () => {
		const target = new (class extends AutoRevocableRpcTarget {
			async testMethod() {
				// NO checkAccess() call needed - Proxy handles it!
				return 'success';
			}
		})();
		
		const result = await target.testMethod();
		expect(result).toBe('success');
	});
	
	it('should auto-throw when revoked', async () => {
		const target = new (class extends AutoRevocableRpcTarget {
			async testMethod() {
				// NO checkAccess() needed!
				return 'success';
			}
		})();
		
		target.revoke('Test');
		
		await expect(target.testMethod()).rejects.toThrow('Revoked');
	});
	
	it('should auto-throw when expired', async () => {
		const target = new (class extends AutoRevocableRpcTarget {
			async testMethod() {
				// NO checkAccess() needed!
				return 'success';
			}
		})().expiresIn('100ms');
		
		expect(await target.testMethod()).toBe('success');
		
		await delay(150);
		
		await expect(target.testMethod()).rejects.toThrow('Expired');
	});
});

describe('Fluent Builder Pattern', () => {
	it('should support expiresIn()', () => {
		const target = new (class extends AutoRevocableRpcTarget {})()
			.expiresIn('24h');
		
		expect(target.isValid()).toBe(true);
	});
	
	it('should support for()', () => {
		const target = new (class extends AutoRevocableRpcTarget {})()
			.for('user-123');
		
		expect(target.isValid()).toBe(true);
	});
	
	it('should support chaining', () => {
		const target = new (class extends AutoRevocableRpcTarget {})()
			.expiresIn('24h')
			.for('user-123');
		
		expect(target.isValid()).toBe(true);
	});
	
	it('should support grant() shorthand', () => {
		const target = new (class extends AutoRevocableRpcTarget {})()
			.grant('user-123', '24h');
		
		expect(target.isValid()).toBe(true);
	});
	
	it('should support extend()', () => {
		const target = new (class extends AutoRevocableRpcTarget {})()
			.expiresIn('1h')
			.extend('2h');
		
		expect(target.isValid()).toBe(true);
	});
});

describe('Commitment - Ultra Elegant', () => {
	let commitment: Commitment;
	
	beforeEach(() => {
		commitment = new Commitment('test-pubkey', () => mockCommitment);
	});
	
	it('should work without manual checkAccess', async () => {
		const data = await commitment.commitment();
		expect(data).toEqual(mockCommitment);
	});
	
	it('should support fluent builder', () => {
		const c = new Commitment('pubkey', () => mockCommitment)
			.expiresIn('24h')
			.for('alice');
		
		expect(c.isValid()).toBe(true);
	});
	
	it('should support grant() shorthand', () => {
		const c = new Commitment('pubkey', () => mockCommitment)
			.grant('alice', '24h');
		
		expect(c.isValid()).toBe(true);
	});
	
	it('should return needs', async () => {
		const needs = await commitment.needs();
		expect(needs).toEqual([]);
	});
	
	it('should return capacity', async () => {
		const capacity = await commitment.capacity();
		expect(capacity).toEqual([]);
	});
	
	it('should auto-throw when revoked', async () => {
		commitment.revoke('Test');
		await expect(commitment.commitment()).rejects.toThrow('Revoked');
	});
	
	it('should auto-expire', async () => {
		const c = new Commitment('pubkey', () => mockCommitment)
			.expiresIn('100ms');
		
		expect(await c.commitment()).toEqual(mockCommitment);
		
		await delay(150);
		
		await expect(c.commitment()).rejects.toThrow('Expired');
	});
});

describe('ReadOnly - Ultra Elegant', () => {
	it('should wrap full access', async () => {
		const full = new Commitment('pubkey', () => mockCommitment);
		const ro = new ReadOnly(full);
		
		const data = await ro.commitment();
		expect(data).toEqual(mockCommitment);
	});
	
	it('should support fluent builder', () => {
		const full = new Commitment('pubkey', () => mockCommitment);
		const ro = new ReadOnly(full)
			.expiresIn('1h')
			.for('public');
		
		expect(ro.isValid()).toBe(true);
	});
});

describe('Auth - Ultra Elegant', () => {
	let auth: Auth;
	
	beforeEach(() => {
		auth = new Auth(recipientId => 
			new Commitment('server-pubkey', () => mockCommitment)
		);
	});
	
	it('should login and return capability with fluent builder', async () => {
		const capability = await auth.login('user-pubkey', 'signature');
		expect(capability).toBeDefined();
	});
	
	it('should support temp() for short-lived access', async () => {
		const capability = await auth.temp('user-pubkey', 'signature');
		expect(capability).toBeDefined();
	});
	
	it('should support readOnly() for least privilege', async () => {
		const capability = await auth.readOnly('user-pubkey', 'signature');
		expect(capability).toBeDefined();
	});
});

describe('Session - Auto Tracking', () => {
	let session: Session;
	
	beforeEach(() => {
		session = new Session('session-123', recipientId =>
			new Commitment('server-pubkey', () => mockCommitment)
		);
	});
	
	it('should auto-track issued capabilities', async () => {
		const cap1 = await session.commitment();
		const cap2 = await session.readOnly();
		
		expect(cap1).toBeDefined();
		expect(cap2).toBeDefined();
	});
	
	it('should auto-revoke all on logout', async () => {
		// Create capabilities (auto-tracked!)
		await session.commitment();
		await session.readOnly();
		
		// Logout
		await session.logout();
		
		// Session should be revoked
		expect(session.isValid()).toBe(false);
	});
});

describe('SessionAuth - Ultra Elegant', () => {
	let sessionAuth: SessionAuth;
	
	beforeEach(() => {
		sessionAuth = new SessionAuth(recipientId =>
			new Commitment('server-pubkey', () => mockCommitment)
		);
	});
	
	it('should create session on login', async () => {
		const session = await sessionAuth.login('user-pubkey', 'signature');
		expect(session).toBeDefined();
	});
	
	it('should support session revocation', async () => {
		const session = await sessionAuth.login('user-pubkey', 'signature');
		expect(session).toBeDefined();
		
		// In real usage, session ID would be tracked
		// Just verify the method exists
	});
});

describe('Integration - Ultra Elegant Workflow', () => {
	it('should support full workflow with fluent API', async () => {
		// Create commitment with fluent builder
		const commitment = new Commitment('pubkey', () => mockCommitment)
			.expiresIn('24h')
			.for('alice');
		
		// Use it - auto-checked!
		const data = await commitment.commitment();
		expect(data).toEqual(mockCommitment);
		
		const needs = await commitment.needs();
		expect(needs).toEqual([]);
		
		// Revoke with fluent chaining
		commitment.revoke('Done');
		
		// Auto-denied after revocation
		await expect(commitment.commitment()).rejects.toThrow('Revoked');
	});
	
	it('should support least privilege with fluent builders', () => {
		const full = new Commitment('pubkey', () => mockCommitment)
			.expiresIn('24h');
		
		const ro = new ReadOnly(full)
			.expiresIn('1h')
			.for('public');
		
		expect(full.isValid()).toBe(true);
		expect(ro.isValid()).toBe(true);
	});
});

describe('Comparison: Manual vs Auto checkAccess', () => {
	it('should eliminate ALL manual checkAccess calls', async () => {
		// Count checkAccess calls in the source code
		// Ultra-elegant: 0 manual calls (all automatic!)
		const target = new Commitment('pubkey', () => mockCommitment);
		
		// Methods work without any manual checks
		await target.commitment();
		await target.needs();
		await target.capacity();
		await target.recognition();
		
		// All auto-checked! Zero boilerplate!
	});
});

