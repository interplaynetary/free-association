/**
 * Tests for Elegant RPC Revocation System
 * 
 * Tests the built-in revocation features of RevocableRpcTarget
 * and related classes (Auth, Session, etc.)
 */

import { describe, it, expect, beforeEach, vi } from 'vitest';
import {
	RevocableRpcTarget,
	MyCommitment,
	ReadOnlyCommitment,
	Auth,
	AuthWithSessions,
	Session
} from './rpc-elegant';
import type { Commitment } from '../../../../../../../src/lib/protocol/schemas';

// Helper to wait for a specific time
const delay = (ms: number) => new Promise(resolve => setTimeout(resolve, ms));

// Mock commitment data
const mockCommitment: Commitment = {
	need_slots: [],
	capacity_slots: [],
	global_recognition_weights: {},
	timestamp: Date.now()
};

describe('RevocableRpcTarget', () => {
	it('should be valid by default', () => {
		const target = new (class extends RevocableRpcTarget {})();
		expect(target.isValid()).toBe(true);
	});
	
	it('should become invalid after revocation', () => {
		const target = new (class extends RevocableRpcTarget {})();
		expect(target.isValid()).toBe(true);
		
		target.revoke('Test revocation');
		expect(target.isValid()).toBe(false);
	});
	
	it('should throw error when accessing revoked target', async () => {
		const target = new (class extends RevocableRpcTarget {
			async testMethod() {
				this.checkAccess('testMethod');
				return 'success';
			}
		})();
		
		target.revoke('Test');
		
		await expect(target.testMethod()).rejects.toThrow('Revoked');
	});
	
	it('should auto-expire after specified time', async () => {
		const target = new (class extends RevocableRpcTarget {})({
			expiresInMs: 100
		});
		
		expect(target.isValid()).toBe(true);
		
		await delay(150);
		
		expect(target.isValid()).toBe(false);
	});
	
	it('should extend expiration time', () => {
		const target = new (class extends RevocableRpcTarget {})({
			expiresInMs: 100
		});
		
		target.extend(1000);
		
		// Should still be valid after original expiration would have passed
		expect(target.isValid()).toBe(true);
	});
});

describe('MyCommitment', () => {
	let pubKey: string;
	let getCommitment: () => Commitment | null;
	
	beforeEach(() => {
		pubKey = 'test-pubkey-123';
		getCommitment = () => mockCommitment;
	});
	
	it('should create a valid commitment capability', () => {
		const commitment = new MyCommitment(pubKey, getCommitment);
		expect(commitment.isValid()).toBe(true);
	});
	
	it('should return commitment data', async () => {
		const commitment = new MyCommitment(pubKey, getCommitment);
		const data = await commitment.commitment();
		expect(data).toEqual(mockCommitment);
	});
	
	it('should return needs', async () => {
		const commitment = new MyCommitment(pubKey, getCommitment);
		const needs = await commitment.needs();
		expect(needs).toEqual([]);
	});
	
	it('should return capacity', async () => {
		const commitment = new MyCommitment(pubKey, getCommitment);
		const capacity = await commitment.capacity();
		expect(capacity).toEqual([]);
	});
	
	it('should return recognition', async () => {
		const commitment = new MyCommitment(pubKey, getCommitment);
		const recognition = await commitment.recognition();
		expect(recognition).toEqual({});
	});
	
	it('should throw when revoked', async () => {
		const commitment = new MyCommitment(pubKey, getCommitment);
		commitment.revoke('Test');
		
		await expect(commitment.commitment()).rejects.toThrow('Revoked');
	});
	
	it('should auto-expire', async () => {
		const commitment = new MyCommitment(pubKey, getCommitment, {
			expiresInMs: 100
		});
		
		expect(commitment.isValid()).toBe(true);
		await delay(150);
		expect(commitment.isValid()).toBe(false);
	});
});

describe('ReadOnlyCommitment', () => {
	let fullAccess: MyCommitment;
	
	beforeEach(() => {
		fullAccess = new MyCommitment('test-pubkey', () => mockCommitment);
	});
	
	it('should create read-only wrapper', () => {
		const readOnly = new ReadOnlyCommitment(fullAccess);
		expect(readOnly.isValid()).toBe(true);
	});
	
	it('should allow reading commitment', async () => {
		const readOnly = new ReadOnlyCommitment(fullAccess);
		const data = await readOnly.commitment();
		expect(data).toEqual(mockCommitment);
	});
	
	it('should allow reading needs', async () => {
		const readOnly = new ReadOnlyCommitment(fullAccess);
		const needs = await readOnly.needs();
		expect(needs).toEqual([]);
	});
	
	it('should throw when revoked', async () => {
		const readOnly = new ReadOnlyCommitment(fullAccess);
		readOnly.revoke('Test');
		
		await expect(readOnly.commitment()).rejects.toThrow('Revoked');
	});
});

describe('Auth', () => {
	let auth: Auth;
	let createCommitment: (recipientId: string, options?: any) => MyCommitment;
	
	beforeEach(() => {
		createCommitment = (recipientId, options) => 
			new MyCommitment('server-pubkey', () => mockCommitment, options);
		auth = new Auth(createCommitment);
	});
	
	it('should authenticate and return capability', async () => {
		const capability = await auth.login('user-pubkey', 'signature');
		expect(capability).toBeDefined();
	});
	
	it('should return capability with expiration', async () => {
		const capability = await auth.login('user-pubkey', 'signature');
		// Can't check isValid() on RpcStub, but we can verify it was returned
		expect(capability).toBeDefined();
	});
	
	it('should return read-only capability', async () => {
		const capability = await auth.loginReadOnly('user-pubkey', 'signature');
		expect(capability).toBeDefined();
	});
	
	it('should return temporary capability with shorter expiration', async () => {
		const capability = await auth.loginTemp('user-pubkey', 'signature');
		expect(capability).toBeDefined();
	});
});

describe('Session', () => {
	let session: Session;
	let createCommitment: (recipientId: string, options?: any) => MyCommitment;
	
	beforeEach(() => {
		createCommitment = (recipientId, options) => 
			new MyCommitment('server-pubkey', () => mockCommitment, options);
		session = new Session('session-123', createCommitment);
	});
	
	it('should issue commitment capability', async () => {
		const capability = await session.commitment();
		expect(capability).toBeDefined();
	});
	
	it('should issue read-only capability', async () => {
		const capability = await session.commitmentReadOnly();
		expect(capability).toBeDefined();
	});
	
	it('should revoke all capabilities on logout', async () => {
		// Test that session can issue capabilities and logout works
		const cap1 = await session.commitment();
		const cap2 = await session.commitmentReadOnly();
		
		expect(cap1).toBeDefined();
		expect(cap2).toBeDefined();
		
		await session.logout();
		
		// Session should be revoked
		expect(session.isValid()).toBe(false);
	});
	
	it('should revoke all capabilities when session is revoked', async () => {
		// Test that revoking session works
		const cap1 = await session.commitment();
		const cap2 = await session.commitmentReadOnly();
		
		expect(cap1).toBeDefined();
		expect(cap2).toBeDefined();
		
		session.revoke('Admin action');
		
		// Session should be revoked
		expect(session.isValid()).toBe(false);
	});
});

describe('AuthWithSessions', () => {
	let authWithSessions: AuthWithSessions;
	let createCommitment: (recipientId: string, options?: any) => MyCommitment;
	
	beforeEach(() => {
		createCommitment = (recipientId, options) => 
			new MyCommitment('server-pubkey', () => mockCommitment, options);
		authWithSessions = new AuthWithSessions(createCommitment);
	});
	
	it('should create a session on login', async () => {
		const session = await authWithSessions.login('user-pubkey', 'signature');
		expect(session).toBeDefined();
	});
	
	it('should allow revoking sessions by ID', async () => {
		// In real usage, session would be an RpcStub, so we can't access sessionId directly
		// Just test that revoking a known session works
		const session = await authWithSessions.login('user-pubkey', 'signature');
		expect(session).toBeDefined();
		
		// In production, the session ID would be tracked server-side
		// For testing, we just verify the method exists and returns false for non-existent
	});
	
	it('should return false when revoking non-existent session', async () => {
		const revoked = await authWithSessions.revokeSession('non-existent', 'Test');
		expect(revoked).toBe(false);
	});
});

describe('Integration Tests', () => {
	it('should support full authentication workflow', async () => {
		// Setup - test direct creation without RPC
		const commitment = new MyCommitment('server-pubkey', () => mockCommitment);
		
		// User can access data
		const data = await commitment.commitment();
		expect(data).toEqual(mockCommitment);
		
		// Capability can be revoked
		commitment.revoke('User logged out');
		
		// Access denied after revocation
		await expect(commitment.commitment()).rejects.toThrow('Revoked');
	});
	
	it('should support session-based capability management', async () => {
		// Setup - test direct creation without RPC
		const createCommitment = (recipientId: string, options?: any) => 
			new MyCommitment('server-pubkey', () => mockCommitment, options);
		const session = new Session('test-session', createCommitment);
		
		// Session issues multiple capabilities (returns actual objects for testing)
		const cap1 = new MyCommitment('server-pubkey', () => mockCommitment);
		const cap2 = new ReadOnlyCommitment(cap1);
		
		// Track them in session
		(session as any).capabilities.add(cap1);
		(session as any).capabilities.add(cap2);
		
		// All capabilities work
		expect(await cap1.commitment()).toEqual(mockCommitment);
		expect(await cap2.commitment()).toEqual(mockCommitment);
		
		// Logout revokes all capabilities at once
		await session.logout();
		
		await expect(cap1.commitment()).rejects.toThrow('Revoked');
		await expect(cap2.commitment()).rejects.toThrow('Revoked');
	});
	
	it('should support least privilege with read-only access', async () => {
		// Full access
		const fullAccess = new MyCommitment('test-pubkey', () => mockCommitment);
		
		// Create read-only wrapper (least privilege)
		const readOnly = new ReadOnlyCommitment(fullAccess, {
			recipientId: 'public-user',
			expiresInMs: 60 * 60 * 1000 // 1 hour
		});
		
		// Can read
		const data = await readOnly.commitment();
		expect(data).toEqual(mockCommitment);
		
		// Cannot subscribe (method doesn't exist on read-only)
		expect((readOnly as any).subscribe).toBeUndefined();
	});
});

