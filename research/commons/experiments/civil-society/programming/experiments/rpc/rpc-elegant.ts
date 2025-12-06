/**
 * Elegant RPC with Built-in Revocation
 * 
 * Aligned with Cap'n Web philosophy:
 * ✅ Absolutely trivial to set up
 * ✅ Almost no boilerplate
 * ✅ Natural JavaScript patterns
 * ✅ Revocation built-in (not bolted-on)
 * 
 * Instead of separate managers and wrappers, capabilities ARE revocable by default.
 */

import { RpcTarget, type RpcStub } from 'capnweb';
import type {
	Commitment,
	RootNode,
	NeedSlot,
	AvailabilitySlot,
	GlobalRecognitionWeights,
	SlotAllocationRecord
} from '../../../../../../../src/lib/protocol/schemas';

// ═══════════════════════════════════════════════════════════════════
// REVOCABLE RPC TARGET (Base Class)
// ═══════════════════════════════════════════════════════════════════

/**
 * RevocableRpcTarget - All RPC targets extend this
 * 
 * Revocation is built-in, not an add-on!
 * Each instance can be revoked individually.
 */
abstract class RevocableRpcTarget extends RpcTarget {
	private _revoked = false;
	private _revokeReason?: string;
	private _expiresAt?: number;
	protected _recipientId?: string;
	
	constructor(options?: {
		expiresInMs?: number;
		recipientId?: string;
	}) {
		super();
		
		if (options?.expiresInMs) {
			this._expiresAt = Date.now() + options.expiresInMs;
			setTimeout(() => this.revoke('Expired'), options.expiresInMs);
		}
		
		this._recipientId = options?.recipientId;
	}
	
	/**
	 * Check if still valid before any operation
	 * Override this to add custom checks
	 */
	protected checkAccess(methodName?: string): void {
		if (this._revoked) {
			throw new Error(`Revoked: ${this._revokeReason || 'Access denied'}`);
		}
		
		if (this._expiresAt && Date.now() > this._expiresAt) {
			this.revoke('Expired');
			throw new Error(`Expired at ${new Date(this._expiresAt).toISOString()}`);
		}
	}
	
	/**
	 * Revoke this capability
	 */
	revoke(reason?: string): void {
		if (this._revoked) return;
		this._revoked = true;
		this._revokeReason = reason;
		console.log(`[REVOKED] ${this.constructor.name} - ${reason || 'No reason'}`);
	}
	
	/**
	 * Check if valid
	 */
	isValid(): boolean {
		return !this._revoked && (!this._expiresAt || Date.now() <= this._expiresAt);
	}
	
	/**
	 * Extend expiration
	 */
	extend(ms: number): void {
		if (!this._expiresAt) {
			this._expiresAt = Date.now() + ms;
		} else {
			this._expiresAt += ms;
		}
	}
}

// ═══════════════════════════════════════════════════════════════════
// COMMITMENT RPC (Elegant)
// ═══════════════════════════════════════════════════════════════════

/**
 * MyCommitment - Expose your commitment via RPC
 * 
 * Simple, natural, revocable by default.
 */
class MyCommitment extends RevocableRpcTarget {
	private pubKey: string;
	private getCommitment: () => Commitment | null;
	
	constructor(
		pubKey: string,
		getCommitment: () => Commitment | null,
		options?: {
			expiresInMs?: number;
			recipientId?: string;
		}
	) {
		super(options);
		this.pubKey = pubKey;
		this.getCommitment = getCommitment;
	}
	
	// Natural method calls - just like regular JavaScript!
	async commitment(): Promise<Commitment> {
		this.checkAccess('commitment');
		const c = this.getCommitment();
		if (!c) throw new Error('No commitment');
		return c;
	}
	
	async needs(): Promise<NeedSlot[]> {
		this.checkAccess('needs');
		return (await this.commitment()).need_slots || [];
	}
	
	async capacity(): Promise<AvailabilitySlot[]> {
		this.checkAccess('capacity');
		return (await this.commitment()).capacity_slots || [];
	}
	
	async recognition(): Promise<GlobalRecognitionWeights> {
		this.checkAccess('recognition');
		return (await this.commitment()).global_recognition_weights || {};
	}
	
	async allocations(): Promise<SlotAllocationRecord[]> {
		this.checkAccess('allocations');
		return (await this.commitment()).slot_allocations || [];
	}
	
	/**
	 * Compute mutual recognition with another commitment
	 * Demonstrates object capability passing!
	 */
	async mutualWith(other: RpcStub<MyCommitment>): Promise<number> {
		this.checkAccess('mutualWith');
		
		const myRec = await this.recognition();
		const theirPub = await other.getPubKey();
		const myRecOfThem = myRec[theirPub] || 0;
		
		const theirRec = await other.recognition();
		const theirRecOfMe = theirRec[this.pubKey] || 0;
		
		return Math.min(myRecOfThem, theirRecOfMe);
	}
	
	/**
	 * Subscribe to updates (bidirectional!)
	 */
	async subscribe(callback: (c: Commitment) => void): Promise<void> {
		this.checkAccess('subscribe');
		// Implementation would connect to store
		// For now, just validate
		if (typeof callback !== 'function') {
			throw new Error('Callback must be a function');
		}
	}
	
	async getPubKey(): Promise<string> {
		return this.pubKey;
	}
}

/**
 * ReadOnlyCommitment - Least privilege version
 * 
 * Only exposes read methods.
 */
class ReadOnlyCommitment extends RevocableRpcTarget {
	private innerCommitment: MyCommitment;
	
	constructor(
		commitment: MyCommitment,
		options?: {
			expiresInMs?: number;
			recipientId?: string;
		}
	) {
		super(options);
		this.innerCommitment = commitment;
	}
	
	async commitment(): Promise<Commitment> {
		this.checkAccess();
		return this.innerCommitment.commitment();
	}
	
	async needs(): Promise<NeedSlot[]> {
		this.checkAccess();
		return this.innerCommitment.needs();
	}
	
	async capacity(): Promise<AvailabilitySlot[]> {
		this.checkAccess();
		return this.innerCommitment.capacity();
	}
	
	async getPubKey(): Promise<string> {
		return this.innerCommitment.getPubKey();
	}
	
	// No subscribe, no mutualWith - read-only!
}

// ═══════════════════════════════════════════════════════════════════
// AUTHENTICATION (Natural Pattern)
// ═══════════════════════════════════════════════════════════════════

/**
 * Auth - The capability you get on initial connection
 * 
 * Authenticate to receive other capabilities.
 * This is Cap'n Web's natural authentication pattern.
 */
class Auth extends RpcTarget {
	constructor(
		private createCommitment: (recipientId: string, options?: any) => MyCommitment
	) {
		super();
	}
	
	/**
	 * Authenticate and receive a capability
	 * 
	 * This IS the capability - no wrapper needed!
	 */
	async login(pubKey: string, signature: string): Promise<RpcStub<MyCommitment>> {
		// Verify signature
		if (!this.verify(pubKey, signature)) {
			throw new Error('Invalid signature');
		}
		
		// Return a NEW capability (auto-expires in 24h)
		return this.createCommitment(pubKey, {
			recipientId: pubKey,
			expiresInMs: 24 * 60 * 60 * 1000
		}) as any;
	}
	
	/**
	 * Login with read-only access
	 */
	async loginReadOnly(pubKey: string, signature: string): Promise<RpcStub<ReadOnlyCommitment>> {
		if (!this.verify(pubKey, signature)) {
			throw new Error('Invalid signature');
		}
		
		const fullAccess = this.createCommitment(pubKey, {
			recipientId: pubKey,
			expiresInMs: 24 * 60 * 60 * 1000
		});
		
		return new ReadOnlyCommitment(fullAccess, {
			recipientId: pubKey,
			expiresInMs: 24 * 60 * 60 * 1000
		}) as any;
	}
	
	/**
	 * Temporary access (1 hour)
	 */
	async loginTemp(pubKey: string, signature: string): Promise<RpcStub<MyCommitment>> {
		if (!this.verify(pubKey, signature)) {
			throw new Error('Invalid signature');
		}
		
		return this.createCommitment(pubKey, {
			recipientId: pubKey,
			expiresInMs: 60 * 60 * 1000 // 1 hour
		}) as any;
	}
	
	private verify(pubKey: string, signature: string): boolean {
		// TODO: Implement signature verification
		return true;
	}
}

// ═══════════════════════════════════════════════════════════════════
// SESSION (Natural Revocation)
// ═══════════════════════════════════════════════════════════════════

/**
 * Session - Represents an authenticated session
 * 
 * When revoked, all capabilities issued in this session become invalid.
 * This is the natural way to do session management with capabilities!
 */
class Session extends RevocableRpcTarget {
	private capabilities = new Set<RevocableRpcTarget>();
	
	constructor(
		public sessionId: string,
		private createCommitment: (recipientId: string, options?: any) => MyCommitment
	) {
		super();
	}
	
	/**
	 * Issue a commitment capability within this session
	 */
	async commitment(): Promise<RpcStub<MyCommitment>> {
		this.checkAccess();
		
		const cap = this.createCommitment(this.sessionId, {
			recipientId: this.sessionId,
			expiresInMs: 24 * 60 * 60 * 1000
		});
		
		this.capabilities.add(cap);
		return cap as any;
	}
	
	/**
	 * Issue read-only commitment capability
	 */
	async commitmentReadOnly(): Promise<RpcStub<ReadOnlyCommitment>> {
		this.checkAccess();
		
		const fullAccess = this.createCommitment(this.sessionId);
		const readOnly = new ReadOnlyCommitment(fullAccess, {
			recipientId: this.sessionId,
			expiresInMs: 24 * 60 * 60 * 1000
		});
		
		this.capabilities.add(readOnly);
		return readOnly as any;
	}
	
	/**
	 * Logout - revokes this session and all capabilities issued within it
	 */
	async logout(): Promise<void> {
		for (const cap of this.capabilities) {
			cap.revoke('Session logout');
		}
		this.revoke('Session logout');
	}
	
	/**
	 * Override revoke to also revoke all capabilities
	 */
	revoke(reason?: string): void {
		for (const cap of this.capabilities) {
			cap.revoke(reason);
		}
		super.revoke(reason);
	}
}

/**
 * Auth with Sessions - Natural session management
 */
class AuthWithSessions extends RpcTarget {
	private sessions = new Map<string, Session>();
	
	constructor(
		private createCommitment: (recipientId: string, options?: any) => MyCommitment
	) {
		super();
	}
	
	/**
	 * Login and create a session
	 * 
	 * Returns a Session capability that can issue other capabilities.
	 */
	async login(pubKey: string, signature: string): Promise<RpcStub<Session>> {
		if (!this.verify(pubKey, signature)) {
			throw new Error('Invalid signature');
		}
		
		const sessionId = crypto.randomUUID();
		const session = new Session(sessionId, this.createCommitment);
		
		this.sessions.set(sessionId, session);
		
		console.log(`[SESSION] Created: ${sessionId} for ${pubKey.slice(0, 8)}`);
		
		return session as any;
	}
	
	/**
	 * Revoke a session (admin function)
	 */
	async revokeSession(sessionId: string, reason?: string): Promise<boolean> {
		const session = this.sessions.get(sessionId);
		if (!session) return false;
		
		session.revoke(reason);
		this.sessions.delete(sessionId);
		
		console.log(`[SESSION] Revoked: ${sessionId} - ${reason || 'No reason'}`);
		
		return true;
	}
	
	private verify(pubKey: string, signature: string): boolean {
		// TODO: Implement signature verification
		return true;
	}
}

// ═══════════════════════════════════════════════════════════════════
// USAGE EXAMPLES
// ═══════════════════════════════════════════════════════════════════

/**
 * Example 1: Simple Direct Access
 * 
 * ```typescript
 * // Server exposes commitment
 * const myCommitment = new MyCommitment(myPubKey, () => getCurrentCommitment());
 * 
 * // Client connects
 * const alice = newWebSocketRpcSession<MyCommitment>("wss://alice.com/rpc");
 * 
 * // Use it - natural JavaScript!
 * const commitment = await alice.commitment();
 * const needs = await alice.needs();
 * 
 * // Revoke access
 * myCommitment.revoke('User requested');
 * ```
 */

/**
 * Example 2: Authentication Pattern (Recommended)
 * 
 * ```typescript
 * // Server exposes auth endpoint
 * const auth = new Auth((recipientId, options) => 
 *   new MyCommitment(myPubKey, () => getCurrentCommitment(), options)
 * );
 * 
 * // Client authenticates
 * const authApi = newWebSocketRpcSession<Auth>("wss://server.com/rpc");
 * const myAccess = await authApi.login(myPubKey, mySignature);
 * 
 * // Use authenticated capability
 * const commitment = await myAccess.commitment();
 * 
 * // Auto-expires in 24 hours!
 * ```
 */

/**
 * Example 3: Session Management (Enterprise)
 * 
 * ```typescript
 * // Server exposes session auth
 * const authWithSessions = new AuthWithSessions((recipientId, options) => 
 *   new MyCommitment(myPubKey, () => getCurrentCommitment(), options)
 * );
 * 
 * // Client logs in (gets session)
 * const session = await authApi.login(myPubKey, mySignature);
 * 
 * // Get capabilities from session
 * const commitment = await session.commitment();
 * const readOnly = await session.commitmentReadOnly();
 * 
 * // Logout - revokes ALL capabilities
 * await session.logout();
 * ```
 */

/**
 * Example 4: Read-Only Access (Least Privilege)
 * 
 * ```typescript
 * // Server issues read-only access
 * const fullAccess = new MyCommitment(myPubKey, () => getCurrentCommitment());
 * const readOnly = new ReadOnlyCommitment(fullAccess, {
 *   recipientId: publicUserId,
 *   expiresInMs: 24 * 60 * 60 * 1000
 * });
 * 
 * // Client can only read
 * await readOnly.commitment(); // ✅ Works
 * await readOnly.needs();      // ✅ Works
 * await readOnly.subscribe(); // ❌ Error: Method doesn't exist
 * ```
 */

/**
 * Example 5: Mutual Recognition (Object Capability Passing)
 * 
 * ```typescript
 * // Alice and Bob both have commitments
 * const aliceCommitment = new MyCommitment(alicePubKey, () => aliceData);
 * const bobCommitment = new MyCommitment(bobPubKey, () => bobData);
 * 
 * // Compute mutual recognition by passing capabilities!
 * const mr = await aliceCommitment.mutualWith(bobCommitment);
 * 
 * // This works over RPC too:
 * const bob = newWebSocketRpcSession<MyCommitment>("wss://bob.com/rpc");
 * const mr = await aliceCommitment.mutualWith(bob); // ✨ Magic!
 * ```
 */

// ═══════════════════════════════════════════════════════════════════
// EXPORTS
// ═══════════════════════════════════════════════════════════════════

export {
	type RpcStub,
	MyCommitment,
	MyCommitment as Commitment,
	ReadOnlyCommitment,
	Auth,
	AuthWithSessions,
	Session,
	RevocableRpcTarget
};

// ═══════════════════════════════════════════════════════════════════
// COMPARISON: Before vs After
// ═══════════════════════════════════════════════════════════════════

/**
 * BEFORE (Complex):
 * 
 * ```typescript
 * // Create target
 * const realTarget = new CommitmentRpcTarget(pubKey, store);
 * 
 * // Create manager
 * const manager = new CapabilityManager(realTarget);
 * 
 * // Issue revocable capability
 * const capability = manager.issue({
 *   recipientId: userId,
 *   expiresInMs: 24 * 60 * 60 * 1000,
 *   permissions: ['getCommitment', 'getNeedSlots']
 * });
 * 
 * // Revoke
 * manager.revoke(capability.getMetadata().id, 'Reason');
 * ```
 * 
 * AFTER (Elegant):
 * 
 * ```typescript
 * // Create commitment (revocable by default!)
 * const commitment = new MyCommitment(pubKey, () => getCurrentCommitment(), {
 *   recipientId: userId,
 *   expiresInMs: 24 * 60 * 60 * 1000
 * });
 * 
 * // Revoke
 * commitment.revoke('Reason');
 * ```
 * 
 * **60% less code, 100% more natural!**
 */

if (typeof window !== 'undefined') {
	(window as any).MyCommitment = MyCommitment;
	(window as any).Auth = Auth;
	(window as any).AuthWithSessions = AuthWithSessions;
	console.log('[RPC-ELEGANT] 🎨 Elegant RPC available in window');
}

