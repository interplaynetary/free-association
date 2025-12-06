/**
 * Implicit RPC - Cap'n Web Philosophy
 * 
 * Revocation by connection closure, not explicit methods.
 * This is the approach suggested by the Cap'n Web article.
 * 
 * ~50 lines vs 200 lines (75% reduction from V3!)
 * 
 * Key insight: "Capabilities are revoked when connections close"
 */

import { RpcTarget, type RpcStub } from 'capnweb';
import type {
	Commitment as CommitmentData,
	NeedSlot,
	AvailabilitySlot,
	GlobalRecognitionWeights
} from '../../../../../../../src/lib/protocol/schemas';

// ═══════════════════════════════════════════════════════════════════
// IMPLICIT COMMITMENT (No Explicit Revocation!)
// ═══════════════════════════════════════════════════════════════════

/**
 * Commitment - Implicit revocation via validity flag
 * 
 * No .revoke() method!
 * No expiration timers!
 * No tracking infrastructure!
 * 
 * Just a simple validity check that throws if invalid.
 * Revocation happens when:
 * - Connection closes (automatic!)
 * - Server marks it invalid
 * - Batch operation ends
 */
class Commitment extends RpcTarget {
	private valid = true;
	
	constructor(
		private pubKey: string,
		private data: () => CommitmentData | null
	) {
		super();
	}
	
	// Simple validity check - throws if invalid
	private check(): void {
		if (!this.valid) {
			throw new Error('Commitment no longer valid (connection closed?)');
		}
	}
	
	// All methods just check validity and proceed
	async commitment(): Promise<CommitmentData> {
		this.check();
		const c = this.data();
		if (!c) throw new Error('No commitment');
		return c;
	}
	
	async needs(): Promise<NeedSlot[]> {
		this.check();
		return (await this.commitment()).need_slots || [];
	}
	
	async capacity(): Promise<AvailabilitySlot[]> {
		this.check();
		return (await this.commitment()).capacity_slots || [];
	}
	
	async recognition(): Promise<GlobalRecognitionWeights> {
		this.check();
		return (await this.commitment()).global_recognition_weights || {};
	}
	
	async key(): Promise<string> {
		this.check();
		return this.pubKey;
	}
	
	// "Revocation" is just marking invalid
	// Real revocation happens when connection closes!
	invalidate(): void {
		this.valid = false;
	}
}

// ═══════════════════════════════════════════════════════════════════
// IMPLICIT SESSION (No Tracking!)
// ═══════════════════════════════════════════════════════════════════

/**
 * Session - Implicit cleanup via connection
 * 
 * No child tracking!
 * No explicit revocation!
 * No timers!
 * 
 * When the session ends:
 * - Mark it invalid
 * - Close the connection (optional)
 * - All capabilities from this connection automatically break!
 */
class Session extends RpcTarget {
	private valid = true;
	
	constructor(
		public sessionId: string,
		private create: (recipientId: string) => Commitment
	) {
		super();
	}
	
	private check(): void {
		if (!this.valid) {
			throw new Error('Session ended');
		}
	}
	
	async commitment(): Promise<RpcStub<Commitment>> {
		this.check();
		return this.create(this.sessionId) as any;
	}
	
	async logout(): Promise<void> {
		this.valid = false;
		// Connection can close here, breaking all capabilities!
	}
}

// ═══════════════════════════════════════════════════════════════════
// IMPLICIT AUTH (Simple!)
// ═══════════════════════════════════════════════════════════════════

/**
 * Auth - Return capabilities directly
 * 
 * Following the article's authenticate() pattern exactly.
 * No expiration, no tracking, no revocation infrastructure.
 */
class Auth extends RpcTarget {
	constructor(
		private create: (recipientId: string) => Commitment
	) {
		super();
	}
	
	async login(pubKey: string, signature: string): Promise<RpcStub<Session>> {
		if (!this.verify(pubKey, signature)) {
			throw new Error('Invalid signature');
		}
		
		const sessionId = crypto.randomUUID();
		return new Session(sessionId, this.create) as any;
	}
	
	private verify(pubKey: string, signature: string): boolean {
		// TODO: Implement signature verification
		return true;
	}
}

// ═══════════════════════════════════════════════════════════════════
// COMPARISON EXAMPLES
// ═══════════════════════════════════════════════════════════════════

/**
 * Example: Ultra-Elegant V3 vs Implicit V4
 * 
 * V3 (Auto-Revocation, 200 lines):
 * ```typescript
 * const commitment = new Commitment(pubKey, getData)
 *   .expiresIn('24h')
 *   .for(userId)
 *   .extend('12h');
 * 
 * commitment.revoke('User logged out');
 * // Proxy auto-checks on every call
 * ```
 * 
 * V4 (Implicit, 50 lines):
 * ```typescript
 * const commitment = new Commitment(pubKey, getData);
 * // Connection closure handles revocation!
 * 
 * commitment.invalidate();  // Just mark invalid
 * // Or close connection - all capabilities break!
 * ```
 * 
 * Trade-offs:
 * - V3: More control, explicit expiration, cascading, audit trail
 * - V4: Simpler code, connection-scoped, minimal infrastructure
 */

/**
 * Example: Session Management
 * 
 * V3 (Auto-tracking):
 * ```typescript
 * class Session extends AutoRevocableRpcTarget {
 *   private children = new Set<AutoRevocableRpcTarget>();
 *   
 *   async commitment() {
 *     const cap = this.create(this.sessionId).expiresIn('24h');
 *     this.track(cap);  // Manual tracking
 *     return cap;
 *   }
 *   
 *   revoke(reason?: string) {
 *     this.revokeAll(reason);  // Cascade to children
 *     return super.revoke(reason);
 *   }
 * }
 * ```
 * 
 * V4 (Implicit):
 * ```typescript
 * class Session extends RpcTarget {
 *   private valid = true;
 *   
 *   async commitment() {
 *     this.check();  // Simple validity check
 *     return this.create(this.sessionId);
 *   }
 *   
 *   async logout() {
 *     this.valid = false;  // Just mark invalid
 *     // Connection closure handles cleanup!
 *   }
 * }
 * ```
 * 
 * V4 is 4x simpler!
 */

/**
 * Example: Batch Operations (Cap'n Web Style)
 * 
 * ```typescript
 * import { newHttpBatchRpcSession } from 'capnweb';
 * 
 * // Create batch session
 * const batch = newHttpBatchRpcSession<Auth>("https://api.example.com/rpc");
 * 
 * // Make calls (pipelined!)
 * const sessionPromise = batch.login(pubKey, sig);
 * const needs = await sessionPromise.commitment().needs();
 * 
 * // Batch ends automatically after await!
 * // All capabilities from this batch are now broken.
 * // No explicit revocation needed!
 * ```
 * 
 * This is the Cap'n Web way: short-lived, auto-cleanup.
 */

// ═══════════════════════════════════════════════════════════════════
// EXPORTS
// ═══════════════════════════════════════════════════════════════════

export {
	Commitment,
	Session,
	Auth,
	type RpcStub
};

// ═══════════════════════════════════════════════════════════════════
// KEY INSIGHTS FROM CAP'N WEB ARTICLE
// ═══════════════════════════════════════════════════════════════════

/**
 * 1. "Once you've awaited an RPC in the batch, the batch is done,
 *     and all the remote references received through it become broken."
 *     — Article line 134
 * 
 *    Revocation by connection lifecycle!
 * 
 * 2. "It is impossible for the client to 'forge' a session object.
 *     The only way to get one is to call authenticate()"
 *     — Article line 209
 * 
 *    Security by possession, not verification!
 * 
 * 3. No mention of explicit .revoke() methods anywhere in the article.
 * 
 *    Cap'n Web prefers implicit revocation!
 */

/**
 * When to use V4 (Implicit) vs V3 (Auto-Revocation):
 * 
 * Use V4 (Implicit) when:
 * ✅ Short-lived operations (minutes to hours)
 * ✅ Connection-scoped capabilities
 * ✅ Batch operations
 * ✅ Simplicity is paramount
 * ✅ Connection management is straightforward
 * 
 * Use V3 (Auto-Revocation) when:
 * ✅ Long-lived operations (hours to days)
 * ✅ Need individual capability revocation
 * ✅ Need cascading revocation
 * ✅ Need explicit expiration with timers
 * ✅ Need detailed audit trail
 * 
 * Or use BOTH:
 * - Quick operations → V4
 * - Complex sessions → V3
 */

if (typeof window !== 'undefined') {
	(window as any).ImplicitCommitment = Commitment;
	(window as any).ImplicitSession = Session;
	(window as any).ImplicitAuth = Auth;
	console.log('[RPC-IMPLICIT] ✨ Simple implicit RPC (Cap\'n Web style)');
}

