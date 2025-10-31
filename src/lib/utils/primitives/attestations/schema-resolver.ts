import { type Attestation } from './common.js';
import {
	type ISchemaResolver,
	type ValidationResult,
	type AttestationContext
} from './ischema-resolver.js';

export class ValidationError extends Error {
	public readonly code?: string;

	constructor(message: string, code?: string) {
		super(message);
		this.name = 'ValidationError';
		this.code = code;
	}
}

/**
 * SchemaResolver - The base schema resolver class
 *
 * Provides default implementations for business logic validation.
 * Subclasses override the protected validation methods to implement
 * schema-specific rules.
 *
 * Separation of concerns:
 * - Zod schemas handle structural validation (types, formats, required fields)
 * - SchemaResolver handles business logic validation (permissions, constraints, state)
 */
export abstract class SchemaResolver implements ISchemaResolver {
	protected readonly _version: { major: number; minor: number; patch: number };

	/**
	 * Creates a new resolver
	 */
	constructor() {
		this._version = { major: 2, minor: 0, patch: 0 };
	}

	/**
	 * Returns the version of the resolver
	 */
	public get version(): string {
		return `${this._version.major}.${this._version.minor}.${this._version.patch}`;
	}

	/**
	 * Checks if the resolver accepts value with operations
	 * Override this method to enable payable functionality
	 */
	public isPayable(): boolean {
		return false;
	}

	/**
	 * Validates a single attestation (public interface)
	 */
	public async validateAttest(context: AttestationContext): Promise<ValidationResult> {
		// Check if value is provided but resolver isn't payable
		if (context.value && context.value > 0n && !this.isPayable()) {
			return {
				valid: false,
				reason: 'Resolver does not accept value',
				code: 'NOT_PAYABLE'
			};
		}

		// Delegate to abstract method
		return this.onValidateAttest(context);
	}

	/**
	 * Validates multiple attestations (public interface)
	 */
	public async validateMultiAttest(contexts: AttestationContext[]): Promise<ValidationResult> {
		// Check payable requirement
		if (!this.isPayable()) {
			const hasValue = contexts.some((ctx) => ctx.value && ctx.value > 0n);
			if (hasValue) {
				return {
					valid: false,
					reason: 'Resolver does not accept value',
					code: 'NOT_PAYABLE'
				};
			}
		}

		// Delegate to abstract method
		return this.onValidateMultiAttest(contexts);
	}

	/**
	 * Validates a single revocation (public interface)
	 */
	public async validateRevoke(context: AttestationContext): Promise<ValidationResult> {
		// Check if attestation is revocable
		if (!context.attestation.revocable) {
			return {
				valid: false,
				reason: 'Attestation is not revocable',
				code: 'NOT_REVOCABLE'
			};
		}

		// Check if already revoked
		if (context.attestation.revocationTime > 0n) {
			return {
				valid: false,
				reason: 'Attestation is already revoked',
				code: 'ALREADY_REVOKED'
			};
		}

		// Check payable requirement
		if (context.value && context.value > 0n && !this.isPayable()) {
			return {
				valid: false,
				reason: 'Resolver does not accept value',
				code: 'NOT_PAYABLE'
			};
		}

		// Delegate to abstract method
		return this.onValidateRevoke(context);
	}

	/**
	 * Validates multiple revocations (public interface)
	 */
	public async validateMultiRevoke(contexts: AttestationContext[]): Promise<ValidationResult> {
		// Check revocable and already-revoked for all
		for (const context of contexts) {
			if (!context.attestation.revocable) {
				return {
					valid: false,
					reason: `Attestation ${context.attestation.uid} is not revocable`,
					code: 'NOT_REVOCABLE'
				};
			}

			if (context.attestation.revocationTime > 0n) {
				return {
					valid: false,
					reason: `Attestation ${context.attestation.uid} is already revoked`,
					code: 'ALREADY_REVOKED'
				};
			}
		}

		// Check payable requirement
		if (!this.isPayable()) {
			const hasValue = contexts.some((ctx) => ctx.value && ctx.value > 0n);
			if (hasValue) {
				return {
					valid: false,
					reason: 'Resolver does not accept value',
					code: 'NOT_PAYABLE'
				};
			}
		}

		// Delegate to abstract method
		return this.onValidateMultiRevoke(contexts);
	}

	/**
	 * Override this method to implement attestation business logic validation.
	 * Called AFTER structural validation and basic checks.
	 *
	 * @param context - The attestation context
	 * @returns Validation result
	 *
	 * @example
	 * ```typescript
	 * protected async onValidateAttest(context: AttestationContext): Promise<ValidationResult> {
	 *   // Check if attester has permission
	 *   const hasPermission = await this.checkPermission(context.attestation.attester);
	 *   if (!hasPermission) {
	 *     return { valid: false, reason: "Attester lacks permission", code: "NO_PERMISSION" };
	 *   }
	 *
	 *   // Check if referenced attestation exists
	 *   if (context.attestation.refUID !== EMPTY_UID) {
	 *     const exists = await this.attestationExists(context.attestation.refUID);
	 *     if (!exists) {
	 *       return { valid: false, reason: "Referenced attestation not found", code: "REF_NOT_FOUND" };
	 *     }
	 *   }
	 *
	 *   return { valid: true };
	 * }
	 * ```
	 */
	protected abstract onValidateAttest(context: AttestationContext): Promise<ValidationResult>;

	/**
	 * Override this method to implement multi-attestation business logic validation.
	 * Called AFTER structural validation and basic checks.
	 *
	 * Default implementation validates each attestation individually.
	 * Override to implement cross-attestation validation logic.
	 *
	 * @param contexts - Array of attestation contexts
	 * @returns Validation result
	 */
	protected async onValidateMultiAttest(
		contexts: AttestationContext[]
	): Promise<ValidationResult> {
		// Default: validate each individually
		for (const context of contexts) {
			const result = await this.onValidateAttest(context);
			if (!result.valid) {
				return result;
			}
		}
		return { valid: true };
	}

	/**
	 * Override this method to implement revocation business logic validation.
	 * Called AFTER structural validation and basic checks (revocable, not already revoked).
	 *
	 * @param context - The attestation context
	 * @returns Validation result
	 *
	 * @example
	 * ```typescript
	 * protected async onValidateRevoke(context: AttestationContext): Promise<ValidationResult> {
	 *   // Only original attester can revoke
	 *   const currentUser = this.getCurrentUser();
	 *   if (context.attestation.attester !== currentUser) {
	 *     return { valid: false, reason: "Only attester can revoke", code: "NOT_ATTESTER" };
	 *   }
	 *
	 *   return { valid: true };
	 * }
	 * ```
	 */
	protected abstract onValidateRevoke(context: AttestationContext): Promise<ValidationResult>;

	/**
	 * Override this method to implement multi-revocation business logic validation.
	 * Called AFTER structural validation and basic checks.
	 *
	 * Default implementation validates each revocation individually.
	 * Override to implement cross-revocation validation logic.
	 *
	 * @param contexts - Array of attestation contexts
	 * @returns Validation result
	 */
	protected async onValidateMultiRevoke(
		contexts: AttestationContext[]
	): Promise<ValidationResult> {
		// Default: validate each individually
		for (const context of contexts) {
			const result = await this.onValidateRevoke(context);
			if (!result.valid) {
				return result;
			}
		}
		return { valid: true };
	}
}
