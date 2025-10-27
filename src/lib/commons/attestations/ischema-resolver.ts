import { type Attestation } from './common.js';

/**
 * Validation result for business logic checks
 */
export type ValidationResult =
	| { valid: true }
	| { valid: false; reason: string; code?: string };

/**
 * Context for attestation validation
 */
export type AttestationContext = {
	attestation: Attestation;
	value?: bigint; // Optional payment/stake amount
};

/**
 * ISchemaResolver Interface
 *
 * Handles business logic validation for attestations.
 * Structural validation is handled by Zod schemas before reaching the resolver.
 *
 * The resolver's job is to enforce semantic rules:
 * - Does the attester have permission?
 * - Do referenced attestations exist?
 * - Are business constraints satisfied?
 * - Is this operation allowed in the current context?
 */
export interface ISchemaResolver {
	/**
	 * Checks if the resolver accepts value (payments/stakes) with operations.
	 * @returns Whether the resolver supports value transfers
	 */
	isPayable(): boolean;

	/**
	 * Validates business logic for a single attestation.
	 * Called AFTER Zod validation, assumes structure is valid.
	 *
	 * @param context - The attestation and optional value
	 * @returns Validation result with reason if invalid
	 *
	 * @example
	 * ```typescript
	 * async validateAttest(context) {
	 *   // Check if referenced attestation exists
	 *   if (context.attestation.refUID !== EMPTY_UID) {
	 *     const exists = await this.attestationExists(context.attestation.refUID);
	 *     if (!exists) {
	 *       return { valid: false, reason: "Referenced attestation not found" };
	 *     }
	 *   }
	 *   return { valid: true };
	 * }
	 * ```
	 */
	validateAttest(context: AttestationContext): Promise<ValidationResult>;

	/**
	 * Validates business logic for multiple attestations.
	 * Called AFTER Zod validation, assumes structures are valid.
	 *
	 * @param contexts - Array of attestations with their optional values
	 * @returns Validation result with reason if any are invalid
	 *
	 * @example
	 * ```typescript
	 * async validateMultiAttest(contexts) {
	 *   for (const context of contexts) {
	 *     const result = await this.validateAttest(context);
	 *     if (!result.valid) {
	 *       return result; // Fail fast
	 *     }
	 *   }
	 *   return { valid: true };
	 * }
	 * ```
	 */
	validateMultiAttest(contexts: AttestationContext[]): Promise<ValidationResult>;

	/**
	 * Validates business logic for attestation revocation.
	 * Called AFTER Zod validation, assumes structure is valid.
	 *
	 * @param context - The attestation to revoke and optional value
	 * @returns Validation result with reason if invalid
	 *
	 * @example
	 * ```typescript
	 * async validateRevoke(context) {
	 *   // Check if attestation is revocable
	 *   if (!context.attestation.revocable) {
	 *     return { valid: false, reason: "Attestation is not revocable" };
	 *   }
	 *
	 *   // Check if already revoked
	 *   if (context.attestation.revocationTime > 0n) {
	 *     return { valid: false, reason: "Attestation already revoked" };
	 *   }
	 *
	 *   return { valid: true };
	 * }
	 * ```
	 */
	validateRevoke(context: AttestationContext): Promise<ValidationResult>;

	/**
	 * Validates business logic for multiple revocations.
	 * Called AFTER Zod validation, assumes structures are valid.
	 *
	 * @param contexts - Array of attestations to revoke with their optional values
	 * @returns Validation result with reason if any are invalid
	 */
	validateMultiRevoke(contexts: AttestationContext[]): Promise<ValidationResult>;
}
