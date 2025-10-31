import { z } from 'zod';

// Constants
export const EMPTY_UID = '0x0000000000000000000000000000000000000000000000000000000000000000';
export const NO_EXPIRATION_TIME = 0n;

// Error types
export class AccessDeniedError extends Error {
	constructor() {
		super('Access denied');
		this.name = 'AccessDeniedError';
	}
}

export class DeadlineExpiredError extends Error {
	constructor() {
		super('Deadline expired');
		this.name = 'DeadlineExpiredError';
	}
}

export class InvalidEASError extends Error {
	constructor() {
		super('Invalid EAS');
		this.name = 'InvalidEASError';
	}
}

export class InvalidLengthError extends Error {
	constructor() {
		super('Invalid length');
		this.name = 'InvalidLengthError';
	}
}

export class InvalidSignatureError extends Error {
	constructor() {
		super('Invalid signature');
		this.name = 'InvalidSignatureError';
	}
}

export class NotFoundError extends Error {
	constructor() {
		super('Not found');
		this.name = 'NotFoundError';
	}
}

// Zod schemas for ECDSA signature data
export const SignatureSchema = z.object({
	v: z.number().int().min(0).max(255), // uint8: The recovery ID
	r: z.string().regex(/^0x[0-9a-fA-F]{64}$/), // bytes32: The x-coordinate of the nonce R
	s: z.string().regex(/^0x[0-9a-fA-F]{64}$/) // bytes32: The signature data
});

export type Signature = z.infer<typeof SignatureSchema>;

// Zod schema for a single attestation
export const AttestationSchema = z.object({
	uid: z.string().regex(/^0x[0-9a-fA-F]{64}$/), // bytes32: A unique identifier of the attestation
	schema: z.string().regex(/^0x[0-9a-fA-F]{64}$/), // bytes32: The unique identifier of the schema
	time: z.bigint().nonnegative(), // uint64: The time when the attestation was created (Unix timestamp)
	expirationTime: z.bigint().nonnegative(), // uint64: The time when the attestation expires (Unix timestamp)
	revocationTime: z.bigint().nonnegative(), // uint64: The time when the attestation was revoked (Unix timestamp)
	refUID: z.string().regex(/^0x[0-9a-fA-F]{64}$/), // bytes32: The UID of the related attestation
	recipient: z.string().regex(/^0x[0-9a-fA-F]{40}$/), // address: The recipient of the attestation
	attester: z.string().regex(/^0x[0-9a-fA-F]{40}$/), // address: The attester/sender of the attestation
	revocable: z.boolean(), // bool: Whether the attestation is revocable
	data: z.string().regex(/^0x[0-9a-fA-F]*$/) // bytes: Custom attestation data (hex string)
});

export type Attestation = z.infer<typeof AttestationSchema>;
