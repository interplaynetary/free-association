import { z } from 'zod';
import { AttestationSchema, SignatureSchema } from './common.js';

// Attestation Request Data Schema
export const AttestationRequestDataSchema = z.object({
	recipient: z.string().regex(/^0x[0-9a-fA-F]{40}$/), // address: The recipient of the attestation
	expirationTime: z.bigint().nonnegative(), // uint64: The time when the attestation expires (Unix timestamp)
	revocable: z.boolean(), // bool: Whether the attestation is revocable
	refUID: z.string().regex(/^0x[0-9a-fA-F]{64}$/), // bytes32: The UID of the related attestation
	data: z.string().regex(/^0x[0-9a-fA-F]*$/), // bytes: Custom attestation data (hex string)
	value: z.bigint().nonnegative() // uint256: An explicit ETH amount to send to the resolver
});

export type AttestationRequestData = z.infer<typeof AttestationRequestDataSchema>;

// Attestation Request Schema
export const AttestationRequestSchema = z.object({
	schema: z.string().regex(/^0x[0-9a-fA-F]{64}$/), // bytes32: The unique identifier of the schema
	data: AttestationRequestDataSchema // The arguments of the attestation request
});

export type AttestationRequest = z.infer<typeof AttestationRequestSchema>;

// Delegated Attestation Request Schema
export const DelegatedAttestationRequestSchema = z.object({
	schema: z.string().regex(/^0x[0-9a-fA-F]{64}$/), // bytes32: The unique identifier of the schema
	data: AttestationRequestDataSchema, // The arguments of the attestation request
	signature: SignatureSchema, // The ECDSA signature data
	attester: z.string().regex(/^0x[0-9a-fA-F]{40}$/), // address: The attesting account
	deadline: z.bigint().nonnegative() // uint64: The deadline of the signature/request
});

export type DelegatedAttestationRequest = z.infer<typeof DelegatedAttestationRequestSchema>;

// Multi Attestation Request Schema
export const MultiAttestationRequestSchema = z.object({
	schema: z.string().regex(/^0x[0-9a-fA-F]{64}$/), // bytes32: The unique identifier of the schema
	data: z.array(AttestationRequestDataSchema) // The arguments of the attestation requests
});

export type MultiAttestationRequest = z.infer<typeof MultiAttestationRequestSchema>;

// Multi Delegated Attestation Request Schema
export const MultiDelegatedAttestationRequestSchema = z.object({
	schema: z.string().regex(/^0x[0-9a-fA-F]{64}$/), // bytes32: The unique identifier of the schema
	data: z.array(AttestationRequestDataSchema), // The arguments of the attestation requests
	signatures: z.array(SignatureSchema), // The ECDSA signatures data (signed with increasing nonces)
	attester: z.string().regex(/^0x[0-9a-fA-F]{40}$/), // address: The attesting account
	deadline: z.bigint().nonnegative() // uint64: The deadline of the signature/request
});

export type MultiDelegatedAttestationRequest = z.infer<
	typeof MultiDelegatedAttestationRequestSchema
>;

// Revocation Request Data Schema
export const RevocationRequestDataSchema = z.object({
	uid: z.string().regex(/^0x[0-9a-fA-F]{64}$/), // bytes32: The UID of the attestation to revoke
	value: z.bigint().nonnegative() // uint256: An explicit ETH amount to send to the resolver
});

export type RevocationRequestData = z.infer<typeof RevocationRequestDataSchema>;

// Revocation Request Schema
export const RevocationRequestSchema = z.object({
	schema: z.string().regex(/^0x[0-9a-fA-F]{64}$/), // bytes32: The unique identifier of the schema
	data: RevocationRequestDataSchema // The arguments of the revocation request
});

export type RevocationRequest = z.infer<typeof RevocationRequestSchema>;

// Delegated Revocation Request Schema
export const DelegatedRevocationRequestSchema = z.object({
	schema: z.string().regex(/^0x[0-9a-fA-F]{64}$/), // bytes32: The unique identifier of the schema
	data: RevocationRequestDataSchema, // The arguments of the revocation request
	signature: SignatureSchema, // The ECDSA signature data
	revoker: z.string().regex(/^0x[0-9a-fA-F]{40}$/), // address: The revoking account
	deadline: z.bigint().nonnegative() // uint64: The deadline of the signature/request
});

export type DelegatedRevocationRequest = z.infer<typeof DelegatedRevocationRequestSchema>;

// Multi Revocation Request Schema
export const MultiRevocationRequestSchema = z.object({
	schema: z.string().regex(/^0x[0-9a-fA-F]{64}$/), // bytes32: The unique identifier of the schema
	data: z.array(RevocationRequestDataSchema) // The arguments of the revocation requests
});

export type MultiRevocationRequest = z.infer<typeof MultiRevocationRequestSchema>;

// Multi Delegated Revocation Request Schema
export const MultiDelegatedRevocationRequestSchema = z.object({
	schema: z.string().regex(/^0x[0-9a-fA-F]{64}$/), // bytes32: The unique identifier of the schema
	data: z.array(RevocationRequestDataSchema), // The arguments of the revocation requests
	signatures: z.array(SignatureSchema), // The ECDSA signatures data (signed with increasing nonces)
	revoker: z.string().regex(/^0x[0-9a-fA-F]{40}$/), // address: The revoking account
	deadline: z.bigint().nonnegative() // uint64: The deadline of the signature/request
});

export type MultiDelegatedRevocationRequest = z.infer<
	typeof MultiDelegatedRevocationRequestSchema
>;

// Event types for attestation operations
export type AttestationEvent = {
	recipient: string; // address
	attester: string; // address
	uid: string; // bytes32
	schemaUID: string; // bytes32
};

export type RevocationEvent = {
	recipient: string; // address
	attester: string; // address
	uid: string; // bytes32
	schemaUID: string; // bytes32
};

export type TimestampedEvent = {
	data: string; // bytes32
	timestamp: bigint; // uint64
};

export type RevokedOffchainEvent = {
	revoker: string; // address
	data: string; // bytes32
	timestamp: bigint; // uint64
};

// IEAS Interface representation (for documentation and type checking)
export interface IEAS {
	// Attestation methods
	attest(request: AttestationRequest): Promise<string>; // returns UID (bytes32)
	attestByDelegation(delegatedRequest: DelegatedAttestationRequest): Promise<string>;
	multiAttest(multiRequests: MultiAttestationRequest[]): Promise<string[]>;
	multiAttestByDelegation(
		multiDelegatedRequests: MultiDelegatedAttestationRequest[]
	): Promise<string[]>;

	// Revocation methods
	revoke(request: RevocationRequest): Promise<void>;
	revokeByDelegation(delegatedRequest: DelegatedRevocationRequest): Promise<void>;
	multiRevoke(multiRequests: MultiRevocationRequest[]): Promise<void>;
	multiRevokeByDelegation(
		multiDelegatedRequests: MultiDelegatedRevocationRequest[]
	): Promise<void>;

	// Timestamp methods
	timestamp(data: string): Promise<bigint>; // returns uint64
	multiTimestamp(data: string[]): Promise<bigint>;
	revokeOffchain(data: string): Promise<bigint>;
	multiRevokeOffchain(data: string[]): Promise<bigint>;

	// Query methods
	getAttestation(uid: string): Promise<z.infer<typeof AttestationSchema>>;
	isAttestationValid(uid: string): Promise<boolean>;
	getTimestamp(data: string): Promise<bigint>;
	getRevokeOffchain(revoker: string, data: string): Promise<bigint>;
}
