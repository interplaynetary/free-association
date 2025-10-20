/**
 * Schema definitions using zod
 */
import { z } from 'zod';

// Basic types
export const IdSchema = z.string().min(1);
export const NameSchema = z.string().min(1);
export const PointsSchema = z.number().gte(0);
export const PercentageSchema = z.number().gte(0).lte(1);

// Timestamp field for Holster
// Holster strips internal metadata, so we use explicit application-level timestamps
// Use holsterTimestamp utilities from $lib/utils/holsterTimestamp.ts
export const TimestampFieldSchema = z.number().positive().optional();

// Helper to add timestamp to any schema
export function withTimestamp<T extends z.ZodTypeAny>(schema: T) {
	return schema.and(z.object({ _updatedAt: TimestampFieldSchema }));
}

// ShareMap - a record of node IDs to percentage values
export const ShareMapSchema = z.record(IdSchema, PercentageSchema);

export const NonRootNodeSchema = z.object({
	id: IdSchema,
	name: NameSchema,
	type: z.literal('NonRootNode'),
	manual_fulfillment: z.nullable(z.number()),
	children: z.array(z.any()),
	points: PointsSchema,
	parent_id: IdSchema,
	contributor_ids: z.array(IdSchema),
	anti_contributors_ids: z.array(IdSchema)
});

export const RootNodeSchema = z.object({
	id: IdSchema,
	name: NameSchema,
	type: z.literal('RootNode'),
	manual_fulfillment: z.nullable(z.number()),
	children: z.array(z.any()),
	created_at: z.string(),
	updated_at: z.string()
});

// Union type for Node (either RootNode or NonRootNode)
export const NodeSchema = z.union([RootNodeSchema, NonRootNodeSchema]);

// Availability Slot Schema - unified allocation model
export const AvailabilitySlotSchema = z.object({
	id: IdSchema, // Unique identifier for this slot
	quantity: z.number().gte(0),

	// Timing constraints (for edge cases - replaces manual claiming)
	advance_notice_hours: z.optional(z.number().gte(0)), // Required notice for booking
	booking_window_hours: z.optional(z.number().gte(0)), // How far in advance can book

	// Time pattern fields (moved from BaseCapacity)
	all_day: z.optional(z.boolean()),
	recurrence: z.optional(z.nullable(z.string())),
	custom_recurrence_repeat_every: z.optional(z.nullable(z.number())),
	custom_recurrence_repeat_unit: z.optional(z.nullable(z.string())),
	custom_recurrence_end_type: z.optional(z.nullable(z.string())),
	custom_recurrence_end_value: z.optional(z.nullable(z.string())),
	start_date: z.optional(z.nullable(z.string())),
	start_time: z.optional(z.nullable(z.string())),
	end_date: z.optional(z.nullable(z.string())),
	end_time: z.optional(z.nullable(z.string())),
	time_zone: z.optional(z.string()),

	// Location fields (moved from BaseCapacity - each slot can have different location)
	location_type: z.optional(z.string()),
	longitude: z.optional(z.number().min(-180).max(180)),
	latitude: z.optional(z.number().min(-90).max(90)),
	street_address: z.optional(z.string()),
	city: z.optional(z.string()),
	state_province: z.optional(z.string()),
	postal_code: z.optional(z.string()),
	country: z.optional(z.string()),
	online_link: z.optional(z.string().url().or(z.string().length(0))), // URL for online meetings/events

	// Hierarchical relationship for subset allocation
	parent_slot_id: z.optional(IdSchema), // If this is a subset of another slot

	// Mutual agreement for coordination
	mutual_agreement_required: z.optional(z.boolean().default(false)), // Requires explicit bilateral consent

	// Slot-specific metadata
	priority: z.optional(z.number()) // For conflict resolution
});

// Base capacity schema without any share information
export const BaseCapacitySchema = z.object({
	id: IdSchema,
	name: z.string(),
	emoji: z.optional(z.string()),
	unit: z.optional(
		z.string().regex(/^$|^(?!\s*\d+\.?\d*\s*$).+$/, {
			message: 'Unit must be text, not a number (e.g., "hours", "kg", "items")'
		})
	),
	description: z.optional(z.string()),
	max_natural_div: z.optional(z.number().gte(1)),
	max_percentage_div: z.optional(PercentageSchema),

	hidden_until_request_accepted: z.optional(z.boolean()),
	owner_id: z.optional(IdSchema),
	filter_rule: z.optional(z.nullable(z.any())),

	// Multiple availability slots instead of single quantity/time/location
	availability_slots: z.array(AvailabilitySlotSchema)

	// use-conditions: effects (possibly triggers)
});

// Provider perspective - efficient algorithm handles allocation internally
export const ProviderCapacitySchema = z.object({
	...BaseCapacitySchema.shape
	// DELETED: recipient_shares - Replaced by computedProviderAllocations
});

// DELETED: SlotComputedQuantitySchema - Replaced by efficient algorithm
// Old computed quantities replaced by direct slot allocations

// DELETED: SlotClaimSchema - Replaced by efficient algorithm
// Old individual claim tracking replaced by provider-computed allocations

// DELETED: UserSlotClaimsSchema - Replaced by unified compose-from model
// DELETED: NetworkSlotClaimsSchema - Slot claims are now handled as compose-from-self

// DELETED: UserSlotQuantitiesSchema - Replaced by efficient algorithm
// Old user slot quantities replaced by efficientSlotAllocations

// DELETED: NetworkSlotQuantitiesSchema - Replaced by efficient algorithm
// Old network slot quantities replaced by networkAllocationStates

// Recipient perspective - simplified for efficient algorithm
export const RecipientCapacitySchema = z.object({
	...BaseCapacitySchema.shape,
	provider_id: IdSchema
	// DELETED: share_percentage - Overall percentages meaningless with discrete allocation
	// DELETED: computed_quantities - Replaced by efficientSlotAllocations
});

// Union type for Capacity (either provider or recipient perspective)
export const CapacitySchema = z.union([ProviderCapacitySchema, RecipientCapacitySchema]);

// CapacitiesCollection schema (unwrapped - Gun tracks timestamps internally)
export const CapacitiesCollectionSchema = z.record(IdSchema, CapacitySchema);

// DELETED: CapacitySharesSchema - Replaced by efficient provider-centric algorithm
// Old capacity-level percentage shares no longer needed

// Recognition cache entry schema
export const RecognitionCacheEntrySchema = z.object({
	ourShare: PercentageSchema,
	theirShare: PercentageSchema,
	timestamp: z.number()
});

// Recognition cache schema
export const RecognitionCacheSchema = z.record(IdSchema, RecognitionCacheEntrySchema);

// Re-export the infer type for convenience
export type RootNode = z.infer<typeof RootNodeSchema>;
export type NonRootNode = z.infer<typeof NonRootNodeSchema>;
export type Node = z.infer<typeof NodeSchema>;
export type AvailabilitySlot = z.infer<typeof AvailabilitySlotSchema>;
// DELETED: SlotComputedQuantity and SlotClaim types - No longer needed
// DELETED: UserSlotClaims and NetworkSlotClaims types - Replaced by unified compose-from model
// DELETED: UserSlotQuantities and NetworkSlotQuantities types - No longer needed
export type BaseCapacity = z.infer<typeof BaseCapacitySchema>;
export type ProviderCapacity = z.infer<typeof ProviderCapacitySchema>;
export type RecipientCapacity = z.infer<typeof RecipientCapacitySchema>;
export type Capacity = z.infer<typeof CapacitySchema>;
export type CapacitiesCollection = z.infer<typeof CapacitiesCollectionSchema>;
export type ShareMap = z.infer<typeof ShareMapSchema>;
export type UserSlotCompositionData = z.infer<typeof UserSlotCompositionDataSchema>;
export type ContactsCollectionData = z.infer<typeof ContactsCollectionDataSchema>;
export type ChatReadStatesData = z.infer<typeof ChatReadStatesDataSchema>;
export type RecognitionCacheEntry = z.infer<typeof RecognitionCacheEntrySchema>;
export type RecognitionCache = z.infer<typeof RecognitionCacheSchema>;

// ========================
// Wallet and KYC Schemas
// ========================

// Basic EVM wallet info stored under user profile
export const WalletEvmSchema = z.object({
    address: z.string().regex(/^0x[a-fA-F0-9]{40}$/),
    chainId: z.number().optional(),
    connectedAt: z.number().optional()
});

export const WalletsSchema = z.object({
    evm: z.optional(WalletEvmSchema)
});

// KYC structures – lightweight and provider‑agnostic
export const KycStatusSchema = z.enum(['unverified', 'pending', 'verified', 'rejected']);

export const KycSelfAttestationSchema = z.object({
    method: z.literal('self'),
    full_name: z.string().min(1),
    country: z.string().min(1),
    dob: z.string().regex(/^\d{4}-\d{2}-\d{2}$/),
    over18: z.boolean(),
    submittedAt: z.number()
});

export const KycRecordSchema = z.object({
    status: KycStatusSchema,
    provider: z.string().default('self'),
    data: KycSelfAttestationSchema.optional(),
    updatedAt: z.number()
});

export type WalletEvm = z.infer<typeof WalletEvmSchema>;
export type Wallets = z.infer<typeof WalletsSchema>;
export type KycStatus = z.infer<typeof KycStatusSchema>;
export type KycSelfAttestation = z.infer<typeof KycSelfAttestationSchema>;
export type KycRecord = z.infer<typeof KycRecordSchema>;

// ==================================
// Bank Details (encrypted at rest)
// ==================================

// Very lightweight IBAN/BIC validation (basic shape only). Use stronger validation in UI if available.
export const IbanSchema = z
    .string()
    .transform((s) => s.replace(/\s+/g, '').toUpperCase())
    .refine((s) => /^[A-Z]{2}[0-9A-Z]{13,34}$/.test(s), 'Invalid IBAN format');

export const BicSchema = z
    .string()
    .transform((s) => s.replace(/\s+/g, '').toUpperCase())
    .refine((s) => /^[A-Z]{4}[A-Z]{2}[A-Z0-9]{2}([A-Z0-9]{3})?$/.test(s), 'Invalid BIC/SWIFT format');

export const BankDetailsSchema = z.object({
    holderName: z.string().min(1),
    iban: IbanSchema,
    bic: z.optional(BicSchema),
    bankName: z.optional(z.string().min(1)),
    country: z.optional(z.string().length(2)),
    updatedAt: z.number()
});

export type BankDetails = z.infer<typeof BankDetailsSchema>;

// Composition desire schema - maps target capacity ID to desired absolute units
export const CompositionDesireSchema = z.record(IdSchema, z.number().gte(0));

// User composition schema - maps source capacity ID to composition desires
// Structure: sourceCapacityId → targetCapacityId → absoluteUnits
export const UserCompositionSchema = z.record(IdSchema, CompositionDesireSchema);

// Network composition schema - maps user ID to their composition data
// Structure: userId → sourceCapacityId → targetCapacityId → absoluteUnits
export const NetworkCompositionSchema = z.record(IdSchema, UserCompositionSchema);

// Export types
export type CompositionDesire = z.infer<typeof CompositionDesireSchema>;
export type UserComposition = z.infer<typeof UserCompositionSchema>;
export type NetworkComposition = z.infer<typeof NetworkCompositionSchema>;

// Enhanced target identifier for slot composition - supports capacity IDs, pubkeys, and collectives
export const CompositionTargetSchema = z.union([
	z.string().regex(/^[0-9a-fA-F]{64}$/, 'Invalid pubkey format'), // Individual pubkey (64 hex chars)
	z.string().regex(/^collective:[0-9a-fA-F,]{64,}$/, 'Invalid collective format'), // collective:pubkey1,pubkey2,...
	IdSchema // Fallback for capacity IDs and other string identifiers (most permissive, should be last)
]);

// Slot-aware composition schemas - enables slot-to-slot composition with enhanced targets
// Structure: targetIdentifier → targetSlotId → desiredAbsoluteUnits
// targetIdentifier can be: capacityId | pubkey | "collective:pubkey1,pubkey2,..."
// NOTE: Using string keys and validating content separately to avoid Zod v4 union key issues
export const SlotCompositionDesireSchema = z.record(
	z.string(), // Use simple string key - validation happens at content level
	z.record(IdSchema, z.number().gte(0))
);

// User slot composition schema (unwrapped) - maps source slots to target slots
// Structure: sourceCapacityId → sourceSlotId → targetCapacityId → targetSlotId → absoluteUnits
export const UserSlotCompositionDataSchema = z.record(
	IdSchema,
	z.record(IdSchema, SlotCompositionDesireSchema)
);
export const UserSlotCompositionSchema = UserSlotCompositionDataSchema;

// Network slot composition schema - maps user ID to their slot composition data
// Structure: userId → sourceCapacityId → sourceSlotId → targetCapacityId → targetSlotId → absoluteUnits
export const NetworkSlotCompositionSchema = z.record(IdSchema, UserSlotCompositionSchema);

// Export slot composition types
export type CompositionTarget = z.infer<typeof CompositionTargetSchema>;
export type SlotCompositionDesire = z.infer<typeof SlotCompositionDesireSchema>;
export type UserSlotComposition = z.infer<typeof UserSlotCompositionSchema>;
export type NetworkSlotComposition = z.infer<typeof NetworkSlotCompositionSchema>;

// For backward compatibility - UserSlotCompositionData is now the same as UserSlotComposition
export type { UserSlotComposition as UserSlotCompositionDataType };

// ===== EFFICIENT DISTRIBUTION ALGORITHM SCHEMAS =====

// What recipients send to provider: their desires for specific slots
export const SlotDesireSchema = z.object({
	recipient_id: IdSchema,
	slot_id: IdSchema,
	desired_quantity: z.number().gte(0),
	updated_at: z.string()
});

// What provider computes and stores per slot after running the efficient algorithm
// FULLY TRANSPARENT: Contains all input data and intermediate calculations
export const SlotAllocationResultSchema = z.object({
	slot_id: IdSchema,
	total_quantity: z.number().gte(0),

	// Phase 1: MUTUAL desires (transparent input from compose-from/into intersection)
	all_desires: z.record(IdSchema, z.number().gte(0)), // recipient_id -> mutual_desired_amount

	// Phase 2: Mutually desiring recipients (those with mutual desires > 0)
	mutually_desiring_recipients: z.array(IdSchema),

	// Phase 3: MR normalization among mutually desiring recipients only (transparent calculations)
	mr_values: z.record(IdSchema, z.number().gte(0)), // recipient_id -> raw MR value (after capacity filtering)
	filtered_mr_sum: z.number().gte(0), // Sum of MR values for mutually desiring recipients only
	normalized_mr_shares: z.record(IdSchema, z.number().gte(0).lte(1)), // recipient_id -> normalized share

	// Phase 4: Raw MR allocations before mutual desire constraints (transparent intermediate step)
	raw_mr_allocations: z.record(IdSchema, z.number().gte(0)), // recipient_id -> raw_allocation

	// Phase 4: Mutual-desire-constrained allocations (transparent constraint application)
	desire_constrained_allocations: z.record(IdSchema, z.number().gte(0)), // recipient_id -> constrained_allocation

	// Phase 5: Redistribution details (transparent redistribution logic)
	unsatisfied_recipients: z.array(IdSchema), // Who still wants more after initial allocation (based on mutual desires)
	redistribution_amounts: z.record(IdSchema, z.number().gte(0)), // recipient_id -> redistribution_amount

	// MR-BASED ALLOCATION RESULTS (Form-Drive approach - Priority Sovereignty)
	final_allocations: z.record(IdSchema, z.number().gte(0)), // recipient_id -> final MR-based allocation (LEGACY: backward compatibility)
	unused_capacity: z.number().gte(0), // Remaining capacity after MR-based allocation (LEGACY: backward compatibility)
	mr_based_final_allocations: z.record(IdSchema, z.number().gte(0)), // recipient_id -> final MR-based allocation
	mr_based_unused_capacity: z.number().gte(0), // Remaining capacity after MR-based allocation

	// DESIRE-SCALED ALLOCATION (Sense-Drive approach - Desire Sovereignty)
	desire_scaled_provider_desires: z.record(IdSchema, z.number().gte(0)), // recipient_id -> provider_desired_amount
	desire_scaled_provider_sum: z.number().gte(0), // Sum of provider desires for mutually desiring recipients
	desire_scaled_normalized_shares: z.record(IdSchema, z.number().gte(0).lte(1)), // recipient_id -> normalized desire share
	desire_scaled_raw_allocations: z.record(IdSchema, z.number().gte(0)), // recipient_id -> raw desire-scaled allocation
	desire_scaled_constrained_allocations: z.record(IdSchema, z.number().gte(0)), // recipient_id -> desire allocation constrained by mutual desire
	desire_scaled_final_allocations: z.record(IdSchema, z.number().gte(0)), // recipient_id -> final desire-scaled allocation (with redistribution)
	desire_scaled_unused_capacity: z.number().gte(0), // Unused capacity in desire-scaled approach

	// ALLOCATION DEVIATION ANALYSIS (Play-Drive insights)
	mr_vs_desire_deviation: z.record(IdSchema, z.number()), // recipient_id -> (mr_allocation - desire_allocation)
	total_absolute_deviation: z.number().gte(0), // Sum of absolute deviations across all recipients
	max_recipient_deviation: z.number().gte(0), // Maximum individual recipient deviation
	deviation_recipients: z.array(IdSchema), // Recipients with significant allocation differences

	// Metadata for transparency and debugging
	computation_timestamp: z.string(),
	algorithm_version: z.string().default('dual_allocation_v1') // Updated to reflect dual approach
});

// Provider's complete allocation state per capacity (unwrapped)
export const ProviderAllocationStateDataSchema = z.record(
	IdSchema, // slot_id
	SlotAllocationResultSchema
);
export const ProviderAllocationStateSchema = ProviderAllocationStateDataSchema;

// Collection of all providers' allocation states (what recipients receive)
export const NetworkAllocationStatesSchema = z.record(
	IdSchema, // provider_id -> capacity_id
	z.record(IdSchema, ProviderAllocationStateDataSchema) // capacity_id -> slot allocations
);

// Types
export type SlotDesire = z.infer<typeof SlotDesireSchema>;
export type SlotAllocationResult = z.infer<typeof SlotAllocationResultSchema>;
export type ProviderAllocationStateData = z.infer<typeof ProviderAllocationStateDataSchema>;
export type ProviderAllocationState = z.infer<typeof ProviderAllocationStateSchema>;
export type NetworkAllocationStates = z.infer<typeof NetworkAllocationStatesSchema>;

// DELETED: SlotAllocationMetadata/Analysis schemas - Replaced by efficient algorithm
// Old recipient-centric allocation analysis replaced by allocationTransparencyAnalysis

// Contact schema - simplified to just essential fields
export const ContactSchema = z.object({
	contact_id: IdSchema,
	name: NameSchema, // User-assigned name
	public_key: z.optional(z.string()), // Their Gun public key (if they have one)
	created_at: z.string(),
	updated_at: z.string()
});

// ContactsCollection schema (unwrapped) - indexed by contact_id
export const ContactsCollectionDataSchema = z.record(IdSchema, ContactSchema);
export const ContactsCollectionSchema = ContactsCollectionDataSchema;

// Export contact types
export type Contact = z.infer<typeof ContactSchema>;
export type ContactsCollection = z.infer<typeof ContactsCollectionSchema>;

// Chat read state schema - tracks last read timestamp for each chat
export const ChatReadStateSchema = z.object({
	chatId: IdSchema,
	lastReadTimestamp: z.number(),
	updatedAt: z.number()
});

// Chat read states collection schema (unwrapped) - indexed by chatId
export const ChatReadStatesDataSchema = z.record(IdSchema, ChatReadStateSchema);
export const ChatReadStatesSchema = ChatReadStatesDataSchema;

// Export chat read state types
export type ChatReadState = z.infer<typeof ChatReadStateSchema>;
export type ChatReadStates = z.infer<typeof ChatReadStatesSchema>;

// Collective Tree Schema
// it should have a root node (the id being the hash of the ids of the contributors)
// it should be a merge of all the jsons of all the contributors
// except that when we merge the children have weights in %'s not points!
// we took the child's % of the siblings of a parent along a path across all userTrees and merged the %'s weighted by that user's share of collective recognition!

// Collective Node Schema - similar to Node but with percentage-based children
export const CollectiveNonRootNodeSchema = z.object({
	id: IdSchema,
	name: NameSchema,
	type: z.literal('CollectiveNonRootNode'),
	manual_fulfillment: z.nullable(z.number()),
	children: z.array(z.any()), // Will be CollectiveNode instances
	weight_percentage: PercentageSchema, // Percentage weight instead of points
	parent_id: IdSchema,
	contributor_ids: z.array(IdSchema),
	// Additional collective-specific fields
	source_contributors: z.record(IdSchema, PercentageSchema), // Maps contributor ID to their weight in this node
	merged_from_nodes: z.array(IdSchema) // Original node IDs that were merged to create this
});

export const CollectiveRootNodeSchema = z.object({
	id: IdSchema, // Hash of all contributor IDs
	name: NameSchema,
	type: z.literal('CollectiveRootNode'),
	manual_fulfillment: z.nullable(z.number()),
	children: z.array(z.any()), // Will be CollectiveNode instances
	created_at: z.string(),
	updated_at: z.string(),
	// Collective-specific fields
	contributors: z.array(IdSchema), // All contributor IDs
	contributor_weights: z.record(IdSchema, PercentageSchema), // Each contributor's recognition share
	source_trees: z.record(IdSchema, z.any()) // Maps contributor ID to their original tree
});

// Union type for CollectiveNode
export const CollectiveNodeSchema = z.union([
	CollectiveRootNodeSchema,
	CollectiveNonRootNodeSchema
]);

// Collective Tree Schema - the complete structure
export const CollectiveTreeSchema = z.object({
	id: IdSchema, // Hash of contributor IDs
	root: CollectiveRootNodeSchema,
	contributors: z.array(IdSchema),
	recognition_matrix: z.record(IdSchema, z.record(IdSchema, PercentageSchema)), // Mutual recognition between contributors
	creation_timestamp: z.string(),
	last_updated: z.string(),
	// Metadata about the merge process
	merge_algorithm_version: z.string(),
	total_nodes_merged: z.number(),
	merge_conflicts: z.optional(
		z.array(
			z.object({
				path: z.string(),
				contributors: z.array(IdSchema),
				resolution: z.string()
			})
		)
	)
});

// Tree merge configuration schema
export const TreeMergeConfigSchema = z.object({
	contributor_trees: z.record(IdSchema, NodeSchema), // Maps contributor ID to their tree
	recognition_shares: z.record(IdSchema, PercentageSchema), // Each contributor's recognition weight
	merge_strategy: z.enum(['weighted_average', 'consensus_threshold', 'priority_based']),
	conflict_resolution: z.enum(['skip', 'merge', 'prioritize_highest_weight']),
	minimum_weight_threshold: z.optional(PercentageSchema), // Minimum weight to include a node
	name_collision_strategy: z.enum(['append_contributor', 'weighted_priority', 'manual_resolve'])
});

// Export collective types
export type CollectiveRootNode = z.infer<typeof CollectiveRootNodeSchema>;
export type CollectiveNonRootNode = z.infer<typeof CollectiveNonRootNodeSchema>;
export type CollectiveNode = z.infer<typeof CollectiveNodeSchema>;
export type CollectiveTree = z.infer<typeof CollectiveTreeSchema>;
export type TreeMergeConfig = z.infer<typeof TreeMergeConfigSchema>;

// Utility type for tree merging operations
export const TreeMergeResultSchema = z.object({
	collective_tree: CollectiveTreeSchema,
	merge_stats: z.object({
		total_contributors: z.number(),
		nodes_merged: z.number(),
		conflicts_resolved: z.number(),
		execution_time_ms: z.number()
	}),
	warnings: z.array(z.string()),
	errors: z.array(z.string())
});

export type TreeMergeResult = z.infer<typeof TreeMergeResultSchema>;

// DELETED: Timestamped utility functions - No longer needed
// Use getGunTimestamp() from $lib/utils/gunTimestamp.ts to work with Gun's native timestamps
// Gun tracks all timestamps automatically via GUN.state.is()
