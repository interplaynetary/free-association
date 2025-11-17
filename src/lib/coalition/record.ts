/**
 * Free Association Coalition - Secretariat Record Schemas
 * 
 * Zod schemas for all record types defined in format.md
 * These schemas provide runtime validation and TypeScript types
 */

import { z } from 'zod';

// ============================================================================
// Base Schemas & Utilities
// ============================================================================

/**
 * UUID v4 validation
 */
export const UUIDSchema = z.string().uuid();

/**
 * ISO 8601 DateTime string
 */
export const ISODateTimeSchema = z.string().datetime();

/**
 * Record status enum
 */
export const RecordStatusSchema = z.enum(['pending', 'adopted', 'rejected']);

/**
 * Base record structure common to all record types
 */
export const BaseRecordSchema = z.object({
  id: UUIDSchema,
  timestamp: ISODateTimeSchema,
  issuer: UUIDSchema,
  type: z.string(),
  status: RecordStatusSchema,
  decision_timestamp: ISODateTimeSchema.optional(),
});

// ============================================================================
// 1. Identity & Membership (Who we are)
// ============================================================================

export const MembershipActionSchema = z.enum(['add', 'remove', 'replace']);

export const MembershipUpdateDataSchema = z.object({
  organization: z.string(),
  members: z.array(UUIDSchema),
  action: MembershipActionSchema,
});

export const MembershipUpdateRecordSchema = BaseRecordSchema.extend({
  type: z.literal('membership_update'),
  data: MembershipUpdateDataSchema,
});

// ---

export const RegistryTypeSchema = z.enum(['participants', 'members', 'contacts', 'consultants', 'observers']);
export const RegistryActionSchema = z.enum(['add', 'update']);

export const RegistryEntryDataSchema = z.object({
  registry_type: RegistryTypeSchema,
  entry_id: UUIDSchema,
  entry_data: z.record(z.unknown()),
  action: RegistryActionSchema,
  effective_from: ISODateTimeSchema,
});

export const RegistryEntryRecordSchema = BaseRecordSchema.extend({
  type: z.literal('registry_entry'),
  data: RegistryEntryDataSchema,
});

// ---

export const VerificationMethodSchema = z.enum(['pgp', 'x509', 'did']);

export const ContactInfoDataSchema = z.object({
  participant_id: UUIDSchema,
  email: z.string().email(),
  public_key: z.string(),
  verification_method: VerificationMethodSchema,
  verified_at: ISODateTimeSchema,
});

export const ContactInfoRecordSchema = BaseRecordSchema.extend({
  type: z.literal('contact_info'),
  data: ContactInfoDataSchema,
});

// ============================================================================
// 2. Recognition & Relationships (How we relate)
// ============================================================================

export const RecognitionTypeSchema = z.enum(['contribution', 'allocation_weight']);

export const RecognitionDistributionDataSchema = z.object({
  recognized_entity: UUIDSchema,
  recognition_percentage: z.number().min(-100).max(100),
  rationale: z.string(),
  recognition_type: RecognitionTypeSchema,
});

export const RecognitionDistributionRecordSchema = BaseRecordSchema.extend({
  type: z.literal('recognition_distribution'),
  data: RecognitionDistributionDataSchema,
});

// ============================================================================
// 3. State Declarations (What we have/need)
// ============================================================================

export const StateCategorySchema = z.enum(['capacities', 'needs', 'environment']);

export const StateDeclarationDataSchema = z.object({
  category: StateCategorySchema,
  assets: z.record(z.unknown()),
  valid_until: ISODateTimeSchema,
});

export const StateDeclarationRecordSchema = BaseRecordSchema.extend({
  type: z.literal('state_declaration'),
  data: StateDeclarationDataSchema,
});

// ---

export const CapacityOfferDataSchema = z.object({
  resource_type: z.string(),
  quantity: z.number(),
  conditions: z.string(),
  expiry: ISODateTimeSchema,
});

export const CapacityOfferRecordSchema = BaseRecordSchema.extend({
  type: z.literal('capacity_offer'),
  data: CapacityOfferDataSchema,
});

// ============================================================================
// 4. Proposals & Expressions (What we propose)
// ============================================================================

export const ProposalDataSchema = z.object({
  proposal_type: z.string(),
  title: z.string(),
  content: z.record(z.unknown()),
  requires_decision: z.boolean(),
  decision_deadline: ISODateTimeSchema,
});

export const ProposalRecordSchema = BaseRecordSchema.extend({
  type: z.literal('proposal'),
  data: ProposalDataSchema,
});

// ---

export const StatementTypeSchema = z.enum(['declaration', 'position', 'announcement', 'clarification', 'deliberation']);

export const StatementDataSchema = z.object({
  statement_type: StatementTypeSchema,
  content: z.string(),
  referenced_records: z.array(UUIDSchema),
});

export const StatementRecordSchema = BaseRecordSchema.extend({
  type: z.literal('statement'),
  data: StatementDataSchema,
});

// ============================================================================
// 5. Decision-Making (How we decide)
// ============================================================================

export const PositionTypeSchema = z.enum(['challenge', 'oppose', 'abstain']);

export const PositionDataSchema = z.object({
  proposal_id: UUIDSchema,
  position: PositionTypeSchema,
  rationale: z.string(),
});

export const PositionRecordSchema = BaseRecordSchema.extend({
  type: z.literal('position'),
  data: PositionDataSchema,
});

// ---

export const SupportExpressionDataSchema = z.object({
  proposal_id: UUIDSchema,
  weights: z.record(UUIDSchema, z.number().min(0).max(1)),
  total_weight: z.number().min(0).max(1),
});

export const SupportExpressionRecordSchema = BaseRecordSchema.extend({
  type: z.literal('support_expression'),
  data: SupportExpressionDataSchema,
});

// ---

export const DecisionOutcomeSchema = z.enum(['adopted', 'rejected', 'tabled']);

export const DecisionOutcomeDataSchema = z.object({
  proposal_id: UUIDSchema,
  outcome: DecisionOutcomeSchema,
  vote_summary: z.record(z.unknown()),
});

export const DecisionOutcomeRecordSchema = BaseRecordSchema.extend({
  type: z.literal('decision_outcome'),
  data: DecisionOutcomeDataSchema,
});

// ---

export const ProtocolAdoptionDataSchema = z.object({
  protocol_name: z.string(),
  protocol_version: z.string(),
  rules: z.record(z.unknown()),
  replaces_previous: UUIDSchema.nullable(),
  content_hash: z.string(),
});

export const ProtocolAdoptionRecordSchema = BaseRecordSchema.extend({
  type: z.literal('protocol_adoption'),
  data: ProtocolAdoptionDataSchema,
});

// ============================================================================
// 6. Invitations & Responses (How we convene)
// ============================================================================

export const InvitationTypeSchema = z.enum([
  'assemble',
  'secretariat_membership',
  'consultant',
  'working_group',
  'observer',
  'emergency',
  'annual',
]);

export const InvitationDataSchema = z.object({
  invitation_type: InvitationTypeSchema,
  invited_participants: z.array(UUIDSchema),
  role: z.string(),
  context: z.record(z.unknown()),
  response_deadline: ISODateTimeSchema,
});

export const InvitationRecordSchema = BaseRecordSchema.extend({
  type: z.literal('invitation'),
  data: InvitationDataSchema,
});

// ---

export const InvitationResponseTypeSchema = z.enum(['accept', 'decline', 'conditional']);

export const InvitationResponseDataSchema = z.object({
  invitation_id: UUIDSchema,
  response: InvitationResponseTypeSchema,
  conditions: z.string().nullable(),
  availability: z.record(z.unknown()),
});

export const InvitationResponseRecordSchema = BaseRecordSchema.extend({
  type: z.literal('invitation_response'),
  data: InvitationResponseDataSchema,
});

// ============================================================================
// 7. Meetings & Assemblies (How we meet)
// ============================================================================

export const AssemblyMinutesDataSchema = z.object({
  invitation_id: UUIDSchema,
  attendees: z.array(UUIDSchema),
  decisions_made: z.array(UUIDSchema),
  action_items: z.array(z.string()),
  deliberation_summary: z.string().optional(),
  next_assembly_date: ISODateTimeSchema.optional(),
});

export const AssemblyMinutesRecordSchema = BaseRecordSchema.extend({
  type: z.literal('assembly_minutes'),
  data: AssemblyMinutesDataSchema,
});

// ============================================================================
// 8. Secretariat Actions (What we allocate)
// ============================================================================

export const AllocationDecisionDataSchema = z.object({
  resources: z.record(z.unknown()),
  from_participant: UUIDSchema,
  to_participant: UUIDSchema,
  recognition_basis: z.record(z.unknown()),
  conditions: z.string(),
});

export const AllocationDecisionRecordSchema = BaseRecordSchema.extend({
  type: z.literal('allocation_decision'),
  data: AllocationDecisionDataSchema,
});

// ============================================================================
// 9. Data Subscriptions (How we stay informed)
// ============================================================================

export const SubscriptionTypeSchema = z.enum([
  'membership',
  'recognition',
  'state',
  'derivation',
  'proposals',
  'decisions',
]);

export const NotificationMethodSchema = z.enum(['webhook', 'poll']);

export const SubscriptionDataSchema = z.object({
  subscription_type: SubscriptionTypeSchema,
  source_entity: UUIDSchema,
  filters: z.record(z.unknown()),
  notification_method: NotificationMethodSchema,
});

export const SubscriptionRecordSchema = BaseRecordSchema.extend({
  type: z.literal('subscription'),
  data: SubscriptionDataSchema,
});

// ---

export const SubscriptionActionSchema = z.enum(['pause', 'resume', 'cancel']);

export const SubscriptionUpdateDataSchema = z.object({
  subscription_id: UUIDSchema,
  action: SubscriptionActionSchema,
  reason: z.string(),
});

export const SubscriptionUpdateRecordSchema = BaseRecordSchema.extend({
  type: z.literal('subscription_update'),
  data: SubscriptionUpdateDataSchema,
});

// ============================================================================
// 10. Derivations & Computations (What we compute)
// ============================================================================

export const RuleTypeSchema = z.enum([
  'mutual_recognition',
  'org_recognition',
  'allocation',
  'filter',
]);

export const DerivationRuleDataSchema = z.object({
  rule_name: z.string(),
  rule_type: RuleTypeSchema,
  algorithm: z.string(),
  parameters: z.record(z.unknown()),
  applies_to: z.array(UUIDSchema),
});

export const DerivationRuleRecordSchema = BaseRecordSchema.extend({
  type: z.literal('derivation_rule'),
  data: DerivationRuleDataSchema,
});

// ---

export const FilterAppliesTypeSchema = z.enum(['recognition', 'state', 'membership']);

export const FilterDefinitionDataSchema = z.object({
  filter_name: z.string(),
  criteria: z.record(z.unknown()),
  applies_to_type: FilterAppliesTypeSchema,
  priority: z.number(),
});

export const FilterDefinitionRecordSchema = BaseRecordSchema.extend({
  type: z.literal('filter_definition'),
  data: FilterDefinitionDataSchema,
});

// ---

export const ComputedResultDataSchema = z.object({
  computation_type: z.string(),
  input_records: z.array(UUIDSchema),
  result_data: z.record(z.unknown()),
  algorithm_version: z.string(),
  computed_at: ISODateTimeSchema,
});

export const ComputedResultRecordSchema = BaseRecordSchema.extend({
  type: z.literal('computed_result'),
  data: ComputedResultDataSchema,
});

// ============================================================================
// 11. Maintenance & Governance (How we evolve)
// ============================================================================

export const AmendmentTypeSchema = z.enum(['correction', 'clarification', 'supersede']);

export const RecordAmendmentDataSchema = z.object({
  original_record: UUIDSchema,
  amendment_type: AmendmentTypeSchema,
  changes: z.record(z.unknown()),
  justification: z.string(),
});

export const RecordAmendmentRecordSchema = BaseRecordSchema.extend({
  type: z.literal('record_amendment'),
  data: RecordAmendmentDataSchema,
});

// ---

export const FrameworkVersionDataSchema = z.object({
  version_id: z.string(),
  changes_from_previous: z.string(),
  adoption_record: UUIDSchema,
  effective_date: ISODateTimeSchema,
});

export const FrameworkVersionRecordSchema = BaseRecordSchema.extend({
  type: z.literal('framework_version'),
  data: FrameworkVersionDataSchema,
});

// ============================================================================
// 12. Validation & Disputes (How we ensure quality)
// ============================================================================

export const ValidationTypeSchema = z.enum(['format', 'logic', 'authority']);
export const ValidationStatusSchema = z.enum(['valid', 'invalid', 'warning']);

export const ValidationReportDataSchema = z.object({
  validated_record: UUIDSchema,
  validation_type: ValidationTypeSchema,
  status: ValidationStatusSchema,
  issues: z.array(z.record(z.unknown())),
});

export const ValidationReportRecordSchema = BaseRecordSchema.extend({
  type: z.literal('validation_report'),
  data: ValidationReportDataSchema,
});

// ---

export const DisputeTypeSchema = z.enum(['factual', 'procedural', 'interpretive']);

export const DisputeDataSchema = z.object({
  disputed_record: UUIDSchema,
  dispute_type: DisputeTypeSchema,
  complainant: UUIDSchema,
  grounds: z.string(),
  proposed_resolution: z.record(z.unknown()),
});

export const DisputeRecordSchema = BaseRecordSchema.extend({
  type: z.literal('dispute'),
  data: DisputeDataSchema,
});

// ---

export const ResolutionTypeSchema = z.enum(['accepted', 'modified', 'rejected', 'referred']);

export const DisputeResolutionDataSchema = z.object({
  dispute_id: UUIDSchema,
  resolution_type: ResolutionTypeSchema,
  resolution_details: z.record(z.unknown()),
  decided_by: z.array(UUIDSchema),
});

export const DisputeResolutionRecordSchema = BaseRecordSchema.extend({
  type: z.literal('dispute_resolution'),
  data: DisputeResolutionDataSchema,
});

// ============================================================================
// Union of All Record Types
// ============================================================================

/**
 * Discriminated union of all possible record types
 * Enables exhaustive type checking and runtime validation
 */
export const RecordSchema = z.discriminatedUnion('type', [
  // 1. Identity & Membership
  MembershipUpdateRecordSchema,
  RegistryEntryRecordSchema,
  ContactInfoRecordSchema,
  
  // 2. Recognition & Relationships
  RecognitionDistributionRecordSchema,
  
  // 3. State Declarations
  StateDeclarationRecordSchema,
  CapacityOfferRecordSchema,
  
  // 4. Proposals & Expressions
  ProposalRecordSchema,
  StatementRecordSchema,
  
  // 5. Decision-Making
  PositionRecordSchema,
  SupportExpressionRecordSchema,
  DecisionOutcomeRecordSchema,
  ProtocolAdoptionRecordSchema,
  
  // 6. Invitations & Responses
  InvitationRecordSchema,
  InvitationResponseRecordSchema,
  
  // 7. Meetings & Assemblies
  AssemblyMinutesRecordSchema,
  
  // 8. Secretariat Actions
  AllocationDecisionRecordSchema,
  
  // 9. Data Subscriptions
  SubscriptionRecordSchema,
  SubscriptionUpdateRecordSchema,
  
  // 10. Derivations & Computations
  DerivationRuleRecordSchema,
  FilterDefinitionRecordSchema,
  ComputedResultRecordSchema,
  
  // 11. Maintenance & Governance
  RecordAmendmentRecordSchema,
  FrameworkVersionRecordSchema,
  
  // 12. Validation & Disputes
  ValidationReportRecordSchema,
  DisputeRecordSchema,
  DisputeResolutionRecordSchema,
]);

// ============================================================================
// TypeScript Type Exports
// ============================================================================

export type UUID = z.infer<typeof UUIDSchema>;
export type ISODateTime = z.infer<typeof ISODateTimeSchema>;
export type RecordStatus = z.infer<typeof RecordStatusSchema>;
export type BaseRecord = z.infer<typeof BaseRecordSchema>;

// 1. Identity & Membership
export type MembershipAction = z.infer<typeof MembershipActionSchema>;
export type MembershipUpdateData = z.infer<typeof MembershipUpdateDataSchema>;
export type MembershipUpdateRecord = z.infer<typeof MembershipUpdateRecordSchema>;

export type RegistryType = z.infer<typeof RegistryTypeSchema>;
export type RegistryAction = z.infer<typeof RegistryActionSchema>;
export type RegistryEntryData = z.infer<typeof RegistryEntryDataSchema>;
export type RegistryEntryRecord = z.infer<typeof RegistryEntryRecordSchema>;

export type VerificationMethod = z.infer<typeof VerificationMethodSchema>;
export type ContactInfoData = z.infer<typeof ContactInfoDataSchema>;
export type ContactInfoRecord = z.infer<typeof ContactInfoRecordSchema>;

// 2. Recognition & Relationships
export type RecognitionType = z.infer<typeof RecognitionTypeSchema>;
export type RecognitionDistributionData = z.infer<typeof RecognitionDistributionDataSchema>;
export type RecognitionDistributionRecord = z.infer<typeof RecognitionDistributionRecordSchema>;

// 3. State Declarations
export type StateCategory = z.infer<typeof StateCategorySchema>;
export type StateDeclarationData = z.infer<typeof StateDeclarationDataSchema>;
export type StateDeclarationRecord = z.infer<typeof StateDeclarationRecordSchema>;

export type CapacityOfferData = z.infer<typeof CapacityOfferDataSchema>;
export type CapacityOfferRecord = z.infer<typeof CapacityOfferRecordSchema>;

// 4. Proposals & Expressions
export type ProposalData = z.infer<typeof ProposalDataSchema>;
export type ProposalRecord = z.infer<typeof ProposalRecordSchema>;

export type StatementType = z.infer<typeof StatementTypeSchema>;
export type StatementData = z.infer<typeof StatementDataSchema>;
export type StatementRecord = z.infer<typeof StatementRecordSchema>;

// 5. Decision-Making
export type PositionType = z.infer<typeof PositionTypeSchema>;
export type PositionData = z.infer<typeof PositionDataSchema>;
export type PositionRecord = z.infer<typeof PositionRecordSchema>;

export type SupportExpressionData = z.infer<typeof SupportExpressionDataSchema>;
export type SupportExpressionRecord = z.infer<typeof SupportExpressionRecordSchema>;

export type DecisionOutcome = z.infer<typeof DecisionOutcomeSchema>;
export type DecisionOutcomeData = z.infer<typeof DecisionOutcomeDataSchema>;
export type DecisionOutcomeRecord = z.infer<typeof DecisionOutcomeRecordSchema>;

export type ProtocolAdoptionData = z.infer<typeof ProtocolAdoptionDataSchema>;
export type ProtocolAdoptionRecord = z.infer<typeof ProtocolAdoptionRecordSchema>;

// 6. Invitations & Responses
export type InvitationType = z.infer<typeof InvitationTypeSchema>;
export type InvitationData = z.infer<typeof InvitationDataSchema>;
export type InvitationRecord = z.infer<typeof InvitationRecordSchema>;

export type InvitationResponseType = z.infer<typeof InvitationResponseTypeSchema>;
export type InvitationResponseData = z.infer<typeof InvitationResponseDataSchema>;
export type InvitationResponseRecord = z.infer<typeof InvitationResponseRecordSchema>;

// 7. Meetings & Assemblies
export type AssemblyMinutesData = z.infer<typeof AssemblyMinutesDataSchema>;
export type AssemblyMinutesRecord = z.infer<typeof AssemblyMinutesRecordSchema>;

// 8. Secretariat Actions
export type AllocationDecisionData = z.infer<typeof AllocationDecisionDataSchema>;
export type AllocationDecisionRecord = z.infer<typeof AllocationDecisionRecordSchema>;

// 9. Data Subscriptions
export type SubscriptionType = z.infer<typeof SubscriptionTypeSchema>;
export type NotificationMethod = z.infer<typeof NotificationMethodSchema>;
export type SubscriptionData = z.infer<typeof SubscriptionDataSchema>;
export type SubscriptionRecord = z.infer<typeof SubscriptionRecordSchema>;

export type SubscriptionAction = z.infer<typeof SubscriptionActionSchema>;
export type SubscriptionUpdateData = z.infer<typeof SubscriptionUpdateDataSchema>;
export type SubscriptionUpdateRecord = z.infer<typeof SubscriptionUpdateRecordSchema>;

// 10. Derivations & Computations
export type RuleType = z.infer<typeof RuleTypeSchema>;
export type DerivationRuleData = z.infer<typeof DerivationRuleDataSchema>;
export type DerivationRuleRecord = z.infer<typeof DerivationRuleRecordSchema>;

export type FilterAppliesType = z.infer<typeof FilterAppliesTypeSchema>;
export type FilterDefinitionData = z.infer<typeof FilterDefinitionDataSchema>;
export type FilterDefinitionRecord = z.infer<typeof FilterDefinitionRecordSchema>;

export type ComputedResultData = z.infer<typeof ComputedResultDataSchema>;
export type ComputedResultRecord = z.infer<typeof ComputedResultRecordSchema>;

// 11. Maintenance & Governance
export type AmendmentType = z.infer<typeof AmendmentTypeSchema>;
export type RecordAmendmentData = z.infer<typeof RecordAmendmentDataSchema>;
export type RecordAmendmentRecord = z.infer<typeof RecordAmendmentRecordSchema>;

export type FrameworkVersionData = z.infer<typeof FrameworkVersionDataSchema>;
export type FrameworkVersionRecord = z.infer<typeof FrameworkVersionRecordSchema>;

// 12. Validation & Disputes
export type ValidationType = z.infer<typeof ValidationTypeSchema>;
export type ValidationStatus = z.infer<typeof ValidationStatusSchema>;
export type ValidationReportData = z.infer<typeof ValidationReportDataSchema>;
export type ValidationReportRecord = z.infer<typeof ValidationReportRecordSchema>;

export type DisputeType = z.infer<typeof DisputeTypeSchema>;
export type DisputeData = z.infer<typeof DisputeDataSchema>;
export type DisputeRecord = z.infer<typeof DisputeRecordSchema>;

export type ResolutionType = z.infer<typeof ResolutionTypeSchema>;
export type DisputeResolutionData = z.infer<typeof DisputeResolutionDataSchema>;
export type DisputeResolutionRecord = z.infer<typeof DisputeResolutionRecordSchema>;

// Union Type
export type Record = z.infer<typeof RecordSchema>;

// ============================================================================
// Utility Functions
// ============================================================================

/**
 * Validates a record against the appropriate schema
 * @param record - The record to validate
 * @returns Validated and typed record
 * @throws ZodError if validation fails
 */
export function validateRecord(record: unknown): Record {
  return RecordSchema.parse(record);
}

/**
 * Safely validates a record, returning success/error result
 * @param record - The record to validate
 * @returns SafeParseReturnType with either success data or error
 */
export function safeValidateRecord(record: unknown) {
  return RecordSchema.safeParse(record);
}

/**
 * Type guard to check if a record is of a specific type
 * @param record - The record to check
 * @param type - The expected record type
 * @returns Type predicate
 */
export function isRecordType<T extends Record['type']>(
  record: Record,
  type: T
): record is Extract<Record, { type: T }> {
  return record.type === type;
}

/**
 * Extract all records of a specific type from an array
 * @param records - Array of records
 * @param type - The record type to filter
 * @returns Filtered and typed array
 */
export function filterRecordsByType<T extends Record['type']>(
  records: Record[],
  type: T
): Extract<Record, { type: T }>[] {
  return records.filter((r): r is Extract<Record, { type: T }> => r.type === type);
}

