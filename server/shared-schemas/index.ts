/**
 * Shared Zod schemas for Free Association services
 * OpenRouter-first architecture
 */

// Completion schemas
export {
  MessageSchema,
  BaseCompletionRequestSchema,
  ExtendedCompletionRequestSchema,
  TokenRequestSchema,
  type Message,
  type BaseCompletionRequest,
  type ExtendedCompletionRequest,
  type TokenRequest
} from './completion';

// Routing schemas
export {
  FlowMetadataSchema,
  PromptConfigSchema,
  RoutingResponseSchema,
  KeyHealthStatus,
  HealthReportSchema,
  KeyResponseSchema,
  type FlowMetadata,
  type PromptConfig,
  type RoutingResponse,
  type KeyHealthStatus as KeyHealthStatusType,
  type HealthReport,
  type KeyResponse
} from './routing';

