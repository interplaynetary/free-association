import { z } from 'zod';

/**
 * Message schema for chat completions
 */
export const MessageSchema = z.object({
  role: z.enum(['system', 'user', 'assistant', 'function']),
  content: z.string(),
  name: z.string().optional(),
  function_call: z.any().optional()
});

export type Message = z.infer<typeof MessageSchema>;

/**
 * AI Completion Request Schema
 * Supports both chat (messages) and completion (prompt) formats
 */
export const CompletionRequestSchema = z.object({
  // Either messages or prompt must be provided
  messages: z.array(MessageSchema).optional(),
  prompt: z.string().optional(),
  
  // Optional parameters
  maxTokens: z.number().int().positive().optional().default(1024),
  max_tokens: z.number().int().positive().optional(), // Alternative naming
  temperature: z.number().min(0).max(2).optional().default(0.7),
  model: z.string().optional(),
  requestType: z.string().optional(), // For typed flow routing
  
  // Additional LLM parameters
  top_p: z.number().min(0).max(1).optional(),
  frequency_penalty: z.number().min(-2).max(2).optional(),
  presence_penalty: z.number().min(-2).max(2).optional(),
  stop: z.union([z.string(), z.array(z.string())]).optional(),
  n: z.number().int().positive().optional(),
  stream: z.boolean().optional(),
  logit_bias: z.record(z.string(), z.number()).optional(),  
  user: z.string().optional()
}).refine(
  (data) => data.messages || data.prompt,
  { message: 'Either messages or prompt must be provided' }
);

export type CompletionRequest = z.infer<typeof CompletionRequestSchema>;

/**
 * Auth Token Request Schema
 */
export const TokenRequestSchema = z.object({
  apiKey: z.string().min(1),
  userId: z.string().optional()
});

export type TokenRequest = z.infer<typeof TokenRequestSchema>;

/**
 * Routing Response Schema (from LLM Router)
 */
export const RoutingResponseSchema = z.object({
  success: z.boolean(),
  model: z.string(),
  provider: z.string(),
  key: z.string(),
  flow: z.object({
    name: z.string(),
    requestType: z.string(),
    description: z.string()
  }).optional(),
  promptConfig: z.object({
    system: z.string().optional(),
    user: z.string(),
    temperature: z.number().optional(),
    maxTokens: z.number().optional()
  }).optional(),
  selection: z.object({
    category: z.string(),
    reason: z.string()
  }).optional()
});

export type RoutingResponse = z.infer<typeof RoutingResponseSchema>;

/**
 * Health Report Schema (to Key Pool)
 */
export const HealthReportSchema = z.object({
  key: z.string(),
  status: z.enum(['healthy', 'degraded', 'failed', 'rate_limited', 'depleted']),
  error: z.string().nullable().optional(),
  cost: z.number().nullable().optional()
});

export type HealthReport = z.infer<typeof HealthReportSchema>;

