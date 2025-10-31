import { z } from 'zod';

/**
 * Message schema for OpenRouter chat completions
 * OpenRouter uses OpenAI-compatible format
 */
export const MessageSchema = z.object({
  role: z.enum(['system', 'user', 'assistant', 'function']),
  content: z.string(),
  name: z.string().optional(),
  function_call: z.any().optional()
});

export type Message = z.infer<typeof MessageSchema>;

/**
 * Base Completion Request Schema
 * OpenRouter-compatible (OpenAI format)
 */
export const BaseCompletionRequestSchema = z.object({
  // Either messages or prompt must be provided
  messages: z.array(MessageSchema).optional(),
  prompt: z.string().optional(),
  
  // Model selection (OpenRouter model names: provider/model-name)
  model: z.string().optional(),
  
  // Request type for custom flow routing
  requestType: z.string().optional(),
  
  // Common OpenRouter/OpenAI parameters
  maxTokens: z.number().int().positive().optional(),
  max_tokens: z.number().int().positive().optional(), // Alternative naming
  temperature: z.number().min(0).max(2).optional().default(0.7),
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

export type BaseCompletionRequest = z.infer<typeof BaseCompletionRequestSchema>;

/**
 * Extended request schema for typed flows
 * Allows domain-specific fields while maintaining base compatibility
 */
export const TypedFlowFieldsSchema = z.object({
  // Free-association specific fields (optional, for typed flows)
  players: z.array(z.object({
    id: z.string(),
    name: z.string(),
    recognitions: z.record(z.string(), z.number())
  })).optional(),
  
  analysisType: z.enum(['mutual-recognition', 'capacity-distribution', 'network-health']).optional(),
  
  player: z.object({
    id: z.string(),
    name: z.string(),
    surplusCapacities: z.array(z.object({
      type: z.string(),
      quantity: z.number(),
      description: z.string()
    }))
  }).optional(),
  
  contributors: z.array(z.object({
    id: z.string(),
    name: z.string(),
    mutualRecognition: z.number(),
    needs: z.array(z.string())
  })).optional(),
  
  // Code generation fields
  language: z.string().optional(),
  task: z.string().optional(),
  context: z.string().optional(),
  
  // Data analysis fields
  data: z.any().optional(),
  question: z.string().optional(),
  
  // Quest generation fields
  recognitionTree: z.any().optional(), // RootNodeSchema from v5/schemas
  capacities: z.array(z.any()).optional(), // AvailabilitySlotSchema array
  needs: z.array(z.any()).optional(), // NeedSlotSchema array
  locations: z.array(z.object({
    city: z.string().optional(),
    state_province: z.string().optional(),
    country: z.string().optional(),
    latitude: z.number().optional(),
    longitude: z.number().optional(),
    online: z.boolean().optional()
  })).optional(),
  peerQuests: z.array(z.any()).optional(), // QuestSchema array
  maxQuests: z.number().int().positive().optional(),
  preferredTypes: z.array(z.string()).optional()
});

export const ExtendedCompletionRequestSchema = BaseCompletionRequestSchema.and(TypedFlowFieldsSchema);

export type ExtendedCompletionRequest = z.infer<typeof ExtendedCompletionRequestSchema>;

/**
 * Auth Token Request Schema
 */
export const TokenRequestSchema = z.object({
  apiKey: z.string().min(1),
  userId: z.string().optional()
});

export type TokenRequest = z.infer<typeof TokenRequestSchema>;

/**
 * AI Completion Request Schema (for ai-proxy)
 */
export const CompletionRequestSchema = BaseCompletionRequestSchema;
export type CompletionRequest = z.infer<typeof CompletionRequestSchema>;

