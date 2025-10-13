import { z } from 'zod';

/**
 * Base request schema - all requests must have at least these fields
 */
export const BaseRequestSchema = z.object({
  requestType: z.string().optional(), // Custom request type for routing
  prompt: z.string().optional(),
  messages: z.array(z.object({
    role: z.enum(['system', 'user', 'assistant']),
    content: z.string()
  })).optional(),
  maxTokens: z.number().optional(),
  temperature: z.number().optional()
});

/**
 * Free Association specific request types
 */

// Recognition analysis request
export const RecognitionAnalysisSchema = z.object({
  requestType: z.literal('recognition-analysis'),
  players: z.array(z.object({
    id: z.string(),
    name: z.string(),
    recognitions: z.record(z.string(), z.number()) // player_id -> percentage
  })),
  analysisType: z.enum(['mutual-recognition', 'capacity-distribution', 'network-health']),
  maxTokens: z.number().optional().default(1000)
});

// Capacity recommendation request
export const CapacityRecommendationSchema = z.object({
  requestType: z.literal('capacity-recommendation'),
  player: z.object({
    id: z.string(),
    name: z.string(),
    surplusCapacities: z.array(z.object({
      type: z.string(),
      quantity: z.number(),
      description: z.string()
    }))
  }),
  contributors: z.array(z.object({
    id: z.string(),
    name: z.string(),
    mutualRecognition: z.number(),
    needs: z.array(z.string())
  })),
  maxTokens: z.number().optional().default(800)
});

// General chat/completion request
export const ChatCompletionSchema = z.object({
  requestType: z.literal('chat').optional(),
  messages: z.array(z.object({
    role: z.enum(['system', 'user', 'assistant']),
    content: z.string()
  })),
  maxTokens: z.number().optional().default(1024),
  temperature: z.number().optional().default(0.7)
});

// Code generation request
export const CodeGenerationSchema = z.object({
  requestType: z.literal('code-generation'),
  language: z.string(),
  task: z.string(),
  context: z.string().optional(),
  maxTokens: z.number().optional().default(2000)
});

// Data analysis request
export const DataAnalysisSchema = z.object({
  requestType: z.literal('data-analysis'),
  data: z.any(), // Can be JSON, CSV, etc
  question: z.string(),
  analysisType: z.enum(['summary', 'trend', 'anomaly', 'comparison']),
  maxTokens: z.number().optional().default(1500)
});

/**
 * Union of all supported request types
 */
export const RequestSchema = z.discriminatedUnion('requestType', [
  RecognitionAnalysisSchema,
  CapacityRecommendationSchema,
  CodeGenerationSchema,
  DataAnalysisSchema
]).or(ChatCompletionSchema).or(BaseRequestSchema);

export type RequestType = z.infer<typeof RequestSchema>;
export type RecognitionAnalysisRequest = z.infer<typeof RecognitionAnalysisSchema>;
export type CapacityRecommendationRequest = z.infer<typeof CapacityRecommendationSchema>;
export type CodeGenerationRequest = z.infer<typeof CodeGenerationSchema>;
export type DataAnalysisRequest = z.infer<typeof DataAnalysisSchema>;
export type ChatCompletionRequest = z.infer<typeof ChatCompletionSchema>;

