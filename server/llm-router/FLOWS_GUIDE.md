# LLM Router Typed Flows Guide

## Overview

The typed flow system allows you to define **custom request types** with **Zod schemas**, map them to **specific models**, and wrap them in **structured prompts**—all in TypeScript for full type safety.

## Architecture

```
Request → Zod Validation → Flow Selection → Prompt Template → Model Selection → Key Pool → Response
```

### Components

1. **Schemas** (`schemas/requestTypes.ts`) - Zod schemas defining request structure
2. **Flows** (`flows/flowRegistry.ts`) - Flow definitions with prompts and model preferences
3. **Router** (`router.ts`) - Orchestrates validation, flow selection, and routing
4. **Server** (`server-typed.ts`) - Express server exposing the typed routing API

## Creating a New Flow

### Step 1: Define Zod Schema

Edit `schemas/requestTypes.ts`:

```typescript
export const MyCustomFlowSchema = z.object({
  requestType: z.literal('my-custom-flow'),
  customField: z.string(),
  options: z.array(z.string()),
  maxTokens: z.number().optional().default(1000)
});

export type MyCustomFlowRequest = z.infer<typeof MyCustomFlowSchema>;
```

### Step 2: Add to Request Union

```typescript
export const RequestSchema = z.discriminatedUnion('requestType', [
  RecognitionAnalysisSchema,
  // ... other schemas
  MyCustomFlowSchema  // Add here
]).or(ChatCompletionSchema).or(BaseRequestSchema);
```

### Step 3: Create Flow Definition

Edit `flows/flowRegistry.ts`:

```typescript
export const myCustomFlow: FlowDefinition = {
  name: 'My Custom Flow',
  requestType: 'my-custom-flow',
  description: 'Does something specific and awesome',
  preferredModels: [
    'openai/gpt-4',                    // Try this first
    'anthropic/claude-3-sonnet',       // Then this
    'openai/gpt-3.5-turbo'            // Fallback
  ],
  
  promptTemplate: (request: MyCustomFlowRequest) => ({
    system: `You are an expert at ${request.customField}`,
    user: `Process these options: ${request.options.join(', ')}
    
    Provide detailed analysis and recommendations.`,
    temperature: 0.5,
    maxTokens: request.maxTokens
  }),
  
  postProcess: (response: any) => {
    // Optional: transform response
    return {
      ...response,
      customMetadata: 'added by post-processing'
    };
  }
};
```

### Step 4: Register Flow

```typescript
export const flowRegistry: Record<string, FlowDefinition> = {
  'recognition-analysis': recognitionAnalysisFlow,
  // ... other flows
  'my-custom-flow': myCustomFlow  // Add here
};
```

## Example Flows

### 1. Recognition Analysis Flow

**Request:**
```json
{
  "requestType": "recognition-analysis",
  "players": [
    {
      "id": "alice",
      "name": "Alice",
      "recognitions": {
        "bob": 30,
        "charlie": 20
      }
    },
    {
      "id": "bob",
      "name": "Bob",
      "recognitions": {
        "alice": 40,
        "charlie": 10
      }
    }
  ],
  "analysisType": "mutual-recognition"
}
```

**What it does:**
- Uses Claude 3 Opus (best for reasoning)
- Wraps request in free-association expertise prompt
- Calculates mutual recognition values
- Provides network analysis and recommendations

### 2. Capacity Recommendation Flow

**Request:**
```json
{
  "requestType": "capacity-recommendation",
  "player": {
    "id": "alice",
    "name": "Alice",
    "surplusCapacities": [
      {
        "type": "housing",
        "quantity": 2,
        "description": "2 spare bedrooms"
      }
    ]
  },
  "contributors": [
    {
      "id": "bob",
      "name": "Bob",
      "mutualRecognition": 35,
      "needs": ["housing", "workspace"]
    }
  ]
}
```

**What it does:**
- Uses Claude 3 Sonnet (balanced for recommendations)
- Analyzes capacity-need matches
- Considers mutual recognition strength
- Suggests optimal distribution

### 3. Code Generation Flow

**Request:**
```json
{
  "requestType": "code-generation",
  "language": "typescript",
  "task": "Create a function to calculate mutual recognition",
  "context": "Part of free-association system",
  "maxTokens": 2000
}
```

**What it does:**
- Uses GPT-4 (excellent for code)
- Low temperature (0.2) for deterministic output
- Includes error handling and examples
- Follows language best practices

## Using Flows

### Via HTTP

```bash
curl -X POST http://localhost:8768/route \
  -H "Content-Type: application/json" \
  -d '{
    "requestType": "recognition-analysis",
    "players": [...],
    "analysisType": "mutual-recognition"
  }'
```

**Response:**
```json
{
  "success": true,
  "flow": {
    "name": "Recognition Analysis",
    "requestType": "recognition-analysis",
    "description": "Analyzes free-association recognition networks"
  },
  "model": "anthropic/claude-3-opus",
  "provider": "openrouter",
  "key": "sk-or-v1-...",
  "promptConfig": {
    "system": "You are an expert in...",
    "user": "Analyze the mutual recognition...",
    "temperature": 0.3,
    "maxTokens": 1000
  }
}
```

### List Available Flows

```bash
curl http://localhost:8768/flows
```

## Flow Configuration Options

### Prompt Template

```typescript
promptTemplate: (request: MyRequest) => ({
  system: 'System prompt (optional)',      // Sets AI role/context
  user: 'User prompt (required)',          // The actual request
  temperature: 0.7,                        // 0.0-1.0, higher = more creative
  maxTokens: 1000                         // Response length limit
})
```

### Model Preferences

Models are tried in order until a healthy key is found:

```typescript
preferredModels: [
  'anthropic/claude-3-opus',    // Best reasoning, highest cost
  'openai/gpt-4-turbo',         // Fast, good reasoning
  'openai/gpt-3.5-turbo'       // Cheapest, good for simple tasks
]
```

### Post-Processing

Optional function to transform the LLM response:

```typescript
postProcess: (response: any) => {
  // Extract structured data
  const matches = response.match(/Score: (\d+)/);
  return {
    ...response,
    extractedScore: matches ? parseInt(matches[1]) : null
  };
}
```

## Advanced: LangChain Integration

You can use LangChain for more complex flows:

```typescript
import { ChatOpenAI } from '@langchain/openai';
import { PromptTemplate } from '@langchain/core/prompts';

export const langchainFlow: FlowDefinition = {
  name: 'LangChain Flow',
  requestType: 'langchain-example',
  description: 'Uses LangChain for advanced prompting',
  preferredModels: ['openai/gpt-4'],
  
  promptTemplate: async (request: any) => {
    // Use LangChain's prompt templates
    const template = PromptTemplate.fromTemplate(`
      You are analyzing: {topic}
      
      Context: {context}
      
      Provide: {output_format}
    `);
    
    const formatted = await template.format({
      topic: request.topic,
      context: request.context,
      output_format: 'JSON with analysis and recommendations'
    });
    
    return {
      user: formatted,
      temperature: 0.4,
      maxTokens: 1500
    };
  }
};
```

## Type Safety Benefits

1. **Request Validation** - Invalid requests rejected before routing
2. **IDE Autocomplete** - Full IntelliSense for request fields
3. **Compile-Time Checks** - Catch errors before deployment
4. **Refactoring Safety** - Rename fields across entire codebase

## Testing Flows

```typescript
// test-flow.ts
import { routeRequest } from './router';

const testRequest = {
  requestType: 'recognition-analysis',
  players: [/* ... */],
  analysisType: 'mutual-recognition'
};

const result = await routeRequest(testRequest);
console.log('Selected model:', result.model);
console.log('Prompt:', result.promptConfig);
```

## Production Tips

1. **Add flows incrementally** - Start with a few key flows
2. **Monitor flow usage** - Track which flows are most used
3. **Optimize model selection** - Adjust based on cost/quality tradeoffs
4. **Cache common prompts** - For frequently-used templates
5. **Version your schemas** - Use Zod transforms for backwards compatibility

## Next Steps

- Add more free-association specific flows
- Integrate with frontend type definitions
- Add flow analytics and monitoring
- Create flow marketplace for community contributions

