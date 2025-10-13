# Quick Start: Typed Flows

## 5-Minute Setup

### 1. Install Dependencies

```bash
cd server/llm-router
bun install
```

### 2. Start Server

```bash
bun start
```

The server will start on `http://localhost:8768`

## Basic Usage

### List Available Flows

```bash
curl http://localhost:8768/flows
```

### Send a Request

#### Example 1: Recognition Analysis

```bash
curl -X POST http://localhost:8768/route \
  -H "Content-Type: application/json" \
  -d '{
    "requestType": "recognition-analysis",
    "players": [
      {
        "id": "alice",
        "name": "Alice",
        "recognitions": {"bob": 30, "charlie": 20}
      },
      {
        "id": "bob",
        "name": "Bob",
        "recognitions": {"alice": 40, "charlie": 10}
      }
    ],
    "analysisType": "mutual-recognition"
  }'
```

#### Example 2: Code Generation

```bash
curl -X POST http://localhost:8768/route \
  -H "Content-Type: application/json" \
  -d '{
    "requestType": "code-generation",
    "language": "typescript",
    "task": "Create a function to calculate mutual recognition between players",
    "maxTokens": 1500
  }'
```

#### Example 3: Chat (Simple)

```bash
curl -X POST http://localhost:8768/route \
  -H "Content-Type: application/json" \
  -d '{
    "requestType": "chat",
    "messages": [
      {"role": "user", "content": "Explain mutual recognition"}
    ]
  }'
```

## Creating Your First Flow

### Step 1: Define Zod Schema

Edit `schemas/requestTypes.ts`:

```typescript
export const MyFlowSchema = z.object({
  requestType: z.literal('my-flow'),
  input: z.string(),
  options: z.array(z.string()).optional()
});

export type MyFlowRequest = z.infer<typeof MyFlowSchema>;
```

### Step 2: Add to Request Union

```typescript
export const RequestSchema = z.discriminatedUnion('requestType', [
  // ... existing schemas
  MyFlowSchema  // Add here
]).or(ChatCompletionSchema).or(BaseRequestSchema);
```

### Step 3: Create Flow Definition

Edit `flows/flowRegistry.ts`:

```typescript
export const myFlow: FlowDefinition = {
  name: 'My Custom Flow',
  requestType: 'my-flow',
  description: 'Does something cool',
  preferredModels: ['openai/gpt-4', 'anthropic/claude-3-sonnet'],
  
  promptTemplate: (request: MyFlowRequest) => ({
    system: `You are an expert at ${request.input}`,
    user: `Process this: ${request.input}
    
    Options: ${request.options?.join(', ') || 'none'}`,
    temperature: 0.7,
    maxTokens: 1000
  })
};
```

### Step 4: Register Flow

```typescript
export const flowRegistry: Record<string, FlowDefinition> = {
  // ... existing flows
  'my-flow': myFlow  // Add here
};
```

### Step 5: Test It

```bash
curl -X POST http://localhost:8768/route \
  -H "Content-Type: application/json" \
  -d '{
    "requestType": "my-flow",
    "input": "analyzing data patterns",
    "options": ["detailed", "visual"]
  }'
```

## Common Patterns

### Pattern 1: Free-Association Flows

For flows specific to the free-association system:

```typescript
export const myFreeAssociationFlow: FlowDefinition = {
  name: 'Recognition Pattern Analysis',
  requestType: 'recognition-patterns',
  preferredModels: ['anthropic/claude-3-opus'], // Best reasoning
  
  promptTemplate: (request) => ({
    system: `You are an expert in free-association networks.
    
    Key concepts:
    - Mutual recognition = min(A→B, B→A)
    - Total recognition per person = 100%
    - Capacity shares flow proportionally`,
    
    user: `Analyze this pattern: ${JSON.stringify(request.data, null, 2)}`,
    temperature: 0.3, // Low for analytical
    maxTokens: 1200
  })
};
```

### Pattern 2: Cost-Optimized Flows

For simple tasks where cost matters:

```typescript
export const summarizationFlow: FlowDefinition = {
  name: 'Text Summarization',
  requestType: 'summarize',
  preferredModels: [
    'openai/gpt-3.5-turbo',     // Cheapest
    'anthropic/claude-3-haiku'  // Fast fallback
  ],
  
  promptTemplate: (request) => ({
    user: `Summarize in 2-3 sentences: ${request.text}`,
    temperature: 0.5,
    maxTokens: 150 // Keep it short
  })
};
```

### Pattern 3: LangChain Integration

For advanced prompt chaining:

```typescript
import { PromptTemplate } from '@langchain/core/prompts';

export const advancedFlow: FlowDefinition = {
  name: 'Advanced Analysis',
  requestType: 'advanced-analysis',
  preferredModels: ['openai/gpt-4'],
  
  promptTemplate: async (request) => {
    const template = PromptTemplate.fromTemplate(`
      Context: {context}
      
      Question: {question}
      
      Provide analysis in the following format:
      1. Summary
      2. Key Points (bullet list)
      3. Recommendations
    `);
    
    const formatted = await template.format({
      context: request.context,
      question: request.question
    });
    
    return {
      user: formatted,
      temperature: 0.4,
      maxTokens: 2000
    };
  }
};
```

## Docker Usage

### Build Image

```bash
cd server/llm-router
docker build -t llm-router-typed .
```

### Run Container

```bash
docker run -p 8768:8768 \
  -e KEY_POOL_URL=http://key-pool:8769 \
  llm-router-typed
```

### With Docker Compose

The `docker-compose.yml` in `server/` already includes the typed router:

```bash
cd server
docker-compose up llm-router
```

## Integration with AI Proxy

The AI proxy automatically uses the typed router. No changes needed!

Example from `ai-proxy/server.js`:

```javascript
const routingResponse = await fetch(`${LLM_ROUTER_URL}/route`, {
  method: 'POST',
  headers: { 'Content-Type': 'application/json' },
  body: JSON.stringify({
    requestType: 'recognition-analysis', // Use typed flows!
    players: [...],
    analysisType: 'mutual-recognition'
  })
});
```

## Response Format

All routing responses follow this structure:

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
    "system": "You are an expert...",
    "user": "Analyze...",
    "temperature": 0.3,
    "maxTokens": 1000
  }
}
```

The AI proxy uses this to make the actual LLM call.

## Debugging

### Enable Detailed Logging

The server logs all routing decisions:

```
Using flow: Recognition Analysis for request type: recognition-analysis
Selected model: anthropic/claude-3-opus
```

### Validate Request Structure

If your request is invalid, you'll get a detailed Zod error:

```json
{
  "error": "Routing failed",
  "message": "Invalid request: Expected literal value 'recognition-analysis', received 'recogntion-analysis'"
}
```

### Test Individual Components

```typescript
// Test schema validation
import { RequestSchema } from './schemas/requestTypes';

const result = RequestSchema.safeParse(myRequest);
console.log(result.success, result.error);

// Test flow selection
import { getFlow } from './flows/flowRegistry';

const flow = getFlow('recognition-analysis');
console.log(flow.preferredModels);

// Test prompt generation
const prompt = flow.promptTemplate(myRequest);
console.log(prompt);
```

## Next Steps

1. **Read the Full Guide**: See `FLOWS_GUIDE.md` for comprehensive documentation
2. **Check Examples**: See `example-requests.json` for real-world requests
3. **Explore Flows**: Read flow definitions in `flows/flowRegistry.ts`
4. **Build Custom Flows**: Start with the patterns above
5. **Integrate with Frontend**: Export types for client-side use

## Need Help?

- **Architecture**: See `TYPED_FLOWS_SUMMARY.md`
- **API Reference**: See `README.md`
- **Custom Flows**: See `FLOWS_GUIDE.md`
- **Issues**: Check TypeScript/Zod error messages (they're very helpful!)

