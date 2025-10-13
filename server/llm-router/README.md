# LLM Router Service

Intelligent request router with **typed flows** using TypeScript, Zod schemas, and LangChain patterns. Define custom request types, map them to models, and wrap them in structured prompts—all type-safe.

## Features

- 🎯 **Typed Request Flows** - Define custom request types with Zod schemas
- 📝 **Dynamic Prompt Templates** - Map request types to structured prompts
- 🧠 **Intelligent Model Selection** - Per-flow model preferences with automatic failover
- ✅ **Type Safety** - Full TypeScript support with compile-time validation
- 🔄 **Extensible** - Add new flows without modifying core routing logic
- 🔑 **Key Pool Integration** - Queries key-pool service for healthy API keys
- 📈 **Health Reporting** - Reports key performance back to key pool
- 🦙 **LangChain Ready** - Built-in support for LangChain prompt templates

## Configuration

Environment variables:
- `LLM_ROUTER_HOST` - Bind host (default: 0.0.0.0)
- `LLM_ROUTER_PORT` - Port (default: 8768)
- `KEY_POOL_URL` - Key pool service URL (default: http://key-pool:8769)

## Endpoints

### POST /route
Route a typed request to the optimal LLM provider.

**Example: Recognition Analysis**
```json
{
  "requestType": "recognition-analysis",
  "players": [
    {
      "id": "alice",
      "name": "Alice",
      "recognitions": { "bob": 30, "charlie": 20 }
    },
    {
      "id": "bob",
      "name": "Bob",
      "recognitions": { "alice": 40, "charlie": 10 }
    }
  ],
  "analysisType": "mutual-recognition"
}
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
    "system": "You are an expert in analyzing free-association networks...",
    "user": "Analyze the mutual recognition network...",
    "temperature": 0.3,
    "maxTokens": 1000
  }
}
```

### GET /flows
List all available typed flows.

**Response:**
```json
{
  "flows": [
    {
      "name": "Recognition Analysis",
      "requestType": "recognition-analysis",
      "description": "Analyzes free-association recognition networks",
      "preferredModels": ["anthropic/claude-3-opus", "openai/gpt-4"]
    },
    ...
  ],
  "count": 5
}
```

### GET /health
Service health check with flow count.

## Available Flows

### 1. Recognition Analysis
Analyzes free-association recognition networks and capacity distribution.

**Request Type:** `recognition-analysis`  
**Preferred Models:** Claude 3 Opus, GPT-4, GPT-4 Turbo  
**Use Cases:** Mutual recognition calculation, network health, capacity flows

### 2. Capacity Recommendation
Recommends optimal capacity distribution based on mutual recognition.

**Request Type:** `capacity-recommendation`  
**Preferred Models:** Claude 3 Sonnet, GPT-4, Claude 3 Opus  
**Use Cases:** Surplus capacity matching, contributor recommendations

### 3. Code Generation
Generates clean, documented, production-quality code.

**Request Type:** `code-generation`  
**Preferred Models:** GPT-4, Claude 3 Sonnet, GPT-4 Turbo  
**Use Cases:** Algorithm implementation, type-safe code, utilities

### 4. Data Analysis
Analyzes data and provides structured insights.

**Request Type:** `data-analysis`  
**Preferred Models:** GPT-4, Claude 3 Sonnet, GPT-4 Turbo  
**Use Cases:** Trend analysis, anomaly detection, comparisons

### 5. General Chat (Fallback)
General conversational AI.

**Request Type:** `chat` (default)  
**Preferred Models:** GPT-3.5 Turbo, Claude 3 Haiku, Mistral Medium  
**Use Cases:** Q&A, explanations, general conversation

## Creating Custom Flows

See [`FLOWS_GUIDE.md`](./FLOWS_GUIDE.md) for detailed instructions on creating your own typed flows with Zod schemas and LangChain patterns.

## Running Locally

```bash
# Install dependencies
bun install

# Run typed server (TypeScript)
bun start

# Run with auto-reload
bun dev

# Run legacy server (JavaScript)
bun run start:legacy
```

## Running with Docker

```bash
docker build -t llm-router .
docker run -p 8768:8768 llm-router
```

