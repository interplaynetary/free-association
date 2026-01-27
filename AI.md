1. Key Infrastructure (The Fuel)
The system relies on a community-donated pool of OpenRouter keys.

Frontend: src/routes/donate-key/+page.svelte allows users to donate keys.
API: src/routes/api/keys/donate/+server.ts validates keys and adds them to the pool.
core: src/lib/server/key-pool/manager.ts manages the in-memory pool, tracking health, success rates, and rotating keys round-robin style to ensure reliability.
2. The Quest Generation Service (The Trigger)
Quest generation happens automatically on the client side based on schedule or data changes.

Auto-Scheduler: src/lib/services/quest-auto-generation.ts monitors your stores (myRecognitionTreeStore, myCommitmentStore) for changes and runs on a daily/weekly schedule.
Service: src/lib/services/quest-service.ts gathers all necessary context (Tree, Capacities, Needs, Location) and sends it to the server via POST /api/llm/quest-generation.
3. Server-Side Processing (The Brain)
The server handles the request in a layered approach:

Quest Endpoint: src/routes/api/llm/quest-generation/+server.ts receives the user data. It doesn't call the LLM directly; instead, it formats a request for the unified AI system.
Unified AI Endpoint: src/routes/api/ai/completion/+server.ts is the central hub. It:
Calls the Router (src/lib/server/llm/router.ts) to select the best model and a healthy key.
Uses the Flow (src/lib/server/llm/flows.ts) to generate the specific system/user prompts for quests.
Executes the actual HTTP request to OpenRouter.
Reports key health/usage back to the pool manager.
4. The Intelligence (The Logic)
Flows: src/lib/server/llm/flows.ts contains the questGenerationFlow. This defines the specific prompt engineering that translates your tree and capacities into JSON-formatted quests.
Router: src/lib/server/llm/router.ts decides which model to use (preferring Claude-3 Opus or GPT-4 for quests) and grabs a key from the pool.
The architecture is clean and robust, separating concerns between the "What" (Flows), the "How" (Router/Keys), and the "When" (Client Services).