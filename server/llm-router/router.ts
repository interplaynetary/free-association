import { ExtendedCompletionRequestSchema } from '../shared-schemas/completion';
import { getFlow, listFlows } from './flows/flowRegistry';
import type { FlowDefinition } from './flows/flowRegistry';

const KEY_POOL_URL = process.env.KEY_POOL_URL || 'http://key-pool:8769';

/**
 * Route a request using typed flows (OpenRouter-only)
 */
export async function routeRequest(requestBody: any) {
  // Step 1: Validate and parse request
  const parseResult = ExtendedCompletionRequestSchema.safeParse(requestBody);
  
  if (!parseResult.success) {
    throw new Error(`Invalid request: ${parseResult.error.message}`);
  }
  
  const request = parseResult.data;
  const requestType = request.requestType || 'chat';
  
  // Step 2: Get flow definition for this request type
  const flow = getFlow(requestType);
  console.log(`Using flow: ${flow.name} for request type: ${requestType}`);
  
  // Step 3: Generate prompt from flow template
  const promptConfig = flow.promptTemplate(request);
  
  // Step 4: Try preferred models in order
  for (const modelName of flow.preferredModels) {
    const key = await getOpenRouterKey();
    
    if (key) {
      console.log(`Selected model: ${modelName}`);
      
      return {
        success: true,
        flow: {
          name: flow.name,
          requestType: flow.requestType,
          description: flow.description
        },
        model: modelName, // OpenRouter model name (provider/model)
        provider: 'openrouter', // Always OpenRouter
        key,
        promptConfig,
        postProcess: flow.postProcess ? 'enabled' : 'disabled'
      };
    }
    
    console.log(`No OpenRouter key available, trying next model...`);
  }
  
  // No keys available
  throw new Error(`No OpenRouter keys available for flow: ${flow.name}`);
}

/**
 * Get OpenRouter key from key pool
 */
async function getOpenRouterKey(): Promise<string | null> {
  try {
    const response = await fetch(`${KEY_POOL_URL}/keys/openrouter`, {
      method: 'GET',
      headers: { 'Content-Type': 'application/json' }
    });
    
    if (!response.ok) {
      return null;
    }
    
    const data = await response.json();
    return data.key || null;
  } catch (error) {
    console.error(`Error fetching OpenRouter key:`, error);
    return null;
  }
}

/**
 * Get all available flows
 */
export function getAvailableFlows() {
  return listFlows().map(flow => ({
    name: flow.name,
    requestType: flow.requestType,
    description: flow.description,
    preferredModels: flow.preferredModels
  }));
}

/**
 * Execute post-processing if defined in flow
 */
export function postProcessResponse(flowName: string, response: any) {
  const flow = getFlow(flowName);
  
  if (flow.postProcess) {
    return flow.postProcess(response);
  }
  
  return response;
}
