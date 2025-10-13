import { RequestSchema } from './schemas/requestTypes';
import { getFlow, listFlows } from './flows/flowRegistry';
import type { FlowDefinition } from './flows/flowRegistry';

const KEY_POOL_URL = process.env.KEY_POOL_URL || 'http://key-pool:8769';

/**
 * Route a request using typed flows
 */
export async function routeRequest(requestBody: any) {
  // Step 1: Validate and parse request
  const parseResult = RequestSchema.safeParse(requestBody);
  
  if (!parseResult.success) {
    throw new Error(`Invalid request: ${parseResult.error.message}`);
  }
  
  const request = parseResult.data;
  const requestType = 'requestType' in request ? request.requestType : 'chat';
  
  // Step 2: Get flow definition for this request type
  const flow = getFlow(requestType);
  console.log(`Using flow: ${flow.name} for request type: ${requestType}`);
  
  // Step 3: Generate prompt from flow template
  const promptConfig = flow.promptTemplate(request);
  
  // Step 4: Try preferred models in order
  for (const modelName of flow.preferredModels) {
    const key = await getKeyForModel(modelName);
    
    if (key) {
      console.log(`Selected model: ${modelName}`);
      
      return {
        success: true,
        flow: {
          name: flow.name,
          requestType: flow.requestType,
          description: flow.description
        },
        model: modelName,
        provider: getProviderFromModel(modelName),
        key,
        promptConfig,
        postProcess: flow.postProcess ? 'enabled' : 'disabled'
      };
    }
    
    console.log(`No key available for ${modelName}, trying next...`);
  }
  
  // No models available
  throw new Error(`No providers available for flow: ${flow.name}`);
}

/**
 * Get key for a model from key pool
 */
async function getKeyForModel(modelName: string): Promise<string | null> {
  try {
    // Extract pool name (openrouter for OpenRouter models, model name for direct)
    const poolName = modelName.includes('/') ? 'openrouter' : modelName;
    
    const response = await fetch(`${KEY_POOL_URL}/keys/${poolName}`, {
      method: 'GET',
      headers: { 'Content-Type': 'application/json' }
    });
    
    if (!response.ok) {
      return null;
    }
    
    const data = await response.json();
    return data.key || null;
  } catch (error) {
    console.error(`Error fetching key for ${modelName}:`, error);
    return null;
  }
}

/**
 * Get provider from model name
 */
function getProviderFromModel(modelName: string): string {
  if (modelName.includes('/')) {
    return 'openrouter';
  }
  
  if (modelName.startsWith('gpt-')) {
    return 'openai';
  }
  
  if (modelName.startsWith('claude-')) {
    return 'anthropic';
  }
  
  if (modelName.startsWith('mistral-')) {
    return 'mistral';
  }
  
  return 'unknown';
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

