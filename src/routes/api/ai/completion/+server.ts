import { json, error } from '@sveltejs/kit';
import type { RequestHandler } from '@sveltejs/kit';
import { CompletionRequestSchema, RoutingResponseSchema, HealthReportSchema, type CompletionRequest, type RoutingResponse } from '$lib/server/schemas';
import { requireAuth } from '$lib/server/middleware/unified-auth';
import { checkGeneralRateLimit, checkAiRateLimit, checkTokenRateLimit } from '$lib/server/middleware/rate-limit';
import { config } from '$lib/server/config';

const OPENROUTER_ENDPOINT = `${config.openrouterBaseUrl}/chat/completions`;

/**
 * POST /api/ai/completion - Main AI completion endpoint
 */
export const POST: RequestHandler = async ({ request, fetch }) => {
  try {
    // Authenticate
    const authResult = requireAuth(request);
    const userId = authResult.user?.userId;
    
    // Rate limiting
    checkGeneralRateLimit(request, userId);
    checkAiRateLimit(request, userId);
    
    // Parse and validate request
    const body = await request.json();
    const parsed = CompletionRequestSchema.safeParse(body);
    
    if (!parsed.success) {
      throw error(400, 'Invalid request: ' + JSON.stringify(parsed.error.format()));
    }
    
    const requestData: CompletionRequest = parsed.data;
    const { prompt, messages, maxTokens, max_tokens, temperature, model: requestedModel } = requestData;
    
    // Normalize max_tokens
    const normalizedMaxTokens = maxTokens || max_tokens || 1024;
    
    // Token-based rate limiting
    checkTokenRateLimit(request, normalizedMaxTokens, userId);
    
    // Step 1: Get routing decision from LLM Router
    const routingResponse = await fetch('/api/llm/route', {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({
        ...requestData,
        maxTokens: normalizedMaxTokens
      })
    });
    
    if (!routingResponse.ok) {
      console.error('LLM Router unavailable');
      throw error(503, 'LLM routing service unavailable - Unable to select optimal model');
    }
    
    const routingData = await routingResponse.json();
    
    // Validate routing response
    const routingParsed = RoutingResponseSchema.safeParse(routingData);
    if (!routingParsed.success) {
      console.error('Invalid routing response:', routingParsed.error);
      throw error(502, 'Invalid routing response - Router returned invalid data');
    }
    
    const routing: RoutingResponse = routingParsed.data;
    
    console.log('Routing decision:', {
      model: routing.model,
      provider: routing.provider,
      flow: routing.flow?.name
    });
    
    // Step 2: Build OpenRouter request
    if (routing.provider !== 'openrouter') {
      throw error(500, 'Invalid provider - Only OpenRouter is supported');
    }
    
    // Use promptConfig from flow if available (typed flows)
    // Otherwise fall back to raw messages/prompt (legacy/chat)
    let requestMessages;
    let requestTemperature;
    let requestMaxTokens;
    
    if (routing.promptConfig) {
      // Typed flow: use the flow's generated prompt
      requestMessages = [];
      if (routing.promptConfig.system) {
        requestMessages.push({ role: 'system', content: routing.promptConfig.system });
      }
      requestMessages.push({ role: 'user', content: routing.promptConfig.user });
      requestTemperature = routing.promptConfig.temperature || 0.7;
      requestMaxTokens = routing.promptConfig.maxTokens || normalizedMaxTokens;
      
      console.log('[AI-COMPLETION] Using flow-generated prompt:', routing.flow?.name);
    } else {
      // Legacy/chat: use raw messages or prompt
      requestMessages = messages || [{ role: 'user', content: prompt }];
      requestTemperature = temperature || 0.7;
      requestMaxTokens = normalizedMaxTokens;
      
      console.log('[AI-COMPLETION] Using raw messages/prompt');
    }
    
    const requestBody = {
      model: routing.model,
      messages: requestMessages,
      max_tokens: requestMaxTokens,
      temperature: requestTemperature
    };
    
    const headers = {
      'Content-Type': 'application/json',
      'Authorization': `Bearer ${routing.key}`,
      'HTTP-Referer': config.appUrl,
      'X-Title': 'Free Association AI'
    };
    
    // Step 3: Call OpenRouter
    const startTime = Date.now();
    const providerResponse = await fetch(OPENROUTER_ENDPOINT, {
      method: 'POST',
      headers,
      body: JSON.stringify(requestBody)
    });
    
    const responseTime = Date.now() - startTime;
    const result: any = await providerResponse.json();
    
    // Step 4: Report key health back to key pool
    let healthStatus: 'healthy' | 'degraded' | 'failed' | 'rate_limited' | 'depleted' = 
      providerResponse.ok ? 'healthy' :
      providerResponse.status === 429 ? 'rate_limited' : 'degraded';
    
    // Check for depleted credits
    if (!providerResponse.ok && result.error?.message?.includes('insufficient')) {
      healthStatus = 'depleted';
    }
    
    // Extract cost
    const cost = result.usage
      ? (result.usage.prompt_tokens * 0.000001) + (result.usage.completion_tokens * 0.000001)
      : null;
    
    // Fire and forget health report
    const healthReport = HealthReportSchema.parse({
      key: routing.key,
      status: healthStatus,
      error: providerResponse.ok ? null : result.error?.message,
      cost
    });
    
    fetch('/api/keys/health/openrouter', {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify(healthReport)
    }).catch(err => console.error('Failed to report key health:', err.message));
    
    // Step 5: Return formatted response
    return json({
      ...(result as object),
      _routing: {
        model: routing.model,
        provider: routing.provider,
        flow: routing.flow?.name,
        responseTimeMs: responseTime
      }
    }, { status: providerResponse.status });
    
  } catch (err: any) {
    console.error('AI completion error:', err);
    
    // If it's already a SvelteKit error, rethrow it
    if (err.status) {
      throw err;
    }
    
    throw error(502, 'AI service unavailable: ' + err.message);
  }
};

