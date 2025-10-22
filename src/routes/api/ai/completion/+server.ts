import { json, error } from '@sveltejs/kit';
import type { RequestHandler } from '@sveltejs/kit';
import { CompletionRequestSchema, RoutingResponseSchema, HealthReportSchema, type CompletionRequest, type RoutingResponse } from '$lib/server/schemas';
import { requireAuth } from '$lib/server/middleware/auth';
import { checkGeneralRateLimit, checkAiRateLimit, checkTokenRateLimit } from '$lib/server/middleware/rate-limit';

// Import env vars with fallbacks for static builds
let OPENROUTER_BASE_URL: string | undefined;
let APP_URL: string | undefined;

try {
  const env = await import('$env/static/private');
  OPENROUTER_BASE_URL = env.OPENROUTER_BASE_URL;
  APP_URL = env.APP_URL;
} catch (e) {
  OPENROUTER_BASE_URL = undefined;
  APP_URL = undefined;
}

const OPENROUTER_ENDPOINT = `${OPENROUTER_BASE_URL || 'https://openrouter.ai/api/v1'}/chat/completions`;

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
    
    const requestBody = {
      model: routing.model,
      messages: messages || [{ role: 'user', content: prompt }],
      max_tokens: normalizedMaxTokens,
      temperature: temperature || 0.7
    };
    
    const headers = {
      'Content-Type': 'application/json',
      'Authorization': `Bearer ${routing.key}`,
      'HTTP-Referer': APP_URL || '',
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

