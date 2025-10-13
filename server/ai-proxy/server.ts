import express, { Request, Response, NextFunction } from 'express';
import cors from 'cors';
import fetch from 'node-fetch';
import { z } from 'zod';

// Import schemas
import {
  CompletionRequestSchema,
  TokenRequestSchema,
  RoutingResponseSchema,
  HealthReportSchema,
  type CompletionRequest,
  type RoutingResponse
} from './schemas/completion';

// Import middleware
import { authenticateEither, generateToken } from './middleware/auth.js';
import {
  generalLimiter,
  aiLimiter,
  aiTokenLimiter,
  authLimiter,
  configureHelmet,
  configureCors,
  requestLogger,
  errorHandler,
  notFoundHandler
} from './middleware/security.js';

interface Config {
  host: string;
  port: number;
  llmRouterUrl: string;
  nodeEnv: string;
}

const config: Config = {
  host: process.env.AI_PROXY_HOST || '0.0.0.0',
  port: parseInt(process.env.AI_PROXY_PORT || '8767'),
  llmRouterUrl: process.env.LLM_ROUTER_URL || 'http://llm-router:8768',
  nodeEnv: process.env.NODE_ENV || 'development',
};

console.log('\n=== AI PROXY GATEWAY (TypeScript) ===\n');
console.log('Configuration:', JSON.stringify(config, null, 2));

const app = express();
app.set('trust proxy', true);
app.use(configureHelmet());
app.use(cors(configureCors()));

const bodyLimit = process.env.BODY_PARSER_LIMIT || '1mb';
app.use(express.json({ limit: bodyLimit }));
app.use(express.urlencoded({ extended: true, limit: bodyLimit }));
app.use(requestLogger);

// Root endpoint
app.get('/', (req: Request, res: Response) => {
  res.json({
    service: 'AI Proxy Gateway',
    version: '2.0.0',
    language: 'TypeScript',
    environment: config.nodeEnv,
    endpoints: {
      root: 'GET /',
      health: 'GET /health',
      auth: 'POST /auth/token',
      aiCompletion: 'POST /api/ai/completion',
    },
    features: {
      typed: true,
      zodValidation: true,
      flowRouting: true
    }
  });
});

// Health check
app.get('/health', (req: Request, res: Response) => {
  res.json({
    status: 'ok',
    service: 'ai-proxy-ts',
    timestamp: Date.now(),
    environment: config.nodeEnv
  });
});

// Token generation endpoint
app.post('/auth/token', authLimiter, (req: Request, res: Response) => {
  try {
    const parsed = TokenRequestSchema.safeParse(req.body);
    
    if (!parsed.success) {
      return res.status(400).json({
        error: 'Invalid request',
        details: parsed.error.format()
      });
    }
    
    const { apiKey, userId } = parsed.data;
    const masterKey = process.env.MASTER_API_KEY || 'dev-key-12345-change-me';
    
    if (apiKey !== masterKey) {
      return res.status(401).json({ error: 'Invalid API key' });
    }
    
    const expiresIn = process.env.JWT_EXPIRY || '24h';
    const token = generateToken(
      { userId: userId || 'anonymous', role: 'user', issued: Date.now() },
      expiresIn
    );
    
    res.json({ token, expiresIn, type: 'Bearer' });
  } catch (error: any) {
    console.error('Token generation error:', error);
    res.status(500).json({ error: 'Token generation failed', detail: error.message });
  }
});

// Provider endpoints configuration
interface ProviderEndpoint {
  baseUrl: string;
  chatPath?: string;
  completionPath?: string;
  messagesPath?: string;
}

const PROVIDER_ENDPOINTS: Record<string, ProviderEndpoint> = {
  'openrouter': {
    baseUrl: process.env.OPENROUTER_BASE_URL || 'https://openrouter.ai/api/v1',
    chatPath: '/chat/completions'
  },
  'openai': {
    baseUrl: process.env.OPENAI_BASE_URL || 'https://api.openai.com/v1',
    chatPath: '/chat/completions',
    completionPath: '/completions'
  },
  'anthropic': {
    baseUrl: process.env.ANTHROPIC_BASE_URL || 'https://api.anthropic.com/v1',
    messagesPath: '/messages'
  },
  'mistral': {
    baseUrl: process.env.MISTRAL_BASE_URL || 'https://api.mistral.ai/v1',
    chatPath: '/chat/completions'
  }
};

// Main AI completion endpoint
app.post(
  '/api/ai/completion',
  generalLimiter,
  aiLimiter,
  aiTokenLimiter,
  authenticateEither,
  async (req: Request, res: Response) => {
    try {
      // Validate request with Zod
      const parsed = CompletionRequestSchema.safeParse(req.body);
      
      if (!parsed.success) {
        return res.status(400).json({
          error: 'Invalid request',
          details: parsed.error.format()
        });
      }
      
      const requestData: CompletionRequest = parsed.data;
      const { prompt, messages, maxTokens, max_tokens, temperature, model: requestedModel } = requestData;
      
      // Normalize max_tokens (support both camelCase and snake_case)
      const normalizedMaxTokens = maxTokens || max_tokens || 1024;
      
      // Step 1: Get routing decision from LLM Router
      const routingResponse = await fetch(`${config.llmRouterUrl}/route`, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({
          ...requestData,
          maxTokens: normalizedMaxTokens
        })
      });
      
      if (!routingResponse.ok) {
        console.error('LLM Router unavailable');
        return res.status(503).json({
          error: 'LLM routing service unavailable',
          message: 'Unable to select optimal model'
        });
      }
      
      const routingData = await routingResponse.json();
      
      // Validate routing response
      const routingParsed = RoutingResponseSchema.safeParse(routingData);
      if (!routingParsed.success) {
        console.error('Invalid routing response:', routingParsed.error);
        return res.status(502).json({
          error: 'Invalid routing response',
          detail: 'Router returned invalid data'
        });
      }
      
      const routing: RoutingResponse = routingParsed.data;
      
      console.log('Routing decision:', {
        model: routing.model,
        provider: routing.provider,
        flow: routing.flow?.name,
        category: routing.selection?.category
      });
      
      // Step 2: Build provider-specific request
      const providerConfig = PROVIDER_ENDPOINTS[routing.provider];
      if (!providerConfig) {
        return res.status(500).json({
          error: 'Unsupported provider',
          provider: routing.provider
        });
      }
      
      let endpoint: string;
      let requestBody: any;
      let headers: Record<string, string>;
      
      if (routing.provider === 'openrouter') {
        endpoint = `${providerConfig.baseUrl}${providerConfig.chatPath}`;
        requestBody = {
          model: routing.model,
          messages: messages || [{ role: 'user', content: prompt }],
          max_tokens: normalizedMaxTokens,
          temperature: temperature || 0.7
        };
        headers = {
          'Content-Type': 'application/json',
          'Authorization': `Bearer ${routing.key}`,
          'HTTP-Referer': process.env.APP_URL || 'https://free-association.playnet.lol',
          'X-Title': 'Free Association AI'
        };
      } else if (routing.provider === 'openai') {
        endpoint = `${providerConfig.baseUrl}${messages ? providerConfig.chatPath : providerConfig.completionPath}`;
        requestBody = messages
          ? { model: routing.model, messages, max_tokens: normalizedMaxTokens, temperature }
          : { model: routing.model, prompt, max_tokens: normalizedMaxTokens, temperature };
        headers = {
          'Content-Type': 'application/json',
          'Authorization': `Bearer ${routing.key}`
        };
      } else if (routing.provider === 'anthropic') {
        endpoint = `${providerConfig.baseUrl}${providerConfig.messagesPath}`;
        requestBody = {
          model: routing.model,
          messages: messages || [{ role: 'user', content: prompt }],
          max_tokens: normalizedMaxTokens,
          temperature: temperature || 0.7
        };
        headers = {
          'Content-Type': 'application/json',
          'x-api-key': routing.key,
          'anthropic-version': '2023-06-01'
        };
      } else if (routing.provider === 'mistral') {
        endpoint = `${providerConfig.baseUrl}${providerConfig.chatPath}`;
        requestBody = {
          model: routing.model,
          messages: messages || [{ role: 'user', content: prompt }],
          max_tokens: normalizedMaxTokens,
          temperature
        };
        headers = {
          'Content-Type': 'application/json',
          'Authorization': `Bearer ${routing.key}`
        };
      } else {
        return res.status(500).json({ error: 'Provider configuration incomplete' });
      }
      
      // Step 3: Call LLM provider
      const startTime = Date.now();
      const providerResponse = await fetch(endpoint, {
        method: 'POST',
        headers,
        body: JSON.stringify(requestBody)
      });
      
      const responseTime = Date.now() - startTime;
      const result = await providerResponse.json();
      
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
      const poolName = routing.provider === 'openrouter' ? 'openrouter' : routing.model;
      const healthReport = HealthReportSchema.parse({
        key: routing.key,
        status: healthStatus,
        error: providerResponse.ok ? null : result.error?.message,
        cost
      });
      
      fetch(`${config.llmRouterUrl.replace('llm-router', 'key-pool')}/health/${poolName}`, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify(healthReport)
      }).catch(err => console.error('Failed to report key health:', err.message));
      
      // Step 5: Return formatted response
      return res.status(providerResponse.status).json({
        ...result,
        _routing: {
          model: routing.model,
          provider: routing.provider,
          flow: routing.flow?.name,
          category: routing.selection?.category,
          reason: routing.selection?.reason,
          responseTimeMs: responseTime
        }
      });
      
    } catch (error: any) {
      console.error('AI completion error:', error);
      return res.status(502).json({
        error: 'AI service unavailable',
        detail: error.message
      });
    }
  }
);

app.use(notFoundHandler);
app.use(errorHandler);

app.listen(config.port, config.host, () => {
  console.log(`\n=== AI PROXY STARTED ===`);
  console.log(`Listening on: ${config.host}:${config.port}`);
  console.log(`Environment: ${config.nodeEnv}`);
  console.log(`LLM Router: ${config.llmRouterUrl}`);
  console.log(`Endpoints:`);
  console.log(`  - Documentation: http://${config.host}:${config.port}/`);
  console.log(`  - Health check: http://${config.host}:${config.port}/health`);
  console.log(`  - AI Completion: http://${config.host}:${config.port}/api/ai/completion`);
  console.log(`========================\n`);
});

process.on('SIGTERM', () => {
  console.log('\nSIGTERM received, shutting down gracefully');
  process.exit(0);
});

process.on('SIGINT', () => {
  console.log('\nSIGINT received, shutting down gracefully');
  process.exit(0);
});

