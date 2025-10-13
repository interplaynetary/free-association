import express, { Request, Response } from 'express';
import { routeRequest, getAvailableFlows } from './router';

const config = {
  host: process.env.LLM_ROUTER_HOST || '0.0.0.0',
  port: parseInt(process.env.LLM_ROUTER_PORT || '8768'),
  keyPoolUrl: process.env.KEY_POOL_URL || 'http://key-pool:8769'
};

console.log('\n=== TYPED LLM ROUTER SERVICE ===\n');
console.log('Configuration:', JSON.stringify(config, null, 2));

const app = express();
app.use(express.json({ limit: '10mb' }));

/**
 * POST /route - Main routing endpoint with typed flows
 */
app.post('/route', async (req: Request, res: Response) => {
  try {
    const routing = await routeRequest(req.body);
    
    console.log('Routing decision:', {
      flow: routing.flow.name,
      model: routing.model,
      provider: routing.provider
    });
    
    return res.json(routing);
    
  } catch (error: any) {
    console.error('Routing error:', error);
    return res.status(error.message.includes('No providers') ? 503 : 400).json({
      error: 'Routing failed',
      message: error.message
    });
  }
});

/**
 * GET /flows - List all available flows
 */
app.get('/flows', (req: Request, res: Response) => {
  const flows = getAvailableFlows();
  res.json({
    flows,
    count: flows.length
  });
});

/**
 * GET /health - Health check
 */
app.get('/health', (req: Request, res: Response) => {
  res.json({
    status: 'ok',
    service: 'llm-router-typed',
    timestamp: Date.now(),
    flowsAvailable: getAvailableFlows().length
  });
});

// Start server
app.listen(config.port, config.host, () => {
  console.log(`\n=== TYPED LLM ROUTER STARTED ===`);
  console.log(`Listening on: ${config.host}:${config.port}`);
  console.log(`Key Pool URL: ${config.keyPoolUrl}`);
  console.log(`Flows configured: ${getAvailableFlows().length}`);
  console.log(`=================================\n`);
});

process.on('SIGTERM', () => {
  console.log('SIGTERM received, shutting down');
  process.exit(0);
});

process.on('SIGINT', () => {
  console.log('SIGINT received, shutting down');
  process.exit(0);
});

