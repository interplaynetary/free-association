import 'dotenv/config';
import express from 'express';
import cors from 'cors';
import bodyParser from 'body-parser';
import fetch from 'node-fetch';
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

const config = {
  host: process.env.DATA_API_HOST || '0.0.0.0',
  port: parseInt(process.env.DATA_API_PORT) || 8767,
  aiBackend: process.env.AI_API_URL || 'http://localhost:8000/v1/completions',
  nodeEnv: process.env.NODE_ENV || 'development',
};

console.log('\n=== AI PROXY GATEWAY ===\n');
console.log('Configuration:');
console.log(JSON.stringify(config, null, 2));

const app = express();
app.set('trust proxy', true);
app.use(configureHelmet());
app.use(cors(configureCors()));
const bodyLimit = process.env.BODY_PARSER_LIMIT || '1mb';
app.use(bodyParser.json({ limit: bodyLimit }));
app.use(bodyParser.urlencoded({ extended: true, limit: bodyLimit }));
app.use(requestLogger);

app.get('/', (req, res) => {
  res.json({
    service: 'AI Proxy Gateway',
    version: '1.0.0',
    environment: config.nodeEnv,
    endpoints: {
      root: 'GET /',
      health: 'GET /health',
      auth: 'POST /auth/token',
      aiCompletion: 'POST /api/ai/completion',
    },
    notes: {
      security: 'All /api/* endpoints are protected by API key or JWT, rate-limited, and secured.'
    }
  });
});

app.get('/health', (req, res) => {
  res.json({
    status: 'ok',
    service: 'ai-proxy',
    timestamp: Date.now(),
    environment: config.nodeEnv
  });
});

app.post('/auth/token', authLimiter, (req, res) => {
  const { apiKey, userId } = req.body;
  const masterKey = process.env.MASTER_API_KEY || 'dev-key-12345-change-me';
  if (apiKey !== masterKey) {
    return res.status(401).json({ error: 'Invalid API key' });
  }
  const expiresIn = process.env.JWT_EXPIRY || '24h';
  const token = generateToken({ userId: userId || 'anonymous', role: 'user', issued: Date.now() }, expiresIn);
  res.json({ token, expiresIn, type: 'Bearer' });
});

// AI Completion Proxy Endpoint
app.post('/api/ai/completion', generalLimiter, aiLimiter, aiTokenLimiter, authenticateEither, async (req, res) => {
  try {
    const aiResponse = await fetch(config.aiBackend, {
      method: 'POST',
      headers: { 'Content-Type': 'application/json', ...(process.env.AI_API_KEY ? { 'Authorization': `Bearer ${process.env.AI_API_KEY}` } : {}) },
      body: JSON.stringify(req.body),
      timeout: 25000
    });
    const result = await aiResponse.json();
    return res.status(aiResponse.status).json(result);
  } catch (err) {
    return res.status(502).json({ error: 'AI backend unavailable', detail: err.message });
  }
});

app.use(notFoundHandler);
app.use(errorHandler);

app.listen(config.port, config.host, () => {
  console.log(`\n=== AI PROXY STARTED ===`);
  console.log(`Listening on: ${config.host}:${config.port}`);
  console.log(`Environment: ${config.nodeEnv}`);
  console.log(`Endpoints:`);
  console.log(`  - Documentation: http://${config.host}:${config.port}/`);
  console.log(`  - Health check: http://${config.host}:${config.port}/health`);
  console.log(`  - AI Completion: http://${config.host}:${config.port}/api/ai/completion`);
});

process.on('SIGTERM', () => {
  console.log('\nSIGTERM received, shutting down gracefully');
  process.exit(0);
});
process.on('SIGINT', () => {
  console.log('\nSIGINT received, shutting down gracefully');
  process.exit(0);
});
