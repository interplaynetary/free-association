import express from 'express';
import { validatePathData, validatePathQuery } from '../utils/validate.js';

const router = express.Router();

// Request timeout (configurable via env var)
const REQUEST_TIMEOUT = parseInt(process.env.REQUEST_TIMEOUT) || 5000;

// Holster instance (will be set by server.js)
let holster;

export function setHolsterInstance(holsterInstance) {
  holster = holsterInstance;
}

/**
 * POST /holster/put
 * Write data to Holster database
 */
router.post('/put', validatePathData, async (req, res) => {
  try {
    const { path, data } = req.body;

    const pathParts = path.split('/').filter(p => p);
    let ref = holster;

    for (const part of pathParts) {
      ref = ref.get(part);
    }

    // Add timeout for Holster writes
    let hasResponded = false;
    const timeout = setTimeout(() => {
      if (!hasResponded) {
        hasResponded = true;
        return res.status(504).json({ error: 'Write timeout - relay may be unreachable' });
      }
    }, REQUEST_TIMEOUT);

    // Holster.put doesn't have callback, so we send response immediately
    // The timeout is for the entire operation
    try {
      ref.put(data);
      if (!hasResponded) {
        hasResponded = true;
        clearTimeout(timeout);
        res.json({ success: true, path, data });
      }
    } catch (putError) {
      if (!hasResponded) {
        hasResponded = true;
        clearTimeout(timeout);
        throw putError;
      }
    }
  } catch (error) {
    res.status(500).json({ error: error.message });
  }
});

/**
 * GET /holster/get
 * Read data from Holster database
 */
router.get('/get', validatePathQuery, async (req, res) => {
  try {
    const { path } = req.query;

    const pathParts = path.split('/').filter(p => p);
    let ref = holster;

    for (const part of pathParts) {
      ref = ref.get(part);
    }

    // Add timeout to prevent hanging on missing data
    let hasResponded = false;
    const timeout = setTimeout(() => {
      if (!hasResponded) {
        hasResponded = true;
        return res.status(504).json({ error: 'Request timeout - data not found or relay unreachable' });
      }
    }, REQUEST_TIMEOUT);

    ref.once((data) => {
      if (!hasResponded) {
        hasResponded = true;
        clearTimeout(timeout);
        try {
          if (!data) {
            return res.status(404).json({ error: 'Data not found' });
          }
          res.json({ success: true, path, data });
        } catch (error) {
          res.status(500).json({ error: 'Error processing Holster data' });
        }
      }
    });
  } catch (error) {
    res.status(500).json({ error: error.message });
  }
});

/**
 * POST /holster/seed
 * Seed Holster database with sample data
 */
router.post('/seed', async (req, res) => {
  try {
    const sampleData = req.body.data || {
      players: {
        alice: { name: 'Alice', lastSeen: Date.now() },
        bob: { name: 'Bob', lastSeen: Date.now() },
        charlie: { name: 'Charlie', lastSeen: Date.now() }
      }
    };

    holster.get('freely-associating-players').put(sampleData);
    res.json({ success: true, message: 'Holster database seeded', data: sampleData });
  } catch (error) {
    res.status(500).json({ error: error.message });
  }
});

export default router;
