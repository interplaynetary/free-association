import express from 'express';
import Gun from 'gun';
import { validatePathData, validatePathQuery } from '../utils/validate.js';

const router = express.Router();

// Initialize Gun client (will be set by server.js)
let gun;

export function setGunInstance(gunInstance) {
  gun = gunInstance;
}

/**
 * POST /gun/put
 * Write data to Gun database
 */
router.post('/put', validatePathData, async (req, res) => {
  try {
    const { path, data } = req.body;

    const pathParts = path.split('/').filter(p => p);
    let ref = gun;

    for (const part of pathParts) {
      ref = ref.get(part);
    }

    ref.put(data, (ack) => {
      if (ack.err) {
        return res.status(500).json({ error: ack.err });
      }
      res.json({ success: true, path, data });
    });
  } catch (error) {
    res.status(500).json({ error: error.message });
  }
});

/**
 * GET /gun/get
 * Read data from Gun database
 */
router.get('/get', validatePathQuery, async (req, res) => {
  try {
    const { path } = req.query;

    const pathParts = path.split('/').filter(p => p);
    let ref = gun;

    for (const part of pathParts) {
      ref = ref.get(part);
    }

    ref.once((data) => {
      if (!data) {
        return res.status(404).json({ error: 'Data not found' });
      }
      res.json({ success: true, path, data });
    });
  } catch (error) {
    res.status(500).json({ error: error.message });
  }
});

/**
 * POST /gun/seed
 * Seed Gun database with sample data
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

    gun.get('freely-associating-players').put(sampleData, (ack) => {
      if (ack.err) {
        return res.status(500).json({ error: ack.err });
      }
      res.json({ success: true, message: 'Gun database seeded', data: sampleData });
    });
  } catch (error) {
    res.status(500).json({ error: error.message });
  }
});

export default router;
