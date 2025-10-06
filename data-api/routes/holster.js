import express from 'express';

const router = express.Router();

// Holster instance (will be set by server.js)
let holster;

export function setHolsterInstance(holsterInstance) {
  holster = holsterInstance;
}

/**
 * POST /holster/put
 * Write data to Holster database
 */
router.post('/put', async (req, res) => {
  try {
    const { path, data } = req.body;

    if (!path || !data) {
      return res.status(400).json({ error: 'path and data are required' });
    }

    const pathParts = path.split('/').filter(p => p);
    let ref = holster;

    for (const part of pathParts) {
      ref = ref.get(part);
    }

    ref.put(data);
    res.json({ success: true, path, data });
  } catch (error) {
    res.status(500).json({ error: error.message });
  }
});

/**
 * GET /holster/get
 * Read data from Holster database
 */
router.get('/get', async (req, res) => {
  try {
    const { path } = req.query;

    if (!path) {
      return res.status(400).json({ error: 'path query parameter is required' });
    }

    const pathParts = path.split('/').filter(p => p);
    let ref = holster;

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
