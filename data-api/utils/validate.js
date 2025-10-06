/**
 * Simple input validation (no Joi dependency needed)
 */

export function validatePathData(req, res, next) {
  const { path, data } = req.body;

  if (!path || typeof path !== 'string') {
    return res.status(400).json({
      error: 'Validation error',
      message: 'path is required and must be a string'
    });
  }

  if (path.length > 500) {
    return res.status(400).json({
      error: 'Validation error',
      message: 'path must be less than 500 characters'
    });
  }

  if (!data || typeof data !== 'object' || Array.isArray(data)) {
    return res.status(400).json({
      error: 'Validation error',
      message: 'data is required and must be an object'
    });
  }

  // Sanitize path - remove dangerous characters
  req.body.path = path.replace(/[<>]/g, '').trim();

  next();
}

export function validatePathQuery(req, res, next) {
  const { path } = req.query;

  if (!path || typeof path !== 'string') {
    return res.status(400).json({
      error: 'Validation error',
      message: 'path query parameter is required and must be a string'
    });
  }

  if (path.length > 500) {
    return res.status(400).json({
      error: 'Validation error',
      message: 'path must be less than 500 characters'
    });
  }

  // Sanitize path
  req.query.path = path.replace(/[<>]/g, '').trim();

  next();
}
