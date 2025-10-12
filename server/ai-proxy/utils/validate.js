/**
 * Simple input validation (no Joi dependency needed)
 */

// Path validation regex: alphanumeric, slashes, hyphens, underscores, dots only
const SAFE_PATH_REGEX = /^[a-zA-Z0-9/_\-.]+$/;

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

  // Strict path validation - prevent injection and traversal
  if (!SAFE_PATH_REGEX.test(path)) {
    return res.status(400).json({
      error: 'Validation error',
      message: 'path contains invalid characters (allowed: a-z A-Z 0-9 / _ - .)'
    });
  }

  // Prevent path traversal
  if (path.includes('..') || path.includes('//')) {
    return res.status(400).json({
      error: 'Validation error',
      message: 'path cannot contain ".." or "//" sequences'
    });
  }

  if (!data || typeof data !== 'object' || Array.isArray(data)) {
    return res.status(400).json({
      error: 'Validation error',
      message: 'data is required and must be an object'
    });
  }

  // Path already validated, no need to sanitize
  req.body.path = path.trim();

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

  // Strict path validation - prevent injection and traversal
  if (!SAFE_PATH_REGEX.test(path)) {
    return res.status(400).json({
      error: 'Validation error',
      message: 'path contains invalid characters (allowed: a-z A-Z 0-9 / _ - .)'
    });
  }

  // Prevent path traversal
  if (path.includes('..') || path.includes('//')) {
    return res.status(400).json({
      error: 'Validation error',
      message: 'path cannot contain ".." or "//" sequences'
    });
  }

  // Path already validated
  req.query.path = path.trim();

  next();
}
