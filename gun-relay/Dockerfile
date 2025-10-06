# Gun Relay Server Dockerfile
FROM node:20-alpine

# Set working directory
WORKDIR /app

# Copy package files
COPY package*.json ./

# Install dependencies
RUN npm install --production

# Copy server code
COPY server.js ./

# Create public directory for static files
RUN mkdir -p public

# Create store directory for Gun data persistence
RUN mkdir -p store && chown -R node:node /app

# Expose Gun relay port
EXPOSE 8765

# Environment variables with defaults
ENV GUN_RELAY_HOST=0.0.0.0
ENV GUN_RELAY_PORT=8765
ENV GUN_RELAY_STORE=true
ENV GUN_RELAY_PATH=public
ENV GUN_RELAY_SHOW_QR=false

# Health check
HEALTHCHECK --interval=30s --timeout=10s --start-period=5s --retries=3 \
  CMD node -e "require('http').get('http://localhost:8765/gun', (r) => {process.exit(r.statusCode === 200 ? 0 : 1)})"

# Run as non-root user
USER node

# Start the server
CMD ["npm", "start"]
