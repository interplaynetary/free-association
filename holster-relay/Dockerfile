# Holster Relay Server Dockerfile
FROM node:20-alpine

# Set working directory
WORKDIR /app

# Copy package files
COPY package*.json ./

# Install dependencies
RUN npm install --production

# Copy server code
COPY server.js ./

# Create data directory for Holster persistence
RUN mkdir -p holster-data

# Expose Holster relay port
EXPOSE 8766

# Environment variables with defaults
ENV HOLSTER_RELAY_HOST=0.0.0.0
ENV HOLSTER_RELAY_PORT=8766
ENV HOLSTER_RELAY_STORAGE=true
ENV HOLSTER_RELAY_STORAGE_PATH=./holster-data

# Health check
HEALTHCHECK --interval=30s --timeout=10s --start-period=5s --retries=3 \
  CMD node -e "require('http').get('http://localhost:8766/health', (r) => {process.exit(r.statusCode === 200 ? 0 : 1)})"

# Start the server
CMD ["npm", "start"]
