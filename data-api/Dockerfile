# Secure API Gateway Dockerfile
FROM node:20-alpine

# Set working directory
WORKDIR /app

# Copy package files
COPY package*.json ./

# Install dependencies
RUN npm install --production

# Copy all source code
COPY server.js ./
COPY middleware/ ./middleware/
COPY routes/ ./routes/
COPY utils/ ./utils/

# Expose API port
EXPOSE 8767

# Environment variables with defaults
ENV DATA_API_HOST=0.0.0.0
ENV DATA_API_PORT=8767
ENV NODE_ENV=production
ENV GUN_PEER_URL=http://gun-relay:8765/gun
ENV HOLSTER_PEER_URL=ws://holster-relay:8766/holster

# Security defaults (CHANGE IN PRODUCTION!)
ENV MASTER_API_KEY=CHANGE-ME-IN-PRODUCTION
ENV JWT_SECRET=CHANGE-ME-IN-PRODUCTION-MIN-32-CHARS
ENV ALLOWED_ORIGINS=http://localhost:5173
ENV DEBUG=false

# Health check
HEALTHCHECK --interval=30s --timeout=10s --start-period=10s --retries=3 \
  CMD node -e "require('http').get('http://localhost:8767/health', (r) => {process.exit(r.statusCode === 200 ? 0 : 1)})"

# Run as non-root user
USER node

# Start the server
CMD ["npm", "start"]
