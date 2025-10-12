# Bun-based AI Proxy Dockerfile
FROM oven/bun:latest
WORKDIR /app

# Copy package manifest
COPY package.json ./

# Install dependencies with bun
RUN bun install --production

# Copy all app code
COPY . .

# Expose the API port
EXPOSE 8767

# Environment variables
ENV DATA_API_HOST=0.0.0.0
ENV DATA_API_PORT=8767

# Healthcheck using Bun's HTTP
HEALTHCHECK --interval=30s --timeout=10s --start-period=5s --retries=3 \
  CMD curl -f http://localhost:8767/health || exit 1

# Run the server
CMD ["bun", "run", "server.js"]
