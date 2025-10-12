# Bun-based Gun Relay Dockerfile
FROM oven/bun:latest
WORKDIR /app

# Copy package manifest
COPY package.json ./

# Install dependencies
RUN bun install --production

# Copy code
COPY . .

# Expose relay port
EXPOSE 8765

# Env vars
ENV GUN_RELAY_HOST=0.0.0.0
ENV GUN_RELAY_PORT=8765

# Healthcheck (optional, curl must be available for health check)
HEALTHCHECK --interval=30s --timeout=10s --start-period=5s --retries=3 \
  CMD curl -f http://localhost:8765/gun || exit 1

CMD ["bun", "run", "server.js"]
