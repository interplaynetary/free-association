# Bun-based Holster Relay Dockerfile
FROM oven/bun:latest
WORKDIR /app

# Copy package manifest
COPY package.json ./

# Install dependencies
RUN bun install --production

# Copy code
COPY . .

EXPOSE 8766

ENV HOLSTER_RELAY_HOST=0.0.0.0
ENV HOLSTER_RELAY_PORT=8766
ENV HOLSTER_RELAY_STORAGE=true
ENV HOLSTER_RELAY_STORAGE_PATH=./holster-data

HEALTHCHECK --interval=30s --timeout=10s --start-period=5s --retries=3 \
  CMD curl -f http://localhost:8766/health || exit 1

CMD ["bun", "run", "server.js"]
