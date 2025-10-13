# Bun-based Key Pool Manager Dockerfile
FROM oven/bun:latest
WORKDIR /app

# Copy package manifest and TypeScript config
COPY package.json ./
COPY tsconfig.json ./

# Install dependencies (includes dev deps for types)
RUN bun install

# Copy code
COPY . .

EXPOSE 8769

ENV KEY_POOL_HOST=0.0.0.0
ENV KEY_POOL_PORT=8769
ENV HEALTH_CHECK_INTERVAL=30000

HEALTHCHECK --interval=30s --timeout=10s --start-period=5s --retries=3 \
  CMD curl -f http://localhost:8769/health || exit 1

# Run TypeScript server (Bun runs TS natively)
CMD ["bun", "run", "server.ts"]

