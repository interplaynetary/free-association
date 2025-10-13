# Bun-based LLM Router Dockerfile
FROM oven/bun:latest
WORKDIR /app

# Copy package manifest and TypeScript config
COPY package.json ./
COPY tsconfig.json ./

# Install dependencies (includes dev deps for TypeScript types)
RUN bun install

# Copy code
COPY . .

EXPOSE 8768

ENV LLM_ROUTER_HOST=0.0.0.0
ENV LLM_ROUTER_PORT=8768
ENV KEY_POOL_URL=http://key-pool:8769

HEALTHCHECK --interval=30s --timeout=10s --start-period=5s --retries=3 \
  CMD curl -f http://localhost:8768/health || exit 1

# Bun can run TypeScript directly without compilation
CMD ["bun", "run", "server-typed.ts"]

