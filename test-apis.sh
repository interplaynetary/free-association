#!/bin/bash
# Test all migrated API endpoints

PORT=${1:-5173}
BASE="http://localhost:$PORT"

echo "🧪 Testing SvelteKit APIs on port $PORT..."
echo ""

echo "=== 1. AI Proxy Health ==="
curl -s "$BASE/api/ai/health" && echo "" || echo "❌ Failed"
echo ""

echo "=== 2. AI Proxy Documentation ==="
curl -s "$BASE/api/ai" && echo "" || echo "❌ Failed"
echo ""

echo "=== 3. LLM Router Health ==="
curl -s "$BASE/api/llm/health" && echo "" || echo "❌ Failed"
echo ""

echo "=== 4. LLM Flows List ==="
curl -s "$BASE/api/llm/flows" && echo "" || echo "❌ Failed"
echo ""

echo "=== 5. Key Pool Health ==="
curl -s "$BASE/api/keys/health" && echo "" || echo "❌ Failed"
echo ""

echo "=== 6. Key Pool Status ==="
curl -s "$BASE/api/keys/status" && echo "" || echo "❌ Failed"
echo ""

echo "✅ All tests complete!"

