# LLM Router Documentation Index

## 🚀 Quick Navigation

**New to Typed Flows?** Start here: [`QUICK_START.md`](./QUICK_START.md)

**Creating Custom Flows?** Read: [`FLOWS_GUIDE.md`](./FLOWS_GUIDE.md)

**Migrating from Legacy?** See: [`MIGRATION.md`](./MIGRATION.md)

**Architecture Details?** Check: [`TYPED_FLOWS_SUMMARY.md`](./TYPED_FLOWS_SUMMARY.md)

**API Reference?** Main docs: [`README.md`](./README.md)

## 📚 Documentation Overview

| Document | Purpose | Audience | Lines |
|----------|---------|----------|-------|
| **QUICK_START.md** | Get started in 5 minutes | Developers | 300+ |
| **FLOWS_GUIDE.md** | Create custom flows | Developers | 400+ |
| **TYPED_FLOWS_SUMMARY.md** | Implementation details | Technical | 400+ |
| **MIGRATION.md** | Legacy to typed migration | DevOps | 400+ |
| **README.md** | Main service docs | All | 150+ |
| **example-requests.json** | Request examples | Developers | 100+ |

**Total: 1,750+ lines of documentation**

## 🎯 By Use Case

### I Want To...

#### Start Using the Typed Router
1. Read [`QUICK_START.md`](./QUICK_START.md) (5 min)
2. Run `bun start`
3. Test with examples from [`example-requests.json`](./example-requests.json)

#### Create a Custom Flow
1. Read ["Creating Custom Flows"](./FLOWS_GUIDE.md#creating-a-new-flow)
2. Define Zod schema in [`schemas/requestTypes.ts`](./schemas/requestTypes.ts)
3. Create flow in [`flows/flowRegistry.ts`](./flows/flowRegistry.ts)
4. Test with curl

#### Understand the Architecture
1. Read [`TYPED_FLOWS_SUMMARY.md`](./TYPED_FLOWS_SUMMARY.md)
2. Review architecture diagrams
3. Explore source code in [`router.ts`](./router.ts)

#### Migrate from Legacy Router
1. Read [`MIGRATION.md`](./MIGRATION.md)
2. Choose migration strategy (full, gradual, or fallback)
3. Update requests to include `requestType`
4. Test both modes side-by-side

#### Integrate with My App
1. Read ["Integration"](./TYPED_FLOWS_SUMMARY.md#integration-with-existing-system)
2. Add `requestType` to your LLM requests
3. Use typed responses from router
4. Export Zod schemas for client-side validation

## 🏗️ Architecture Quick Reference

```
Request → Validation → Flow Selection → Prompt Gen → Model Selection → Response
   ↓          ↓              ↓              ↓              ↓             ↓
  JSON    Zod Schema   flowRegistry   promptTemplate  Key Pool   Routing Decision
```

## 📖 Code Examples

### Basic Request (General Chat)
```bash
curl -X POST http://localhost:8768/route \
  -H "Content-Type: application/json" \
  -d '{"requestType": "chat", "messages": [{"role": "user", "content": "Hello"}]}'
```

### Domain-Specific Request (Recognition Analysis)
```bash
curl -X POST http://localhost:8768/route \
  -H "Content-Type: application/json" \
  -d '{
    "requestType": "recognition-analysis",
    "players": [...],
    "analysisType": "mutual-recognition"
  }'
```

### Creating a Flow (TypeScript)
```typescript
// schemas/requestTypes.ts
export const MyFlowSchema = z.object({
  requestType: z.literal('my-flow'),
  input: z.string()
});

// flows/flowRegistry.ts
export const myFlow: FlowDefinition = {
  name: 'My Flow',
  requestType: 'my-flow',
  preferredModels: ['openai/gpt-4'],
  promptTemplate: (request) => ({
    user: request.input,
    temperature: 0.7
  })
};
```

## 🔧 Configuration

### Environment Variables
```bash
LLM_ROUTER_HOST=0.0.0.0      # Bind host
LLM_ROUTER_PORT=8768          # Port
KEY_POOL_URL=http://key-pool:8769  # Key pool service
```

### Run Modes
```bash
bun start              # Typed flows (default)
bun start:legacy       # Legacy JavaScript router
bun dev                # Development with auto-reload
```

## 📦 What's Included

### 5 Pre-Built Flows

1. **recognition-analysis** - Free-association network analysis
   - Models: Claude 3 Opus → GPT-4 → GPT-4 Turbo
   - Use: Mutual recognition, capacity flows, network health

2. **capacity-recommendation** - Surplus-to-needs matching
   - Models: Claude 3 Sonnet → GPT-4 → Claude 3 Opus
   - Use: Resource distribution recommendations

3. **code-generation** - Production-quality code
   - Models: GPT-4 → Claude 3 Sonnet → GPT-4 Turbo
   - Use: Algorithm implementation, utilities

4. **data-analysis** - Structured insights
   - Models: GPT-4 → Claude 3 Sonnet → GPT-4 Turbo
   - Use: Trends, anomalies, comparisons

5. **chat** - General conversation (fallback)
   - Models: GPT-3.5 Turbo → Claude 3 Haiku → Mistral Medium
   - Use: Q&A, explanations, general tasks

### Technologies

- **TypeScript** - Type safety and IDE support
- **Zod v3.23** - Runtime validation with type inference
- **LangChain** - Advanced prompt patterns (ready)
- **Bun** - Fast runtime with native TS support
- **Express** - HTTP server

## 🧪 Testing

### Verify Installation
```bash
curl http://localhost:8768/health
# {"status":"ok","service":"llm-router-typed","timestamp":...,"flowsAvailable":5}

curl http://localhost:8768/flows
# Lists all 5 flows with their configurations
```

### Test a Flow
```bash
# Use examples from example-requests.json
curl -X POST http://localhost:8768/route \
  -H "Content-Type: application/json" \
  -d @example-requests.json
```

## 🔍 Troubleshooting

### Schema Validation Error
**Problem**: Request rejected with Zod error  
**Solution**: Check request structure matches schema in `schemas/requestTypes.ts`

### No Providers Available
**Problem**: "No providers available for flow"  
**Solution**: Check Key Pool has keys for preferred models

### Type Errors
**Problem**: TypeScript compilation errors  
**Solution**: Run `bun install` to get latest types

### Legacy Mode Confusion
**Problem**: Unsure which mode is running  
**Solution**: Check health endpoint - returns `service: "llm-router-typed"` or `"llm-router"`

## 📊 Statistics

- **Files Created**: 13 (TS, JSON, MD, config)
- **Documentation**: 1,750+ lines across 6 files
- **Source Code**: ~600 lines of TypeScript
- **Flows Included**: 5 domain-specific + 1 fallback
- **Dependencies**: 128 packages (LangChain, Zod, Express, types)
- **Test Status**: ✅ All endpoints verified

## 🎓 Learning Path

### Beginner (30 minutes)
1. Read `QUICK_START.md`
2. Run the server
3. Test with example requests
4. Review included flows

### Intermediate (2 hours)
1. Read `FLOWS_GUIDE.md`
2. Create a simple custom flow
3. Test your flow
4. Explore prompt templates

### Advanced (1 day)
1. Read `TYPED_FLOWS_SUMMARY.md`
2. Study flow architecture
3. Create complex flows with LangChain
4. Integrate with your application
5. Add custom validation logic

## 🔗 Related Documentation

### In This Repository
- [`/server/ai-proxy/`](../ai-proxy/) - AI Proxy that uses this router
- [`/server/key-pool/`](../key-pool/) - Key Pool Manager for API keys
- [`/server/docker-compose.yml`](../docker-compose.yml) - Full stack orchestration

### External Resources
- [Zod Documentation](https://zod.dev)
- [LangChain TypeScript](https://js.langchain.com)
- [Bun Documentation](https://bun.sh/docs)
- [OpenRouter API](https://openrouter.ai/docs)

## 🆘 Getting Help

### Check These First
1. Error messages (Zod provides detailed diagnostics)
2. TypeScript compiler errors (very helpful)
3. Server logs (routing decisions logged)
4. Documentation for your use case

### Common Issues
- **Schema validation**: See `FLOWS_GUIDE.md` examples
- **Model selection**: Check Key Pool has keys
- **Prompt templates**: Review existing flows for patterns
- **Type errors**: Ensure `bun install` completed successfully

## 📝 Version Info

- **Router Version**: 1.0.0
- **TypeScript**: ES2022 target
- **Zod**: v3.23.8 (v4 compatible when released)
- **LangChain**: v0.3.x
- **Bun**: Compatible with latest

## ✅ Production Ready

- [x] Type-safe routing with Zod
- [x] 5 pre-built flows
- [x] Comprehensive documentation
- [x] Docker support
- [x] Legacy mode fallback
- [x] Health checks
- [x] Error handling
- [x] Validated and tested

## 🚦 Next Steps

Choose your path:

**👨‍💻 Developer Path**
→ Read `QUICK_START.md` → Create custom flow → Integrate

**🏗️ Architecture Path**
→ Read `TYPED_FLOWS_SUMMARY.md` → Study router.ts → Extend

**🔄 Migration Path**
→ Read `MIGRATION.md` → Test both modes → Gradually migrate

**📚 Deep Dive Path**
→ Read all docs → Explore LangChain → Build advanced flows

---

**Last Updated**: October 2025  
**Status**: ✅ Production Ready  
**Maintainer**: Free-Association Project

