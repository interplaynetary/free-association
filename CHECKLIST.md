# Post-Migration Checklist

## ✅ Migration Complete

- [x] Migrate schemas to `src/lib/server/schemas/`
- [x] Migrate AI proxy to `src/routes/api/ai/`
- [x] Migrate LLM router to `src/routes/api/llm/`
- [x] Migrate key pool to `src/routes/api/keys/`
- [x] Migrate middleware (auth, rate limiting)
- [x] Create flows system
- [x] Add jsonwebtoken dependency
- [x] Fix all TypeScript errors
- [x] Create documentation
- [x] Create `.env.example`

## 📋 Your Tasks

### Before First Run

- [ ] Install dependencies: `bun install`
- [ ] Copy `.env.example` to `.env`
- [ ] Generate JWT_SECRET: `openssl rand -base64 32`
- [ ] Generate MASTER_API_KEY: `openssl rand -hex 32`
- [ ] Add your OpenRouter keys to `.env`
- [ ] Review `SETUP_GUIDE.md`

### First Run

- [ ] Start server: `bun run dev`
- [ ] Test health endpoints:
  - [ ] `curl http://localhost:5173/api/ai/health`
  - [ ] `curl http://localhost:5173/api/llm/health`
  - [ ] `curl http://localhost:5173/api/keys/health`
- [ ] Check for console errors
- [ ] Verify key pool initialized

### Testing

- [ ] Generate a JWT token
- [ ] Make a test AI completion request
- [ ] Test rate limiting
- [ ] Test error responses
- [ ] Test with invalid auth
- [ ] Test all flow types

### Client Updates

- [ ] Update client API URLs from old Docker ports to `/api/*`
- [ ] Update authentication headers
- [ ] Test existing functionality
- [ ] Update any hardcoded URLs

### Gun/Holster Decision

- [ ] Read `GUN_HOLSTER_OPTIONS.md`
- [ ] Decide: Keep Docker / Use public relays / Migrate / Replace
- [ ] Update configuration accordingly
- [ ] Test data persistence

### Production Prep

- [ ] Generate strong production secrets
- [ ] Set `NODE_ENV=production`
- [ ] Configure CORS/allowed origins
- [ ] Set up monitoring
- [ ] Configure backup strategy
- [ ] Test production build: `bun run build`
- [ ] Deploy to hosting platform

### Cleanup

- [ ] Archive old `server/` folder
- [ ] Update README if needed
- [ ] Remove old Docker configurations (if not using Gun/Holster)
- [ ] Update deployment documentation

## 📖 Documentation Reference

| When you need to... | Read this |
|-------------------|-----------|
| Set up for the first time | `SETUP_GUIDE.md` |
| Understand what changed | `SERVER_MIGRATION.md` |
| Quick API reference | `API_QUICK_REFERENCE.md` |
| Decide on Gun/Holster | `GUN_HOLSTER_OPTIONS.md` |
| See migration status | `MIGRATION_COMPLETE.md` |

## 🆘 Troubleshooting

### Server won't start
- Check `.env` file exists
- Verify all required env vars are set
- Run `bun install` again

### "No OpenRouter keys available"
- Add keys to `OPENROUTER_KEYS` in `.env`
- Check key format: `sk-or-v1-...`
- Verify keys are valid

### "JWT_SECRET not configured"
- Add `JWT_SECRET` to `.env`
- Must be a long random string

### TypeScript errors
- Run `bun install`
- Check Bun version (should be latest)
- Try deleting `node_modules` and reinstalling

### Rate limit issues
- Limits are per-user or per-IP
- Wait for the retry period
- Or adjust limits in code

## ✨ Optional Enhancements

Consider these improvements:

- [ ] Add Redis for distributed rate limiting
- [ ] Set up Sentry for error tracking
- [ ] Add Prometheus metrics
- [ ] Implement API key rotation
- [ ] Add request logging to file/service
- [ ] Set up CI/CD pipeline
- [ ] Add integration tests
- [ ] Implement request caching
- [ ] Add API versioning
- [ ] Create admin dashboard

## 🎉 Success!

Once you've completed all the tasks above, your migration is complete and you're ready for production!

**Estimated Time**: 1-2 hours for setup and testing

**Questions?** Check the documentation files listed above.

