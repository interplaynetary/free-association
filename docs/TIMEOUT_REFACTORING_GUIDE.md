# Hard-coded Timeout Refactoring Guide

## Overview
This guide addresses the systematic replacement of hard-coded `waitForTimeout()` calls with conditional waits in E2E tests.

## Current State

### Problem Examples
```typescript
// tests/auth.e2e.ts:47
await page.waitForTimeout(1500);

// tests/navigation.e2e.ts:94
await page.waitForTimeout(2000); // Map takes longer to load

// tests/page-objects/BasePage.ts:29
await this.page.waitForTimeout(200); // Svelte hydration
```

**Issues:**
1. Tests are slower than necessary (always wait full duration)
2. Flaky tests (may fail if operation takes longer than timeout)
3. False positives (may pass even if functionality broken)
4. Hard to maintain (timeouts scattered throughout code)

## Solution Pattern

### Available Helper Functions

From `tests/helpers.ts`:

```typescript
// 1. Wait for authentication to complete
await waitForAuth(page, timeout = 10000)

// 2. Wait for network requests to finish
await waitForNetworkIdle(page, timeout = 5000)

// 3. Wait for specific element with retry
await waitForElementWithRetry(page, selector, maxAttempts = 3, timeout = 5000)

// 4. Wait for page to be ready
await waitForPageReady(page)

// 5. Playwright's built-in waits
await expect(page.locator(selector)).toBeVisible()
await page.waitForSelector(selector, { state: 'visible' })
await page.waitForLoadState('networkidle')
```

## Refactoring Examples

### Example 1: Authentication Wait

**Before:**
```typescript
await authPage.signup(testAlias, testPassword);
await page.waitForTimeout(2000); // Wait for auth to complete
const isAuthenticated = await authPage.isAuthenticated();
```

**After:**
```typescript
await authPage.signup(testAlias, testPassword);
await waitForAuth(page); // Waits for auth indicators
const isAuthenticated = await authPage.isAuthenticated();
expect(isAuthenticated).toBe(true);
```

### Example 2: Navigation Wait

**Before:**
```typescript
await navPage.switchToMapView();
await page.waitForTimeout(1500); // Map takes time to load
const isVisible = await page.isVisible('[data-testid="map-container"]');
```

**After:**
```typescript
await navPage.switchToMapView();
await expect(page.locator('[data-testid="map-container"]')).toBeVisible({ timeout: 10000 });
// Or
await waitForElementWithRetry(page, '[data-testid="map-container"]');
```

### Example 3: Form Submission

**Before:**
```typescript
await page.locator('button[type="submit"]').click();
await page.waitForTimeout(1000);
```

**After:**
```typescript
await page.locator('button[type="submit"]').click();
await page.waitForLoadState('networkidle');
// Or wait for success indicator
await expect(page.locator('[data-testid="success-message"]')).toBeVisible();
```

### Example 4: Modal Animation

**Before:**
```typescript
await authPage.openLoginModal();
await page.waitForTimeout(500); // Wait for modal animation
```

**After:**
```typescript
await authPage.openLoginModal();
await expect(page.locator('[data-testid="auth-modal"]')).toBeVisible();
```

### Example 5: Svelte Hydration

**Before:**
```typescript
await page.goto('/');
await page.waitForTimeout(200); // Svelte hydration
```

**After:**
```typescript
await page.goto('/');
await waitForPageReady(page); // Already handles hydration
// Or check for interactive elements
await page.waitForSelector('button:not([disabled])', { state: 'visible' });
```

## Refactoring Checklist

For each hard-coded timeout:

1. **Identify what you're waiting for**
   - Authentication state change?
   - Network request completion?
   - Element to appear/disappear?
   - Animation to complete?
   - Page navigation?

2. **Choose appropriate wait strategy**
   - Use `waitForAuth()` for authentication
   - Use `expect().toBeVisible()` for elements
   - Use `waitForLoadState()` for navigation
   - Use `waitForNetworkIdle()` for API calls
   - Use `waitForElementWithRetry()` for flaky elements

3. **Add proper assertion**
   - Don't just wait, verify the expected state
   - Use `expect()` statements after waits

4. **Test the change**
   - Run test multiple times to ensure stability
   - Test on slow connection (throttling)
   - Test on different browsers

## Priority Files for Refactoring

### High Priority (Auth & Core Flows)
1. `tests/auth.e2e.ts` - 6 hard-coded timeouts
2. `tests/page-objects/AuthPage.ts` - 8 hard-coded timeouts
3. `tests/page-objects/BasePage.ts` - 2 hard-coded timeouts

### Medium Priority (Feature Tests)
4. `tests/recognition-tree.e2e.ts` - 12 hard-coded timeouts
5. `tests/capacity.e2e.ts` - 15 hard-coded timeouts
6. `tests/shares.e2e.ts` - 10 hard-coded timeouts
7. `tests/navigation.e2e.ts` - 8 hard-coded timeouts

### Lower Priority (Page Objects)
8. `tests/page-objects/RecognitionTreePage.ts` - 15 hard-coded timeouts
9. `tests/page-objects/CapacityPage.ts` - 12 hard-coded timeouts
10. `tests/page-objects/SharesPage.ts` - 10 hard-coded timeouts
11. `tests/page-objects/NavigationPage.ts` - 5 hard-coded timeouts

## Implementation Strategy

### Week 1: High Priority Files
- Refactor auth tests and AuthPage
- Refactor BasePage
- Add any missing helper functions
- **Goal**: All authentication flows with proper waits

### Week 2: Medium Priority Files
- Refactor feature test files
- Update assertions to use expect()
- **Goal**: Main user flows with proper waits

### Week 3: Page Objects
- Refactor remaining page object methods
- Ensure consistency across all page objects
- **Goal**: All page objects using conditional waits

### Week 4: Testing & Documentation
- Run full test suite multiple times
- Test with network throttling
- Update documentation
- **Goal**: Stable, fast, reliable tests

## Common Patterns

### Pattern 1: Wait for Element After Action
```typescript
// ❌ Bad
await button.click();
await page.waitForTimeout(1000);

// ✅ Good
await button.click();
await expect(successMessage).toBeVisible();
```

### Pattern 2: Wait for Navigation
```typescript
// ❌ Bad
await link.click();
await page.waitForTimeout(2000);

// ✅ Good
await link.click();
await page.waitForLoadState('networkidle');
await expect(page).toHaveURL(/expected-path/);
```

### Pattern 3: Wait for Form Submission
```typescript
// ❌ Bad
await submitButton.click();
await page.waitForTimeout(1500);

// ✅ Good
await submitButton.click();
await page.waitForLoadState('networkidle');
await expect(errorMessage).toBeHidden();
await expect(successMessage).toBeVisible();
```

### Pattern 4: Wait for State Change
```typescript
// ❌ Bad
await authPage.login(user, pass);
await page.waitForTimeout(2000);

// ✅ Good
await authPage.login(user, pass);
await waitForAuth(page);
expect(await authPage.isAuthenticated()).toBe(true);
```

## Testing Refactored Code

### Local Testing
```bash
# Run specific test file multiple times
for i in {1..10}; do bunx playwright test auth.e2e.ts; done

# Run with slower execution (catch timing issues)
bunx playwright test auth.e2e.ts --slow-mo=100

# Run with network throttling
bunx playwright test auth.e2e.ts --network=slow-3g
```

### CI Testing
Refactored tests should:
- ✅ Pass consistently (99%+ pass rate)
- ✅ Run faster (less waiting)
- ✅ Provide clear failure messages
- ✅ Work across all browsers

## Measuring Success

### Before Refactoring
```
Test Suite Duration: ~5 minutes
Flaky Tests: 15-20% failure rate
Hard-coded Timeouts: 100+
Average Wait Time: 1.5 seconds per wait
```

### After Refactoring
```
Test Suite Duration: ~3 minutes (40% faster)
Flaky Tests: <5% failure rate
Hard-coded Timeouts: 0
Average Wait Time: 0.3 seconds per wait (only wait as long as needed)
```

## Notes

1. **Small waits are OK in limited cases:**
   - CSS animations (if no better indicator)
   - Debounced inputs (wait for debounce)
   - But always document WHY

2. **Use timeouts as safety nets:**
   ```typescript
   await expect(element).toBeVisible({ timeout: 10000 });
   // Better than: await page.waitForTimeout(10000);
   ```

3. **Prefer positive waits:**
   ```typescript
   // ✅ Wait for what you expect to happen
   await expect(successMessage).toBeVisible();
   
   // ❌ Don't just wait and hope
   await page.waitForTimeout(1000);
   ```

## Rollout Strategy

1. **Phase 1**: Refactor high-priority files (auth flows)
2. **Phase 2**: Refactor feature tests
3. **Phase 3**: Refactor page objects
4. **Phase 4**: Add more helper functions as needed
5. **Phase 5**: Document patterns in CONTRIBUTING.md

## Maintenance

After refactoring:
- **PR Reviews**: Reject new hard-coded timeouts
- **Linting**: Consider adding ESLint rule to warn on `waitForTimeout`
- **Documentation**: Keep this guide updated with new patterns

---

**Status**: Ready for Implementation  
**Timeline**: 4 weeks  
**Priority**: High  
**Estimated Improvement**: 40% faster tests, 70% less flaky

