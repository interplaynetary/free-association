# E2E Tests with Playwright

This directory contains end-to-end tests for the application using Playwright.

## Setup

First, install the Playwright browsers:

```bash
bun run playwright:install
```

This command installs Chromium, Firefox, and WebKit browsers along with their system dependencies. You may need to enter your password for sudo access.

## Running Tests

### Run all tests (headless mode)
```bash
bun run test:e2e
```

### Run tests with UI mode (interactive)
```bash
bun run test:e2e:ui
```

### Run tests in headed mode (see the browser)
```bash
bun run test:e2e:headed
```

### Debug tests
```bash
bun run test:e2e:debug
```

## Test Configuration

The Playwright configuration is in `playwright.config.ts` at the project root.

Key features:
- **Test files**: `**/*.e2e.ts` in the `tests/` directory
- **Web server**: Automatically builds and serves the app on port 4173 before running tests
- **Browsers**: Tests run on Chromium, Firefox, WebKit, and mobile viewports
- **Parallel execution**: Tests run in parallel for faster execution
- **Retries**: Tests automatically retry on CI (2 retries)
- **Reports**: HTML reports are generated in `playwright-report/`
- **Screenshots**: Captured on test failure
- **Traces**: Recorded on first retry for debugging

## Writing Tests

Create new test files in this directory with the `.e2e.ts` extension:

```typescript
import { test, expect } from '@playwright/test';

test('my test', async ({ page }) => {
  await page.goto('/');
  await expect(page.locator('h1')).toBeVisible();
});
```

## Browser Testing

Tests run against:
- Desktop Chrome (Chromium)
- Desktop Firefox
- Desktop Safari (WebKit)
- Mobile Chrome (Pixel 5)
- Mobile Safari (iPhone 12)

To run tests on a specific browser:
```bash
bun run test:e2e --project=chromium
bun run test:e2e --project=firefox
bun run test:e2e --project=webkit
```

## Documentation

- [Playwright Documentation](https://playwright.dev/)
- [Playwright Best Practices](https://playwright.dev/docs/best-practices)
- [Playwright API Reference](https://playwright.dev/docs/api/class-playwright)

