# E2E Tests with Playwright

This directory contains comprehensive end-to-end tests for the Free-Association application using Playwright.

## 📁 Test Structure

```
tests/
├── page-objects/          # Page Object Model (POM) pattern
│   ├── BasePage.ts       # Base class with common functionality
│   ├── AuthPage.ts       # Authentication flows
│   ├── RecognitionTreePage.ts  # Recognition tree operations
│   ├── CapacityPage.ts   # Capacity management
│   ├── SharesPage.ts     # Network shares and desires
│   ├── NavigationPage.ts # Navigation and UI
│   └── index.ts          # Barrel exports
├── auth.e2e.ts           # Authentication tests
├── recognition-tree.e2e.ts  # Recognition tree tests
├── capacity.e2e.ts       # Capacity management tests
├── shares.e2e.ts         # Network shares tests
├── navigation.e2e.ts     # Navigation tests
├── accessibility.e2e.ts  # Accessibility (WCAG 2.0 AA) tests
├── helpers.ts            # Test utilities and helpers
└── README.md            # This file
```

## 🚀 Setup

### First-Time Setup

Install Playwright browsers:

```bash
bun run playwright:install
```

This installs Chromium, Firefox, and WebKit browsers along with system dependencies.

### Dependencies

The project uses:
- `@playwright/test` - Playwright test runner
- `@axe-core/playwright` - Accessibility testing

All dependencies are in `package.json` and installed automatically with `bun install`.

## 🧪 Running Tests

### Run All Tests (Headless)
```bash
bun run test:e2e
```

### Run Tests with UI Mode (Interactive)
```bash
bun run test:e2e:ui
```

This opens Playwright's UI mode where you can:
- See all tests
- Run tests individually
- Watch tests in real-time
- Debug failures with time-travel

### Run Tests in Headed Mode (See Browser)
```bash
bun run test:e2e:headed
```

### Debug Tests
```bash
bun run test:e2e:debug
```

Opens Playwright Inspector for step-by-step debugging.

### Run Specific Test File
```bash
bunx playwright test auth.e2e.ts
```

### Run Specific Test
```bash
bunx playwright test auth.e2e.ts -g "should login"
```

### Run on Specific Browser
```bash
bun run test:e2e --project=chromium
bun run test:e2e --project=firefox
bun run test:e2e --project=webkit
```

## 🎯 Test Coverage

### Authentication (`auth.e2e.ts`)
- **Flows**: Login, signup, logout, session persistence
- **Edge Cases**: Invalid credentials, empty fields, special characters
- **Responsive**: Mobile, tablet viewports

### Recognition Tree (`recognition-tree.e2e.ts`)
- **Operations**: Create, edit, delete nodes
- **Contributors**: Add, remove contributors and anti-contributors
- **Calculations**: Recognition share calculations
- **Manual Fulfillment**: Setting and testing manual fulfillment percentages

### Capacity Management (`capacity.e2e.ts`)
- **CRUD**: Create, read, update, delete capacities
- **Slots**: Availability slot configuration
- **Filters**: Trust filters, location filters, subtree filters
- **Edge Cases**: Zero quantity, large quantities, long names

### Network Shares (`shares.e2e.ts`)
- **Browsing**: View network capacities
- **Desire Expression**: Express and update desires
- **Allocations**: View allocation status, mutual tier indicators
- **Map View**: Geographic browsing, markers, side panels

### Navigation (`navigation.e2e.ts`)
- **View Switching**: Tree, map, inventory views
- **Special Pages**: Collective, decider, unconference
- **Mobile**: Mobile menu, responsive navigation
- **Browser Navigation**: Back/forward, reload

### Accessibility (`accessibility.e2e.ts`)
- **WCAG 2.0 AA Compliance**: Using axe-core
- **Keyboard Navigation**: Tab, Enter, Escape support
- **Screen Readers**: Headings, alt text, labels, landmarks
- **Color Contrast**: Automated contrast checking
- **Mobile Touch Targets**: 44x44px minimum size

## 🏗️ Page Object Model (POM)

Tests use the Page Object Model pattern for maintainability and reusability.

### BasePage

All page objects extend `BasePage`, which provides:

```typescript
class BasePage {
  goto(path: string): Promise<void>
  waitForPageReady(): Promise<void>
  waitForElement(selector: string): Promise<Locator>
  clickAndNavigate(selector: string): Promise<void>
  fillField(selector: string, value: string): Promise<void>
  isVisible(selector: string): Promise<boolean>
  getTextContent(selector: string): Promise<string>
  screenshot(name: string): Promise<void>
}
```

### Example Usage

```typescript
import { AuthPage, RecognitionTreePage } from './page-objects';

test('user flow', async ({ page }) => {
  const authPage = new AuthPage(page);
  const treePage = new RecognitionTreePage(page);
  
  // Authenticate
  await authPage.gotoAuth();
  await authPage.signup('testuser', 'password');
  
  // Navigate to tree
  await treePage.gotoTreeView();
  await treePage.addChildNode('Root', 'Healthcare', 70);
});
```

## 📝 Writing New Tests

### Test Structure

```typescript
import { test, expect } from '@playwright/test';
import { YourPage } from './page-objects';

test.describe('Feature Name', () => {
  let yourPage: YourPage;

  test.beforeEach(async ({ page }) => {
    yourPage = new YourPage(page);
    // Setup common to all tests
  });

  test('should do something', async ({ page }) => {
    // Arrange
    await yourPage.goto('/path');
    
    // Act
    await yourPage.doSomething();
    
    // Assert
    expect(await yourPage.getSomething()).toBe('expected');
  });
});
```

### Best Practices

1. **Use data-testid attributes**
   ```html
   <button data-testid="save-button">Save</button>
   ```
   
2. **Prefer explicit waits over timeouts**
   ```typescript
   // Good ✅
   await expect(page.locator('[data-testid="success"]')).toBeVisible();
   
   // Avoid ❌
   await page.waitForTimeout(2000);
   ```

3. **Use Page Objects for selectors**
   - Keep all selectors in page objects
   - Tests should not contain CSS selectors
   
4. **Write descriptive test names**
   ```typescript
   test('should display error when login fails with invalid credentials', ...)
   ```

5. **Clean up test data**
   ```typescript
   test.afterEach(async () => {
     await cleanupTestData(page);
   });
   ```

## 🌐 Browser Testing

Tests run against:
- **Desktop Chrome** (Chromium)
- **Desktop Firefox**
- **Desktop Safari** (WebKit)
- **Mobile Chrome** (Pixel 5)
- **Mobile Safari** (iPhone 12)

### Custom Viewports

```typescript
import { setViewport } from './helpers';

test('mobile test', async ({ page }) => {
  await setViewport(page, 'mobile');
  // Test continues...
});
```

## 🔧 Configuration

Configuration is in `playwright.config.ts`:

- **Base URL**: `http://localhost:4173`
- **Parallel execution**: 4 workers on CI, unlimited locally
- **Retries**: 2 on CI, 0 locally
- **Screenshots**: On failure
- **Traces**: On first retry
- **Timeout**: 30 seconds per test

### Environment Variables

- `CI=true` - Enables CI-specific settings (retries, parallel workers)

## 🤖 CI/CD Integration

Tests run automatically on GitHub Actions:

### Workflows

1. **Main Tests** (`.github/workflows/playwright.yml`)
   - Runs on: Push to main/master/capacities, PRs
   - Browsers: All (Chromium, Firefox, WebKit, mobile)
   - Duration: ~10-15 minutes

2. **Accessibility Tests**
   - Separate job for accessibility testing
   - Uses Chromium only
   - Duration: ~5 minutes

3. **Visual Regression** (optional)
   - Screenshot comparison tests
   - Tag tests with `@visual` to include

### Artifacts

- **Playwright Report**: HTML report with test results
- **Test Failures**: Screenshots, videos, and traces (on failure)
- **Retention**: Reports for 30 days, failure artifacts for 7 days

### PR Comments

Test results automatically post to PRs with:
- Pass/fail summary
- Links to detailed reports
- Failure screenshots

## 🐛 Debugging

### Using Playwright Inspector

```bash
bun run test:e2e:debug
```

Features:
- Step through tests
- See selector highlights
- Time-travel through actions
- Console logs

### Using Trace Viewer

After a test failure, open the trace:

```bash
bunx playwright show-trace test-results/.../trace.zip
```

### Using VS Code Extension

Install "Playwright Test for VSCode" extension for:
- Run tests from editor
- Set breakpoints
- View test results inline

## 📊 Test Reports

### HTML Report

After running tests:

```bash
bunx playwright show-report
```

Opens interactive HTML report with:
- Test status and duration
- Error messages and stack traces
- Screenshots and videos
- Trace files for debugging

### CI Reports

Reports are uploaded to GitHub Actions artifacts and accessible via:
- Actions tab → Workflow run → Artifacts section
- Direct link in PR comment

## 🔍 Selectors

### Selector Priority

1. **data-testid** (preferred)
   ```typescript
   page.locator('[data-testid="login-button"]')
   ```

2. **Role-based**
   ```typescript
   page.getByRole('button', { name: 'Login' })
   ```

3. **Text-based** (fallback)
   ```typescript
   page.locator('button:has-text("Login")')
   ```

### Adding Test IDs

To improve test reliability, add `data-testid` attributes to interactive elements:

```svelte
<button data-testid="save-button" on:click={save}>
  Save
</button>
```

## 🚨 Troubleshooting

### Tests are flaky

1. **Add explicit waits**:
   ```typescript
   await expect(element).toBeVisible();
   ```

2. **Check network requests**:
   ```typescript
   await page.waitForLoadState('networkidle');
   ```

3. **Increase timeout for slow operations**:
   ```typescript
   await element.waitFor({ timeout: 10000 });
   ```

### Element not found

1. Check selector is correct
2. Ensure element is rendered (not in conditional)
3. Wait for element to appear
4. Check if element is in shadow DOM

### Tests fail on CI but pass locally

1. Check CI logs for specific errors
2. Verify environment variables
3. Check timing issues (add waits)
4. Download CI artifacts for debugging

## 📚 Resources

- [Playwright Documentation](https://playwright.dev/)
- [Playwright Best Practices](https://playwright.dev/docs/best-practices)
- [Playwright API Reference](https://playwright.dev/docs/api/class-playwright)
- [WCAG 2.0 Guidelines](https://www.w3.org/WAI/WCAG21/quickref/)
- [Axe-core Rules](https://github.com/dequelabs/axe-core/blob/develop/doc/rule-descriptions.md)

## 🤝 Contributing

When adding new tests:

1. Use Page Object Model pattern
2. Add proper `data-testid` attributes to UI components
3. Include accessibility checks where relevant
4. Test on multiple viewports if UI changes
5. Document new page objects and helpers
6. Update this README if adding new test categories

## 📞 Support

For issues or questions:
1. Check this README
2. Review existing tests for examples
3. Consult Playwright documentation
4. Ask in project Slack/Discord
