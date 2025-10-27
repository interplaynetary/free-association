import { defineConfig, devices } from '@playwright/test';

/**
 * Playwright Configuration for SvelteKit
 * @see https://playwright.dev/docs/test-configuration
 */
export default defineConfig({
	testDir: './tests',
	testMatch: '**/*.e2e.ts',
	
	// Run tests in files in parallel
	fullyParallel: true,
	
	// Fail the build on CI if you accidentally left test.only in the source code
	forbidOnly: !!process.env.CI,
	
	// Retry on CI only
	retries: process.env.CI ? 2 : 0,
	
	// Run tests in parallel on CI for faster execution
	workers: process.env.CI ? 4 : undefined,
	
	// Reporter to use
	reporter: 'html',
	
	// Shared settings for all the projects below
	use: {
		// Base URL to use in actions like `await page.goto('/')`
		baseURL: 'http://localhost:4173',
		
		// Collect trace when retrying the failed test
		trace: 'on-first-retry',
		
		// Screenshot on failure
		screenshot: 'only-on-failure',
	},

	// Configure projects for major browsers
	projects: [
		{
			name: 'chromium',
			use: { ...devices['Desktop Chrome'] },
		},

		{
			name: 'firefox',
			use: { ...devices['Desktop Firefox'] },
		},

		{
			name: 'webkit',
			use: { ...devices['Desktop Safari'] },
		},

		// Test against mobile viewports
		{
			name: 'Mobile Chrome',
			use: { ...devices['Pixel 5'] },
		},
		{
			name: 'Mobile Safari',
			use: { ...devices['iPhone 12'] },
		},
	],

	// Run your local dev server before starting the tests
	webServer: {
		command: 'bun run build && bun run preview',
		port: 4173,
		reuseExistingServer: !process.env.CI,
		timeout: 120 * 1000,
	},
});

