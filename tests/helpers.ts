import { type Page, expect } from '@playwright/test';

/**
 * Common helper functions for E2E tests
 */

/**
 * Wait for the page to be fully loaded and hydrated
 */
export async function waitForPageReady(page: Page) {
	await page.waitForLoadState('networkidle');
	await page.waitForLoadState('domcontentloaded');
	
	// Wait a bit for Svelte hydration
	await page.waitForTimeout(100);
}

/**
 * Check if the current page has a specific title
 */
export async function expectPageTitle(page: Page, title: string) {
	await expect(page).toHaveTitle(title);
}

/**
 * Check if a URL matches a pattern (useful for navigation tests)
 */
export async function expectURL(page: Page, pattern: string | RegExp) {
	if (typeof pattern === 'string') {
		expect(page.url()).toContain(pattern);
	} else {
		expect(page.url()).toMatch(pattern);
	}
}

/**
 * Navigate and wait for the page to be ready
 */
export async function navigateAndWait(page: Page, url: string) {
	await page.goto(url);
	await waitForPageReady(page);
}

/**
 * Click a link and wait for navigation
 */
export async function clickAndNavigate(page: Page, selector: string) {
	await page.locator(selector).click();
	await waitForPageReady(page);
}

/**
 * Fill a form field and verify the value
 */
export async function fillAndVerify(page: Page, selector: string, value: string) {
	const input = page.locator(selector);
	await input.fill(value);
	await expect(input).toHaveValue(value);
}

/**
 * Take a screenshot with a descriptive name
 */
export async function takeScreenshot(page: Page, name: string) {
	await page.screenshot({ path: `test-results/${name}.png`, fullPage: true });
}

/**
 * Check if an element is visible on the page
 */
export async function expectVisible(page: Page, selector: string) {
	await expect(page.locator(selector)).toBeVisible();
}

/**
 * Check if an element is hidden on the page
 */
export async function expectHidden(page: Page, selector: string) {
	await expect(page.locator(selector)).toBeHidden();
}

/**
 * Wait for a specific element to appear
 */
export async function waitForElement(page: Page, selector: string, timeout = 5000) {
	await page.locator(selector).waitFor({ state: 'visible', timeout });
}

/**
 * Check if the page has no console errors
 * Call this in beforeEach/afterEach to catch JS errors
 */
export function setupConsoleErrorTracking(page: Page): string[] {
	const consoleErrors: string[] = [];
	
	page.on('console', (msg) => {
		if (msg.type() === 'error') {
			consoleErrors.push(msg.text());
		}
	});
	
	page.on('pageerror', (error) => {
		consoleErrors.push(error.message);
	});
	
	return consoleErrors;
}

/**
 * Mock API responses for testing
 */
export async function mockAPIResponse(
	page: Page,
	urlPattern: string | RegExp,
	response: unknown
) {
	await page.route(urlPattern, (route) => {
		route.fulfill({
			status: 200,
			contentType: 'application/json',
			body: JSON.stringify(response)
		});
	});
}

/**
 * Set viewport to common device sizes
 */
export const viewports = {
	mobile: { width: 375, height: 667 },
	tablet: { width: 768, height: 1024 },
	desktop: { width: 1920, height: 1080 },
	desktopSmall: { width: 1366, height: 768 }
};

/**
 * Set a specific viewport
 */
export async function setViewport(page: Page, device: keyof typeof viewports) {
	await page.setViewportSize(viewports[device]);
}

