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
	
	// Wait for Svelte hydration by checking for interactive elements
	try {
		await page.waitForSelector('body', { state: 'attached', timeout: 5000 });
		// Small delay to ensure Svelte components are hydrated
		await page.waitForTimeout(100);
	} catch (error) {
		console.warn('Page ready timeout, continuing anyway');
	}
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
 * Wait for authentication state to be ready
 */
export async function waitForAuth(page: Page, timeout = 10000) {
	try {
		await page.waitForSelector(
			'[data-testid="authenticated"], [data-testid="user-profile"], [data-testid="logout-button"]',
			{ state: 'visible', timeout }
		);
	} catch (error) {
		console.warn('Authentication indicators not found within timeout');
		throw error;
	}
}

/**
 * Wait for a network request to complete
 */
export async function waitForNetworkIdle(page: Page, timeout = 5000) {
	await page.waitForLoadState('networkidle', { timeout });
}

/**
 * Check if the current page has no console errors
 * Call this in beforeEach/afterEach to catch JS errors
 */
export function setupConsoleErrorTracking(page: Page): string[] {
	const consoleErrors: string[] = [];
	
	page.on('console', (msg) => {
		if (msg.type() === 'error') {
			const text = msg.text();
			// Filter out known harmless errors
			if (!text.includes('favicon') && !text.includes('DevTools')) {
				consoleErrors.push(text);
			}
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

/**
 * Wait for an element with retry logic
 */
export async function waitForElementWithRetry(
	page: Page,
	selector: string,
	maxAttempts = 3,
	timeout = 5000
): Promise<void> {
	for (let attempt = 1; attempt <= maxAttempts; attempt++) {
		try {
			await page.locator(selector).waitFor({ state: 'visible', timeout });
			return;
		} catch (error) {
			if (attempt === maxAttempts) {
				throw new Error(`Element ${selector} not found after ${maxAttempts} attempts`);
			}
			console.warn(`Attempt ${attempt} failed, retrying...`);
			await page.waitForTimeout(1000);
		}
	}
}

/**
 * Cleanup test data (can be extended)
 */
export async function cleanupTestData(page: Page) {
	// Clear local storage
	await page.evaluate(() => {
		localStorage.clear();
		sessionStorage.clear();
	});
	
	// Clear IndexedDB if needed
	await page.evaluate(() => {
		if (window.indexedDB) {
			indexedDB.databases?.().then((databases) => {
				databases.forEach((db) => {
					if (db.name) {
						indexedDB.deleteDatabase(db.name);
					}
				});
			});
		}
	});
}
