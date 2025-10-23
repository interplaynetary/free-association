import { test, expect } from '@playwright/test';

/**
 * Example E2E test for the SvelteKit app
 * Tests basic navigation and page loading
 */

test.describe('Homepage', () => {
	test('should load the homepage successfully', async ({ page }) => {
		// Navigate to the homepage
		await page.goto('/');

		// Wait for the page to load
		await page.waitForLoadState('networkidle');

		// Check that the page title is set
		const title = await page.title();
		expect(title).toBeTruthy();
		expect(title.length).toBeGreaterThan(0);
	});

	test('should have basic HTML structure', async ({ page }) => {
		await page.goto('/');

		// Check for basic HTML elements
		const body = await page.locator('body');
		expect(await body.isVisible()).toBe(true);

		// Check that the page has rendered content
		const content = await body.textContent();
		expect(content).toBeTruthy();
		expect(content!.length).toBeGreaterThan(0);
	});
});

test.describe('Navigation', () => {
	test('should handle client-side navigation', async ({ page }) => {
		await page.goto('/');

		// Get all internal links
		const links = await page.locator('a[href^="/"]').all();

		if (links.length > 0) {
			// Click the first internal link if available
			const firstLink = links[0];
			const href = await firstLink.getAttribute('href');

			if (href && href !== '/') {
				await firstLink.click();
				await page.waitForLoadState('networkidle');

				// Verify navigation occurred
				expect(page.url()).toContain(href);
			}
		}
	});
});

test.describe('Responsive Design', () => {
	test('should render correctly on mobile viewport', async ({ page }) => {
		// Set mobile viewport
		await page.setViewportSize({ width: 375, height: 667 });

		await page.goto('/');
		await page.waitForLoadState('networkidle');

		// Check that the page is visible
		const body = await page.locator('body');
		expect(await body.isVisible()).toBe(true);
	});

	test('should render correctly on tablet viewport', async ({ page }) => {
		// Set tablet viewport
		await page.setViewportSize({ width: 768, height: 1024 });

		await page.goto('/');
		await page.waitForLoadState('networkidle');

		// Check that the page is visible
		const body = await page.locator('body');
		expect(await body.isVisible()).toBe(true);
	});

	test('should render correctly on desktop viewport', async ({ page }) => {
		// Set desktop viewport
		await page.setViewportSize({ width: 1920, height: 1080 });

		await page.goto('/');
		await page.waitForLoadState('networkidle');

		// Check that the page is visible
		const body = await page.locator('body');
		expect(await body.isVisible()).toBe(true);
	});
});

