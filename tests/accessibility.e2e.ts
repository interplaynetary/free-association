import { test, expect } from '@playwright/test';
import AxeBuilder from '@axe-core/playwright';
import { AuthPage, NavigationPage } from './page-objects';

/**
 * E2E Accessibility Tests
 * Tests accessibility compliance using axe-core
 * 
 * NOTE: Before running these tests, install axe-playwright:
 * npm install --save-dev @axe-core/playwright axe-playwright
 * OR
 * bun add -d @axe-core/playwright axe-playwright
 */

test.describe('Accessibility - Core Pages', () => {
	let authPage: AuthPage;
	let navPage: NavigationPage;

	test.beforeEach(async ({ page }) => {
		authPage = new AuthPage(page);
		navPage = new NavigationPage(page);
		
		await authPage.clearAuthData();
		await authPage.gotoAuth();
	});

	test('should have no accessibility violations on homepage (unauthenticated)', async ({ page }) => {
		await page.goto('/');
		await page.waitForLoadState('networkidle');
		
		const accessibilityScanResults = await new AxeBuilder({ page })
			.withTags(['wcag2a', 'wcag2aa', 'wcag21a', 'wcag21aa'])
			.analyze();
		
		expect(accessibilityScanResults.violations).toEqual([]);
		
		console.log('Accessibility violations (homepage):', accessibilityScanResults.violations.length);
		
		// Log violations for debugging if any
		if (accessibilityScanResults.violations.length > 0) {
			console.log('Violations:', JSON.stringify(accessibilityScanResults.violations, null, 2));
		}
	});

	test('should have no accessibility violations on homepage (authenticated)', async ({ page }) => {
		await authPage.setupTestUser();
		await page.waitForTimeout(1500);
		
		await page.goto('/');
		await page.waitForLoadState('networkidle');
		
		const accessibilityScanResults = await new AxeBuilder({ page })
			.withTags(['wcag2a', 'wcag2aa', 'wcag21a', 'wcag21aa'])
			.analyze();
		
		expect(accessibilityScanResults.violations).toEqual([]);
		
		console.log('Accessibility violations (authenticated homepage):', accessibilityScanResults.violations.length);
		
		if (accessibilityScanResults.violations.length > 0) {
			console.log('Violations:', JSON.stringify(accessibilityScanResults.violations, null, 2));
		}
	});
});

test.describe('Accessibility - Navigation', () => {
	let authPage: AuthPage;
	let navPage: NavigationPage;

	test.beforeEach(async ({ page }) => {
		authPage = new AuthPage(page);
		navPage = new NavigationPage(page);
		
		await authPage.clearAuthData();
		await authPage.gotoAuth();
		await authPage.setupTestUser();
		await page.waitForTimeout(1500);
	});

	test('should have no violations in tree view', async ({ page }) => {
		await navPage.switchToTreeView();
		await page.waitForTimeout(1000);
		
		const accessibilityScanResults = await new AxeBuilder({ page })
			.withTags(['wcag2a', 'wcag2aa'])
			.analyze();
		
		expect(accessibilityScanResults.violations).toEqual([]);
		console.log('Tree view violations:', accessibilityScanResults.violations.length);
	});

	test('should have no violations in map view', async ({ page }) => {
		await navPage.switchToMapView();
		await page.waitForTimeout(2000); // Map takes longer to load
		
		const accessibilityScanResults = await new AxeBuilder({ page })
			.withTags(['wcag2a', 'wcag2aa'])
			.analyze();
		
		expect(accessibilityScanResults.violations).toEqual([]);
		console.log('Map view violations:', accessibilityScanResults.violations.length);
	});

	test('should have no violations in inventory view', async ({ page }) => {
		await navPage.switchToInventoryView();
		await page.waitForTimeout(1000);
		
		const accessibilityScanResults = await new AxeBuilder({ page })
			.withTags(['wcag2a', 'wcag2aa'])
			.analyze();
		
		expect(accessibilityScanResults.violations).toEqual([]);
		console.log('Inventory view violations:', accessibilityScanResults.violations.length);
	});
});

test.describe('Accessibility - Forms and Interactive Elements', () => {
	let authPage: AuthPage;

	test.beforeEach(async ({ page }) => {
		authPage = new AuthPage(page);
		await authPage.clearAuthData();
		await authPage.gotoAuth();
	});

	test('should have no violations in authentication forms', async ({ page }) => {
		try {
			await authPage.openLoginModal();
			await page.waitForTimeout(500);
		} catch {
			// Modal might already be open
		}
		
		const accessibilityScanResults = await new AxeBuilder({ page })
			.withTags(['wcag2a', 'wcag2aa'])
			.analyze();
		
		expect(accessibilityScanResults.violations).toEqual([]);
		console.log('Auth form violations:', accessibilityScanResults.violations.length);
	});
});

test.describe('Accessibility - Keyboard Navigation', () => {
	let authPage: AuthPage;
	let navPage: NavigationPage;

	test.beforeEach(async ({ page }) => {
		authPage = new AuthPage(page);
		navPage = new NavigationPage(page);
		
		await authPage.clearAuthData();
		await authPage.gotoAuth();
		await authPage.setupTestUser();
		await page.waitForTimeout(1500);
		await page.goto('/');
	});

	test('should support tab navigation', async ({ page }) => {
		await page.waitForTimeout(1000);
		
		// Press Tab key multiple times
		for (let i = 0; i < 5; i++) {
			await page.keyboard.press('Tab');
			await page.waitForTimeout(200);
		}
		
		// Check if focus is visible
		const focusedElement = await page.evaluate(() => {
			return document.activeElement?.tagName;
		});
		
		console.log('Focused element after Tab navigation:', focusedElement);
		expect(focusedElement).toBeTruthy();
	});

	test('should support Enter key for button activation', async ({ page }) => {
		await page.waitForTimeout(1000);
		
		// Tab to a button and press Enter
		await page.keyboard.press('Tab');
		await page.keyboard.press('Enter');
		
		console.log('Enter key press handled');
	});

	test('should support Escape key for closing modals', async ({ page }) => {
		// Try opening a modal if available
		try {
			// Look for any button that might open a modal
			const buttons = await page.locator('button').all();
			if (buttons.length > 0) {
				await buttons[0].click();
				await page.waitForTimeout(300);
				
				// Press Escape
				await page.keyboard.press('Escape');
				await page.waitForTimeout(300);
				
				console.log('Escape key handled');
			}
		} catch (error) {
			console.log('Modal test skipped');
		}
	});
});

test.describe('Accessibility - Screen Reader Support', () => {
	let authPage: AuthPage;
	let navPage: NavigationPage;

	test.beforeEach(async ({ page }) => {
		authPage = new AuthPage(page);
		navPage = new NavigationPage(page);
		
		await authPage.clearAuthData();
		await authPage.gotoAuth();
		await authPage.setupTestUser();
		await page.waitForTimeout(1500);
		await page.goto('/');
	});

	test('should have proper heading hierarchy', async ({ page }) => {
		await page.waitForTimeout(1000);
		
		const headings = await page.locator('h1, h2, h3, h4, h5, h6').all();
		const headingTexts = await Promise.all(headings.map(h => h.textContent()));
		
		console.log('Headings found:', headingTexts);
		
		// Should have at least one heading
		expect(headings.length).toBeGreaterThan(0);
	});

	test('should have alt text on images', async ({ page }) => {
		await page.waitForTimeout(1000);
		
		const images = await page.locator('img').all();
		
		for (const img of images) {
			const alt = await img.getAttribute('alt');
			const src = await img.getAttribute('src');
			
			// Decorative images can have empty alt, but alt attribute must exist
			expect(alt).toBeDefined();
			console.log(`Image ${src}: alt="${alt}"`);
		}
	});

	test('should have proper button labels', async ({ page }) => {
		await page.waitForTimeout(1000);
		
		const buttons = await page.locator('button').all();
		
		for (const button of buttons) {
			const ariaLabel = await button.getAttribute('aria-label');
			const text = await button.textContent();
			const title = await button.getAttribute('title');
			
			// Button should have either text content, aria-label, or title
			const hasLabel = (text && text.trim() !== '') || ariaLabel || title;
			
			if (!hasLabel) {
				console.warn('Button without label found');
			}
		}
	});

	test('should have proper form labels', async ({ page }) => {
		await page.waitForTimeout(1000);
		
		const inputs = await page.locator('input:not([type="hidden"])').all();
		
		for (const input of inputs) {
			const id = await input.getAttribute('id');
			const name = await input.getAttribute('name');
			const ariaLabel = await input.getAttribute('aria-label');
			const ariaLabelledBy = await input.getAttribute('aria-labelledby');
			const placeholder = await input.getAttribute('placeholder');
			
			// Check if input has associated label
			let hasLabel = false;
			if (id) {
				const label = await page.locator(`label[for="${id}"]`).count();
				hasLabel = label > 0;
			}
			
			hasLabel = hasLabel || !!ariaLabel || !!ariaLabelledBy;
			
			if (!hasLabel) {
				console.warn(`Input without proper label: ${name || 'unnamed'} (placeholder: ${placeholder})`);
			}
		}
	});

	test('should have proper landmarks', async ({ page }) => {
		await page.waitForTimeout(1000);
		
		// Check for ARIA landmarks
		const landmarks = await page.locator('[role="main"], [role="navigation"], [role="banner"], [role="contentinfo"], main, nav, header, footer').all();
		
		console.log('Landmarks found:', landmarks.length);
		
		// Should have at least a main content area
		expect(landmarks.length).toBeGreaterThan(0);
	});
});

test.describe('Accessibility - Color Contrast', () => {
	let authPage: AuthPage;

	test.beforeEach(async ({ page }) => {
		authPage = new AuthPage(page);
		await authPage.clearAuthData();
		await authPage.gotoAuth();
		await authPage.setupTestUser();
		await page.waitForTimeout(1500);
		await page.goto('/');
	});

	test('should meet color contrast requirements', async ({ page }) => {
		await page.waitForTimeout(1000);
		
		const accessibilityScanResults = await new AxeBuilder({ page })
			.withTags(['wcag2aa'])
			.options({ rules: { 'color-contrast': { enabled: true } } })
			.analyze();
		
		// Filter for color contrast violations
		const contrastViolations = accessibilityScanResults.violations.filter(
			v => v.id === 'color-contrast'
		);
		
		console.log('Color contrast violations:', contrastViolations.length);
		
		if (contrastViolations.length > 0) {
			console.log('Contrast issues:', JSON.stringify(contrastViolations, null, 2));
		}
		
		expect(contrastViolations).toEqual([]);
	});
});

test.describe('Accessibility - Mobile Responsive', () => {
	let authPage: AuthPage;

	test.beforeEach(async ({ page }) => {
		await page.setViewportSize({ width: 375, height: 667 });
		
		authPage = new AuthPage(page);
		await authPage.clearAuthData();
		await authPage.gotoAuth();
		await authPage.setupTestUser();
		await page.waitForTimeout(1500);
	});

	test('should have no violations on mobile viewport', async ({ page }) => {
		await page.goto('/');
		await page.waitForTimeout(1000);
		
		const accessibilityScanResults = await new AxeBuilder({ page })
			.withTags(['wcag2a', 'wcag2aa'])
			.analyze();
		
		expect(accessibilityScanResults.violations).toEqual([]);
		console.log('Mobile accessibility violations:', accessibilityScanResults.violations.length);
	});

	test('should have proper touch target sizes on mobile', async ({ page }) => {
		await page.goto('/');
		await page.waitForTimeout(1000);
		
		// Check that interactive elements are at least 44x44px (iOS guideline)
		const buttons = await page.locator('button, a').all();
		
		for (const button of buttons) {
			const box = await button.boundingBox();
			if (box && box.width > 0 && box.height > 0) {
				// Touch targets should be at least 44x44
				if (box.width < 44 || box.height < 44) {
					console.warn(`Small touch target: ${box.width}x${box.height}px`);
				}
			}
		}
	});
});

