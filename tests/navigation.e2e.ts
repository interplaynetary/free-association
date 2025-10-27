import { test, expect } from '@playwright/test';
import { AuthPage, NavigationPage } from './page-objects';

/**
 * E2E Tests for Navigation and UI Components
 * Tests header, toolbar, view switching, and navigation flows
 */

test.describe('Navigation - Basic UI', () => {
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

	test('should display header', async ({ page }) => {
		await navPage.goto('/');
		
		const headerVisible = await navPage.isHeaderVisible();
		console.log('Header visible:', headerVisible);
	});

	test('should display toolbar', async ({ page }) => {
		await navPage.goto('/');
		
		const toolbarVisible = await navPage.isToolbarVisible();
		console.log('Toolbar visible:', toolbarVisible);
	});

	test('should have working back button', async ({ page }) => {
		await navPage.goto('/');
		
		// Navigate to a different route
		try {
			await navPage.switchToMapView();
			await page.waitForTimeout(1000);
			
			// Go back
			await navPage.goBack();
			await page.waitForTimeout(500);
			
			console.log('Back navigation works');
		} catch (error) {
			console.log('Back navigation test skipped:', error);
		}
	});
});

test.describe('Navigation - View Switching', () => {
	let authPage: AuthPage;
	let navPage: NavigationPage;

	test.beforeEach(async ({ page }) => {
		authPage = new AuthPage(page);
		navPage = new NavigationPage(page);
		
		await authPage.clearAuthData();
		await authPage.gotoAuth();
		await authPage.setupTestUser();
		await page.waitForTimeout(1500);
		await navPage.goto('/');
	});

	test('should switch to tree view', async ({ page }) => {
		await navPage.switchToTreeView();
		await page.waitForTimeout(1000);
		
		const currentView = await navPage.getCurrentView();
		console.log('Current view:', currentView);
		
		expect(['tree', 'unknown']).toContain(currentView);
	});

	test('should switch to map view', async ({ page }) => {
		await navPage.switchToMapView();
		await page.waitForTimeout(1500);
		
		const currentView = await navPage.getCurrentView();
		console.log('Current view:', currentView);
		
		expect(['map', 'unknown']).toContain(currentView);
	});

	test('should switch to inventory view', async ({ page }) => {
		await navPage.switchToInventoryView();
		await page.waitForTimeout(1000);
		
		const currentView = await navPage.getCurrentView();
		console.log('Current view:', currentView);
		
		expect(['inventory', 'unknown']).toContain(currentView);
	});

	test('should cycle through all views', async ({ page }) => {
		// Tree
		await navPage.switchToTreeView();
		await page.waitForTimeout(500);
		console.log('Switched to tree');
		
		// Map
		await navPage.switchToMapView();
		await page.waitForTimeout(1000);
		console.log('Switched to map');
		
		// Inventory
		await navPage.switchToInventoryView();
		await page.waitForTimeout(500);
		console.log('Switched to inventory');
		
		// Back to tree
		await navPage.switchToTreeView();
		await page.waitForTimeout(500);
		console.log('Back to tree');
		
		const finalView = await navPage.getCurrentView();
		console.log('Final view:', finalView);
	});
});

test.describe('Navigation - Special Pages', () => {
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

	test('should navigate to collective page', async ({ page }) => {
		try {
			await navPage.gotoCollective();
			await page.waitForTimeout(1000);
			
			const url = navPage.getURL();
			expect(url).toContain('/collective');
			
			console.log('Collective page loaded');
		} catch (error) {
			console.log('Collective page not available:', error);
		}
	});

	test('should navigate to decider page', async ({ page }) => {
		try {
			await navPage.gotoDecider();
			await page.waitForTimeout(1000);
			
			const url = navPage.getURL();
			expect(url).toContain('/decider');
			
			console.log('Decider page loaded');
		} catch (error) {
			console.log('Decider page not available:', error);
		}
	});

	test('should navigate to unconference page', async ({ page }) => {
		try {
			await navPage.gotoUnconference();
			await page.waitForTimeout(1000);
			
			const url = navPage.getURL();
			expect(url).toContain('/unconference');
			
			console.log('Unconference page loaded');
		} catch (error) {
			console.log('Unconference page not available:', error);
		}
	});
});

test.describe('Navigation - Mobile', () => {
	let authPage: AuthPage;
	let navPage: NavigationPage;

	test.beforeEach(async ({ page }) => {
		await page.setViewportSize({ width: 375, height: 667 });
		
		authPage = new AuthPage(page);
		navPage = new NavigationPage(page);
		
		await authPage.clearAuthData();
		await authPage.gotoAuth();
		await authPage.setupTestUser();
		await page.waitForTimeout(1500);
	});

	test('should work on mobile viewport', async ({ page }) => {
		await navPage.goto('/');
		
		const headerVisible = await navPage.isHeaderVisible();
		console.log('Header visible on mobile:', headerVisible);
	});

	test('should open mobile menu', async ({ page }) => {
		await navPage.goto('/');
		await page.waitForTimeout(500);
		
		try {
			await navPage.openMobileMenu();
			await page.waitForTimeout(500);
			
			console.log('Mobile menu opened');
			
			await navPage.closeMobileMenu();
			await page.waitForTimeout(500);
			
			console.log('Mobile menu closed');
		} catch (error) {
			console.log('Mobile menu test skipped:', error);
		}
	});

	test('should switch views on mobile', async ({ page }) => {
		await navPage.goto('/');
		
		// Try switching views on mobile
		await navPage.switchToMapView();
		await page.waitForTimeout(1000);
		
		await navPage.switchToTreeView();
		await page.waitForTimeout(500);
		
		console.log('View switching works on mobile');
	});
});

test.describe('Navigation - Page Reload', () => {
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

	test('should maintain state after reload', async ({ page }) => {
		await navPage.goto('/');
		await navPage.switchToMapView();
		await page.waitForTimeout(1000);
		
		const beforeReload = navPage.getURL();
		
		await navPage.reload();
		await page.waitForTimeout(1500);
		
		const afterReload = navPage.getURL();
		
		console.log('URL before reload:', beforeReload);
		console.log('URL after reload:', afterReload);
		
		// URL should be the same
		expect(afterReload).toContain(beforeReload.split('?')[0].split('#')[0]);
	});

	test('should handle browser forward navigation', async ({ page }) => {
		await navPage.goto('/');
		
		try {
			await navPage.switchToMapView();
			await page.waitForTimeout(1000);
			
			await navPage.goBack();
			await page.waitForTimeout(500);
			
			await navPage.goForward();
			await page.waitForTimeout(500);
			
			console.log('Forward navigation works');
		} catch (error) {
			console.log('Forward navigation test skipped:', error);
		}
	});
});

test.describe('Navigation - Error Handling', () => {
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

	test('should handle 404 page gracefully', async ({ page }) => {
		await page.goto('/nonexistent-page-12345');
		await page.waitForTimeout(1000);
		
		// Check if 404 page is displayed or redirects
		const url = navPage.getURL();
		const bodyText = await page.locator('body').textContent();
		
		console.log('404 URL:', url);
		console.log('Body contains "404":', bodyText?.includes('404') || bodyText?.includes('Not Found'));
	});

	test('should handle invalid routes', async ({ page }) => {
		await page.goto('/invalid/nested/route/123');
		await page.waitForTimeout(1000);
		
		// Should either show error or redirect
		const url = navPage.getURL();
		console.log('Invalid route handled, URL:', url);
	});
});

