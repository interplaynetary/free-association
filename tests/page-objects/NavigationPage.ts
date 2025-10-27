import { type Page } from '@playwright/test';
import { BasePage } from './BasePage';

/**
 * Page Object for Navigation and Common UI Elements
 * Handles header, toolbar, and view switching
 */
export class NavigationPage extends BasePage {
	// Selectors
	private readonly selectors = {
		// Header
		header: '[data-testid="header"], header',
		logo: '[data-testid="logo"]',
		
		// Main navigation
		navMenu: '[data-testid="nav-menu"], nav',
		navLink: '[data-testid="nav-link"]',
		
		// Toolbar (bottom)
		toolbar: '[data-testid="toolbar"], .toolbar',
		
		// View buttons
		treeViewButton: '[data-testid="tree-view"], button:has-text("Tree")',
		mapViewButton: '[data-testid="map-view"], button:has-text("Map")',
		inventoryViewButton: '[data-testid="inventory-view"], button:has-text("Inventory")',
		sharesViewButton: '[data-testid="shares-view"], button:has-text("Shares")',
		
		// Special pages
		collectiveLink: 'a[href="/collective"]',
		deciderLink: 'a[href="/decider"]',
		unconferenceLink: 'a[href="/unconference"]',
		
		// Mobile menu
		mobileMenuButton: '[data-testid="mobile-menu"], button[aria-label="menu"]',
		mobileMenuDrawer: '[data-testid="mobile-drawer"]',
		
		// Breadcrumbs
		breadcrumbs: '[data-testid="breadcrumbs"]',
		breadcrumbItem: '[data-testid="breadcrumb-item"]',
	};

	constructor(page: Page) {
		super(page);
	}

	/**
	 * Check if header is visible
	 */
	async isHeaderVisible(): Promise<boolean> {
		return await this.isVisible(this.selectors.header);
	}

	/**
	 * Check if toolbar is visible
	 */
	async isToolbarVisible(): Promise<boolean> {
		return await this.isVisible(this.selectors.toolbar);
	}

	/**
	 * Switch to tree view
	 */
	async switchToTreeView(): Promise<void> {
		await this.page.locator(this.selectors.treeViewButton).click();
		await this.wait(500);
	}

	/**
	 * Switch to map view
	 */
	async switchToMapView(): Promise<void> {
		await this.page.locator(this.selectors.mapViewButton).click();
		await this.wait(1000); // Map takes longer to load
	}

	/**
	 * Switch to inventory view
	 */
	async switchToInventoryView(): Promise<void> {
		await this.page.locator(this.selectors.inventoryViewButton).click();
		await this.wait(500);
	}

	/**
	 * Switch to shares view
	 */
	async switchToSharesView(): Promise<void> {
		try {
			await this.page.locator(this.selectors.sharesViewButton).click();
			await this.wait(500);
		} catch {
			// Shares view might be combined with inventory
			await this.switchToInventoryView();
		}
	}

	/**
	 * Navigate to collective page
	 */
	async gotoCollective(): Promise<void> {
		await this.page.locator(this.selectors.collectiveLink).click();
		await this.waitForPageReady();
	}

	/**
	 * Navigate to decider page
	 */
	async gotoDecider(): Promise<void> {
		await this.page.locator(this.selectors.deciderLink).click();
		await this.waitForPageReady();
	}

	/**
	 * Navigate to unconference page
	 */
	async gotoUnconference(): Promise<void> {
		await this.page.locator(this.selectors.unconferenceLink).click();
		await this.waitForPageReady();
	}

	/**
	 * Open mobile menu (for mobile viewport)
	 */
	async openMobileMenu(): Promise<void> {
		try {
			await this.page.locator(this.selectors.mobileMenuButton).click();
			await this.wait(300);
		} catch {
			// Not in mobile view or menu already open
		}
	}

	/**
	 * Close mobile menu
	 */
	async closeMobileMenu(): Promise<void> {
		try {
			// Click outside the drawer or on close button
			await this.page.keyboard.press('Escape');
			await this.wait(300);
		} catch {
			// Menu not open
		}
	}

	/**
	 * Get current active view
	 */
	async getCurrentView(): Promise<string> {
		const url = this.getURL();
		
		if (url.includes('/collective')) return 'collective';
		if (url.includes('/decider')) return 'decider';
		if (url.includes('/unconference')) return 'unconference';
		
		// Check which view button is active (has active class or aria-current)
		const buttons = [
			{ name: 'tree', selector: this.selectors.treeViewButton },
			{ name: 'map', selector: this.selectors.mapViewButton },
			{ name: 'inventory', selector: this.selectors.inventoryViewButton },
			{ name: 'shares', selector: this.selectors.sharesViewButton },
		];
		
		for (const button of buttons) {
			try {
				const element = this.page.locator(button.selector).first();
				const classes = await element.getAttribute('class') || '';
				const ariaCurrent = await element.getAttribute('aria-current');
				
				if (classes.includes('active') || ariaCurrent === 'page') {
					return button.name;
				}
			} catch {
				continue;
			}
		}
		
		return 'unknown';
	}

	/**
	 * Navigate back
	 */
	async goBack(): Promise<void> {
		await this.page.goBack();
		await this.waitForPageReady();
	}

	/**
	 * Navigate forward
	 */
	async goForward(): Promise<void> {
		await this.page.goForward();
		await this.waitForPageReady();
	}

	/**
	 * Reload the page
	 */
	async reload(): Promise<void> {
		await this.page.reload();
		await this.waitForPageReady();
	}
}

