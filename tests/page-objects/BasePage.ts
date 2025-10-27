import { type Page, type Locator } from '@playwright/test';

/**
 * Base Page Object class with common functionality
 * All other page objects should extend this class
 */
export class BasePage {
	readonly page: Page;

	constructor(page: Page) {
		this.page = page;
	}

	/**
	 * Navigate to a specific path
	 */
	async goto(path: string = '/'): Promise<void> {
		await this.page.goto(path);
		await this.waitForPageReady();
	}

	/**
	 * Wait for the page to be fully loaded and hydrated
	 */
	async waitForPageReady(): Promise<void> {
		await this.page.waitForLoadState('networkidle');
		await this.page.waitForLoadState('domcontentloaded');
		// Wait for Svelte hydration
		await this.page.waitForTimeout(200);
	}

	/**
	 * Get the current URL
	 */
	getURL(): string {
		return this.page.url();
	}

	/**
	 * Wait for an element to be visible
	 */
	async waitForElement(selector: string, timeout: number = 5000): Promise<Locator> {
		const locator = this.page.locator(selector);
		await locator.waitFor({ state: 'visible', timeout });
		return locator;
	}

	/**
	 * Click an element and wait for navigation
	 */
	async clickAndNavigate(selector: string): Promise<void> {
		await this.page.locator(selector).click();
		await this.waitForPageReady();
	}

	/**
	 * Fill a form field
	 */
	async fillField(selector: string, value: string): Promise<void> {
		await this.page.locator(selector).fill(value);
	}

	/**
	 * Click a button by text
	 */
	async clickButton(text: string): Promise<void> {
		await this.page.getByRole('button', { name: text }).click();
	}

	/**
	 * Check if element is visible
	 */
	async isVisible(selector: string): Promise<boolean> {
		return await this.page.locator(selector).isVisible();
	}

	/**
	 * Get text content of an element
	 */
	async getTextContent(selector: string): Promise<string> {
		return (await this.page.locator(selector).textContent()) || '';
	}

	/**
	 * Take a screenshot
	 */
	async screenshot(name: string): Promise<void> {
		await this.page.screenshot({ 
			path: `test-results/screenshots/${name}.png`, 
			fullPage: true 
		});
	}

	/**
	 * Wait for a specific amount of time
	 */
	async wait(ms: number): Promise<void> {
		await this.page.waitForTimeout(ms);
	}
}

