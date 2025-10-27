import { type Page } from '@playwright/test';
import { BasePage } from './BasePage';

/**
 * Page Object for Authentication flows
 * Handles login, signup, and authentication state
 */
export class AuthPage extends BasePage {
	// Selectors
	private readonly selectors = {
		// Header authentication buttons
		loginButton: '[data-testid="login-button"], button:has-text("Login"), button:has-text("Sign In")',
		signupButton: '[data-testid="signup-button"], button:has-text("Sign Up"), button:has-text("Register")',
		logoutButton: '[data-testid="logout-button"], button:has-text("Logout"), button:has-text("Sign Out")',
		
		// Authentication modal/form
		authModal: '[data-testid="auth-modal"], [role="dialog"]',
		aliasInput: '[data-testid="auth-alias"], input[name="alias"], input[placeholder*="alias"]',
		passwordInput: '[data-testid="auth-password"], input[name="password"], input[type="password"]',
		confirmPasswordInput: '[data-testid="auth-confirm-password"], input[name="confirmPassword"]',
		submitButton: '[data-testid="auth-submit"], button[type="submit"]',
		
		// User state indicators
		userProfile: '[data-testid="user-profile"]',
		userAlias: '[data-testid="user-alias"]',
		authenticatedIndicator: '[data-testid="authenticated"]',
		
		// Error messages
		errorMessage: '[data-testid="auth-error"], .error-message, [role="alert"]',
	};

	constructor(page: Page) {
		super(page);
	}

	/**
	 * Navigate to the authentication page
	 */
	async gotoAuth(): Promise<void> {
		await this.goto('/');
	}

	/**
	 * Check if user is authenticated
	 */
	async isAuthenticated(): Promise<boolean> {
		try {
			// Check for multiple indicators of authentication
			const hasLogoutButton = await this.isVisible(this.selectors.logoutButton);
			const hasUserProfile = await this.isVisible(this.selectors.userProfile);
			return hasLogoutButton || hasUserProfile;
		} catch {
			return false;
		}
	}

	/**
	 * Open the login modal
	 */
	async openLoginModal(): Promise<void> {
		if (await this.isAuthenticated()) {
			await this.logout();
		}
		
		try {
			await this.page.locator(this.selectors.loginButton).first().click();
			await this.wait(500); // Wait for modal animation
		} catch {
			// Login modal might already be open or embedded on page
		}
	}

	/**
	 * Open the signup modal
	 */
	async openSignupModal(): Promise<void> {
		if (await this.isAuthenticated()) {
			await this.logout();
		}
		
		try {
			await this.page.locator(this.selectors.signupButton).first().click();
			await this.wait(500); // Wait for modal animation
		} catch {
			// Signup form might already be visible
		}
	}

	/**
	 * Login with existing credentials
	 */
	async login(alias: string, password: string): Promise<void> {
		await this.openLoginModal();
		
		// Fill in credentials
		await this.fillField(this.selectors.aliasInput, alias);
		await this.fillField(this.selectors.passwordInput, password);
		
		// Submit
		await this.page.locator(this.selectors.submitButton).click();
		
		// Wait for authentication to complete
		await this.wait(1000);
	}

	/**
	 * Sign up with new credentials
	 */
	async signup(alias: string, password: string): Promise<void> {
		await this.openSignupModal();
		
		// Fill in credentials
		await this.fillField(this.selectors.aliasInput, alias);
		await this.fillField(this.selectors.passwordInput, password);
		
		// If confirm password field exists, fill it
		try {
			await this.fillField(this.selectors.confirmPasswordInput, password);
		} catch {
			// Confirm password might not be required
		}
		
		// Submit
		await this.page.locator(this.selectors.submitButton).click();
		
		// Wait for account creation and authentication
		await this.wait(2000);
	}

	/**
	 * Logout the current user
	 */
	async logout(): Promise<void> {
		if (await this.isAuthenticated()) {
			try {
				await this.page.locator(this.selectors.logoutButton).first().click();
				await this.wait(1000);
			} catch {
				// Already logged out or logout button not found
			}
		}
	}

	/**
	 * Get the current user's alias
	 */
	async getUserAlias(): Promise<string | null> {
		try {
			return await this.getTextContent(this.selectors.userAlias);
		} catch {
			return null;
		}
	}

	/**
	 * Check if authentication error is displayed
	 */
	async hasAuthError(): Promise<boolean> {
		try {
			return await this.isVisible(this.selectors.errorMessage);
		} catch {
			return false;
		}
	}

	/**
	 * Get authentication error message
	 */
	async getAuthError(): Promise<string | null> {
		try {
			return await this.getTextContent(this.selectors.errorMessage);
		} catch {
			return null;
		}
	}

	/**
	 * Setup a test user (creates and authenticates)
	 */
	async setupTestUser(alias?: string, password?: string): Promise<{ alias: string; password: string }> {
		const testAlias = alias || `test_user_${Date.now()}`;
		const testPassword = password || 'TestPassword123!';
		
		await this.signup(testAlias, testPassword);
		
		return { alias: testAlias, password: testPassword };
	}

	/**
	 * Clear local storage (simulates fresh start)
	 */
	async clearAuthData(): Promise<void> {
		await this.page.evaluate(() => {
			localStorage.clear();
			sessionStorage.clear();
		});
	}
}

