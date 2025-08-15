import { browser } from '$app/environment';

/**
 * Viewport Service - Handles iOS keyboard detection and viewport changes
 *
 * This service manages:
 * - Visual Viewport API for iOS keyboard detection
 * - CSS custom properties for keyboard height
 * - Cross-browser viewport handling
 */
class ViewportService {
	private keyboardHeight = $state(0);
	private hasVisualViewport = false;
	private isInitialized = false;

	constructor() {
		if (browser && !this.isInitialized) {
			this.initialize();
		}
	}

	private initialize() {
		console.log('[VIEWPORT-SERVICE] Initializing viewport service');
		this.isInitialized = true;
		this.hasVisualViewport = 'visualViewport' in window;

		// Set up viewport change handling
		this.setupViewportListeners();

		// Initial calculation
		this.handleViewportChange();
	}

	private handleViewportChange = () => {
		// Multiple levels of feature detection for better compatibility
		if (!window.visualViewport && !window.innerHeight) return;

		let keyboardHeight = 0;

		// Primary method: Visual Viewport API (best support)
		if (window.visualViewport) {
			keyboardHeight = window.innerHeight - window.visualViewport.height;
		}
		// Fallback method: Screen height comparison
		else if (window.screen && window.screen.height) {
			keyboardHeight = Math.max(0, window.screen.height - window.innerHeight - 100);
		}

		// Update reactive state
		this.keyboardHeight = keyboardHeight;

		// Apply dynamic adjustment for virtual keyboard with vendor prefixes
		if (keyboardHeight > 0) {
			document.documentElement.style.setProperty('--keyboard-height', `${keyboardHeight}px`);
			document.documentElement.style.setProperty('-webkit-keyboard-height', `${keyboardHeight}px`);
			document.documentElement.style.setProperty('-moz-keyboard-height', `${keyboardHeight}px`);

			console.log('[VIEWPORT-SERVICE] Keyboard detected, height:', keyboardHeight);
		} else {
			document.documentElement.style.setProperty('--keyboard-height', '0px');
			document.documentElement.style.setProperty('-webkit-keyboard-height', '0px');
			document.documentElement.style.setProperty('-moz-keyboard-height', '0px');
		}
	};

	private scrollHandler = (event: Event) => {
		// Only handle if viewport height is significantly smaller (keyboard is open)
		const keyboardHeight = window.innerHeight - (window?.visualViewport?.height || 0);
		if (keyboardHeight > 100) {
			// Only if keyboard is likely open
			this.handleViewportChange();
		}
	};

	private setupViewportListeners() {
		if (this.hasVisualViewport && window.visualViewport) {
			console.log('[VIEWPORT-SERVICE] Setting up Visual Viewport API listeners');
			window.visualViewport.addEventListener('resize', this.handleViewportChange);
			window.visualViewport.addEventListener('scroll', this.scrollHandler);
		} else {
			console.log('[VIEWPORT-SERVICE] Using fallback viewport listeners');
			window.addEventListener('resize', this.handleViewportChange);
			// Additional orientation change listener for mobile
			window.addEventListener('orientationchange', () => {
				// Delay to ensure dimensions are updated
				setTimeout(this.handleViewportChange, 100);
			});
		}
	}

	// Public API
	get currentKeyboardHeight() {
		return this.keyboardHeight;
	}

	get isKeyboardVisible() {
		return this.keyboardHeight > 100;
	}

	// Cleanup method for testing or manual cleanup
	destroy() {
		if (!browser) return;

		if (this.hasVisualViewport && window.visualViewport) {
			window.visualViewport.removeEventListener('resize', this.handleViewportChange);
			window.visualViewport.removeEventListener('scroll', this.scrollHandler);
		} else {
			window.removeEventListener('resize', this.handleViewportChange);
			window.removeEventListener('orientationchange', this.handleViewportChange);
		}

		this.isInitialized = false;
		console.log('[VIEWPORT-SERVICE] Viewport service destroyed');
	}
}

// Create singleton instance
export const viewportService = new ViewportService();

// Export for testing
export { ViewportService };
