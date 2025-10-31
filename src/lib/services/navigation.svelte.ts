import { browser } from '$app/environment';
import { pushState } from '$app/navigation';
import { globalState } from '$lib/global.svelte';

/**
 * Navigation Service - Handles global navigation and keyboard shortcuts
 *
 * This service manages:
 * - Global keyboard shortcuts (Escape for navigation/edit exit)
 * - Browser history manipulation for back button handling
 * - Navigation state coordination
 */
class NavigationService {
	private isInitialized = false;
	private initialHistoryPushed = false;
	private routerReady = false;

	constructor() {
		if (browser && !this.isInitialized) {
			// Wait for SvelteKit router to be ready before initializing
			// Use a longer delay to ensure router is mounted
			setTimeout(() => {
				this.initialize();
			}, 1000);
		}
	}

	private initialize() {
		if (this.isInitialized) return;
		
		console.log('[NAVIGATION-SERVICE] Initializing navigation service');
		this.isInitialized = true;

		// Set up event listeners
		this.setupKeyboardListeners();
		this.setupHistoryListeners();

		// Delay initial history push further to ensure router is ready
		setTimeout(() => {
			this.routerReady = true;
			this.setupInitialHistory();
		}, 1500);
	}

	private handleGlobalKeydown = (event: KeyboardEvent) => {
		// Handle escape key for zoom out navigation or exit edit mode
		if (event.key === 'Escape') {
			this.handleBackNavigation();
		}
	};

	private handleBackNavigation() {
		// If we're in edit mode, let the edit mode handle the escape
		if (globalState.editMode) {
			// The Child component will handle exiting edit mode
			// Don't prevent default here to allow the input blur to work
			return;
		}

		// Check if we're currently editing (input fields, etc.) - fallback check
		const activeElement = document.activeElement;
		const isEditing = this.isCurrentlyEditing(activeElement);

		// Only trigger navigation if we're not currently editing
		if (!isEditing) {
			globalState.zoomOut();
		}
	}

	private handlePopState = (event: PopStateEvent) => {
		// Don't interfere with text editing focus events
		const activeElement = document.activeElement;
		const isEditingText = this.isCurrentlyEditing(activeElement);

		// If currently editing text, don't handle popstate to avoid iOS focus issues
		if (isEditingText) {
			console.log('[NAVIGATION-SERVICE] Skipping popstate handling - text editing active');
			return;
		}

		// Check if we're in edit mode or can zoom out
		const canHandleBack = globalState.editMode || globalState.canZoomOut();

		if (canHandleBack) {
			// Handle back button same as escape key
			event.preventDefault();
			this.handleBackNavigation();

			// Push a new state to maintain the current position
			// This prevents the browser from actually going back
			this.safePushState(window.location.href, {});
		}
		// If we can't handle the back action, let the browser handle it normally
	};

	private isCurrentlyEditing(activeElement: Element | null): boolean {
		return (
			activeElement &&
			(activeElement.tagName === 'INPUT' ||
				activeElement.tagName === 'TEXTAREA' ||
				(activeElement as HTMLElement).isContentEditable ||
				activeElement.closest('.node-edit-input') !== null)
		) || false
	}

	private setupKeyboardListeners() {
		document.addEventListener('keydown', this.handleGlobalKeydown);
		console.log('[NAVIGATION-SERVICE] Global keyboard listeners set up');
	}

	private setupHistoryListeners() {
		window.addEventListener('popstate', this.handlePopState);
		console.log('[NAVIGATION-SERVICE] History listeners set up');
	}

	/**
	 * Safely call pushState only when router is ready
	 */
	private safePushState(url: string, state: any) {
		if (!this.routerReady) {
			console.log('[NAVIGATION-SERVICE] Router not ready, skipping pushState');
			return;
		}
		
		try {
			pushState(url, state);
		} catch (error) {
			console.warn('[NAVIGATION-SERVICE] Failed to pushState:', error);
		}
	}

	private setupInitialHistory() {
		if (!this.initialHistoryPushed) {
			// Push initial state to ensure back button can be intercepted
			this.safePushState(window.location.href, {});
			this.initialHistoryPushed = true;
			console.log('[NAVIGATION-SERVICE] Initial history state pushed');
		}
	}

	// Public API methods
	public canNavigateBack(): boolean {
		return globalState.canZoomOut();
	}

	public navigateBack(): void {
		this.handleBackNavigation();
	}

	// Cleanup method for testing or manual cleanup
	destroy() {
		if (!browser) return;

		document.removeEventListener('keydown', this.handleGlobalKeydown);
		window.removeEventListener('popstate', this.handlePopState);

		this.isInitialized = false;
		this.initialHistoryPushed = false;
		console.log('[NAVIGATION-SERVICE] Navigation service destroyed');
	}
}

// Create singleton instance
export const navigationService = new NavigationService();

// Export for testing
export { NavigationService };
