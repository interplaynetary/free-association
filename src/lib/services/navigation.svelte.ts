import { browser } from '$app/environment';
import { pushState } from '$app/navigation';
import { globalState } from '$lib/global.svelte';

console.log('[TRACE] src/lib/services/navigation.svelte.ts: <module scope>');

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
		console.log('[TRACE] src/lib/services/navigation.svelte.ts: constructor');
	}

	public initialize() {
		console.log('[TRACE] src/lib/services/navigation.svelte.ts: initialize');
		if (this.isInitialized) return;

		console.log('[NAVIGATION-SERVICE] Initializing navigation service');
		this.isInitialized = true;
		// Set up event listeners
		this.setupKeyboardListeners();
		this.setupHistoryListeners();

		// Defer initial history setup to ensure SvelteKit router is fully hydrated
		// onMount is sometimes slightly too early for pushState
		setTimeout(() => {
			this.routerReady = true;
			this.setupInitialHistory();
		}, 100);

		console.log('[TRACE] [EXIT] src/lib/services/navigation.svelte.ts: initialize');
	}

	private handleGlobalKeydown = (event: KeyboardEvent) => {
		// console.log('[TRACE] src/lib/services/navigation.svelte.ts: handleGlobalKeydown', { key: event.key }); // potentially noisy, commented out for now or enable if needed
		// Handle escape key for zoom out navigation or exit edit mode
		if (event.key === 'Escape') {
			console.log('[TRACE] src/lib/services/navigation.svelte.ts: handleGlobalKeydown (Escape)');
			this.handleBackNavigation();
		}
	};

	private handleBackNavigation() {
		console.log('[TRACE] src/lib/services/navigation.svelte.ts: handleBackNavigation');
		// If we're in edit mode, let the edit mode handle the escape
		if (globalState.editMode) {
			// The Child component will handle exiting edit mode
			// Don't prevent default here to allow the input blur to work
			console.log('[TRACE] [EXIT] src/lib/services/navigation.svelte.ts: handleBackNavigation (edit mode handling)');
			return;
		}

		// Check if we're currently editing (input fields, etc.) - fallback check
		const activeElement = document.activeElement;
		const isEditing = this.isCurrentlyEditing(activeElement);

		// Only trigger navigation if we're not currently editing
		if (!isEditing) {
			globalState.zoomOut();
		}
		console.log('[TRACE] [EXIT] src/lib/services/navigation.svelte.ts: handleBackNavigation');
	}

	private handlePopState = (event: PopStateEvent) => {
		console.log('[TRACE] src/lib/services/navigation.svelte.ts: handlePopState');
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
			this.safePushState('', {});
		}
		// If we can't handle the back action, let the browser handle it normally
		console.log('[TRACE] [EXIT] src/lib/services/navigation.svelte.ts: handlePopState');
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
		console.log('[TRACE] src/lib/services/navigation.svelte.ts: setupKeyboardListeners');
		document.addEventListener('keydown', this.handleGlobalKeydown);
		console.log('[NAVIGATION-SERVICE] Global keyboard listeners set up');
		console.log('[TRACE] [EXIT] src/lib/services/navigation.svelte.ts: setupKeyboardListeners');
	}

	private setupHistoryListeners() {
		console.log('[TRACE] src/lib/services/navigation.svelte.ts: setupHistoryListeners');
		window.addEventListener('popstate', this.handlePopState);
		console.log('[NAVIGATION-SERVICE] History listeners set up');
		console.log('[TRACE] [EXIT] src/lib/services/navigation.svelte.ts: setupHistoryListeners');
	}

	/**
	 * Safely call pushState only when router is ready
	 */
	private safePushState(url: string, state: any) {
		console.log('[TRACE] src/lib/services/navigation.svelte.ts: safePushState', { url });
		if (!this.routerReady) {
			console.log('[NAVIGATION-SERVICE] Router not ready, skipping pushState');
			return;
		}

		try {
			pushState(url, state);
		} catch (error) {
			console.warn('[NAVIGATION-SERVICE] Failed to pushState:', error);
		}
		console.log('[TRACE] [EXIT] src/lib/services/navigation.svelte.ts: safePushState');
	}

	private setupInitialHistory() {
		console.log('[TRACE] src/lib/services/navigation.svelte.ts: setupInitialHistory');
		if (!this.initialHistoryPushed) {
			// Push initial state to ensure back button can be intercepted
			this.safePushState('', {});
			this.initialHistoryPushed = true;
			console.log('[NAVIGATION-SERVICE] Initial history state pushed');
		}
		console.log('[TRACE] [EXIT] src/lib/services/navigation.svelte.ts: setupInitialHistory');
	}

	// Public API methods
	public canNavigateBack(): boolean {
		return globalState.canZoomOut();
	}

	public navigateBack(): void {
		console.log('[TRACE] src/lib/services/navigation.svelte.ts: navigateBack');
		this.handleBackNavigation();
		console.log('[TRACE] [EXIT] src/lib/services/navigation.svelte.ts: navigateBack');
	}

	// Cleanup method for testing or manual cleanup
	destroy() {
		console.log('[TRACE] src/lib/services/navigation.svelte.ts: destroy');
		if (!browser) return;

		document.removeEventListener('keydown', this.handleGlobalKeydown);
		window.removeEventListener('popstate', this.handlePopState);

		this.isInitialized = false;
		this.initialHistoryPushed = false;
		console.log('[NAVIGATION-SERVICE] Navigation service destroyed');
		console.log('[TRACE] [EXIT] src/lib/services/navigation.svelte.ts: destroy');
	}
}

// Create singleton instance
export const navigationService = new NavigationService();

// Export for testing
export { NavigationService };
