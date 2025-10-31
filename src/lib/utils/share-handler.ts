/**
 * Generic Share Handler System
 * 
 * Provides a flexible, plugin-based system for handling different types of shared content.
 * Allows registering custom handlers for different share scenarios.
 */

export interface ShareData {
	title?: string;
	text?: string;
	url?: string;
	files?: File[];
	timestamp: number;
}

export interface ShareHandlerContext {
	data: ShareData;
	navigate: (path: string) => void;
	toast: (message: string) => void;
}

export type ShareHandler = (context: ShareHandlerContext) => Promise<boolean> | boolean;

export interface ShareHandlerRegistration {
	name: string;
	description: string;
	priority: number; // Higher priority runs first
	canHandle: (data: ShareData) => boolean;
	handle: ShareHandler;
}

// Global handler registry
const handlers: ShareHandlerRegistration[] = [];

/**
 * Register a share handler
 */
export function registerShareHandler(handler: ShareHandlerRegistration): void {
	handlers.push(handler);
	// Sort by priority (descending)
	handlers.sort((a, b) => b.priority - a.priority);
	console.log(`[Share] Registered handler: ${handler.name} (priority: ${handler.priority})`);
}

/**
 * Unregister a share handler
 */
export function unregisterShareHandler(name: string): void {
	const index = handlers.findIndex(h => h.name === name);
	if (index !== -1) {
		handlers.splice(index, 1);
		console.log(`[Share] Unregistered handler: ${name}`);
	}
}

/**
 * Process shared data through registered handlers
 */
export async function processSharedData(
	data: ShareData,
	context: Omit<ShareHandlerContext, 'data'>
): Promise<{ handled: boolean; handlerName?: string }> {
	console.log('[Share] Processing shared data:', data);
	
	// Find handlers that can handle this data
	const applicableHandlers = handlers.filter(h => h.canHandle(data));
	
	if (applicableHandlers.length === 0) {
		console.log('[Share] No handlers found for this data');
		return { handled: false };
	}
	
	console.log(`[Share] Found ${applicableHandlers.length} applicable handler(s)`);
	
	// Try each handler in priority order
	for (const handler of applicableHandlers) {
		console.log(`[Share] Trying handler: ${handler.name}`);
		
		try {
			const result = await handler.handle({ data, ...context });
			
			if (result) {
				console.log(`[Share] ✓ Handled by: ${handler.name}`);
				return { handled: true, handlerName: handler.name };
			}
		} catch (error) {
			console.error(`[Share] Handler ${handler.name} failed:`, error);
			// Continue to next handler
		}
	}
	
	console.log('[Share] No handler successfully processed the data');
	return { handled: false };
}

/**
 * Get all registered handlers
 */
export function getShareHandlers(): ReadonlyArray<ShareHandlerRegistration> {
	return handlers;
}

// ============================================================================
// BUILT-IN HANDLERS
// ============================================================================

/**
 * Handler for URLs that could be recognition links
 */
export const urlToRecognitionHandler: ShareHandlerRegistration = {
	name: 'url-to-recognition',
	description: 'Converts shared URLs into recognition tree nodes',
	priority: 100,
	canHandle: (data) => !!data.url,
	handle: async ({ data, navigate, toast }) => {
		if (!data.url) return false;
		
		// Store for processing in the recognition tree
		sessionStorage.setItem('pending-url-share', JSON.stringify({
			url: data.url,
			title: data.title || 'Shared Link',
			text: data.text || '',
			timestamp: data.timestamp
		}));
		
		toast('Adding shared link to your tree...');
		navigate('/');
		return true;
	}
};

/**
 * Handler for text that could be notes or descriptions
 */
export const textToNoteHandler: ShareHandlerRegistration = {
	name: 'text-to-note',
	description: 'Converts shared text into notes',
	priority: 90,
	canHandle: (data) => !!data.text && !data.url,
	handle: async ({ data, navigate, toast }) => {
		if (!data.text) return false;
		
		sessionStorage.setItem('pending-text-share', JSON.stringify({
			text: data.text,
			title: data.title || 'Shared Text',
			timestamp: data.timestamp
		}));
		
		toast('Adding shared text as a note...');
		navigate('/');
		return true;
	}
};

/**
 * Handler for location/map URLs
 */
export const locationHandler: ShareHandlerRegistration = {
	name: 'location-handler',
	description: 'Handles shared location/map links',
	priority: 110, // Higher priority than generic URL handler
	canHandle: (data) => {
		if (!data.url) return false;
		const url = data.url.toLowerCase();
		return url.includes('maps.google') || 
		       url.includes('openstreetmap') ||
		       url.includes('geo:') ||
		       (data.title?.toLowerCase().includes('location') ?? false);
	},
	handle: async ({ data, navigate, toast }) => {
		sessionStorage.setItem('pending-location-share', JSON.stringify({
			url: data.url,
			title: data.title || 'Shared Location',
			text: data.text || '',
			timestamp: data.timestamp
		}));
		
		toast('Adding location to your map...');
		navigate('/map');
		return true;
	}
};

/**
 * Handler for YouTube/video links
 */
export const videoHandler: ShareHandlerRegistration = {
	name: 'video-handler',
	description: 'Handles shared video links',
	priority: 110,
	canHandle: (data) => {
		if (!data.url) return false;
		const url = data.url.toLowerCase();
		return url.includes('youtube.com') || 
		       url.includes('youtu.be') ||
		       url.includes('vimeo.com') ||
		       url.includes('video');
	},
	handle: async ({ data, navigate, toast }) => {
		sessionStorage.setItem('pending-video-share', JSON.stringify({
			url: data.url,
			title: data.title || 'Shared Video',
			text: data.text || '',
			timestamp: data.timestamp
		}));
		
		toast('Adding video to your collection...');
		navigate('/');
		return true;
	}
};

/**
 * Handler for social media posts
 */
export const socialMediaHandler: ShareHandlerRegistration = {
	name: 'social-media-handler',
	description: 'Handles shared social media posts',
	priority: 105,
	canHandle: (data) => {
		if (!data.url) return false;
		const url = data.url.toLowerCase();
		return url.includes('twitter.com') ||
		       url.includes('x.com') ||
		       url.includes('facebook.com') ||
		       url.includes('linkedin.com') ||
		       url.includes('instagram.com');
	},
	handle: async ({ data, navigate, toast }) => {
		sessionStorage.setItem('pending-social-share', JSON.stringify({
			url: data.url,
			title: data.title || 'Shared Post',
			text: data.text || '',
			timestamp: data.timestamp
		}));
		
		toast('Adding social post to your feed...');
		navigate('/');
		return true;
	}
};

/**
 * Default fallback handler
 */
export const defaultHandler: ShareHandlerRegistration = {
	name: 'default-handler',
	description: 'Default fallback for unhandled shares',
	priority: 0, // Lowest priority - only if nothing else handles it
	canHandle: () => true, // Always can handle
	handle: async ({ data, navigate, toast }) => {
		// Store generic shared content
		sessionStorage.setItem('generic-share', JSON.stringify(data));
		
		const type = data.url ? 'link' : data.text ? 'text' : 'content';
		toast(`Received shared ${type}`);
		navigate('/');
		return true;
	}
};

// ============================================================================
// UTILITY FUNCTIONS
// ============================================================================

/**
 * Get pending shares of a specific type
 */
export function getPendingShare(key: string): any | null {
	const stored = sessionStorage.getItem(key);
	if (stored) {
		sessionStorage.removeItem(key); // Clear after reading
		return JSON.parse(stored);
	}
	return null;
}

/**
 * Clear all pending shares
 */
export function clearAllPendingShares(): void {
	const keys = [
		'pending-url-share',
		'pending-text-share',
		'pending-location-share',
		'pending-video-share',
		'pending-social-share',
		'generic-share',
		'shared-content' // Legacy
	];
	
	keys.forEach(key => sessionStorage.removeItem(key));
}

/**
 * Check if there are any pending shares
 */
export function hasPendingShares(): boolean {
	return !!(
		sessionStorage.getItem('pending-url-share') ||
		sessionStorage.getItem('pending-text-share') ||
		sessionStorage.getItem('pending-location-share') ||
		sessionStorage.getItem('pending-video-share') ||
		sessionStorage.getItem('pending-social-share') ||
		sessionStorage.getItem('generic-share') ||
		sessionStorage.getItem('shared-content')
	);
}

/**
 * Initialize default handlers
 */
export function initializeDefaultHandlers(): void {
	registerShareHandler(locationHandler);
	registerShareHandler(videoHandler);
	registerShareHandler(socialMediaHandler);
	registerShareHandler(urlToRecognitionHandler);
	registerShareHandler(textToNoteHandler);
	registerShareHandler(defaultHandler);
}

