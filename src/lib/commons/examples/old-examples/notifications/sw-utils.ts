export interface ServiceWorkerSubscriptionConfig {
	id: string;
	path: string;
	scope: 'gun' | 'user';
	filter?: {
		// Optional filter to determine if data should trigger notification
		requiredFields?: string[]; // Fields that must exist in data
		excludeFields?: string[]; // Fields that if present, exclude the data
		customFilter?: string; // Name of custom filter function
	};
	processor?: string; // Name of data processor function
	notification: {
		title: string; // Template string with {{field}} placeholders
		body: string; // Template string with {{field}} placeholders
		icon?: string;
		badge?: string;
		tag?: string; // Template string, defaults to {{id}}-{{key}}
		requireInteraction?: boolean;
		vibrate?: number[];
		actions?: Array<{
			action: string;
			title: string;
			icon?: string;
			condition?: string; // Optional condition for when to show this action
		}>;
	};
	routing?: {
		defaultRoute?: string;
		actions?: Record<string, string>; // action name -> route template
	};
}

interface ProcessedData {
	[key: string]: any;
	_key: string;
	_preview: string;
	_timestamp: string;
}

/**
 * Load subscription configuration with fallback
 */
export async function loadSubscriptionConfig(): Promise<ServiceWorkerSubscriptionConfig[]> {
	try {
		const response = await fetch('/sw-config.json');
		if (response.ok) {
			const config = await response.json();
			return config.subscriptions || getDefaultConfig();
		}
	} catch (error) {
		console.warn('[SW] Using default config:', error);
	}
	return getDefaultConfig();
}

/**
 * Default minimal configuration
 */
function getDefaultConfig(): ServiceWorkerSubscriptionConfig[] {
	return [
		{
			id: 'global-updates',
			path: '',
			scope: 'gun',
			filter: { excludeFields: ['_', 'heartbeat'] },
			notification: {
				title: 'Network Update',
				body: '{{_preview}}',
				requireInteraction: false
			},
			routing: { defaultRoute: '/' }
		}
	];
}

/**
 * Enhanced data filtering with better logic
 */
export function shouldProcessData(
	data: any,
	key: string,
	subscription: ServiceWorkerSubscriptionConfig
): boolean {
	if (!data || data === null || data === undefined) return false;

	const { filter } = subscription;
	if (!filter) return true;

	// Check exclude fields (key or data properties)
	if (filter.excludeFields?.some((field) => field === key || data[field] !== undefined)) {
		return false;
	}

	// Check required fields
	if (filter.requiredFields?.some((field) => data[field] === undefined)) {
		return false;
	}

	return true;
}

/**
 * Enhanced data processing with consistent structure
 */
export function processDataForNotification(data: any, key: string): ProcessedData {
	return {
		...data,
		_key: key,
		_preview: createDataPreview(data),
		_timestamp: new Date().toISOString()
	};
}

/**
 * Smart data preview generation
 */
function createDataPreview(data: any, maxLength: number = 50): string {
	// Handle primitives
	if (typeof data === 'string') {
		return data.length > maxLength ? `${data.substring(0, maxLength)}...` : data;
	}
	if (typeof data === 'number' || typeof data === 'boolean') {
		return String(data);
	}

	// Handle objects
	if (typeof data === 'object' && data !== null) {
		// Look for common display fields
		const displayFields = ['name', 'title', 'message', 'text', 'what', 'content', 'value'];
		const displayValue = displayFields
			.map((field) => data[field])
			.find((value) => typeof value === 'string' && value.length > 0);

		if (displayValue) {
			return displayValue.length > maxLength
				? `${displayValue.substring(0, maxLength)}...`
				: displayValue;
		}

		// Fallback to object structure
		const keys = Object.keys(data)
			.filter((k) => !k.startsWith('_'))
			.slice(0, 3);
		return keys.length > 0 ? `{${keys.join(', ')}}` : 'Object';
	}

	return 'Data';
}

/**
 * Advanced template interpolation with error handling
 */
export function interpolateTemplate(template: string, data: ProcessedData): string {
	return template.replace(/\{\{([^}]+)\}\}/g, (match, expression) => {
		try {
			const value = expression
				.trim()
				.split('.')
				.reduce((obj: any, key: string) => {
					return obj?.[key];
				}, data);

			return value !== undefined && value !== null ? String(value) : match;
		} catch {
			return match;
		}
	});
}

/**
 * Simplified URL builder
 */
export function buildNotificationUrl(
	subscription: ServiceWorkerSubscriptionConfig,
	action: string,
	data: ProcessedData
): string {
	const routing = subscription.routing;
	const template = routing?.actions?.[action] || routing?.defaultRoute || '/';

	return interpolateTemplate(template, data);
}

/**
 * Validate subscription configuration
 */
export function validateSubscription(sub: any): sub is ServiceWorkerSubscriptionConfig {
	return (
		typeof sub === 'object' &&
		typeof sub.id === 'string' &&
		typeof sub.path === 'string' &&
		['gun', 'user'].includes(sub.scope) &&
		typeof sub.notification === 'object' &&
		typeof sub.notification.title === 'string' &&
		typeof sub.notification.body === 'string'
	);
}
