/**
 * Utility functions for handling map-related interactions like opening map apps
 * and copying addresses to clipboard
 */

export interface MapAppOption {
	name: string;
	icon: string;
	urlTemplate: (address: string, lat?: number, lng?: number) => string;
	isAvailable: () => boolean;
}

/**
 * Detects the user's device/platform
 */
export function detectPlatform(): 'ios' | 'android' | 'desktop' {
	if (typeof window === 'undefined') return 'desktop';

	const userAgent = window.navigator.userAgent.toLowerCase();

	if (/iphone|ipad|ipod/.test(userAgent)) {
		return 'ios';
	} else if (/android/.test(userAgent)) {
		return 'android';
	} else {
		return 'desktop';
	}
}

/**
 * Gets available map applications based on the current platform
 */
export function getAvailableMapApps(): MapAppOption[] {
	const platform = detectPlatform();

	const apps: MapAppOption[] = [
		// Google Maps (universal)
		{
			name: 'Google Maps',
			icon: '🗺️',
			urlTemplate: (address: string, lat?: number, lng?: number) => {
				if (lat !== undefined && lng !== undefined) {
					return `https://www.google.com/maps?q=${lat},${lng}`;
				}
				return `https://www.google.com/maps/search/${encodeURIComponent(address)}`;
			},
			isAvailable: () => true
		}
	];

	// Platform-specific apps
	if (platform === 'ios') {
		apps.push({
			name: 'Apple Maps',
			icon: '🍎',
			urlTemplate: (address: string, lat?: number, lng?: number) => {
				if (lat !== undefined && lng !== undefined) {
					return `maps://?q=${lat},${lng}`;
				}
				return `maps://?q=${encodeURIComponent(address)}`;
			},
			isAvailable: () => true
		});
	}

	if (platform === 'android') {
		apps.push({
			name: 'Google Maps App',
			icon: '📱',
			urlTemplate: (address: string, lat?: number, lng?: number) => {
				if (lat !== undefined && lng !== undefined) {
					return `geo:${lat},${lng}?q=${lat},${lng}`;
				}
				return `geo:0,0?q=${encodeURIComponent(address)}`;
			},
			isAvailable: () => true
		});
	}

	// Additional universal options
	apps.push(
		{
			name: 'OpenStreetMap',
			icon: '🌍',
			urlTemplate: (address: string, lat?: number, lng?: number) => {
				if (lat !== undefined && lng !== undefined) {
					return `https://www.openstreetmap.org/?mlat=${lat}&mlon=${lng}&zoom=16`;
				}
				return `https://www.openstreetmap.org/search?query=${encodeURIComponent(address)}`;
			},
			isAvailable: () => true
		},
		{
			name: 'Waze',
			icon: '🚗',
			urlTemplate: (address: string, lat?: number, lng?: number) => {
				if (lat !== undefined && lng !== undefined) {
					return `https://waze.com/ul?ll=${lat},${lng}&navigate=yes`;
				}
				return `https://waze.com/ul?q=${encodeURIComponent(address)}`;
			},
			isAvailable: () => true
		}
	);

	return apps.filter((app) => app.isAvailable());
}

/**
 * Copies text to clipboard with fallback for older browsers
 */
export async function copyToClipboard(text: string): Promise<boolean> {
	try {
		// Modern clipboard API
		if (navigator.clipboard && window.isSecureContext) {
			await navigator.clipboard.writeText(text);
			return true;
		}

		// Fallback for older browsers or non-secure contexts
		const textArea = document.createElement('textarea');
		textArea.value = text;
		textArea.style.position = 'fixed';
		textArea.style.opacity = '0';
		textArea.style.left = '-9999px';
		textArea.style.top = '-9999px';

		document.body.appendChild(textArea);
		textArea.focus();
		textArea.select();

		const successful = document.execCommand('copy');
		document.body.removeChild(textArea);

		return successful;
	} catch (error) {
		console.warn('Failed to copy to clipboard:', error);
		return false;
	}
}

/**
 * Opens a URL in a new tab/window, with fallback for app URLs
 */
export function openUrl(url: string): void {
	try {
		// For app URLs (like maps:// or geo:), try to open directly
		if (url.startsWith('maps://') || url.startsWith('geo:')) {
			window.location.href = url;
		} else {
			// For web URLs, open in new tab
			window.open(url, '_blank', 'noopener,noreferrer');
		}
	} catch (error) {
		console.warn('Failed to open URL:', url, error);
		// Fallback: try direct navigation
		try {
			window.location.href = url;
		} catch (fallbackError) {
			console.error('All attempts to open URL failed:', url, fallbackError);
		}
	}
}

/**
 * Shows a toast-like notification (simple implementation)
 */
export function showNotification(
	message: string,
	type: 'success' | 'error' | 'info' = 'info'
): void {
	// Create a simple toast notification
	const toast = document.createElement('div');
	toast.textContent = message;
	toast.style.cssText = `
		position: fixed;
		top: 20px;
		right: 20px;
		padding: 12px 20px;
		border-radius: 6px;
		color: white;
		font-weight: 500;
		font-size: 14px;
		z-index: 10000;
		max-width: 300px;
		word-wrap: break-word;
		box-shadow: 0 4px 12px rgba(0, 0, 0, 0.15);
		transition: opacity 0.3s ease;
		${type === 'success' ? 'background: #10b981;' : ''}
		${type === 'error' ? 'background: #ef4444;' : ''}
		${type === 'info' ? 'background: #3b82f6;' : ''}
	`;

	document.body.appendChild(toast);

	// Auto-remove after 3 seconds
	setTimeout(() => {
		toast.style.opacity = '0';
		setTimeout(() => {
			if (toast.parentNode) {
				toast.parentNode.removeChild(toast);
			}
		}, 300);
	}, 3000);
}

/**
 * Formats an address for display and map usage
 */
export function formatAddressForMap(slot: any): {
	displayAddress: string;
	lat?: number;
	lng?: number;
	hasCoordinates: boolean;
	hasAddressComponents: boolean;
} {
	let displayAddress = '';
	let lat: number | undefined;
	let lng: number | undefined;
	let hasCoordinates = false;
	let hasAddressComponents = false;

	if (slot.location_type === 'Specific') {
		// Build address from components
		const addressParts = [];
		if (slot.street_address) addressParts.push(slot.street_address);
		if (slot.city) addressParts.push(slot.city);
		if (slot.state_province) addressParts.push(slot.state_province);
		if (slot.postal_code) addressParts.push(slot.postal_code);
		if (slot.country) addressParts.push(slot.country);

		if (addressParts.length > 0) {
			displayAddress = addressParts.join(', ');
			hasAddressComponents = true;
		}

		// Get coordinates if available
		if (slot.latitude !== undefined && slot.longitude !== undefined) {
			lat = slot.latitude;
			lng = slot.longitude;
			hasCoordinates = true;

			// If no address components, use coordinates as display
			if (!displayAddress) {
				displayAddress = `${slot.latitude.toFixed(6)}, ${slot.longitude.toFixed(6)}`;
			}
		}
	} else {
		displayAddress = slot.location_type || 'No location';
	}

	return {
		displayAddress,
		lat,
		lng,
		hasCoordinates,
		hasAddressComponents
	};
}

/**
 * Creates a map app selector modal and handles the user's choice
 */
export function showMapAppSelector(
	address: string,
	lat?: number,
	lng?: number,
	onAppSelected?: (app: MapAppOption) => void
): void {
	const apps = getAvailableMapApps();

	// Create modal backdrop
	const backdrop = document.createElement('div');
	backdrop.style.cssText = `
		position: fixed;
		top: 0;
		left: 0;
		right: 0;
		bottom: 0;
		background: rgba(0, 0, 0, 0.5);
		z-index: 10000;
		display: flex;
		align-items: center;
		justify-content: center;
		padding: 20px;
	`;

	// Create modal content
	const modal = document.createElement('div');
	modal.style.cssText = `
		background: white;
		border-radius: 12px;
		padding: 24px;
		max-width: 400px;
		width: 100%;
		box-shadow: 0 20px 25px -5px rgba(0, 0, 0, 0.1);
		animation: modalSlideIn 0.2s ease-out;
	`;

	// Add animation keyframes
	const style = document.createElement('style');
	style.textContent = `
		@keyframes modalSlideIn {
			from { opacity: 0; transform: scale(0.95) translateY(-10px); }
			to { opacity: 1; transform: scale(1) translateY(0); }
		}
	`;
	document.head.appendChild(style);

	// Modal content
	modal.innerHTML = `
		<div style="margin-bottom: 20px;">
			<h3 style="margin: 0 0 8px 0; font-size: 18px; font-weight: 600; color: #111827;">
				Open in Map App
			</h3>
			<p style="margin: 0; color: #6b7280; font-size: 14px; word-break: break-word;">
				${address}
			</p>
		</div>
		<div style="display: grid; gap: 8px;">
			${apps
				.map(
					(app, index) => `
				<button 
					data-app-index="${index}"
					style="
						display: flex; 
						align-items: center; 
						gap: 12px; 
						padding: 12px 16px; 
						border: 1px solid #e5e7eb; 
						border-radius: 8px; 
						background: white; 
						cursor: pointer; 
						transition: all 0.2s ease;
						font-size: 14px;
						font-weight: 500;
						color: #374151;
						text-align: left;
						width: 100%;
					"
					onmouseover="this.style.background='#f9fafb'; this.style.borderColor='#d1d5db';"
					onmouseout="this.style.background='white'; this.style.borderColor='#e5e7eb';"
				>
					<span style="font-size: 18px;">${app.icon}</span>
					<span>${app.name}</span>
				</button>
			`
				)
				.join('')}
		</div>
		<div style="margin-top: 16px; padding-top: 16px; border-top: 1px solid #f3f4f6;">
			<button 
				id="copy-address-btn"
				style="
					display: flex; 
					align-items: center; 
					gap: 8px; 
					padding: 8px 12px; 
					border: 1px solid #d1d5db; 
					border-radius: 6px; 
					background: #f9fafb; 
					cursor: pointer; 
					font-size: 13px;
					color: #6b7280;
					width: 100%;
					justify-content: center;
					transition: all 0.2s ease;
				"
				onmouseover="this.style.background='#f3f4f6';"
				onmouseout="this.style.background='#f9fafb';"
			>
				<span>📋</span>
				<span>Copy Address to Clipboard</span>
			</button>
		</div>
		<div style="margin-top: 12px; text-align: center;">
			<button 
				id="cancel-btn"
				style="
					padding: 6px 12px; 
					border: none; 
					background: none; 
					cursor: pointer; 
					color: #9ca3af;
					font-size: 13px;
					transition: color 0.2s ease;
				"
				onmouseover="this.style.color='#6b7280';"
				onmouseout="this.style.color='#9ca3af';"
			>
				Cancel
			</button>
		</div>
	`;

	backdrop.appendChild(modal);
	document.body.appendChild(backdrop);

	// Handle app selection
	apps.forEach((app, index) => {
		const button = modal.querySelector(`[data-app-index="${index}"]`) as HTMLButtonElement;
		if (button) {
			button.addEventListener('click', () => {
				const url = app.urlTemplate(address, lat, lng);
				openUrl(url);
				onAppSelected?.(app);
				cleanup();
			});
		}
	});

	// Handle copy to clipboard
	const copyBtn = modal.querySelector('#copy-address-btn') as HTMLButtonElement;
	if (copyBtn) {
		copyBtn.addEventListener('click', async () => {
			const success = await copyToClipboard(address);
			if (success) {
				copyBtn.innerHTML = '<span>✅</span><span>Copied to Clipboard!</span>';
				copyBtn.style.color = '#10b981';
				setTimeout(cleanup, 1000);
			} else {
				copyBtn.innerHTML = '<span>❌</span><span>Copy Failed</span>';
				copyBtn.style.color = '#ef4444';
			}
		});
	}

	// Handle cancel
	const cancelBtn = modal.querySelector('#cancel-btn') as HTMLButtonElement;
	if (cancelBtn) {
		cancelBtn.addEventListener('click', cleanup);
	}

	// Handle backdrop click
	backdrop.addEventListener('click', (e) => {
		if (e.target === backdrop) {
			cleanup();
		}
	});

	// Handle escape key
	const handleEscape = (e: KeyboardEvent) => {
		if (e.key === 'Escape') {
			cleanup();
		}
	};
	document.addEventListener('keydown', handleEscape);

	function cleanup() {
		document.removeEventListener('keydown', handleEscape);
		if (backdrop.parentNode) {
			backdrop.parentNode.removeChild(backdrop);
		}
		if (style.parentNode) {
			style.parentNode.removeChild(style);
		}
	}
}

/**
 * Main function to handle address/coordinate clicking
 */
export async function handleAddressClick(
	slot: any,
	options: {
		copyToClipboard?: boolean;
		showAppSelector?: boolean;
		onSuccess?: (action: 'copied' | 'opened', app?: MapAppOption) => void;
		onError?: (error: string) => void;
	} = {}
): Promise<void> {
	const {
		copyToClipboard: shouldCopy = true,
		showAppSelector = true,
		onSuccess,
		onError
	} = options;

	try {
		const { displayAddress, lat, lng } = formatAddressForMap(slot);

		if (!displayAddress || displayAddress === 'No location') {
			onError?.('No address or coordinates available');
			showNotification('No location data available', 'error');
			return;
		}

		// Copy to clipboard
		if (shouldCopy) {
			const copySuccess = await copyToClipboard(displayAddress);
			if (copySuccess) {
				onSuccess?.('copied');
				showNotification('Address copied to clipboard!', 'success');
			} else {
				console.warn('Failed to copy address to clipboard');
			}
		}

		// Show app selector
		if (showAppSelector) {
			showMapAppSelector(displayAddress, lat, lng, (app) => {
				onSuccess?.('opened', app);
				showNotification(`Opening in ${app.name}...`, 'info');
			});
		}
	} catch (error) {
		const errorMessage = error instanceof Error ? error.message : 'Unknown error occurred';
		onError?.(errorMessage);
		showNotification('Failed to process location', 'error');
		console.error('Address click handling error:', error);
	}
}
