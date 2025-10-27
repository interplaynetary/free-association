import { Workbox, type WorkboxLifecycleEvent } from 'workbox-window';
import toast from 'svelte-french-toast';

let wb: Workbox | undefined;

/**
 * Register service worker and handle updates
 */
export function registerServiceWorker() {
	if (!('serviceWorker' in navigator)) {
		console.log('Service Worker not supported');
		return;
	}

	// Create a new Workbox instance
	wb = new Workbox('/service-worker.js', {
		scope: '/'
	});

	// Show a prompt when a service worker has installed but is waiting to activate
	wb.addEventListener('waiting', (event: WorkboxLifecycleEvent) => {
		console.log('A new service worker has installed, but it is waiting to activate.');
		
		showUpdatePrompt();
	});

	// Automatically reload when the service worker has been activated
	wb.addEventListener('controlling', (event: WorkboxLifecycleEvent) => {
		console.log('Service worker is now controlling the page');
		if (typeof window !== 'undefined') {
			window.location.reload();
		}
	});

	// Register the service worker
	wb.register()
		.then((registration: ServiceWorkerRegistration | undefined) => {
			console.log('Service Worker registered successfully:', registration);
		})
		.catch((error: Error) => {
			console.error('Service Worker registration failed:', error);
		});

	// Check for updates periodically (every hour)
	setInterval(() => {
		wb?.update();
	}, 60 * 60 * 1000);
}

/**
 * Show update prompt to user
 */
function showUpdatePrompt() {
	toast(
		(t: { id: string }) => {
			return `
				<div style="display: flex; flex-direction: column; gap: 8px;">
					<div style="font-weight: 600;">New Version Available</div>
					<div style="font-size: 14px; color: #666;">A new version of Playnet is ready to install.</div>
					<div style="display: flex; gap: 8px; margin-top: 8px;">
						<button 
							onclick="window.pwaUpdate()"
							style="padding: 8px 16px; background: #000; color: #fff; border: none; border-radius: 6px; cursor: pointer; font-weight: 500;"
						>
							Update Now
						</button>
						<button 
							onclick="window.pwaDismiss(${t.id})"
							style="padding: 8px 16px; background: #f3f4f6; color: #374151; border: none; border-radius: 6px; cursor: pointer;"
						>
							Later
						</button>
					</div>
				</div>
			`;
		},
		{
			duration: Infinity,
			position: 'top-center'
		}
	);
}

/**
 * Update the service worker (skip waiting)
 */
export function updateServiceWorker() {
	if (!wb) {
		console.error('Workbox instance not available');
		return;
	}

	// Send SKIP_WAITING message to waiting service worker
	wb.messageSkipWaiting();
	
	// The 'controlling' event will handle the reload
}

/**
 * Dismiss update notification
 */
export function dismissUpdateNotification(toastId: string) {
	toast.dismiss(toastId);
}

// Make functions available globally for toast buttons
declare global {
	interface Window {
		pwaUpdate: () => void;
		pwaDismiss: (toastId: string) => void;
	}
}

if (typeof window !== 'undefined') {
	window.pwaUpdate = updateServiceWorker;
	window.pwaDismiss = dismissUpdateNotification;
}

