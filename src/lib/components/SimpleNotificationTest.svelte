<script lang="ts">
	import { gun } from '$lib/state/gun.svelte';
	import { onMount } from 'svelte';

	let testMessage = $state('Hello from service worker notification test!');
	let status = $state('Ready to test');

	// Test the service worker notification system
	async function testNotification() {
		try {
			status = 'Sending test data...';
			console.log('[Test] Sending test data to Gun:', testMessage);

			// Update the Gun path that the service worker is watching
			await gun.get('test').get('paste').put(testMessage);

			status = 'Test data sent! Check for notification...';
			console.log('[Test] Test data sent to Gun');

			// Debug: Verify the data was written
			setTimeout(() => {
				gun
					.get('test')
					.get('paste')
					.once((data, key) => {
						console.log('[Test] Verification - data in Gun:', data, 'key:', key);
					});
			}, 500);

			// Reset status after a delay
			setTimeout(() => {
				status = 'Ready to test';
			}, 3000);
		} catch (error) {
			console.error('Error testing notification:', error);
			status = 'Error sending test data';
		}
	}

	// Test the notification system directly (bypass Gun)
	async function testDirectNotification() {
		try {
			status = 'Testing direct notification...';
			console.log('[Test] Testing direct notification');

			if ('serviceWorker' in navigator && navigator.serviceWorker.controller) {
				navigator.serviceWorker.controller.postMessage({
					type: 'TEST_NOTIFICATION',
					data: {
						message: testMessage
					}
				});

				status = 'Direct test sent! Check for notification...';
				console.log('[Test] Direct test message sent to service worker');
			} else {
				status = 'Service worker not available';
				console.error('[Test] Service worker not available');
			}

			// Reset status after a delay
			setTimeout(() => {
				status = 'Ready to test';
			}, 3000);
		} catch (error) {
			console.error('Error testing direct notification:', error);
			status = 'Error sending direct test';
		}
	}

	// Clear all notifications
	async function clearNotifications() {
		if ('serviceWorker' in navigator && navigator.serviceWorker.controller) {
			navigator.serviceWorker.controller.postMessage({
				type: 'CLEAR_ALL_NOTIFICATIONS'
			});
			status = 'Notifications cleared';
			console.log('[Test] Clear notifications message sent');
		}
	}

	// Test Gun specifically in service worker
	async function testGunInServiceWorker() {
		try {
			status = 'Testing Gun in service worker...';
			console.log('[Test] Testing Gun in service worker');

			if ('serviceWorker' in navigator && navigator.serviceWorker.controller) {
				navigator.serviceWorker.controller.postMessage({
					type: 'TEST_GUN',
					data: {
						message: 'Testing Gun from main thread'
					}
				});

				status = 'Gun test sent! Check SW console...';
				console.log('[Test] Gun test message sent to service worker');
			} else {
				status = 'Service worker not available';
				console.error('[Test] Service worker not available');
			}

			// Reset status after a delay
			setTimeout(() => {
				status = 'Ready to test';
			}, 3000);
		} catch (error) {
			console.error('Error testing Gun in service worker:', error);
			status = 'Error testing Gun';
		}
	}

	// Test if service worker is receiving messages at all
	async function pingServiceWorker() {
		try {
			status = 'Pinging service worker...';
			console.log('[Test] Pinging service worker');

			if ('serviceWorker' in navigator && navigator.serviceWorker.controller) {
				navigator.serviceWorker.controller.postMessage({
					type: 'PING',
					data: {
						timestamp: Date.now()
					}
				});

				status = 'Ping sent! Check SW console for response...';
				console.log('[Test] Ping message sent to service worker');
			} else {
				status = 'Service worker not available';
				console.error('[Test] Service worker not available');
			}

			// Reset status after a delay
			setTimeout(() => {
				status = 'Ready to test';
			}, 3000);
		} catch (error) {
			console.error('Error pinging service worker:', error);
			status = 'Error pinging service worker';
		}
	}

	// Debug the notification database
	async function debugNotifications() {
		try {
			status = 'Debugging notifications...';
			console.log('[Test] Debugging notification database');

			if ('serviceWorker' in navigator && navigator.serviceWorker.controller) {
				navigator.serviceWorker.controller.postMessage({
					type: 'DEBUG_NOTIFICATIONS'
				});

				status = 'Debug sent! Check SW console...';
				console.log('[Test] Debug message sent to service worker');
			} else {
				status = 'Service worker not available';
				console.error('[Test] Service worker not available');
			}

			// Reset status after a delay
			setTimeout(() => {
				status = 'Ready to test';
			}, 3000);
		} catch (error) {
			console.error('Error debugging notifications:', error);
			status = 'Error debugging notifications';
		}
	}

	// Clear processed notifications from database
	async function clearProcessedNotifications() {
		try {
			status = 'Clearing processed notifications...';
			console.log('[Test] Clearing processed notifications');

			if ('serviceWorker' in navigator && navigator.serviceWorker.controller) {
				navigator.serviceWorker.controller.postMessage({
					type: 'CLEAR_PROCESSED'
				});

				status = 'Clear processed sent! Check SW console...';
				console.log('[Test] Clear processed message sent to service worker');
			} else {
				status = 'Service worker not available';
				console.error('[Test] Service worker not available');
			}

			// Reset status after a delay
			setTimeout(() => {
				status = 'Ready to test';
			}, 3000);
		} catch (error) {
			console.error('Error clearing processed notifications:', error);
			status = 'Error clearing processed notifications';
		}
	}

	// Test paste path from service worker
	async function testPasteFromSW() {
		try {
			status = 'Testing paste from service worker...';
			console.log('[Test] Testing paste path from service worker');

			if ('serviceWorker' in navigator && navigator.serviceWorker.controller) {
				navigator.serviceWorker.controller.postMessage({
					type: 'TEST_PASTE_FROM_SW'
				});

				status = 'SW paste test sent! Check SW console...';
				console.log('[Test] SW paste test message sent to service worker');
			} else {
				status = 'Service worker not available';
				console.error('[Test] Service worker not available');
			}

			// Reset status after a delay
			setTimeout(() => {
				status = 'Ready to test';
			}, 3000);
		} catch (error) {
			console.error('Error testing paste from service worker:', error);
			status = 'Error testing paste from service worker';
		}
	}

	// Debug Gun connectivity comparison
	async function debugGunConnectivity() {
		try {
			status = 'Debugging Gun connectivity...';
			console.log('[Test] === MAIN THREAD GUN DEBUG ===');
			console.log('[Test] Gun instance:', gun);
			console.log('[Test] Gun options:', gun._.opt);
			console.log('[Test] Gun state:', gun._);

			// Check current data in paste path
			gun
				.get('test')
				.get('paste')
				.once((data, key) => {
					console.log('[Test] Current data in test/paste from main thread:', data, 'key:', key);
				});

			// Ask service worker to debug its Gun state
			if ('serviceWorker' in navigator && navigator.serviceWorker.controller) {
				navigator.serviceWorker.controller.postMessage({
					type: 'DEBUG_GUN_CONNECTIVITY'
				});
			}

			status = 'Gun debug complete! Check console...';
			console.log('[Test] Gun connectivity debug sent to service worker');

			// Reset status after a delay
			setTimeout(() => {
				status = 'Ready to test';
			}, 3000);
		} catch (error) {
			console.error('Error debugging Gun connectivity:', error);
			status = 'Error debugging Gun connectivity';
		}
	}

	// Quick check of main thread data
	async function checkMainThreadData() {
		try {
			status = 'Checking main thread data...';
			console.log('[Test] === MAIN THREAD DATA CHECK ===');

			// Check what's in the test/paste path right now
			gun
				.get('test')
				.get('paste')
				.once((data, key) => {
					console.log('[Test] Main thread sees in test/paste:', data, 'key:', key);
				});

			// Check the broader test node
			gun.get('test').once((data, key) => {
				console.log('[Test] Main thread sees in test node:', data, 'key:', key);
			});

			status = 'Main thread data check complete!';

			// Reset status after a delay
			setTimeout(() => {
				status = 'Ready to test';
			}, 2000);
		} catch (error) {
			console.error('Error checking main thread data:', error);
			status = 'Error checking main thread data';
		}
	}

	// Check main thread Gun peer connectivity
	async function checkMainThreadPeers() {
		try {
			status = 'Checking main thread peers...';
			console.log('[Test] === MAIN THREAD PEER CHECK ===');
			console.log('[Test] Gun instance:', gun);
			console.log('[Test] Gun internal state:', gun._);
			console.log('[Test] Gun options:', gun._.opt);

			// Test manual WebSocket to same peers
			console.log('[Test] Testing WebSocket connection from main thread...');
			try {
				const testWS = new WebSocket('wss://gun-manhattan.herokuapp.com/gun');
				testWS.onopen = () => {
					console.log('[Test] Main thread WebSocket test: SUCCESS');
					testWS.close();
				};
				testWS.onerror = (error) => {
					console.error('[Test] Main thread WebSocket test: FAILED -', error);
				};
			} catch (wsError) {
				console.error('[Test] Main thread WebSocket test: EXCEPTION -', wsError);
			}

			status = 'Main thread peer check complete!';

			// Reset status after a delay
			setTimeout(() => {
				status = 'Ready to test';
			}, 2000);
		} catch (error) {
			console.error('Error checking main thread peers:', error);
			status = 'Error checking main thread peers';
		}
	}

	// Force reconnection in service worker
	async function forceReconnect() {
		try {
			status = 'Forcing reconnection...';
			console.log('[Test] Forcing Gun reconnection in service worker');

			if ('serviceWorker' in navigator && navigator.serviceWorker.controller) {
				navigator.serviceWorker.controller.postMessage({
					type: 'FORCE_RECONNECT'
				});

				status = 'Reconnection forced! Check SW console...';
				console.log('[Test] Force reconnection message sent to service worker');
			} else {
				status = 'Service worker not available';
				console.error('[Test] Service worker not available');
			}

			// Reset status after a delay
			setTimeout(() => {
				status = 'Ready to test';
			}, 3000);
		} catch (error) {
			console.error('Error forcing reconnection:', error);
			status = 'Error forcing reconnection';
		}
	}

	// Get peer status from service worker
	async function getPeerStatus() {
		try {
			status = 'Getting peer status...';
			console.log('[Test] Getting peer status from service worker');

			if ('serviceWorker' in navigator && navigator.serviceWorker.controller) {
				navigator.serviceWorker.controller.postMessage({
					type: 'GET_PEER_STATUS'
				});

				status = 'Peer status requested! Check SW console...';
				console.log('[Test] Peer status message sent to service worker');
			} else {
				status = 'Service worker not available';
				console.error('[Test] Service worker not available');
			}

			// Reset status after a delay
			setTimeout(() => {
				status = 'Ready to test';
			}, 3000);
		} catch (error) {
			console.error('Error getting peer status:', error);
			status = 'Error getting peer status';
		}
	}

	// Start connectivity monitoring
	async function startMonitoring() {
		try {
			status = 'Starting monitoring...';
			console.log('[Test] Starting connectivity monitoring');

			if ('serviceWorker' in navigator && navigator.serviceWorker.controller) {
				navigator.serviceWorker.controller.postMessage({
					type: 'START_MONITORING'
				});

				status = 'Monitoring started! Check SW console...';
				console.log('[Test] Start monitoring message sent to service worker');
			} else {
				status = 'Service worker not available';
				console.error('[Test] Service worker not available');
			}

			// Reset status after a delay
			setTimeout(() => {
				status = 'Ready to test';
			}, 3000);
		} catch (error) {
			console.error('Error starting monitoring:', error);
			status = 'Error starting monitoring';
		}
	}

	// Stop connectivity monitoring
	async function stopMonitoring() {
		try {
			status = 'Stopping monitoring...';
			console.log('[Test] Stopping connectivity monitoring');

			if ('serviceWorker' in navigator && navigator.serviceWorker.controller) {
				navigator.serviceWorker.controller.postMessage({
					type: 'STOP_MONITORING'
				});

				status = 'Monitoring stopped! Check SW console...';
				console.log('[Test] Stop monitoring message sent to service worker');
			} else {
				status = 'Service worker not available';
				console.error('[Test] Service worker not available');
			}

			// Reset status after a delay
			setTimeout(() => {
				status = 'Ready to test';
			}, 3000);
		} catch (error) {
			console.error('Error stopping monitoring:', error);
			status = 'Error stopping monitoring';
		}
	}

	onMount(() => {
		// Request notification permission if not already granted
		if ('Notification' in window && Notification.permission === 'default') {
			Notification.requestPermission().then((permission) => {
				if (permission === 'granted') {
					console.log('[Test] Notification permission granted');
				} else {
					console.log('[Test] Notification permission denied:', permission);
				}
			});
		}

		console.log('[Test] Component mounted');
		console.log('[Test] Notification permission:', Notification.permission);
		console.log('[Test] Service worker supported:', 'serviceWorker' in navigator);
	});
</script>

<div class="notification-test">
	<h3>Service Worker Notification Test</h3>
	<p>Status: {status}</p>

	<div class="test-controls">
		<input
			type="text"
			bind:value={testMessage}
			placeholder="Enter test message"
			class="test-input"
		/>
		<button onclick={testNotification} class="test-button"> Send Test Notification </button>
		<button onclick={testDirectNotification} class="test-button"> Test Direct Notification </button>
		<button onclick={clearNotifications} class="clear-button"> Clear All Notifications </button>
		<button onclick={testGunInServiceWorker} class="test-button">
			Test Gun in Service Worker
		</button>
		<button onclick={pingServiceWorker} class="test-button"> Ping Service Worker </button>
		<button onclick={debugNotifications} class="test-button"> Debug Notifications </button>
		<button onclick={clearProcessedNotifications} class="test-button">
			Clear Processed Notifications
		</button>
		<button onclick={testPasteFromSW} class="test-button"> Test Paste Path from SW </button>
		<button onclick={debugGunConnectivity} class="test-button"> Debug Gun Connectivity </button>
		<button onclick={checkMainThreadData} class="test-button"> Check Main Thread Data </button>
		<button onclick={checkMainThreadPeers} class="test-button"> Check Main Thread Peers </button>
		<button onclick={forceReconnect} class="test-button"> Force Reconnection </button>
		<button onclick={getPeerStatus} class="test-button"> Get Peer Status </button>
		<button onclick={startMonitoring} class="test-button"> Start Monitoring </button>
		<button onclick={stopMonitoring} class="test-button"> Stop Monitoring </button>
	</div>

	<div class="info">
		<p><strong>How it works:</strong></p>
		<ul>
			<li>Service worker subscribes to <code>gun.get('test').get('paste')</code></li>
			<li>When you click "Send Test Notification", data is sent to that Gun path</li>
			<li>Service worker receives the data and shows a notification</li>
			<li>Notifications are managed via IndexedDB queue</li>
		</ul>

		<p><strong>Debug Info:</strong></p>
		<ul>
			<li>Notification permission: <code>{Notification.permission}</code></li>
			<li>Service worker supported: <code>{'serviceWorker' in navigator}</code></li>
			<li>
				Service worker controller: <code
					>{navigator.serviceWorker?.controller ? 'Available' : 'Not available'}</code
				>
			</li>
		</ul>

		<p><strong>Troubleshooting:</strong></p>
		<ul>
			<li>1. Try "Test Direct Notification" to bypass Gun</li>
			<li>2. Check browser console for detailed logs</li>
			<li>3. Check DevTools → Application → Service Workers for SW console</li>
			<li>4. Ensure notification permission is granted</li>
		</ul>

		<p><strong>Gun Connectivity Features:</strong></p>
		<ul>
			<li>
				<strong>Force Reconnection:</strong> Manually trigger Gun peer reconnection in service worker
			</li>
			<li><strong>Get Peer Status:</strong> Check current Gun peer connection status</li>
			<li>
				<strong>Start/Stop Monitoring:</strong> Control automatic connectivity monitoring (30s intervals)
			</li>
			<li>
				<strong>Debug Gun Connectivity:</strong> Compare Gun state between main thread and service worker
			</li>
		</ul>

		<p><strong>Reconnection Logic:</strong></p>
		<ul>
			<li>Automatic reconnection attempts when peers disconnect</li>
			<li>Periodic connectivity checks every 30 seconds</li>
			<li>Re-adds peers to Gun options and sends heartbeat to force connection</li>
			<li>Addresses Gun.js offline/online reconnection issues</li>
		</ul>
	</div>
</div>

<style>
	.notification-test {
		border: 1px solid #ddd;
		border-radius: 8px;
		padding: 1rem;
		margin: 1rem 0;
		background: #f9f9f9;
	}

	.test-controls {
		display: flex;
		gap: 0.5rem;
		margin: 1rem 0;
		flex-wrap: wrap;
	}

	.test-input {
		flex: 1;
		padding: 0.5rem;
		border: 1px solid #ccc;
		border-radius: 4px;
		min-width: 200px;
	}

	.test-button {
		background: #007bff;
		color: white;
		border: none;
		padding: 0.5rem 1rem;
		border-radius: 4px;
		cursor: pointer;
	}

	.test-button:hover {
		background: #0056b3;
	}

	.clear-button {
		background: #dc3545;
		color: white;
		border: none;
		padding: 0.5rem 1rem;
		border-radius: 4px;
		cursor: pointer;
	}

	.clear-button:hover {
		background: #c82333;
	}

	.info {
		margin-top: 1rem;
		padding: 1rem;
		background: #e9ecef;
		border-radius: 4px;
		font-size: 0.9rem;
	}

	.info code {
		background: #fff;
		padding: 0.2rem 0.4rem;
		border-radius: 3px;
		font-family: monospace;
	}

	.info ul {
		margin: 0.5rem 0;
		padding-left: 1.5rem;
	}
</style>
