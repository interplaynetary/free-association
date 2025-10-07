import { get } from 'svelte/store';
import { holster, user, userAlias, userPub } from './holster.svelte';

// Development-only test utilities
export function initializeHolsterTests() {
	if (!import.meta.env.DEV || typeof window === 'undefined') {
		return;
	}

	(window as any).testHolster = {
		// Basic operations test
		testBasic: () => {
			console.log('[HOLSTER-TEST] Testing basic put/get operations...');
			holster.get('test-key').put('test-value', (err) => {
				if (err) {
					console.error('[HOLSTER-TEST] Put failed:', err);
					return;
				}
				console.log('[HOLSTER-TEST] ✅ Put succeeded');

				holster.get('test-key', (data) => {
					console.log('[HOLSTER-TEST] Get result:', data);
					if (data === 'test-value') {
						console.log('[HOLSTER-TEST] ✅ Basic operations working!');
					} else {
						console.error('[HOLSTER-TEST] ❌ Value mismatch:', data);
					}
				});
			});
		},

		// Check connection and status
		checkStatus: () => {
			console.log('[HOLSTER-TEST] === Holster Status ===');
			console.log('[HOLSTER-TEST] Holster instance:', holster);
			console.log('[HOLSTER-TEST] User instance:', user);
			console.log('[HOLSTER-TEST] User authenticated:', !!user.is);
			if (user.is) {
				console.log('[HOLSTER-TEST] Username:', user.is.username);
				console.log('[HOLSTER-TEST] Public key:', user.is.pub?.slice(0, 20) + '...');
			}
			console.log('[HOLSTER-TEST] User stores:', {
				alias: get(userAlias),
				pub: get(userPub)?.slice(0, 20) + '...'
			});
		},

		// Authentication tests
		auth: {
			// Generate unique test username
			_generateUsername: () => {
				return `test-user-${Date.now()}-${Math.random().toString(36).substring(7)}`;
			},

			// Create and login to test account
			createAndLogin: async () => {
				const username = (window as any).testHolster.auth._generateUsername();
				console.log(`[HOLSTER-AUTH] Creating and logging in: ${username}`);

				// Create account first
				await new Promise((resolve) => {
					user.create(username, 'test-password-123', (err) => {
						if (err) {
							console.error('[HOLSTER-AUTH] ❌ Create failed:', err);
						} else {
							console.log('[HOLSTER-AUTH] ✅ Account created successfully');
						}
						resolve(null);
					});
				});

				// Then login
				return new Promise((resolve) => {
					user.auth(username, 'test-password-123', (err) => {
						if (err) {
							console.error('[HOLSTER-AUTH] ❌ Login failed:', err);
						} else {
							console.log('[HOLSTER-AUTH] ✅ Login successful');
							console.log('[HOLSTER-AUTH] User:', user.is);
							userAlias.set(user.is.username);
							userPub.set(user.is.pub);
							user.store();
						}
						resolve(err);
					});
				});
			},

			// Logout
			logout: () => {
				console.log('[HOLSTER-AUTH] Logging out...');
				user.leave();
				userAlias.set('');
				userPub.set('');
				console.log('[HOLSTER-AUTH] ✅ Logged out. User state:', user.is);
			},

			// Full auth flow test
			fullTest: async () => {
				console.log('[HOLSTER-AUTH] === Running full auth flow test ===');

				console.log('[HOLSTER-AUTH] Step 1: Create and login...');
				await (window as any).testHolster.auth.createAndLogin();
				await new Promise((r) => setTimeout(r, 1000));

				console.log('[HOLSTER-AUTH] Step 2: Logout...');
				(window as any).testHolster.auth.logout();

				console.log('[HOLSTER-AUTH] === Full test complete ===');
			}
		},

		// User data operations
		userData: {
			// Write user data
			write: async (key: string, value: any) => {
				if (!user.is) {
					console.error('[HOLSTER-DATA] ❌ Not authenticated');
					return;
				}

				console.log(`[HOLSTER-DATA] Writing "${key}":`, value);
				return new Promise((resolve) => {
					user.get(key).put(value, (err) => {
						if (err) {
							console.error('[HOLSTER-DATA] ❌ Write failed:', err);
						} else {
							console.log('[HOLSTER-DATA] ✅ Write succeeded');
						}
						resolve(err);
					});
				});
			},

			// Read user data
			read: (key: string) => {
				if (!user.is) {
					console.error('[HOLSTER-DATA] ❌ Not authenticated');
					return;
				}

				console.log(`[HOLSTER-DATA] Reading "${key}"...`);
				user.get(key, (data) => {
					console.log('[HOLSTER-DATA] Result:', data);
				});
			},

			// Test write and read (creates user if needed)
			testUserData: async () => {
				console.log('[HOLSTER-DATA] === Testing user data operations ===');

				// Ensure user is logged in
				if (!user.is) {
					console.log('[HOLSTER-DATA] No user logged in, creating test user...');
					await (window as any).testHolster.auth.createAndLogin();
					await new Promise((r) => setTimeout(r, 500));
				}

				const testData = {
					test: 'hello',
					timestamp: Date.now()
				};

				await (window as any).testHolster.userData.write('test-data', testData);
				await new Promise((r) => setTimeout(r, 500));
				(window as any).testHolster.userData.read('test-data');
			}
		}
	};

	console.log('[HOLSTER] Test utilities available:');
	console.log('[HOLSTER]   window.testHolster.testBasic() - Test basic put/get');
	console.log('[HOLSTER]   window.testHolster.checkStatus() - Check Holster status');
	console.log('[HOLSTER]   window.testHolster.auth.fullTest() - Test full auth flow');
	console.log('[HOLSTER]   window.testHolster.userData.testUserData() - Test user data');
}
