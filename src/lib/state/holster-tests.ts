import { get } from 'svelte/store';
import { holster, holsterUser, holsterUserAlias, holsterUserPub } from './holster.svelte';

// Development-only test utilities
export function initializeHolsterTests() {
	if (!import.meta.env.DEV || typeof window === 'undefined') {
		return;
	}

	(window as any).testHolster = {
		// Basic operations test
		testBasic: () => {
			console.log('[HOLSTER-TEST] Testing basic put/get operations...');
			holster.get('test-key').put('test-value', (err: any) => {
				if (err) {
					console.error('[HOLSTER-TEST] Put failed:', err);
					return;
				}
				console.log('[HOLSTER-TEST] ✅ Put succeeded');

				holster.get('test-key', (data: any) => {
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
			console.log('[HOLSTER-TEST] User instance:', holsterUser);
			console.log('[HOLSTER-TEST] User authenticated:', !!holsterUser.is);
			if (holsterUser.is) {
				console.log('[HOLSTER-TEST] Username:', holsterUser.is.holsterUsername);
				console.log('[HOLSTER-TEST] Public key:', holsterUser.is.pub?.slice(0, 20) + '...');
			}
			console.log('[HOLSTER-TEST] User stores:', {
				alias: get(holsterUserAlias),
				pub: get(holsterUserPub)?.slice(0, 20) + '...'
			});
		},

		// Authentication tests
		auth: {
			// Generate unique test holsterUsername
			_generateUsername: () => {
				return `test-holsterUser-${Date.now()}-${Math.random().toString(36).substring(7)}`;
			},

			// Create and login to test account
			createAndLogin: async () => {
				const holsterUsername = (window as any).testHolster.auth._generateUsername();
				console.log(`[HOLSTER-AUTH] Creating and logging in: ${holsterUsername}`);

				// Create account first
				await new Promise((resolve) => {
					holsterUser.create(holsterUsername, 'test-password-123', (err: any) => {
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
					holsterUser.auth(holsterUsername, 'test-password-123', (err: any) => {
						if (err) {
							console.error('[HOLSTER-AUTH] ❌ Login failed:', err);
						} else {
							console.log('[HOLSTER-AUTH] ✅ Login successful');
							console.log('[HOLSTER-AUTH] User:', holsterUser.is);
							holsterUserAlias.set(holsterUser.is.username);
							holsterUserPub.set(holsterUser.is.pub);
							holsterUser.store(true);
						}
						resolve(err);
					});
				});
			},

			// Logout
			logout: () => {
				console.log('[HOLSTER-AUTH] Logging out...');
				holsterUser.leave();
				holsterUserAlias.set('');
				holsterUserPub.set('');
				console.log('[HOLSTER-AUTH] ✅ Logged out. User state:', holsterUser.is);
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
		holsterUserData: {
			// Write holsterUser data
			write: async (key: string, value: any) => {
				if (!holsterUser.is) {
					console.error('[HOLSTER-DATA] ❌ Not authenticated');
					return;
				}

				console.log(`[HOLSTER-DATA] Writing "${key}":`, value);
				return new Promise((resolve) => {
					holsterUser.get(key).put(value, (err: any) => {
						if (err) {
							console.error('[HOLSTER-DATA] ❌ Write failed:', err);
						} else {
							console.log('[HOLSTER-DATA] ✅ Write succeeded');
						}
						resolve(err);
					});
				});
			},

			// Read holsterUser data
			read: (key: string) => {
				if (!holsterUser.is) {
					console.error('[HOLSTER-DATA] ❌ Not authenticated');
					return;
				}

				console.log(`[HOLSTER-DATA] Reading "${key}"...`);
				holsterUser.get(key, (data: any) => {
					console.log('[HOLSTER-DATA] Result:', data);
				});
			},

			// Test write and read (creates holsterUser if needed)
			testUserData: async () => {
				console.log('[HOLSTER-DATA] === Testing holsterUser data operations ===');

				// Ensure holsterUser is logged in
				if (!holsterUser.is) {
					console.log('[HOLSTER-DATA] No holsterUser logged in, creating test holsterUser...');
					await (window as any).testHolster.auth.createAndLogin();
					await new Promise((r) => setTimeout(r, 500));
				}

				const testData = {
					test: 'hello',
					timestamp: Date.now()
				};

				await (window as any).testHolster.holsterUserData.write('test-data', testData);
				await new Promise((r) => setTimeout(r, 500));
				(window as any).testHolster.holsterUserData.read('test-data');
			}
		}
	};

	console.log('[HOLSTER] Test utilities available:');
	console.log('[HOLSTER]   window.testHolster.testBasic() - Test basic put/get');
	console.log('[HOLSTER]   window.testHolster.checkStatus() - Check Holster status');
	console.log('[HOLSTER]   window.testHolster.auth.fullTest() - Test full auth flow');
	console.log('[HOLSTER]   window.testHolster.holsterUserData.testUserData() - Test holsterUser data');
}
