<script lang="ts">
	import { onMount } from 'svelte';
	import { globalState, type UserData } from '$lib/global.svelte';

	// Reactive stores for user state
	let isAuthenticated = $state(false);
	let username = $state('');
	let password = $state('');
	let userAlias = $state('');
	let errorMessage = $state('');
	let isLoading = $state(false);

	// Mock user data - in a real implementation, this would come from a backend
	const mockUsers = {
		alice: { password: 'password123', alias: 'Alice', pub: 'ALICE123' },
		bob: { password: 'password123', alias: 'Bob', pub: 'BOB456' },
		charlie: { password: 'password123', alias: 'Charlie', pub: 'CHARLIE789' }
	};

	// Mock user object
	let currentUser = $state<{
		is: { pub: string; alias: string; username?: string } | null;
	}>({ is: null });

	// Check if user is already logged in on mount
	onMount(async () => {
		isLoading = true;

		try {
			// Check for stored credentials in localStorage
			const storedUser = localStorage.getItem('centralizedUser');
			if (storedUser) {
				const userData = JSON.parse(storedUser);
				currentUser.is = {
					pub: userData.pub,
					alias: userData.alias,
					username: userData.username
				};
				isAuthenticated = true;
				userAlias = userData.alias;
			}
		} catch (error) {
			console.error('Error recalling user session:', error);
		} finally {
			isLoading = false;
		}
	});

	// Handle login/signup
	async function handleAuthenticate() {
		if (!username || !password) {
			errorMessage = 'Please enter both username and password';
			return;
		}

		errorMessage = '';
		isLoading = true;

		try {
			// Simulate authentication delay
			await new Promise((resolve) => setTimeout(resolve, 800));

			// Check if user exists in our mock data
			const user = mockUsers[username.toLowerCase() as keyof typeof mockUsers];

			if (user && user.password === password) {
				// Login successful
				const userData: UserData = {
					pub: user.pub,
					alias: user.alias,
					username: username.toLowerCase()
				};

				currentUser.is = { ...userData };
				isAuthenticated = true;
				userAlias = user.alias;

				// Store user in localStorage
				localStorage.setItem('centralizedUser', JSON.stringify(userData));

				// Update global state with the logged in user
				globalState.setCurrentUser(userData);

				// Reset form
				username = '';
				password = '';

				// Show success message
				globalState.showToast(`Logged in as ${userData.alias}`, 'success');
			} else {
				// No user found or password incorrect
				errorMessage = 'Invalid username or password';
			}
		} catch (error) {
			console.error('Authentication error:', error);
			errorMessage = error instanceof Error ? error.message : 'Authentication error';
		} finally {
			isLoading = false;
		}
	}

	// Handle logout
	function handleLogout() {
		localStorage.removeItem('centralizedUser');
		currentUser.is = null;
		isAuthenticated = false;
		userAlias = '';

		// Update global state to reflect logged out status
		globalState.setCurrentUser(null);
		globalState.showToast('Logged out successfully', 'info');
	}

	// Close the popup
	function closePopup() {
		// Emit an event or use a callback to notify parent
		const closeEvent = new CustomEvent('close');
		document.dispatchEvent(closeEvent);
	}

	// Generate avatar based on public key
	function generateAvatar(options: {
		pub: string;
		size?: number;
		round?: boolean;
		draw?: string;
		reflect?: boolean;
	}) {
		const size = options.size || 70;
		const pub = options.pub;

		// Generate a color based on public key
		const hue = Math.abs(pub.split('').reduce((a, b) => a + b.charCodeAt(0), 0) % 360);
		const saturation = 70;
		const lightness = 60;

		// Generate avatar SVG
		return `data:image/svg+xml,${encodeURIComponent(`
			<svg xmlns="http://www.w3.org/2000/svg" width="${size}" height="${size}" viewBox="0 0 ${size} ${size}">
				<rect width="100%" height="100%" fill="hsl(${hue}, ${saturation}%, ${lightness}%)" />
				<text x="50%" y="50%" font-family="Arial" font-size="${size / 2}px" fill="white" 
					text-anchor="middle" dominant-baseline="middle">
					${userAlias.charAt(0).toUpperCase()}
				</text>
			</svg>
		`)}`;
	}
</script>

<div class="login-container">
	{#if isLoading}
		<div class="loading">
			<div class="spinner"></div>
			<p>Authenticating...</p>
		</div>
	{:else if isAuthenticated}
		<div class="welcome-panel">
			<div class="avatar">
				{#if currentUser.is?.pub}
					<img
						src={generateAvatar({
							pub: currentUser.is.pub,
							size: 70,
							round: true,
							draw: 'circles',
							reflect: true
						})}
						alt="User Avatar"
					/>
				{:else}
					<!-- Fallback avatar -->
					<svg
						xmlns="http://www.w3.org/2000/svg"
						width="40"
						height="40"
						viewBox="0 0 24 24"
						fill="none"
						stroke="currentColor"
						stroke-width="1.5"
					>
						<path d="M20 21v-2a4 4 0 0 0-4-4H8a4 4 0 0 0-4 4v2"></path>
						<circle cx="12" cy="7" r="4"></circle>
					</svg>
				{/if}
			</div>
			<h2>Welcome, {userAlias}</h2>
			<p>You are signed in to this tree</p>
			<div class="button-group">
				<button class="logout-btn" onclick={handleLogout}>
					<svg
						xmlns="http://www.w3.org/2000/svg"
						width="18"
						height="18"
						viewBox="0 0 24 24"
						fill="none"
						stroke="currentColor"
						stroke-width="2"
					>
						<path d="M9 21H5a2 2 0 0 1-2-2V5a2 2 0 0 1 2-2h4"></path>
						<polyline points="16 17 21 12 16 7"></polyline>
						<line x1="21" y1="12" x2="9" y2="12"></line>
					</svg>
					<span>Log Out</span>
				</button>
				<button class="close-btn" onclick={closePopup}>
					<span>Close</span>
				</button>
			</div>
		</div>
	{:else}
		<div class="login-form">
			<h2>Sign In / Sign Up</h2>

			{#if errorMessage}
				<div class="error-message">
					<svg
						xmlns="http://www.w3.org/2000/svg"
						width="16"
						height="16"
						viewBox="0 0 24 24"
						fill="none"
						stroke="currentColor"
						stroke-width="2"
					>
						<circle cx="12" cy="12" r="10"></circle>
						<line x1="12" y1="8" x2="12" y2="12"></line>
						<line x1="12" y1="16" x2="12.01" y2="16"></line>
					</svg>
					<span>{errorMessage}</span>
				</div>
			{/if}

			<div class="form-group">
				<label for="username">Username</label>
				<div class="input-wrapper">
					<svg
						xmlns="http://www.w3.org/2000/svg"
						width="16"
						height="16"
						viewBox="0 0 24 24"
						fill="none"
						stroke="currentColor"
						stroke-width="2"
					>
						<path d="M20 21v-2a4 4 0 0 0-4-4H8a4 4 0 0 0-4 4v2"></path>
						<circle cx="12" cy="7" r="4"></circle>
					</svg>
					<input
						type="text"
						id="username"
						bind:value={username}
						placeholder="Enter username"
						autocomplete="username"
					/>
				</div>
			</div>

			<div class="form-group">
				<label for="password">Password</label>
				<div class="input-wrapper">
					<svg
						xmlns="http://www.w3.org/2000/svg"
						width="16"
						height="16"
						viewBox="0 0 24 24"
						fill="none"
						stroke="currentColor"
						stroke-width="2"
					>
						<rect x="3" y="11" width="18" height="11" rx="2" ry="2"></rect>
						<path d="M7 11V7a5 5 0 0 1 10 0v4"></path>
					</svg>
					<input
						type="password"
						id="password"
						bind:value={password}
						placeholder="Enter password"
						autocomplete="current-password"
					/>
				</div>
			</div>

			<div class="button-group">
				<button
					class="submit-btn"
					onclick={handleAuthenticate}
					disabled={isLoading || !username || !password}
				>
					<svg
						xmlns="http://www.w3.org/2000/svg"
						width="18"
						height="18"
						viewBox="0 0 24 24"
						fill="none"
						stroke="currentColor"
						stroke-width="2"
					>
						<path d="M15 3h4a2 2 0 0 1 2 2v14a2 2 0 0 1-2 2h-4"></path>
						<polyline points="10 17 15 12 10 7"></polyline>
						<line x1="15" y1="12" x2="3" y2="12"></line>
					</svg>
					<span>Continue</span>
				</button>
				<button class="cancel-btn" onclick={closePopup}>
					<span>Cancel</span>
				</button>
			</div>

			<div class="note">
				<svg
					xmlns="http://www.w3.org/2000/svg"
					width="14"
					height="14"
					viewBox="0 0 24 24"
					fill="none"
					stroke="currentColor"
					stroke-width="2"
				>
					<circle cx="12" cy="12" r="10"></circle>
					<line x1="12" y1="16" x2="12" y2="12"></line>
					<line x1="12" y1="8" x2="12.01" y2="8"></line>
				</svg>
				<span>Secure login for tree access</span>
			</div>
		</div>
	{/if}
</div>

<style>
	.login-container {
		background: white;
		border-radius: 12px;
		overflow: hidden;
		font-family:
			system-ui,
			-apple-system,
			BlinkMacSystemFont,
			'Segoe UI',
			Roboto,
			sans-serif;
		width: 360px;
		box-shadow: 0 10px 25px rgba(0, 0, 0, 0.15);
		/* Center positioning */
		position: absolute;
		top: 50%;
		left: 50%;
		transform: translate(-50%, -50%);
	}

	h2 {
		color: #1e293b;
		font-weight: 600;
		margin-top: 0;
		margin-bottom: 0.75rem;
		font-size: 1.35rem;
		text-align: center;
	}

	.form-group {
		margin-bottom: 1.25rem;
	}

	label {
		display: block;
		color: #475569;
		font-weight: 500;
		margin-bottom: 0.35rem;
		font-size: 0.9rem;
	}

	.input-wrapper {
		position: relative;
		display: flex;
		align-items: center;
	}

	.input-wrapper svg {
		position: absolute;
		left: 12px;
		color: #64748b;
	}

	input {
		width: 100%;
		padding: 0.75rem 0.75rem 0.75rem 2.5rem;
		border: 1px solid #e2e8f0;
		border-radius: 8px;
		font-size: 0.95rem;
		color: #1e293b;
		transition: all 0.2s;
		background: #f8fafc;
	}

	input:focus {
		outline: none;
		border-color: #3060b0;
		box-shadow: 0 0 0 3px rgba(48, 96, 176, 0.15);
		background: #fff;
	}

	input::placeholder {
		color: #94a3b8;
	}

	.button-group {
		display: flex;
		gap: 12px;
		margin-top: 1.25rem;
	}

	.submit-btn {
		flex: 1;
		padding: 0.75rem;
		display: flex;
		align-items: center;
		justify-content: center;
		gap: 8px;
		background: #3060b0;
		color: white;
		border: none;
		border-radius: 8px;
		font-size: 0.95rem;
		font-weight: 500;
		cursor: pointer;
		transition: all 0.2s;
	}

	.submit-btn:hover {
		background: #2050a0;
		transform: translateY(-1px);
		box-shadow: 0 2px 8px rgba(48, 96, 176, 0.2);
	}

	.submit-btn:disabled {
		background: #94a3b8;
		cursor: not-allowed;
		transform: none;
		box-shadow: none;
	}

	.cancel-btn {
		padding: 0.75rem 1.25rem;
		background: #f1f5f9;
		color: #475569;
		border: none;
		border-radius: 8px;
		font-size: 0.95rem;
		font-weight: 500;
		cursor: pointer;
		transition: all 0.2s;
	}

	.cancel-btn:hover {
		background: #e2e8f0;
		transform: translateY(-1px);
	}

	.error-message {
		display: flex;
		align-items: center;
		gap: 10px;
		background: #fef2f2;
		color: #b91c1c;
		padding: 0.75rem 1rem;
		border-radius: 8px;
		margin-bottom: 1.25rem;
		font-size: 0.9rem;
		border-left: 3px solid #dc2626;
	}

	.loading {
		display: flex;
		flex-direction: column;
		align-items: center;
		justify-content: center;
		padding: 2.5rem;
	}

	.spinner {
		width: 36px;
		height: 36px;
		border: 3px solid rgba(48, 96, 176, 0.1);
		border-radius: 50%;
		border-top-color: #3060b0;
		animation: spin 0.8s ease-in-out infinite;
		margin-bottom: 1rem;
	}

	@keyframes spin {
		to {
			transform: rotate(360deg);
		}
	}

	.welcome-panel {
		display: flex;
		flex-direction: column;
		align-items: center;
		padding: 2rem 1.5rem;
		text-align: center;
	}

	.avatar {
		width: 70px;
		height: 70px;
		background: #e0e7ff;
		border-radius: 50%;
		display: flex;
		align-items: center;
		justify-content: center;
		margin-bottom: 1.5rem;
		color: #4f46e5;
		overflow: hidden;
	}

	.avatar img {
		width: 100%;
		height: 100%;
		object-fit: cover;
		border-radius: 50%;
	}

	.welcome-panel p {
		color: #64748b;
		margin-bottom: 2rem;
		font-size: 1rem;
	}

	.logout-btn {
		display: flex;
		align-items: center;
		gap: 8px;
		background: #ef4444;
		color: white;
		border: none;
		border-radius: 8px;
		padding: 0.75rem 1.25rem;
		font-size: 0.95rem;
		font-weight: 500;
		cursor: pointer;
		transition: all 0.2s;
	}

	.logout-btn:hover {
		background: #dc2626;
		transform: translateY(-1px);
		box-shadow: 0 2px 8px rgba(239, 68, 68, 0.2);
	}

	.close-btn {
		background: #f1f5f9;
		color: #475569;
		border: none;
		border-radius: 8px;
		padding: 0.75rem 1.25rem;
		font-size: 0.95rem;
		font-weight: 500;
		cursor: pointer;
		transition: all 0.2s;
	}

	.close-btn:hover {
		background: #e2e8f0;
		transform: translateY(-1px);
	}

	.note {
		display: flex;
		align-items: center;
		justify-content: center;
		gap: 6px;
		margin-top: 1.5rem;
		font-size: 0.8rem;
		color: #64748b;
		text-align: center;
	}

	.login-form,
	.welcome-panel {
		padding: 1.75rem;
	}
</style>
