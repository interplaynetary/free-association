<script lang="ts">
	import { globalState } from '$lib/global.svelte';
	import { Tree, enterChild, exitToParent } from '$lib/centralized';
	import { onMount } from 'svelte';
	import { page } from '$app/stores';
	import { goto } from '$app/navigation';
	import { user, login, signup, signout, username, userpub, type UserData } from '../gun/user';

	// Get current zipper, path and pathInfo from global state
	let currentZipper = $derived(globalState.currentZipper);
	let currentPath = $derived(globalState.currentPath);
	let pathInfo = $derived(globalState.pathInfo);

	// Get user data with Svelte 5 derived values
	// Use $ prefix for stores in Svelte 5
	let alias = $derived($username);
	let pub = $derived($userpub);
	let currentUser = $derived(
		pub
			? {
					pub,
					alias
				}
			: null
	);

	// Get current route from $page store
	let currentRoute = $derived($page.url.pathname);
	let isSoulRoute = $derived(
		currentRoute.startsWith('/') &&
			(currentRoute === '/' ||
				(!currentRoute.startsWith('/inventory') && !currentRoute.startsWith('/contacts')))
	);

	// Login state
	let showLoginPanel = $state(false);
	let isLoading = $state(false);
	let usernameInput = $state('');
	let password = $state('');
	let errorMessage = $state('');
	let authMessage = $state('');
	let showPassword = $state(false);

	// Click outside handler
	let headerRef = $state<HTMLElement | null>(null);

	// Check auth status and show login automatically
	onMount(() => {
		// Add click outside handler
		function handleClickOutside(event: MouseEvent) {
			if (headerRef && !headerRef.contains(event.target as Node) && showLoginPanel) {
				showLoginPanel = false;
			}
		}

		document.addEventListener('mousedown', handleClickOutside);

		// If not authenticated, automatically show login panel
		if (!(user && user.is && user.is.pub)) {
			// A small delay to ensure the component is fully mounted
			setTimeout(() => {
				showLoginPanel = true;
			}, 100);
		}

		return () => {
			document.removeEventListener('mousedown', handleClickOutside);
		};
	});

	// Toggle login panel
	function toggleLoginPanel() {
		showLoginPanel = !showLoginPanel;
		if (!showLoginPanel) {
			// Reset form when closing
			usernameInput = '';
			password = '';
			errorMessage = '';
			authMessage = '';
			showPassword = false;
		}
	}

	// Update handleBreadcrumbClick function to handle unauthenticated state
	function handleBreadcrumbClick(index: number, event: MouseEvent) {
		// If clicking on the root breadcrumb (index 0)
		if (index === 0) {
			// Prevent default navigation behavior
			event.preventDefault();

			// If user is not authenticated, show login panel regardless of route
			if (!pub) {
				showLoginPanel = true;
			}
			// If we're not in the soul route, navigate to the soul route
			else if (!isSoulRoute) {
				// First navigate to the root node in the tree
				globalState.navigateToPathIndex(0);
				// Then go to the soul route
				goto('/');
			}
			// If we're in the soul route and at the root breadcrumb
			else if (index === pathInfo.length - 1) {
				// Toggle login panel
				toggleLoginPanel();
			} else {
				// We're in soul route but not at root level, navigate to root
				globalState.navigateToPathIndex(0);
			}
		} else {
			// For non-root breadcrumbs, navigate as usual
			globalState.navigateToPathIndex(index);
		}
	}

	// Toggle password visibility
	function togglePasswordVisibility() {
		showPassword = !showPassword;
	}

	// Handle login
	function handleLogin(event: Event) {
		// Prevent default form submission
		event.preventDefault();

		if (!usernameInput || !password) {
			errorMessage = 'Please enter both username and password';
			return;
		}

		// Don't attempt to authenticate if we're already loading
		if (isLoading) {
			return;
		}

		// Clean the username input
		const cleanUsername = usernameInput.trim();

		errorMessage = '';
		authMessage = '';
		isLoading = true;

		try {
			// Try to login
			login(cleanUsername, password);

			// Reset form fields
			usernameInput = '';
			password = '';

			// Add a short delay to allow Gun to process the auth
			setTimeout(() => {
				isLoading = false;

				// If login failed (no pub), show error
				if (!pub) {
					errorMessage = 'Login failed. Would you like to create a new account?';
				} else {
					// Success, close the login panel
					showLoginPanel = false;
				}
			}, 500);
		} catch (error) {
			console.error('Authentication error:', error);
			errorMessage = error instanceof Error ? error.message : 'Authentication error';
			isLoading = false;
		}
	}

	// Handle signup
	function handleSignup() {
		if (!usernameInput || !password) {
			errorMessage = 'Please enter both username and password';
			return;
		}

		// Don't attempt to authenticate if we're already loading
		if (isLoading) {
			return;
		}

		// Clean the username input
		const cleanUsername = usernameInput.trim();

		errorMessage = '';
		authMessage = '';
		isLoading = true;

		try {
			// Use the signup function from user.ts
			signup(cleanUsername, password);

			// Add a short delay to allow Gun to process the signup
			setTimeout(() => {
				isLoading = false;

				// If signup succeeded, pub will be set
				if (pub) {
					// Success, close the login panel
					showLoginPanel = false;

					// Reset form fields
					usernameInput = '';
					password = '';
				} else {
					errorMessage = 'Signup failed. Please try a different username.';
				}
			}, 500);
		} catch (error) {
			console.error('Signup error:', error);
			errorMessage = error instanceof Error ? error.message : 'Signup error';
			isLoading = false;
		}
	}

	// Handle logout
	function handleLogout() {
		// Set loading state to prevent UI interactions during logout
		isLoading = true;

		try {
			// Call signout from user.ts
			signout();

			// Reset form fields
			usernameInput = '';
			password = '';
			errorMessage = '';
			authMessage = '';
			showPassword = false;

			// Show success message
			globalState.showToast('Logged out successfully', 'info');
		} catch (error) {
			console.error('Logout error:', error);
			globalState.showToast('Error during logout', 'error');
		} finally {
			// Small delay to ensure Gun has time to clean up
			setTimeout(() => {
				isLoading = false;
			}, 200);
		}
	}

	// Generate avatar
	function generateAvatar(pubKey: string, size = 32) {
		// Safety check for empty public key
		if (!pubKey) {
			// Return default avatar for empty pub key
			return `data:image/svg+xml,${encodeURIComponent(`
				<svg xmlns="http://www.w3.org/2000/svg" width="${size}" height="${size}" viewBox="0 0 ${size} ${size}">
					<rect width="100%" height="100%" fill="#6b7280" rx="${size / 4}" ry="${size / 4}" />
					<text x="50%" y="50%" font-family="Arial" font-size="${size / 2}px" fill="white" 
						text-anchor="middle" dominant-baseline="middle">
						?
					</text>
				</svg>
			`)}`;
		}

		try {
			// Generate a color based on public key
			const hue = Math.abs(pubKey.split('').reduce((a, b) => a + b.charCodeAt(0), 0) % 360);
			const saturation = 70;
			const lightness = 60;

			// Get first character for avatar text (safely)
			const firstChar = alias && alias.length > 0 ? alias.charAt(0).toUpperCase() : '?';

			// Generate avatar SVG
			return `data:image/svg+xml,${encodeURIComponent(`
				<svg xmlns="http://www.w3.org/2000/svg" width="${size}" height="${size}" viewBox="0 0 ${size} ${size}">
					<rect width="100%" height="100%" fill="hsl(${hue}, ${saturation}%, ${lightness}%)" rx="${size / 4}" ry="${size / 4}" />
					<text x="50%" y="50%" font-family="Arial" font-size="${size / 2}px" fill="white" 
						text-anchor="middle" dominant-baseline="middle">
						${firstChar}
					</text>
				</svg>
			`)}`;
		} catch (error) {
			// Handle any errors with a fallback avatar
			console.error('Error generating avatar:', error);
			return `data:image/svg+xml,${encodeURIComponent(`
				<svg xmlns="http://www.w3.org/2000/svg" width="${size}" height="${size}" viewBox="0 0 ${size} ${size}">
					<rect width="100%" height="100%" fill="#6b7280" rx="${size / 4}" ry="${size / 4}" />
					<text x="50%" y="50%" font-family="Arial" font-size="${size / 2}px" fill="white" 
						text-anchor="middle" dominant-baseline="middle">
						!
					</text>
				</svg>
			`)}`;
		}
	}

	// Check if current node has direct contribution children
	let hasDirectContributionChild = $derived(() => {
		if (!currentZipper) return false;

		// Check if any child has contributors
		for (const child of currentZipper.zipperCurrent.nodeChildren.values()) {
			// nodeContributors only exists on non-root nodes
			if (child.type === 'non-root' && child.nodeContributors?.size > 0) return true;
		}

		return false;
	});
</script>

<div class="header" bind:this={headerRef}>
	<!-- Main header row -->
	<div class="header-main">
		<div class="node-name">
			<div class="breadcrumbs">
				{#if pathInfo.length === 0}
					<div class="loading-path">Loading...</div>
				{:else}
					{#each pathInfo as segment, index}
						{#if index > 0}
							<div class="breadcrumb-separator">/</div>
						{/if}
						<a
							href={`/${segment.id}${segment.name ? ':' + segment.name.replace(/\s+/g, '-').toLowerCase() : ''}`}
							class="breadcrumb-item"
							class:current={index === pathInfo.length - 1}
							class:auth-root={index === 0}
							onclick={(e) => {
								handleBreadcrumbClick(index, e);
							}}
							tabindex="0"
							aria-label={segment.name}
						>
							{segment.name}
						</a>
					{/each}
				{/if}
			</div>
		</div>

		<div class="header-controls">
			<a href="/inventory" class="icon-button inventory-button" title="View inventory">
				<span>📊</span>
			</a>

			<a href="/contacts" class="icon-button contacts-button" title="View contacts">
				<span>👥</span>
			</a>

			<button
				class="icon-button add-button"
				title="Add new node"
				onclick={globalState.handleAddNode}><span>➕</span></button
			>
			<button
				class="icon-button delete-button"
				title="Toggle delete mode"
				onclick={globalState.toggleDeleteMode}><span>🗑️</span></button
			>
		</div>
	</div>

	<!-- Login panel (dropdown) -->
	{#if showLoginPanel}
		<div class="login-panel">
			{#if user && user.is && user.is.pub}
				<!-- Logged in view -->
				<div class="welcome-panel">
					<div class="user-profile">
						<div class="avatar large">
							<img src={generateAvatar(user.is.pub, 60)} alt={alias} />
						</div>
						<div class="user-details">
							<h3>Welcome, {alias}</h3>
							<p class="user-id">@{alias}</p>
						</div>
					</div>
					<div class="actions">
						<button class="logout-btn" onclick={handleLogout}>Log Out</button>
						<button class="close-btn" onclick={toggleLoginPanel}>Close</button>
					</div>
				</div>
			{:else}
				<!-- Login form -->
				<div class="login-form">
					<h3>Sign In / Sign Up</h3>
					<p class="form-info">Enter your details to sign in, or create a new account</p>

					{#if errorMessage}
						<div class="error-message">{errorMessage}</div>
					{/if}

					{#if authMessage}
						<div class="auth-message">{authMessage}</div>
					{/if}

					<form onsubmit={handleLogin}>
						<div class="form-group">
							<label for="username">Username</label>
							<input
								type="text"
								id="username"
								bind:value={usernameInput}
								placeholder="Enter username"
								disabled={isLoading}
								autocomplete="username"
								oninput={() =>
									(authMessage = usernameInput
										? 'Will attempt to sign in or create a new account if needed'
									: '')}
							/>
						</div>

						<div class="form-group password-group">
							<label for="password">Password</label>
							<div class="password-input-container">
								<input
									type={showPassword ? 'text' : 'password'}
									id="password"
									bind:value={password}
									placeholder="Enter password"
									disabled={isLoading}
									autocomplete="current-password"
								/>
								<button
									type="button"
									class="toggle-password"
									onclick={togglePasswordVisibility}
									title={showPassword ? 'Hide password' : 'Show password'}
								>
									<span>{showPassword ? '🙈' : '🐵'}</span>
								</button>
							</div>
						</div>

						<div class="actions">
							<button
								type="submit"
								class="login-btn"
								disabled={isLoading || !usernameInput || !password}
							>
								{#if isLoading}
									<div class="spinner small"></div>
									Signing in...
								{:else}
									Sign In
								{/if}
							</button>
							<button
								type="button"
								class="signup-btn"
								onclick={handleSignup}
								disabled={isLoading || !usernameInput || !password}
							>
								Sign Up
							</button>
							<button
								type="button"
								class="cancel-btn"
								onclick={toggleLoginPanel}
								disabled={isLoading}
							>
								Cancel
							</button>
						</div>
					</form>
				</div>
			{/if}
		</div>
	{/if}
</div>

<style>
	.header {
		width: 100%;
		display: flex;
		flex-direction: column;
		position: relative;
	}

	.header-main {
		display: flex;
		justify-content: space-between;
		align-items: center;
		width: 100%;
		padding: 8px 16px;
		background-color: white;
		z-index: 20;
	}

	.login-panel {
		position: absolute;
		top: 100%;
		left: 16px;
		width: 320px;
		background-color: white;
		box-shadow: 0 4px 12px rgba(0, 0, 0, 0.15);
		border-radius: 8px;
		margin-top: 8px;
		padding: 16px;
		z-index: 10;
		animation: dropDown 0.2s ease-out;
	}

	@keyframes dropDown {
		from {
			opacity: 0;
			transform: translateY(-10px);
		}
		to {
			opacity: 1;
			transform: translateY(0);
		}
	}

	/* Adding a pointer/arrow to the login panel */
	.login-panel::before {
		content: '';
		position: absolute;
		top: -8px;
		left: 24px;
		width: 16px;
		height: 16px;
		background-color: white;
		transform: rotate(45deg);
		box-shadow: -2px -2px 5px rgba(0, 0, 0, 0.05);
	}

	.node-name {
		display: flex;
		align-items: center;
		font-weight: bold;
		user-select: none;
		overflow: hidden;
		max-width: calc(100% - 120px);
	}

	/* Breadcrumb styles */
	.breadcrumbs {
		display: flex;
		align-items: center;
		flex-wrap: nowrap;
		overflow-x: auto;
		scrollbar-width: none; /* Firefox */
		-ms-overflow-style: none; /* IE and Edge */
		max-width: 100%;
		padding-bottom: 4px;
	}

	.breadcrumbs::-webkit-scrollbar {
		display: none; /* Chrome, Safari, Opera */
	}

	.breadcrumb-item {
		white-space: nowrap;
		padding: 3px 5px;
		border-radius: 4px;
		cursor: pointer;
		transition: background-color 0.2s;
		font-size: 1.95em;
		display: flex;
		align-items: center;
	}

	.breadcrumb-item:hover {
		background-color: rgba(0, 0, 0, 0.05);
	}

	.breadcrumb-item.current {
		font-weight: bold;
		color: #2196f3;
	}

	.breadcrumb-item.auth-root {
		display: flex;
		align-items: center;
		gap: 6px;
		cursor: pointer;
		transition: background-color 0.2s;
	}

	.avatar-inline {
		display: inline-flex;
		width: 24px;
		height: 24px;
		margin-left: 6px;
		vertical-align: middle;
	}

	.avatar-inline img {
		width: 100%;
		height: 100%;
		border-radius: 50%;
	}

	.breadcrumb-separator {
		margin: 0 5px;
		color: #888;
		font-size: 1.8em;
	}

	.loading-path {
		color: #888;
		font-style: italic;
		font-size: 1.95em;
	}

	.header-controls {
		display: flex;
		gap: 10px;
		align-items: center;
	}

	.icon-button {
		background: none;
		border: none;
		font-size: 20px;
		padding: 0;
		width: 30px;
		height: 30px;
		display: flex;
		align-items: center;
		justify-content: center;
		transition: transform 0.1s ease;
		cursor: pointer;
	}

	.icon-button:hover {
		transform: scale(1.1);
	}

	.login-form,
	.welcome-panel {
		display: flex;
		flex-direction: column;
	}

	.login-form h3,
	.welcome-panel h3 {
		margin-top: 0;
		margin-bottom: 14px;
		font-size: 1.2em;
		color: #333;
	}

	.form-info {
		text-align: center;
		color: #64748b;
		font-size: 0.9rem;
		margin-top: -0.5rem;
		margin-bottom: 1rem;
	}

	.form-group {
		margin-bottom: 12px;
	}

	.form-group label {
		display: block;
		margin-bottom: 4px;
		font-size: 0.9em;
		color: #555;
	}

	.form-group input {
		width: 100%;
		padding: 8px 10px;
		border: 1px solid #ddd;
		border-radius: 4px;
		font-size: 0.95em;
	}

	/* Add right padding to password field to make room for toggle button */
	.password-input-container input {
		padding-right: 38px;
	}

	.form-group input:focus {
		outline: none;
		border-color: #2196f3;
	}

	.error-message {
		background: #ffebee;
		color: #c62828;
		padding: 8px 10px;
		border-radius: 4px;
		margin-bottom: 12px;
		font-size: 0.9em;
		border-left: 3px solid #e57373;
	}

	.auth-message {
		background: #f0f9ff;
		color: #0369a1;
		padding: 8px 10px;
		border-radius: 4px;
		margin-bottom: 12px;
		font-size: 0.9em;
		border-left: 3px solid #0ea5e9;
	}

	.actions {
		display: flex;
		gap: 8px;
		margin-top: 16px;
		flex-wrap: wrap;
	}

	.login-btn,
	.logout-btn,
	.signup-btn {
		flex: 1;
		background: #2196f3;
		color: white;
		border: none;
		border-radius: 4px;
		padding: 8px 12px;
		font-size: 0.95em;
		cursor: pointer;
		display: flex;
		align-items: center;
		justify-content: center;
		gap: 8px;
		min-width: 90px;
	}

	.login-btn:hover,
	.logout-btn:hover,
	.signup-btn:hover {
		background: #1976d2;
	}

	.login-btn:disabled,
	.signup-btn:disabled {
		background: #bbdefb;
		cursor: not-allowed;
	}

	.logout-btn {
		background: #f44336;
	}

	.logout-btn:hover {
		background: #d32f2f;
	}

	.signup-btn {
		background: #4caf50;
	}

	.signup-btn:hover {
		background: #388e3c;
	}

	.cancel-btn,
	.close-btn {
		background: #f5f5f5;
		color: #333;
		border: none;
		border-radius: 4px;
		padding: 8px 12px;
		font-size: 0.95em;
		cursor: pointer;
	}

	.cancel-btn:hover,
	.close-btn:hover {
		background: #e0e0e0;
	}

	.cancel-btn:disabled {
		color: #999;
		cursor: not-allowed;
	}

	.avatar {
		width: 32px;
		height: 32px;
		border-radius: 50%;
		overflow: hidden;
		display: flex;
		align-items: center;
		justify-content: center;
	}

	.avatar.large {
		width: 60px;
		height: 60px;
	}

	.avatar img {
		width: 100%;
		height: 100%;
		object-fit: cover;
	}

	.user-profile {
		display: flex;
		align-items: center;
		gap: 16px;
		margin-bottom: 16px;
	}

	.user-details h3 {
		margin: 0 0 4px 0;
	}

	.user-id {
		margin: 0;
		color: #666;
		font-size: 0.9em;
	}

	.spinner {
		width: 24px;
		height: 24px;
		border: 2px solid rgba(255, 255, 255, 0.2);
		border-top-color: white;
		border-radius: 50%;
		animation: spin 0.8s linear infinite;
	}

	.spinner.small {
		width: 14px;
		height: 14px;
		border-width: 2px;
	}

	@keyframes spin {
		to {
			transform: rotate(360deg);
		}
	}

	/* Password field with toggle button */
	.password-group {
		position: relative;
	}

	.password-input-container {
		position: relative;
		display: flex;
		align-items: center;
	}

	.toggle-password {
		position: absolute;
		right: 8px;
		top: 50%;
		transform: translateY(-50%);
		background: none;
		border: none;
		cursor: pointer;
		padding: 0;
		display: flex;
		align-items: center;
		justify-content: center;
		font-size: 16px;
		color: #666;
		opacity: 0.7;
		transition: opacity 0.2s;
	}

	.toggle-password:hover {
		opacity: 1;
	}
</style>
