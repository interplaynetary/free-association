<script lang="ts">
	import { globalState, type UserData } from '$lib/global.svelte';
	import { enterChild, exitToParent } from '$lib/centralized';
	import { onMount } from 'svelte';

	// Get current zipper, path and pathInfo from global state
	let currentZipper = $derived(globalState.currentZipper);
	let currentPath = $derived(globalState.currentPath);
	let pathInfo = $derived(globalState.pathInfo);
	let currentUser = $derived(globalState.currentUser);

	// Login state
	let showLoginPanel = $state(false);
	let isLoading = $state(false);
	let username = $state('');
	let password = $state('');
	let errorMessage = $state('');
	let showPassword = $state(false);

	// Click outside handler
	let headerRef = $state<HTMLElement | null>(null);

	// Add document click listener to close dropdown when clicking outside
	onMount(() => {
		function handleClickOutside(event: MouseEvent) {
			if (headerRef && !headerRef.contains(event.target as Node) && showLoginPanel) {
				showLoginPanel = false;
			}
		}

		document.addEventListener('mousedown', handleClickOutside);

		return () => {
			document.removeEventListener('mousedown', handleClickOutside);
		};
	});

	// Mock user data
	const mockUsers = {
		alice: { password: 'password123', alias: 'Alice', pub: 'ALICE123' },
		bob: { password: 'password123', alias: 'Bob', pub: 'BOB456' },
		charlie: { password: 'password123', alias: 'Charlie', pub: 'CHARLIE789' }
	};

	// Toggle login panel
	function toggleLoginPanel() {
		showLoginPanel = !showLoginPanel;
		if (!showLoginPanel) {
			// Reset form when closing
			username = '';
			password = '';
			errorMessage = '';
			showPassword = false;
		}
	}

	// Toggle password visibility
	function togglePasswordVisibility() {
		showPassword = !showPassword;
	}

	// Handle login
	async function handleLogin(event: Event) {
		// Prevent default form submission
		event.preventDefault();

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

				// Store user in localStorage
				localStorage.setItem('centralizedUser', JSON.stringify(userData));

				// Update global state with the logged in user
				globalState.setCurrentUser(userData);

				// Reset form fields but keep panel open
				username = '';
				password = '';
				errorMessage = '';
				showPassword = false;

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
		globalState.setCurrentUser(null);
		globalState.showToast('Logged out successfully', 'info');
		// Keep panel open and reset form fields
		username = '';
		password = '';
		errorMessage = '';
		showPassword = false;
	}

	// Generate avatar
	function generateAvatar(pub: string, size = 32) {
		// Generate a color based on public key
		const hue = Math.abs(pub.split('').reduce((a, b) => a + b.charCodeAt(0), 0) % 360);
		const saturation = 70;
		const lightness = 60;

		// Generate avatar SVG
		return `data:image/svg+xml,${encodeURIComponent(`
			<svg xmlns="http://www.w3.org/2000/svg" width="${size}" height="${size}" viewBox="0 0 ${size} ${size}">
				<rect width="100%" height="100%" fill="hsl(${hue}, ${saturation}%, ${lightness}%)" rx="${size / 4}" ry="${size / 4}" />
				<text x="50%" y="50%" font-family="Arial" font-size="${size / 2}px" fill="white" 
					text-anchor="middle" dominant-baseline="middle">
					${currentUser?.alias.charAt(0).toUpperCase()}
				</text>
			</svg>
		`)}`;
	}

	// Check if current node has direct contribution children
	let hasDirectContributionChild = $derived(() => {
		if (!currentZipper) return false;

		// Check if any child has contributors
		for (const child of currentZipper.zipperCurrent.nodeChildren.values()) {
			if (child.nodeContributors.size > 0) return true;
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
							onclick={(e) => {
								globalState.navigateToPathIndex(index);
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
			<a href="/inventory" class="icon-button inventory-button" title="View inventory"
				><span>📊</span></a
			>

			{#if currentUser}
				<!-- Show user avatar and name when logged in -->
				<div class="user-info" onclick={toggleLoginPanel}>
					<div class="avatar">
						<img src={generateAvatar(currentUser.pub)} alt={currentUser.alias} />
					</div>
					<span class="user-name">{currentUser.alias}</span>
				</div>
			{:else}
				<!-- Show login button when not logged in -->
				<button class="icon-button login-button" title="Login" onclick={toggleLoginPanel}>
					<span>🔑</span>
				</button>
			{/if}

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
			{#if currentUser}
				<!-- Logged in view -->
				<div class="welcome-panel">
					<div class="user-profile">
						<div class="avatar large">
							<img src={generateAvatar(currentUser.pub, 60)} alt={currentUser.alias} />
						</div>
						<div class="user-details">
							<h3>Welcome, {currentUser.alias}</h3>
							<p class="user-id">@{currentUser.username}</p>
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
					<h3>Sign In</h3>

					{#if errorMessage}
						<div class="error-message">{errorMessage}</div>
					{/if}

					<form onsubmit={handleLogin}>
						<div class="form-group">
							<label for="username">Username</label>
							<input
								type="text"
								id="username"
								bind:value={username}
								placeholder="alice, bob, or charlie"
								disabled={isLoading}
								autocomplete="username"
							/>
						</div>

						<div class="form-group password-group">
							<label for="password">Password</label>
							<div class="password-input-container">
								<input
									type={showPassword ? 'text' : 'password'}
									id="password"
									bind:value={password}
									placeholder="password123"
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
								disabled={isLoading || !username || !password}
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
								class="cancel-btn"
								onclick={toggleLoginPanel}
								disabled={isLoading}
							>
								Cancel
							</button>
						</div>

						<div class="hint">Try: alice/bob/charlie with password: password123</div>
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
		right: 16px;
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
		right: 24px;
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
		max-width: calc(100% - 180px);
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
	}

	.breadcrumb-item:hover {
		background-color: rgba(255, 255, 255, 0.2);
	}

	.breadcrumb-item.current {
		font-weight: bold;
		color: #2196f3;
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

	.actions {
		display: flex;
		gap: 8px;
		margin-top: 16px;
	}

	.login-btn,
	.logout-btn {
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
	}

	.login-btn:hover,
	.logout-btn:hover {
		background: #1976d2;
	}

	.login-btn:disabled {
		background: #bbdefb;
		cursor: not-allowed;
	}

	.logout-btn {
		background: #f44336;
	}

	.logout-btn:hover {
		background: #d32f2f;
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

	.user-info {
		display: flex;
		align-items: center;
		gap: 6px;
		cursor: pointer;
		padding: 4px 8px;
		border-radius: 16px;
		transition: background-color 0.2s;
	}

	.user-info:hover {
		background-color: rgba(0, 0, 0, 0.05);
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

	.user-name {
		font-size: 0.9em;
		font-weight: 500;
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

	.hint {
		margin-top: 12px;
		font-size: 0.8em;
		color: #666;
		text-align: center;
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
