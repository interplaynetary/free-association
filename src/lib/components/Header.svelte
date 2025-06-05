<script lang="ts">
	import { globalState, currentPath } from '$lib/global.svelte';
	import { onMount } from 'svelte';
	import { page } from '$app/stores';
	import { goto } from '$app/navigation';
	import { get } from 'svelte/store';
	import {
		username,
		userpub,
		login,
		signup,
		signout,
		userTree,
		isAuthenticating
	} from '$lib/state.svelte';
	import { findNodeById, addChild, createNonRootNode } from '$lib/protocol';
	import { type Node as TreeNode, type RootNode } from '$lib/schema';
	import { gunAvatar } from 'gun-avatar';

	// Define type for path info
	type PathInfo = Array<{ id: string; name: string }>;

	// For tracking tree updates and forcing rerenders
	let updateCounter = $state(0);

	// Function to manually trigger a UI update when tree changes
	function triggerUpdate() {
		updateCounter++;
		console.log('[UI FLOW] Header triggered update', updateCounter);
	}

	// Reactive store subscriptions
	const tree = $derived($userTree);
	const path = $derived($currentPath);
	const pub = $derived($userpub);
	const user = $derived($username);
	const isAuthenticatingState = $derived($isAuthenticating);

	// Derived path info based on current path and tree
	const currentPathInfo = $derived.by(() => {
		// Force update when counter changes
		updateCounter;

		if (!tree || path.length === 0) return [];

		// Map each path ID to a name, finding the name in the tree
		return path.map((id) => {
			const node = findNodeById(tree, id);
			return {
				id,
				name: node ? node.name : 'Unknown'
			};
		});
	});

	// Subscribe to userTree changes to ensure UI updates
	onMount(() => {
		const unsubscribe = userTree.subscribe((value) => {
			if (value) {
				console.log('[UI FLOW] Header userTree updated, triggering UI update');
				triggerUpdate();
			}
		});

		// Also subscribe to currentPath changes
		const unsubscribePath = currentPath.subscribe((value) => {
			console.log('[UI FLOW] Header path updated:', value);
			triggerUpdate();
		});

		return () => {
			unsubscribe();
			unsubscribePath();
		};
	});

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
	let confirmPassword = $state(''); // Added for registration
	let isRegisterMode = $state(false); // Toggle between login and register
	let errorMessage = $state('');
	let authMessage = $state('');
	let showPassword = $state(false);
	let agreedToTerms = $state(false); // Added for terms agreement

	// Initialize error state with URL error message if present
	$effect(() => {
		const urlError = $page.url.searchParams.get('error');
		if (urlError) {
			errorMessage = 'Login failed. Please check your credentials.';
			showLoginPanel = true;
		}
	});

	// Click outside handler
	let headerRef = $state<HTMLElement | null>(null);
	let loginPanelRef = $state<HTMLElement | null>(null);

	// Check auth status and show login automatically
	onMount(() => {
		// Add click outside handler
		function handleClickOutside(event: MouseEvent) {
			if (
				showLoginPanel &&
				loginPanelRef &&
				!loginPanelRef.contains(event.target as EventTarget & HTMLElement)
			) {
				// Check if the click is on any header control buttons (inventory, add, delete)
				const target = event.target as HTMLElement;
				const isHeaderControlButton = target.closest('.header-controls');

				// Don't close the login panel if clicking on header control buttons
				if (!isHeaderControlButton) {
					showLoginPanel = false;
				}
			}
		}

		document.addEventListener('mousedown', handleClickOutside);

		// If not authenticated, automatically show login panel
		if (!user) {
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
			resetForm();
		}
	}

	// Reset form state
	function resetForm() {
		usernameInput = '';
		password = '';
		confirmPassword = '';
		errorMessage = '';
		authMessage = '';
		showPassword = false;
		isRegisterMode = false;
		agreedToTerms = false;
	}

	// Add new node handler
	function handleAddNode() {
		if (!tree || path.length === 0) return;

		// Get current node ID (last in path)
		const currentNodeId = path[path.length - 1];

		// Create a deep clone of the tree to ensure reactivity
		const updatedTree = structuredClone(tree);

		// Find the current node in the cloned tree
		const currentNode = findNodeById(updatedTree, currentNodeId);
		if (!currentNode) {
			globalState.showToast('Error creating node: Current node not found', 'error');
			return;
		}

		// Create a unique ID for the new node
		const newNodeId = `node_${Date.now()}_${Math.random().toString(36).substring(2, 9)}`;
		const newNodeName = 'New Node';
		const newPoints = 10;

		try {
			// Add child to the current node in the cloned tree
			addChild(currentNode, newNodeId, newNodeName, newPoints);

			// Update the store with the new tree to trigger reactivity
			userTree.set(updatedTree);

			// Force update
			triggerUpdate();

			// Show success message
			globalState.showToast('New node created', 'success');

			// Set node to edit mode
			setTimeout(() => globalState.setNodeToEditMode(newNodeId), 50);
		} catch (err) {
			console.error('Error in handleAddNode:', err);
			globalState.showToast('Error creating node', 'error');
		}
	}

	// Simplified breadcrumb click handler
	function handleBreadcrumbClick(index: number, event: MouseEvent) {
		// Prevent default navigation behavior
		event.preventDefault();

		// If user is not authenticated, show login panel
		if (!user) {
			showLoginPanel = true;
			return;
		}

		// If we're already at the root (only one breadcrumb item) and clicking the first item,
		// and we're on the soul route, toggle the login panel instead of navigating
		if (index === 0 && currentPathInfo.length === 1 && isSoulRoute) {
			showLoginPanel = !showLoginPanel;
			return;
		}

		// Simply update the path using the globalState function
		globalState.navigateToPathIndex(index);

		// If we're not in the soul route, navigate to the soul route
		if (!isSoulRoute) {
			goto('/');
		}
	}

	// Handler for clicking the "Login" text when not authenticated
	function handleLoginClick(event: MouseEvent) {
		event.preventDefault();
		showLoginPanel = true;
	}

	// Toggle password visibility
	function togglePasswordVisibility() {
		showPassword = !showPassword;
	}

	// Toggle between login and register mode
	function toggleAuthMode() {
		isRegisterMode = !isRegisterMode;
		errorMessage = '';
		authMessage = '';
		agreedToTerms = false;
	}

	// Validate the form data
	function validateForm() {
		if (!usernameInput || !password) {
			errorMessage = 'Please enter both username and password';
			return false;
		}

		if (isRegisterMode) {
			if (!agreedToTerms) {
				errorMessage = 'Please agree to the Privacy Policy and Terms of Use';
				return false;
			}

			if (password !== confirmPassword) {
				errorMessage = 'Passwords do not match';
				return false;
			}

			if (password.length < 8) {
				errorMessage = 'Password must be at least 8 characters';
				return false;
			}
		}

		return true;
	}

	// Handle form submission (login or register)
	async function handleSubmit(event: Event) {
		// Prevent default form submission
		event.preventDefault();

		// Validate form inputs
		if (!validateForm()) {
			return;
		}

		// Don't attempt to authenticate if we're already loading
		if (isLoading) {
			return;
		}

		errorMessage = '';
		authMessage = '';
		isLoading = true;

		try {
			if (isRegisterMode) {
				await handleSignup();
			} else {
				await handleLogin();
			}
		} catch (error) {
			console.error('Authentication error:', error);
			errorMessage = error instanceof Error ? error.message : 'Authentication error';
		} finally {
			isLoading = false;
		}
	}

	// Handle login with Gun
	async function handleLogin() {
		try {
			login(usernameInput.trim(), password);
			// Success handling is done via Gun's 'auth' event in gunSetup
			showLoginPanel = false;
			resetForm();
			globalState.showToast('Signed in successfully', 'success');
		} catch (error) {
			console.error('Authentication error:', error);
			errorMessage = error instanceof Error ? error.message : 'Authentication error';
		}
	}

	// Handle signup/register with Gun
	async function handleSignup() {
		try {
			signup(usernameInput.trim(), password);
			// Success handling is done via Gun's 'auth' event in gunSetup
			showLoginPanel = false;
			resetForm();
			globalState.showToast('Account created successfully!', 'success');
		} catch (error) {
			console.error('Registration error:', error);
			errorMessage = error instanceof Error ? error.message : 'Registration error';
		}
	}

	// Handle logout with Gun
	async function handleLogout() {
		try {
			await signout();
			globalState.resetState();
			globalState.showToast('Signed out successfully', 'info');
			goto('/');
		} catch (error) {
			console.error('Logout error:', error);
			globalState.showToast('Error during sign out', 'error');
		}
	}

	// Generate avatar using gun-avatar or fallback
	function generateAvatar(
		name: string | null | undefined,
		userPub: string | null | undefined = null,
		size = 32
	) {
		// Use gun-avatar if we have a valid public key
		if (userPub && userPub.length >= 87) {
			try {
				return gunAvatar({
					pub: userPub,
					size,
					round: true,
					draw: 'circles'
				});
			} catch (error) {
				console.warn('Failed to generate gun-avatar:', error);
			}
		}

		// Fallback avatar generation
		// Safety check for empty name
		if (!name) {
			// Return default avatar
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
			// Generate a color based on name
			const hue = Math.abs(name.split('').reduce((a, b) => a + b.charCodeAt(0), 0) % 360);
			const saturation = 70;
			const lightness = 60;

			// Get first character for avatar text
			const firstChar = name.charAt(0).toUpperCase();

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

	// Add this function before the closing script tag
	async function handleDownloadTree() {
		if (!tree) {
			globalState.showToast('No tree data available to download', 'error');
			return;
		}

		try {
			// Create the JSON data
			const jsonData = JSON.stringify(tree, null, 2);
			const blob = new Blob([jsonData], { type: 'application/json' });

			// Show the file picker
			const handle = await window.showSaveFilePicker({
				suggestedName: `userTree_${user || 'anonymous'}_${new Date().toISOString().split('T')[0]}.json`,
				types: [
					{
						description: 'JSON File',
						accept: { 'application/json': ['.json'] }
					}
				]
			});

			// Write the file
			const writable = await handle.createWritable();
			await writable.write(blob);
			await writable.close();

			globalState.showToast('Tree data downloaded successfully', 'success');
		} catch (error: unknown) {
			// Don't show error for user cancellation
			if (error instanceof Error && error.name !== 'AbortError') {
				console.error('Error downloading tree:', error);
				globalState.showToast('Error downloading tree data', 'error');
			}
		}
	}
</script>

<div class="header" bind:this={headerRef}>
	<!-- Main header row -->
	<div class="header-main">
		<div class="node-name">
			<div class="breadcrumbs">
				{#if isAuthenticatingState}
					<div class="breadcrumb-item loading-path">Loading...</div>
				{:else if currentPathInfo.length === 0 && $username}
					<a
						href="/"
						class="breadcrumb-item auth-root current"
						onclick={(e) => handleBreadcrumbClick(0, e)}
						tabindex="0"
						aria-label={$username}
					>
						{$username}
					</a>
				{:else if currentPathInfo.length === 0}
					<button
						class="breadcrumb-item loading-path"
						onclick={handleLoginClick}
						tabindex="0"
						aria-label="Show login panel"
					>
						Login
					</button>
				{:else}
					{#each currentPathInfo as segment, index}
						{#if index > 0}
							<div class="breadcrumb-separator">/</div>
						{/if}
						<a
							href="/"
							class="breadcrumb-item"
							class:current={index === currentPathInfo.length - 1}
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

			<button class="icon-button add-button" title="Add new node" onclick={handleAddNode}
				><span>➕</span></button
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
		<div class="login-panel" bind:this={loginPanelRef}>
			{#if isAuthenticatingState}
				<div class="loading-state">
					<div class="spinner"></div>
					<p>Checking authentication...</p>
				</div>
			{:else if $username}
				<!-- Logged in view -->
				<div class="welcome-panel">
					<div class="user-profile">
						<div class="avatar large">
							<img src={generateAvatar($username, $userpub, 60)} alt={$username} />
						</div>
						<div class="user-details">
							<h3>Welcome, {$username}</h3>
							<p class="user-id">@{$username || 'anonymous'}</p>
						</div>
					</div>
					<div class="actions">
						<button class="download-btn" onclick={handleDownloadTree}>
							<span>📥</span>
						</button>
						<button class="logout-btn" onclick={handleLogout}>Log Out</button>
						<button class="close-btn" onclick={toggleLoginPanel}>Close</button>
					</div>
				</div>
			{:else}
				<!-- Login/Register form -->
				<div class="login-form">
					<h3>{isRegisterMode ? 'Create Account' : 'Sign In'}</h3>
					<p class="form-info">
						{isRegisterMode
							? 'Enter your details to create a new account'
							: 'Enter your details to sign in'}
					</p>

					{#if errorMessage}
						<div class="error-message">{errorMessage}</div>
					{/if}

					{#if authMessage}
						<div class="auth-message">{authMessage}</div>
					{/if}

					<form onsubmit={handleSubmit}>
						<div class="form-group">
							<label for="username">Username</label>
							<input
								type="text"
								id="username"
								bind:value={usernameInput}
								placeholder="Enter username"
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
									placeholder="Enter password"
									disabled={isLoading}
									autocomplete={isRegisterMode ? 'new-password' : 'current-password'}
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

						{#if isRegisterMode}
							<div class="form-group password-group">
								<label for="confirmPassword">Confirm Password</label>
								<div class="password-input-container">
									<input
										type={showPassword ? 'text' : 'password'}
										id="confirmPassword"
										bind:value={confirmPassword}
										placeholder="Confirm password"
										disabled={isLoading}
										autocomplete="new-password"
									/>
								</div>
							</div>

							<div class="form-group checkbox-group">
								<label class="checkbox-label">
									<input
										type="checkbox"
										id="agreedToTerms"
										bind:checked={agreedToTerms}
										disabled={isLoading}
									/>
									<span class="checkbox-text">
										I agree to the <a href="/privacy" target="_blank" class="terms-link"
											>Privacy Policy</a
										>
										and <a href="/terms" target="_blank" class="terms-link">Terms of Use</a>
									</span>
								</label>
							</div>
						{/if}

						<div class="actions">
							<button
								type="submit"
								class={isRegisterMode ? 'signup-btn' : 'login-btn'}
								disabled={isLoading ||
									!usernameInput ||
									!password ||
									(isRegisterMode && (!confirmPassword || !agreedToTerms))}
							>
								{#if isLoading}
									<div class="spinner small"></div>
									{isRegisterMode ? 'Creating account...' : 'Signing in...'}
								{:else}
									{isRegisterMode ? 'Create Account' : 'Sign In'}
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

						<div class="toggle-auth-mode">
							{#if isRegisterMode}
								Already have an account?
								<button type="button" class="text-link" onclick={toggleAuthMode}>
									Sign in instead
								</button>
							{:else}
								Don't have an account?
								<button type="button" class="text-link" onclick={toggleAuthMode}>
									Create one now
								</button>
							{/if}
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
		flex: 1;
	}

	.header-main {
		display: flex;
		justify-content: space-between;
		align-items: center;
		width: 100%;
		padding: 8px 16px;
		background-color: white;
		z-index: 20;
		min-height: 52px;
	}

	.login-panel {
		position: absolute;
		top: calc(100% - 8px);
		left: 16px;
		width: 320px;
		background-color: white;
		box-shadow: 0 4px 12px rgba(0, 0, 0, 0.15);
		border-radius: 8px;
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
		background: none;
		border: none;
		cursor: pointer;
		padding: 3px 5px;
		border-radius: 4px;
		transition: background-color 0.2s;
	}

	.loading-path:hover {
		background-color: rgba(0, 0, 0, 0.05);
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
		margin-bottom: 16px;
	}

	.toggle-auth-mode {
		text-align: center;
		margin-top: 8px;
		font-size: 0.9em;
		color: #666;
	}

	.text-link {
		background: none;
		border: none;
		color: #2196f3;
		padding: 0;
		font-size: 1em;
		cursor: pointer;
		text-decoration: underline;
	}

	.text-link:hover {
		color: #1976d2;
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

	.loading-state {
		display: flex;
		flex-direction: column;
		align-items: center;
		justify-content: center;
		padding: 20px;
		text-align: center;
	}

	.loading-state p {
		margin-top: 12px;
		color: #666;
	}

	.download-btn {
		background: #4caf50;
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
		flex: 1;
	}

	.download-btn:hover {
		background: #388e3c;
	}

	.download-btn span {
		font-size: 1.1em;
	}

	/* Checkbox styles */
	.checkbox-group {
		margin-bottom: 16px;
	}

	.checkbox-label {
		display: flex;
		align-items: flex-start;
		gap: 8px;
		cursor: pointer;
		font-size: 0.9em;
		line-height: 1.4;
	}

	.checkbox-label input[type='checkbox'] {
		width: auto;
		margin: 0;
		margin-top: 2px;
		flex-shrink: 0;
	}

	.checkbox-text {
		color: #555;
	}

	.terms-link {
		color: #2196f3;
		text-decoration: none;
	}

	.terms-link:hover {
		text-decoration: underline;
	}
</style>
