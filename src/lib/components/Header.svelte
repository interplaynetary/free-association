<script lang="ts">
	import { globalState, currentPath } from '$lib/global.svelte';
	import { onMount } from 'svelte';
	import { page } from '$app/stores';
	import { goto } from '$app/navigation';
	import { get } from 'svelte/store';
	import { base } from '$app/paths';
	import {
		userAlias,
		userPub,
		login,
		signup,
		signout,
		isAuthenticating,
		changePassword
	} from '$lib/state/gun.svelte';
	import { userTree } from '$lib/state/core.svelte';
	import { findNodeById } from '$lib/protocol';
	import { searchTreeForNavigation } from '$lib/utils/treeSearch';
	import { type Node, type RootNode } from '$lib/schema';
	import { gunAvatar } from 'gun-avatar';
	import { startTour } from '$lib/utils/tour';
	import { browser } from '$app/environment';

	// Helper function to truncate text
	function truncateText(text: string, maxLength: number = 14): string {
		if (text.length <= maxLength) return text;
		return text.slice(0, maxLength - 3) + '...';
	}

	// Helper function to copy text to clipboard
	async function copyToClipboard(text: string) {
		try {
			await navigator.clipboard.writeText(text);
			globalState.showToast('Public key copied to clipboard', 'success');
		} catch (err) {
			console.error('Failed to copy text: ', err);
			globalState.showToast('Failed to copy public key', 'error');
		}
	}

	// Function to start the guided tour
	function handleTourClick() {
		if (!browser) return;

		try {
			startTour();
		} catch (error) {
			console.error('Failed to start tour:', error);
			globalState.showToast('Failed to start tour', 'error');
		}
	}

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
	const pub = $derived($userPub);
	const user = $derived($userAlias);
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

	// Effect to scroll breadcrumbs when path changes
	$effect(() => {
		// React to currentPathInfo changes
		currentPathInfo;
		// Scroll to end when path changes
		scrollBreadcrumbsToEnd();
	});

	// Subscribe to userTree changes to ensure UI updates
	onMount(() => {
		const unsubscribe = userTree.subscribe((value) => {
			if (value) {
				console.log('[UI FLOW] Header userTree updated, triggering UI update');
				triggerUpdate();
				scrollBreadcrumbsToEnd();
			}
		});

		// Also subscribe to currentPath changes
		const unsubscribePath = currentPath.subscribe((value) => {
			console.log('[UI FLOW] Header path updated:', value);
			triggerUpdate();
			scrollBreadcrumbsToEnd();
		});

		// Add mouse wheel scrolling to breadcrumbs
		const handleBreadcrumbScroll = (event: WheelEvent) => {
			const target = event.target;
			if (breadcrumbsRef && target && breadcrumbsRef.contains(target as HTMLElement)) {
				event.preventDefault();
				// Smooth scrolling for two-finger mousepad gestures
				const scrollAmount = event.deltaY * 0.5; // Reduce sensitivity for smoother scrolling
				breadcrumbsRef.scrollLeft += scrollAmount;
			}
		};

		document.addEventListener('wheel', handleBreadcrumbScroll, { passive: false });

		return () => {
			unsubscribe();
			unsubscribePath();
			document.removeEventListener('wheel', handleBreadcrumbScroll);
		};
	});

	// Get current route from $page store
	let currentRoute = $derived($page.url.pathname);
	// Remove base path to get the actual route
	let routeWithoutBase = $derived(
		currentRoute.startsWith(base) ? currentRoute.slice(base.length) : currentRoute
	);
	let isSoulRoute = $derived(
		routeWithoutBase === '/' ||
			routeWithoutBase === '' ||
			(!routeWithoutBase.startsWith('/inventory') && !routeWithoutBase.startsWith('/contacts'))
	);

	// Login state
	let showLoginPanel = $state(false);
	let isLoading = $state(false);
	// Global state for form values that persist when panel is closed/reopened
	let usernameInput = $state('');
	let password = $state('');
	let confirmPassword = $state(''); // Added for registration
	let isRegisterMode = $state(false); // Toggle between login and register
	let errorMessage = $state('');
	let authMessage = $state('');
	let showPassword = $state(false);
	let showConfirmPassword = $state(false); // For registration confirm password
	let agreedToTerms = $state(false); // Added for terms agreement
	let loginPanelTimer = $state<number | null>(null); // Timer for auto-closing login panel
	let isPanelClosing = $state(false); // State for smooth fade-out transition
	let shouldPreserveFormValues = $state(true); // Flag to control whether form values should be preserved

	// Notification state
	let showNotificationPanel = $state(false);

	// Search state
	let showSearchPanel = $state(false);
	let searchQuery = $state('');
	// Derived search results
	const searchResults = $derived(
		searchQuery.trim() && tree ? searchTreeForNavigation(tree, searchQuery) : []
	);

	// Mutable state for selected index
	let selectedResultIndex = $state(-1);

	// Reactive update of selected index when search results change
	$effect(() => {
		selectedResultIndex = searchResults.length > 0 ? 0 : -1;
	});

	// Password change state
	let showPasswordChange = $state(false);
	let currentPassword = $state('');
	let newPassword = $state('');
	let confirmNewPassword = $state('');
	let passwordChangeError = $state('');
	let isChangingPassword = $state(false);
	let showCurrentPassword = $state(false); // For password change current password
	let showNewPassword = $state(false); // For password change new password
	let showConfirmNewPassword = $state(false); // For password change confirm new password

	// Add a flag to prevent recursive navigation
	let isPreviewingSearchResult = $state(false);
	let recursionCounter = $state(0);

	// Initialize error state with URL error message if present
	$effect(() => {
		const urlError = $page.url.searchParams.get('error');
		if (urlError) {
			errorMessage = 'Login failed. Please check your credentials.';
			showLoginPanel = true;
			startLoginPanelTimer();
		}
	});

	// Click outside handler
	let headerRef = $state<HTMLElement | null>(null);
	let loginPanelRef = $state<HTMLElement | null>(null);
	let notificationPanelRef = $state<HTMLElement | null>(null);
	let searchPanelRef = $state<HTMLElement | null>(null);
	let searchInputRef = $state<HTMLInputElement | null>(null);
	let breadcrumbsRef = $state<HTMLElement | null>(null);

	// Check auth status and show login automatically
	onMount(() => {
		// Add click/touch outside handler
		function handleClickOutside(event: MouseEvent | TouchEvent) {
			const target = event.target as HTMLElement;

			if (showLoginPanel && loginPanelRef && !loginPanelRef.contains(target)) {
				// Check if the click is on any header control buttons (inventory, add, delete)
				const isHeaderControlButton = target.closest('.header-controls');

				// Don't close the login panel if clicking on header control buttons
				if (!isHeaderControlButton) {
					showLoginPanel = false;
					// Form values are preserved when panel is closed by clicking outside
					// because shouldPreserveFormValues is true by default
				}
			}

			if (showNotificationPanel && notificationPanelRef && !notificationPanelRef.contains(target)) {
				// Check if the click is on any header control buttons
				const isHeaderControlButton = target.closest('.header-controls');

				// Don't close the notification panel if clicking on header control buttons
				if (!isHeaderControlButton) {
					showNotificationPanel = false;
				}
			}

			if (showSearchPanel && searchPanelRef && !searchPanelRef.contains(target)) {
				// Check if the click is on any header control buttons
				const isHeaderControlButton = target.closest('.header-controls');

				// Don't close the search panel if clicking on header control buttons
				if (!isHeaderControlButton) {
					showSearchPanel = false;
					searchQuery = '';
					selectedResultIndex = -1;
				}
			}
		}

		document.addEventListener('mousedown', handleClickOutside);
		document.addEventListener('touchstart', handleClickOutside);

		// If not authenticated, automatically show login panel
		if (!user) {
			// A small delay to ensure the component is fully mounted
			setTimeout(() => {
				showLoginPanel = true;
				startLoginPanelTimer();
			}, 100);
		}

		return () => {
			document.removeEventListener('mousedown', handleClickOutside);
			document.removeEventListener('touchstart', handleClickOutside);
			clearLoginPanelTimer();
		};
	});

	// Toggle login panel
	function toggleLoginPanel() {
		showLoginPanel = !showLoginPanel;
		if (!showLoginPanel) {
			// Keep form values when panel is closed accidentally
			resetForm();
			clearLoginPanelTimer();
			isPanelClosing = false;
		} else {
			startLoginPanelTimer();
		}
	}

	// Toggle notification panel
	function toggleNotificationPanel() {
		showNotificationPanel = !showNotificationPanel;
	}

	// Function to scroll breadcrumbs to show the rightmost (current) item
	function scrollBreadcrumbsToEnd() {
		if (breadcrumbsRef) {
			// Small delay to ensure DOM is updated
			setTimeout(() => {
				if (breadcrumbsRef) {
					breadcrumbsRef.scrollLeft = breadcrumbsRef.scrollWidth;
				}
			}, 50);
		}
	}

	// Reset form state
	function resetForm() {
		// Only clear form values if we don't want to preserve them
		if (!shouldPreserveFormValues) {
			usernameInput = '';
			password = '';
			confirmPassword = '';
			isRegisterMode = false;
			agreedToTerms = false;
		}

		// Always reset these UI state values
		errorMessage = '';
		authMessage = '';
		showPassword = false;
		showConfirmPassword = false;

		// Also reset password change form when login panel closes
		showPasswordChange = false;
		resetPasswordChangeForm();
	}

	// Start timer to auto-close login panel
	function startLoginPanelTimer() {
		clearLoginPanelTimer();
		// Cast to any to avoid type conflicts between browser and Node.js timer types
		loginPanelTimer = setTimeout(() => {
			if (showLoginPanel && !isLoading) {
				// Start fade-out transition
				isPanelClosing = true;
				// Wait for transition to complete before hiding
				setTimeout(() => {
					showLoginPanel = false;
					isPanelClosing = false;
					// Preserve form values when auto-closing
					resetForm();
				}, 300); // Match the CSS transition duration
			}
		}, 10000) as any; // 10 seconds (10,000 ms)
	}

	// Clear the login panel timer
	function clearLoginPanelTimer() {
		if (loginPanelTimer) {
			clearTimeout(loginPanelTimer);
			loginPanelTimer = null;
		}
		// Reset closing state if timer is cleared
		isPanelClosing = false;
	}

	// Simplified breadcrumb click handler
	function handleBreadcrumbClick(index: number, event: MouseEvent) {
		// Prevent default navigation behavior
		event.preventDefault();

		// If user is not authenticated, show login panel
		if (!user) {
			showLoginPanel = true;
			startLoginPanelTimer();
			return;
		}

		// If we're already at the root (no path items) and clicking the first item,
		// and we're on the soul route, toggle the login panel instead of navigating
		if (index === 0 && currentPathInfo.length === 0 && isSoulRoute) {
			showLoginPanel = !showLoginPanel;
			if (showLoginPanel) {
				startLoginPanelTimer();
			} else {
				clearLoginPanelTimer();
				isPanelClosing = false;
			}
			return;
		}

		// Simply update the path using the globalState function
		globalState.navigateToPathIndex(index);

		// If we're not in the soul route, navigate to the soul route
		if (!isSoulRoute) {
			goto(base + '/');
		}
	}

	// Handler for clicking the "Login" text when not authenticated
	function handleLoginClick(event: MouseEvent) {
		event.preventDefault();
		showLoginPanel = true;
		startLoginPanelTimer();
	}

	// Toggle password visibility
	function togglePasswordVisibility() {
		showPassword = !showPassword;
	}

	// Toggle confirm password visibility
	function toggleConfirmPasswordVisibility() {
		showConfirmPassword = !showConfirmPassword;
	}

	// Toggle current password visibility for password change
	function toggleCurrentPasswordVisibility() {
		showCurrentPassword = !showCurrentPassword;
	}

	// Toggle new password visibility for password change
	function toggleNewPasswordVisibility() {
		showNewPassword = !showNewPassword;
	}

	// Toggle confirm new password visibility for password change
	function toggleConfirmNewPasswordVisibility() {
		showConfirmNewPassword = !showConfirmNewPassword;
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
			await login(usernameInput.trim(), password);
			// Success handling is done via Gun's 'auth' event in gunSetup
			showLoginPanel = false;
			// On successful login, we can clear the form values
			shouldPreserveFormValues = false;
			resetForm();
			shouldPreserveFormValues = true; // Reset flag for future use
			clearLoginPanelTimer();
			isPanelClosing = false;
			// globalState.showToast('Signed in successfully', 'success');
		} catch (error) {
			console.error('Authentication error:', error);
			errorMessage = error instanceof Error ? error.message : 'Authentication error';
		}
	}

	// Handle signup/register with Gun
	async function handleSignup() {
		try {
			await signup(usernameInput.trim(), password);
			// Success handling is done via Gun's 'auth' event in gunSetup
			showLoginPanel = false;
			// On successful signup, we can clear the form values
			shouldPreserveFormValues = false;
			resetForm();
			shouldPreserveFormValues = true; // Reset flag for future use
			clearLoginPanelTimer();
			isPanelClosing = false;
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
			clearLoginPanelTimer();
			isPanelClosing = false;
			// Clear form values on logout
			shouldPreserveFormValues = false;
			resetForm();
			shouldPreserveFormValues = true; // Reset flag for future use
			globalState.showToast('Signed out successfully', 'info');
			goto(base + '/');
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
				// Using any type to bypass incorrect type definitions in gun-avatar
				const avatarConfig: any = {
					pub: userPub,
					size,
					round: true,
					draw: 'circles'
				};
				return gunAvatar(avatarConfig);
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

	// Replace handleDownloadTree with handleCopyTree
	async function handleCopyTree() {
		if (!tree) {
			globalState.showToast('No tree data available to copy', 'error');
			return;
		}

		try {
			const jsonData = JSON.stringify(tree, null, 2);
			await navigator.clipboard.writeText(jsonData);
			globalState.showToast('Tree data copied to clipboard', 'success');
		} catch (error) {
			console.error('Error copying tree:', error);
			globalState.showToast('Failed to copy tree data', 'error');
		}
	}

	// Search functionality - now using improved protocol.ts functions

	// Toggle search panel
	function toggleSearchPanel() {
		showSearchPanel = !showSearchPanel;

		if (showSearchPanel) {
			// Close other panels
			showLoginPanel = false;
			showNotificationPanel = false;

			// Focus search input after a short delay
			setTimeout(() => {
				if (searchInputRef) {
					searchInputRef.focus();
				}
			}, 100);
		} else {
			// Clear search when closing
			searchQuery = '';
			selectedResultIndex = -1;
		}
	}

	// Navigate to selected search result
	function navigateToSearchResult(result: {
		node: Node;
		navigationPath: string[];
		displayPath: string;
		score: number;
	}) {
		if (!user) {
			showLoginPanel = true;
			startLoginPanelTimer();
			return;
		}

		// Directly navigate to the result
		globalState.navigateToPath(result.navigationPath);

		// Close search panel
		toggleSearchPanel();

		globalState.showToast(`Navigated to "${result.node.name}"`, 'success');
	}

	// Handle keyboard navigation in search
	function handleSearchKeydown(event: KeyboardEvent) {
		if (!searchResults.length) return;

		switch (event.key) {
			case 'ArrowDown':
				event.preventDefault();
				selectedResultIndex = Math.min(selectedResultIndex + 1, searchResults.length - 1);
				break;

			case 'ArrowUp':
				event.preventDefault();
				selectedResultIndex = Math.max(selectedResultIndex - 1, 0);
				break;

			case 'Enter':
				event.preventDefault();
				if (searchResults[selectedResultIndex]) {
					navigateToSearchResult(searchResults[selectedResultIndex]);
				}
				break;

			case 'Escape':
				event.preventDefault();
				toggleSearchPanel();
				break;
		}
	}

	// Timer reset is handled directly in form interaction handlers

	// Toggle password change form
	function togglePasswordChange() {
		showPasswordChange = !showPasswordChange;
		if (!showPasswordChange) {
			resetPasswordChangeForm();
		}
	}

	// Reset password change form
	function resetPasswordChangeForm() {
		currentPassword = '';
		newPassword = '';
		confirmNewPassword = '';
		passwordChangeError = '';
		showCurrentPassword = false;
		showNewPassword = false;
		showConfirmNewPassword = false;
	}

	// Handle password change
	async function handlePasswordChange() {
		// Validate inputs
		if (!currentPassword || !newPassword || !confirmNewPassword) {
			passwordChangeError = 'Please fill in all fields';
			return;
		}

		if (newPassword !== confirmNewPassword) {
			passwordChangeError = 'New passwords do not match';
			return;
		}

		if (newPassword.length < 8) {
			passwordChangeError = 'New password must be at least 8 characters';
			return;
		}

		if (newPassword === currentPassword) {
			passwordChangeError = 'New password must be different from current password';
			return;
		}

		isChangingPassword = true;
		passwordChangeError = '';

		try {
			await changePassword(currentPassword, newPassword);
			globalState.showToast('Password changed successfully', 'success');
			resetPasswordChangeForm();
			showPasswordChange = false;
		} catch (error) {
			console.error('Password change error:', error);
			passwordChangeError = error instanceof Error ? error.message : 'Failed to change password';
		} finally {
			isChangingPassword = false;
		}
	}
</script>

<div class="header" bind:this={headerRef}>
	<!-- Main header row -->
	<div class="header-main">
		<div class="node-name">
			<div class="breadcrumbs" bind:this={breadcrumbsRef}>
				{#if isAuthenticatingState}
					<div class="breadcrumb-item loading-path">Loading...</div>
				{:else if currentPathInfo.length === 0 && $userAlias}
					<a
						href="{base}/"
						class="breadcrumb-item auth-root current"
						onclick={(e) => handleBreadcrumbClick(0, e)}
						tabindex="0"
						aria-label={$userAlias}
					>
						{truncateText($userAlias)}
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
							href="{base}/"
							class="breadcrumb-item"
							class:current={index === currentPathInfo.length - 1}
							class:auth-root={index === 0}
							class:drop-target={globalState.isDragging}
							data-node-id={segment.id}
							onclick={(e) => {
								handleBreadcrumbClick(index, e);
							}}
							ondragover={(e) => {
								e.preventDefault();
							}}
							ondrop={(e) => {
								e.preventDefault();
								if (globalState.isDragging && globalState.draggedNodeId) {
									globalState.handleNodeReorder(globalState.draggedNodeId, segment.id);
									globalState.endDrag();
								}
							}}
							onpointerenter={(e) => {
								if (globalState.isDragging) {
									e.currentTarget.style.backgroundColor = 'rgba(33, 150, 243, 0.2)';
								}
							}}
							onpointerleave={(e) => {
								if (globalState.isDragging) {
									e.currentTarget.style.backgroundColor = 'rgba(33, 150, 243, 0.1)';
								}
							}}
							tabindex="0"
							aria-label={segment.name}
							title={segment.name}
						>
							{truncateText(segment.name)}
						</a>
					{/each}
				{/if}
			</div>
		</div>

		<div class="header-controls">
			<a href="{base}/inventory" class="icon-button inventory-button" title="View inventory">
				<span>📊</span>
			</a>

			<button class="icon-button help-button" title="Start guided tour" onclick={handleTourClick}>
				<span>❓</span>
			</button>

			<!--
			<button
				class="icon-button notification-button"
				title="View notifications"
				onclick={toggleNotificationPanel}
			>
				<span>🔔</span>
			</button>
		-->
		</div>
	</div>

	<!-- Login panel (dropdown) -->
	{#if showLoginPanel}
		<div class="login-panel" class:closing={isPanelClosing} bind:this={loginPanelRef}>
			{#if isAuthenticatingState}
				<div class="loading-state">
					<div class="spinner"></div>
					<p>Checking authentication...</p>
				</div>
			{:else if $userAlias}
				<!-- Logged in view -->
				<div class="welcome-panel">
					<div class="user-profile">
						<div class="avatar large">
							<img src={generateAvatar($userAlias, $userPub, 60)} alt={$userAlias} />
						</div>
						<div class="user-details">
							<h3 title={$userAlias}>Welcome, {truncateText($userAlias, 15)}</h3>
							<p class="user-id" title={'@' + ($userAlias || 'anonymous')}>
								@{truncateText($userAlias || 'anonymous', 15)}
							</p>
							{#if $userPub}
								<button
									class="copy-pub-btn"
									title="Click to copy your public key"
									onclick={() => copyToClipboard($userPub)}
								>
									<span class="pub-text"> {truncateText($userPub, 20)}</span>
									<span class="copy-icon">📋</span>
								</button>
							{/if}
						</div>
					</div>

					{#if showPasswordChange}
						<div class="password-change-form">
							<h4>Change Password</h4>

							{#if passwordChangeError}
								<div class="error-message">{passwordChangeError}</div>
							{/if}

							<form
								onsubmit={(e) => {
									e.preventDefault();
									handlePasswordChange();
								}}
							>
								<div class="form-group password-group">
									<label for="currentPassword">Current Password</label>
									<div class="password-input-container">
										<input
											type={showCurrentPassword ? 'text' : 'password'}
											id="currentPassword"
											name="current-password"
											bind:value={currentPassword}
											placeholder="Enter current password"
											disabled={isChangingPassword}
											autocomplete="current-password"
											required
										/>
										<button
											type="button"
											class="toggle-password"
											onclick={toggleCurrentPasswordVisibility}
											title={showCurrentPassword ? 'Hide password' : 'Show password'}
										>
											<span>{showCurrentPassword ? '🙈' : '🐵'}</span>
										</button>
									</div>
								</div>

								<div class="form-group password-group">
									<label for="newPassword">New Password</label>
									<div class="password-input-container">
										<input
											type={showNewPassword ? 'text' : 'password'}
											id="newPassword"
											name="new-password"
											bind:value={newPassword}
											placeholder="Enter new password"
											disabled={isChangingPassword}
											autocomplete="new-password"
											required
										/>
										<button
											type="button"
											class="toggle-password"
											onclick={toggleNewPasswordVisibility}
											title={showNewPassword ? 'Hide password' : 'Show password'}
										>
											<span>{showNewPassword ? '🙈' : '🐵'}</span>
										</button>
									</div>
								</div>

								<div class="form-group password-group">
									<label for="confirmNewPassword">Confirm New Password</label>
									<div class="password-input-container">
										<input
											type={showConfirmNewPassword ? 'text' : 'password'}
											id="confirmNewPassword"
											name="new-password"
											bind:value={confirmNewPassword}
											placeholder="Confirm new password"
											disabled={isChangingPassword}
											autocomplete="new-password"
											required
										/>
										<button
											type="button"
											class="toggle-password"
											onclick={toggleConfirmNewPasswordVisibility}
											title={showConfirmNewPassword ? 'Hide password' : 'Show password'}
										>
											<span>{showConfirmNewPassword ? '🙈' : '🐵'}</span>
										</button>
									</div>
								</div>

								<div class="password-change-actions">
									<button
										type="submit"
										class="change-password-btn"
										disabled={isChangingPassword ||
											!currentPassword ||
											!newPassword ||
											!confirmNewPassword}
									>
										{#if isChangingPassword}
											<div class="spinner small"></div>
											Changing...
										{:else}
											Change Password
										{/if}
									</button>
									<button
										type="button"
										class="cancel-password-btn"
										onclick={togglePasswordChange}
										disabled={isChangingPassword}
									>
										Cancel
									</button>
								</div>
							</form>
						</div>
					{/if}

					<div class="actions">
						<button class="key-btn" title="Change password" onclick={togglePasswordChange}>
							<span>🔑</span>
						</button>
						<button class="copy-tree-btn" title="Copy tree" onclick={handleCopyTree}>
							<span>🌲📋</span>
						</button>
						<button class="logout-btn" onclick={handleLogout}>Log Out</button>
					</div>
				</div>
			{:else}
				<!-- Login/Register form -->
				<div class="login-form">
					<h3>{isRegisterMode ? 'Create Account' : 'Sign In'}</h3>
					{#if errorMessage}
						<div class="error-message">{errorMessage}</div>
					{/if}

					{#if authMessage}
						<div class="auth-message">{authMessage}</div>
					{/if}

					<form method="post" onsubmit={handleSubmit}>
						<div class="form-group">
							<label for="username">Username</label>
							<input
								type="text"
								id="username"
								name="username"
								bind:value={usernameInput}
								placeholder="Enter username"
								disabled={isLoading}
								autocomplete="username"
								required
								oninput={() => showLoginPanel && startLoginPanelTimer()}
							/>
							{#if isRegisterMode}
								<div class="form-info">
									⚠️ Choose wisely! Usernames cannot be changed after creation
								</div>
							{/if}
						</div>

						<div class="form-group password-group">
							<label for="password">Password</label>
							<div class="password-input-container">
								<input
									type={showPassword ? 'text' : 'password'}
									id="password"
									name="password"
									bind:value={password}
									placeholder="Enter password"
									disabled={isLoading}
									autocomplete={isRegisterMode ? 'new-password' : 'current-password'}
									required
									oninput={() => showLoginPanel && startLoginPanelTimer()}
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
										type={showConfirmPassword ? 'text' : 'password'}
										id="confirmPassword"
										name="confirmPassword"
										bind:value={confirmPassword}
										placeholder="Confirm password"
										disabled={isLoading}
										autocomplete="new-password"
										required
										oninput={() => showLoginPanel && startLoginPanelTimer()}
									/>
									<button
										type="button"
										class="toggle-password"
										onclick={toggleConfirmPasswordVisibility}
										title={showConfirmPassword ? 'Hide password' : 'Show password'}
									>
										<span>{showConfirmPassword ? '🙈' : '🐵'}</span>
									</button>
								</div>
							</div>
							{#if isRegisterMode}
								<div class="form-info">🔐 Save this password!</div>
							{/if}

							<div class="form-group checkbox-group">
								<label class="checkbox-label">
									<input
										type="checkbox"
										id="agreedToTerms"
										bind:checked={agreedToTerms}
										disabled={isLoading}
									/>
									<span class="checkbox-text">
										I agree to the <a href="{base}/privacy" target="_blank" class="terms-link"
											>Privacy Policy</a
										>
										and <a href="{base}/terms" target="_blank" class="terms-link">Terms of Use</a>
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

	<!-- Notification panel (dropdown) -->
	{#if showNotificationPanel}
		<div class="notification-panel" bind:this={notificationPanelRef}>
			<div class="notification-content">
				<h3>Notifications</h3>
				<p class="feature-message">This feature is not implemented yet</p>
				<div class="actions">
					<button class="close-btn" onclick={toggleNotificationPanel}>Close</button>
				</div>
			</div>
		</div>
	{/if}

	<!-- Search panel (dropdown) -->
	{#if showSearchPanel}
		<div class="search-panel" bind:this={searchPanelRef}>
			<div class="search-content">
				<h3>Search Tree</h3>
				<div class="search-input-container">
					<input
						type="text"
						bind:this={searchInputRef}
						bind:value={searchQuery}
						placeholder="Type to search nodes..."
						class="search-input"
						onkeydown={handleSearchKeydown}
					/>
					<span class="search-icon">🔍</span>
				</div>

				{#if searchResults.length > 0}
					<div class="search-results">
						{#each searchResults as result, index}
							<button
								class="search-result"
								class:selected={index === selectedResultIndex}
								onclick={() => navigateToSearchResult(result)}
								title={`Navigate to ${result.node.name}`}
							>
								<div class="result-info">
									<span class="result-name">{result.node.name}</span>
									<span class="result-path">{result.displayPath}</span>
								</div>
							</button>
						{/each}
					</div>
				{:else if searchQuery.trim()}
					<div class="no-results">
						<p>No nodes found matching "{searchQuery}"</p>
					</div>
				{/if}

				<div class="search-help">
					<p>
						<kbd>↑</kbd><kbd>↓</kbd> to navigate • <kbd>Enter</kbd> to select • <kbd>Esc</kbd> to close
					</p>
				</div>

				<div class="actions">
					<button class="close-btn" onclick={toggleSearchPanel}>Close</button>
				</div>
			</div>
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
		transition:
			opacity 0.3s ease-out,
			transform 0.3s ease-out;
	}

	.login-panel.closing {
		opacity: 0;
		transform: translateY(-10px);
	}

	.notification-panel {
		position: absolute;
		top: calc(100% - 8px);
		right: 16px;
		width: 280px;
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

	/* Adding a pointer/arrow to the notification panel */
	.notification-panel::before {
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
		touch-action: pan-x; /* Allow horizontal scrolling but prevent vertical gestures */
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
		touch-action: manipulation; /* Disable double-tap zoom but allow other touch gestures */
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

	.breadcrumb-item.drop-target {
		background-color: rgba(33, 150, 243, 0.1);
		border: 1px dashed #2196f3;
		border-radius: 4px;
		transition: all 0.2s ease;
	}

	.breadcrumb-item.drop-target:hover {
		background-color: rgba(33, 150, 243, 0.2);
		border-color: #1976d2;
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

	/* Pulse animation for additional emphasis */
	@keyframes pulse {
		0%,
		100% {
			box-shadow: 0 0 8px rgba(244, 67, 54, 0.3);
		}
		50% {
			box-shadow:
				0 0 16px rgba(244, 67, 54, 0.6),
				0 0 24px rgba(244, 67, 54, 0.3);
		}
	}

	/* Blue pulse animation for recompose button */
	@keyframes pulse-blue {
		0%,
		100% {
			box-shadow: 0 0 8px rgba(33, 150, 243, 0.3);
		}
		50% {
			box-shadow:
				0 0 16px rgba(33, 150, 243, 0.6),
				0 0 24px rgba(33, 150, 243, 0.3);
		}
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

	.close-btn {
		background: #f5f5f5;
		color: #333;
		border: none;
		border-radius: 4px;
		padding: 8px 12px;
		font-size: 0.95em;
		cursor: pointer;
	}

	.close-btn:hover {
		background: #e0e0e0;
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

	.copy-tree-btn {
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

	.copy-tree-btn:hover {
		background: #388e3c;
	}

	.copy-tree-btn:active {
		transform: scale(0.98);
	}

	.key-btn {
		background: #f5f5f5;
		color: #333;
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

	.key-btn:hover {
		background: #e0e0e0;
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

	.copy-pub-btn {
		background: none;
		border: 1px solid #e0e0e0;
		border-radius: 4px;
		padding: 4px 8px;
		font-size: 0.8em;
		color: #666;
		cursor: pointer;
		display: flex;
		align-items: center;
		justify-content: space-between;
		width: 100%;
		margin-top: 8px;
		transition: all 0.2s ease;
	}

	.copy-pub-btn:hover {
		background: #f5f5f5;
		border-color: #ccc;
	}

	.pub-text {
		overflow: hidden;
		text-overflow: ellipsis;
		white-space: nowrap;
		margin-right: 8px;
	}

	.copy-icon {
		flex-shrink: 0;
		font-size: 1.1em;
	}

	/* Notification panel styles */
	.notification-content {
		display: flex;
		flex-direction: column;
	}

	.notification-content h3 {
		margin-top: 0;
		margin-bottom: 16px;
		font-size: 1.2em;
		color: #333;
	}

	.feature-message {
		color: #666;
		font-style: italic;
		text-align: center;
		margin: 20px 0;
		padding: 16px;
		background: #f5f5f5;
		border-radius: 4px;
		border-left: 3px solid #2196f3;
	}

	/* Search panel styles */
	.search-panel {
		position: absolute;
		top: calc(100% - 8px);
		right: 60px;
		width: 350px;
		max-height: 500px;
		background-color: white;
		box-shadow: 0 4px 12px rgba(0, 0, 0, 0.15);
		border-radius: 8px;
		padding: 16px;
		z-index: 10;
		animation: dropDown 0.2s ease-out;
		overflow-y: auto;
	}

	/* Add mobile-friendly media query */
	@media (max-width: 480px) {
		.search-panel {
			position: fixed;
			top: 50%;
			left: 50%;
			transform: translate(-50%, -50%);
			width: calc(100% - 32px);
			max-width: 350px;
			max-height: 80vh;
			margin: 0 auto;
			box-shadow: 0 4px 20px rgba(0, 0, 0, 0.2);
			z-index: 100;
		}

		.search-panel::before {
			display: none; /* Remove the arrow on mobile */
		}
	}

	.search-panel::before {
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

	.search-content {
		display: flex;
		flex-direction: column;
	}

	.search-content h3 {
		margin-top: 0;
		margin-bottom: 16px;
		font-size: 1.2em;
		color: #333;
	}

	.search-input-container {
		position: relative;
		margin-bottom: 16px;
	}

	.search-input {
		width: 100%;
		padding: 10px 40px 10px 12px;
		border: 2px solid #e0e0e0;
		border-radius: 6px;
		font-size: 0.95em;
		background: #fafafa;
		transition: all 0.2s ease;
	}

	.search-input:focus {
		outline: none;
		border-color: #2196f3;
		background: white;
		box-shadow: 0 0 0 3px rgba(33, 150, 243, 0.1);
	}

	.search-icon {
		position: absolute;
		right: 12px;
		top: 50%;
		transform: translateY(-50%);
		color: #666;
		font-size: 16px;
		pointer-events: none;
	}

	.search-results {
		max-height: 300px;
		overflow-y: auto;
		margin-bottom: 16px;
		border-radius: 6px;
		border: 1px solid #e0e0e0;
	}

	.search-result {
		width: 100%;
		display: flex;
		justify-content: space-between;
		align-items: center;
		padding: 12px;
		border: none;
		background: white;
		text-align: left;
		cursor: pointer;
		transition: background-color 0.2s ease;
		border-bottom: 1px solid #f0f0f0;
	}

	.search-result:last-child {
		border-bottom: none;
	}

	.search-result:hover,
	.search-result.selected {
		background-color: #f5f9ff;
		border-left: 3px solid #2196f3;
	}

	.result-info {
		display: flex;
		flex-direction: column;
		flex: 1;
		min-width: 0;
	}

	.result-name {
		font-weight: 600;
		color: #333;
		margin-bottom: 2px;
	}

	.result-path {
		font-size: 0.8em;
		color: #666;
		overflow: hidden;
		text-overflow: ellipsis;
		white-space: nowrap;
	}

	.no-results {
		text-align: center;
		padding: 20px;
		color: #666;
		background: #f9f9f9;
		border-radius: 6px;
		margin-bottom: 16px;
	}

	.search-help {
		margin-bottom: 16px;
		text-align: center;
	}

	.search-help p {
		margin: 0;
		font-size: 0.8em;
		color: #666;
	}

	.search-help kbd {
		background: #f0f0f0;
		border: 1px solid #ccc;
		border-radius: 3px;
		padding: 2px 6px;
		font-size: 0.8em;
		margin: 0 2px;
	}

	/* Password change form styles */
	.password-change-form {
		margin: 16px 0 0 0;
		padding-top: 16px;
		border-top: 1px solid #e0e0e0;
	}

	.password-change-form h4 {
		margin: 0 0 12px 0;
		color: #333;
		font-size: 1.1em;
	}

	.password-change-actions {
		display: flex;
		gap: 8px;
		margin-top: 16px;
	}

	.change-password-btn {
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

	.change-password-btn:hover {
		background: #1976d2;
	}

	.change-password-btn:disabled {
		background: #bbdefb;
		cursor: not-allowed;
	}

	.cancel-password-btn {
		background: #f5f5f5;
		color: #333;
		border: none;
		border-radius: 4px;
		padding: 8px 12px;
		font-size: 0.95em;
		cursor: pointer;
	}

	.cancel-password-btn:hover {
		background: #e0e0e0;
	}

	.cancel-password-btn:disabled {
		color: #999;
		cursor: not-allowed;
	}

	.form-info {
		margin-top: 4px;
		color: #64748b;
		font-size: 0.85em;
		line-height: 1.4;
	}
</style>
