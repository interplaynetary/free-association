<script lang="ts">
	import { page } from '$app/stores';
	import { goto } from '$app/navigation';
	import { browser } from '$app/environment';

	let isLoading = $state(false);
	let userId = $derived($page.data.session?.user?.id);

	// Function to redirect to user's root node
	async function redirectToUserNode() {
		if (!browser || !userId) return;
		isLoading = true;
		try {
			goto(`/${userId}`);
		} catch (error) {
			console.error('Error redirecting:', error);
		} finally {
			isLoading = false;
		}
	}

	// Function to trigger the header login panel
	function openLoginPanel() {
		if (!browser) return;
		// Find the first breadcrumb and click it to open login panel
		const loginTrigger = document.querySelector('.breadcrumb-item.auth-root') as HTMLElement | null;
		if (loginTrigger) {
			loginTrigger.click();
		}
	}

	// Run the redirect if needed
	if (userId) {
		redirectToUserNode();
	}
</script>

{#if isLoading}
	<div class="loading-container">
		<p>Loading your data...</p>
	</div>
{:else}
	<div class="home-container">
		<div class="welcome-card">
			<h1>Welcome to Free Association</h1>
			<p>Connect, share, and fulfill needs in your network</p>

			<div class="actions">
				<button class="btn btn-primary" onclick={openLoginPanel}> Get Started </button>
			</div>
		</div>
	</div>
{/if}

<style>
	.loading-container {
		display: flex;
		justify-content: center;
		align-items: center;
		min-height: calc(100vh - 120px);
		font-size: 1.2rem;
		color: #666;
	}

	.home-container {
		display: flex;
		justify-content: center;
		align-items: center;
		min-height: calc(100vh - 120px);
		padding: 1rem;
	}

	.welcome-card {
		background: white;
		border-radius: 8px;
		box-shadow: 0 4px 12px rgba(0, 0, 0, 0.1);
		padding: 2rem;
		text-align: center;
		max-width: 500px;
		width: 100%;
	}

	h1 {
		color: #2196f3;
		margin-bottom: 1rem;
	}

	p {
		color: #666;
		margin-bottom: 2rem;
		font-size: 1.1rem;
	}

	.actions {
		display: flex;
		gap: 1rem;
		justify-content: center;
	}

	.btn {
		display: inline-block;
		padding: 0.75rem 1.5rem;
		border-radius: 4px;
		font-weight: 500;
		text-decoration: none;
		font-size: 1rem;
		transition: all 0.2s ease;
		cursor: pointer;
		border: none;
	}

	.btn-primary {
		background-color: #2196f3;
		color: white;
	}

	.btn-primary:hover {
		background-color: #1976d2;
	}
</style>
