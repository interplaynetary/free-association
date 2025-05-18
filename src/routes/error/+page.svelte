<script lang="ts">
	import { page } from '$app/stores';

	// Get the error message from URL params using Svelte 5 derived state
	let error = $derived($page.url.searchParams.get('error'));
</script>

<div class="error-container">
	<div class="error-card">
		<h1>Authentication Error</h1>

		{#if error}
			<div class="error-message">
				{#if error === 'Signin'}
					There was a problem signing in.
				{:else if error === 'OAuthSignin'}
					There was a problem with the OAuth provider.
				{:else if error === 'OAuthCallback'}
					There was a problem with the OAuth callback.
				{:else if error === 'OAuthCreateAccount'}
					There was a problem creating your account with the OAuth provider.
				{:else if error === 'EmailCreateAccount'}
					There was a problem creating your account with your email.
				{:else if error === 'Callback'}
					There was a problem with the authentication callback.
				{:else if error === 'OAuthAccountNotLinked'}
					Your account is already linked to another sign-in method.
				{:else if error === 'EmailSignin'}
					There was a problem sending the email for sign-in.
				{:else if error === 'CredentialsSignin'}
					The credentials you provided were invalid.
				{:else if error === 'SessionRequired'}
					You must be signed in to access this page.
				{:else}
					An unknown authentication error occurred.
				{/if}
			</div>
		{:else}
			<div class="error-message">An unknown error occurred.</div>
		{/if}

		<div class="actions">
			<a href="/auth/login" class="button primary">Return to Sign In</a>
			<a href="/" class="button secondary">Go to Home</a>
		</div>
	</div>
</div>

<style>
	.error-container {
		display: flex;
		justify-content: center;
		align-items: center;
		height: 100vh;
		padding: 1rem;
		background-color: #f5f5f5;
	}

	.error-card {
		background: white;
		border-radius: 8px;
		box-shadow: 0 4px 12px rgba(0, 0, 0, 0.1);
		padding: 2rem;
		width: 100%;
		max-width: 500px;
		text-align: center;
	}

	h1 {
		color: #d32f2f;
		margin-bottom: 1.5rem;
	}

	.error-message {
		background-color: #ffebee;
		color: #c62828;
		padding: 1rem;
		border-radius: 4px;
		margin-bottom: 2rem;
		font-size: 1rem;
		line-height: 1.5;
	}

	.actions {
		display: flex;
		flex-direction: column;
		gap: 0.75rem;
	}

	.button {
		padding: 0.75rem 1.5rem;
		border-radius: 4px;
		font-weight: 500;
		text-decoration: none;
		transition: all 0.2s;
	}

	.primary {
		background-color: #2196f3;
		color: white;
	}

	.primary:hover {
		background-color: #1976d2;
	}

	.secondary {
		background-color: #f5f5f5;
		color: #333;
	}

	.secondary:hover {
		background-color: #e0e0e0;
	}

	@media (min-width: 640px) {
		.actions {
			flex-direction: row;
			justify-content: center;
		}
	}
</style>
