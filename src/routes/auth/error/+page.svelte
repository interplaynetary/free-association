<script lang="ts">
	import { page } from '$app/stores';

	// Get error message directly with a single derived value
	let errorMessage = $derived(getErrorMessage($page.url.searchParams.get('error')));

	function getErrorMessage(error: string | null): string {
		if (!error) return 'Unknown error';

		switch (error) {
			case 'Signin':
				return 'Try signing in with a different account.';
			case 'OAuthSignin':
				return 'Try signing in with a different account.';
			case 'OAuthCallback':
				return 'Try signing in with a different account.';
			case 'OAuthCreateAccount':
				return 'Try signing in with a different account.';
			case 'EmailCreateAccount':
				return 'Try signing in with a different account.';
			case 'Callback':
				return 'Try signing in with a different account.';
			case 'OAuthAccountNotLinked':
				return 'To confirm your identity, sign in with the same account you used originally.';
			case 'EmailSignin':
				return 'Check your email address.';
			case 'CredentialsSignin':
				return 'Sign in failed. Check the details you provided are correct.';
			case 'default':
				return 'Unable to sign in.';
			default:
				return `An error occurred: ${error}`;
		}
	}
</script>

<div class="error-container">
	<div class="error-card">
		<h1>Authentication Error</h1>
		<div class="error-message">
			{errorMessage}
		</div>
		<div class="actions">
			<a href="/auth/login" class="button">Try again</a>
			<a href="/" class="link">Back to home</a>
		</div>
	</div>
</div>

<style>
	.error-container {
		display: flex;
		justify-content: center;
		align-items: center;
		min-height: 80vh;
		padding: 1rem;
	}

	.error-card {
		background: #fff;
		border-radius: 8px;
		box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1);
		padding: 2rem;
		width: 100%;
		max-width: 500px;
		text-align: center;
	}

	h1 {
		margin-top: 0;
		color: #dc2626;
	}

	.error-message {
		margin: 1.5rem 0;
		padding: 1rem;
		background-color: #fee2e2;
		border-radius: 4px;
		color: #b91c1c;
	}

	.actions {
		display: flex;
		flex-direction: column;
		gap: 1rem;
		margin-top: 1.5rem;
	}

	.button {
		display: inline-block;
		padding: 0.75rem 1.5rem;
		background-color: #4f46e5;
		color: white;
		text-decoration: none;
		border-radius: 4px;
		font-weight: 500;
		transition: background-color 0.2s;
	}

	.button:hover {
		background-color: #4338ca;
	}

	.link {
		color: #4f46e5;
		text-decoration: none;
		margin-top: 0.5rem;
	}

	.link:hover {
		text-decoration: underline;
	}
</style>
