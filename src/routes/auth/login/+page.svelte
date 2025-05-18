<script lang="ts">
	import { signIn } from '@auth/sveltekit/client';
	import { page } from '$app/stores';
	import { goto } from '$app/navigation';

	// Form state
	let username = $state('');
	let password = $state('');
	let error = $state('');
	let loading = $state(false);

	// Derive error from URL if any
	let urlError = $derived($page.url.searchParams.get('error'));

	// Set error message when URL param is present
	$effect(() => {
		if (urlError) {
			error = 'Login failed. Please check your credentials.';
		}
	});

	async function handleSubmit() {
		if (!username || !password) {
			error = 'Please enter both username and password';
			return;
		}

		loading = true;
		error = '';

		try {
			// Call Auth.js sign in
			const result = await signIn('credentials', {
				username,
				password,
				redirect: false
			});

			if (result?.error) {
				error = 'Invalid credentials';
			} else {
				console.log('redirecting home');
				// Redirect to home on success
				goto('/');
			}
		} catch (err) {
			console.error('Login error:', err);
			error = 'An error occurred during login';
		} finally {
			loading = false;
		}
	}
</script>

<div class="login-container">
	<div class="login-card">
		<h1>Sign In</h1>

		{#if error}
			<div class="error">{error}</div>
		{/if}

		<form on:submit|preventDefault={handleSubmit}>
			<div class="input-group">
				<label for="username">Username</label>
				<input
					type="text"
					id="username"
					bind:value={username}
					disabled={loading}
					placeholder="Enter your username"
					autocomplete="username"
				/>
			</div>

			<div class="input-group">
				<label for="password">Password</label>
				<input
					type="password"
					id="password"
					bind:value={password}
					disabled={loading}
					placeholder="Enter your password"
					autocomplete="current-password"
				/>
			</div>

			<button type="submit" class="login-button" disabled={loading}>
				{#if loading}
					Signing in...
				{:else}
					Sign In
				{/if}
			</button>
		</form>

		<div class="register-link">
			Don't have an account? <a href="/auth/register">Sign up</a>
		</div>
	</div>
</div>

<style>
	.login-container {
		display: flex;
		justify-content: center;
		align-items: center;
		height: 100vh;
		padding: 1rem;
	}

	.login-card {
		background: white;
		border-radius: 8px;
		box-shadow: 0 4px 12px rgba(0, 0, 0, 0.1);
		padding: 2rem;
		width: 100%;
		max-width: 400px;
	}

	h1 {
		text-align: center;
		margin-bottom: 1.5rem;
		color: #333;
	}

	.input-group {
		margin-bottom: 1.25rem;
	}

	label {
		display: block;
		margin-bottom: 0.5rem;
		font-weight: 500;
		color: #555;
	}

	input {
		width: 100%;
		padding: 0.75rem;
		border: 1px solid #ddd;
		border-radius: 4px;
		font-size: 1rem;
	}

	input:focus {
		outline: none;
		border-color: #2196f3;
		box-shadow: 0 0 0 2px rgba(33, 150, 243, 0.2);
	}

	.login-button {
		width: 100%;
		padding: 0.75rem;
		background-color: #2196f3;
		color: white;
		border: none;
		border-radius: 4px;
		font-size: 1rem;
		font-weight: 500;
		cursor: pointer;
		transition: background-color 0.2s;
	}

	.login-button:hover {
		background-color: #1976d2;
	}

	.login-button:disabled {
		background-color: #ccc;
		cursor: not-allowed;
	}

	.error {
		background-color: #ffebee;
		color: #c62828;
		padding: 0.75rem;
		border-radius: 4px;
		margin-bottom: 1.25rem;
		font-size: 0.9rem;
	}

	.register-link {
		text-align: center;
		margin-top: 1.5rem;
		font-size: 0.9rem;
		color: #666;
	}

	.register-link a {
		color: #2196f3;
		text-decoration: none;
		font-weight: 500;
	}

	.register-link a:hover {
		text-decoration: underline;
	}
</style>
