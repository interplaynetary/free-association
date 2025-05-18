<script lang="ts">
	import { goto } from '$app/navigation';
	import { signIn } from '@auth/sveltekit/client';

	// Form state
	let username = $state('');
	let password = $state('');
	let confirmPassword = $state('');
	let error = $state('');
	let loading = $state(false);

	// Validation function
	function validateForm() {
		if (!username || !password) {
			error = 'Please enter both username and password';
			return false;
		}

		if (password !== confirmPassword) {
			error = 'Passwords do not match';
			return false;
		}

		if (password.length < 8) {
			error = 'Password must be at least 8 characters';
			return false;
		}

		return true;
	}

	async function handleSubmit() {
		// Reset error
		error = '';

		// Validate form
		if (!validateForm()) {
			return;
		}

		loading = true;

		try {
			// Register the user via our custom endpoint
			const response = await fetch('/auth/register', {
				method: 'POST',
				headers: {
					'Content-Type': 'application/json'
				},
				body: JSON.stringify({
					username,
					password
				})
			});

			const data = await response.json();

			if (!response.ok) {
				error = data.message || 'Registration failed';
				return;
			}

			// Auto sign in after successful registration
			const signInResult = await signIn('credentials', {
				username,
				password,
				redirect: false
			});

			if (signInResult?.error) {
				// If sign in fails, show error but still consider registration successful
				error = 'Account created, but automatic sign-in failed. Please sign in manually.';
			} else {
				console.log('redirecting home');
				// Redirect to home on success
				goto('/');
			}
		} catch (err) {
			console.error('Registration error:', err);
			error = 'An error occurred during registration';
		} finally {
			loading = false;
		}
	}
</script>

<div class="register-container">
	<div class="register-card">
		<h1>Create Account</h1>

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
					placeholder="Choose a username"
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
					placeholder="Create a password"
					autocomplete="new-password"
				/>
			</div>

			<div class="input-group">
				<label for="confirmPassword">Confirm Password</label>
				<input
					type="password"
					id="confirmPassword"
					bind:value={confirmPassword}
					disabled={loading}
					placeholder="Confirm your password"
					autocomplete="new-password"
				/>
			</div>

			<button type="submit" class="register-button" disabled={loading}>
				{#if loading}
					Creating account...
				{:else}
					Create Account
				{/if}
			</button>
		</form>

		<div class="login-link">
			Already have an account? <a href="/auth/login">Sign in</a>
		</div>
	</div>
</div>

<style>
	.register-container {
		display: flex;
		justify-content: center;
		align-items: center;
		height: 100vh;
		padding: 1rem;
	}

	.register-card {
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

	.register-button {
		width: 100%;
		padding: 0.75rem;
		background-color: #4caf50;
		color: white;
		border: none;
		border-radius: 4px;
		font-size: 1rem;
		font-weight: 500;
		cursor: pointer;
		transition: background-color 0.2s;
	}

	.register-button:hover {
		background-color: #388e3c;
	}

	.register-button:disabled {
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

	.login-link {
		text-align: center;
		margin-top: 1.5rem;
		font-size: 0.9rem;
		color: #666;
	}

	.login-link a {
		color: #2196f3;
		text-decoration: none;
		font-weight: 500;
	}

	.login-link a:hover {
		text-decoration: underline;
	}
</style>
