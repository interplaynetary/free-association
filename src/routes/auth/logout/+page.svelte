<script lang="ts">
	import { browser } from '$app/environment';
	import { goto } from '$app/navigation';
	import { signOut } from '@auth/sveltekit/client';

	$effect.root(() => {
		if (!browser) return;

		// Using IIFE for async code
		(async () => {
			try {
				await signOut({ callbackUrl: '/' });
			} catch (error) {
				console.error('Logout error:', error);
			}
		})();

		// Cleanup function
		return () => {};
	});
</script>

<div class="logout-container">
	<div class="logout-card">
		<h1>Logging out...</h1>
		<p>Please wait while we sign you out.</p>
	</div>
</div>

<style>
	.logout-container {
		display: flex;
		justify-content: center;
		align-items: center;
		min-height: 80vh;
		padding: 1rem;
	}

	.logout-card {
		background: #fff;
		border-radius: 8px;
		box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1);
		padding: 2rem;
		width: 100%;
		max-width: 400px;
		text-align: center;
	}

	h1 {
		margin-top: 0;
	}
</style>
