<script lang="ts">
	import { holsterUser, isHolsterAuthenticating, holsterUserAlias } from '$lib/network/holster.svelte';
	import { DeciderWidget } from '$lib/modules/decider/components';
	
	// Unique identifier for this decision game
	let gameId = 'dinner-decision-2024';
	
	// Optional: Set custom agenda
	let agenda = ['What should we have for dinner?'];
</script>

<div class="decider-page">
	{#if $isHolsterAuthenticating}
		<div class="loading-state">
			<p>Authenticating...</p>
		</div>
	{:else if !holsterUser || !holsterUser.is}
		<div class="auth-required">
			<h2>🔐 Authentication Required</h2>
			<p>Please log in to participate in decision-making.</p>
		</div>
	{:else}
		<div class="header">
			<h1>🎯 Group Decision Making</h1>
			{#if $holsterUserAlias}
				<p class="welcome">Welcome, {$holsterUserAlias}!</p>
			{/if}
		</div>
		
		<DeciderWidget user={holsterUser} {gameId} {agenda} variant="inline" />
	{/if}
</div>

<style>
	.decider-page {
		max-width: 1400px;
		margin: 0 auto;
		padding: 2rem;
		min-height: 100vh;
	}
	
	.header {
		text-align: center;
		margin-bottom: 2rem;
	}
	
	.header h1 {
		margin: 0 0 0.5rem 0;
		font-size: 2.5rem;
		color: #333;
	}
	
	.welcome {
		margin: 0;
		font-size: 1.125rem;
		color: #666;
	}
	
	.loading-state,
	.auth-required {
		display: flex;
		flex-direction: column;
		align-items: center;
		justify-content: center;
		min-height: 60vh;
		text-align: center;
	}
	
	.auth-required h2 {
		margin: 0 0 1rem 0;
		font-size: 2rem;
		color: #667eea;
	}
	
	.auth-required p {
		margin: 0;
		font-size: 1.125rem;
		color: #666;
	}
	
	.loading-state p {
		font-size: 1.125rem;
		color: #666;
	}
	
	@media (max-width: 768px) {
		.decider-page {
			padding: 1rem;
		}
		
		.header h1 {
			font-size: 1.75rem;
		}
	}
</style>