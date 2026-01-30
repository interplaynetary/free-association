<script lang="ts">
	import { meshUser, isMeshAuthenticating, meshUserAlias } from '$lib/network/mesh.svelte';
	import { DeciderWidget } from '$lib/modules/decider/components';
	import { TIME } from '$lib/modules/decider/constants';
	
	// Unique identifier for this decision game
	let gameId = 'comprehensive-test-' + Date.now();
	
	// Comprehensive test agenda showcasing all time configuration features
	let agenda = [
		'Quick decision?', // Simple string (uses global defaults: 30s total)
		{ 
			text: 'Important decision with custom time',
			timeWindow: 60000 // 60 seconds for this item
		},
		{
			text: 'Complex decision with custom phases',
			timeWindow: 90000, // 90 seconds total
			phaseTimeConfig: {
				proposing: 30000,   // 30s for proposing
				challenging: 20000,  // 20s for challenging
				commenting: 20000,   // 20s for commenting
				supporting: 20000    // 20s for supporting
			}
		}
	];
	
	// Global default: 30 seconds per round (divided into 4 phases: ~7.5s each)
	let timeWindow = TIME.TEST_WINDOW;
</script>

<div class="decider-page">
	{#if $isMeshAuthenticating}
		<div class="loading-state">
			<p>Authenticating...</p>
		</div>
	{:else if !meshUser || !meshUser.is}
		<div class="auth-required">
			<h2>🔐 Authentication Required</h2>
			<p>Please log in to participate in decision-making.</p>
		</div>
	{:else}
		<div class="header">
			<h1>🎯 Iterative Consensus Protocol</h1>
			{#if $meshUserAlias}
				<p class="welcome">Welcome, {$meshUserAlias}!</p>
			{/if}
			<p class="description">
				Distributed decision-making with time-based phases and meta-governance
			</p>
		</div>
		
		<DeciderWidget user={meshUser} {gameId} {agenda} {timeWindow} variant="inline" />
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
	
	.description {
		margin: 0.5rem 0 0 0;
		font-size: 0.875rem;
		color: #999;
		font-style: italic;
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