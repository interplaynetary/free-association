<script lang="ts">
	import Decider from '$lib/components/Decider.svelte';
	import { holsterUser, isHolsterAuthenticating, holsterUserAlias } from '$lib/state/holster.svelte';
	import { onMount } from 'svelte';
	
	// Generate or use a default game ID
	let gameId = $state('default-game');
	let showGameIdInput = $state(false);
	let gameIdInput = $state('');
	
	// Redirect to home if not authenticated
	$effect(() => {
		if (!$isHolsterAuthenticating && !$holsterUserAlias) {
			// User is not authenticated, redirect to home
			if (typeof window !== 'undefined') {
				window.location.href = '/';
			}
		}
	});
	
	function setCustomGameId() {
		if (gameIdInput.trim()) {
			gameId = gameIdInput.trim();
			showGameIdInput = false;
		}
	}
</script>

<svelte:head>
	<title>P2P Decider - Free Association</title>
</svelte:head>

{#if $isHolsterAuthenticating}
	<div class="container">
		<div class="loading">
			<div class="spinner"></div>
			<p>Authenticating...</p>
		</div>
	</div>
{:else if !$holsterUserAlias}
	<div class="container">
		<div class="auth-required">
			<h1>🔒 Authentication Required</h1>
			<p>You need to be logged in to use the P2P Decider.</p>
			<a href="/" class="btn-home">Go to Home</a>
		</div>
	</div>
{:else}
	<div class="page-container">
		<div class="game-id-control">
			{#if !showGameIdInput}
				<div class="game-id-display">
					<span>Current Game: <strong>{gameId}</strong></span>
					<button onclick={() => showGameIdInput = true} class="btn-change">
						Change Game ID
					</button>
				</div>
			{:else}
				<div class="game-id-input">
					<input 
						type="text" 
						bind:value={gameIdInput}
						placeholder="Enter new game ID..."
						onkeypress={(e) => e.key === 'Enter' && setCustomGameId()}
					/>
					<button onclick={setCustomGameId} class="btn-set">Set</button>
					<button onclick={() => showGameIdInput = false} class="btn-cancel">Cancel</button>
				</div>
			{/if}
		</div>
		
		<Decider user={holsterUser} {gameId} />
	</div>
{/if}

<style>
	.container {
		display: flex;
		align-items: center;
		justify-content: center;
		min-height: 100vh;
		padding: 2rem;
	}
	
	.loading {
		text-align: center;
	}
	
	.spinner {
		width: 50px;
		height: 50px;
		margin: 0 auto 1rem;
		border: 4px solid #f3f3f3;
		border-top: 4px solid #667eea;
		border-radius: 50%;
		animation: spin 1s linear infinite;
	}
	
	@keyframes spin {
		0% { transform: rotate(0deg); }
		100% { transform: rotate(360deg); }
	}
	
	.auth-required {
		text-align: center;
		background: white;
		padding: 3rem;
		border-radius: 12px;
		box-shadow: 0 4px 20px rgba(0, 0, 0, 0.1);
	}
	
	.auth-required h1 {
		margin: 0 0 1rem 0;
		color: #667eea;
	}
	
	.auth-required p {
		margin: 0 0 2rem 0;
		color: #666;
	}
	
	.btn-home {
		display: inline-block;
		padding: 0.75rem 2rem;
		background: #667eea;
		color: white;
		text-decoration: none;
		border-radius: 8px;
		font-weight: 600;
		transition: all 0.2s;
	}
	
	.btn-home:hover {
		background: #5568d3;
		transform: translateY(-2px);
		box-shadow: 0 4px 12px rgba(102, 126, 234, 0.3);
	}
	
	.page-container {
		min-height: 100vh;
	}
	
	.game-id-control {
		background: #f8f9fa;
		padding: 1rem 2rem;
		border-bottom: 2px solid #e0e0e0;
		position: sticky;
		top: 0;
		z-index: 100;
	}
	
	.game-id-display {
		display: flex;
		align-items: center;
		justify-content: space-between;
		max-width: 1200px;
		margin: 0 auto;
	}
	
	.game-id-display span {
		color: #555;
	}
	
	.game-id-display strong {
		color: #667eea;
		font-family: monospace;
	}
	
	.game-id-input {
		display: flex;
		gap: 0.5rem;
		max-width: 1200px;
		margin: 0 auto;
	}
	
	.game-id-input input {
		flex: 1;
		padding: 0.5rem 1rem;
		border: 2px solid #e0e0e0;
		border-radius: 6px;
		font-size: 1rem;
	}
	
	.game-id-input input:focus {
		outline: none;
		border-color: #667eea;
	}
	
	.btn-change, .btn-set, .btn-cancel {
		padding: 0.5rem 1rem;
		border: none;
		border-radius: 6px;
		font-size: 0.9rem;
		font-weight: 600;
		cursor: pointer;
		transition: all 0.2s;
	}
	
	.btn-change {
		background: #667eea;
		color: white;
	}
	
	.btn-change:hover {
		background: #5568d3;
	}
	
	.btn-set {
		background: #4caf50;
		color: white;
	}
	
	.btn-set:hover {
		background: #43a047;
	}
	
	.btn-cancel {
		background: #f44336;
		color: white;
	}
	
	.btn-cancel:hover {
		background: #e53935;
	}
</style>

