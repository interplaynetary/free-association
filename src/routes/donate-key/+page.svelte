<script lang="ts">
	import { onMount } from 'svelte';
	import { holsterUserAlias } from '$lib/network/holster.svelte';
	
	// State
	let apiKey = '';
	let donorName = '';
	let isAnonymous = false;
	let isSubmitting = false;
	let error: string | null = null;
	let success: string | null = null;
	let showKey = false;
	
	// Pool stats
	let poolStats: any = null;
	let loadingStats = true;
	
	// Fetch current pool stats
	async function fetchPoolStats() {
		try {
			const response = await fetch('/api/keys/donate');
			if (response.ok) {
				poolStats = await response.json();
			}
		} catch (err) {
			console.error('Failed to fetch pool stats:', err);
		} finally {
			loadingStats = false;
		}
	}
	
	// Handle donation
	async function handleDonate() {
		error = null;
		success = null;
		
		// Validation
		if (!apiKey.trim()) {
			error = 'Please enter an API key';
			return;
		}
		
		if (!apiKey.startsWith('sk-or-')) {
			error = 'OpenRouter API keys should start with "sk-or-"';
			return;
		}
		
		if (!isAnonymous && !donorName.trim()) {
			donorName = $holsterUserAlias || 'Anonymous';
		}
		
		isSubmitting = true;
		
		try {
			const response = await fetch('/api/keys/donate', {
				method: 'POST',
				headers: { 'Content-Type': 'application/json' },
				body: JSON.stringify({
					apiKey: apiKey.trim(),
					donorName: donorName.trim() || undefined,
					isAnonymous
				})
			});
			
			const result = await response.json();
			
			if (!response.ok) {
				throw new Error(result.message || 'Donation failed');
			}
			
			success = result.message;
			
			// Clear form
			apiKey = '';
			donorName = '';
			showKey = false;
			
			// Refresh stats
			await fetchPoolStats();
			
		} catch (err: any) {
			error = err.message || 'Failed to donate key. Please try again.';
		} finally {
			isSubmitting = false;
		}
	}
	
	onMount(() => {
		fetchPoolStats();
		// Set default donor name to user's alias
		if ($holsterUserAlias) {
			donorName = $holsterUserAlias;
		}
	});
</script>

<svelte:head>
	<title>Donate API Key - Free Association</title>
</svelte:head>

<div class="container">
	<div class="content">
		<!-- Header -->
		<div class="header">
			<h1>🎁 Donate an OpenRouter API Key</h1>
			<p class="subtitle">
				Help power AI features for the Free Association community by contributing your OpenRouter API key.
			</p>
		</div>
		
		<!-- Pool Stats -->
		{#if poolStats && !loadingStats}
			<div class="stats-card">
				<h2>Current Key Pool Status</h2>
				<div class="stats-grid">
					<div class="stat">
						<div class="stat-value">{poolStats.totalKeys}</div>
						<div class="stat-label">Total Keys</div>
					</div>
					<div class="stat">
						<div class="stat-value">{poolStats.healthyKeys}</div>
						<div class="stat-label">Healthy Keys</div>
					</div>
					<div class="stat">
						<div class="stat-value">{poolStats.stats?.successRate || '0%'}</div>
						<div class="stat-label">Success Rate</div>
					</div>
					<div class="stat">
						<div class="stat-value">{poolStats.stats?.totalCostToday || '$0.00'}</div>
						<div class="stat-label">Cost Today</div>
					</div>
				</div>
			</div>
		{/if}
		
		<!-- Info Section -->
		<div class="info-section">
			<h3>Why Donate?</h3>
			<ul>
				<li><strong>Support the Community:</strong> Your key helps everyone access AI-powered features like quest generation, capacity recommendations, and recognition analysis.</li>
				<li><strong>Shared Cost:</strong> By pooling keys, we distribute API costs across the community.</li>
				<li><strong>Transparent Usage:</strong> Track how keys are used and monitor pool health.</li>
				<li><strong>Revocable:</strong> You can revoke your key at any time through OpenRouter.</li>
			</ul>
			
			<h3>How It Works</h3>
			<ol>
				<li>Get a free or paid API key from <a href="https://openrouter.ai/keys" target="_blank" rel="noopener">OpenRouter</a></li>
				<li>Enter your key below (we'll validate it first)</li>
				<li>Choose whether to donate anonymously or with attribution</li>
				<li>Your key joins the pool and helps power AI features</li>
			</ol>
			
			<div class="warning-box">
				<strong>⚠️ Important:</strong> Only donate keys you're comfortable sharing with the community. 
				Keys are stored securely and only used for Free Association AI features. You can always revoke 
				your key through OpenRouter's dashboard.
			</div>
		</div>
		
		<!-- Donation Form -->
		<div class="donation-form">
			<h2>Donate Your Key</h2>
			
			{#if success}
				<div class="alert alert-success">
					{success}
				</div>
			{/if}
			
			{#if error}
				<div class="alert alert-error">
					{error}
				</div>
			{/if}
			
			<form on:submit|preventDefault={handleDonate}>
				<!-- API Key Input -->
				<div class="form-group">
					<label for="apiKey">OpenRouter API Key *</label>
					<div class="input-with-toggle">
						<input
							type={showKey ? 'text' : 'password'}
							id="apiKey"
							bind:value={apiKey}
							placeholder="sk-or-v1-..."
							disabled={isSubmitting}
							required
						/>
						<button
							type="button"
							class="toggle-visibility"
							on:click={() => showKey = !showKey}
							title={showKey ? 'Hide key' : 'Show key'}
						>
							{showKey ? '🙈' : '👁️'}
						</button>
					</div>
					<small>Get your key from <a href="https://openrouter.ai/keys" target="_blank" rel="noopener">openrouter.ai/keys</a></small>
				</div>
				
				<!-- Anonymous Checkbox -->
				<div class="form-group">
					<label class="checkbox-label">
						<input
							type="checkbox"
							bind:checked={isAnonymous}
							disabled={isSubmitting}
						/>
						<span>Donate anonymously (don't show my name)</span>
					</label>
				</div>
				
				<!-- Donor Name -->
				{#if !isAnonymous}
					<div class="form-group">
						<label for="donorName">Your Name (optional)</label>
						<input
							type="text"
							id="donorName"
							bind:value={donorName}
							placeholder={$holsterUserAlias || 'Your name'}
							disabled={isSubmitting}
							maxlength="100"
						/>
						<small>This will be shown in the pool statistics</small>
					</div>
				{/if}
				
				<!-- Submit Button -->
				<button
					type="submit"
					class="submit-btn"
					disabled={isSubmitting || !apiKey.trim()}
				>
					{isSubmitting ? '🔄 Validating...' : '🎁 Donate Key'}
				</button>
			</form>
		</div>
		
		<!-- Get a Key Section -->
		<div class="get-key-section">
			<h3>Don't Have an OpenRouter Key?</h3>
			<p>OpenRouter provides access to multiple AI models through a single API. They offer:</p>
			<ul>
				<li><strong>Free tier:</strong> $0.50 in free credits to get started</li>
				<li><strong>Pay-as-you-go:</strong> Only pay for what you use</li>
				<li><strong>Multiple models:</strong> Access to Claude, GPT-4, and more</li>
			</ul>
			<a href="https://openrouter.ai/keys" target="_blank" rel="noopener" class="cta-button">
				Get Your OpenRouter Key →
			</a>
		</div>
	</div>
</div>

<style>
	.container {
		min-height: 100vh;
		background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
		padding: 2rem 1rem;
	}
	
	.content {
		max-width: 800px;
		margin: 0 auto;
	}
	
	.header {
		text-align: center;
		color: white;
		margin-bottom: 2rem;
	}
	
	.header h1 {
		font-size: 2.5rem;
		font-weight: bold;
		margin-bottom: 0.5rem;
	}
	
	.subtitle {
		font-size: 1.125rem;
		opacity: 0.9;
	}
	
	.stats-card {
		background: white;
		border-radius: 1rem;
		padding: 2rem;
		margin-bottom: 2rem;
		box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1);
	}
	
	.stats-card h2 {
		margin: 0 0 1.5rem 0;
		font-size: 1.5rem;
		color: #333;
	}
	
	.stats-grid {
		display: grid;
		grid-template-columns: repeat(auto-fit, minmax(150px, 1fr));
		gap: 1.5rem;
	}
	
	.stat {
		text-align: center;
	}
	
	.stat-value {
		font-size: 2rem;
		font-weight: bold;
		color: #667eea;
		margin-bottom: 0.5rem;
	}
	
	.stat-label {
		font-size: 0.875rem;
		color: #666;
		text-transform: uppercase;
		letter-spacing: 0.05em;
	}
	
	.info-section, .donation-form, .get-key-section {
		background: white;
		border-radius: 1rem;
		padding: 2rem;
		margin-bottom: 2rem;
		box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1);
	}
	
	.info-section h3 {
		color: #667eea;
		margin-top: 1.5rem;
		margin-bottom: 1rem;
	}
	
	.info-section h3:first-child {
		margin-top: 0;
	}
	
	.info-section ul, .info-section ol {
		margin: 1rem 0;
		padding-left: 1.5rem;
	}
	
	.info-section li {
		margin: 0.5rem 0;
		line-height: 1.6;
	}
	
	.warning-box {
		background: #fff3cd;
		border: 1px solid #ffc107;
		border-radius: 0.5rem;
		padding: 1rem;
		margin-top: 1.5rem;
		color: #856404;
		line-height: 1.6;
	}
	
	.donation-form h2 {
		margin: 0 0 1.5rem 0;
		font-size: 1.5rem;
		color: #333;
	}
	
	.form-group {
		margin-bottom: 1.5rem;
	}
	
	.form-group label {
		display: block;
		font-weight: 600;
		margin-bottom: 0.5rem;
		color: #333;
	}
	
	.form-group input[type="text"],
	.form-group input[type="password"] {
		width: 100%;
		padding: 0.75rem;
		border: 2px solid #e0e0e0;
		border-radius: 0.5rem;
		font-size: 1rem;
		transition: border-color 0.2s;
	}
	
	.form-group input:focus {
		outline: none;
		border-color: #667eea;
	}
	
	.form-group input:disabled {
		background: #f5f5f5;
		cursor: not-allowed;
	}
	
	.input-with-toggle {
		position: relative;
		display: flex;
		align-items: center;
	}
	
	.input-with-toggle input {
		flex: 1;
		padding-right: 3rem;
	}
	
	.toggle-visibility {
		position: absolute;
		right: 0.5rem;
		background: none;
		border: none;
		font-size: 1.25rem;
		cursor: pointer;
		padding: 0.5rem;
	}
	
	.checkbox-label {
		display: flex;
		align-items: center;
		cursor: pointer;
		font-weight: normal !important;
	}
	
	.checkbox-label input {
		margin-right: 0.5rem;
		cursor: pointer;
	}
	
	.form-group small {
		display: block;
		margin-top: 0.5rem;
		color: #666;
		font-size: 0.875rem;
	}
	
	.form-group small a {
		color: #667eea;
		text-decoration: none;
	}
	
	.form-group small a:hover {
		text-decoration: underline;
	}
	
	.alert {
		padding: 1rem;
		border-radius: 0.5rem;
		margin-bottom: 1.5rem;
	}
	
	.alert-success {
		background: #d4edda;
		border: 1px solid #c3e6cb;
		color: #155724;
	}
	
	.alert-error {
		background: #f8d7da;
		border: 1px solid #f5c6cb;
		color: #721c24;
	}
	
	.submit-btn {
		width: 100%;
		padding: 1rem;
		background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
		color: white;
		border: none;
		border-radius: 0.5rem;
		font-size: 1.125rem;
		font-weight: 600;
		cursor: pointer;
		transition: transform 0.2s, box-shadow 0.2s;
	}
	
	.submit-btn:hover:not(:disabled) {
		transform: translateY(-2px);
		box-shadow: 0 4px 12px rgba(102, 126, 234, 0.4);
	}
	
	.submit-btn:disabled {
		opacity: 0.6;
		cursor: not-allowed;
	}
	
	.get-key-section h3 {
		color: #667eea;
		margin-bottom: 1rem;
	}
	
	.get-key-section ul {
		margin: 1rem 0 1.5rem 1.5rem;
	}
	
	.get-key-section li {
		margin: 0.5rem 0;
	}
	
	.cta-button {
		display: inline-block;
		padding: 1rem 2rem;
		background: #667eea;
		color: white;
		text-decoration: none;
		border-radius: 0.5rem;
		font-weight: 600;
		transition: all 0.2s;
	}
	
	.cta-button:hover {
		background: #764ba2;
		transform: translateY(-2px);
		box-shadow: 0 4px 12px rgba(102, 126, 234, 0.4);
	}
	
	@media (max-width: 768px) {
		.header h1 {
			font-size: 2rem;
		}
		
		.stats-grid {
			grid-template-columns: repeat(2, 1fr);
		}
		
		.container {
			padding: 1rem 0.5rem;
		}
		
		.info-section, .donation-form, .get-key-section {
			padding: 1.5rem;
		}
	}
</style>

