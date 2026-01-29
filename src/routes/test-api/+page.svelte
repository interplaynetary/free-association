<script lang="ts">
	import { onMount } from 'svelte';
	
	interface TestResult {
		name: string;
		status: 'pending' | 'success' | 'error';
		message: string;
		duration?: number;
	}
	
	let tests: TestResult[] = $state([
		{ name: 'Health Check', status: 'pending', message: 'Not tested' },
		{ name: 'Authentication (JWT)', status: 'pending', message: 'Not tested' },
		{ name: 'OpenRouter Keys', status: 'pending', message: 'Not tested' },
		{ name: 'LLM Routing', status: 'pending', message: 'Not tested' },
		{ name: 'Environment Config', status: 'pending', message: 'Not tested' }
	]);
	
	let testing = $state(false);
	let masterApiKey = $state('');
	
	async function runTests() {
		testing = true;
		
		// Test 1: Health Check
		await testHealthCheck();
		
		// Test 2: Authentication
		await testAuthentication();
		
		// Test 3: OpenRouter Keys
		await testOpenRouterKeys();
		
		// Test 4: LLM Routing
		await testLLMRouting();
		
		// Test 5: Environment Config
		await testEnvironmentConfig();
		
		testing = false;
	}
	
	async function testHealthCheck() {
		const start = Date.now();
		try {
			const response = await fetch('/api/health');
			const data = await response.json();
			
			if (response.ok && data.status === 'ok') {
				tests[0] = {
					name: 'Health Check',
					status: 'success',
					message: `✓ API is healthy (${data.uptime || 'N/A'})`,
					duration: Date.now() - start
				};
			} else {
				tests[0] = {
					name: 'Health Check',
					status: 'error',
					message: `✗ Unexpected response: ${JSON.stringify(data)}`,
					duration: Date.now() - start
				};
			}
		} catch (error: any) {
			tests[0] = {
				name: 'Health Check',
				status: 'error',
				message: `✗ ${error.message}`,
				duration: Date.now() - start
			};
		}
	}
	
	async function testAuthentication() {
		const start = Date.now();
		try {
			// Test with master API key if provided
			const headers: Record<string, string> = {
				'Content-Type': 'application/json'
			};
			
			if (masterApiKey) {
				headers['Authorization'] = `Bearer ${masterApiKey}`;
			}
			
			const response = await fetch('/api/keys/donate', { headers });
			
			if (response.ok) {
				tests[1] = {
					name: 'Authentication (JWT)',
					status: 'success',
					message: '✓ Authentication working',
					duration: Date.now() - start
				};
			} else if (response.status === 401) {
				tests[1] = {
					name: 'Authentication (JWT)',
					status: 'error',
					message: '✗ JWT_SECRET or MASTER_API_KEY not configured',
					duration: Date.now() - start
				};
			} else {
				tests[1] = {
					name: 'Authentication (JWT)',
					status: 'error',
					message: `✗ Status ${response.status}`,
					duration: Date.now() - start
				};
			}
		} catch (error: any) {
			tests[1] = {
				name: 'Authentication (JWT)',
				status: 'error',
				message: `✗ ${error.message}`,
				duration: Date.now() - start
			};
		}
	}
	
	async function testOpenRouterKeys() {
		const start = Date.now();
		try {
			const headers: Record<string, string> = {
				'Content-Type': 'application/json'
			};
			
			if (masterApiKey) {
				headers['Authorization'] = `Bearer ${masterApiKey}`;
			}
			
			const response = await fetch('/api/keys/health/openrouter', { headers });
			const data = await response.json();
			
			if (response.ok) {
				const totalKeys = data.totalKeys || 0;
				const healthyKeys = data.health?.healthy || 0;
				
				if (totalKeys > 0) {
					tests[2] = {
						name: 'OpenRouter Keys',
						status: 'success',
						message: `✓ ${healthyKeys}/${totalKeys} keys healthy`,
						duration: Date.now() - start
					};
				} else {
					tests[2] = {
						name: 'OpenRouter Keys',
						status: 'error',
						message: '✗ No OPENROUTER_KEYS configured',
						duration: Date.now() - start
					};
				}
			} else {
				tests[2] = {
					name: 'OpenRouter Keys',
					status: 'error',
					message: `✗ Status ${response.status}`,
					duration: Date.now() - start
				};
			}
		} catch (error: any) {
			tests[2] = {
				name: 'OpenRouter Keys',
				status: 'error',
				message: `✗ ${error.message}`,
				duration: Date.now() - start
			};
		}
	}
	
	async function testLLMRouting() {
		const start = Date.now();
		try {
			const headers: Record<string, string> = {
				'Content-Type': 'application/json'
			};
			
			if (masterApiKey) {
				headers['Authorization'] = `Bearer ${masterApiKey}`;
			}
			
			const response = await fetch('/api/llm/route', {
				method: 'POST',
				headers,
				body: JSON.stringify({
					prompt: 'Test',
					maxTokens: 10
				})
			});
			
			if (response.ok) {
				const data = await response.json();
				tests[3] = {
					name: 'LLM Routing',
					status: 'success',
					message: `✓ Routing works (model: ${data.model || 'unknown'})`,
					duration: Date.now() - start
				};
			} else if (response.status === 401) {
				tests[3] = {
					name: 'LLM Routing',
					status: 'error',
					message: '✗ Authentication required',
					duration: Date.now() - start
				};
			} else {
				tests[3] = {
					name: 'LLM Routing',
					status: 'error',
					message: `✗ Status ${response.status}`,
					duration: Date.now() - start
				};
			}
		} catch (error: any) {
			tests[3] = {
				name: 'LLM Routing',
				status: 'error',
				message: `✗ ${error.message}`,
				duration: Date.now() - start
			};
		}
	}
	
	async function testEnvironmentConfig() {
		const start = Date.now();
		try {
			// Check if we're in production
			const isProd = window.location.hostname === 'free.playnet.lol';
			const origin = window.location.origin;
			
			tests[4] = {
				name: 'Environment Config',
				status: 'success',
				message: `✓ ${isProd ? 'Production' : 'Development'} (${origin})`,
				duration: Date.now() - start
			};
		} catch (error: any) {
			tests[4] = {
				name: 'Environment Config',
				status: 'error',
				message: `✗ ${error.message}`,
				duration: Date.now() - start
			};
		}
	}
	
	function getStatusColor(status: string) {
		switch (status) {
			case 'success': return 'var(--color-success, #22c55e)';
			case 'error': return 'var(--color-error, #ef4444)';
			default: return 'var(--color-muted, #6b7280)';
		}
	}
	
	function getStatusIcon(status: string) {
		switch (status) {
			case 'success': return '✓';
			case 'error': return '✗';
			default: return '○';
		}
	}
</script>

<div class="test-page">
	<div class="header">
		<h1>🧪 API Test Suite</h1>
		<p>Verify your production environment is configured correctly</p>
	</div>
	
	<div class="auth-section">
		<label for="api-key">
			Master API Key (optional - for authenticated tests):
		</label>
		<input
			id="api-key"
			type="password"
			bind:value={masterApiKey}
			placeholder="Enter MASTER_API_KEY to test authenticated endpoints"
		/>
	</div>
	
	<button
		class="run-button"
		onclick={runTests}
		disabled={testing}
	>
		{testing ? '⏳ Running Tests...' : '▶ Run All Tests'}
	</button>
	
	<div class="results">
		{#each tests as test}
			<div class="test-result" style="border-left-color: {getStatusColor(test.status)}">
				<div class="test-header">
					<span class="test-icon" style="color: {getStatusColor(test.status)}">
						{getStatusIcon(test.status)}
					</span>
					<span class="test-name">{test.name}</span>
					{#if test.duration}
						<span class="test-duration">{test.duration}ms</span>
					{/if}
				</div>
				<div class="test-message">{test.message}</div>
			</div>
		{/each}
	</div>
	
	<div class="help-section">
		<h3>📋 Troubleshooting</h3>
		<ul>
			<li><strong>Health Check fails:</strong> API server not running or not accessible</li>
			<li><strong>Authentication fails:</strong> JWT_SECRET or MASTER_API_KEY not set in App Platform</li>
			<li><strong>OpenRouter Keys fails:</strong> OPENROUTER_KEYS not configured or invalid</li>
			<li><strong>LLM Routing fails:</strong> Check authentication and OpenRouter keys</li>
		</ul>
		
		<h3>🔧 How to Fix</h3>
		<ol>
			<li>Go to App Platform → Settings → web component → Environment Variables</li>
			<li>Set required variables (JWT_SECRET, MASTER_API_KEY, OPENROUTER_KEYS)</li>
			<li>Enable "Encrypt" for sensitive values</li>
			<li>Redeploy your app</li>
			<li>Run tests again</li>
		</ol>
	</div>
</div>

<style>
	.test-page {
		max-width: 800px;
		margin: 2rem auto;
		padding: 2rem;
		font-family: system-ui, -apple-system, sans-serif;
	}
	
	.header {
		text-align: center;
		margin-bottom: 2rem;
	}
	
	.header h1 {
		margin: 0 0 0.5rem 0;
		font-size: 2rem;
		color: var(--color-text, #1f2937);
	}
	
	.header p {
		margin: 0;
		color: var(--color-muted, #6b7280);
	}
	
	.auth-section {
		margin-bottom: 1.5rem;
	}
	
	.auth-section label {
		display: block;
		margin-bottom: 0.5rem;
		font-weight: 500;
		color: var(--color-text, #1f2937);
	}
	
	.auth-section input {
		width: 100%;
		padding: 0.75rem;
		border: 1px solid var(--color-border, #d1d5db);
		border-radius: 0.5rem;
		font-family: monospace;
		font-size: 0.875rem;
	}
	
	.run-button {
		width: 100%;
		padding: 1rem;
		background: var(--color-primary, #3b82f6);
		color: white;
		border: none;
		border-radius: 0.5rem;
		font-size: 1rem;
		font-weight: 600;
		cursor: pointer;
		transition: background 0.2s;
		margin-bottom: 2rem;
	}
	
	.run-button:hover:not(:disabled) {
		background: var(--color-primary-dark, #2563eb);
	}
	
	.run-button:disabled {
		opacity: 0.6;
		cursor: not-allowed;
	}
	
	.results {
		display: flex;
		flex-direction: column;
		gap: 1rem;
		margin-bottom: 2rem;
	}
	
	.test-result {
		padding: 1rem;
		background: var(--color-bg, #f9fafb);
		border-left: 4px solid;
		border-radius: 0.5rem;
	}
	
	.test-header {
		display: flex;
		align-items: center;
		gap: 0.75rem;
		margin-bottom: 0.5rem;
	}
	
	.test-icon {
		font-size: 1.25rem;
		font-weight: bold;
	}
	
	.test-name {
		flex: 1;
		font-weight: 600;
		color: var(--color-text, #1f2937);
	}
	
	.test-duration {
		font-size: 0.875rem;
		color: var(--color-muted, #6b7280);
		font-family: monospace;
	}
	
	.test-message {
		margin-left: 2rem;
		color: var(--color-text-secondary, #4b5563);
		font-family: monospace;
		font-size: 0.875rem;
	}
	
	.help-section {
		margin-top: 3rem;
		padding: 1.5rem;
		background: var(--color-info-bg, #eff6ff);
		border-radius: 0.5rem;
	}
	
	.help-section h3 {
		margin: 0 0 1rem 0;
		color: var(--color-text, #1f2937);
	}
	
	.help-section ul,
	.help-section ol {
		margin: 0;
		padding-left: 1.5rem;
	}
	
	.help-section li {
		margin-bottom: 0.5rem;
		color: var(--color-text-secondary, #4b5563);
	}
	
	.help-section strong {
		color: var(--color-text, #1f2937);
	}
</style>
