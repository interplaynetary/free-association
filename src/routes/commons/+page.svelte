<script lang="ts">
	import { onMount } from 'svelte';
	import { Visualization, initializeAllocationStores, myCommitmentStore, myRecognitionWeightsStore } from '$lib/commons';
	import { holsterUserPub } from '$lib/state/holster.svelte';
	
	let initialized = $state(false);
	
	onMount(async () => {
		// Initialize allocation stores if user is authenticated
		if ($holsterUserPub) {
			try {
				initializeAllocationStores();
				initialized = true;
				
				// Optionally set some demo data
				await setDemoData();
			} catch (error) {
				console.error('[CIRCLES-DEMO] Failed to initialize:', error);
			}
		}
	});
	
	async function setDemoData() {
		// Example: Set demo commitment data
		try {
			await myCommitmentStore.set({
				capacity_slots: [
					{
						id: 'demo-capacity-1',
						quantity: 100,
						location_type: 'remote',
						online_link: 'https://meet.example.com',
						start_date: '2025-10-01',
						end_date: '2025-12-31'
					}
				],
				need_slots: [
					{
						id: 'demo-need-1',
						quantity: 50,
						location_type: 'remote',
						start_date: '2025-10-01',
						end_date: '2025-11-30'
					}
				],
				timestamp: Date.now()
			});
			
			await myRecognitionWeightsStore.set({
				// Add recognition weights for demo participants
				// In a real app, these would come from user selections
			});
			
			console.log('[CIRCLES-DEMO] Demo data loaded');
		} catch (error) {
			console.error('[CIRCLES-DEMO] Failed to set demo data:', error);
		}
	}
</script>

<div class="circles-demo">
	<div class="header-section">
		<h1>Commons Demo</h1>
		<p class="subtitle">
			Real-time visualization of the slot-native two-tier allocation algorithm
		</p>
		
		{#if !$holsterUserPub}
			<div class="auth-notice">
				<svg xmlns="http://www.w3.org/2000/svg" width="24" height="24" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round">
					<circle cx="12" cy="12" r="10"></circle>
					<line x1="12" y1="8" x2="12" y2="12"></line>
					<line x1="12" y1="16" x2="12.01" y2="16"></line>
				</svg>
				<div>
					<h3>Authentication Required</h3>
					<p>Please log in with Holster to use the circle visualization.</p>
				</div>
			</div>
		{:else if !initialized}
			<div class="loading-notice">
				<div class="spinner"></div>
				<p>Initializing allocation stores...</p>
			</div>
		{:else}
			<div class="info-box">
				<h3>About This Demo</h3>
				<p>
					This visualization shows the living denominator system in action. Circles represent 
					participants with capacity (outer pie slices showing who they allocate to) and needs 
					(inner pie slices showing who provides to them).
				</p>
				<ul>
					<li><strong>Mutual Recognition (Blue/Green):</strong> Priority tier based on bilateral relationships</li>
					<li><strong>Generous Giving (Amber/Lime):</strong> One-way recognition for leftover capacity</li>
					<li><strong>Dashed Lines:</strong> Mutual recognition connections</li>
					<li><strong>Circle Size:</strong> Total capacity or need magnitude</li>
				</ul>
			</div>
		{/if}
	</div>
	
	{#if $holsterUserPub && initialized}
		<Visualization width={900} height={700} />
		
		<div class="dev-info">
			<h3>Developer Info</h3>
			<p><strong>Your Public Key:</strong> <code>{$holsterUserPub.slice(0, 20)}...</code></p>
			<p><strong>Stores:</strong> myCommitmentStore, myAllocationStateStore, networkCommitments</p>
			<p><strong>Algorithm:</strong> Slot-native two-tier allocation with adaptive damping</p>
			
			<details>
				<summary>How to add more participants</summary>
				<p>
					To see network effects, have other users:
				</p>
				<ol>
					<li>Log in and initialize their stores</li>
					<li>Set recognition weights pointing to you</li>
					<li>Publish commitments with capacity or needs</li>
					<li>The visualization will automatically update via P2P sync</li>
				</ol>
			</details>
		</div>
	{/if}
</div>

<style>
	.circles-demo {
		min-height: 100vh;
		background: linear-gradient(135deg, #0f172a 0%, #1e293b 50%, #0f172a 100%);
		padding: 2rem;
	}
	
	.header-section {
		max-width: 1200px;
		margin: 0 auto 2rem;
	}
	
	h1 {
		font-size: 2.5rem;
		font-weight: bold;
		color: white;
		margin-bottom: 0.5rem;
		text-align: center;
	}
	
	.subtitle {
		font-size: 1.125rem;
		color: #cbd5e1;
		text-align: center;
		margin-bottom: 2rem;
	}
	
	.auth-notice,
	.loading-notice,
	.info-box {
		background: rgba(30, 41, 59, 0.5);
		border: 1px solid #475569;
		border-radius: 0.75rem;
		padding: 1.5rem;
		margin-bottom: 1.5rem;
	}
	
	.auth-notice {
		display: flex;
		gap: 1rem;
		align-items: flex-start;
		border-color: #f59e0b;
	}
	
	.auth-notice svg {
		color: #f59e0b;
		flex-shrink: 0;
	}
	
	.auth-notice h3 {
		color: white;
		font-weight: 600;
		margin: 0 0 0.5rem;
	}
	
	.auth-notice p {
		color: #cbd5e1;
		margin: 0;
	}
	
	.loading-notice {
		display: flex;
		align-items: center;
		gap: 1rem;
		justify-content: center;
	}
	
	.spinner {
		width: 24px;
		height: 24px;
		border: 3px solid rgba(255, 255, 255, 0.3);
		border-radius: 50%;
		border-top-color: #0ea5e9;
		animation: spin 0.8s linear infinite;
	}
	
	@keyframes spin {
		to { transform: rotate(360deg); }
	}
	
	.loading-notice p {
		color: #cbd5e1;
		margin: 0;
	}
	
	.info-box {
		border-color: #0ea5e9;
	}
	
	.info-box h3 {
		color: white;
		font-weight: 600;
		margin: 0 0 0.75rem;
	}
	
	.info-box p {
		color: #cbd5e1;
		margin: 0 0 0.75rem;
		line-height: 1.6;
	}
	
	.info-box ul {
		color: #cbd5e1;
		margin: 0;
		padding-left: 1.5rem;
	}
	
	.info-box li {
		margin-bottom: 0.5rem;
		line-height: 1.6;
	}
	
	.info-box strong {
		color: white;
	}
	
	.dev-info {
		max-width: 1200px;
		margin: 2rem auto 0;
		background: rgba(30, 41, 59, 0.5);
		border: 1px solid #475569;
		border-radius: 0.75rem;
		padding: 1.5rem;
	}
	
	.dev-info h3 {
		color: white;
		font-weight: 600;
		margin: 0 0 1rem;
	}
	
	.dev-info p {
		color: #cbd5e1;
		margin: 0 0 0.5rem;
		line-height: 1.6;
	}
	
	.dev-info code {
		background: rgba(0, 0, 0, 0.3);
		padding: 0.25rem 0.5rem;
		border-radius: 0.25rem;
		font-family: 'Courier New', monospace;
		color: #0ea5e9;
	}
	
	.dev-info details {
		margin-top: 1rem;
		border-top: 1px solid #475569;
		padding-top: 1rem;
	}
	
	.dev-info summary {
		color: #0ea5e9;
		cursor: pointer;
		font-weight: 600;
		margin-bottom: 0.75rem;
	}
	
	.dev-info summary:hover {
		color: #38bdf8;
	}
	
	.dev-info ol {
		color: #cbd5e1;
		padding-left: 1.5rem;
		margin: 0.5rem 0 0;
	}
	
	.dev-info li {
		margin-bottom: 0.5rem;
		line-height: 1.6;
	}
</style>

