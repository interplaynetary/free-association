<script lang="ts">
	/**
	 * @component LoadingState
	 * Loading indicator with optional message
	 * 
	 * @prop {string} message - Optional loading message
	 * @prop {boolean} inline - Inline variant (smaller)
	 */
	
	let { message = 'Loading...', inline = false }: {
		message?: string;
		inline?: boolean;
	} = $props();
</script>

<div class="loading-state" class:inline>
	<div class="spinner"></div>
	<p class="message">{message}</p>
</div>

<style>
	.loading-state {
		display: flex;
		flex-direction: column;
		align-items: center;
		justify-content: center;
		gap: 1rem;
		padding: 3rem 1.5rem;
	}
	
	.loading-state.inline {
		padding: 1rem;
		flex-direction: row;
	}
	
	.spinner {
		width: 2.5rem;
		height: 2.5rem;
		border: 3px solid var(--bg-muted, #f3f3f3);
		border-top: 3px solid var(--primary-color, #667eea);
		border-radius: 50%;
		animation: spin 1s linear infinite;
	}
	
	.inline .spinner {
		width: 1.25rem;
		height: 1.25rem;
		border-width: 2px;
	}
	
	@keyframes spin {
		0% { transform: rotate(0deg); }
		100% { transform: rotate(360deg); }
	}
	
	.message {
		margin: 0;
		color: var(--text-muted, #666);
		font-size: 0.9375rem;
	}
	
	.inline .message {
		font-size: 0.875rem;
	}
</style>

