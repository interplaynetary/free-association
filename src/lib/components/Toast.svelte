<script lang="ts">
	import { toastStore, type Toast } from '$lib/stores/toast.svelte';
	
	// Subscribe to toast store
	let toasts = $derived(toastStore.toasts);
</script>

<div class="toast-container">
	{#each toasts as toast (toast.id)}
		<div 
			class="toast toast-{toast.type}"
			role="alert"
		>
			<div class="toast-content">
				<span class="toast-icon">
					{#if toast.type === 'success'}✓{/if}
					{#if toast.type === 'error'}✕{/if}
					{#if toast.type === 'warning'}⚠{/if}
					{#if toast.type === 'info'}ℹ{/if}
				</span>
				<span class="toast-message">{toast.message}</span>
			</div>
			<button
				class="toast-close"
				onclick={() => toastStore.remove(toast.id)}
				aria-label="Close notification"
			>
				×
			</button>
		</div>
	{/each}
</div>

<style>
	.toast-container {
		position: fixed;
		top: 80px;
		left: 50%;
		transform: translateX(-50%);
		z-index: 100000;
		display: flex;
		flex-direction: column;
		gap: 8px;
		pointer-events: none;
		max-width: 90vw;
		width: auto;
	}

	.toast {
		display: flex;
		align-items: center;
		gap: 12px;
		padding: 12px 16px;
		border-radius: 8px;
		box-shadow: 0 4px 12px rgba(0, 0, 0, 0.15);
		pointer-events: auto;
		animation: slideIn 0.3s ease-out;
		min-width: 280px;
		max-width: 500px;
	}

	@keyframes slideIn {
		from {
			opacity: 0;
			transform: translateY(-20px);
		}
		to {
			opacity: 1;
			transform: translateY(0);
		}
	}

	.toast-success {
		background: #4caf50;
		color: white;
	}

	.toast-error {
		background: #f44336;
		color: white;
	}

	.toast-warning {
		background: #ff9800;
		color: white;
	}

	.toast-info {
		background: #2196f3;
		color: white;
	}

	.toast-content {
		display: flex;
		align-items: center;
		gap: 8px;
		flex: 1;
	}

	.toast-icon {
		font-size: 16px;
		font-weight: bold;
		flex-shrink: 0;
	}

	.toast-message {
		font-size: 14px;
		line-height: 1.4;
	}

	.toast-close {
		background: transparent;
		border: none;
		color: white;
		font-size: 24px;
		line-height: 1;
		cursor: pointer;
		padding: 0 4px;
		opacity: 0.8;
		transition: opacity 0.2s;
		flex-shrink: 0;
	}

	.toast-close:hover {
		opacity: 1;
	}

	@media (max-width: 768px) {
		.toast-container {
			top: 70px;
			max-width: calc(100vw - 20px);
		}

		.toast {
			min-width: 260px;
		}
	}
</style>

