<script lang="ts">
	/**
	 * @component ActionModal
	 * Modal overlay for actions (challenge, comment, support)
	 * 
	 * @prop {boolean} isOpen - Whether modal is open
	 * @prop {Function} onClose - Callback when modal closes
	 */
	
	import { onMount } from 'svelte';
	
	let { isOpen = $bindable(false), onClose, children }: {
		isOpen?: boolean;
		onClose?: () => void;
		children?: any;
	} = $props();
	
	function handleOverlayClick(e: MouseEvent) {
		if (e.target === e.currentTarget) {
			close();
		}
	}
	
	function handleKeyDown(e: KeyboardEvent) {
		if (e.key === 'Escape') {
			close();
		}
	}
	
	function close() {
		isOpen = false;
		onClose?.();
	}
	
	onMount(() => {
		const cleanup = () => {
			document.removeEventListener('keydown', handleKeyDown);
		};
		
		if (isOpen) {
			document.addEventListener('keydown', handleKeyDown);
		}
		
		return cleanup;
	});
</script>

{#if isOpen}
	<div 
		class="modal-overlay" 
		onclick={handleOverlayClick}
		onkeydown={(e) => e.key === 'Escape' && close()}
		role="button"
		tabindex="0"
		aria-label="Close modal"
	>
		<div class="modal-content" role="dialog" aria-modal="true">
			{@render children?.()}
		</div>
	</div>
{/if}

<style>
	.modal-overlay {
		position: fixed;
		top: 0;
		left: 0;
		right: 0;
		bottom: 0;
		background: rgba(0, 0, 0, 0.6);
		display: flex;
		align-items: center;
		justify-content: center;
		z-index: 1000;
		padding: 1rem;
		backdrop-filter: blur(4px);
		animation: fadeIn 0.2s ease-out;
	}
	
	@keyframes fadeIn {
		from {
			opacity: 0;
		}
		to {
			opacity: 1;
		}
	}
	
	.modal-content {
		max-width: 90vw;
		max-height: 90vh;
		overflow-y: auto;
		animation: slideUp 0.3s ease-out;
	}
	
	@keyframes slideUp {
		from {
			opacity: 0;
			transform: translateY(2rem);
		}
		to {
			opacity: 1;
			transform: translateY(0);
		}
	}
	
	/* Scrollbar styling */
	.modal-content::-webkit-scrollbar {
		width: 0.5rem;
	}
	
	.modal-content::-webkit-scrollbar-track {
		background: transparent;
	}
	
	.modal-content::-webkit-scrollbar-thumb {
		background: rgba(0, 0, 0, 0.2);
		border-radius: 0.25rem;
	}
</style>

