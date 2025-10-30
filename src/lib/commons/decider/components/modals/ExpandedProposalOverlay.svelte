<script lang="ts">
	/**
	 * @component ExpandedProposalOverlay
	 * Full-screen overlay for viewing a proposal in detail
	 * 
	 * @prop {boolean} isOpen - Whether overlay is open
	 * @prop {Function} onClose - Callback when closed
	 */
	
	import { onMount } from 'svelte';
	
	let { isOpen = $bindable(false), onClose, children }: {
		isOpen?: boolean;
		onClose?: () => void;
		children?: any;
	} = $props();
	
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
	<div class="expanded-overlay">
		<div class="header">
			<h2 class="title">Proposal Details</h2>
			<button class="close-btn" onclick={close} aria-label="Close">
				✕
			</button>
		</div>
		
		<div class="content">
			{@render children?.()}
		</div>
	</div>
{/if}

<style>
	.expanded-overlay {
		position: fixed;
		top: 0;
		left: 0;
		right: 0;
		bottom: 0;
		background: white;
		z-index: 1000;
		display: flex;
		flex-direction: column;
		animation: slideIn 0.3s ease-out;
	}
	
	@keyframes slideIn {
		from {
			opacity: 0;
			transform: translateX(100%);
		}
		to {
			opacity: 1;
			transform: translateX(0);
		}
	}
	
	.header {
		display: flex;
		justify-content: space-between;
		align-items: center;
		padding: 1.5rem;
		border-bottom: 2px solid var(--border-color, #e0e0e0);
		background: white;
		position: sticky;
		top: 0;
		z-index: 10;
	}
	
	.title {
		margin: 0;
		font-size: 1.5rem;
		font-weight: 600;
		color: var(--text-primary, #333);
	}
	
	.close-btn {
		width: 2.5rem;
		height: 2.5rem;
		border: none;
		background: var(--bg-muted, #f0f0f0);
		border-radius: 50%;
		font-size: 1.25rem;
		cursor: pointer;
		transition: all 0.2s;
		display: flex;
		align-items: center;
		justify-content: center;
	}
	
	.close-btn:hover {
		background: var(--primary-color, #667eea);
		color: white;
		transform: rotate(90deg);
	}
	
	.content {
		flex: 1;
		overflow-y: auto;
		padding: 1.5rem;
	}
	
	.content::-webkit-scrollbar {
		width: 0.5rem;
	}
	
	.content::-webkit-scrollbar-track {
		background: transparent;
	}
	
	.content::-webkit-scrollbar-thumb {
		background: rgba(0, 0, 0, 0.2);
		border-radius: 0.25rem;
	}
</style>

