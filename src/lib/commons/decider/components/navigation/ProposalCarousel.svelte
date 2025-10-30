<script lang="ts">
	/**
	 * @component ProposalCarousel
	 * Horizontal carousel for navigating between proposals
	 * 
	 * @prop {Array} proposals - Array of proposals
	 * @prop {number} selectedIndex - Currently selected proposal index
	 * @prop {Function} onSelect - Callback when proposal is selected
	 */
	
	interface Props {
		proposals: Array<any>;
		selectedIndex?: number;
		onSelect?: (index: number) => void;
	}
	
	let { proposals, selectedIndex = $bindable(0), onSelect }: Props = $props();
	
	function handlePrevious() {
		const newIndex = Math.max(0, selectedIndex - 1);
		selectedIndex = newIndex;
		onSelect?.(newIndex);
	}
	
	function handleNext() {
		const newIndex = Math.min(proposals.length - 1, selectedIndex + 1);
		selectedIndex = newIndex;
		onSelect?.(newIndex);
	}
	
	function handleSelect(index: number) {
		selectedIndex = index;
		onSelect?.(index);
	}
</script>

<div class="proposal-carousel">
	<div class="navigation">
		<button 
			class="nav-btn prev" 
			onclick={handlePrevious}
			disabled={selectedIndex === 0}
			aria-label="Previous proposal"
		>
			←
		</button>
		
		<div class="indicators">
			{#each proposals as _, i}
				<button
					class="indicator"
					class:active={i === selectedIndex}
					onclick={() => handleSelect(i)}
					aria-label="Go to proposal {i + 1}"
				></button>
			{/each}
		</div>
		
		<button 
			class="nav-btn next" 
			onclick={handleNext}
			disabled={selectedIndex === proposals.length - 1}
			aria-label="Next proposal"
		>
			→
		</button>
	</div>
	
	<div class="counter">
		{selectedIndex + 1} / {proposals.length}
	</div>
</div>

<style>
	.proposal-carousel {
		display: flex;
		flex-direction: column;
		gap: 0.75rem;
		align-items: center;
	}
	
	.navigation {
		display: flex;
		align-items: center;
		gap: 1rem;
	}
	
	.nav-btn {
		width: 2.5rem;
		height: 2.5rem;
		border: 2px solid var(--border-color, #e0e0e0);
		background: white;
		border-radius: 50%;
		font-size: 1.25rem;
		cursor: pointer;
		transition: all 0.2s;
		display: flex;
		align-items: center;
		justify-content: center;
	}
	
	.nav-btn:hover:not(:disabled) {
		border-color: var(--primary-color, #667eea);
		background: var(--primary-color, #667eea);
		color: white;
		transform: scale(1.1);
	}
	
	.nav-btn:disabled {
		opacity: 0.3;
		cursor: not-allowed;
	}
	
	.indicators {
		display: flex;
		gap: 0.5rem;
		align-items: center;
	}
	
	.indicator {
		width: 0.75rem;
		height: 0.75rem;
		border: none;
		background: var(--bg-muted, #ddd);
		border-radius: 50%;
		cursor: pointer;
		transition: all 0.3s;
		padding: 0;
	}
	
	.indicator:hover {
		background: var(--primary-color-light, #9face6);
		transform: scale(1.2);
	}
	
	.indicator.active {
		background: var(--primary-color, #667eea);
		transform: scale(1.3);
	}
	
	.counter {
		font-size: 0.875rem;
		font-weight: 600;
		color: var(--text-muted, #666);
	}
</style>

