<script lang="ts">
	/**
	 * @component SupportCard
	 * Interface for allocating support points across candidates
	 * 
	 * @prop {string[]} candidates - Array of candidate versions
	 * @prop {number} totalPoints - Total points to distribute
	 * @prop {Function} onSubmit - Callback when support is submitted
	 * @prop {Function} onCancel - Callback when cancelled
	 */
	
	import PointAllocator from '../shared/PointAllocator.svelte';
	
	let { candidates, totalPoints = 10, onSubmit, onCancel }: {
		candidates: string[];
		totalPoints?: number;
		onSubmit: (allocation: Record<string, number>) => Promise<void>;
		onCancel: () => void;
	} = $props();
	
	let allocation = $state<Record<string, number>>(
		Object.fromEntries(candidates.map(c => [c, 0]))
	);
	let isSubmitting = $state(false);
	
	const totalAllocated = $derived(
		Object.values(allocation).reduce((sum, val) => sum + val, 0)
	);
	
	const isOverLimit = $derived(totalAllocated > totalPoints);
	const remainingPoints = $derived(totalPoints - totalAllocated);
	
	async function handleSubmit() {
		if (isOverLimit || isSubmitting) return;
		
		isSubmitting = true;
		try {
			await onSubmit(allocation);
		} finally {
			isSubmitting = false;
		}
	}
</script>

<div class="support-card">
	<div class="header">
		<h3 class="title">
			<span class="icon">👍</span>
			Express Your Support
		</h3>
	</div>
	
	<div class="instructions">
		Distribute <strong>{totalPoints} points</strong> across the candidate versions. 
		Give more points to versions you support more strongly.
	</div>
	
	<div class="candidates">
		{#each candidates as candidate, i}
			<div class="candidate-row">
				<div class="candidate-info">
					<div class="candidate-label">
						{i === 0 ? 'Original' : `Modified ${i}`}
					</div>
					<div class="candidate-content">{candidate}</div>
				</div>
				<div class="allocator">
					<PointAllocator 
						bind:value={allocation[candidate]}
						max={totalPoints}
					/>
				</div>
			</div>
		{/each}
	</div>
	
	<div class="summary" class:over-limit={isOverLimit}>
		<div class="summary-content">
			<div class="allocated">
				<span class="label">Total Allocated:</span>
				<span class="value" class:over={isOverLimit}>
					{totalAllocated} / {totalPoints}
				</span>
			</div>
			<div class="remaining">
				<span class="label">Remaining:</span>
				<span class="value" class:negative={remainingPoints < 0}>
					{remainingPoints}
				</span>
			</div>
		</div>
		{#if isOverLimit}
			<div class="warning">
				⚠️ You've allocated too many points! Please reduce your allocation.
			</div>
		{/if}
	</div>
	
	<div class="actions">
		<button class="cancel-btn" onclick={onCancel} disabled={isSubmitting}>
			Cancel
		</button>
		<button 
			class="submit-btn" 
			onclick={handleSubmit}
			disabled={isOverLimit || isSubmitting}
		>
			{isSubmitting ? 'Submitting...' : 'Submit Support'}
		</button>
	</div>
</div>

<style>
	.support-card {
		background: white;
		padding: 1.5rem;
		border-radius: 1rem;
		box-shadow: 0 4px 20px rgba(0, 0, 0, 0.15);
		max-width: 50rem;
	}
	
	.header {
		margin-bottom: 1rem;
	}
	
	.title {
		margin: 0;
		font-size: 1.25rem;
		font-weight: 600;
		color: #4caf50;
		display: flex;
		align-items: center;
		gap: 0.5rem;
	}
	
	.icon {
		font-size: 1.5rem;
	}
	
	.instructions {
		background: #f1f8f4;
		padding: 0.875rem;
		border-radius: 0.625rem;
		font-size: 0.9375rem;
		line-height: 1.5;
		color: var(--text-secondary, #555);
		margin-bottom: 1.5rem;
	}
	
	.candidates {
		display: flex;
		flex-direction: column;
		gap: 1rem;
		margin-bottom: 1.5rem;
	}
	
	.candidate-row {
		background: #f8f9fa;
		padding: 1rem;
		border-radius: 0.75rem;
		display: flex;
		gap: 1rem;
		align-items: center;
	}
	
	.candidate-info {
		flex: 1;
		min-width: 0;
	}
	
	.candidate-label {
		font-size: 0.8125rem;
		font-weight: 600;
		text-transform: uppercase;
		letter-spacing: 0.5px;
		color: var(--text-muted, #666);
		margin-bottom: 0.5rem;
	}
	
	.candidate-content {
		font-size: 0.9375rem;
		line-height: 1.5;
		color: var(--text-primary, #333);
	}
	
	.allocator {
		flex-shrink: 0;
		min-width: 12rem;
	}
	
	.summary {
		background: #e3f2fd;
		padding: 1rem;
		border-radius: 0.75rem;
		margin-bottom: 1rem;
		transition: all 0.3s;
	}
	
	.summary.over-limit {
		background: #ffebee;
	}
	
	.summary-content {
		display: flex;
		justify-content: space-between;
		gap: 1rem;
		flex-wrap: wrap;
	}
	
	.allocated,
	.remaining {
		display: flex;
		align-items: center;
		gap: 0.5rem;
	}
	
	.label {
		font-size: 0.875rem;
		font-weight: 600;
		color: var(--text-muted, #666);
	}
	
	.value {
		font-size: 1.125rem;
		font-weight: 700;
		color: var(--text-primary, #333);
	}
	
	.value.over,
	.value.negative {
		color: #c62828;
	}
	
	.warning {
		margin-top: 0.75rem;
		padding: 0.625rem;
		background: white;
		border-radius: 0.5rem;
		font-size: 0.875rem;
		font-weight: 600;
		color: #c62828;
		text-align: center;
	}
	
	.actions {
		display: flex;
		justify-content: flex-end;
		gap: 0.75rem;
	}
	
	button {
		padding: 0.75rem 1.5rem;
		border: none;
		border-radius: 0.625rem;
		font-size: 1rem;
		font-weight: 600;
		cursor: pointer;
		transition: all 0.2s;
	}
	
	.cancel-btn {
		background: var(--bg-muted, #e0e0e0);
		color: var(--text-primary, #333);
	}
	
	.cancel-btn:hover:not(:disabled) {
		background: #d0d0d0;
	}
	
	.submit-btn {
		background: #4caf50;
		color: white;
	}
	
	.submit-btn:hover:not(:disabled) {
		transform: translateY(-2px);
		box-shadow: 0 4px 12px rgba(76, 175, 80, 0.3);
	}
	
	button:disabled {
		opacity: 0.5;
		cursor: not-allowed;
	}
	
	@media (max-width: 640px) {
		.candidate-row {
			flex-direction: column;
			align-items: stretch;
		}
		
		.allocator {
			width: 100%;
		}
	}
</style>

