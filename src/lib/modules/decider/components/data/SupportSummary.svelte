<script lang="ts">
	/**
	 * @component SupportSummary
	 * Visualizes support distribution across candidates
	 * 
	 * @prop {Array} candidates - Array of candidate strings
	 * @prop {Array} supportExpressions - Array of support objects with point allocations
	 * @prop {string} winningCandidate - The winning candidate (if determined)
	 */
	
	let { candidates, supportExpressions, winningCandidate = undefined }: {
		candidates: string[];
		supportExpressions: Array<Record<string, number>>;
		winningCandidate?: string;
	} = $props();
	
	const supportTotals = $derived.by(() => {
		const totals: Record<string, number> = {};
		candidates.forEach(c => totals[c] = 0);
		
		supportExpressions.forEach(expr => {
			Object.entries(expr).forEach(([candidate, points]) => {
				if (totals[candidate] !== undefined) {
					totals[candidate] += points;
				}
			});
		});
		
		return totals;
	});
	
	const maxSupport = $derived(Math.max(...Object.values(supportTotals), 1));
	
	const candidateData = $derived(candidates.map(candidate => ({
		content: candidate,
		points: supportTotals[candidate] || 0,
		percentage: ((supportTotals[candidate] || 0) / maxSupport) * 100,
		isWinner: candidate === winningCandidate
	})));
</script>

<div class="support-summary">
	<h4 class="section-title">
		<span class="icon">👍</span>
		Support Distribution
		<span class="vote-count">({supportExpressions.length} votes)</span>
	</h4>
	
	<div class="candidates">
		{#each candidateData as candidate}
			<div class="candidate-row" class:winner={candidate.isWinner}>
				<div class="candidate-content">{candidate.content}</div>
				<div class="support-bar-container">
					<div class="support-bar" style="width: {candidate.percentage}%"></div>
					<span class="points">{candidate.points} pts</span>
				</div>
				{#if candidate.isWinner}
					<span class="winner-badge">🏆 Winner</span>
				{/if}
			</div>
		{/each}
	</div>
</div>

<style>
	.support-summary {
		margin-top: 1rem;
		padding: 1rem;
		background: #f1f8f4;
		border-radius: 0.75rem;
		border: 1px solid #c8e6c9;
	}
	
	.section-title {
		margin: 0 0 0.875rem 0;
		font-size: 1rem;
		font-weight: 600;
		color: #4caf50;
		display: flex;
		align-items: center;
		gap: 0.5rem;
	}
	
	.icon {
		font-size: 1.125rem;
	}
	
	.vote-count {
		font-size: 0.8125rem;
		font-weight: 500;
		color: var(--text-muted, #666);
	}
	
	.candidates {
		display: flex;
		flex-direction: column;
		gap: 0.875rem;
	}
	
	.candidate-row {
		background: white;
		padding: 0.875rem;
		border-radius: 0.625rem;
		transition: all 0.2s;
	}
	
	.candidate-row.winner {
		background: linear-gradient(135deg, #fff9c4 0%, #fff59d 100%);
		box-shadow: 0 2px 8px rgba(255, 193, 7, 0.3);
	}
	
	.candidate-content {
		margin-bottom: 0.625rem;
		font-weight: 500;
		line-height: 1.5;
	}
	
	.support-bar-container {
		position: relative;
		height: 2rem;
		background: var(--bg-muted, #f0f0f0);
		border-radius: 0.5rem;
		overflow: hidden;
		display: flex;
		align-items: center;
	}
	
	.support-bar {
		height: 100%;
		background: linear-gradient(90deg, #4caf50 0%, #81c784 100%);
		transition: width 0.5s ease;
		border-radius: 0.5rem;
	}
	
	.points {
		position: absolute;
		right: 0.75rem;
		font-size: 0.875rem;
		font-weight: 600;
		color: var(--text-primary, #333);
		z-index: 1;
	}
	
	.winner-badge {
		display: inline-flex;
		align-items: center;
		gap: 0.25rem;
		margin-top: 0.5rem;
		font-size: 0.875rem;
		font-weight: 600;
		color: #f57f17;
	}
</style>

