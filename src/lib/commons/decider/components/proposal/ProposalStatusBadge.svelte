<script lang="ts">
	/**
	 * @component ProposalStatusBadge
	 * Visual status indicator for proposals
	 * 
	 * @prop {string} status - Status: 'passed-no-challenges' | 'passed-as-is' | 'in-process' | 'awaiting-support' | 'complete'
	 */
	
	let { status }: {
		status: 'passed-no-challenges' | 'passed-as-is' | 'in-process' | 'awaiting-support' | 'complete';
	} = $props();
	
	const statusConfig = {
		'passed-no-challenges': { emoji: '✅', label: 'Passed Early', color: '#4caf50' },
		'passed-as-is': { emoji: '✅', label: 'Passed As-Is', color: '#ff9800' },
		'in-process': { emoji: '⏳', label: 'In Process', color: '#2196f3' },
		'awaiting-support': { emoji: '👍', label: 'Awaiting Support', color: '#9c27b0' },
		'complete': { emoji: '🏆', label: 'Complete', color: '#4caf50' }
	};
	
	const config = $derived(statusConfig[status]);
</script>

<div class="status-badge" style="--status-color: {config.color}">
	<span class="emoji">{config.emoji}</span>
	<span class="label">{config.label}</span>
</div>

<style>
	.status-badge {
		display: inline-flex;
		align-items: center;
		gap: 0.375rem;
		background: color-mix(in srgb, var(--status-color) 15%, white);
		color: var(--status-color);
		padding: 0.375rem 0.875rem;
		border-radius: 1rem;
		font-size: 0.8125rem;
		font-weight: 600;
		white-space: nowrap;
	}
	
	.emoji {
		font-size: 0.875rem;
	}
</style>

