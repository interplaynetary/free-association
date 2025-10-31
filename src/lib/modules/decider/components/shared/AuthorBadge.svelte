<script lang="ts">
	/**
	 * @component AuthorBadge
	 * Displays author information consistently across the app
	 * 
	 * @prop {string} authorPub - Public key of the author
	 * @prop {string} currentUserPub - Current user's public key
	 * @prop {boolean} compact - Show compact version
	 */
	
	let { authorPub, currentUserPub, compact = false }: {
		authorPub: string;
		currentUserPub: string;
		compact?: boolean;
	} = $props();
	
	const isCurrentUser = $derived(authorPub === currentUserPub);
	const displayName = $derived(isCurrentUser ? 'You' : `${authorPub.slice(0, 8)}...`);
</script>

<span class="author-badge" class:is-you={isCurrentUser} class:compact>
	{displayName}
</span>

<style>
	.author-badge {
		display: inline-flex;
		align-items: center;
		gap: 0.375rem;
		background: var(--badge-bg, #e3f2fd);
		color: var(--badge-color, #1976d2);
		padding: 0.375rem 0.875rem;
		border-radius: 1rem;
		font-size: 0.875rem;
		font-weight: 600;
		white-space: nowrap;
	}
	
	.author-badge.compact {
		padding: 0.25rem 0.625rem;
		font-size: 0.8125rem;
	}
	
	.author-badge.is-you {
		background: var(--primary-color, #667eea);
		color: white;
	}
</style>

