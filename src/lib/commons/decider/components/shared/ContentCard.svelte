<script lang="ts">
	/**
	 * @component ContentCard
	 * Reusable card for challenges, comments, and modifications
	 * 
	 * @prop {string} content - The content text
	 * @prop {string} authorPub - Author's public key
	 * @prop {string} currentUserPub - Current user's public key
	 * @prop {number} timestamp - Optional timestamp
	 * @prop {string} type - Type: 'challenge' | 'comment' | 'modification'
	 */
	
	import AuthorBadge from './AuthorBadge.svelte';
	
	let { 
		content, 
		authorPub, 
		currentUserPub,
		timestamp = undefined,
		type = 'comment' 
	}: {
		content: string;
		authorPub: string;
		currentUserPub: string;
		timestamp?: number;
		type?: 'challenge' | 'comment' | 'modification';
	} = $props();
	
	const typeColors = {
		challenge: '#f44336',
		comment: '#2196f3',
		modification: '#ff9800'
	};
	
	const accentColor = $derived(typeColors[type]);
</script>

<div class="content-card" style="--accent-color: {accentColor}">
	<p class="content">{content}</p>
	<div class="meta">
		<AuthorBadge {authorPub} {currentUserPub} compact />
		{#if timestamp}
			<time class="timestamp">{new Date(timestamp).toLocaleString()}</time>
		{/if}
	</div>
</div>

<style>
	.content-card {
		background: var(--card-bg, white);
		padding: 0.875rem;
		border-radius: 0.625rem;
		border-left: 3px solid var(--accent-color);
		transition: box-shadow 0.2s;
	}
	
	.content-card:hover {
		box-shadow: 0 2px 8px rgba(0, 0, 0, 0.08);
	}
	
	.content {
		margin: 0 0 0.625rem 0;
		line-height: 1.5;
		color: var(--text-primary, #333);
	}
	
	.meta {
		display: flex;
		align-items: center;
		gap: 0.75rem;
		flex-wrap: wrap;
	}
	
	.timestamp {
		font-size: 0.8125rem;
		color: var(--text-muted, #666);
	}
</style>

