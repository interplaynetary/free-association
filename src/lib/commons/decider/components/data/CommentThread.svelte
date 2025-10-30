<script lang="ts">
	/**
	 * @component CommentThread
	 * Displays discussion comments
	 * 
	 * @prop {Array} comments - Array of comment objects
	 * @prop {string} currentUserPub - Current user's public key
	 */
	
	import ContentCard from '../shared/ContentCard.svelte';
	import EmptyState from '../shared/EmptyState.svelte';
	
	let { comments, currentUserPub }: {
		comments: Array<{ content: string; authorPub: string; timestamp?: number }>;
		currentUserPub: string;
	} = $props();
</script>

<div class="comment-thread">
	<h4 class="section-title">
		<span class="icon">💬</span>
		Discussion ({comments.length})
	</h4>
	
	{#if comments.length === 0}
		<EmptyState 
			message="No comments yet. Share your thoughts to help improve this proposal."
			emoji="💭"
		/>
	{:else}
		<div class="items">
			{#each comments as comment}
				<ContentCard
					content={comment.content}
					authorPub={comment.authorPub}
					{currentUserPub}
					timestamp={comment.timestamp}
					type="comment"
				/>
			{/each}
		</div>
	{/if}
</div>

<style>
	.comment-thread {
		margin-top: 1rem;
		padding: 1rem;
		background: #f3f9ff;
		border-radius: 0.75rem;
		border: 1px solid #e3f2fd;
	}
	
	.section-title {
		margin: 0 0 0.875rem 0;
		font-size: 1rem;
		font-weight: 600;
		color: #2196f3;
		display: flex;
		align-items: center;
		gap: 0.5rem;
	}
	
	.icon {
		font-size: 1.125rem;
	}
	
	.items {
		display: flex;
		flex-direction: column;
		gap: 0.75rem;
	}
</style>

