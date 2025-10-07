<script lang="ts">
	import { liveLocationBlockList, liveLocationAccessList } from '$lib/state/location.svelte';
	import { persistLiveLocationBlockList } from '$lib/state/persistence.svelte';
	import { getUserName } from '$lib/state/users.svelte';

	function toggleBlockUser(userId: string) {
		liveLocationBlockList.update((list) => {
			if (list.includes(userId)) {
				// Unblock user
				return list.filter((id) => id !== userId);
			} else {
				// Block user
				return [...list, userId];
			}
		});

		// Persist changes
		persistLiveLocationBlockList();
	}

	function isBlocked(userId: string): boolean {
		return $liveLocationBlockList.includes(userId);
	}

	function formatUserId(userId: string): string {
		const name = getUserName(userId);
		if (name) {
			return `${name} (${userId.substring(0, 8)}...)`;
		}
		return `${userId.substring(0, 8)}...`;
	}
</script>

<div class="privacy-controls">
	<div class="header">
		<h3>🔒 Live Location Privacy</h3>
		<p class="description">
			Control who can see your live location. By default, all users with allocations in your
			capacities can see your location.
		</p>
	</div>

	{#if $liveLocationAccessList.length === 0}
		<div class="empty-state">
			<p>No users currently have access to your live location.</p>
			<p class="hint">Users get access when they have allocations in your capacities.</p>
		</div>
	{:else}
		<div class="access-list">
			<h4>Users with potential access ({$liveLocationAccessList.length})</h4>
			<div class="user-list">
				{#each $liveLocationAccessList as userId}
					{@const blocked = isBlocked(userId)}
					<div class="user-item" class:blocked>
						<div class="user-info">
							<span class="user-name">{formatUserId(userId)}</span>
							<span class="status-badge" class:blocked>
								{blocked ? '🚫 Blocked' : '✅ Can see'}
							</span>
						</div>
						<button
							class="block-toggle"
							class:blocked
							onclick={() => toggleBlockUser(userId)}
							title={blocked ? 'Unblock this user' : 'Block this user'}
						>
							{blocked ? 'Unblock' : 'Block'}
						</button>
					</div>
				{/each}
			</div>
		</div>
	{/if}

	{#if $liveLocationBlockList.length > 0}
		<div class="blocked-summary">
			<strong>{$liveLocationBlockList.length}</strong> user{$liveLocationBlockList.length === 1
				? ' is'
				: 's are'} blocked from seeing your live location
		</div>
	{/if}
</div>

<style>
	.privacy-controls {
		background: var(--color-bg-secondary, #f5f5f5);
		border: 1px solid var(--color-border, #ddd);
		border-radius: 8px;
		padding: 1rem;
		margin: 1rem 0;
	}

	.header {
		margin-bottom: 1rem;
	}

	.header h3 {
		margin: 0 0 0.5rem 0;
		font-size: 1.1rem;
	}

	.description {
		margin: 0;
		color: var(--color-text-secondary, #666);
		font-size: 0.9rem;
		line-height: 1.4;
	}

	.empty-state {
		background: white;
		border-radius: 4px;
		padding: 1.5rem;
		text-align: center;
	}

	.empty-state p {
		margin: 0.5rem 0;
		color: var(--color-text-secondary, #666);
	}

	.empty-state .hint {
		font-size: 0.85rem;
		font-style: italic;
	}

	.access-list {
		background: white;
		border-radius: 4px;
		padding: 1rem;
	}

	.access-list h4 {
		margin: 0 0 0.75rem 0;
		font-size: 1rem;
		color: var(--color-text-primary, #333);
	}

	.user-list {
		display: flex;
		flex-direction: column;
		gap: 0.5rem;
	}

	.user-item {
		display: flex;
		justify-content: space-between;
		align-items: center;
		padding: 0.75rem;
		background: var(--color-bg-tertiary, #fafafa);
		border: 1px solid var(--color-border, #ddd);
		border-radius: 4px;
		transition: all 0.2s ease;
	}

	.user-item.blocked {
		background: var(--color-error-bg, #fee);
		border-color: var(--color-error, #fcc);
	}

	.user-info {
		display: flex;
		flex-direction: column;
		gap: 0.25rem;
		flex: 1;
	}

	.user-name {
		font-weight: 600;
		font-size: 0.95rem;
	}

	.status-badge {
		font-size: 0.8rem;
		padding: 2px 6px;
		border-radius: 4px;
		background: var(--color-success-bg, #efe);
		color: var(--color-success, #080);
		display: inline-block;
		width: fit-content;
	}

	.status-badge.blocked {
		background: var(--color-error-bg, #fee);
		color: var(--color-error, #c00);
	}

	.block-toggle {
		padding: 0.5rem 1rem;
		border: 1px solid var(--color-error, #c00);
		border-radius: 4px;
		background: white;
		color: var(--color-error, #c00);
		cursor: pointer;
		font-weight: 600;
		font-size: 0.85rem;
		transition: all 0.2s ease;
	}

	.block-toggle:hover {
		background: var(--color-error, #c00);
		color: white;
	}

	.block-toggle.blocked {
		border-color: var(--color-success, #4caf50);
		color: var(--color-success, #4caf50);
	}

	.block-toggle.blocked:hover {
		background: var(--color-success, #4caf50);
		color: white;
	}

	.blocked-summary {
		margin-top: 1rem;
		padding: 0.75rem;
		background: var(--color-warning-bg, #fff3cd);
		border: 1px solid var(--color-warning, #ffc107);
		border-radius: 4px;
		text-align: center;
		color: var(--color-warning-dark, #856404);
	}

	.blocked-summary strong {
		color: var(--color-warning-darker, #533f03);
	}
</style>
