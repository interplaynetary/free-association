<script lang="ts">
	/**
	 * @component ActivityFeed
	 * Slide-out activity panel showing user's actions
	 * Elegant way to track participation and enable undo
	 */
	
	import { slide } from 'svelte/transition';
	import type { Activity } from '../../composables/useActivityFeed.svelte';
	
	interface Props {
		activities: Activity[];
		onView?: (activity: Activity) => void;
		onDelete?: (activity: Activity) => void;
	}
	
	let { activities, onView, onDelete }: Props = $props();
	
	let isOpen = $state(false);
	
	const recentActivities = $derived(activities.slice(0, 10));
	
	const activityLabels: Record<Activity['type'], { icon: string; label: string; color: string }> = {
		proposal: { icon: '📝', label: 'Submitted proposal', color: '#667eea' },
		challenge: { icon: '⚠️', label: 'Raised challenge', color: '#f44336' },
		comment: { icon: '💬', label: 'Added comment', color: '#2196f3' },
		modification: { icon: '✏️', label: 'Suggested modification', color: '#ff9800' },
		support: { icon: '👍', label: 'Expressed support', color: '#4caf50' }
	};
	
	function formatRelativeTime(date: Date): string {
		const now = new Date();
		const diff = now.getTime() - date.getTime();
		
		const seconds = Math.floor(diff / 1000);
		const minutes = Math.floor(seconds / 60);
		const hours = Math.floor(minutes / 60);
		const days = Math.floor(hours / 24);
		
		if (seconds < 60) return 'Just now';
		if (minutes < 60) return `${minutes}m ago`;
		if (hours < 24) return `${hours}h ago`;
		return `${days}d ago`;
	}
	
	function shortPub(pub: string): string {
		return pub.slice(0, 8) + '...';
	}
	
	function getDescription(activity: Activity): string {
		const base = activityLabels[activity.type].label;
		if (activity.targetPub) {
			return `${base} on ${shortPub(activity.targetPub)}'s proposal`;
		}
		return base;
	}
	
	function canDelete(activity: Activity): boolean {
		// Can delete if less than 5 minutes old
		const fiveMinutes = 5 * 60 * 1000;
		return Date.now() - activity.timestamp.getTime() < fiveMinutes;
	}
	
	function toggle() {
		isOpen = !isOpen;
	}
</script>

<!-- Toggle Button -->
<button 
	class="activity-toggle"
	onclick={toggle}
	aria-label="View your activity"
	class:active={isOpen}
>
	<span class="icon">📜</span>
	{#if recentActivities.length > 0}
		<span class="badge">{recentActivities.length}</span>
	{/if}
</button>

<!-- Slide-out Panel -->
{#if isOpen}
	<div class="activity-overlay" onclick={toggle} transition:slide={{ axis: 'x', duration: 300 }}></div>
	<aside class="activity-panel" transition:slide={{ axis: 'x', duration: 300 }}>
		<header class="panel-header">
			<h3>Your Activity</h3>
			<button class="close-btn" onclick={toggle} aria-label="Close">
				×
			</button>
		</header>
		
		<div class="activity-list">
			{#if recentActivities.length === 0}
				<div class="empty-state">
					<p class="icon">📭</p>
					<p>No activity yet</p>
					<p class="hint">Your actions will appear here</p>
				</div>
			{:else}
				{#each recentActivities as activity (activity.id)}
					{@const meta = activityLabels[activity.type]}
					<div class="activity-item" style="--accent-color: {meta.color}">
						<div class="activity-icon">
							{meta.icon}
						</div>
						
						<div class="activity-content">
							<div class="activity-time">
								{formatRelativeTime(activity.timestamp)}
							</div>
							<div class="activity-description">
								{getDescription(activity)}
							</div>
							{#if activity.content}
								<div class="activity-preview">
									"{activity.content.slice(0, 60)}{activity.content.length > 60 ? '...' : ''}"
								</div>
							{/if}
						</div>
						
						<div class="activity-actions">
							{#if onView}
								<button 
									class="action-link"
									onclick={() => onView?.(activity)}
									title="View"
								>
									👁️
								</button>
							{/if}
							
							{#if onDelete && canDelete(activity)}
								<button 
									class="action-link danger"
									onclick={() => onDelete?.(activity)}
									title="Delete"
								>
									🗑️
								</button>
							{/if}
						</div>
					</div>
				{/each}
			{/if}
		</div>
		
		{#if activities.length > 10}
			<div class="panel-footer">
				Showing latest 10 of {activities.length} actions
			</div>
		{/if}
	</aside>
{/if}

<style>
	/* Toggle Button */
	.activity-toggle {
		position: fixed;
		bottom: 1.5rem;
		right: 1.5rem;
		width: 3.5rem;
		height: 3.5rem;
		border-radius: 50%;
		background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
		color: white;
		border: none;
		cursor: pointer;
		display: flex;
		align-items: center;
		justify-content: center;
		box-shadow: 0 4px 16px rgba(0, 0, 0, 0.2);
		transition: all 0.3s ease;
		z-index: 90;
		position: relative;
	}
	
	.activity-toggle:hover {
		transform: translateY(-4px);
		box-shadow: 0 6px 24px rgba(0, 0, 0, 0.25);
	}
	
	.activity-toggle.active {
		background: linear-gradient(135deg, #764ba2 0%, #667eea 100%);
	}
	
	.icon {
		font-size: 1.5rem;
	}
	
	.badge {
		position: absolute;
		top: -4px;
		right: -4px;
		background: #f44336;
		color: white;
		font-size: 0.75rem;
		font-weight: 700;
		padding: 0.25rem 0.5rem;
		border-radius: 1rem;
		min-width: 1.5rem;
		text-align: center;
		box-shadow: 0 2px 8px rgba(0, 0, 0, 0.2);
	}
	
	/* Overlay */
	.activity-overlay {
		position: fixed;
		top: 0;
		left: 0;
		right: 0;
		bottom: 0;
		background: rgba(0, 0, 0, 0.5);
		z-index: 99;
	}
	
	/* Panel */
	.activity-panel {
		position: fixed;
		top: 0;
		right: 0;
		bottom: 0;
		width: min(400px, 90vw);
		background: white;
		box-shadow: -4px 0 24px rgba(0, 0, 0, 0.15);
		display: flex;
		flex-direction: column;
		z-index: 100;
	}
	
	.panel-header {
		display: flex;
		justify-content: space-between;
		align-items: center;
		padding: 1.5rem;
		border-bottom: 1px solid #e0e0e0;
	}
	
	.panel-header h3 {
		margin: 0;
		font-size: 1.25rem;
		font-weight: 600;
		color: #333;
	}
	
	.close-btn {
		background: none;
		border: none;
		font-size: 2rem;
		line-height: 1;
		cursor: pointer;
		color: #666;
		padding: 0;
		width: 2rem;
		height: 2rem;
		display: flex;
		align-items: center;
		justify-content: center;
		transition: color 0.2s;
	}
	
	.close-btn:hover {
		color: #333;
	}
	
	/* Activity List */
	.activity-list {
		flex: 1;
		overflow-y: auto;
		padding: 1rem;
	}
	
	.empty-state {
		display: flex;
		flex-direction: column;
		align-items: center;
		justify-content: center;
		padding: 3rem 1rem;
		text-align: center;
		color: #999;
	}
	
	.empty-state .icon {
		font-size: 3rem;
		margin-bottom: 0.5rem;
	}
	
	.empty-state .hint {
		font-size: 0.875rem;
		margin-top: 0.25rem;
	}
	
	.activity-item {
		display: flex;
		gap: 0.75rem;
		padding: 1rem;
		border-left: 3px solid var(--accent-color, #667eea);
		background: #f8f9fa;
		border-radius: 0.5rem;
		margin-bottom: 0.75rem;
		transition: all 0.2s;
	}
	
	.activity-item:hover {
		background: #f0f1f3;
	}
	
	.activity-icon {
		font-size: 1.25rem;
		flex-shrink: 0;
	}
	
	.activity-content {
		flex: 1;
		min-width: 0;
	}
	
	.activity-time {
		font-size: 0.75rem;
		font-weight: 600;
		color: #999;
		text-transform: uppercase;
		letter-spacing: 0.5px;
		margin-bottom: 0.25rem;
	}
	
	.activity-description {
		font-size: 0.9375rem;
		font-weight: 600;
		color: #333;
		margin-bottom: 0.25rem;
	}
	
	.activity-preview {
		font-size: 0.875rem;
		color: #666;
		font-style: italic;
		line-height: 1.4;
	}
	
	.activity-actions {
		display: flex;
		gap: 0.5rem;
		align-items: flex-start;
	}
	
	.action-link {
		background: none;
		border: none;
		font-size: 1.25rem;
		cursor: pointer;
		padding: 0.25rem;
		opacity: 0.6;
		transition: all 0.2s;
	}
	
	.action-link:hover {
		opacity: 1;
		transform: scale(1.1);
	}
	
	.action-link.danger:hover {
		filter: brightness(1.2);
	}
	
	.panel-footer {
		padding: 1rem 1.5rem;
		border-top: 1px solid #e0e0e0;
		background: #f8f9fa;
		font-size: 0.875rem;
		color: #666;
		text-align: center;
	}
	
	@media (max-width: 640px) {
		.activity-panel {
			width: 100vw;
		}
	}
</style>


