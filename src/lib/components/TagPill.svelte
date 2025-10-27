<script lang="ts">
	import { getColorForUserId } from '$lib/utils/colorUtils';
	import { getUserName } from '$lib/state/users.svelte';

	// Props using Svelte 5 runes
	let {
		userId,
		displayName: providedDisplayName,
		truncateLength = 10,
		removable = true,
		onClick = (userId: string, event?: MouseEvent) => {},
		onRemove = (userId: string) => {}
	} = $props<{
		userId: string;
		displayName?: string;
		truncateLength?: number;
		removable?: boolean;
		onClick?: (userId: string, event?: MouseEvent) => void;
		onRemove?: (userId: string) => void;
	}>();

	// State
	let fetchedDisplayName = $state<string | null>(null);
	let displayName = $derived(providedDisplayName || fetchedDisplayName || userId);
	let isLoading = $state(!providedDisplayName);

	// Load user data using centralized function (only if no displayName provided)
	async function loadUserData() {
		if (!userId || providedDisplayName || fetchedDisplayName) {
			return;
		}

		isLoading = true;
		try {
			fetchedDisplayName = await getUserName(userId);
		} catch (error) {
			console.error(`Error loading user data for ${userId}:`, error);
		} finally {
			isLoading = false;
		}
	}

	// Initial load of user data
	$effect(() => {
		loadUserData();
	});

	// Function to truncate and format display name
	function getFormattedName(name: string): string {
		return name?.length > truncateLength ? name?.substring(0, truncateLength - 2) + '...' : name;
	}

	// Handle pill click
	function handlePillClick(event: MouseEvent): void {
		// Don't trigger if clicked on remove button
		if ((event.target as HTMLElement).classList.contains('remove-tag')) {
			return;
		}

		event.stopPropagation();
		onClick(userId, event);
	}

	// Handle remove button click
	function handleRemoveClick(event: MouseEvent): void {
		event.stopPropagation();
		onRemove(userId);
	}
</script>

<div
	class="tag-pill"
	data-user-id={userId}
	style="background: {getColorForUserId(userId)}"
	role="button"
	tabindex="0"
	onclick={handlePillClick}
	title={removable
				? `${displayName}: Click to view tree, click × to remove`
				: `${displayName}: Click to view tree`}
	onkeydown={(e) => {
		if (e.key === 'Enter' || e.key === ' ') {
			e.preventDefault();
			// Create a synthetic MouseEvent for consistency
			const syntheticEvent = new MouseEvent('click', {
				bubbles: true,
				cancelable: true
			});
			handlePillClick(syntheticEvent);
		}
	}}
>
	<span>{getFormattedName(displayName)}</span>

	{#if removable}
		<span 
			class="remove-tag" 
			role="button"
			tabindex="0"
			onclick={handleRemoveClick}
			onkeydown={(e) => {
				if (e.key === 'Enter' || e.key === ' ') {
					e.preventDefault();
					// Create a synthetic MouseEvent for consistency
					const syntheticEvent = new MouseEvent('click', {
						bubbles: true,
						cancelable: true
					});
					handleRemoveClick(syntheticEvent);
				}
			}}
		>×</span>
	{/if}
</div>

<style>
	.tag-pill {
		display: flex;
		align-items: center;
		border-radius: 12px;
		padding: 3px 10px;
		margin: 2px;
		height: 22px;
		font-size: 12px;
		white-space: nowrap;
		box-shadow: 0 1px 3px rgba(0, 0, 0, 0.1);
		border: 1px solid rgba(255, 255, 255, 0.2);
		opacity: 0.9;
		cursor: pointer;
		transition: all 0.2s ease;
	}

	.tag-pill:hover {
		opacity: 1;
		transform: translateY(-1px);
		box-shadow: 0 2px 4px rgba(0, 0, 0, 0.2);
	}

	.tag-pill span {
		color: #000;
		margin-right: 5px;
		font-weight: 500;
		text-shadow: 0 1px 0 rgba(255, 255, 255, 0.4);
	}

	.remove-tag {
		color: #000;
		font-size: 14px;
		line-height: 14px;
		opacity: 0.7;
		font-weight: bold;
		margin-left: 2px;
		display: flex;
		align-items: center;
		justify-content: center;
		width: 16px;
		height: 16px;
		border-radius: 50%;
		background: rgba(255, 255, 255, 0.4);
		cursor: pointer;
		transition: all 0.2s ease;
	}

	.remove-tag:hover {
		opacity: 1;
		background: rgba(255, 255, 255, 0.7);
	}
</style>
