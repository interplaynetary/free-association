<script lang="ts">
	import { getColorForUserId } from '../utils/colorUtils';
	import { gun } from '$lib/state.svelte';

	// Props using Svelte 5 runes
	let {
		userId,
		truncateLength = 10,
		removable = true,
		onClick = (userId: string, event?: MouseEvent) => {},
		onRemove = (userId: string) => {}
	} = $props<{
		userId: string;
		truncateLength?: number;
		removable?: boolean;
		onClick?: (userId: string, event?: MouseEvent) => void;
		onRemove?: (userId: string) => void;
	}>();

	// State
	let displayName = $state(userId);
	let isLoading = $state(true);

	// Element reference
	let pillElement: HTMLDivElement;

	// Load user data from Gun
	async function loadUserData() {
		isLoading = true;
		try {
			gun
				.get('users')
				.get(userId)
				.once((pubUser: any) => {
					if (pubUser && pubUser.name) {
						displayName = pubUser.name;
					} else {
						// If no name is found, keep using the userId
						displayName = userId;
					}
					isLoading = false;
					updateTooltip(displayName);
				});
			isLoading = false;
			updateTooltip(displayName);
		} catch (error) {
			console.error(`Error loading user data for ${userId}:`, error);
			displayName = userId;
			isLoading = false;
			updateTooltip(displayName);
		}
	}

	// Initial load of user data
	$effect(() => {
		if (userId) {
			loadUserData();
		}
	});

	// Function to truncate and format display name
	function getFormattedName(name: string): string {
		return name.length > truncateLength ? name.substring(0, truncateLength - 2) + '...' : name;
	}

	// Update tooltip text
	function updateTooltip(name: string): void {
		if (pillElement) {
			pillElement.title = removable
				? `${name}: Click to view tree, click × to remove`
				: `${name}: Click to view tree`;
		}
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
	bind:this={pillElement}
	class="tag-pill"
	data-user-id={userId}
	style="background: {getColorForUserId(userId)}"
	onclick={handlePillClick}
>
	<span>{getFormattedName(displayName)}</span>

	{#if removable}
		<span class="remove-tag" onclick={handleRemoveClick}>×</span>
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
