<script lang="ts">
	import { getColorForNameHash } from '$lib/utils/colorUtils';

	// Props
	let {
		show = false,
		nodeName = '',
		nodeColor = '',
		x = 0,
		y = 0
	} = $props<{
		show: boolean;
		nodeName: string;
		nodeColor?: string;
		x: number;
		y: number;
	}>();

	// Calculate node color if not provided
	const computedColor = $derived(nodeColor || getColorForNameHash(nodeName));
</script>

{#if show}
	<div
		class="dragged-node"
		style="
			left: {x}px;
			top: {y}px;
			background-color: {computedColor};
		"
	>
		<div class="dragged-node-content">
			{nodeName.charAt(0).toUpperCase()}
		</div>
	</div>
{/if}

<style>
	.dragged-node {
		position: fixed;
		width: 40px;
		height: 40px;
		border-radius: 6px;
		z-index: 10000;
		pointer-events: none;
		box-shadow:
			0 4px 12px rgba(0, 0, 0, 0.3),
			0 0 0 2px rgba(255, 255, 255, 0.5);
		transform: translate(-50%, -50%) scale(1.1);
		transition: transform 0.1s ease;
		opacity: 0.9;
		border: 1px solid rgba(255, 255, 255, 0.3);
	}

	.dragged-node-content {
		width: 100%;
		height: 100%;
		display: flex;
		align-items: center;
		justify-content: center;
		font-weight: bold;
		font-size: 18px;
		color: rgba(0, 0, 0, 0.8);
		text-shadow:
			0px 0px 3px rgba(255, 255, 255, 0.8),
			0px 0px 2px rgba(255, 255, 255, 0.6);
	}
</style>
