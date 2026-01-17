<script lang="ts">
	import { Marker } from 'svelte-maplibre-gl';
	import { tweened } from 'svelte/motion';
	import { cubicOut, linear } from 'svelte/easing';
	import type { LiveLocationData } from '$lib/location/location.svelte';

	interface Props {
		location: LiveLocationData;
		pubkey: string;
		draggable?: boolean;
	}

	let { location, pubkey, draggable = false }: Props = $props();

	// Smooth coordinates store
	// Duration matches simulator update rate (200ms) plus a bit for smoothness
	const coords = tweened(
		{ lng: location.longitude, lat: location.latitude },
		{ duration: 400, easing: linear }
	);

	// Update coordinates when location prop changes
	$effect(() => {
		const target = { lng: location.longitude, lat: location.latitude };
		const current = $coords;

		// Check for dateline crossing (e.g. 179 -> -179)
		// If crossing, snap instantly to avoid flying across the map
		if (Math.abs(current.lng - target.lng) > 180) {
			coords.set(target, { duration: 0 });
		} else {
			coords.set(target);
		}
	});

	// Display text
	let shortPubkey = $derived(pubkey.slice(0, 8) + '...');
	let coordsText = $derived(`(${location.latitude.toFixed(4)}, ${location.longitude.toFixed(4)})`);
	
	// Display emoji or default
	let markerEmoji = $derived(location.emoji || '👤');
	
	// Visual altitude offset (fake 3D)
	// Map meters to pixels loosely (e.g. 50km -> 1px? No, maybe simpler log scale or clamped)
	// For visibility: Satellites (800km) should be clearly above ground, but not off screen.
	let altitudeOffset = $derived.by(() => {
		const alt = location.altitude || 0;
		if (alt > 500000) return -60; // Satellites high up
		if (alt > 10000) return -20; // Planes
		if (alt > 100) return -5; // Ships/visual lift
		return 0;
	});

	// Dynamic z-index based on latitude + altitude
	let zIndex = $derived.by(() => {
		if (markerEmoji === '🚀') return 2000;
		if (markerEmoji === '🛰️') return 2100; // Satellites on top
		if (markerEmoji === '✈️') return 1000;
		if (markerEmoji === '🚁') return 900;
		return 100 + Math.round((90 - location.latitude) * 10);
	});
</script>

<Marker lnglat={$coords} {draggable}>
	{#snippet content()}
		<div 
			class="network-marker" 
			style="
				z-index: {zIndex};
				transform: translateY({altitudeOffset}px);
			"
		>
			<div class="marker-icon">{markerEmoji}</div>
		</div>
	{/snippet}
</Marker>

<style>
	.network-marker {
		display: flex;
		flex-direction: column;
		align-items: center;
		text-align: center;
		cursor: pointer;
		/* Transition transform for smooth movement + altitude changes */
		transition: transform 0.2s linear; 
		pointer-events: auto;
	}

	.network-marker:hover {
		transform: scale(1.2);
		z-index: 2000 !important;
	}

	.marker-icon {
		font-size: 20px;
		line-height: 1;
		filter: drop-shadow(0 4px 6px rgba(0, 0, 0, 0.3));
		transform-origin: center bottom;
		transition: transform 0.3s cubic-bezier(0.34, 1.56, 0.64, 1);
	}
	
	.network-marker:hover .marker-icon {
		transform: scale(1.2) translateY(-5px);
	}


</style>
