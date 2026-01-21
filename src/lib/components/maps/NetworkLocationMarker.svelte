<script lang="ts">
	import { Marker } from 'svelte-maplibre-gl';
	import { tweened } from 'svelte/motion';
	import { cubicOut, linear } from 'svelte/easing';
	import type { LiveLocationData } from '$lib/location/location.svelte';

	interface Props {
		location: LiveLocationData;
		pubkey: string;
		draggable?: boolean;
		zoom?: number; // Added zoom prop for dynamic scaling
	}

	let { location, pubkey, draggable = false, zoom = 10 }: Props = $props();

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
	// Apply zoom scaling: At low zoom (globe), shift more pixels. at high zoom, shift fewer? 
	// Actually, projected height differences are larger at high zoom (close up) if we consider meters -> pixels.
	// But at low zoom (globe), huge altitude (800km) is visible.
	// 800km at zoom 0 is ~10px? 
	// Let's use a log scale or simple heuristic.
	let altitudeOffset = $derived.by(() => {
		const alt = location.altitude || 0;
		if (alt < 100) return 0;
		
		// Scale factor based on zoom? 
		// Zoom 0 = World is 512px. 800km is ~1/50 of earth radius. 
		// Earth radius ~6371km. 800km is significant.
		// Let's make it proportional to zoom somewhat?
		
		let baseOffset = 0;
		if (alt > 500000) baseOffset = -60; // Satellites
		else if (alt > 10000) baseOffset = -20; // Planes
		else if (alt > 100) baseOffset = -5; // Ships
		
		// Adjust by zoom? 
		// If zoomed in (zoom 10), we are close to ground. Satellite is WAY up.
		// If we keep -60px, it looks "low". It should be off screen (too high).
		// But we want to see it! 
		// So keep it clamped for visibility, maybe slight scaling.
		// Let's keep it simple for now as the user liked the "fake" look but said it was "far off" 
		// maybe laterally? 
		// If I add zoom factor it might help parallax.
		
		return baseOffset * (1 + (zoom - 1) * 0.1); // Slight increase with zoom
	});

	// Dynamic z-index based on latitude + altitude
	let zIndex = $derived.by(() => {
		// Base z-index needs to be high enough to be above DeckGL overlay (which might be z-index 1 or higher)
		// but MapLibre markers are DOM elements, so they usually sit above canvas if z-index is sufficient.
		// Boosting all values significantly to ensure they pop over paths.
		const BASE_Z = 100000;
		if (markerEmoji === '🚀') return BASE_Z + 2000;
		if (markerEmoji === '🛰️') return BASE_Z + 2100; // Satellites on top
		if (markerEmoji === '✈️') return BASE_Z + 1000;
		if (markerEmoji === '🚁') return BASE_Z + 900;
		return BASE_Z + 100 + Math.round((90 - location.latitude) * 10);
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
		z-index: 200000 !important;
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
