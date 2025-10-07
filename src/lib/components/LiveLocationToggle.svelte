<script lang="ts">
	import { onMount } from 'svelte';
	import {
		currentLocation,
		isLocationTracking,
		locationError,
		updateLocation,
		setLocationTracking,
		setLocationError,
		filteredLiveLocationAccessList,
		currentLocationText
	} from '$lib/state/location.svelte';
	import { persistLiveLocation } from '$lib/state/persistence.svelte';

	let watchId: number | null = null;

	// Auto-persist location when it changes
	$effect(() => {
		if ($isLocationTracking && $currentLocation) {
			persistLiveLocation();
		}
	});

	// Start/stop location tracking
	function toggleLocationTracking() {
		if ($isLocationTracking) {
			stopLocationTracking();
		} else {
			startLocationTracking();
		}
	}

	function startLocationTracking() {
		if (!navigator.geolocation) {
			setLocationError('Geolocation is not supported by your browser');
			return;
		}

		// Clear any previous errors
		setLocationError(null);

		// Set tracking state immediately
		setLocationTracking(true);

		const options = {
			enableHighAccuracy: true,
			timeout: 30000, // 30 seconds - enough time for GPS fix
			maximumAge: 5000 // Accept cached position up to 5 seconds old
		};

		// First, get current position to establish initial location
		navigator.geolocation.getCurrentPosition(
			(position) => {
				updateLocation(position.coords, position.timestamp);
				console.log('[LIVE-LOCATION] Got initial position');

				// Now start watching for updates
				watchId = navigator.geolocation.watchPosition(
					(position) => {
						updateLocation(position.coords, position.timestamp);
					},
					(error) => {
						console.error('[LIVE-LOCATION] Watch error:', error);
						// Only show error if it's not just a timeout (we already have a position)
						if (error.code !== 3) {
							setLocationError(error.message);
						}
					},
					options
				);

				console.log('[LIVE-LOCATION] Started location watching, watch ID:', watchId);
			},
			(error) => {
				console.error('[LIVE-LOCATION] Initial position error:', error);
				let errorMessage = error.message;

				// Provide more helpful error messages
				if (error.code === 1) {
					errorMessage = 'Location permission denied. Please enable location access in your browser settings.';
				} else if (error.code === 2) {
					errorMessage = 'Location unavailable. Please check your device location settings.';
				} else if (error.code === 3) {
					errorMessage = 'Location request timed out. Please try again or check if location services are enabled.';
				}

				setLocationError(errorMessage);
				setLocationTracking(false);
			},
			options
		);

		console.log('[LIVE-LOCATION] Requesting initial location...');
	}

	function stopLocationTracking() {
		if (watchId !== null) {
			navigator.geolocation.clearWatch(watchId);
			watchId = null;
			console.log('[LIVE-LOCATION] Stopped location tracking');
		}

		setLocationTracking(false);
		// Clear location from Gun when tracking is disabled
		persistLiveLocation();
	}

	onMount(() => {
		return () => {
			// Clean up watch on unmount
			if (watchId !== null) {
				navigator.geolocation.clearWatch(watchId);
			}
		};
	});
</script>

<div class="live-location-toggle">
	<div class="toggle-header">
		<h3>📍 Live Location Sharing</h3>
		<button
			onclick={toggleLocationTracking}
			class="toggle-button"
			class:active={$isLocationTracking}
		>
			{$isLocationTracking ? '🟢 Sharing' : '⭕ Not Sharing'}
		</button>
	</div>

	{#if $locationError}
		<div class="error">
			❌ {$locationError}
		</div>
	{/if}

	{#if $isLocationTracking && $currentLocation}
		<div class="location-info">
			<div class="info-row">
				<span class="label">Location:</span>
				<span class="value">{$currentLocationText}</span>
			</div>
			{#if $currentLocation.accuracy}
				<div class="info-row">
					<span class="label">Accuracy:</span>
					<span class="value">±{Math.round($currentLocation.accuracy)}m</span>
				</div>
			{/if}
			<div class="info-row">
				<span class="label">Sharing with:</span>
				<span class="value"
					>{$filteredLiveLocationAccessList.length} user{$filteredLiveLocationAccessList.length ===
					1
						? ''
						: 's'}</span
				>
			</div>
		</div>
	{:else if $isLocationTracking}
		<div class="loading">Getting location...</div>
	{:else}
		<div class="description">
			Share your live location with users who have allocations in your capacities. You can block
			specific users from seeing your location.
		</div>
	{/if}

	{#if $filteredLiveLocationAccessList.length > 0}
		<details class="access-list">
			<summary>Who can see my location ({$filteredLiveLocationAccessList.length})</summary>
			<ul>
				{#each $filteredLiveLocationAccessList as userId}
					<li>{userId.substring(0, 8)}...</li>
				{/each}
			</ul>
		</details>
	{/if}
</div>

<style>
	.live-location-toggle {
		background: var(--color-bg-secondary, #f5f5f5);
		border: 1px solid var(--color-border, #ddd);
		border-radius: 8px;
		padding: 1rem;
		margin: 1rem 0;
	}

	.toggle-header {
		display: flex;
		justify-content: space-between;
		align-items: center;
		margin-bottom: 0.5rem;
	}

	.toggle-header h3 {
		margin: 0;
		font-size: 1.1rem;
	}

	.toggle-button {
		padding: 0.5rem 1rem;
		border: 2px solid var(--color-border, #ddd);
		border-radius: 20px;
		background: white;
		cursor: pointer;
		font-weight: 600;
		transition: all 0.2s ease;
	}

	.toggle-button:hover {
		background: var(--color-bg-hover, #f0f0f0);
	}

	.toggle-button.active {
		background: var(--color-success, #4caf50);
		color: white;
		border-color: var(--color-success, #4caf50);
	}

	.error {
		background: var(--color-error-bg, #fee);
		color: var(--color-error, #c00);
		padding: 0.5rem;
		border-radius: 4px;
		margin-top: 0.5rem;
	}

	.location-info {
		background: white;
		border-radius: 4px;
		padding: 0.75rem;
		margin-top: 0.5rem;
	}

	.info-row {
		display: flex;
		justify-content: space-between;
		padding: 0.25rem 0;
	}

	.info-row .label {
		font-weight: 600;
		color: var(--color-text-secondary, #666);
	}

	.info-row .value {
		font-family: monospace;
	}

	.loading {
		text-align: center;
		padding: 1rem;
		font-style: italic;
		color: var(--color-text-secondary, #666);
	}

	.description {
		color: var(--color-text-secondary, #666);
		font-size: 0.9rem;
		margin-top: 0.5rem;
		line-height: 1.4;
	}

	.access-list {
		margin-top: 1rem;
		border-top: 1px solid var(--color-border, #ddd);
		padding-top: 0.5rem;
	}

	.access-list summary {
		cursor: pointer;
		font-weight: 600;
		padding: 0.5rem 0;
		user-select: none;
	}

	.access-list summary:hover {
		color: var(--color-primary, #007bff);
	}

	.access-list ul {
		list-style: none;
		padding: 0.5rem 0 0 0;
		margin: 0;
	}

	.access-list li {
		padding: 0.25rem 0.5rem;
		font-family: monospace;
		font-size: 0.9rem;
	}
</style>
