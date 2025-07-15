<script lang="ts">
	import {
		currentLocation,
		currentLocationText,
		isLocationTracking,
		locationError
	} from '$lib/state/location.svelte';
</script>

<div class="location-demo">
	<h3>Live Location Demo</h3>

	<div class="status-grid">
		<div class="status-item">
			<label>Tracking Status:</label>
			<span class="status-value {$isLocationTracking ? 'active' : 'inactive'}">
				{$isLocationTracking ? '🟢 Active' : '🔴 Inactive'}
			</span>
		</div>

		<div class="status-item">
			<label>Current Location:</label>
			<span class="location-value">{$currentLocationText}</span>
		</div>

		{#if $locationError}
			<div class="status-item error">
				<label>Error:</label>
				<span class="error-value">{$locationError}</span>
			</div>
		{/if}

		{#if $currentLocation}
			<div class="location-details">
				<h4>Location Details:</h4>
				<div class="details-grid">
					<div class="detail-item">
						<label>Latitude:</label>
						<span>{$currentLocation.latitude.toFixed(6)}</span>
					</div>
					<div class="detail-item">
						<label>Longitude:</label>
						<span>{$currentLocation.longitude.toFixed(6)}</span>
					</div>
					{#if $currentLocation.accuracy}
						<div class="detail-item">
							<label>Accuracy:</label>
							<span>{$currentLocation.accuracy.toFixed(1)}m</span>
						</div>
					{/if}
					{#if $currentLocation.altitude !== null && $currentLocation.altitude !== undefined}
						<div class="detail-item">
							<label>Altitude:</label>
							<span>{$currentLocation.altitude.toFixed(1)}m</span>
						</div>
					{/if}
					{#if $currentLocation.speed !== null && $currentLocation.speed !== undefined}
						<div class="detail-item">
							<label>Speed:</label>
							<span>{$currentLocation.speed.toFixed(1)} m/s</span>
						</div>
					{/if}
					<div class="detail-item">
						<label>Last Updated:</label>
						<span>{new Date($currentLocation.timestamp).toLocaleTimeString()}</span>
					</div>
				</div>
			</div>
		{/if}
	</div>
</div>

<style>
	.location-demo {
		background: white;
		border-radius: 8px;
		padding: 16px;
		box-shadow: 0 2px 8px rgba(0, 0, 0, 0.1);
		border: 1px solid #e5e7eb;
		max-width: 500px;
		margin: 16px 0;
	}

	.location-demo h3 {
		margin: 0 0 16px 0;
		color: #111827;
		font-size: 18px;
		font-weight: 600;
	}

	.status-grid {
		display: flex;
		flex-direction: column;
		gap: 12px;
	}

	.status-item {
		display: flex;
		justify-content: space-between;
		align-items: center;
		padding: 8px 0;
		border-bottom: 1px solid #f3f4f6;
	}

	.status-item:last-child {
		border-bottom: none;
	}

	.status-item.error {
		color: #dc2626;
	}

	.status-item label {
		font-weight: 500;
		color: #6b7280;
	}

	.status-value.active {
		color: #22c55e;
		font-weight: 500;
	}

	.status-value.inactive {
		color: #6b7280;
		font-weight: 500;
	}

	.location-value {
		font-family: monospace;
		color: #111827;
		background: #f9fafb;
		padding: 4px 8px;
		border-radius: 4px;
		font-size: 12px;
	}

	.error-value {
		color: #dc2626;
		font-size: 12px;
	}

	.location-details {
		margin-top: 16px;
		padding-top: 16px;
		border-top: 2px solid #f3f4f6;
	}

	.location-details h4 {
		margin: 0 0 12px 0;
		color: #111827;
		font-size: 14px;
		font-weight: 600;
	}

	.details-grid {
		display: grid;
		grid-template-columns: 1fr 1fr;
		gap: 8px;
	}

	.detail-item {
		display: flex;
		flex-direction: column;
		gap: 2px;
	}

	.detail-item label {
		font-size: 11px;
		font-weight: 500;
		color: #9ca3af;
		text-transform: uppercase;
		letter-spacing: 0.05em;
	}

	.detail-item span {
		font-family: monospace;
		font-size: 12px;
		color: #111827;
		background: #f9fafb;
		padding: 2px 6px;
		border-radius: 3px;
	}
</style>
