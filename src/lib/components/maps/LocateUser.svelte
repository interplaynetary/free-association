<script lang="ts">
	import {
		MapLibre,
		GeolocateControl,
		GeoJSONSource,
		CircleLayer,
		Marker,
		Popup,
		QuerySourceFeatures,
		QueryRenderedFeatures,
		FillExtrusionLayer,
		CustomControl,
		RasterDEMTileSource,
		HillshadeLayer,
		Terrain,
		Light,
		Sky,
		Projection
	} from 'svelte-maplibre-gl';
	import maplibregl from 'maplibre-gl';
	import { userCapacitiesWithShares } from '$lib/state/core.svelte';
	import {
		getCapacitiesWithCoordinates,
		formatCapacityPopupContent
	} from '$lib/utils/capacityMarkers';
	import {
		updateLocation,
		setLocationTracking,
		setLocationError,
		currentLocation,
		currentLocationText,
		isLocationTracking
	} from '$lib/state/location.svelte';

	interface Props {
		onCapacityUpdate?: (id: string, lnglat: { lng: number; lat: number }) => void;
	}

	let { onCapacityUpdate }: Props = $props();

	let features: maplibregl.MapGeoJSONFeature[] = $state.raw([]);
	let mode: 'source' = $state('source');
	let show3DBuildings = $state(false);
	let map: maplibregl.Map | undefined = $state.raw();
	let pitch = $state(0);
	let isTerrainVisible = $state(false);
	let isGlobeMode = $state(false);

	// Simple derived store to get capacities with coordinates
	const capacitiesWithCoords = $derived(() => {
		if (!$userCapacitiesWithShares) {
			console.log('[LocateUser] No userCapacitiesWithShares available');
			return [];
		}

		console.log('[LocateUser] userCapacitiesWithShares:', $userCapacitiesWithShares);
		console.log('[LocateUser] Total capacities:', Object.keys($userCapacitiesWithShares).length);

		// Check each capacity for coordinates
		Object.entries($userCapacitiesWithShares).forEach(([id, capacity]) => {
			console.log(`[LocateUser] Capacity ${id}:`, {
				name: capacity.name,
				location_type: capacity.location_type,
				latitude: capacity.latitude,
				longitude: capacity.longitude,
				hasValidCoords: capacity.latitude !== undefined && capacity.longitude !== undefined
			});
		});

		const capacities = getCapacitiesWithCoordinates($userCapacitiesWithShares);
		console.log(`[LocateUser] Found ${capacities.length} capacities with coordinates:`, capacities);
		return capacities;
	});

	// Simple coordinate change handler
	function handleCapacityCoordinateChange(id: string, lnglat: { lng: number; lat: number }) {
		if (onCapacityUpdate) {
			onCapacityUpdate(id, lnglat);
		} else {
			console.log('Capacity marker moved:', { id, newCoordinates: lnglat });
		}
	}

	// Geolocation event handlers
	function handleGeolocate(event: GeolocationPosition) {
		updateLocation(event.coords, event.timestamp);
	}

	function handleTrackUserLocationStart() {
		console.log('[LocateUser] Location tracking started');
		setLocationTracking(true);
	}

	function handleTrackUserLocationEnd() {
		console.log('[LocateUser] Location tracking ended');
		setLocationTracking(false);
	}

	function handleGeolocateError(event: GeolocationPositionError) {
		let errorMessage: string;

		switch (event.code) {
			case event.PERMISSION_DENIED:
				errorMessage = 'Location access denied by user.';
				break;
			case event.POSITION_UNAVAILABLE:
				errorMessage = 'Location information is unavailable.';
				break;
			case event.TIMEOUT:
				errorMessage = 'Location request timed out.';
				break;
			default:
				errorMessage = 'An unknown error occurred while retrieving location.';
				break;
		}

		setLocationError(errorMessage);
	}
</script>

<div class="flex h-[55vh] min-h-[300px] overflow-hidden rounded-md">
	<MapLibre
		bind:map
		bind:pitch
		class="flex-1"
		style="https://basemaps.cartocdn.com/gl/voyager-gl-style/style.json"
		zoom={3}
		center={{ lng: 120, lat: 20 }}
		minZoom={1}
		maxPitch={85}
		attributionControl={false}
	>
		<Projection type={isGlobeMode ? 'globe' : undefined} />
		<Light anchor="map" />
		<Sky
			sky-color="#001560"
			horizon-color="#0090c0"
			fog-color="#ffffff"
			sky-horizon-blend={0.9}
			horizon-fog-blend={0.8}
			fog-ground-blend={0.7}
			atmosphere-blend={['interpolate', ['linear'], ['zoom'], 2, 0.8, 4, 0.3, 7, 0]}
		/>
		<GeolocateControl
			position="top-left"
			positionOptions={{ enableHighAccuracy: true }}
			trackUserLocation={true}
			showAccuracyCircle={true}
			autoTrigger={true}
			ontrackuserlocationstart={handleTrackUserLocationStart}
			ontrackuserlocationend={handleTrackUserLocationEnd}
			ongeolocate={handleGeolocate}
			onerror={handleGeolocateError}
		/>
		<CustomControl position="top-right">
			<button
				onclick={() => {
					isGlobeMode = !isGlobeMode;
				}}
				title="Toggle globe mode"
			>
				<span>🌐</span>
			</button>
		</CustomControl>
		<CustomControl position="top-right">
			<button
				onclick={() => {
					show3DBuildings = !show3DBuildings;
					if (map) map.setPitch(show3DBuildings ? 70 : 0);
				}}
				title="Toggle 3D buildings"
			>
				<span>🏢</span>
			</button>
		</CustomControl>
		<CustomControl position="top-right">
			<button
				onclick={() => {
					isTerrainVisible = !isTerrainVisible;
					if (map) map.setPitch(isTerrainVisible ? 70 : 0);
				}}
				title="Toggle terrain"
			>
				<span>🏔️</span>
			</button>
		</CustomControl>

		<!-- Location status display -->
		<CustomControl position="bottom-left">
			<div class="location-status">
				<div class="status-indicator {$isLocationTracking ? 'tracking' : 'not-tracking'}">
					{$isLocationTracking ? '📍' : '📍'}
				</div>
				<div class="location-text">
					{$currentLocationText}
				</div>
			</div>
		</CustomControl>

		<RasterDEMTileSource
			id="terrain"
			tiles={['https://s3.amazonaws.com/elevation-tiles-prod/terrarium/{z}/{x}/{y}.png']}
			minzoom={2}
			maxzoom={15}
			encoding="terrarium"
			attribution="<a href='https://github.com/tilezen/joerd/blob/master/docs/attribution.md'>Mapzen (Terrain)</a>"
		>
			{#if isTerrainVisible}
				<Terrain exaggeration={1.0} />
				<HillshadeLayer
					paint={{
						'hillshade-exaggeration': 0.5,
						'hillshade-illumination-anchor': 'map',
						'hillshade-shadow-color': '#473B24',
						'hillshade-accent-color': '#aaff00',
						'hillshade-highlight-color': '#ffffff'
					}}
				/>
			{/if}
		</RasterDEMTileSource>

		<GeoJSONSource
			id="capacities"
			data={'https://maplibre.org/maplibre-gl-js/docs/assets/significant-earthquakes-2015.geojson'}
			promoteId="ids"
		>
			{#if mode === 'source'}
				<QuerySourceFeatures bind:features>
					{#snippet children(feature: maplibregl.MapGeoJSONFeature)}
						{#if feature.geometry.type === 'Point'}
							<Marker
								lnglat={feature.geometry.coordinates as [number, number]}
								color="#ef4444"
								scale={0.8}
							/>
						{/if}
					{/snippet}
				</QuerySourceFeatures>
			{/if}
		</GeoJSONSource>

		<!-- User capacity markers - simple approach like MarkerPopup.svelte -->
		{#each capacitiesWithCoords as markerData (markerData.id)}
			{@const { id, capacity, lnglat } = markerData}
			{@const popupContent = formatCapacityPopupContent(capacity)}
			{@const lngLatText = `${lnglat.lat.toFixed(6)}, ${lnglat.lng.toFixed(6)}`}

			<Marker
				lnglat={{ lng: lnglat.lng, lat: lnglat.lat }}
				draggable={true}
				color="#3b82f6"
				scale={1.2}
				ondragend={(event) => {
					const newLnglat = event.target.getLngLat();
					handleCapacityCoordinateChange(id, { lng: newLnglat.lng, lat: newLnglat.lat });
				}}
			>
				{#snippet content()}
					<div class="capacity-marker">
						<div class="marker-icon">{capacity.emoji || '🏠'}</div>
						<div class="marker-label">{capacity.name}</div>
					</div>
				{/snippet}

				<Popup anchor="bottom" offset={[0, -10]} closeButton={true}>
					<div class="capacity-popup">
						<h3 class="popup-title">{popupContent.title}</h3>
						<div class="popup-details">
							{#each popupContent.details as detail}
								<div class="detail-row">
									<span class="detail-label">{detail.label}:</span>
									<span class="detail-value">{detail.value}</span>
								</div>
							{/each}
						</div>
						<div class="coordinates">
							<small class="coordinate-text">{lngLatText}</small>
						</div>
					</div>
				</Popup>
			</Marker>
		{/each}

		{#if show3DBuildings}
			<FillExtrusionLayer
				source="carto"
				sourceLayer="building"
				minzoom={14}
				filter={['!=', ['get', 'hide_3d'], true]}
				paint={{
					'fill-extrusion-color': [
						'interpolate',
						['linear'],
						['get', 'render_height'],
						0,
						'#aaccbb',
						200,
						'royalblue',
						400,
						'purple'
					],
					'fill-extrusion-height': [
						'interpolate',
						['linear'],
						['zoom'],
						14,
						0,
						15,
						['get', 'render_height']
					],
					'fill-extrusion-base': [
						'case',
						['>=', ['get', 'zoom'], 14],
						['get', 'render_min_height'],
						0
					]
				}}
			/>
		{/if}
	</MapLibre>
</div>

<style>
	.map-control-btn {
		background: white;
		border: 1px solid #ccc;
		border-radius: 4px;
		padding: 8px;
		margin: 2px;
		cursor: pointer;
		box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1);
		transition: all 0.2s ease;
	}

	.map-control-btn:hover {
		background: #f5f5f5;
		transform: scale(1.05);
	}

	.capacity-marker {
		display: flex;
		flex-direction: column;
		align-items: center;
		cursor: pointer;
		transition: transform 0.2s ease;
	}

	.capacity-marker:hover {
		transform: scale(1.1);
	}

	.marker-icon {
		font-size: 20px;
		text-shadow: 0 2px 4px rgba(0, 0, 0, 0.3);
	}

	.marker-label {
		background: rgba(255, 255, 255, 0.9);
		padding: 2px 6px;
		border-radius: 12px;
		font-size: 11px;
		font-weight: 500;
		color: #374151;
		white-space: nowrap;
		max-width: 100px;
		overflow: hidden;
		text-overflow: ellipsis;
		margin-top: 2px;
		box-shadow: 0 1px 3px rgba(0, 0, 0, 0.2);
	}

	.capacity-popup {
		min-width: 200px;
		max-width: 300px;
	}

	.popup-title {
		font-size: 16px;
		font-weight: 600;
		color: #111827;
		margin: 0 0 12px 0;
	}

	.popup-details {
		display: flex;
		flex-direction: column;
		gap: 6px;
	}

	.detail-row {
		display: flex;
		justify-content: space-between;
		align-items: center;
		font-size: 14px;
	}

	.detail-label {
		font-weight: 500;
		color: #6b7280;
	}

	.detail-value {
		color: #111827;
		text-align: right;
	}

	.coordinates {
		margin-top: 8px;
		padding-top: 8px;
		border-top: 1px solid #e5e7eb;
		text-align: center;
	}

	.coordinate-text {
		color: #9ca3af;
		font-family: monospace;
	}

	.location-status {
		background: rgba(255, 255, 255, 0.95);
		border-radius: 8px;
		padding: 8px 12px;
		box-shadow: 0 2px 8px rgba(0, 0, 0, 0.15);
		border: 1px solid #e5e7eb;
		font-size: 12px;
		min-width: 200px;
	}

	.status-indicator {
		display: flex;
		align-items: center;
		font-weight: 500;
		margin-bottom: 4px;
	}

	.status-indicator.tracking {
		color: #22c55e;
	}

	.status-indicator.not-tracking {
		color: #6b7280;
	}

	.location-text {
		font-family: monospace;
		font-size: 11px;
		color: #4b5563;
		line-height: 1.2;
	}
</style>
