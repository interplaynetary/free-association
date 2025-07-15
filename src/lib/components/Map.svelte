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
	import { userCapacitiesWithShares, userCapacities } from '$lib/state/core.svelte';
	import { globalState } from '$lib/global.svelte';
	import {
		getCapacitiesWithCoordinates,
		formatCapacityPopupContent,
		getAllCapacityMarkers,
		type CapacityMarkerData
	} from '$lib/utils/capacityMarkers';
	import {
		reverseGeocode,
		parseAddressComponents,
		type ReverseGeocodeResult
	} from '$lib/utils/geocoding';
	import {
		updateLocation,
		setLocationTracking,
		setLocationError,
		currentLocation,
		currentLocationText,
		isLocationTracking
	} from '$lib/state/location.svelte';
	import { debounce } from '$lib/state/subscriptions.svelte';

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

	// Capacity markers state (includes both coordinate and geocoded markers)
	let capacitiesWithCoords = $state<CapacityMarkerData[]>([]);
	let isLoadingGeocode = $state(false);

	// Async function to load markers
	async function loadCapacityMarkers(capacities: typeof $userCapacitiesWithShares) {
		if (!capacities) {
			console.log('[Map] No userCapacitiesWithShares available');
			capacitiesWithCoords = [];
			return;
		}

		console.log('[Map] userCapacitiesWithShares:', capacities);
		console.log('[Map] Total capacities:', Object.keys(capacities).length);

		// Check each capacity for coordinates and addresses
		Object.entries(capacities).forEach(([id, capacity]) => {
			console.log(`[Map] Capacity ${id}:`, {
				name: capacity.name,
				location_type: capacity.location_type,
				latitude: capacity.latitude,
				longitude: capacity.longitude,
				hasValidCoords: capacity.latitude !== undefined && capacity.longitude !== undefined,
				hasAddress: !!(
					capacity.street_address ||
					capacity.city ||
					capacity.state_province ||
					capacity.postal_code ||
					capacity.country
				)
			});
		});

		try {
			isLoadingGeocode = true;
			const markers = await getAllCapacityMarkers(capacities);
			capacitiesWithCoords = markers;
			console.log(
				`[Map] Found ${markers.length} total markers (coordinate + geocoded):`,
				markers
			);
		} catch (error) {
			console.error('[Map] Error getting capacity markers:', error);
			// Fallback to just coordinate-based markers
			capacitiesWithCoords = getCapacitiesWithCoordinates(capacities);
		} finally {
			isLoadingGeocode = false;
		}
	}

	// Create a debounced version of loadCapacityMarkers to prevent excessive geocoding
	// when userCapacitiesWithShares updates frequently as new capacities and shares stream in
	const debouncedLoadCapacityMarkers = debounce(loadCapacityMarkers, 500);

	// Reactively update markers when userCapacitiesWithShares changes
	$effect(() => {
		debouncedLoadCapacityMarkers($userCapacitiesWithShares);
	});

	// Update capacity coordinates in the store
	async function handleCapacityCoordinateChange(id: string, lnglat: { lng: number; lat: number }) {
		try {
			// Call the optional callback first
		if (onCapacityUpdate) {
			onCapacityUpdate(id, lnglat);
			}

			// Update the capacity in the store
			if ($userCapacities && $userCapacities[id]) {
				// Create a deep clone of current capacities
				const newCapacities = structuredClone($userCapacities);

				// Update the specific capacity's coordinates
				newCapacities[id] = {
					...newCapacities[id],
					longitude: lnglat.lng,
					latitude: lnglat.lat
				};

				// Try to reverse geocode to get address (optional, don't fail if it doesn't work)
				try {
					console.log(`[Map] Reverse geocoding coordinates: ${lnglat.lat}, ${lnglat.lng}`);
					const reverseResult = await reverseGeocode(lnglat.lat, lnglat.lng);
					const addressComponents = parseAddressComponents(reverseResult.address);

					// Update address fields if reverse geocoding succeeded
					newCapacities[id] = {
						...newCapacities[id],
						...addressComponents
					};

					console.log(`[Map] Reverse geocoded address:`, addressComponents);
				} catch (geocodeError) {
					// Don't fail the coordinate update if geocoding fails
					console.warn(
						'[Map] Reverse geocoding failed, coordinates updated without address:',
						geocodeError
					);
				}

				// Set the store with the new value
				userCapacities.set(newCapacities);

				console.log(`[Map] Updated capacity ${id} coordinates:`, lnglat);
				globalState.showToast(`Updated location for ${newCapacities[id].name}`, 'success');
		} else {
				console.warn(`[Map] Capacity ${id} not found in user capacities`);
			}
		} catch (error) {
			console.error('[Map] Error updating capacity coordinates:', error);
			globalState.showToast('Error updating location', 'error');
		}
	}

	// Geolocation event handlers
	function handleGeolocate(event: GeolocationPosition) {
		updateLocation(event.coords, event.timestamp);
	}

	function handleTrackUserLocationStart() {
		console.log('[Map] Location tracking started');
		setLocationTracking(true);
	}

	function handleTrackUserLocationEnd() {
		console.log('[Map] Location tracking ended');
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

<div class="flex h-[400px] max-h-[50vh] min-h-[300px] overflow-hidden rounded-md">
	<MapLibre
		bind:map
		bind:pitch
		class="map-container flex-1"
		style="https://basemaps.cartocdn.com/gl/voyager-gl-style/style.json"
		zoom={3}
		center={{ lng: 120, lat: 20 }}
		minZoom={1}
		maxPitch={85}
		attributionControl={false}
		touchZoomRotate={true}
		touchPitch={true}
		dragPan={true}
		scrollZoom={true}
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

		<!-- User capacity markers - both coordinate and geocoded from addresses -->
		{#each capacitiesWithCoords as markerData (markerData.id)}
			{@const { id, capacity, lnglat, source } = markerData}
			{@const popupContent = formatCapacityPopupContent(capacity)}
			{@const lngLatText = `${lnglat.lat.toFixed(6)}, ${lnglat.lng.toFixed(6)}`}
			{@const isGeocoded = source === 'geocoded'}

			<Marker
				lnglat={{ lng: lnglat.lng, lat: lnglat.lat }}
				draggable={!isGeocoded}
				color={isGeocoded ? '#f59e0b' : '#3b82f6'}
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
						{#if isGeocoded}
							<div class="geocoded-badge">📍</div>
						{/if}
					</div>
				{/snippet}

				<Popup anchor="bottom" offset={[0, -10]} closeButton={true}>
					<div class="capacity-popup">
						<h3 class="popup-title">{popupContent.title}</h3>
						{#if isGeocoded}
							<div class="geocoded-notice">
								<small>📍 Location from address geocoding</small>
							</div>
						{/if}
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

		<!-- Loading indicator for geocoding -->
		{#if isLoadingGeocode}
			<CustomControl position="bottom-right">
				<div class="geocoding-loading">
					<span>🌍 Finding addresses...</span>
				</div>
			</CustomControl>
		{/if}

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

	.geocoded-badge {
		position: absolute;
		top: -8px;
		right: -8px;
		background: #f59e0b;
		color: white;
		border-radius: 50%;
		width: 16px;
		height: 16px;
		display: flex;
		align-items: center;
		justify-content: center;
		font-size: 8px;
		font-weight: bold;
		box-shadow: 0 1px 3px rgba(0, 0, 0, 0.3);
	}

	.geocoded-notice {
		background: #fef3c7;
		border: 1px solid #f59e0b;
		border-radius: 4px;
		padding: 4px 8px;
		margin-bottom: 8px;
		color: #92400e;
		text-align: center;
	}

	.geocoding-loading {
		background: rgba(255, 255, 255, 0.95);
		border-radius: 8px;
		padding: 8px 12px;
		box-shadow: 0 2px 8px rgba(0, 0, 0, 0.15);
		border: 1px solid #e5e7eb;
		font-size: 12px;
		color: #6b7280;
		font-weight: 500;
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

	/* Ensure map doesn't interfere with page scrolling */
	:global(.map-container) {
		/* Contain touch events within the map */
		touch-action: pan-x pan-y zoom-in zoom-out;
		/* Prevent map from capturing touch events outside its bounds */
		isolation: isolate;
	}

	:global(.maplibregl-canvas) {
		/* Ensure map canvas doesn't interfere with page scrolling */
		touch-action: pan-x pan-y zoom-in zoom-out;
	}
</style>
