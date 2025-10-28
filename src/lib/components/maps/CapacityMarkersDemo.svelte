<script lang="ts">
	import {
		MapLibre,
		GeolocateControl,
		Marker,
		Popup,
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
		formatCapacityPopupContent,
		type CapacityMarkerData
	} from '$lib/utils/capacityMarkers';
	import type { Commitment } from '$lib/commons/v5/schemas';
	
	// Legacy type alias
	type Capacity = Commitment;

	interface Props {
		onCapacityUpdate?: (id: string, lnglat: { lng: number; lat: number }) => void;
	}

	let { onCapacityUpdate }: Props = $props();

	// UI state
	let map: maplibregl.Map | undefined = $state.raw();
	let pitch = $state(0);
	let isTerrainVisible = $state(false);
	let isGlobeMode = $state(false);

	// Convert capacities to markers with reactive coordinates
	const capacityMarkers = $derived(() => {
		if (!$userCapacitiesWithShares) return [];

		return getCapacitiesWithCoordinates($userCapacitiesWithShares).map((marker) => {
			// Create reactive state for each marker's coordinates
			const lnglat = $state({ lng: marker.lnglat.lng, lat: marker.lnglat.lat });
			return {
				...marker,
				lnglat
			};
		});
	});

	// Simple coordinate change handler
	function handleCoordinateChange(id: string, lnglat: { lng: number; lat: number }) {
		if (onCapacityUpdate) {
			onCapacityUpdate(id, lnglat);
		} else {
			console.log('Capacity marker moved:', { id, newCoordinates: lnglat });
		}
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

		<!-- User location controls -->
		<GeolocateControl
			position="top-left"
			positionOptions={{ enableHighAccuracy: true }}
			trackUserLocation={true}
			showAccuracyCircle={true}
			autoTrigger={true}
		/>

		<!-- Map controls -->
		<CustomControl position="top-right">
			<button
				onclick={() => {
					isGlobeMode = !isGlobeMode;
				}}
				class="map-control-btn"
				title="Toggle globe mode"
			>
				<span>🌐</span>
			</button>
		</CustomControl>
		<CustomControl position="top-right">
			<button
				onclick={() => {
					isTerrainVisible = !isTerrainVisible;
					if (map) map.setPitch(isTerrainVisible ? 70 : 0);
				}}
				class="map-control-btn"
				title="Toggle terrain"
			>
				<span>🏔️</span>
			</button>
		</CustomControl>

		<!-- Terrain layer -->
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

		<!-- Capacity markers - much simpler! -->
		{#each capacityMarkers as markerData (markerData.id)}
			{@const { id, capacity } = markerData}
			{@const popupContent = formatCapacityPopupContent(capacity)}
			{@const lngLatText = `${markerData.lnglat.lat.toFixed(6)}, ${markerData.lnglat.lng.toFixed(6)}`}

			<Marker
				lnglat={{ lng: markerData.lnglat.lng, lat: markerData.lnglat.lat }}
				draggable={true}
				color="#3b82f6"
				scale={1.2}
				ondragend={(event) => {
					const newLnglat = event.target.getLngLat();
					handleCoordinateChange(id, { lng: newLnglat.lng, lat: newLnglat.lat });
				}}
			>
				{#snippet content()}
					<div class="capacity-marker">
						<div class="marker-icon">{capacity.emoji || '📍'}</div>
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
	</MapLibre>
</div>

<!-- Capacity list sidebar -->
{#if capacityMarkers.length > 0}
	<div class="capacity-list">
		<h4 class="list-title">Capacity Locations ({capacityMarkers.length})</h4>
		{#each capacityMarkers as markerData}
			<button
				class="capacity-item"
				onclick={() => {
					if (map) {
						map.flyTo({
							center: [markerData.lnglat.lng, markerData.lnglat.lat],
							zoom: 15
						});
					}
				}}
			>
				<div class="item-name">{markerData.capacity.name}</div>
				{#if markerData.capacity.quantity}
					<div class="item-quantity">
						{markerData.capacity.quantity}{markerData.capacity.unit
							? ` ${markerData.capacity.unit}`
							: ''}
					</div>
				{/if}
				<div class="item-coordinates">
					{markerData.lnglat.lat.toFixed(4)}, {markerData.lnglat.lng.toFixed(4)}
				</div>
			</button>
		{/each}
	</div>
{/if}

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
		font-size: 24px;
		text-shadow: 0 2px 4px rgba(0, 0, 0, 0.3);
	}

	.marker-label {
		background: rgba(255, 255, 255, 0.9);
		padding: 2px 6px;
		border-radius: 12px;
		font-size: 12px;
		font-weight: 500;
		color: #374151;
		white-space: nowrap;
		max-width: 120px;
		overflow: hidden;
		text-overflow: ellipsis;
		margin-top: 4px;
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

	.capacity-list {
		width: 280px;
		background: white;
		border-left: 1px solid #e5e7eb;
		padding: 16px;
		overflow-y: auto;
	}

	.list-title {
		font-size: 14px;
		font-weight: 600;
		color: #374151;
		margin: 0 0 12px 0;
	}

	.capacity-item {
		display: block;
		width: 100%;
		text-align: left;
		padding: 12px;
		border: 1px solid #e5e7eb;
		border-radius: 8px;
		background: white;
		cursor: pointer;
		transition: all 0.2s ease;
		margin-bottom: 8px;
	}

	.capacity-item:hover {
		background: #f9fafb;
		border-color: #3b82f6;
	}

	.item-name {
		font-weight: 500;
		color: #111827;
		margin-bottom: 4px;
	}

	.item-quantity {
		font-size: 12px;
		color: #6b7280;
	}

	.item-coordinates {
		font-size: 11px;
		color: #9ca3af;
		font-family: monospace;
		margin-top: 2px;
	}
</style>
