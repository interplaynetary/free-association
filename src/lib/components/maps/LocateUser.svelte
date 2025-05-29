<script lang="ts">
	import {
		MapLibre,
		GeolocateControl,
		GeoJSONSource,
		CircleLayer,
		Marker,
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

	let features: maplibregl.MapGeoJSONFeature[] = $state.raw([]);
	let mode: 'source' = $state('source');
	let show3DBuildings = $state(false);
	let map: maplibregl.Map | undefined = $state.raw();
	let pitch = $state(0);
	let isTerrainVisible = $state(false);
	let isGlobeMode = $state(false);
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
			ontrackuserlocationstart={() => console.log('trackuserlocationstart')}
			ontrackuserlocationend={() => console.log('trackuserlocationend')}
			ongeolocate={(ev) => console.log(`geolocate ${JSON.stringify(ev.coords, null, 2)}`)}
		/>
		<CustomControl position="top-right">
			<button
				onclick={() => {
					isGlobeMode = !isGlobeMode;
				}}
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
			>
				<span>🏔️</span>
			</button>
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
							<Marker lnglat={feature.geometry.coordinates as [number, number]} />
						{/if}
					{/snippet}
				</QuerySourceFeatures>
			{/if}
		</GeoJSONSource>

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
