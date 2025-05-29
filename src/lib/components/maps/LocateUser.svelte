<script lang="ts">
	import {
		MapLibre,
		GeolocateControl,
		GeoJSONSource,
		CircleLayer,
		Marker,
		GlobeControl,
		QuerySourceFeatures,
		QueryRenderedFeatures,
		FillExtrusionLayer,
		CustomControl
	} from 'svelte-maplibre-gl';
	import maplibregl from 'maplibre-gl';

	let features: maplibregl.MapGeoJSONFeature[] = $state.raw([]);
	let mode: 'source' | 'rendered' = $state('source');
	let show3DBuildings = $state(false);
	let map: maplibregl.Map | undefined = $state.raw();
	let pitch = $state(0);
</script>

<div class="flex h-[55vh] min-h-[300px] overflow-hidden rounded-md">
	<MapLibre
		bind:map
		bind:pitch
		class="flex-1"
		style="https://basemaps.cartocdn.com/gl/voyager-gl-style/style.json"
		zoom={3}
		center={{ lng: 120, lat: 20 }}
		minZoom={2}
		attributionControl={false}
	>
		<GeolocateControl
			position="top-left"
			positionOptions={{ enableHighAccuracy: true }}
			trackUserLocation={true}
			showAccuracyCircle={true}
			ontrackuserlocationstart={() => console.log('trackuserlocationstart')}
			ontrackuserlocationend={() => console.log('trackuserlocationend')}
			ongeolocate={(ev) => console.log(`geolocate ${JSON.stringify(ev.coords, null, 2)}`)}
		/>
		<GlobeControl position="top-right" />
		<CustomControl position="top-right">
			<button
				onclick={() => {
					show3DBuildings = !show3DBuildings;
					if (show3DBuildings && map) {
						map.setPitch(70);
					} else if (map) {
						map.setPitch(0);
					}
				}}
			>
				<span>🏢</span>
			</button>
		</CustomControl>
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
			<CircleLayer paint={{ 'circle-color': 'red', 'circle-radius': 4 }}>
				{#if mode === 'rendered'}
					<QueryRenderedFeatures bind:features>
						{#snippet children(feature: maplibregl.MapGeoJSONFeature)}
							{#if feature.geometry.type === 'Point'}
								<Marker lnglat={feature.geometry.coordinates as [number, number]} />
							{/if}
						{/snippet}
					</QueryRenderedFeatures>
				{/if}
			</CircleLayer>
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
