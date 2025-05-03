<script lang="ts">
	import {
		MapLibre,
		GeoJSONSource,
		CircleLayer,
		Marker,
		GlobeControl,
		QuerySourceFeatures,
		QueryRenderedFeatures
	} from 'svelte-maplibre-gl';
	import maplibregl from 'maplibre-gl';

	let features: maplibregl.MapGeoJSONFeature[] = $state.raw([]);
	let mode: 'source' | 'rendered' = $state('source');
</script>

<div class="flex h-[55vh] min-h-[300px] gap-x-3">
	<MapLibre
		class="h-[55vh] min-h-[300px] grow"
		style="https://basemaps.cartocdn.com/gl/voyager-gl-style/style.json"
		zoom={3}
		center={{ lng: 120, lat: 20 }}
	>
		<GlobeControl />
		<GeoJSONSource
			id="earthquakes"
			data={'https://maplibre.org/maplibre-gl-js/docs/assets/significant-earthquakes-2015.geojson'}
			promoteId="ids"
		>
			{#if mode == 'source'}
				<!-- map.querySourceFeatures() -->
				<QuerySourceFeatures bind:features>
					{#snippet children(feature: maplibregl.MapGeoJSONFeature)}
						{#if feature.geometry.type === 'Point'}
							<Marker lnglat={feature.geometry.coordinates as [number, number]} />
						{/if}
					{/snippet}
				</QuerySourceFeatures>
			{/if}
			<CircleLayer paint={{ 'circle-color': 'red', 'circle-radius': 4 }}>
				{#if mode == 'rendered'}
					<!-- map.queryRenderedFeatures() -->
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
	</MapLibre>
</div>
