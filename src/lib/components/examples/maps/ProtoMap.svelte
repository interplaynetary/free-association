<script context="module">
	// Declare the global type with the correct function signature
	declare global {
		interface Window {
			protomaps_themes_base?: {
				default: (sourceName: string, theme: string, language: string) => any[];
			};
		}
	}
</script>

<script lang="ts">
	import { onMount, onDestroy, createEventDispatcher } from 'svelte';
	import maplibregl from 'maplibre-gl';
	import { Protocol } from 'pmtiles';

	// Props
	export let mapId = 'map';
	export let initialCenter: [number, number] = [0, 0];
	export let initialZoom = 1;
	export let mapStyle: maplibregl.StyleSpecification | undefined = undefined;
	export let pmtilesUrl: string | undefined = undefined;
	export let pmtilesSourceName = 'protomaps';
	export let pmtilesSourceType: 'vector' | 'raster' | 'raster-dem' = 'vector';
	export let theme: 'light' | 'dark' | 'white' | 'black' | 'grayscale' = 'light';
	export let useTheme = false; // Whether to use the theme-based styling
	export let showSearch = true;
	export let showControls = true;
	export let showStyleControls = true;
	export let height = '500px';
	export let styles: Array<{
		id: string;
		name: string;
		url: string;
		type: 'vector' | 'raster' | 'raster-dem';
		center?: [number, number];
		zoom?: number;
	}> = [
		{
			id: 'vector',
			name: 'Vector',
			url: 'https://r2-public.protomaps.com/protomaps-sample-datasets/protomaps-basemap-opensource-20230408.pmtiles',
			type: 'vector'
		}
	];

	// Map instance
	let map: maplibregl.Map;
	let container: HTMLElement;
	let protocol: Protocol;
	let isLoading = true;
	let searchInput: HTMLInputElement;
	let searchQuery = '';
	let searchResults: any[] = [];
	let isSearching = false;
	let showSearchResults = false;
	let showLayersPanel = false;
	let showBasemapPanel = false;
	let activeStyleId = styles[0]?.id || '';
	let activeStyle = styles[0];
	let mapMode: 'vector' | 'styled' = 'vector';

	// Near the top of the script, expand layer visibility state
	let showLabels = true;
	let showRoads = true;
	let showBuildings = true;
	let showWater = true;
	let showLandmarks = true;
	let showLanduse = true;

	const themeOptions = [
		{ id: 'light' as const, name: 'Light', icon: '☀️' },
		{ id: 'dark' as const, name: 'Dark', icon: '🌙' },
		{ id: 'white' as const, name: 'White', icon: '⚪' },
		{ id: 'black' as const, name: 'Black', icon: '⚫' },
		{ id: 'grayscale' as const, name: 'Gray', icon: '🔘' }
	];

	const dispatch = createEventDispatcher();

	// Define source types to replace AnySourceData
	type VectorSourceSpecification = {
		type: 'vector';
		url: string;
		tiles?: string[];
		bounds?: [number, number, number, number];
		scheme?: 'xyz' | 'tms';
		minzoom?: number;
		maxzoom?: number;
		attribution?: string;
		promoteId?: string;
	};

	type RasterSourceSpecification = {
		type: 'raster';
		url: string;
		tiles?: string[];
		bounds?: [number, number, number, number];
		minzoom?: number;
		maxzoom?: number;
		tileSize?: number;
		scheme?: 'xyz' | 'tms';
		attribution?: string;
	};

	type RasterDEMSourceSpecification = {
		type: 'raster-dem';
		url: string;
		tiles?: string[];
		bounds?: [number, number, number, number];
		minzoom?: number;
		maxzoom?: number;
		tileSize?: number;
		attribution?: string;
		encoding?: 'terrarium' | 'mapbox';
	};

	// Union type for our source types
	type SourceSpecification =
		| VectorSourceSpecification
		| RasterSourceSpecification
		| RasterDEMSourceSpecification;

	// Define a proper interface for layer visibility state
	interface LayerVisibilityState {
		labels: boolean;
		roads: boolean;
		buildings: boolean;
		water: boolean;
		landmarks: boolean;
		landuse: boolean;
	}

	onMount(async () => {
		try {
			// Initialize PMTiles protocol handler
			protocol = new Protocol();
			maplibregl.addProtocol('pmtiles', protocol.tile);

			// If styles is provided, set the activeStyleId and activeStyle
			if (styles && styles.length > 0) {
				activeStyleId = styles[0].id;
				activeStyle = styles[0];
			}

			// If we're using a theme, generate the style
			if (useTheme && pmtilesUrl) {
				try {
					// Import the protomaps-themes-base from npm package
					const protomapsThemes = await import('protomaps-themes-base');

					// Ensure proper glyphs (font) loading for labels
					mapStyle = {
						version: 8,
						glyphs: 'https://protomaps.github.io/basemaps-assets/fonts/{fontstack}/{range}.pbf',
						sprite: `https://protomaps.github.io/basemaps-assets/sprites/v3/${theme}`,
						sources: {
							[pmtilesSourceName]: {
								type: 'vector',
								url: `pmtiles://${pmtilesUrl}`,
								attribution:
									'<a href="https://protomaps.com">Protomaps</a> © <a href="https://openstreetmap.org">OpenStreetMap</a>'
							}
						},
						layers: protomapsThemes.default(pmtilesSourceName, theme as any, 'en')
					};

					console.log('Creating themed map with layers:', mapStyle.layers?.length);
				} catch (error) {
					console.error('Error loading protomaps theme:', error);
				}
			}

			// Create the map
			map = new maplibregl.Map({
				container: mapId,
				style:
					mapStyle ||
					({
						version: 8,
						glyphs: 'https://protomaps.github.io/basemaps-assets/fonts/{fontstack}/{range}.pbf', // Always include glyphs
						sprite: `https://protomaps.github.io/basemaps-assets/sprites/v3/light`,
						sources: {},
						layers: []
					} as maplibregl.StyleSpecification),
				center: initialCenter,
				zoom: initialZoom
			});

			// Add navigation controls if enabled
			if (showControls) {
				map.addControl(new maplibregl.NavigationControl(), 'top-right');
				map.addControl(
					new maplibregl.GeolocateControl({
						positionOptions: {
							enableHighAccuracy: true
						},
						trackUserLocation: true
					}),
					'top-right'
				);
				map.addControl(new maplibregl.ScaleControl({}), 'bottom-left');
			}

			// Wait for map to load
			await new Promise<void>((resolve) => {
				map.on('load', () => {
					isLoading = false;
					console.log('Map loaded successfully');

					// Debug - log all layers after initial load
					setTimeout(() => {
						logAllLayers();
					}, 200);

					resolve();
				});
			});

			// Add PMTiles source if provided and not already using a themed style
			if (pmtilesUrl && !useTheme) {
				try {
					// Create source specification based on type
					const sourceSpec = {
						type: pmtilesSourceType,
						url: `pmtiles://${pmtilesUrl}`,
						attribution:
							'<a href="https://protomaps.com">Protomaps</a> © <a href="https://openstreetmap.org">OpenStreetMap</a>'
					};

					// Add the source
					map.addSource(pmtilesSourceName, sourceSpec);

					// For vector tile sources, add basic layer (if no custom style is provided)
					if (pmtilesSourceType === 'vector' && !mapStyle) {
						// Add a default layer for common source-layer names
						const commonSourceLayers = ['default', 'main', 'data', 'buildings', 'roads', 'water'];

						// Try each source layer
						for (const sourceLayer of commonSourceLayers) {
							try {
								map.addLayer({
									id: `${pmtilesSourceName}-${sourceLayer}-fill`,
									type: 'fill',
									source: pmtilesSourceName,
									'source-layer': sourceLayer,
									paint: {
										'fill-color': '#0080ff',
										'fill-opacity': 0.5,
										'fill-outline-color': '#0066cc'
									}
								});

								map.addLayer({
									id: `${pmtilesSourceName}-${sourceLayer}-line`,
									type: 'line',
									source: pmtilesSourceName,
									'source-layer': sourceLayer,
									paint: {
										'line-color': '#0080ff',
										'line-width': 1
									}
								});

								map.addLayer({
									id: `${pmtilesSourceName}-${sourceLayer}-point`,
									type: 'circle',
									source: pmtilesSourceName,
									'source-layer': sourceLayer,
									paint: {
										'circle-radius': 3,
										'circle-color': '#0080ff'
									}
								});
							} catch (error) {
								console.log(`Source layer "${sourceLayer}" not available or error adding it`);
							}
						}
					}
				} catch (error) {
					console.error('Error adding PMTiles source:', error);
				}
			}

			// Add click event handler
			map.on('click', (e) => {
				dispatch('mapclick', {
					lngLat: e.lngLat,
					point: e.point,
					originalEvent: e.originalEvent
				});
			});
		} catch (error) {
			console.error('Error initializing map:', error);
			isLoading = false;
		}
	});

	// Watch for theme changes
	$: if (map && useTheme && typeof window !== 'undefined') {
		updateTheme(theme);
	}

	async function updateTheme(newTheme: string) {
		try {
			console.log('Starting theme update to:', newTheme);

			// Dynamically update the theme
			const protomapsThemes = await import('protomaps-themes-base');

			if (map) {
				// Check if we have a valid PMTiles URL
				if (!pmtilesUrl) {
					console.error('No PMTiles URL provided for themed map');
					return;
				}

				console.log('Using PMTiles URL:', pmtilesUrl);

				// Follow the exact format from the Protomaps documentation
				const newStyle: maplibregl.StyleSpecification = {
					version: 8,
					glyphs: 'https://protomaps.github.io/basemaps-assets/fonts/{fontstack}/{range}.pbf',
					sprite: `https://protomaps.github.io/basemaps-assets/sprites/v3/${newTheme}`,
					sources: {
						[pmtilesSourceName]: {
							type: 'vector',
							url: `pmtiles://${pmtilesUrl}`,
							attribution:
								'<a href="https://protomaps.com">Protomaps</a> © <a href="https://openstreetmap.org">OpenStreetMap</a>'
						}
					},
					layers: protomapsThemes.default(pmtilesSourceName, newTheme as any, 'en')
				};

				console.log('Generated new style with theme:', newTheme);
				console.log('Number of layers in new style:', newStyle.layers?.length);

				// Save current visibility states using the interface
				const visibilityState: LayerVisibilityState = {
					labels: showLabels,
					roads: showRoads,
					buildings: showBuildings,
					water: showWater,
					landmarks: showLandmarks,
					landuse: showLanduse
				};

				// Apply the new style with the themed layers
				map.setStyle(newStyle);

				// Wait for the style to be loaded
				await new Promise<void>((resolve) => {
					const onStyleData = () => {
						map.off('styledata', onStyleData);
						console.log('Style data loaded');
						resolve();
					};
					map.on('styledata', onStyleData);
				});

				// Verify the layers were added correctly
				const layers = map.getStyle().layers || [];
				console.log('Actual layers in map after style change:', layers.length);

				// Dump the first few layers to check if they're as expected
				if (layers.length > 0) {
					console.log(
						'First 5 layers:',
						layers.slice(0, 5).map((l) => l.id)
					);
				}

				// Reapply visibility settings with a slightly longer delay
				setTimeout(() => {
					console.log('Reapplying layer visibility settings');

					// Apply the saved visibility settings - only toggle when needed
					if (!visibilityState.labels) toggleLabels(false);
					if (!visibilityState.roads) toggleRoads(false);
					if (!visibilityState.buildings) toggleBuildings(false);
					if (!visibilityState.water) toggleWater(false);
					if (!visibilityState.landmarks) toggleLandmarks(false);
					if (!visibilityState.landuse) toggleLanduse(false);

					// Log all layers after applying visibility
					logAllLayers();
				}, 500);
			}
		} catch (error) {
			console.error('Error updating theme:', error);
		}
	}

	async function handleSearch() {
		if (!searchQuery.trim()) return;

		isSearching = true;
		showSearchResults = true;

		try {
			// Using Nominatim OpenStreetMap API for geocoding
			const response = await fetch(
				`https://nominatim.openstreetmap.org/search?format=json&q=${encodeURIComponent(searchQuery)}`
			);
			const data = await response.json();

			searchResults = data.map((result: any) => ({
				id: result.place_id,
				name: result.display_name,
				lat: parseFloat(result.lat),
				lon: parseFloat(result.lon),
				boundingbox: result.boundingbox
			}));
		} catch (error) {
			console.error('Error searching locations:', error);
			searchResults = [];
		} finally {
			isSearching = false;
		}
	}

	function goToSearchResult(result: {
		id: string | number;
		name: string;
		lat: number;
		lon: number;
		boundingbox?: string[];
	}) {
		if (map) {
			if (result.boundingbox) {
				// If we have a bounding box, use it
				const bounds = [
					[parseFloat(result.boundingbox[2]), parseFloat(result.boundingbox[0])], // SW corner
					[parseFloat(result.boundingbox[3]), parseFloat(result.boundingbox[1])] // NE corner
				];
				map.fitBounds(bounds as maplibregl.LngLatBoundsLike, { padding: 50 });
			} else {
				// Otherwise just center on the point
				map.flyTo({ center: [result.lon, result.lat], zoom: 14 });
			}

			// Add a marker
			const marker = new maplibregl.Marker().setLngLat([result.lon, result.lat]).addTo(map);

			// Add a popup
			new maplibregl.Popup()
				.setLngLat([result.lon, result.lat])
				.setHTML(`<strong>${result.name}</strong>`)
				.addTo(map);

			// Close search results
			showSearchResults = false;
		}
	}

	function handleKeydown(event: KeyboardEvent) {
		if (event.key === 'Enter') {
			handleSearch();
		} else if (event.key === 'Escape') {
			showSearchResults = false;
		}
	}

	// Method to set the theme
	export function setTheme(themeId: 'light' | 'dark' | 'white' | 'black' | 'grayscale'): void {
		if (!map) return;

		const newTheme = themeOptions.find((t) => t.id === themeId);
		if (newTheme) {
			theme = themeId;

			// Update theme using the dynamic theme update function
			updateTheme(themeId);
		}
	}

	onDestroy(() => {
		if (map) {
			map.remove();
		}
		if (maplibregl.removeProtocol) {
			maplibregl.removeProtocol('pmtiles');
		}
	});

	// Method to fit to bounds
	export function fitBounds(
		bounds: maplibregl.LngLatBoundsLike,
		options: maplibregl.FitBoundsOptions = {}
	) {
		if (map) {
			map.fitBounds(bounds, options);
		}
	}

	// Method to fly to location
	export function flyTo(center: maplibregl.LngLatLike, zoom: number) {
		if (map) {
			map.flyTo({ center, zoom });
		}
	}

	// Method to get map instance
	export function getMap(): maplibregl.Map | undefined {
		return map;
	}

	// Method to add a custom layer
	export function addLayer(layer: any) {
		if (map) {
			// Prefix with 'custom-' to distinguish from theme layers
			const customId = layer.id.startsWith('custom-') ? layer.id : `custom-${layer.id}`;
			layer.id = customId;
			map.addLayer(layer);
		}
	}

	// Method to add a GeoJSON source
	export function addGeoJSON(id: string, data: any) {
		if (map) {
			const sourceId = `custom-${id}`;
			if (!map.getSource(sourceId)) {
				map.addSource(sourceId, {
					type: 'geojson',
					data
				});
			} else {
				(map.getSource(sourceId) as maplibregl.GeoJSONSource).setData(data);
			}
			return sourceId;
		}
		return null;
	}

	// Method to add a marker
	export function addMarker(lngLat: [number, number], options: any = {}) {
		if (map) {
			return new maplibregl.Marker(options).setLngLat(lngLat).addTo(map);
		}
		return null;
	}

	function switchStyle(styleId: string) {
		const style = styles.find((s) => s.id === styleId);
		if (style) {
			activeStyleId = style.id;
			activeStyle = style;

			pmtilesUrl = style.url;
			pmtilesSourceType = style.type;

			// Set up a new map style
			useTheme = false;

			// Reset the map with the new style
			if (map) {
				// Fly to the style's center if specified
				if (style.center) {
					map.flyTo({
						center: style.center,
						zoom: style.zoom || 1
					});
				}

				// Save current layer visibility states using the interface
				const visibilityState: LayerVisibilityState = {
					labels: showLabels,
					roads: showRoads,
					buildings: showBuildings,
					water: showWater,
					landmarks: showLandmarks,
					landuse: showLanduse
				};

				// Remove existing source and layers
				if (map.getSource(pmtilesSourceName)) {
					// Need to remove layers that use this source first
					const layers = map.getStyle().layers || [];
					for (const layer of layers) {
						// Use type assertion for layer source check
						if ((layer as any).source === pmtilesSourceName) {
							map.removeLayer(layer.id);
						}
					}
					map.removeSource(pmtilesSourceName);
				}

				// Add the new source
				map.addSource(pmtilesSourceName, {
					type: pmtilesSourceType,
					url: `pmtiles://${pmtilesUrl}`,
					attribution:
						'<a href="https://protomaps.com">Protomaps</a> © <a href="https://openstreetmap.org">OpenStreetMap</a>'
				});

				// Add basic layers if it's a vector source
				if (pmtilesSourceType === 'vector') {
					const commonSourceLayers = ['default', 'main', 'data', 'buildings', 'roads', 'water'];

					for (const sourceLayer of commonSourceLayers) {
						try {
							map.addLayer({
								id: `${pmtilesSourceName}-${sourceLayer}-fill`,
								type: 'fill',
								source: pmtilesSourceName,
								'source-layer': sourceLayer,
								paint: {
									'fill-color': '#0080ff',
									'fill-opacity': 0.5,
									'fill-outline-color': '#0066cc'
								}
							});

							map.addLayer({
								id: `${pmtilesSourceName}-${sourceLayer}-line`,
								type: 'line',
								source: pmtilesSourceName,
								'source-layer': sourceLayer,
								paint: {
									'line-color': '#0080ff',
									'line-width': 1
								}
							});

							map.addLayer({
								id: `${pmtilesSourceName}-${sourceLayer}-point`,
								type: 'circle',
								source: pmtilesSourceName,
								'source-layer': sourceLayer,
								paint: {
									'circle-radius': 3,
									'circle-color': '#0080ff'
								}
							});
						} catch (error) {
							console.log(`Source layer "${sourceLayer}" not available or error adding it`);
						}
					}
				}

				// Reapply the layer visibility settings after a small delay
				setTimeout(() => {
					// Apply the saved visibility settings
					if (!visibilityState.labels) toggleLabels(false);
					if (!visibilityState.roads) toggleRoads(false);
					if (!visibilityState.buildings) toggleBuildings(false);
					if (!visibilityState.water) toggleWater(false);
					if (!visibilityState.landmarks) toggleLandmarks(false);
					if (!visibilityState.landuse) toggleLanduse(false);

					// Log all layers after applying visibility
					console.log('After style change and visibility update:');
					logAllLayers();
				}, 200);
			}

			// Close the panels
			showBasemapPanel = false;
		}
	}

	// Toggle the basemap panel
	function toggleBasemapPanel() {
		showBasemapPanel = !showBasemapPanel;
		showLayersPanel = false;
	}

	// Toggle the layers panel
	function toggleLayersPanel() {
		showLayersPanel = !showLayersPanel;
		showBasemapPanel = false;
	}

	// Define a unified layer toggle function to avoid code repetition and fix type errors
	function toggleMapLayers(
		layerType: 'labels' | 'roads' | 'buildings' | 'water' | 'landmarks' | 'landuse',
		visible: boolean
	) {
		const layerPatterns = {
			labels: ['label', 'name', 'place', 'poi'],
			roads: ['road', 'highway', 'tunnel', 'bridge'],
			buildings: ['building'],
			water: ['water', 'river', 'wetland', 'ocean'],
			landmarks: ['poi', 'landmark', 'airport', 'hospital', 'school'],
			landuse: ['landuse', 'landcover', 'natural', 'park', 'forest', 'grass']
		};

		const specificLayers = {
			labels: [
				'address_label',
				'water_label_ocean',
				'water_label_lakes',
				'water_waterway_label',
				'roads_labels_minor',
				'roads_labels_major',
				'pois',
				'places_subplace',
				'places_locality',
				'places_region',
				'places_country'
			],
			roads: [],
			buildings: [],
			water: [],
			landmarks: [],
			landuse: []
		};

		console.log(`Toggle ${layerType} visibility to ${visible}`);

		// Update the state variable
		switch (layerType) {
			case 'labels':
				showLabels = visible;
				break;
			case 'roads':
				showRoads = visible;
				break;
			case 'buildings':
				showBuildings = visible;
				break;
			case 'water':
				showWater = visible;
				break;
			case 'landmarks':
				showLandmarks = visible;
				break;
			case 'landuse':
				showLanduse = visible;
				break;
		}

		if (!map) return;

		try {
			// Get all layers in the current style
			const style = map.getStyle();
			if (!style || !style.layers) {
				console.warn('No style or layers available');
				return;
			}

			// First try known specific layers
			for (const layerId of specificLayers[layerType]) {
				try {
					if (map.getLayer(layerId)) {
						console.log(
							`Setting known ${layerType} layer ${layerId} to ${visible ? 'visible' : 'none'}`
						);
						map.setLayoutProperty(layerId, 'visibility', visible ? 'visible' : 'none');
					}
				} catch (err) {
					console.warn(`Could not toggle layer ${layerId}`);
				}
			}

			// Then try to find layers based on patterns
			const patterns = layerPatterns[layerType];
			for (const layer of style.layers) {
				if (
					typeof layer.id === 'string' &&
					patterns.some((pattern) => layer.id.includes(pattern))
				) {
					try {
						console.log(
							`Setting ${layerType} layer ${layer.id} to ${visible ? 'visible' : 'none'}`
						);
						map.setLayoutProperty(layer.id, 'visibility', visible ? 'visible' : 'none');
					} catch (err) {
						console.warn(`Could not toggle layer ${layer.id}`);
					}
				}
			}
		} catch (error) {
			console.error(`Error toggling ${layerType} layers:`, error);
		}
	}

	// Simplified wrapper functions that use the unified toggle function
	function toggleLabels(visible: boolean) {
		toggleMapLayers('labels', visible);
	}

	function toggleRoads(visible: boolean) {
		toggleMapLayers('roads', visible);
	}

	function toggleBuildings(visible: boolean) {
		toggleMapLayers('buildings', visible);
	}

	function toggleWater(visible: boolean) {
		toggleMapLayers('water', visible);
	}

	function toggleLandmarks(visible: boolean) {
		toggleMapLayers('landmarks', visible);
	}

	function toggleLanduse(visible: boolean) {
		toggleMapLayers('landuse', visible);
	}

	// Fix for the switchToStyledMap function to use the interface
	function switchToStyledMap() {
		mapMode = 'styled';
		useTheme = true;

		// Force a full update of the theme
		// This ensures the styled map will be displayed with all layers
		if (!pmtilesUrl) {
			// Use a default URL if none is set
			pmtilesUrl =
				'https://r2-public.protomaps.com/protomaps-sample-datasets/protomaps-basemap-opensource-20230408.pmtiles';
		}

		// Log the URL to confirm we're using a valid URL
		console.log('Switching to styled map with URL:', pmtilesUrl);

		// Create currentState object with proper typing
		const currentState: LayerVisibilityState = {
			labels: showLabels,
			roads: showRoads,
			buildings: showBuildings,
			water: showWater,
			landmarks: showLandmarks,
			landuse: showLanduse
		};

		// First apply theme
		updateTheme(theme);

		// Then restore visibility settings after a delay
		setTimeout(() => {
			if (!currentState.labels) toggleLabels(false);
			if (!currentState.roads) toggleRoads(false);
			if (!currentState.buildings) toggleBuildings(false);
			if (!currentState.water) toggleWater(false);
			if (!currentState.landmarks) toggleLandmarks(false);
			if (!currentState.landuse) toggleLanduse(false);
		}, 600);
	}

	// Add additional debug function to list all available layers
	function logAllLayers() {
		if (!map) return;

		try {
			const style = map.getStyle();
			if (!style || !style.layers) {
				console.warn('No style or layers available to log');
				return;
			}

			console.log('===== ALL MAP LAYERS =====');
			style.layers.forEach((layer) => {
				const visibility = map.getLayoutProperty(layer.id, 'visibility') || 'visible';
				console.log(`Layer: ${layer.id}, Type: ${layer.type}, Visibility: ${visibility}`);
			});
			console.log('=========================');
		} catch (error) {
			console.error('Error logging layers:', error);
		}
	}
</script>

<div class="map-wrapper" style="height: {height};">
	{#if showSearch}
		<div class="search-container">
			<div class="search-input-container">
				<input
					bind:this={searchInput}
					bind:value={searchQuery}
					onkeydown={handleKeydown}
					placeholder="Search for places..."
					type="text"
					class="search-input"
				/>
				<button class="search-button" onclick={handleSearch} disabled={isSearching}>
					{#if isSearching}
						<div class="mini-spinner"></div>
					{:else}
						<svg
							xmlns="http://www.w3.org/2000/svg"
							width="18"
							height="18"
							viewBox="0 0 24 24"
							fill="none"
							stroke="currentColor"
							stroke-width="2"
						>
							<circle cx="11" cy="11" r="8"></circle>
							<line x1="21" y1="21" x2="16.65" y2="16.65"></line>
						</svg>
					{/if}
				</button>
			</div>

			{#if showSearchResults && searchResults.length > 0}
				<div class="search-results">
					{#each searchResults as result (result.id)}
						<div
							class="search-result-item"
							onclick={() => goToSearchResult(result)}
							title={result.name}
							aria-label={result.name}
						>
							<div class="search-result-name">{result.name}</div>
						</div>
					{/each}
				</div>
			{:else if showSearchResults && searchResults.length === 0 && !isSearching}
				<div class="search-results">
					<div class="search-result-empty">No results found</div>
				</div>
			{/if}
		</div>
	{/if}

	{#if showStyleControls}
		<div class="map-controls">
			<!-- Basemap Selector Button -->
			<button
				class="control-button"
				class:active={showBasemapPanel}
				onclick={toggleBasemapPanel}
				title="Basemap"
				aria-label="Basemap"
			>
				<svg
					xmlns="http://www.w3.org/2000/svg"
					width="16"
					height="16"
					viewBox="0 0 24 24"
					fill="none"
					stroke="currentColor"
					stroke-width="2"
				>
					<polygon points="1 6 1 22 8 18 16 22 23 18 23 2 16 6 8 2 1 6"></polygon>
					<line x1="8" y1="2" x2="8" y2="18"></line>
					<line x1="16" y1="6" x2="16" y2="22"></line>
				</svg>
			</button>

			<!-- Layers Button -->
			<button
				class="control-button"
				class:active={showLayersPanel}
				onclick={toggleLayersPanel}
				title="Layers"
				aria-label="Layers"
			>
				<svg
					xmlns="http://www.w3.org/2000/svg"
					width="16"
					height="16"
					viewBox="0 0 24 24"
					fill="none"
					stroke="currentColor"
					stroke-width="2"
				>
					<polygon points="12 2 2 7 12 12 22 7 12 2"></polygon>
					<polyline points="2 17 12 22 22 17"></polyline>
					<polyline points="2 12 12 17 22 12"></polyline>
				</svg>
			</button>
		</div>

		<!-- Basemap Panel -->
		{#if showBasemapPanel}
			<div class="side-panel basemap-panel">
				<div class="panel-header">
					<h3>Basemap</h3>
					<button class="close-button" onclick={() => (showBasemapPanel = false)}>×</button>
				</div>
				<div class="panel-content">
					<div class="section-title">Map Style</div>
					<div class="style-options">
						<button
							class="style-button {mapMode === 'vector' ? 'active' : ''}"
							onclick={() => {
								mapMode = 'vector';
								useTheme = false;
								switchStyle(activeStyleId);
							}}
						>
							<div class="style-icon">
								<svg
									xmlns="http://www.w3.org/2000/svg"
									width="18"
									height="18"
									viewBox="0 0 24 24"
									fill="none"
									stroke="currentColor"
									stroke-width="2"
								>
									<polygon points="1 6 1 22 8 18 16 22 23 18 23 2 16 6 8 2 1 6"></polygon>
									<line x1="8" y1="2" x2="8" y2="18"></line>
									<line x1="16" y1="6" x2="16" y2="22"></line>
								</svg>
							</div>
							<div class="style-label">Vector Map</div>
						</button>
						<button
							class="style-button {mapMode === 'styled' ? 'active' : ''}"
							onclick={switchToStyledMap}
						>
							<div class="style-icon">
								<svg
									xmlns="http://www.w3.org/2000/svg"
									width="18"
									height="18"
									viewBox="0 0 24 24"
									fill="none"
									stroke="currentColor"
									stroke-width="2"
								>
									<rect x="3" y="3" width="18" height="18" rx="2" ry="2"></rect>
									<circle cx="8.5" cy="8.5" r="1.5"></circle>
									<polyline points="21 15 16 10 5 21"></polyline>
								</svg>
							</div>
							<div class="style-label">Styled Map</div>
						</button>
					</div>

					{#if mapMode === 'styled'}
						<div class="section-title">Theme Options</div>
						<div class="theme-options">
							{#each themeOptions as themeOption (themeOption.id)}
								<button
									class="theme-button"
									class:active={theme === themeOption.id}
									onclick={() => setTheme(themeOption.id)}
									title={themeOption.name}
								>
									<span class="theme-icon">{themeOption.icon}</span>
								</button>
							{/each}
						</div>
					{/if}
				</div>
			</div>
		{/if}

		<!-- Layers Panel -->
		{#if showLayersPanel}
			<div class="side-panel layers-panel">
				<div class="panel-header">
					<h3>Layers</h3>
					<button class="close-button" onclick={() => (showLayersPanel = false)}>×</button>
				</div>
				<div class="panel-content">
					<div class="section-title">Map Layers</div>
					<div class="layer-list">
						<div class="layer-item">
							<label class="layer-toggle">
								<input type="checkbox" checked disabled />
								<span class="layer-name">Base Map</span>
							</label>
						</div>
						<div class="layer-item">
							<label class="layer-toggle">
								<input
									type="checkbox"
									bind:checked={showLabels}
									onchange={() => toggleLabels(showLabels)}
								/>
								<span class="layer-name">Labels</span>
							</label>
						</div>
						<div class="layer-item">
							<label class="layer-toggle">
								<input
									type="checkbox"
									bind:checked={showRoads}
									onchange={() => toggleRoads(showRoads)}
								/>
								<span class="layer-name">Roads</span>
							</label>
						</div>
						<div class="layer-item">
							<label class="layer-toggle">
								<input
									type="checkbox"
									bind:checked={showBuildings}
									onchange={() => toggleBuildings(showBuildings)}
								/>
								<span class="layer-name">Buildings</span>
							</label>
						</div>
						<div class="layer-item">
							<label class="layer-toggle">
								<input
									type="checkbox"
									bind:checked={showWater}
									onchange={() => toggleWater(showWater)}
								/>
								<span class="layer-name">Water</span>
							</label>
						</div>
						<div class="layer-item">
							<label class="layer-toggle">
								<input
									type="checkbox"
									bind:checked={showLandmarks}
									onchange={() => toggleLandmarks(showLandmarks)}
								/>
								<span class="layer-name">Landmarks</span>
							</label>
						</div>
						<div class="layer-item">
							<label class="layer-toggle">
								<input
									type="checkbox"
									bind:checked={showLanduse}
									onchange={() => toggleLanduse(showLanduse)}
								/>
								<span class="layer-name">Land Use</span>
							</label>
						</div>
					</div>
				</div>
			</div>
		{/if}
	{/if}

	<div id={mapId} class="map-container">
		{#if isLoading}
			<div class="loading-container">
				<div class="loading-spinner"></div>
				<div class="loading-text">Loading map...</div>
			</div>
		{/if}
	</div>

	<slot />
</div>

<style>
	.map-wrapper {
		position: relative;
		width: 100%;
		border-radius: 8px;
		overflow: hidden;
		box-shadow: 0 2px 10px rgba(0, 0, 0, 0.1);
	}

	.map-container {
		width: 100%;
		height: 100%;
		position: relative;
	}

	.loading-container {
		display: flex;
		flex-direction: column;
		align-items: center;
		justify-content: center;
		height: 100%;
		background-color: #f8faff;
		border-radius: 8px;
		position: absolute;
		top: 0;
		left: 0;
		width: 100%;
		z-index: 10;
	}

	.loading-spinner {
		width: 40px;
		height: 40px;
		border: 4px solid rgba(0, 0, 0, 0.1);
		border-radius: 50%;
		border-top-color: #3060b0;
		animation: spin 1s ease-in-out infinite;
		margin-bottom: 1rem;
	}

	.mini-spinner {
		width: 16px;
		height: 16px;
		border: 2px solid rgba(255, 255, 255, 0.3);
		border-radius: 50%;
		border-top-color: white;
		animation: spin 1s ease-in-out infinite;
	}

	.loading-text {
		color: #4a5568;
		font-size: 0.9rem;
	}

	.search-container {
		position: absolute;
		top: 10px;
		left: 10px;
		width: 320px;
		max-width: calc(100% - 20px);
		z-index: 100;
	}

	.search-input-container {
		display: flex;
		border-radius: 4px;
		overflow: hidden;
		box-shadow: 0 2px 10px rgba(0, 0, 0, 0.1);
	}

	.search-input {
		flex: 1;
		padding: 12px 16px;
		font-size: 14px;
		border: none;
		border-radius: 4px 0 0 4px;
		outline: none;
	}

	.search-button {
		background: #3060b0;
		color: white;
		border: none;
		padding: 0 16px;
		cursor: pointer;
		display: flex;
		align-items: center;
		justify-content: center;
		transition: background-color 0.2s;
	}

	.search-button:hover {
		background: #254987;
	}

	.search-button:disabled {
		background: #6890c0;
		cursor: not-allowed;
	}

	.search-results {
		margin-top: 4px;
		background: white;
		border-radius: 4px;
		box-shadow: 0 2px 10px rgba(0, 0, 0, 0.1);
		max-height: 300px;
		overflow-y: auto;
	}

	.search-result-item {
		padding: 12px 16px;
		border-bottom: 1px solid #eee;
		cursor: pointer;
	}

	.search-result-item:last-child {
		border-bottom: none;
	}

	.search-result-item:hover {
		background: #f8f9fa;
	}

	.search-result-name {
		font-size: 14px;
		color: #333;
	}

	.search-result-empty {
		padding: 12px 16px;
		color: #888;
		font-size: 14px;
		text-align: center;
	}

	/* Map controls */
	.map-controls {
		position: absolute;
		bottom: 10px;
		right: 10px;
		z-index: 100;
		display: flex;
		flex-direction: row;
		gap: 8px;
	}

	.control-button {
		width: 40px;
		height: 40px;
		background: #3060b0;
		border: none;
		border-radius: 4px;
		box-shadow: 0 2px 6px rgba(0, 0, 0, 0.15);
		cursor: pointer;
		display: flex;
		align-items: center;
		justify-content: center;
		padding: 0;
		color: white;
	}

	.control-button:hover {
		background: #254987;
	}

	.control-button.active {
		background: #254987;
		color: white;
	}

	/* Side panels */
	.side-panel {
		position: absolute;
		bottom: 60px;
		right: 10px;
		width: 280px;
		background: white;
		border-radius: 8px;
		box-shadow: 0 2px 15px rgba(0, 0, 0, 0.15);
		z-index: 99;
		overflow: hidden;
	}

	.panel-header {
		display: flex;
		justify-content: space-between;
		align-items: center;
		padding: 12px 16px;
		border-bottom: 1px solid #eee;
	}

	.panel-header h3 {
		margin: 0;
		font-size: 16px;
		font-weight: 600;
		color: #333;
	}

	.close-button {
		background: none;
		border: none;
		font-size: 20px;
		color: #666;
		cursor: pointer;
		padding: 0;
		width: 24px;
		height: 24px;
		line-height: 24px;
		text-align: center;
	}

	.close-button:hover {
		color: #333;
	}

	.panel-content {
		padding: 16px;
		max-height: 400px;
		overflow-y: auto;
	}

	/* Section titles */
	.section-title {
		font-size: 14px;
		font-weight: 600;
		margin-bottom: 12px;
		color: #555;
	}

	/* Theme options */
	.theme-options {
		display: flex;
		gap: 10px;
		margin-bottom: 20px;
	}

	.theme-button {
		width: 36px;
		height: 36px;
		border-radius: 50%;
		border: 1px solid #ddd;
		background: white;
		cursor: pointer;
		display: flex;
		align-items: center;
		justify-content: center;
		padding: 0;
		transition: all 0.2s;
	}

	.theme-button:hover {
		transform: scale(1.1);
	}

	.theme-button.active {
		border: 2px solid #3060b0;
	}

	.theme-icon {
		font-size: 18px;
	}

	/* Layer list */
	.layer-list {
		display: flex;
		flex-direction: column;
		gap: 12px;
	}

	.layer-item {
		display: flex;
		align-items: center;
	}

	.layer-toggle {
		display: flex;
		align-items: center;
		cursor: pointer;
	}

	.layer-toggle input {
		margin-right: 8px;
	}

	.layer-name {
		font-size: 14px;
		color: #333;
	}

	@keyframes spin {
		to {
			transform: rotate(360deg);
		}
	}

	:global(.maplibregl-canvas) {
		outline: none;
	}

	:global(.maplibregl-ctrl-attrib) {
		font-size: 10px;
	}

	:global(.maplibregl-ctrl-bottom-right) {
		display: flex;
		flex-direction: column;
	}

	:global(.maplibregl-ctrl-group) {
		box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1) !important;
		border-radius: 4px !important;
	}

	:global(.maplibregl-ctrl-group button) {
		border-radius: 0 !important;
	}

	:global(.maplibregl-popup-content) {
		padding: 12px !important;
		border-radius: 4px !important;
		box-shadow: 0 2px 10px rgba(0, 0, 0, 0.1) !important;
	}

	/* Add this to the style section */
	.style-options {
		display: grid;
		grid-template-columns: repeat(2, 1fr);
		gap: 10px;
		margin-bottom: 16px;
	}

	.style-button {
		display: flex;
		flex-direction: column;
		align-items: center;
		padding: 12px 8px;
		background: #f0f4f8;
		border: none;
		border-radius: 6px;
		cursor: pointer;
		transition: all 0.2s;
	}

	.style-button:hover {
		background: #e2e8f0;
	}

	.style-button.active {
		background: #3060b0;
		color: white;
	}

	.style-icon {
		margin-bottom: 6px;
	}

	.style-label {
		font-size: 13px;
		font-weight: 500;
	}
</style>
