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
	import { userNetworkCapacitiesWithShares } from '$lib/state/core.svelte';
	import { globalState } from '$lib/global.svelte';
	import { getUserName, userNamesOrAliasesCache } from '$lib/state/users.svelte';
	import {
		reverseGeocode,
		parseAddressComponents,
		geocodeCapacityAddress,
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
	import MapSidePanel from './MapSidePanel.svelte';

	interface Props {
		// Map now shows read-only share slots, no update functionality needed
	}

	let {}: Props = $props();

	let features: maplibregl.MapGeoJSONFeature[] = $state.raw([]);
	let mode: 'source' = $state('source');
	let show3DBuildings = $state(false);
	let map: maplibregl.Map | undefined = $state.raw();
	let pitch = $state(0);
	let isTerrainVisible = $state(false);
	let isGlobeMode = $state(false);

	// Grouped slot marker data structure - one marker per unique (capacity, location) combination
	export interface GroupedSlotMarkerData {
		id: string; // unique marker ID based on capacityId + locationKey
		capacityId: string;
		capacity: any;
		slots: any[]; // array of slots at this location
		lnglat: { lat: number; lng: number };
		source: 'coordinates' | 'geocoded';
		providerId?: string;
		providerName?: string;
		locationKey: string; // unique identifier for this location
	}

	// Grouped slot markers state (from shared capacities only)
	let shareSlotMarkers = $state<GroupedSlotMarkerData[]>([]);
	let isLoadingGeocode = $state(false);

	// Selected marker state for side panel
	let selectedMarker = $state<GroupedSlotMarkerData | null>(null);

	// Handle marker click
	function handleMarkerClick(markerData: GroupedSlotMarkerData) {
		selectedMarker = markerData;
	}

	// Handle side panel close
	function handleSidePanelClose() {
		selectedMarker = null;
	}

	// Debug selected marker changes (better pattern)
	$inspect('Selected marker:', selectedMarker?.id);

	// Async function to load grouped slot markers from shared capacities
	async function loadShareSlotMarkers(sharedCapacities: typeof $userNetworkCapacitiesWithShares) {
		console.log('[Map] 🚀 loadShareSlotMarkers called with:', {
			sharedCapacities: !!sharedCapacities,
			type: typeof sharedCapacities,
			keysCount: sharedCapacities ? Object.keys(sharedCapacities).length : 'N/A',
			environment: {
				hostname: typeof window !== 'undefined' ? window.location.hostname : 'SSR',
				protocol: typeof window !== 'undefined' ? window.location.protocol : 'SSR'
			}
		});

		if (!sharedCapacities) {
			console.log('[Map] ❌ No userNetworkCapacitiesWithShares available');
			shareSlotMarkers = [];
			return;
		}

		console.log('[Map] ✅ userNetworkCapacitiesWithShares:', sharedCapacities);
		console.log('[Map] Total shared capacities:', Object.keys(sharedCapacities).length);

		// Prevent multiple simultaneous calls by checking loading state
		if (isLoadingGeocode) {
			console.log('[Map] Geocoding already in progress, skipping...');
			return;
		}

		// Group slots by (capacityId, locationKey) combination
		const slotGroups = new Map<
			string,
			{
				capacityId: string;
				capacity: any;
				slots: any[];
				lnglat: { lat: number; lng: number };
				source: 'coordinates' | 'geocoded';
				providerId?: string;
				providerName?: string;
				locationKey: string;
			}
		>();

		try {
			console.log('[Map] ⏳ Starting geocoding process...');
			isLoadingGeocode = true;

			for (const [capacityId, capacity] of Object.entries(sharedCapacities)) {
				console.log(`[Map] Processing capacity ${capacityId}:`, capacity.name);
				const providerId = (capacity as any).provider_id;
				let providerName = 'Unknown Provider';

				// Try to get provider name
				if (providerId) {
					try {
						const cachedName = $userNamesOrAliasesCache[providerId];
						if (cachedName) {
							providerName = cachedName;
						} else {
							const fetchedName = await getUserName(providerId);
							if (fetchedName) {
								providerName =
									fetchedName.length > 30 ? fetchedName.substring(0, 30) + '...' : fetchedName;
							}
						}
					} catch (error) {
						console.warn(`[Map] Could not get provider name for ${providerId}:`, error);
					}
				}

				// Process slots in this capacity
				if (capacity.availability_slots && Array.isArray(capacity.availability_slots)) {
					for (const slot of capacity.availability_slots) {
						// Check location type first - only show "Specific" locations on map
						if ((slot as any).location_type !== 'Specific') {
							console.log(
								`[Map] Skipping slot ${slot.id} - location_type is ${(slot as any).location_type || 'undefined'}, not 'Specific'`
							);
							continue;
						}

						let slotLnglat: { lat: number; lng: number } | null = null;
						let source: 'coordinates' | 'geocoded' = 'coordinates';

						// Priority 1: Geocode address if we have address components
						if (
							slot.street_address ||
							slot.city ||
							slot.state_province ||
							slot.postal_code ||
							slot.country
						) {
							try {
								const addressStr = [
									(slot as any).street_address,
									(slot as any).city,
									(slot as any).state_province,
									(slot as any).postal_code,
									(slot as any).country
								]
									.filter(Boolean)
									.join(', ');
								console.log(
									`[Map] 🌐 Attempting to geocode slot ${slot.id} address: "${addressStr}"`
								);

								const geocodeResults = await geocodeCapacityAddress({
									street_address: (slot as any).street_address,
									city: (slot as any).city,
									state_province: (slot as any).state_province,
									postal_code: (slot as any).postal_code,
									country: (slot as any).country
								});

								console.log(`[Map] Geocoding API response for slot ${slot.id}:`, geocodeResults);

								if (geocodeResults.length > 0) {
									const result = geocodeResults[0]; // Use the first (best) result
									slotLnglat = { lat: result.latitude, lng: result.longitude };
									source = 'geocoded';
									console.log(
										`[Map] ✅ Successfully geocoded slot ${slot.id}: ${result.latitude}, ${result.longitude}`
									);
								} else {
									console.warn(
										`[Map] ⚠️ No geocoding results for slot ${slot.id} (address: "${addressStr}")`
									);
								}
							} catch (geocodeError) {
								console.error(`[Map] ❌ Geocoding failed for slot ${slot.id}:`, {
									error: geocodeError,
									message: geocodeError instanceof Error ? geocodeError.message : String(geocodeError),
									stack: geocodeError instanceof Error ? geocodeError.stack : undefined,
									address: [
										slot.street_address,
										slot.city,
										slot.state_province,
										slot.postal_code,
										slot.country
									]
										.filter(Boolean)
										.join(', ')
								});
							}
						}

						// Priority 2: Use direct coordinates only if no address was available or geocoding failed
						if (!slotLnglat && slot.latitude !== undefined && slot.longitude !== undefined) {
							slotLnglat = { lat: slot.latitude, lng: slot.longitude };
							source = 'coordinates';
							console.log(
								`[Map] Using direct coordinates for slot ${slot.id}: ${slot.latitude}, ${slot.longitude}`
							);
						}

						// Priority 3: Fall back to capacity coordinates if slot has no location but capacity does
						if (!slotLnglat && (capacity as any).location_type === 'Specific') {
							// Try capacity address first
							if (
								(capacity as any).street_address ||
								(capacity as any).city ||
								(capacity as any).state_province ||
								(capacity as any).postal_code ||
								(capacity as any).country
							) {
								try {
									console.log(
										`[Map] Attempting to geocode capacity ${capacityId} address for slot ${slot.id}`
									);

									const geocodeResults = await geocodeCapacityAddress({
										street_address: (capacity as any).street_address,
										city: (capacity as any).city,
										state_province: (capacity as any).state_province,
										postal_code: (capacity as any).postal_code,
										country: (capacity as any).country
									});

									if (geocodeResults.length > 0) {
										const result = geocodeResults[0];
										slotLnglat = { lat: result.latitude, lng: result.longitude };
										source = 'geocoded';
										console.log(
											`[Map] Successfully geocoded capacity ${capacityId} for slot ${slot.id}: ${result.latitude}, ${result.longitude}`
										);
									}
								} catch (geocodeError) {
									console.warn(
										`[Map] Capacity geocoding failed for slot ${slot.id}:`,
										geocodeError
									);
								}
							}

							// Final fallback to capacity coordinates
							if (
								!slotLnglat &&
								(capacity as any).latitude !== undefined &&
								(capacity as any).longitude !== undefined
							) {
								slotLnglat = { lat: (capacity as any).latitude, lng: (capacity as any).longitude };
								source = 'coordinates';
								console.log(
									`[Map] Using capacity coordinates for slot ${slot.id}: ${(capacity as any).latitude}, ${(capacity as any).longitude}`
								);
							}
						}

						// If we have coordinates, group this slot by location and capacity
						if (slotLnglat) {
							// Create location key based on rounded coordinates (to group nearby locations)
							const locationKey = `${Math.round(slotLnglat.lat * 100000) / 100000},${Math.round(slotLnglat.lng * 100000) / 100000}`;
							const groupKey = `${capacityId}:${locationKey}`;

							if (slotGroups.has(groupKey)) {
								// Add slot to existing group
								slotGroups.get(groupKey)!.slots.push(slot);
							} else {
								// Create new group
								slotGroups.set(groupKey, {
									capacityId: capacityId,
									capacity: capacity,
									slots: [slot],
									lnglat: slotLnglat,
									source: source,
									providerId: providerId,
									providerName: providerName,
									locationKey: locationKey
								});
							}

							console.log(
								`[Map] Added slot to group ${groupKey}: ${capacity.name} - slot ${slot.id} at ${slotLnglat.lat}, ${slotLnglat.lng} (${source})`
							);
						} else {
							console.log(`[Map] No location data available for slot ${slot.id}`);
						}
					}
				}
			}

			// Convert groups to markers array
			const markers: GroupedSlotMarkerData[] = [];
			for (const [groupKey, group] of slotGroups.entries()) {
				markers.push({
					id: groupKey,
					capacityId: group.capacityId,
					capacity: group.capacity,
					slots: group.slots,
					lnglat: group.lnglat,
					source: group.source,
					providerId: group.providerId,
					providerName: group.providerName,
					locationKey: group.locationKey
				});
			}

			console.log(
				'[Map] About to set markers:',
				markers.map((m) => ({ id: m.id, capacity: m.capacity.name, slots: m.slots.length }))
			);
			shareSlotMarkers = markers;
			console.log(`[Map] Created ${markers.length} grouped markers from shared capacities`);
			console.log(
				`[Map] Total slots represented: ${markers.reduce((sum, m) => sum + m.slots.length, 0)}`
			);
		} catch (error) {
			console.error('[Map] Error loading share slot markers:', error);
			shareSlotMarkers = [];
		} finally {
			isLoadingGeocode = false;
		}
	}

	// Reactive Svelte 5 approach using $derived.by for side effects
	let currentCapacities = $derived($userNetworkCapacitiesWithShares);
	let capacitiesCount = $derived(Object.keys(currentCapacities || {}).length);

	// Debounced loading function to prevent excessive API calls
	const debouncedLoadMarkers = debounce(async (capacities: typeof currentCapacities) => {
		if (capacities && Object.keys(capacities).length > 0) {
			console.log('[Map] Loading markers for', Object.keys(capacities).length, 'capacities');
			await loadShareSlotMarkers(capacities);
		}
	}, 300);

	// Reactive loading with $derived.by
	let markersLoader = $derived.by(() => {
		console.log('[Map] markersLoader triggered');
		console.log('[Map] currentCapacities:', currentCapacities);
		console.log('[Map] currentCapacities type:', typeof currentCapacities);
		console.log(
			'[Map] currentCapacities keys:',
			currentCapacities ? Object.keys(currentCapacities) : 'null/undefined'
		);

		if (currentCapacities && Object.keys(currentCapacities).length > 0) {
			console.log('[Map] ✅ Data available, calling debouncedLoadMarkers');
			debouncedLoadMarkers(currentCapacities);
			return 'loaded';
		} else {
			console.log('[Map] ❌ No data available');
			console.log('[Map] userNetworkCapacitiesWithShares raw:', $userNetworkCapacitiesWithShares);
			return 'empty';
		}
	});

	// Expose manual refresh method
	async function refreshMarkers() {
		if (currentCapacities && Object.keys(currentCapacities).length > 0) {
			await loadShareSlotMarkers(currentCapacities);
		}
	}

	// Debug logging
	$inspect('Map state:', { capacitiesCount, markersLoader });

	// Helper function to safely extract time from potentially malformed time strings
	function safeExtractTime(timeValue: string | null | undefined): string | undefined {
		if (!timeValue) return undefined;
		if (/^\d{2}:\d{2}$/.test(timeValue)) {
			return timeValue;
		}
		if (timeValue.includes('T')) {
			try {
				const date = new Date(timeValue);
				return date.toTimeString().substring(0, 5);
			} catch (e) {
				console.warn('Failed to parse time:', timeValue);
				return undefined;
			}
		}
		console.warn('Unknown time format:', timeValue);
		return undefined;
	}

	// Helper function to format time without leading zeros (08:30 → 8:30)
	function formatTimeClean(timeStr: string): string {
		if (!timeStr) return timeStr;
		const [hours, minutes] = timeStr.split(':');
		const cleanHours = parseInt(hours).toString();
		return `${cleanHours}:${minutes}`;
	}

	// Helper function to format date for display with smart labels
	function formatDateForDisplay(date: Date): string {
		const today = new Date();
		const tomorrow = new Date(today);
		tomorrow.setDate(tomorrow.getDate() + 1);
		if (date.toDateString() === today.toDateString()) {
			return 'Today';
		} else if (date.toDateString() === tomorrow.toDateString()) {
			return 'Tomorrow';
		} else {
			return date.toLocaleDateString('en-US', { month: 'short', day: 'numeric' });
		}
	}

	// Format slot time display - clean and comprehensive (matches Share.svelte)
	function formatSlotTimeDisplay(slot: any): string {
		const rawStartTime = safeExtractTime(slot.start_time);
		const rawEndTime = safeExtractTime(slot.end_time);
		const cleanStartTime = rawStartTime ? formatTimeClean(rawStartTime) : '';
		const cleanEndTime = rawEndTime ? formatTimeClean(rawEndTime) : '';

		if (slot.all_day) {
			const startDate = slot.start_date ? new Date(slot.start_date) : null;
			const endDate = slot.end_date ? new Date(slot.end_date) : null;
			if (startDate && endDate && startDate.getTime() !== endDate.getTime()) {
				const startStr = formatDateForDisplay(startDate);
				const endStr = formatDateForDisplay(endDate);
				return `${startStr} - ${endStr}, All day`;
			} else if (startDate) {
				const dateStr = formatDateForDisplay(startDate);
				return `${dateStr}, All day`;
			}
			return 'All day';
		}

		const startDate = slot.start_date ? new Date(slot.start_date) : null;
		const endDate = slot.end_date ? new Date(slot.end_date) : null;

		if (startDate) {
			const startDateStr = formatDateForDisplay(startDate);
			if (endDate && startDate.getTime() !== endDate.getTime()) {
				const endDateStr = formatDateForDisplay(endDate);
				const startTimeStr = cleanStartTime || '';
				const endTimeStr = cleanEndTime || '';
				if (startTimeStr && endTimeStr) {
					return `${startDateStr}, ${startTimeStr} - ${endDateStr}, ${endTimeStr}`;
				} else if (startTimeStr) {
					return `${startDateStr}, ${startTimeStr} - ${endDateStr}`;
				} else {
					return `${startDateStr} - ${endDateStr}`;
				}
			} else {
				if (cleanStartTime) {
					const timeRange = cleanEndTime ? `${cleanStartTime}-${cleanEndTime}` : cleanStartTime;
					return `${startDateStr}, ${timeRange}`;
				}
				return startDateStr;
			}
		}
		if (cleanStartTime) {
			return cleanEndTime ? `${cleanStartTime}-${cleanEndTime}` : cleanStartTime;
		}
		return 'No time set';
	}

	// Helper function to format slot location display
	function formatSlotLocationDisplay(slot: any): string {
		if (slot.location_type === 'Specific') {
			const addressParts = [];

			if (slot.street_address) addressParts.push(slot.street_address);
			if (slot.city) addressParts.push(slot.city);
			if (slot.state_province) addressParts.push(slot.state_province);
			if (slot.postal_code) addressParts.push(slot.postal_code);
			if (slot.country) addressParts.push(slot.country);

			if (addressParts.length > 0) {
				return addressParts.join(', ');
			}

			if (slot.latitude && slot.longitude) {
				return `${slot.latitude.toFixed(4)}, ${slot.longitude.toFixed(4)}`;
			}
		}

		return slot.location_type || 'No location';
	}

	// Helper function to check if a slot is recurring (matches Share.svelte)
	function isSlotRecurring(slot: any): boolean {
		return slot.recurrence && slot.recurrence !== 'Does not repeat';
	}

	// Helper function to check if a slot is in the past (matches Share.svelte)
	function isSlotInPast(slot: any): boolean {
		if (isSlotRecurring(slot)) return false;

		const now = new Date();
		let slotEndDate = slot.end_date ? new Date(slot.end_date) : null;
		let slotStartDate = slot.start_date ? new Date(slot.start_date) : null;

		// Use end date if available, otherwise use start date
		const relevantDate = slotEndDate || slotStartDate;
		if (!relevantDate) return false;

		// Set time to end of day for comparison
		const slotDate = new Date(relevantDate);
		slotDate.setHours(23, 59, 59, 999);

		return slotDate < now;
	}

	// Categorize slots like in Share.svelte
	function categorizeSlots(slots: any[]): {
		recurring: any[];
		currentFuture: any[];
		past: any[];
	} {
		const recurring: any[] = [];
		const currentFuture: any[] = [];
		const past: any[] = [];

		slots.forEach((slot) => {
			if (isSlotRecurring(slot)) {
				recurring.push(slot);
			} else if (isSlotInPast(slot)) {
				past.push(slot);
			} else {
				currentFuture.push(slot);
			}
		});

		return { recurring, currentFuture, past };
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

	// Debug info
	$inspect('[Map] shareSlotMarkers count:', shareSlotMarkers.length);
	$inspect('[Map] isLoadingGeocode:', isLoadingGeocode);
	$inspect('[Map] selectedMarker:', selectedMarker);
	$inspect('[Map] Environment check:', {
		hostname: typeof window !== 'undefined' ? window.location.hostname : 'SSR',
		protocol: typeof window !== 'undefined' ? window.location.protocol : 'SSR',
		userAgent: typeof navigator !== 'undefined' ? navigator.userAgent : 'SSR'
	});
</script>

<div class="map-wrapper relative h-[400px] max-h-[50vh] min-h-[300px] overflow-hidden rounded-md">
	<!-- Side Panel (positioned absolute within this container) -->
	<MapSidePanel markerData={selectedMarker} onClose={handleSidePanelClose} />

	<!-- Map Container (takes remaining space) -->
	<div class="map-content {selectedMarker ? 'with-panel' : 'full-width'}">
		<MapLibre
			bind:map
			bind:pitch
			class="map-container h-full w-full"
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

			<!-- Grouped share slot markers - from shared capacities only -->
			{#each shareSlotMarkers as markerData (markerData.id)}
				{@const { id, capacityId, capacity, slots, lnglat, source, providerName } = markerData}
				{@const isSelected = selectedMarker?.id === markerData.id}
				{@const isGeocoded = source === 'geocoded'}
				{@const totalSlots = slots.length}
				{@const debugInfo = `Marker ${markerData.id}: ${capacity.name} (${totalSlots} slots)`}

				<Marker
					lnglat={{ lng: lnglat.lng, lat: lnglat.lat }}
					draggable={false}
					color={isSelected ? '#f59e0b' : isGeocoded ? '#10b981' : '#6366f1'}
					scale={isSelected ? 1.2 : 1.0}
				>
					{#snippet content()}
						<div
							class="slot-marker {isSelected ? 'selected' : ''}"
							onclick={() => handleMarkerClick(markerData)}
							role="button"
							tabindex="0"
						>
							<div class="marker-icon">{capacity.emoji || '🏠'}</div>
							<div class="marker-label">{capacity.name}</div>
							{#if totalSlots > 1}
								<div class="slot-count">{totalSlots}</div>
							{/if}
							{#if isSelected}
								<div class="selection-ring"></div>
							{/if}
						</div>
					{/snippet}
				</Marker>
			{/each}

			<!-- Loading indicator for geocoding -->
			{#if isLoadingGeocode}
				<CustomControl position="bottom-right">
					<div class="geocoding-loading">
						<span>🗺️ Geocoding slot addresses...</span>
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

	.slot-marker {
		display: flex;
		flex-direction: column;
		align-items: center;
		cursor: pointer;
		transition: transform 0.2s ease;
		position: relative;
	}

	.slot-marker:hover {
		transform: scale(1.1);
	}

	.slot-marker.selected {
		transform: scale(1.15);
	}

	.selection-ring {
		position: absolute;
		top: -6px;
		left: -6px;
		right: -6px;
		bottom: -6px;
		border: 3px solid #f59e0b;
		border-radius: 50%;
		animation: pulse-ring 2s infinite;
	}

	@keyframes pulse-ring {
		0% {
			transform: scale(1);
			opacity: 1;
		}
		50% {
			transform: scale(1.1);
			opacity: 0.7;
		}
		100% {
			transform: scale(1);
			opacity: 1;
		}
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

	.slot-count {
		position: absolute;
		top: -8px;
		right: -8px;
		background: #ef4444;
		color: white;
		border-radius: 50%;
		width: 20px;
		height: 20px;
		display: flex;
		align-items: center;
		justify-content: center;
		font-size: 10px;
		font-weight: bold;
		box-shadow: 0 1px 3px rgba(0, 0, 0, 0.3);
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

	/* Map wrapper and content layout styles */
	.map-wrapper {
		position: relative;
		overflow: hidden;
	}

	.map-content {
		height: 100%;
		transition: margin-left 0.3s ease-out;
	}

	.map-content.full-width {
		margin-left: 0;
	}

	.map-content.with-panel {
		margin-left: 400px; /* Width of side panel */
	}

	/* Responsive margins for map content */
	@media (max-width: 768px) {
		.map-content.with-panel {
			margin-left: 320px;
		}
	}

	@media (max-width: 500px) {
		.map-content.with-panel {
			margin-left: 280px;
		}
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
