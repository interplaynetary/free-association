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
		Projection,
		FullScreenControl
	} from 'svelte-maplibre-gl';
	import maplibregl from 'maplibre-gl';
	import { userNetworkCapacitiesWithSlotQuantities } from '$lib/state/core.svelte';
	import { get } from 'svelte/store';

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
	let isGlobeMode = $state(true);
	// Fullscreen is now handled by FullScreenControl component
	let isControlsExpanded = $state(false);

	// Clustering controls
	let enableClustering = $state(true);
	let clusterMaxZoom = $state(16); // Increased to enable clustering at closer zoom levels
	let clusterRadius = $state(50);

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

	// Cluster marker data structure for grouped capacities
	export interface ClusterMarkerData {
		id: string;
		lnglat: { lat: number; lng: number };
		markers: GroupedSlotMarkerData[]; // grouped markers in this cluster
		totalSlots: number;
		totalCapacities: number;
		clusterLevel: number; // zoom level where this cluster appears
	}

	// Grouped slot markers state (from shared capacities only)
	let shareSlotMarkers = $state<GroupedSlotMarkerData[]>([]);
	// Ultra-efficient streaming state
	let processedCapacityIds = $state<Set<string>>(new Set());
	let geocodingQueue = $state<string[]>([]);
	let isProcessingGeocode = $state(false);

	// Separate immediate and geocoding queues for better performance
	let immediateQueue = $state<string[]>([]);
	let isProcessingImmediate = $state(false);

	// Filtered markers - we'll implement time filtering as a future enhancement
	// For now, show all markers to ensure basic functionality works
	// TODO: Add time filtering back once the basic display is working

	// Selected marker state for side panel
	let selectedMarker = $state<GroupedSlotMarkerData | ClusterMarkerData | null>(null);
	// Cluster view state - separate from search
	let isClusterViewMode = $state(false);
	let clusterViewResults = $state<GroupedSlotMarkerData[]>([]);

	// All search state is now managed globally

	// Listen for panel search events
	$effect(() => {
		const handlePanelSearch = (event: CustomEvent) => {
			performSearch(event.detail.query);
		};

		if (typeof window !== 'undefined') {
			window.addEventListener('panel-search', handlePanelSearch as EventListener);
			return () => {
				window.removeEventListener('panel-search', handlePanelSearch as EventListener);
			};
		}
	});

	// Handle marker click
	function handleMarkerClick(markerData: GroupedSlotMarkerData | ClusterMarkerData) {
		console.log('[Map] Marker clicked:', markerData.id, 'isCluster:', isClusterMarker(markerData));

		if (isClusterMarker(markerData)) {
			// For cluster clicks, show cluster contents in separate cluster view
			console.log('[Map] Setting cluster view mode, markers:', markerData.markers.length);
			selectedMarker = markerData;
			isClusterViewMode = true;
			clusterViewResults = markerData.markers;
		} else {
			// For individual markers, show in side panel
			console.log('[Map] Setting individual marker view');
			selectedMarker = markerData;
			isClusterViewMode = false;
			clusterViewResults = [];
		}
	}

	// Handle side panel close
	function handleSidePanelClose() {
		selectedMarker = null;
		isClusterViewMode = false;
		clusterViewResults = [];
		// If we were in search mode, clear search
		if (globalState.isSearchMode) {
			globalState.clearSearch();
		}
	}

	// Handle back to search (clear marker but keep search active)
	function handleBackToSearch() {
		selectedMarker = null;
		// Don't clear search - keep it active so user returns to search results
	}

	// Search functionality
	function performSearch(query: string) {
		console.log('[Map Search] Performing search for:', query);

		if (!query.trim() && globalState.timeFilterBy === 'any') {
			globalState.clearSearch();
			return;
		}

		globalState.updateSearchQuery(query);

		console.log('[Map Search] Search mode activated, total markers:', shareSlotMarkers.length);

		// Filter markers based on search query (time filter already applied to filteredMarkers)
		const filtered = filteredMarkers.filter((marker) => {
			// Text search filter
			let matchesText = true;
			if (query.trim()) {
				const capacity = marker.capacity;
				const searchTerm = query.toLowerCase();

				// Search in capacity name, description, unit, and provider name
				const matchesName = capacity.name?.toLowerCase().includes(searchTerm);
				const matchesDescription = capacity.description?.toLowerCase().includes(searchTerm);
				const matchesUnit = capacity.unit?.toLowerCase().includes(searchTerm);
				const matchesProvider = marker.providerName?.toLowerCase().includes(searchTerm);

				matchesText = matchesName || matchesDescription || matchesUnit || matchesProvider;
			}

			// Time filter is already applied at the filteredMarkers level
			return matchesText;
		});

		// Sort results based on selected sort method
		sortSearchResults(filtered);
	}

	function sortSearchResults(results: GroupedSlotMarkerData[]) {
		if (globalState.searchSortBy === 'relevance') {
			// Sort by relevance (name match first, then description, etc.)
			results.sort((a, b) => {
				const queryLower = globalState.searchQuery.toLowerCase();

				// Priority scoring
				const getRelevanceScore = (marker: GroupedSlotMarkerData) => {
					let score = 0;
					const capacity = marker.capacity;

					// Exact name match gets highest score
					if (capacity.name?.toLowerCase() === queryLower) score += 100;
					else if (capacity.name?.toLowerCase().startsWith(queryLower)) score += 50;
					else if (capacity.name?.toLowerCase().includes(queryLower)) score += 25;

					// Description matches
					if (capacity.description?.toLowerCase().includes(queryLower)) score += 10;

					// Unit matches
					if (capacity.unit?.toLowerCase().includes(queryLower)) score += 5;

					// Provider name matches
					if (marker.providerName?.toLowerCase().includes(queryLower)) score += 15;

					return score;
				};

				return getRelevanceScore(b) - getRelevanceScore(a);
			});
		} else if (globalState.searchSortBy === 'distance' && $currentLocation) {
			// Sort by distance from user's current location
			results.sort((a, b) => {
				const distanceA = calculateDistance(
					$currentLocation!.latitude,
					$currentLocation!.longitude,
					a.lnglat.lat,
					a.lnglat.lng
				);
				const distanceB = calculateDistance(
					$currentLocation!.latitude,
					$currentLocation!.longitude,
					b.lnglat.lat,
					b.lnglat.lng
				);
				return distanceA - distanceB;
			});
		}

		globalState.updateSearchResults(results);
	}

	// Calculate distance between two coordinates (Haversine formula)
	function calculateDistance(lat1: number, lon1: number, lat2: number, lon2: number): number {
		const R = 6371; // Earth's radius in kilometers
		const dLat = ((lat2 - lat1) * Math.PI) / 180;
		const dLon = ((lon2 - lon1) * Math.PI) / 180;
		const a =
			Math.sin(dLat / 2) * Math.sin(dLat / 2) +
			Math.cos((lat1 * Math.PI) / 180) *
				Math.cos((lat2 * Math.PI) / 180) *
				Math.sin(dLon / 2) *
				Math.sin(dLon / 2);
		const c = 2 * Math.atan2(Math.sqrt(a), Math.sqrt(1 - a));
		return R * c; // Distance in kilometers
	}

	function clearSearch() {
		globalState.clearSearch();
		selectedMarker = null;
	}

	// Time filtering logic
	function passesTimeFilter(marker: GroupedSlotMarkerData): boolean {
		if (globalState.timeFilterBy === 'any') return true;

		const now = new Date();

		// Check if any slot in this marker passes the time filter
		return marker.slots.some((slot) => {
			// Skip slots without dates for time-based filters
			if (!slot.start_date && globalState.timeFilterBy !== 'any') return false;

			const { slotStart, slotEnd } = parseSlotDateTime(slot);

			switch (globalState.timeFilterBy) {
				case 'now': {
					// Slot must be happening right now
					if (!slotStart) return false;

					if (slot.all_day) {
						// For all-day events, check if today falls within the date range
						const today = new Date(now.getFullYear(), now.getMonth(), now.getDate());
						const slotStartDate = new Date(
							slotStart.getFullYear(),
							slotStart.getMonth(),
							slotStart.getDate()
						);
						const slotEndDate = slotEnd
							? new Date(slotEnd.getFullYear(), slotEnd.getMonth(), slotEnd.getDate())
							: slotStartDate;
						const result = today >= slotStartDate && today <= slotEndDate;
						console.log(`[Map Time Filter] All-day slot check:`, {
							slotId: slot.id,
							today: today.toDateString(),
							slotStartDate: slotStartDate.toDateString(),
							slotEndDate: slotEndDate.toDateString(),
							result
						});
						return result;
					}

					// For timed events, use the consistently parsed end time
					const endTime = slotEnd || slotStart;
					const result = now >= slotStart && now <= endTime;
					console.log(`[Map Time Filter] Timed slot check:`, {
						slotId: slot.id,
						now: now.toISOString(),
						slotStart: slotStart.toISOString(),
						endTime: endTime.toISOString(),
						slotStartTime: slot.start_time,
						slotEndTime: slot.end_time,
						result
					});
					return result;
				}
				case 'next24h': {
					// Slot must be available within the next 24 hours
					if (!slotStart) return false;
					const next24h = new Date(now.getTime() + 24 * 60 * 60 * 1000);
					const endTime = slotEnd || slotStart;

					// Slot is available in next 24h if:
					// 1. It starts within the next 24 hours, OR
					// 2. It's already active and continues into the next 24 hours, OR
					// 3. It starts before now but ends after now (currently active)
					const startsWithinNext24h = slotStart >= now && slotStart <= next24h;
					const isCurrentlyActive = now >= slotStart && now <= endTime;
					const endsWithinNext24h = endTime >= now && endTime <= next24h;
					const spansNext24h = slotStart <= now && endTime >= next24h;

					const result =
						startsWithinNext24h || isCurrentlyActive || endsWithinNext24h || spansNext24h;
					console.log(`[Map Time Filter] Next 24h slot check:`, {
						slotId: slot.id,
						now: now.toISOString(),
						next24h: next24h.toISOString(),
						slotStart: slotStart.toISOString(),
						endTime: endTime.toISOString(),
						startsWithinNext24h,
						isCurrentlyActive,
						endsWithinNext24h,
						spansNext24h,
						result
					});
					return result;
				}
				case 'between': {
					// Custom date/time range
					if (!globalState.timeFilterStartDate) return true; // No filter set

					const filterStart = new Date(globalState.timeFilterStartDate);
					if (globalState.timeFilterStartTime) {
						const [hours, minutes] = globalState.timeFilterStartTime.split(':');
						filterStart.setHours(parseInt(hours), parseInt(minutes));
					} else {
						filterStart.setHours(0, 0, 0, 0);
					}

					const filterEnd = globalState.timeFilterEndDate
						? new Date(globalState.timeFilterEndDate)
						: new Date(filterStart);
					if (globalState.timeFilterEndTime) {
						const [hours, minutes] = globalState.timeFilterEndTime.split(':');
						filterEnd.setHours(parseInt(hours), parseInt(minutes));
					} else {
						filterEnd.setHours(23, 59, 59, 999);
					}

					if (!slotStart) return false;
					const slotEndTime = slotEnd || slotStart;

					// Check if slot overlaps with filter range
					return slotStart <= filterEnd && slotEndTime >= filterStart;
				}
				default:
					return true;
			}
		});
	}

	function handleSearchResultClick(marker: GroupedSlotMarkerData) {
		selectedMarker = marker;
		// Clear cluster view mode when selecting individual marker
		isClusterViewMode = false;
		clusterViewResults = [];
		// If this was from regular search, clear search mode
		if (globalState.isSearchMode && !globalState.searchQuery.startsWith('Cluster of')) {
			globalState.clearSearch();
		}
		// Optionally pan map to the selected marker
		if (map) {
			map.flyTo({
				center: [marker.lnglat.lng, marker.lnglat.lat],
				zoom: 14,
				duration: 1000
			});
		}
	}

	// Handle cluster result click (separate from search results)
	function handleClusterResultClick(marker: GroupedSlotMarkerData) {
		console.log('[Cluster] Clicking capacity from cluster:', marker.capacity.name);
		console.log(
			'[Cluster] Before - selectedMarker:',
			selectedMarker?.id,
			'isClusterViewMode:',
			isClusterViewMode
		);

		selectedMarker = marker;
		// Clear cluster view mode to show individual capacity details
		isClusterViewMode = false;
		clusterViewResults = [];

		console.log(
			'[Cluster] After - selectedMarker:',
			selectedMarker?.id,
			'isClusterViewMode:',
			isClusterViewMode
		);

		// Make sure we're not in search mode
		if (globalState.isSearchMode) {
			globalState.clearSearch();
		}
		// Optionally pan map to the selected marker
		if (map) {
			map.flyTo({
				center: [marker.lnglat.lng, marker.lnglat.lat],
				zoom: 14,
				duration: 1000
			});
		}
	}

	function handleSortChange() {
		if (globalState.isSearchMode && globalState.searchResults.length > 0) {
			sortSearchResults([...globalState.searchResults]);
		}
	}

	// Time filter handlers
	function handleTimeFilterChange() {
		// Always trigger search/filter update when time filter changes
		if (globalState.isSearchMode) {
			// If in search mode, re-run search with new time filter
			performSearch(globalState.searchQuery);
		} else {
			// If not in search mode but time filter is active, filter all visible markers
			// This will be handled by the marker filtering logic
			console.log('[Map] Time filter changed to:', globalState.timeFilterBy);
		}
	}

	function handleTimeFilterDetailsChange() {
		// Re-run search/filter when custom time details change
		if (globalState.isSearchMode) {
			performSearch(globalState.searchQuery);
		} else if (globalState.timeFilterBy === 'between') {
			// Time filter details changed while not searching - markers should be filtered
			console.log('[Map] Time filter details changed');
		}
	}

	// Format date for input (helper function)
	function formatDateForInput(date: Date | undefined): string {
		if (!date) return '';
		return date.toISOString().split('T')[0];
	}

	// Debug selected marker changes (better pattern)
	// $inspect('Selected marker:', selectedMarker?.id);

	// Helper function to normalize coordinates for grouping
	// This ensures coordinates from different sources (geocoded vs direct) are grouped together
	function normalizeCoordinates(lat: number, lng: number): { lat: number; lng: number } {
		// Round to 4 decimal places (~11m precision) to group very close locations
		// This is more aggressive than the previous 5 decimal places to handle precision differences
		const precision = 10000; // 4 decimal places
		return {
			lat: Math.round(lat * precision) / precision,
			lng: Math.round(lng * precision) / precision
		};
	}

	// Clustering logic for multi-scale marker grouping
	function createClusters(
		markers: GroupedSlotMarkerData[],
		zoom: number
	): (GroupedSlotMarkerData | ClusterMarkerData)[] {
		if (!enableClustering || zoom > clusterMaxZoom || markers.length === 0) {
			return markers;
		}

		// Calculate clustering distance based on zoom level and cluster radius
		const clusterDistance = clusterRadius / Math.pow(2, zoom - 1);
		const clusters: Map<string, GroupedSlotMarkerData[]> = new Map();

		// Group markers by proximity
		markers.forEach((marker) => {
			const clusterKey = `${Math.round(marker.lnglat.lat / clusterDistance)}_${Math.round(marker.lnglat.lng / clusterDistance)}`;

			if (!clusters.has(clusterKey)) {
				clusters.set(clusterKey, []);
			}
			clusters.get(clusterKey)!.push(marker);
		});

		const result: (GroupedSlotMarkerData | ClusterMarkerData)[] = [];

		clusters.forEach((clusterMarkers, clusterKey) => {
			if (clusterMarkers.length === 1) {
				// Single marker - no clustering needed
				result.push(clusterMarkers[0]);
			} else {
				// Create cluster marker
				const centerLat =
					clusterMarkers.reduce((sum, m) => sum + m.lnglat.lat, 0) / clusterMarkers.length;
				const centerLng =
					clusterMarkers.reduce((sum, m) => sum + m.lnglat.lng, 0) / clusterMarkers.length;
				const totalSlots = clusterMarkers.reduce((sum, m) => sum + m.slots.length, 0);
				const totalCapacities = clusterMarkers.length;

				const clusterMarker: ClusterMarkerData = {
					id: `cluster_${clusterKey}`,
					lnglat: { lat: centerLat, lng: centerLng },
					markers: clusterMarkers,
					totalSlots,
					totalCapacities,
					clusterLevel: Math.floor(zoom)
				};

				result.push(clusterMarker);
			}
		});

		return result;
	}

	// Helper to check if marker is a cluster
	function isClusterMarker(
		marker: GroupedSlotMarkerData | ClusterMarkerData
	): marker is ClusterMarkerData {
		return 'markers' in marker && 'totalCapacities' in marker;
	}

	// Get unique emojis from cluster markers
	function getUniqueEmojis(clusterMarkers: GroupedSlotMarkerData[]): string[] {
		const emojiSet = new Set<string>();
		clusterMarkers.forEach((marker) => {
			const emoji = marker.capacity.emoji || '🎁'; // Default emoji if none
			emojiSet.add(emoji);
		});
		return Array.from(emojiSet);
	}

	// Track zoom level separately to avoid circular dependencies
	let currentZoom = $state(3);

	// Filter markers based on time filter (not search query)
	const filteredMarkers = $derived.by(() => {
		if (globalState.timeFilterBy === 'any') {
			return shareSlotMarkers;
		}

		// Apply time filter to all markers
		const filtered = shareSlotMarkers.filter((marker) => {
			return passesTimeFilter(marker);
		});

		console.log(
			`[Time Filter] Filtered ${shareSlotMarkers.length} markers to ${filtered.length} using filter: ${globalState.timeFilterBy}`
		);

		return filtered;
	});

	// Use derived instead of effect to prevent circular dependencies
	const clusteredMarkers = $derived.by(() => {
		const result = filteredMarkers.length > 0 ? createClusters(filteredMarkers, currentZoom) : [];

		console.log(
			`[Clustering] Computed ${result.length} display markers from ${filteredMarkers.length} filtered markers at zoom ${currentZoom}`
		);

		return result;
	});

	// Set up zoom event handler (separate from clustering update)
	$effect(() => {
		if (map) {
			const mapInstance = map; // Capture map instance to avoid null checks in closures

			// Initial zoom setup
			currentZoom = mapInstance.getZoom();

			const handleZoomEnd = () => {
				const newZoom = mapInstance.getZoom();
				currentZoom = newZoom;

				// Close panel when zoom changes (if showing cluster results)
				if (selectedMarker && isClusterMarker(selectedMarker)) {
					selectedMarker = null;
					isClusterViewMode = false;
					clusterViewResults = [];
				}
			};

			mapInstance.on('zoomend', handleZoomEnd);
			return () => {
				mapInstance.off('zoomend', handleZoomEnd);
			};
		}
	});

	// Ultra-fast immediate processing for capacities with direct coordinates
	function processCapacityImmediate(
		capacityId: string,
		capacity: any,
		providerId?: string,
		providerName?: string
	): GroupedSlotMarkerData[] {
		console.log(`[Map Stream] Immediate processing ${capacityId}...`);

		try {
			// Show ALL capacities for discovery and desire expression (like capacityMarkers.ts)
			// No need to filter by allocation status - users should see all available capacities

			// Group slots by location for this capacity (immediate processing - coordinates only)
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

			// Process slots in this capacity - IMMEDIATE ONLY (no geocoding)
			for (const slot of capacity.availability_slots || []) {
				// Show ALL slots for discovery and desire expression (no allocation filtering)

				// Check location type
				const locationTypeOk =
					(slot as any).location_type === 'Specific' ||
					((slot as any).location_type === 'Undefined' &&
						slot.latitude !== undefined &&
						slot.longitude !== undefined);

				if (!locationTypeOk) {
					continue;
				}

				let slotLnglat: { lat: number; lng: number } | null = null;
				const source: 'coordinates' | 'geocoded' = 'coordinates';

				// IMMEDIATE: Only use direct coordinates (no geocoding)
				if (slot.latitude !== undefined && slot.longitude !== undefined) {
					slotLnglat = { lat: slot.latitude, lng: slot.longitude };
				}
				// Fallback to capacity coordinates
				else if (
					(capacity as any).location_type === 'Specific' &&
					(capacity as any).latitude !== undefined &&
					(capacity as any).longitude !== undefined
				) {
					slotLnglat = { lat: (capacity as any).latitude, lng: (capacity as any).longitude };
				}

				// If we have immediate coordinates, group this slot by location
				if (slotLnglat) {
					const normalizedCoords = normalizeCoordinates(slotLnglat.lat, slotLnglat.lng);
					const locationKey = `${normalizedCoords.lat},${normalizedCoords.lng}`;
					const groupKey = `${capacityId}:${locationKey}`;

					if (slotGroups.has(groupKey)) {
						slotGroups.get(groupKey)!.slots.push(slot);
					} else {
						slotGroups.set(groupKey, {
							capacityId: capacityId,
							capacity: capacity,
							slots: [slot],
							lnglat: normalizedCoords,
							source: source,
							providerId: providerId,
							providerName: providerName,
							locationKey: locationKey
						});
					}
				}
			}

			// Convert groups to markers
			const newMarkers: GroupedSlotMarkerData[] = [];
			for (const [groupKey, group] of slotGroups.entries()) {
				newMarkers.push({
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

			console.log(`[Map Stream] Immediate completed ${capacityId}: ${newMarkers.length} markers`);
			return newMarkers;
		} catch (error) {
			console.error(`[Map Stream] Immediate error ${capacityId}:`, error);
			return [];
		}
	}

	// Separate geocoding processing for addresses that need API calls
	async function processCapacityGeocode(
		capacityId: string,
		capacity: any,
		providerId?: string,
		providerName?: string
	): Promise<GroupedSlotMarkerData[]> {
		console.log(`[Map Geocode] Processing ${capacityId}...`);

		try {
			// Show ALL capacities for discovery and desire expression (like capacityMarkers.ts)
			// No need to filter by allocation status - users should see all available capacities

			// Group slots by location for this capacity (geocoding processing)
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

			// Process slots that need geocoding
			for (const slot of capacity.availability_slots || []) {
				// Show ALL slots for discovery and desire expression (no allocation filtering)

				// Check location type
				const locationTypeOk =
					(slot as any).location_type === 'Specific' ||
					((slot as any).location_type === 'Undefined' &&
						(slot.street_address ||
							slot.city ||
							slot.state_province ||
							slot.postal_code ||
							slot.country));

				if (!locationTypeOk) {
					continue;
				}

				// Skip if already has direct coordinates (handled by immediate processing)
				if (slot.latitude !== undefined && slot.longitude !== undefined) {
					continue;
				}

				let slotLnglat: { lat: number; lng: number } | null = null;
				let source: 'coordinates' | 'geocoded' = 'geocoded';

				// Geocode slot address
				if (
					slot.street_address ||
					slot.city ||
					slot.state_province ||
					slot.postal_code ||
					slot.country
				) {
					try {
						const geocodeResults = await geocodeCapacityAddress({
							street_address: (slot as any).street_address,
							city: (slot as any).city,
							state_province: (slot as any).state_province,
							postal_code: (slot as any).postal_code,
							country: (slot as any).country
						});

						if (geocodeResults.length > 0) {
							const result = geocodeResults[0];
							slotLnglat = { lat: result.latitude, lng: result.longitude };
						}
					} catch (geocodeError) {
						console.error(`[Map Geocode] Slot ${slot.id} failed:`, geocodeError);
					}
				}

				// Fall back to capacity geocoding
				if (
					!slotLnglat &&
					(capacity as any).location_type === 'Specific' &&
					((capacity as any).street_address ||
						(capacity as any).city ||
						(capacity as any).state_province ||
						(capacity as any).postal_code ||
						(capacity as any).country)
				) {
					try {
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
						}
					} catch (geocodeError) {
						console.warn(`[Map Geocode] Capacity ${capacityId} failed:`, geocodeError);
					}
				}

				// If we have geocoded coordinates, group this slot
				if (slotLnglat) {
					const normalizedCoords = normalizeCoordinates(slotLnglat.lat, slotLnglat.lng);
					const locationKey = `${normalizedCoords.lat},${normalizedCoords.lng}`;
					const groupKey = `${capacityId}:${locationKey}`;

					if (slotGroups.has(groupKey)) {
						slotGroups.get(groupKey)!.slots.push(slot);
					} else {
						slotGroups.set(groupKey, {
							capacityId: capacityId,
							capacity: capacity,
							slots: [slot],
							lnglat: normalizedCoords,
							source: source,
							providerId: providerId,
							providerName: providerName,
							locationKey: locationKey
						});
					}
				}
			}

			// Convert groups to markers
			const newMarkers: GroupedSlotMarkerData[] = [];
			for (const [groupKey, group] of slotGroups.entries()) {
				newMarkers.push({
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

			console.log(`[Map Geocode] Completed ${capacityId}: ${newMarkers.length} markers`);
			return newMarkers;
		} catch (error) {
			console.error(`[Map Geocode] Error ${capacityId}:`, error);
			return [];
		}
	}

	// Ultra-fast immediate processing queue (no delays)
	async function processImmediateQueue() {
		if (isProcessingImmediate || immediateQueue.length === 0) {
			return;
		}

		isProcessingImmediate = true;
		console.log(`[Map Stream] Processing ${immediateQueue.length} immediate capacities`);

		// Process all immediate capacities in parallel (they don't need API calls)
		const promises = immediateQueue.map(async (capacityId) => {
			const sharedCapacities = get(userNetworkCapacitiesWithSlotQuantities);
			if (!sharedCapacities || !sharedCapacities[capacityId]) {
				return [];
			}

			const capacity = sharedCapacities[capacityId];
			const providerId = (capacity as any).provider_id;
			let providerName = 'Unknown Provider';

			// Try to get provider name (quick cache lookup)
			if (providerId) {
				const cachedName = $userNamesOrAliasesCache[providerId];
				if (cachedName) {
					providerName = cachedName;
				} else {
					// Don't await - just use fallback for immediate processing
					getUserName(providerId)
						.then((fetchedName) => {
							if (fetchedName) {
								providerName =
									fetchedName.length > 30 ? fetchedName.substring(0, 30) + '...' : fetchedName;
							}
						})
						.catch(() => {});
				}
			}

			return processCapacityImmediate(capacityId, capacity, providerId, providerName);
		});

		// Wait for all immediate processing to complete
		const results = await Promise.all(promises);
		const allNewMarkers = results.flat();

		if (allNewMarkers.length > 0) {
			shareSlotMarkers = [...shareSlotMarkers, ...allNewMarkers];
			console.log(
				`[Map Stream] Added ${allNewMarkers.length} immediate markers, total: ${shareSlotMarkers.length}`
			);
		}

		immediateQueue = [];
		isProcessingImmediate = false;
	}

	// Geocoding queue processor (with rate limiting)
	async function processGeocodingQueue() {
		if (isProcessingGeocode || geocodingQueue.length === 0) {
			return;
		}

		isProcessingGeocode = true;
		console.log(`[Map Geocode] Processing ${geocodingQueue.length} geocoding capacities`);

		while (geocodingQueue.length > 0) {
			const capacityId = geocodingQueue.shift()!;
			const sharedCapacities = get(userNetworkCapacitiesWithSlotQuantities);

			if (!sharedCapacities || !sharedCapacities[capacityId]) {
				continue;
			}

			const capacity = sharedCapacities[capacityId];
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
					console.warn(`[Map Geocode] Could not get provider name for ${providerId}:`, error);
				}
			}

			// Process this capacity with geocoding and immediately add its markers
			const newMarkers = await processCapacityGeocode(
				capacityId,
				capacity,
				providerId,
				providerName
			);

			if (newMarkers.length > 0) {
				shareSlotMarkers = [...shareSlotMarkers, ...newMarkers];
				console.log(
					`[Map Geocode] Added ${newMarkers.length} geocoded markers, total: ${shareSlotMarkers.length}`
				);
			}
		}

		isProcessingGeocode = false;
		console.log(`[Map Geocode] Queue processing completed`);
	}

	// Smart capacity classification and queuing
	function classifyAndQueueCapacities(capacityIds: string[]) {
		const sharedCapacities = get(userNetworkCapacitiesWithSlotQuantities);
		if (!sharedCapacities) return;

		const immediateCapacities: string[] = [];
		const geocodingCapacities: string[] = [];

		// Classify capacities based on whether they need geocoding
		capacityIds.forEach((capacityId) => {
			const capacity = sharedCapacities[capacityId];
			if (!capacity || !capacity.availability_slots) return;

			let hasDirectCoordinates = false;
			let hasAddresses = false;

			// Check if any slots have direct coordinates or addresses
			for (const slot of capacity.availability_slots) {
				if (slot.latitude !== undefined && slot.longitude !== undefined) {
					hasDirectCoordinates = true;
				}
				if (
					slot.street_address ||
					slot.city ||
					slot.state_province ||
					slot.postal_code ||
					slot.country
				) {
					hasAddresses = true;
				}
			}

			// Also check capacity-level coordinates/addresses
			if ((capacity as any).latitude !== undefined && (capacity as any).longitude !== undefined) {
				hasDirectCoordinates = true;
			}
			if (
				(capacity as any).street_address ||
				(capacity as any).city ||
				(capacity as any).state_province ||
				(capacity as any).postal_code ||
				(capacity as any).country
			) {
				hasAddresses = true;
			}

			// Prioritize immediate processing for capacities with direct coordinates
			if (hasDirectCoordinates) {
				immediateCapacities.push(capacityId);
			}

			// Queue for geocoding if it has addresses (even if it also has coordinates)
			if (hasAddresses) {
				geocodingCapacities.push(capacityId);
			}
		});

		console.log(
			`[Map Stream] Classified ${immediateCapacities.length} immediate, ${geocodingCapacities.length} geocoding`
		);

		// Add to queues
		if (immediateCapacities.length > 0) {
			immediateQueue = [...immediateQueue, ...immediateCapacities];
			// Start immediate processing (no delays)
			processImmediateQueue();
		}

		if (geocodingCapacities.length > 0) {
			geocodingQueue = [...geocodingQueue, ...geocodingCapacities];
			// Start geocoding processing (with rate limiting)
			if (!isProcessingGeocode) {
				processGeocodingQueue();
			}
		}
	}

	// Reactive Svelte 5 approach using $derived.by for side effects
	let currentCapacities = $derived($userNetworkCapacitiesWithSlotQuantities);
	let capacitiesCount = $derived(Object.keys(currentCapacities || {}).length);

	// Ultra-efficient streaming: Handle capacity additions with smart classification
	$effect(() => {
		if (!currentCapacities) {
			shareSlotMarkers = [];
			processedCapacityIds = new Set();
			return;
		}

		const newCapacityIds = Object.keys(currentCapacities);
		const newCapacities = newCapacityIds.filter((id) => !processedCapacityIds.has(id));

		if (newCapacities.length > 0) {
			console.log(`[Map Stream] Found ${newCapacities.length} new capacities to process`);

			// Mark as processed immediately to avoid duplicate processing
			processedCapacityIds = new Set([...processedCapacityIds, ...newCapacities]);

			// Smart classification and immediate processing
			classifyAndQueueCapacities(newCapacities);
		}

		// Handle capacity removals
		const removedCapacityIds = Array.from(processedCapacityIds).filter(
			(id) => !newCapacityIds.includes(id)
		);
		if (removedCapacityIds.length > 0) {
			console.log(`[Map Stream] Removing ${removedCapacityIds.length} capacities`);

			// Remove markers for deleted capacities
			shareSlotMarkers = shareSlotMarkers.filter(
				(marker) => !removedCapacityIds.includes(marker.capacityId)
			);

			// Remove from queues
			immediateQueue = immediateQueue.filter((id) => !removedCapacityIds.includes(id));
			geocodingQueue = geocodingQueue.filter((id) => !removedCapacityIds.includes(id));

			// Update processed set
			processedCapacityIds = new Set(newCapacityIds);
		}
	});

	// Fullscreen escape handling is now managed by FullScreenControl

	// Create a derived status for debugging
	let markersLoader = $derived(
		currentCapacities && Object.keys(currentCapacities).length > 0 ? 'loaded' : 'empty'
	);

	// Debug logging for ultra-efficient streaming
	$inspect('Map ultra-stream state:', {
		capacitiesCount,
		markersLoader,
		totalMarkers: shareSlotMarkers.length,
		processedCapacities: processedCapacityIds.size,
		immediateQueueLength: immediateQueue.length,
		geocodingQueueLength: geocodingQueue.length,
		isProcessingImmediate,
		isProcessingGeocode
	});

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

		// Get recurrence display
		const recurrenceDisplay =
			slot.recurrence && slot.recurrence !== 'Does not repeat' ? slot.recurrence : '';

		let timeStr = '';
		if (slot.all_day) {
			const startDate = slot.start_date ? new Date(slot.start_date) : null;
			const endDate = slot.end_date ? new Date(slot.end_date) : null;
			if (startDate && endDate && startDate.getTime() !== endDate.getTime()) {
				const startStr = formatDateForDisplay(startDate);
				const endStr = formatDateForDisplay(endDate);
				timeStr = `${startStr} - ${endStr}, All day`;
			} else if (startDate) {
				const dateStr = formatDateForDisplay(startDate);
				timeStr = `${dateStr}, All day`;
			} else {
				timeStr = 'All day';
			}
		} else {
			const startDate = slot.start_date ? new Date(slot.start_date) : null;
			const endDate = slot.end_date ? new Date(slot.end_date) : null;

			if (startDate) {
				const startDateStr = formatDateForDisplay(startDate);
				if (endDate && startDate.getTime() !== endDate.getTime()) {
					const endDateStr = formatDateForDisplay(endDate);
					const startTimeStr = cleanStartTime || '';
					const endTimeStr = cleanEndTime || '';
					if (startTimeStr && endTimeStr) {
						timeStr = `${startDateStr}, ${startTimeStr} - ${endDateStr}, ${endTimeStr}`;
					} else if (startTimeStr) {
						timeStr = `${startDateStr}, ${startTimeStr} - ${endDateStr}`;
					} else {
						timeStr = `${startDateStr} - ${endDateStr}`;
					}
				} else {
					if (cleanStartTime) {
						const timeRange = cleanEndTime ? `${cleanStartTime}-${cleanEndTime}` : cleanStartTime;
						timeStr = `${startDateStr}, ${timeRange}`;
					} else {
						timeStr = startDateStr;
					}
				}
			} else if (cleanStartTime) {
				timeStr = cleanEndTime ? `${cleanStartTime}-${cleanEndTime}` : cleanStartTime;
			} else {
				timeStr = 'No time set';
			}
		}

		// Add recurrence if present
		return recurrenceDisplay ? `${timeStr} (${recurrenceDisplay})` : timeStr;
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

	// Helper function to parse slot dates and times consistently
	function parseSlotDateTime(slot: any): {
		slotStart: Date | null;
		slotEnd: Date | null;
	} {
		const slotStart = slot.start_date ? new Date(slot.start_date) : null;
		let slotEnd = slot.end_date ? new Date(slot.end_date) : slotStart ? new Date(slotStart) : null;

		// For all-day events, don't add time components - work with dates only
		if (slot.all_day) {
			// For all-day events, set start to beginning of day and end to end of day
			if (slotStart) {
				slotStart.setHours(0, 0, 0, 0);
			}
			if (slotEnd) {
				slotEnd.setHours(23, 59, 59, 999);
			} else if (slotStart) {
				// If no end date, all-day event ends at end of start day
				slotEnd = new Date(slotStart);
				slotEnd.setHours(23, 59, 59, 999);
			}
		} else {
			// For timed events, add time components using safe extraction
			if (slotStart && slot.start_time) {
				const safeStartTime = safeExtractTime(slot.start_time);
				if (safeStartTime) {
					const [hours, minutes] = safeStartTime.split(':');
					slotStart.setHours(parseInt(hours), parseInt(minutes), 0, 0);
				}
			}

			if (slotEnd && slot.end_time) {
				const safeEndTime = safeExtractTime(slot.end_time);
				if (safeEndTime) {
					const [hours, minutes] = safeEndTime.split(':');
					slotEnd.setHours(parseInt(hours), parseInt(minutes), 59, 999);
				}
			}

			// Handle missing end times for timed events (only when no end_date was specified)
			if (slotStart && !slot.end_date) {
				if (!slot.start_time && !slot.end_time) {
					// No specific times - treat as all-day
					slotEnd = new Date(slotStart);
					slotEnd.setHours(23, 59, 59, 999);
				} else if (slot.start_time && !slot.end_time) {
					// Has start time but no end time - assume 1 hour duration
					slotEnd = new Date(slotStart.getTime() + 60 * 60 * 1000);
				}
			}
		}

		return { slotStart, slotEnd };
	}

	// Helper function to check if a slot is in the past (matches Share.svelte)
	function isSlotInPast(slot: any): boolean {
		if (isSlotRecurring(slot)) return false;

		const now = new Date();
		const { slotStart, slotEnd } = parseSlotDateTime(slot);

		if (!slotStart) return false;

		// Use the effective end time (parseSlotDateTime handles all-day vs timed logic)
		const effectiveEndTime = slotEnd || slotStart;
		const result = effectiveEndTime < now;

		console.log(`[Map Slot Categorization] isSlotInPast check:`, {
			slotId: slot.id,
			startDate: slot.start_date,
			endDate: slot.end_date,
			startTime: slot.start_time,
			endTime: slot.end_time,
			allDay: slot.all_day,
			now: now.toISOString(),
			slotStart: slotStart.toISOString(),
			effectiveEndTime: effectiveEndTime.toISOString(),
			result
		});
		return result;
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
	$inspect('[Map] processedCapacityIds:', processedCapacityIds.size);
	$inspect('[Map] selectedMarker:', selectedMarker);
	$inspect('[Map] Environment check:', {
		hostname: typeof window !== 'undefined' ? window.location.hostname : 'SSR',
		protocol: typeof window !== 'undefined' ? window.location.protocol : 'SSR',
		userAgent: typeof navigator !== 'undefined' ? navigator.userAgent : 'SSR'
	});
</script>

<div class="map-wrapper normal-size relative overflow-hidden rounded-md">
	<!-- Map Container -->
	<div class="map-content full-width">
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
				position="bottom-right"
				positionOptions={{ enableHighAccuracy: true }}
				trackUserLocation={true}
				showAccuracyCircle={true}
				autoTrigger={true}
				ontrackuserlocationstart={handleTrackUserLocationStart}
				ontrackuserlocationend={handleTrackUserLocationEnd}
				ongeolocate={handleGeolocate}
				onerror={handleGeolocateError}
			/>

			<!-- Fullscreen Control -->
			<FullScreenControl position="top-right" />

			<!-- Full MapSidePanel as CustomControl (works in both normal and fullscreen) -->
			<CustomControl position="top-left">
				<MapSidePanel
					markerData={selectedMarker}
					onClose={handleSidePanelClose}
					onBackToSearch={handleBackToSearch}
					isSearchMode={globalState.isSearchMode}
					searchQuery={globalState.searchQuery}
					searchResults={globalState.searchResults}
					searchSortBy={globalState.searchSortBy}
					onSearchResultClick={handleSearchResultClick}
					onSortChange={handleSortChange}
					currentLocation={$currentLocation}
					{isClusterViewMode}
					{clusterViewResults}
					onClusterResultClick={handleClusterResultClick}
				/>
			</CustomControl>

			<!-- Collapsible Controls Group -->
			<CustomControl position="bottom-right">
				<div class="controls-group">
					<!-- Main toggle button -->
					<button
						class="map-control-btn controls-toggle"
						onclick={() => {
							isControlsExpanded = !isControlsExpanded;
						}}
						title="Toggle map controls"
					>
						<span>{isControlsExpanded ? '✕' : '⚙️'}</span>
					</button>

					<!-- Expandable controls -->
					{#if isControlsExpanded}
						<div class="expanded-controls">
							<button
								class="map-control-btn"
								onclick={() => {
									enableClustering = !enableClustering;
								}}
								title="Toggle clustering"
								class:active={enableClustering}
							>
								<span>🗂️</span>
							</button>
							<button
								class="map-control-btn"
								onclick={() => {
									isGlobeMode = !isGlobeMode;
								}}
								title="Toggle globe mode"
							>
								<span>🌐</span>
							</button>
							<button
								class="map-control-btn"
								onclick={() => {
									show3DBuildings = !show3DBuildings;
									if (map) map.setPitch(show3DBuildings ? 70 : 0);
								}}
								title="Toggle 3D buildings"
							>
								<span>🏢</span>
							</button>
							<button
								class="map-control-btn"
								onclick={() => {
									isTerrainVisible = !isTerrainVisible;
									if (map) map.setPitch(isTerrainVisible ? 70 : 0);
								}}
								title="Toggle terrain"
							>
								<span>🏔️</span>
							</button>
						</div>
					{/if}
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

			<!-- Clustered markers - includes both individual and cluster markers -->
			{#each clusteredMarkers as markerData (markerData.id)}
				{#if isClusterMarker(markerData)}
					<!-- Cluster marker -->
					{@const { id, lnglat, markers, totalSlots, totalCapacities } = markerData}
					{@const isSelected = selectedMarker?.id === markerData.id}
					{@const uniqueEmojis = getUniqueEmojis(markers)}
					{@const showEmojiPack = uniqueEmojis.length <= 9}

					<Marker
						lnglat={{ lng: lnglat.lng, lat: lnglat.lat }}
						draggable={false}
						color={isSelected ? '#f59e0b' : '#ff6b6b'}
						scale={isSelected ? 1.3 : Math.min(1.5, 1.0 + totalCapacities * 0.1)}
					>
						{#snippet content()}
							<div
								class="cluster-marker {isSelected ? 'selected' : ''}"
								onclick={() => handleMarkerClick(markerData)}
								role="button"
								tabindex="0"
								onkeydown={(e) => {
									if (e.key === 'Enter' || e.key === ' ') {
										e.preventDefault();
										handleMarkerClick(markerData);
									}
								}}
							>
								{#if showEmojiPack}
									<!-- Show emoji pack for 5 or fewer unique emojis -->
									<div class="emoji-pack" class:single-emoji={uniqueEmojis.length === 1}>
										{#each uniqueEmojis as emoji, index}
											<span
												class="packed-emoji"
												style="--emoji-index: {index}; --total-emojis: {uniqueEmojis.length};"
											>
												{emoji}
											</span>
										{/each}
									</div>
								{:else}
									<!-- Show count for more than 5 unique emojis -->
									<div class="cluster-count-display">
										<div class="cluster-count-number">{totalCapacities}</div>
										<div class="cluster-count-label">capacities</div>
									</div>
								{/if}
								{#if isSelected}
									<div class="selection-ring"></div>
								{/if}
							</div>
						{/snippet}
					</Marker>
				{:else}
					<!-- Individual marker -->
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
								onkeydown={(e) => {
									if (e.key === 'Enter' || e.key === ' ') {
										e.preventDefault();
										handleMarkerClick(markerData);
									}
								}}
							>
								<div class="marker-icon">{capacity.emoji || '🏠'}</div>
								<div class="marker-label">{capacity.name}</div>
								{#if isSelected}
									<div class="selection-ring"></div>
								{/if}
							</div>
						{/snippet}
					</Marker>
				{/if}
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
</div>

<style>
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

	/* Cluster marker styles */
	.cluster-marker {
		display: flex;
		flex-direction: column;
		align-items: center;
		justify-content: center;
		cursor: pointer;
		transition: transform 0.2s ease;
		position: relative;
		width: 40px;
		height: 40px;
		background: rgba(255, 107, 107, 0.1);
		border-radius: 50%;
		border: 2px solid #ff6b6b;
	}

	.cluster-marker:hover {
		transform: scale(1.1);
		background: rgba(255, 107, 107, 0.2);
	}

	.cluster-marker.selected {
		transform: scale(1.15);
		background: rgba(245, 158, 11, 0.2);
		border-color: #f59e0b;
	}

	/* Emoji pack container */
	.emoji-pack {
		position: relative;
		width: 32px;
		height: 32px;
		display: flex;
		align-items: center;
		justify-content: center;
	}

	/* Single emoji takes full space - no additional styling needed for .emoji-pack.single-emoji */

	/* Individual packed emojis with circular positioning */
	.packed-emoji {
		position: absolute;
		font-size: 12px;
		text-shadow: 0 1px 2px rgba(0, 0, 0, 0.3);
		line-height: 1;
		transition: all 0.2s ease;
	}

	/* Position emojis in a circle for multiple emojis */
	.emoji-pack:not(.single-emoji) .packed-emoji {
		font-size: 10px;
		transform-origin: center;
	}

	/* Single emoji positioning */
	.emoji-pack.single-emoji .packed-emoji {
		position: relative;
		font-size: 16px;
	}

	/* 2 emojis: side by side */
	.packed-emoji[style*='--total-emojis: 2'][style*='--emoji-index: 0'] {
		transform: translateX(-6px);
	}
	.packed-emoji[style*='--total-emojis: 2'][style*='--emoji-index: 1'] {
		transform: translateX(6px);
	}

	/* 3 emojis: triangle */
	.packed-emoji[style*='--total-emojis: 3'][style*='--emoji-index: 0'] {
		transform: translateY(-6px);
	}
	.packed-emoji[style*='--total-emojis: 3'][style*='--emoji-index: 1'] {
		transform: translate(-6px, 4px);
	}
	.packed-emoji[style*='--total-emojis: 3'][style*='--emoji-index: 2'] {
		transform: translate(6px, 4px);
	}

	/* 4 emojis: square */
	.packed-emoji[style*='--total-emojis: 4'][style*='--emoji-index: 0'] {
		transform: translate(-5px, -5px);
	}
	.packed-emoji[style*='--total-emojis: 4'][style*='--emoji-index: 1'] {
		transform: translate(5px, -5px);
	}
	.packed-emoji[style*='--total-emojis: 4'][style*='--emoji-index: 2'] {
		transform: translate(-5px, 5px);
	}
	.packed-emoji[style*='--total-emojis: 4'][style*='--emoji-index: 3'] {
		transform: translate(5px, 5px);
	}

	/* 5 emojis: pentagon-like */
	.packed-emoji[style*='--total-emojis: 5'][style*='--emoji-index: 0'] {
		transform: translateY(-8px);
	}
	.packed-emoji[style*='--total-emojis: 5'][style*='--emoji-index: 1'] {
		transform: translate(7px, -3px);
	}
	.packed-emoji[style*='--total-emojis: 5'][style*='--emoji-index: 2'] {
		transform: translate(4px, 6px);
	}
	.packed-emoji[style*='--total-emojis: 5'][style*='--emoji-index: 3'] {
		transform: translate(-4px, 6px);
	}
	.packed-emoji[style*='--total-emojis: 5'][style*='--emoji-index: 4'] {
		transform: translate(-7px, -3px);
	}

	/* 6 emojis: hexagon */
	.packed-emoji[style*='--total-emojis: 6'][style*='--emoji-index: 0'] {
		transform: translateY(-9px);
	}
	.packed-emoji[style*='--total-emojis: 6'][style*='--emoji-index: 1'] {
		transform: translate(8px, -4px);
	}
	.packed-emoji[style*='--total-emojis: 6'][style*='--emoji-index: 2'] {
		transform: translate(8px, 4px);
	}
	.packed-emoji[style*='--total-emojis: 6'][style*='--emoji-index: 3'] {
		transform: translateY(9px);
	}
	.packed-emoji[style*='--total-emojis: 6'][style*='--emoji-index: 4'] {
		transform: translate(-8px, 4px);
	}
	.packed-emoji[style*='--total-emojis: 6'][style*='--emoji-index: 5'] {
		transform: translate(-8px, -4px);
	}

	/* 7 emojis: center + hexagon */
	.packed-emoji[style*='--total-emojis: 7'][style*='--emoji-index: 0'] {
		transform: translate(0, 0); /* center */
	}
	.packed-emoji[style*='--total-emojis: 7'][style*='--emoji-index: 1'] {
		transform: translateY(-10px);
	}
	.packed-emoji[style*='--total-emojis: 7'][style*='--emoji-index: 2'] {
		transform: translate(9px, -5px);
	}
	.packed-emoji[style*='--total-emojis: 7'][style*='--emoji-index: 3'] {
		transform: translate(9px, 5px);
	}
	.packed-emoji[style*='--total-emojis: 7'][style*='--emoji-index: 4'] {
		transform: translateY(10px);
	}
	.packed-emoji[style*='--total-emojis: 7'][style*='--emoji-index: 5'] {
		transform: translate(-9px, 5px);
	}
	.packed-emoji[style*='--total-emojis: 7'][style*='--emoji-index: 6'] {
		transform: translate(-9px, -5px);
	}

	/* 8 emojis: octagon */
	.packed-emoji[style*='--total-emojis: 8'][style*='--emoji-index: 0'] {
		transform: translateY(-10px);
	}
	.packed-emoji[style*='--total-emojis: 8'][style*='--emoji-index: 1'] {
		transform: translate(7px, -7px);
	}
	.packed-emoji[style*='--total-emojis: 8'][style*='--emoji-index: 2'] {
		transform: translate(10px, 0);
	}
	.packed-emoji[style*='--total-emojis: 8'][style*='--emoji-index: 3'] {
		transform: translate(7px, 7px);
	}
	.packed-emoji[style*='--total-emojis: 8'][style*='--emoji-index: 4'] {
		transform: translateY(10px);
	}
	.packed-emoji[style*='--total-emojis: 8'][style*='--emoji-index: 5'] {
		transform: translate(-7px, 7px);
	}
	.packed-emoji[style*='--total-emojis: 8'][style*='--emoji-index: 6'] {
		transform: translate(-10px, 0);
	}
	.packed-emoji[style*='--total-emojis: 8'][style*='--emoji-index: 7'] {
		transform: translate(-7px, -7px);
	}

	/* 9 emojis: 3x3 grid */
	.packed-emoji[style*='--total-emojis: 9'][style*='--emoji-index: 0'] {
		transform: translate(-6px, -6px);
	}
	.packed-emoji[style*='--total-emojis: 9'][style*='--emoji-index: 1'] {
		transform: translate(0, -6px);
	}
	.packed-emoji[style*='--total-emojis: 9'][style*='--emoji-index: 2'] {
		transform: translate(6px, -6px);
	}
	.packed-emoji[style*='--total-emojis: 9'][style*='--emoji-index: 3'] {
		transform: translate(-6px, 0);
	}
	.packed-emoji[style*='--total-emojis: 9'][style*='--emoji-index: 4'] {
		transform: translate(0, 0);
	}
	.packed-emoji[style*='--total-emojis: 9'][style*='--emoji-index: 5'] {
		transform: translate(6px, 0);
	}
	.packed-emoji[style*='--total-emojis: 9'][style*='--emoji-index: 6'] {
		transform: translate(-6px, 6px);
	}
	.packed-emoji[style*='--total-emojis: 9'][style*='--emoji-index: 7'] {
		transform: translate(0, 6px);
	}
	.packed-emoji[style*='--total-emojis: 9'][style*='--emoji-index: 8'] {
		transform: translate(6px, 6px);
	}

	/* Count display for >9 emojis */
	.cluster-count-display {
		display: flex;
		flex-direction: column;
		align-items: center;
		justify-content: center;
		text-align: center;
	}

	.cluster-count-number {
		font-size: 14px;
		font-weight: bold;
		color: #374151;
		line-height: 1;
		text-shadow: 0 1px 2px rgba(255, 255, 255, 0.8);
	}

	.cluster-count-label {
		font-size: 8px;
		font-weight: 500;
		color: #6b7280;
		line-height: 1;
		margin-top: 1px;
		text-shadow: 0 1px 2px rgba(255, 255, 255, 0.8);
	}

	/* Map wrapper and content layout styles */
	.map-wrapper {
		position: relative;
		overflow: hidden;
		transition: all 0.3s ease-out;
	}

	.map-wrapper.normal-size {
		height: 400px;
		max-height: 50vh;
		min-height: 300px;
	}

	.map-content {
		height: 100%;
		transition: margin-left 0.3s ease-out;
	}

	.map-content.full-width {
		margin-left: 0;
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

	@keyframes fadeInFromLeft {
		from {
			opacity: 0;
			transform: translateX(-20px);
		}
		to {
			opacity: 1;
			transform: translateX(0);
		}
	}

	@keyframes fadeInDown {
		from {
			opacity: 0;
			transform: translateY(-20px);
		}
		to {
			opacity: 1;
			transform: translateY(0);
		}
	}

	/* Map Control Styles */
	:global(.map-control-btn) {
		background: rgba(255, 255, 255, 0.9);
		border: 1px solid rgba(0, 0, 0, 0.1);
		border-radius: 4px;
		width: 32px;
		height: 32px;
		display: flex;
		align-items: center;
		justify-content: center;
		cursor: pointer;
		font-size: 14px;
		transition: all 0.2s ease;
		backdrop-filter: blur(2px);
		box-shadow: 0 1px 3px rgba(0, 0, 0, 0.1);
	}

	:global(.map-control-btn:hover) {
		background: rgba(255, 255, 255, 1);
		box-shadow: 0 2px 6px rgba(0, 0, 0, 0.15);
		transform: translateY(-1px);
	}

	:global(.map-control-btn:active) {
		transform: translateY(0);
		box-shadow: 0 1px 2px rgba(0, 0, 0, 0.1);
	}

	:global(.map-control-btn.active) {
		background: rgba(59, 130, 246, 0.1);
		border-color: #3b82f6;
		color: #3b82f6;
	}

	/* Controls Group Container */
	:global(.controls-group) {
		display: flex;
		flex-direction: column;
		gap: 4px;
		align-items: flex-end;
	}

	/* Expanded Controls Animation */
	:global(.expanded-controls) {
		display: flex;
		flex-direction: column;
		gap: 4px;
		animation: expandControls 0.3s ease-out;
		transform-origin: bottom;
	}

	@keyframes expandControls {
		from {
			opacity: 0;
			transform: scaleY(0) translateY(10px);
		}
		to {
			opacity: 1;
			transform: scaleY(1) translateY(0);
		}
	}

	/* Toggle Button Styling */
	:global(.controls-toggle) {
		position: relative;
	}

	:global(.controls-toggle span) {
		transition: transform 0.2s ease;
	}

	/* Ensure CustomControl respects map dimensions */
	:global(.maplibregl-ctrl-top-left) {
		max-height: calc(100% - 20px);
		max-width: calc(100% - 20px);
	}

	/* Ensure map container establishes proper sizing context */
	:global(.maplibregl-map) {
		position: relative;
	}
</style>
