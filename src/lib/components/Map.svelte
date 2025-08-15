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
	import { debounce } from '$lib/utils/debounce';
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
	// Use global fullscreen state instead of local state
	let isMaximized = $derived(globalState.isMapFullscreen);
	let isControlsExpanded = $state(false);

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

	// Filtered markers - we'll implement time filtering as a future enhancement
	// For now, show all markers to ensure basic functionality works
	// TODO: Add time filtering back once the basic display is working

	// Selected marker state for side panel
	let selectedMarker = $state<GroupedSlotMarkerData | null>(null);

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
	function handleMarkerClick(markerData: GroupedSlotMarkerData) {
		selectedMarker = markerData;
	}

	// Handle side panel close
	function handleSidePanelClose() {
		selectedMarker = null;
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

		// Filter markers based on search query and time filters
		const filtered = shareSlotMarkers.filter((marker) => {
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

			// Time filter
			const matchesTime = passesTimeFilter(marker);

			return matchesText && matchesTime;
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
	$inspect('Selected marker:', selectedMarker?.id);

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

	// Async function to load grouped slot markers from shared capacities
	async function loadShareSlotMarkers(
		sharedCapacities: typeof $userNetworkCapacitiesWithSlotQuantities
	) {
		if (!sharedCapacities) {
			shareSlotMarkers = [];
			return;
		}

		if (isLoadingGeocode) {
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
			isLoadingGeocode = true;
			const capacityEntries = Object.entries(sharedCapacities);

			for (const [capacityId, capacity] of capacityEntries) {
				if (!capacity.availability_slots || !Array.isArray(capacity.availability_slots)) {
					continue;
				}

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

				// Helper function to count active slots (slots with computed quantity > 0)
				// This matches the logic from Shares.svelte
				function getActiveSlotCount(capacity: any): number {
					// Type guard: only RecipientCapacity has computed_quantities
					if (
						!('computed_quantities' in capacity) ||
						!capacity.computed_quantities ||
						!Array.isArray(capacity.computed_quantities)
					) {
						return 0;
					}
					return capacity.computed_quantities.filter((slot: any) => slot.quantity > 0).length;
				}

				// Helper function to get computed quantity for a specific slot
				function getSlotComputedQuantity(capacity: any, slotId: string): number {
					// Type guard: only RecipientCapacity has computed_quantities
					if (
						!('computed_quantities' in capacity) ||
						!capacity.computed_quantities ||
						!Array.isArray(capacity.computed_quantities)
					) {
						return 0;
					}
					const slotQuantity = capacity.computed_quantities.find(
						(cq: any) => cq.slot_id === slotId
					);
					return slotQuantity?.quantity || 0;
				}

				// First check if this capacity has any active slots (with computed quantity > 0)
				// This respects divisibility constraints like max_natural_div and max_percentage_div
				const activeSlotCount = getActiveSlotCount(capacity);
				if (activeSlotCount === 0) {
					// Skip capacities with no active slots (respects divisibility constraints)
					continue;
				}

				// Process slots in this capacity
				for (const slot of capacity.availability_slots) {
					// Check if this specific slot has a computed quantity > 0
					const slotComputedQuantity = getSlotComputedQuantity(capacity, slot.id);
					if (slotComputedQuantity <= 0) {
						// Skip slots with no computed quantity (respects divisibility constraints)
						continue;
					}

					// Check location type - only show "Specific" locations on map
					// Also allow Undefined if it has actual location data
					const locationTypeOk =
						(slot as any).location_type === 'Specific' ||
						((slot as any).location_type === 'Undefined' &&
							// Has coordinates
							((slot.latitude !== undefined && slot.longitude !== undefined) ||
								// Has address components
								slot.street_address ||
								slot.city ||
								slot.state_province ||
								slot.postal_code ||
								slot.country));

					if (!locationTypeOk) {
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
							const geocodeResults = await geocodeCapacityAddress({
								street_address: (slot as any).street_address,
								city: (slot as any).city,
								state_province: (slot as any).state_province,
								postal_code: (slot as any).postal_code,
								country: (slot as any).country
							});

							if (geocodeResults.length > 0) {
								const result = geocodeResults[0]; // Use the first (best) result
								slotLnglat = { lat: result.latitude, lng: result.longitude };
								source = 'geocoded';
							}
						} catch (geocodeError) {
							console.error(`[Map] ❌ Geocoding failed for slot ${slot.id}:`, {
								error: geocodeError,
								message:
									geocodeError instanceof Error ? geocodeError.message : String(geocodeError),
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
								console.warn(`[Map] Capacity geocoding failed for slot ${slot.id}:`, geocodeError);
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
						// Normalize coordinates to handle precision differences from different sources
						const normalizedCoords = normalizeCoordinates(slotLnglat.lat, slotLnglat.lng);
						const locationKey = `${normalizedCoords.lat},${normalizedCoords.lng}`;
						const groupKey = `${capacityId}:${locationKey}`;

						if (slotGroups.has(groupKey)) {
							// Add slot to existing group
							slotGroups.get(groupKey)!.slots.push(slot);
						} else {
							// Create new group using normalized coordinates for consistent positioning
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

						console.log(
							`[Map] Added slot to group ${groupKey}: ${capacity.name} - slot ${slot.id} at ${slotLnglat.lat}, ${slotLnglat.lng} -> normalized: ${normalizedCoords.lat}, ${normalizedCoords.lng} (${source})`
						);
					} else {
						console.log(`[Map] No location data available for slot ${slot.id}`);
					}
				}
			}

			// 🚨 CRITICAL BREAKPOINT 6: Check final markers creation
			console.log('[Map] 🚨 PRODUCTION DEBUG: Converting', slotGroups.size, 'groups to markers');

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

			// 🚨 CRITICAL BREAKPOINT 7: Final marker assignment
			console.log(
				'[Map] 🚨 PRODUCTION DEBUG: Setting shareSlotMarkers to',
				markers.length,
				'markers'
			);
			shareSlotMarkers = markers;
			console.log(
				'[Map] 🚨 PRODUCTION DEBUG: shareSlotMarkers.length is now:',
				shareSlotMarkers.length
			);

			console.log(`[Map] Created ${markers.length} grouped markers from shared capacities`);
			console.log(
				`[Map] Total slots represented: ${markers.reduce((sum, m) => sum + m.slots.length, 0)}`
			);
		} catch (error) {
			console.error('[Map] ❌ Error loading share slot markers:', error);
			console.error('[Map] 🚨 PRODUCTION DEBUG: Full error details:', {
				name: error instanceof Error ? error.name : 'Unknown',
				message: error instanceof Error ? error.message : String(error),
				stack: error instanceof Error ? error.stack : undefined
			});
			shareSlotMarkers = [];
		} finally {
			console.log('[Map] 🚨 PRODUCTION DEBUG: Setting isLoadingGeocode = false');
			isLoadingGeocode = false;
			console.log('[Map] 🚨 PRODUCTION DEBUG: isLoadingGeocode is now:', isLoadingGeocode);
		}
	}

	// Reactive Svelte 5 approach using $derived.by for side effects
	let currentCapacities = $derived($userNetworkCapacitiesWithSlotQuantities);
	let capacitiesCount = $derived(Object.keys(currentCapacities || {}).length);

	// 🚨 CRITICAL BREAKPOINT 8: Check debounce function
	console.log('[Map] 🚨 PRODUCTION DEBUG: Creating debounced function');

	// Debounced loading function to prevent excessive API calls
	const debouncedLoadMarkers = debounce(async (capacities: typeof currentCapacities) => {
		console.log(
			'[Map] 🚨 PRODUCTION DEBUG: debouncedLoadMarkers called with',
			capacities ? Object.keys(capacities).length : 'null',
			'capacities'
		);
		if (capacities && Object.keys(capacities).length > 0) {
			console.log('[Map] Loading markers for', Object.keys(capacities).length, 'capacities');
			await loadShareSlotMarkers(capacities);
		}
	}, 300);

	$effect(() => {
		if (currentCapacities && Object.keys(currentCapacities).length > 0) {
			debouncedLoadMarkers(currentCapacities);
		} else {
			shareSlotMarkers = [];
		}
	});

	// Handle escape key to minimize map
	$effect(() => {
		function handleKeyDown(event: KeyboardEvent) {
			if (event.key === 'Escape' && isMaximized) {
				globalState.setMapFullscreen(false);
			}
		}

		if (typeof window !== 'undefined') {
			window.addEventListener('keydown', handleKeyDown);
			return () => {
				window.removeEventListener('keydown', handleKeyDown);
			};
		}
	});

	// Create a derived status for debugging
	let markersLoader = $derived(
		currentCapacities && Object.keys(currentCapacities).length > 0 ? 'loaded' : 'empty'
	);

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
	$inspect('[Map] isLoadingGeocode:', isLoadingGeocode);
	$inspect('[Map] selectedMarker:', selectedMarker);
	$inspect('[Map] Environment check:', {
		hostname: typeof window !== 'undefined' ? window.location.hostname : 'SSR',
		protocol: typeof window !== 'undefined' ? window.location.protocol : 'SSR',
		userAgent: typeof navigator !== 'undefined' ? navigator.userAgent : 'SSR'
	});
</script>

<div
	class="map-wrapper relative overflow-hidden rounded-md {isMaximized
		? 'fullscreen'
		: 'normal-size'}"
>
	<!-- Side Panel (positioned absolute within this container) -->
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
		timeFilterBy={globalState.timeFilterBy}
		timeFilterStartDate={globalState.timeFilterStartDate}
		timeFilterEndDate={globalState.timeFilterEndDate}
		timeFilterStartTime={globalState.timeFilterStartTime}
		timeFilterEndTime={globalState.timeFilterEndTime}
		showTimeFilterDetails={globalState.showTimeFilterDetails}
		onTimeFilterChange={(filter) => {
			globalState.updateTimeFilter(filter);
			handleTimeFilterChange();
		}}
		onTimeFilterDetailsChange={(details) => {
			globalState.updateTimeFilterDetails(details);
			handleTimeFilterDetailsChange();
		}}
	/>

	<!-- Map Container (takes full space - panel is positioned absolutely) -->
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

			<!-- Maximize/Minimize Control -->
			<CustomControl position="bottom-right">
				<button
					class="map-control-btn"
					onclick={() => {
						globalState.toggleMapFullscreen();
					}}
					title={isMaximized ? 'Minimize map' : 'Maximize map'}
				>
					<span>{isMaximized ? '⊟' : '⊞'}</span>
				</button>
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

			<!-- Grouped share slot markers - filtered by time if applicable -->
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
							onkeydown={(e) => {
								if (e.key === 'Enter' || e.key === ' ') {
									e.preventDefault();
									handleMarkerClick(markerData);
								}
							}}
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

			<!-- Loading indicator for geocoding 
			{#if isLoadingGeocode}
				<CustomControl position="bottom-right">
					<div class="geocoding-loading">
						<span>🗺️ Geocoding slot addresses...</span>
					</div>
				</CustomControl>
			{/if}-->

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

	.map-wrapper.fullscreen {
		position: fixed;
		top: 76px; /* Account for header height */
		left: 16px; /* Match app-content padding */
		right: 16px; /* Match app-content padding */
		bottom: 16px; /* Match app-content padding */
		z-index: 1000;
		border-radius: 8px;
		max-height: none;
		min-height: none;
		width: auto;
		height: auto;
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
</style>
