import { writable, derived } from 'svelte/store';
import type { Writable, Readable } from 'svelte/store';
// V5: Import from v5 stores - both needs AND capacities
import {
	myCapacitySlotsStore,
	myNeedSlotsStore,
	myCommitmentStore,
	networkAllocations
} from '$lib/protocol/stores/stores.svelte';
import type { AvailabilitySlot, NeedSlot } from '$lib/protocol/schemas';

// Live location data interface
export interface LiveLocationData {
	latitude: number;
	longitude: number;
	accuracy?: number;
	altitude?: number | null;
	altitudeAccuracy?: number | null;
	heading?: number | null;
	speed?: number | null;
	timestamp: number;
	emoji?: string; // Transport/Status emoji
}

// Location tracking state
export const currentLocation: Writable<LiveLocationData | null> = writable(null);
export const isLocationTracking = writable(false);
export const locationError = writable<string | null>(null);

// Block list for live location access
export const liveLocationBlockList: Writable<string[]> = writable([]);

// Network participants' locations (from network or simulator)
export const networkLocations: Writable<Record<string, LiveLocationData>> = writable({});

// Trips for visualization
export interface TripWaypoint {
	coords: [number, number, number]; // [lng, lat, altitude] for 3D paths
	timestamp: number; // 0 to LOOP_DURATION
}

export interface TripData {
	id: string;
	waypoints: TripWaypoint[];
	color: [number, number, number];
	transportMode: TransportMode;
}

export const simulatedTrips: Writable<TripData[]> = writable([]);
export const LOOP_DURATION = 120; // 2 minutes loop for everyone
export const TRAIL_LENGTH = 50; // Length of the trail in seconds

// Simulator state
export const isSimulatorActive = writable(false);
let simulatorIntervalId: number | null = null;

// Update location from geolocation events
export function updateLocation(coords: GeolocationCoordinates, timestamp?: number) {
	const locationData: LiveLocationData = {
		latitude: coords.latitude,
		longitude: coords.longitude,
		accuracy: coords.accuracy,
		altitude: coords.altitude,
		altitudeAccuracy: coords.altitudeAccuracy,
		heading: coords.heading,
		speed: coords.speed,
		timestamp: timestamp || Date.now()
	};

	currentLocation.set(locationData);
	locationError.set(null);

	console.log('[LIVE-LOCATION] Updated:', locationData);
}

// Set location tracking state
export function setLocationTracking(isTracking: boolean) {
	isLocationTracking.set(isTracking);
}

// Set location error
export function setLocationError(error: string | null) {
	locationError.set(error);
	if (error) {
		console.log('[LIVE-LOCATION] Error:', error);
	}
}

// Derived store for location coordinates as [lng, lat] (MapLibre format)
export const currentLocationLngLat = derived(currentLocation, ($location) => {
	if (!$location) return null;
	return [$location.longitude, $location.latitude] as [number, number];
});

// Derived store for location text display
export const currentLocationText = derived(currentLocation, ($location) => {
	if (!$location) return 'No location';
	return `${$location.latitude.toFixed(6)}, ${$location.longitude.toFixed(6)}`;
});

// Helper: Check if a slot is marked for live location sharing
function isLiveLocationSlot(slot: AvailabilitySlot | NeedSlot): boolean {
	return slot.location_type === 'Live';
}

// Helper: Check if slot is active right now (simplified - checks if availability_window exists)
// TODO: Implement full time-matching logic against availability_window schedules
function isSlotActiveNow(slot: AvailabilitySlot | NeedSlot): boolean {
	if (!slot.availability_window) return false;

	const window = slot.availability_window;

	// For MVP: if any availability window is defined, consider it potentially active
	// Full implementation would check current month/week/day/time against schedules
	return !!(window.month_schedules || window.week_schedules ||
		window.day_schedules || window.time_ranges);
}

// Derived store: Slots marked for live location sharing (from both needs AND capacities)
export const liveLocationSlots: Readable<(AvailabilitySlot | NeedSlot)[]> = derived(
	[myCapacitySlotsStore, myNeedSlotsStore],
	([$capacity, $needs]) => {
		const slots: (AvailabilitySlot | NeedSlot)[] = [];

		if ($capacity) {
			slots.push(...$capacity.filter(isLiveLocationSlot));
		}

		if ($needs) {
			slots.push(...$needs.filter(isLiveLocationSlot));
		}

		console.log(`[LIVE-LOCATION-SLOTS] Found ${slots.length} Live location slots`);
		return slots;
	}
);

// Derived store: Whether live location should be shared right now
export const shouldShareLiveLocation: Readable<boolean> = derived(
	[liveLocationSlots],
	([$slots]) => {
		if ($slots.length === 0) return false;

		// Check if ANY slot is currently active (within availability window)
		const hasActiveSlot = $slots.some(isSlotActiveNow);

		console.log(`[LIVE-LOCATION-ACTIVE] ${hasActiveSlot ? 'YES' : 'NO'} - ${$slots.length} slots, ${$slots.filter(isSlotActiveNow).length} active`);
		return hasActiveSlot;
	}
);

// Derived store: Who has access to live location (allocation-based + time-aware)
export const liveLocationAccessList: Readable<string[]> = derived(
	[myCommitmentStore, networkAllocations, liveLocationSlots, shouldShareLiveLocation],
	([$commitment, $networkAllocs, $liveSlots, $shouldShare]) => {
		if (!$shouldShare) {
			console.log('[LIVE-LOCATION-ACCESS] Not sharing - no active slots');
			return [];
		}

		const accessSet = new Set<string>();

		// From CAPACITY slots: Share with allocated recipients
		for (const slot of $liveSlots) {
			const allocations = $commitment?.slot_allocations || [];
			const slotAllocs = allocations.filter(a => a.availability_slot_id === slot.id);

			for (const alloc of slotAllocs) {
				accessSet.add(alloc.recipient_pubkey);
				console.log(`[LIVE-LOCATION-ACCESS] Added recipient: ${alloc.recipient_pubkey.slice(0, 20)}... from capacity slot ${slot.id}`);
			}
		}

		// From NEED slots: Share with allocated providers
		for (const slot of $liveSlots) {
			// Check network allocations for providers who allocated to this need
			for (const [providerPub, theirAllocs] of Object.entries($networkAllocs)) {
				if (!theirAllocs) continue;

				const hasAllocation = theirAllocs.some(a =>
					a.recipient_need_slot_id === slot.id
				);

				if (hasAllocation) {
					accessSet.add(providerPub);
					console.log(`[LIVE-LOCATION-ACCESS] Added provider: ${providerPub.slice(0, 20)}... from need slot ${slot.id}`);
				}
			}
		}

		const accessList = Array.from(accessSet);
		console.log(`[LIVE-LOCATION-ACCESS] Total access list: ${accessList.length} users`);
		return accessList;
	}
);

// Filtered access list that excludes blocked users
export const filteredLiveLocationAccessList: Readable<string[]> = derived(
	[liveLocationAccessList, liveLocationBlockList],
	([$accessList, $blockList]) => {
		const filteredList = $accessList.filter((userId) => !$blockList.includes(userId));
		console.log(
			'[LIVE-LOCATION-ACCESS] Filtered access list (excluding blocked users):',
			filteredList
		);
		return filteredList;
	}
);

// ═══════════════════════════════════════════════════════════════════
// LOCATION SIMULATOR (For Testing & Visualization)
// ═══════════════════════════════════════════════════════════════════

// Transport Types & Speeds
type TransportMode = 'walking' | 'cycling' | 'driving' | 'bussing' | 'training' | 'flying' | 'helicoptering' | 'rocketing' | 'sailing';

interface TransportConfig {
	mode: TransportMode;
	emoji: string;
	speedIds: number; // relative speed multiplier
	maxSpeedMps: number; // max speed in m/s
}

const TRANSPORT_MODES: TransportConfig[] = [
	{ mode: 'walking', emoji: '🏃', speedIds: 1, maxSpeedMps: 1.5 },
	{ mode: 'cycling', emoji: '🚲', speedIds: 4, maxSpeedMps: 6 },
	{ mode: 'driving', emoji: '🚗', speedIds: 15, maxSpeedMps: 30 },
	{ mode: 'bussing', emoji: '🚌', speedIds: 12, maxSpeedMps: 20 },
	{ mode: 'training', emoji: '🚄', speedIds: 40, maxSpeedMps: 80 },
	{ mode: 'flying', emoji: '✈️', speedIds: 150, maxSpeedMps: 250 },
	{ mode: 'helicoptering', emoji: '🚁', speedIds: 60, maxSpeedMps: 60 },
	{ mode: 'rocketing', emoji: '🚀', speedIds: 500, maxSpeedMps: 8000 },
	{ mode: 'sailing', emoji: '🚢', speedIds: 8, maxSpeedMps: 10 }
];

/**
 * Get altitude based on transport mode
 */
function getAltitudeForMode(mode: TransportMode): number {
	switch (mode) {
		case 'rocketing': return 200000; // 200km
		case 'flying': return 10000; // 10km
		case 'helicoptering': return 1000; // 1km
		default: return 0;
	}
}

/**
 * Generate a random location within radius of center or global if radius is large
 */
function randomLocationNear(center: [number, number], radius: number): [number, number] {
	if (radius > 100) {
		// Global distribution
		const lat = (Math.random() - 0.5) * 160; // Avoid extreme poles (-80 to 80)
		const lng = (Math.random() - 0.5) * 360;
		return [lat, lng];
	}
	const angle = Math.random() * 2 * Math.PI;
	const distance = Math.random() * radius;
	const lat = center[0] + distance * Math.cos(angle);
	const lng = center[1] + distance * Math.sin(angle);
	return [lat, lng];
}


/**
 * Interpolate position along a trip path based on current loop time
 */
function moveAlongPath(
	waypoints: TripWaypoint[],
	time: number
): { coords: [number, number, number]; heading: number } {
	// Find the segment we are currently in
	// timestamps are sorted 0..LOOP_DURATION

	// Handle wrapping time just in case
	const t = time % LOOP_DURATION;

	// Find index where waypoints[i].timestamp <= t
	let idx = 0;
	for (let i = 0; i < waypoints.length - 1; i++) {
		if (t >= waypoints[i].timestamp && t < waypoints[i + 1].timestamp) {
			idx = i;
			break;
		}
	}

	// If we are at the very end, wrap to start
	if (t >= waypoints[waypoints.length - 1].timestamp) {
		idx = waypoints.length - 1;
	}

	const p1 = waypoints[idx];
	const p2 = waypoints[(idx + 1) % waypoints.length];

	// Calculate progress between p1 and p2
	let duration = p2.timestamp - p1.timestamp;
	if (duration < 0) duration += LOOP_DURATION; // Wrapping case

	const elapsed = t - p1.timestamp;
	let progress = duration > 0 ? elapsed / duration : 0;

	// Interpolate coords
	const lng1 = p1.coords[0];
	const lat1 = p1.coords[1];
	const alt1 = p1.coords[2] || 0;
	const lng2 = p2.coords[0];
	const lat2 = p2.coords[1];
	const alt2 = p2.coords[2] || 0;

	// Handle dateline crossing for interpolation
	let dLng = lng2 - lng1;
	if (dLng > 180) dLng -= 360;
	if (dLng < -180) dLng += 360;

	const curLng = lng1 + dLng * progress;
	const curLat = lat1 + (lat2 - lat1) * progress;
	const curAlt = alt1 + (alt2 - alt1) * progress;

	// Calculate heading
	const headingRad = Math.atan2(dLng, lat2 - lat1); // Simple approx
	let heading = (headingRad * 180) / Math.PI;
	if (heading < 0) heading += 360;

	return {
		coords: [curLat, curLng, curAlt], // Return [lat, lng, alt] for LiveLocationData
		heading
	};
}

// Major Hubs for global travel
const HUBS = {
	SF: [37.7749, -122.4194],
	NY: [40.7128, -74.0060],
	London: [51.5074, -0.1278],
	Tokyo: [35.6762, 139.6503],
	Sydney: [-33.8688, 151.2093],
	CapeTown: [-33.9249, 18.4241],
	Dubai: [25.2048, 55.2708],
	Singapore: [1.3521, 103.8198],
	Rio: [-22.9068, -43.1729],
	Paris: [48.8566, 2.3522]
};

const HUB_KEYS = Object.keys(HUBS) as (keyof typeof HUBS)[];

/**
 * Calculate point on Great Circle path
 * f: fraction 0..1
 */
function getGreatCircleWaypoint(p1: [number, number], p2: [number, number], f: number, maxAlt: number = 0): [number, number, number] {
	// Simple linear interpolation is visually acceptable for this scale if we don't want strict Haversine math overhead
	// But for "curved" lines on Mercator (Great Circles), we need a bit of math.
	// Let's use a simpler approximation: Interpolate linearly but add a "arc" to latitude to simulate the curve?
	// Actually, strict Great Circle math is best.

	const lat1 = p1[0] * Math.PI / 180;
	const lon1 = p1[1] * Math.PI / 180;
	const lat2 = p2[0] * Math.PI / 180;
	const lon2 = p2[1] * Math.PI / 180;

	const d = 2 * Math.asin(Math.sqrt(Math.pow(Math.sin((lat1 - lat2) / 2), 2) +
		Math.cos(lat1) * Math.cos(lat2) * Math.pow(Math.sin((lon1 - lon2) / 2), 2)));

	const A = Math.sin((1 - f) * d) / Math.sin(d);
	const B = Math.sin(f * d) / Math.sin(d);

	const x = A * Math.cos(lat1) * Math.cos(lon1) + B * Math.cos(lat2) * Math.cos(lon2);
	const y = A * Math.cos(lat1) * Math.sin(lon1) + B * Math.cos(lat2) * Math.sin(lon2);
	const z = A * Math.sin(lat1) + B * Math.sin(lat2);

	const lat = Math.atan2(z, Math.sqrt(x * x + y * y));
	const lon = Math.atan2(y, x);

	// Add altitude arc (parabola)
	// Peak altitude at f=0.5
	const alt = maxAlt * (1 - Math.pow(2 * f - 1, 2));

	return [lat * 180 / Math.PI, lon * 180 / Math.PI, alt];
}

/**
 * Generate a Global Round Trip (A -> B -> A)
 */
function generateGlobalTrip(
	startHub: keyof typeof HUBS,
	endHub: keyof typeof HUBS,
	transport: TransportConfig
): TripWaypoint[] {
	const p1 = HUBS[startHub] as [number, number];
	const p2 = HUBS[endHub] as [number, number];

	const waypoints: TripWaypoint[] = [];
	const LEG_SEGMENTS = 40;

	// Determine altitude based on mode
	let maxAlt = 0; // meters
	if (transport.mode === 'rocketing') maxAlt = 400000; // 400km
	else if (transport.mode === 'flying') maxAlt = 20000; // 20km (higher for visibility)
	else if (transport.mode === 'helicoptering') maxAlt = 2000; // 2km

	// For surface global travel (ships), lift slightly to avoid z-fighting
	if (transport.mode === 'sailing') maxAlt = 500; // 500m "hover"

	// Leg 1: A -> B (0 to 50% of time)
	for (let i = 0; i <= LEG_SEGMENTS; i++) {
		const f = i / LEG_SEGMENTS;
		// Use fixed altitude for visual clarity on globe, or arc?
		// Arc looks better for rockets/planes. Flat for ships?
		// Let's use arc for all long distance for now to ensure visibility
		const coords = getGreatCircleWaypoint(p1, p2, f, maxAlt);
		// Map to 0..LOOP_DURATION/2
		const t = (f * LOOP_DURATION) / 2;

		// DeckGL needs [lng, lat, alt]
		waypoints.push({ coords: [coords[1], coords[0], coords[2]], timestamp: t });
	}

	// Leg 2: B -> A (50% to 100% of time)
	for (let i = 0; i <= LEG_SEGMENTS; i++) {
		const f = i / LEG_SEGMENTS;
		const coords = getGreatCircleWaypoint(p2, p1, f, maxAlt);
		// Map to LOOP_DURATION/2 .. LOOP_DURATION
		const t = (LOOP_DURATION / 2) + (f * LOOP_DURATION) / 2;

		waypoints.push({ coords: [coords[1], coords[0], coords[2]], timestamp: t });
	}

	// Apply buffer logic for seamless looping
	const preBuffer = waypoints
		.filter(w => w.timestamp >= LOOP_DURATION - TRAIL_LENGTH)
		.map(w => ({ ...w, timestamp: w.timestamp - LOOP_DURATION }));

	const postBuffer = waypoints
		.filter(w => w.timestamp <= TRAIL_LENGTH)
		.map(w => ({ ...w, timestamp: w.timestamp + LOOP_DURATION }));

	return [...preBuffer, ...waypoints, ...postBuffer].sort((a, b) => a.timestamp - b.timestamp);
}

/**
 * Generate a local smooth loop path (for walkers/cars)
 */
function generateLocalLoop(
	center: [number, number],
	radius: number,
	transport: TransportConfig,
	segments: number = 20
): TripWaypoint[] {
	const waypoints: TripWaypoint[] = [];

	// Calculate total distance roughly
	// We want the vehicle to travel at roughly its maxSpeed (or valid portion of it)
	// speed (m/s) * 120s = Total Distance
	// 1 degree lat ~ 111km = 111,000m
	const speedDegPerSec = (transport.maxSpeedMps / 111000);

	// Generate a shape (ellipse/circle with noise)
	// Base radius depends on speed to ensure they complete the loop in 120s
	// Circumference = speed * time
	// 2 * PI * r = speed * 120
	// r = (speed * 120) / (2 * PI)
	const dynamicRadius = (speedDegPerSec * LOOP_DURATION) / (2 * Math.PI);

	// Use the larger of config radius vs dynamic radius?
	// Actually, strictly obey physics: dynamicRadius determines the size of the loop
	// to match the 120s duration

	const loopCenter = center;

	// Randomize shape slightly
	const aspect = 0.5 + Math.random(); // Ellipse ratio
	const tilt = Math.random() * Math.PI;

	// Base altitude to avoid clipping
	const baseAlt = 100; // 100m lift

	for (let i = 0; i <= segments; i++) {
		const fraction = i / segments;
		const angle = fraction * 2 * Math.PI;
		const t = fraction * LOOP_DURATION;

		// Ellipse logic
		const rx = dynamicRadius;
		const ry = dynamicRadius * aspect;

		// Rotate ellipse
		const x = rx * Math.cos(angle);
		const y = ry * Math.sin(angle);

		const rotX = x * Math.cos(tilt) - y * Math.sin(tilt);
		const rotY = x * Math.sin(tilt) + y * Math.cos(tilt);

		let lat = loopCenter[0] + rotY;
		let lng = loopCenter[1] + rotX; // simplified logic, ignores projection distortion at poles

		// Wrap
		if (lng > 180) lng -= 360;
		if (lng < -180) lng += 360;
		if (lat > 85) lat = 85;
		if (lat < -85) lat = -85;


		waypoints.push({
			coords: [lng, lat, baseAlt], // [lng, lat, alt] with lift
			timestamp: t
		});
	}

	// Add buffer for seamless looping
	// Pre-buffer: Copy end segment (timestamps > LOOP_DURATION - TRAIL_LENGTH) and shift to negative
	const preBuffer = waypoints
		.filter(w => w.timestamp >= LOOP_DURATION - TRAIL_LENGTH)
		.map(w => ({
			coords: w.coords,
			timestamp: w.timestamp - LOOP_DURATION
		}));

	// Post-buffer: Copy start segment (timestamps < TRAIL_LENGTH) and shift to > LOOP_DURATION
	const postBuffer = waypoints
		.filter(w => w.timestamp <= TRAIL_LENGTH)
		.map(w => ({
			coords: w.coords,
			timestamp: w.timestamp + LOOP_DURATION
		}));

	// Sort just in case, though filtering preserves order
	return [...preBuffer, ...waypoints, ...postBuffer].sort((a, b) => a.timestamp - b.timestamp);
}

interface SimParticipant {
	pubkey: string;
	trip: TripData;
}

/**
 * Simulator Configuration
 */
export interface SimulatorConfig {
	/** Number of participants to simulate */
	participantCount: number;
	/** Update interval in milliseconds */
	updateIntervalMs: number;
	/** Maximum movement per update (in degrees) */
	maxMovement: number;
	/** Center point for simulation [lat, lng] */
	center: [number, number];
	/** Radius around center (in degrees) */
	radius: number;
}

const DEFAULT_SIMULATOR_CONFIG: SimulatorConfig = {
	participantCount: 15, // Increased for more activity
	updateIntervalMs: 200, // Update every 200ms for smoothness
	maxMovement: 0.00005, // ~5 meters per update base speed
	center: [37.7749, -122.4194], // San Francisco
	radius: 0.05 // ~5km radius
};

/**
 * Start the location simulator
 */
export function startLocationSimulator(config: Partial<SimulatorConfig> = {}) {
	const cfg = { ...DEFAULT_SIMULATOR_CONFIG, ...config };

	// If radius is small (default), make it global
	if (cfg.radius <= 0.1) {
		cfg.radius = 200; // Trigger global mode
		cfg.maxMovement = 0.0001; // Slightly faster global base for visibility
	}

	console.log('[LOCATION-SIMULATOR] Starting with config:', cfg);

	// Initialize participant locations with random transport modes
	const participants: Record<string, SimParticipant> = {};
	const trips: TripData[] = [];

	for (let i = 0; i < cfg.participantCount; i++) {
		const pubkey = `sim_user_${i + 1}`;
		// Weighted random transport distribution
		const rand = Math.random();
		let transport: TransportConfig;

		if (rand > 0.95) transport = TRANSPORT_MODES[7]; // 5% Rocket
		else if (rand > 0.90) transport = TRANSPORT_MODES[6]; // 5% Helicopter
		else if (rand > 0.80) transport = TRANSPORT_MODES[5]; // 10% Plane
		else if (rand > 0.75) transport = TRANSPORT_MODES[8]; // 5% Ship
		else if (rand > 0.65) transport = TRANSPORT_MODES[4]; // 10% Train
		else if (rand > 0.50) transport = TRANSPORT_MODES[3]; // 15% Bus
		else if (rand > 0.40) transport = TRANSPORT_MODES[2]; // 10% Car
		else if (rand > 0.25) transport = TRANSPORT_MODES[1]; // 15% Bike
		else transport = TRANSPORT_MODES[0]; // 25% Walking


		// Determine which trip type to generate based on transport
		let waypoints: TripWaypoint[];

		// High speed transport = Global Trip
		const isLongDistance = ['flying', 'rocketing', 'sailing', 'helicoptering', 'training', 'bussing'].includes(transport.mode);

		if (isLongDistance) {
			// Pick two random distinct hubs
			const startIdx = Math.floor(Math.random() * HUB_KEYS.length);
			let endIdx = Math.floor(Math.random() * HUB_KEYS.length);
			while (endIdx === startIdx) {
				endIdx = Math.floor(Math.random() * HUB_KEYS.length);
			}

			waypoints = generateGlobalTrip(HUB_KEYS[startIdx], HUB_KEYS[endIdx], transport);
		} else {
			// Low speed = Local Loop around user center
			const userCenter = randomLocationNear(cfg.center, cfg.radius);
			waypoints = generateLocalLoop(userCenter, cfg.radius, transport);
		}

		// Define color based on transport
		let color: [number, number, number] = [255, 165, 0]; // Orange default
		if (transport.mode === 'rocketing') color = [255, 0, 0];
		if (transport.mode === 'flying') color = [0, 191, 255];
		if (transport.mode === 'helicoptering') color = [0, 255, 127];
		if (transport.mode === 'training') color = [148, 0, 211];
		if (transport.mode === 'sailing') color = [65, 105, 225];

		const trip: TripData = {
			id: pubkey,
			waypoints,
			color,
			transportMode: transport.mode
		};
		trips.push(trip);

		participants[pubkey] = {
			pubkey,
			trip
		};
	}

	// Update trips store once
	simulatedTrips.set(trips);

	// Update locations periodically
	function updateSimulatedLocations() {
		const updates: Record<string, LiveLocationData> = {};
		const now = Date.now();
		// Use modulo of LOOP_DURATION (in seconds)
		const t = (now / 1000) % LOOP_DURATION;

		for (const [pubkey, participant] of Object.entries(participants)) {
			// Move location along path
			const { coords, heading } = moveAlongPath(participant.trip.waypoints, t);

			// Find configured transport for this participant
			// (We need to look it up or store it better, but we have it in trip.transportMode)
			const mode = participant.trip.transportMode;
			// Find full config
			const transport = TRANSPORT_MODES.find(m => m.mode === mode) || TRANSPORT_MODES[0];

			// Create location data
			updates[pubkey] = {
				latitude: coords[0],
				longitude: coords[1],
				accuracy: 10,
				altitude: coords[2] ?? getAltitudeForMode(mode), // Use 3D altitude if available
				altitudeAccuracy: 10,
				heading: heading,
				speed: transport.maxSpeedMps,
				timestamp: now,
				emoji: transport.emoji
			};
		}

		networkLocations.set(updates);
	}

	// Initial update
	updateSimulatedLocations();

	// Start interval
	stopLocationSimulator(); // Clear any existing
	// Fast updates for smoothness (100ms) - let the UI interpolate if needed, 
	// but frequent updates look better than interpolation over long gaps for fast objects
	simulatorIntervalId = window.setInterval(updateSimulatedLocations, 200);
	isSimulatorActive.set(true);

	console.log('[LOCATION-SIMULATOR] ✅ Started');
}

/**
 * Stop the location simulator
 */
export function stopLocationSimulator() {
	if (simulatorIntervalId !== null) {
		window.clearInterval(simulatorIntervalId);
		simulatorIntervalId = null;
		isSimulatorActive.set(false);
		console.log('[LOCATION-SIMULATOR] ⏹ Stopped');
	}
}

/**
 * Get locations for specific users (reactive)
 */
export const getNetworkLocation = derived(
	networkLocations,
	($locations) => (pubkey: string) => $locations[pubkey] || null
);

/**
 * All network locations as array (for map rendering)
 */
export const networkLocationsArray: Readable<Array<LiveLocationData & { pubkey: string }>> = derived(
	networkLocations,
	($locations) => {
		return Object.entries($locations).map(([pubkey, location]) => ({
			...location,
			pubkey
		}));
	}
);

/**
 * Network locations in MapLibre format [[lng, lat], ...]
 */
export const networkLocationsLngLat: Readable<Array<{ pubkey: string; coords: [number, number] }>> = derived(
	networkLocations,
	($locations) => {
		return Object.entries($locations).map(([pubkey, location]) => ({
			pubkey,
			coords: [location.longitude, location.latitude] as [number, number]
		}));
	}
);
