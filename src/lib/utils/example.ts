// V5: Import from v5 stores and schemas
import type { RootNode, AvailabilitySlot, NeedSlot, AvailabilityWindow } from '../protocol/schemas';

import { getLocalTimeZone } from '@internationalized/date';
import { populateSDGTree } from '../templates/sdg';

/**
 * Populate tree with all 17 Sustainable Development Goals
 * This is now delegated to the SDG module
 */
export function populateWithExampleData(rootNode: RootNode): RootNode {
	console.log('[EXAMPLE] Populating tree with SDG structure...');
	return populateSDGTree(rootNode);
}

/**
 * South American cities with coordinates
 */
const SOUTH_AMERICA_CITIES = [
	// Brazil
	{ name: 'São Paulo', country: 'Brazil', lat: -23.5505, lng: -46.6333 },
	{ name: 'Rio de Janeiro', country: 'Brazil', lat: -22.9068, lng: -43.1729 },
	{ name: 'Brasília', country: 'Brazil', lat: -15.8267, lng: -47.9218 },
	{ name: 'Salvador', country: 'Brazil', lat: -12.9714, lng: -38.5014 },
	{ name: 'Fortaleza', country: 'Brazil', lat: -3.7172, lng: -38.5433 },
	{ name: 'Belo Horizonte', country: 'Brazil', lat: -19.9167, lng: -43.9345 },
	{ name: 'Manaus', country: 'Brazil', lat: -3.119, lng: -60.0217 },
	{ name: 'Curitiba', country: 'Brazil', lat: -25.4284, lng: -49.2733 },
	{ name: 'Recife', country: 'Brazil', lat: -8.0476, lng: -34.877 },
	{ name: 'Porto Alegre', country: 'Brazil', lat: -30.0346, lng: -51.2177 },

	// Argentina
	{ name: 'Buenos Aires', country: 'Argentina', lat: -34.6037, lng: -58.3816 },
	{ name: 'Córdoba', country: 'Argentina', lat: -31.4201, lng: -64.1888 },
	{ name: 'Rosario', country: 'Argentina', lat: -32.9442, lng: -60.6505 },
	{ name: 'Mendoza', country: 'Argentina', lat: -32.8895, lng: -68.8458 },
	{ name: 'La Plata', country: 'Argentina', lat: -34.9215, lng: -57.9545 },
	{ name: 'Mar del Plata', country: 'Argentina', lat: -38.0055, lng: -57.5426 },
	{ name: 'Salta', country: 'Argentina', lat: -24.7859, lng: -65.4117 },

	// Colombia
	{ name: 'Bogotá', country: 'Colombia', lat: 4.711, lng: -74.0721 },
	{ name: 'Medellín', country: 'Colombia', lat: 6.2442, lng: -75.5812 },
	{ name: 'Cali', country: 'Colombia', lat: 3.4516, lng: -76.532 },
	{ name: 'Barranquilla', country: 'Colombia', lat: 10.9685, lng: -74.7813 },
	{ name: 'Cartagena', country: 'Colombia', lat: 10.391, lng: -75.4794 },

	// Peru
	{ name: 'Lima', country: 'Peru', lat: -12.0464, lng: -77.0428 },
	{ name: 'Cusco', country: 'Peru', lat: -13.5319, lng: -71.9675 },
	{ name: 'Arequipa', country: 'Peru', lat: -16.409, lng: -71.5375 },
	{ name: 'Iquitos', country: 'Peru', lat: -3.7437, lng: -73.2516 },

	// Chile
	{ name: 'Santiago', country: 'Chile', lat: -33.4489, lng: -70.6693 },
	{ name: 'Valparaíso', country: 'Chile', lat: -33.0472, lng: -71.6127 },
	{ name: 'Concepción', country: 'Chile', lat: -36.8201, lng: -73.0444 },
	{ name: 'Antofagasta', country: 'Chile', lat: -23.6509, lng: -70.3975 },

	// Ecuador
	{ name: 'Quito', country: 'Ecuador', lat: -0.1807, lng: -78.4678 },
	{ name: 'Guayaquil', country: 'Ecuador', lat: -2.1894, lng: -79.8888 },
	{ name: 'Cuenca', country: 'Ecuador', lat: -2.9001, lng: -79.0059 },

	// Bolivia
	{ name: 'La Paz', country: 'Bolivia', lat: -16.5, lng: -68.15 },
	{ name: 'Santa Cruz', country: 'Bolivia', lat: -17.7863, lng: -63.1812 },
	{ name: 'Cochabamba', country: 'Bolivia', lat: -17.3895, lng: -66.1568 },

	// Venezuela
	{ name: 'Caracas', country: 'Venezuela', lat: 10.4806, lng: -66.9036 },
	{ name: 'Maracaibo', country: 'Venezuela', lat: 10.6666, lng: -71.6124 },
	{ name: 'Valencia', country: 'Venezuela', lat: 10.1621, lng: -68.0077 },

	// Paraguay
	{ name: 'Asunción', country: 'Paraguay', lat: -25.2637, lng: -57.5759 },
	{ name: 'Ciudad del Este', country: 'Paraguay', lat: -25.5095, lng: -54.6154 },

	// Uruguay
	{ name: 'Montevideo', country: 'Uruguay', lat: -34.9011, lng: -56.1645 },
	{ name: 'Punta del Este', country: 'Uruguay', lat: -34.9667, lng: -54.95 },

	// Guyana, Suriname, French Guiana
	{ name: 'Georgetown', country: 'Guyana', lat: 6.8013, lng: -58.1551 },
	{ name: 'Paramaribo', country: 'Suriname', lat: 5.852, lng: -55.2038 },
	{ name: 'Cayenne', country: 'French Guiana', lat: 4.9333, lng: -52.3333 }
];

/**
 * Global cities (for the remaining 50 capacities)
 */
const GLOBAL_CITIES = [
	// Africa
	{ name: 'Nairobi', country: 'Kenya', lat: -1.2921, lng: 36.8219 },
	{ name: 'Lagos', country: 'Nigeria', lat: 6.5244, lng: 3.3792 },
	{ name: 'Cairo', country: 'Egypt', lat: 30.0444, lng: 31.2357 },
	{ name: 'Johannesburg', country: 'South Africa', lat: -26.2041, lng: 28.0473 },
	{ name: 'Accra', country: 'Ghana', lat: 5.6037, lng: -0.187 },
	{ name: 'Addis Ababa', country: 'Ethiopia', lat: 9.032, lng: 38.7469 },
	{ name: 'Dar es Salaam', country: 'Tanzania', lat: -6.7924, lng: 39.2083 },
	{ name: 'Casablanca', country: 'Morocco', lat: 33.5731, lng: -7.5898 },

	// Asia
	{ name: 'Mumbai', country: 'India', lat: 19.076, lng: 72.8777 },
	{ name: 'Delhi', country: 'India', lat: 28.7041, lng: 77.1025 },
	{ name: 'Dhaka', country: 'Bangladesh', lat: 23.8103, lng: 90.4125 },
	{ name: 'Jakarta', country: 'Indonesia', lat: -6.2088, lng: 106.8456 },
	{ name: 'Manila', country: 'Philippines', lat: 14.5995, lng: 120.9842 },
	{ name: 'Bangkok', country: 'Thailand', lat: 13.7563, lng: 100.5018 },
	{ name: 'Ho Chi Minh City', country: 'Vietnam', lat: 10.8231, lng: 106.6297 },
	{ name: 'Kathmandu', country: 'Nepal', lat: 27.7172, lng: 85.324 },

	// Europe
	{ name: 'Berlin', country: 'Germany', lat: 52.52, lng: 13.405 },
	{ name: 'Paris', country: 'France', lat: 48.8566, lng: 2.3522 },
	{ name: 'London', country: 'United Kingdom', lat: 51.5074, lng: -0.1278 },
	{ name: 'Barcelona', country: 'Spain', lat: 41.3851, lng: 2.1734 },
	{ name: 'Amsterdam', country: 'Netherlands', lat: 52.3676, lng: 4.9041 },
	{ name: 'Copenhagen', country: 'Denmark', lat: 55.6761, lng: 12.5683 },

	// North America
	{ name: 'Mexico City', country: 'Mexico', lat: 19.4326, lng: -99.1332 },
	{ name: 'Oaxaca', country: 'Mexico', lat: 17.0732, lng: -96.7266 },
	{ name: 'Guatemala City', country: 'Guatemala', lat: 14.6349, lng: -90.5069 },
	{ name: 'San José', country: 'Costa Rica', lat: 9.9281, lng: -84.0907 },
	{ name: 'Panama City', country: 'Panama', lat: 8.9824, lng: -79.5199 },
	{ name: 'Havana', country: 'Cuba', lat: 23.1136, lng: -82.3666 },

	// Oceania
	{ name: 'Sydney', country: 'Australia', lat: -33.8688, lng: 151.2093 },
	{ name: 'Melbourne', country: 'Australia', lat: -37.8136, lng: 144.9631 },
	{ name: 'Auckland', country: 'New Zealand', lat: -36.8485, lng: 174.7633 },

	// Middle East
	{ name: 'Amman', country: 'Jordan', lat: 31.9454, lng: 35.9284 },
	{ name: 'Beirut', country: 'Lebanon', lat: 33.8886, lng: 35.4955 },
	{ name: 'Istanbul', country: 'Turkey', lat: 41.0082, lng: 28.9784 }
];

/**
 * Expanded list of global cities (Africa, Asia, NA, Europe, Middle East, Oceania)
 * to ensure map is densely populated.
 */
const MORE_GLOBAL_CITIES = [
	// === AFRICA ===
	// West Africa
	{ name: 'Abuja', country: 'Nigeria', lat: 9.0765, lng: 7.3986 },
	{ name: 'Kumasi', country: 'Ghana', lat: 6.6885, lng: -1.6244 }, // Accra is in GLOBAL
	{ name: 'Dakar', country: 'Senegal', lat: 14.7167, lng: -17.4677 },
	{ name: 'Abidjan', country: 'Ivory Coast', lat: 5.3600, lng: -4.0083 },
	// East Africa
	{ name: 'Mombasa', country: 'Kenya', lat: -4.0435, lng: 39.6682 }, // Nairobi is in GLOBAL
	{ name: 'Zanzibar City', country: 'Tanzania', lat: -6.1659, lng: 39.2026 },
	{ name: 'Kampala', country: 'Uganda', lat: 0.3476, lng: 32.5825 },
	{ name: 'Kigali', country: 'Rwanda', lat: -1.9441, lng: 30.0619 },
	// North Africa
	{ name: 'Alexandria', country: 'Egypt', lat: 31.2001, lng: 29.9187 }, // Cairo is in GLOBAL
	{ name: 'Marrakech', country: 'Morocco', lat: 31.6295, lng: -7.9811 },
	{ name: 'Tangier', country: 'Morocco', lat: 35.7595, lng: -5.8340 },
	{ name: 'Tunis', country: 'Tunisia', lat: 36.8065, lng: 10.1815 },
	// South Africa
	{ name: 'Cape Town', country: 'South Africa', lat: -33.9249, lng: 18.4241 }, // JHB is in GLOBAL
	{ name: 'Durban', country: 'South Africa', lat: -29.8587, lng: 31.0218 },
	{ name: 'Harare', country: 'Zimbabwe', lat: -17.8216, lng: 31.0492 },

	// === ASIA ===
	// East Asia (China)
	{ name: 'Beijing', country: 'China', lat: 39.9042, lng: 116.4074 },
	{ name: 'Shanghai', country: 'China', lat: 31.2304, lng: 121.4737 },
	{ name: 'Shenzhen', country: 'China', lat: 22.5431, lng: 114.0579 },
	{ name: 'Chengdu', country: 'China', lat: 30.5728, lng: 104.0668 },
	{ name: 'Xi\'an', country: 'China', lat: 34.3416, lng: 108.9398 },
	// East Asia (Japan/Korea/Taiwan)
	{ name: 'Tokyo', country: 'Japan', lat: 35.6762, lng: 139.6503 },
	{ name: 'Osaka', country: 'Japan', lat: 34.6937, lng: 135.5023 },
	{ name: 'Sapporo', country: 'Japan', lat: 43.0618, lng: 141.3545 },
	{ name: 'Fukuoka', country: 'Japan', lat: 33.5902, lng: 130.4017 },
	{ name: 'Seoul', country: 'South Korea', lat: 37.5665, lng: 126.9780 },
	{ name: 'Busan', country: 'South Korea', lat: 35.1796, lng: 129.0756 },
	{ name: 'Taipei', country: 'Taiwan', lat: 25.0330, lng: 121.5654 },
	// South Asia
	{ name: 'Bangalore', country: 'India', lat: 12.9716, lng: 77.5946 }, // Mumbai/Delhi in GLOBAL
	{ name: 'Chennai', country: 'India', lat: 13.0827, lng: 80.2707 },
	{ name: 'Kolkata', country: 'India', lat: 22.5726, lng: 88.3639 },
	{ name: 'Hyderabad', country: 'India', lat: 17.3850, lng: 78.4867 },
	{ name: 'Jaipur', country: 'India', lat: 26.9124, lng: 75.7873 },
	{ name: 'Karachi', country: 'Pakistan', lat: 24.8607, lng: 67.0011 },
	{ name: 'Lahore', country: 'Pakistan', lat: 31.5204, lng: 74.3587 },
	{ name: 'Chittagong', country: 'Bangladesh', lat: 22.3569, lng: 91.7832 },
	{ name: 'Colombo', country: 'Sri Lanka', lat: 6.9271, lng: 79.8612 },
	// Southeast Asia
	{ name: 'Denpasar', country: 'Indonesia', lat: -8.6705, lng: 115.2126 }, // Bali
	{ name: 'Surabaya', country: 'Indonesia', lat: -7.2575, lng: 112.7521 },
	{ name: 'Hanoi', country: 'Vietnam', lat: 21.0285, lng: 105.8542 },
	{ name: 'Da Nang', country: 'Vietnam', lat: 16.0544, lng: 108.2022 },
	{ name: 'Chiang Mai', country: 'Thailand', lat: 18.7061, lng: 98.9817 },
	{ name: 'Cebu City', country: 'Philippines', lat: 10.3157, lng: 123.8854 },
	{ name: 'Davao City', country: 'Philippines', lat: 7.1907, lng: 125.4553 },
	{ name: 'Kuala Lumpur', country: 'Malaysia', lat: 3.1390, lng: 101.6869 },
	{ name: 'Penang', country: 'Malaysia', lat: 5.4141, lng: 100.3119 },
	{ name: 'Singapore', country: 'Singapore', lat: 1.3521, lng: 103.8198 },
	// Central Asia
	{ name: 'Almaty', country: 'Kazakhstan', lat: 43.2220, lng: 76.8512 },
	{ name: 'Tashkent', country: 'Uzbekistan', lat: 41.2995, lng: 69.2401 },

	// === MIDDLE EAST ===
	{ name: 'Dubai', country: 'UAE', lat: 25.2048, lng: 55.2708 },
	{ name: 'Abu Dhabi', country: 'UAE', lat: 24.4539, lng: 54.3773 },
	{ name: 'Riyadh', country: 'Saudi Arabia', lat: 24.7136, lng: 46.6753 },
	{ name: 'Jeddah', country: 'Saudi Arabia', lat: 21.5433, lng: 39.1979 },
	{ name: 'Tel Aviv', country: 'Israel', lat: 32.0853, lng: 34.7818 },
	{ name: 'Tehran', country: 'Iran', lat: 35.6892, lng: 51.3890 },
	{ name: 'Doha', country: 'Qatar', lat: 25.2854, lng: 51.5310 },

	// === NORTH AMERICA ===
	// USA
	{ name: 'New York City', country: 'USA', lat: 40.7128, lng: -74.0060 },
	{ name: 'Los Angeles', country: 'USA', lat: 34.0522, lng: -118.2437 },
	{ name: 'Chicago', country: 'USA', lat: 41.8781, lng: -87.6298 },
	{ name: 'Houston', country: 'USA', lat: 29.7604, lng: -95.3698 },
	{ name: 'Phoenix', country: 'USA', lat: 33.4484, lng: -112.0740 },
	{ name: 'Philadelphia', country: 'USA', lat: 39.9526, lng: -75.1652 },
	{ name: 'San Antonio', country: 'USA', lat: 29.4241, lng: -98.4936 },
	{ name: 'San Diego', country: 'USA', lat: 32.7157, lng: -117.1611 },
	{ name: 'Dallas', country: 'USA', lat: 32.7767, lng: -96.7970 },
	{ name: 'San Jose', country: 'USA', lat: 37.3382, lng: -121.8863 },
	{ name: 'Austin', country: 'USA', lat: 30.2672, lng: -97.7431 },
	{ name: 'Seattle', country: 'USA', lat: 47.6062, lng: -122.3321 },
	{ name: 'Denver', country: 'USA', lat: 39.7392, lng: -104.9903 },
	{ name: 'Boston', country: 'USA', lat: 42.3601, lng: -71.0589 },
	{ name: 'Miami', country: 'USA', lat: 25.7617, lng: -80.1918 },
	// Canada
	{ name: 'Toronto', country: 'Canada', lat: 43.6532, lng: -79.3832 },
	{ name: 'Montreal', country: 'Canada', lat: 45.5017, lng: -73.5673 },
	{ name: 'Vancouver', country: 'Canada', lat: 49.2827, lng: -123.1207 },
	{ name: 'Calgary', country: 'Canada', lat: 51.0447, lng: -114.0719 },
	{ name: 'Ottawa', country: 'Canada', lat: 45.4215, lng: -75.6972 },

	// === EUROPE & RUSSIA ===
	{ name: 'Moscow', country: 'Russia', lat: 55.7558, lng: 37.6173 },
	{ name: 'Saint Petersburg', country: 'Russia', lat: 59.9343, lng: 30.3351 },
	{ name: 'Warsaw', country: 'Poland', lat: 52.2297, lng: 21.0122 },
	{ name: 'Krakow', country: 'Poland', lat: 50.0647, lng: 19.9450 },
	{ name: 'Prague', country: 'Czechia', lat: 50.0755, lng: 14.4378 },
	{ name: 'Budapest', country: 'Hungary', lat: 47.4979, lng: 19.0402 },
	{ name: 'Bucharest', country: 'Romania', lat: 44.4268, lng: 26.1025 },
	{ name: 'Stockholm', country: 'Sweden', lat: 59.3293, lng: 18.0686 },
	{ name: 'Oslo', country: 'Norway', lat: 59.9139, lng: 10.7522 },
	{ name: 'Helsinki', country: 'Finland', lat: 60.1699, lng: 24.9384 },
	{ name: 'Athens', country: 'Greece', lat: 37.9838, lng: 23.7275 },
	{ name: 'Lisbon', country: 'Portugal', lat: 38.7223, lng: -9.1393 },
	{ name: 'Porto', country: 'Portugal', lat: 41.1579, lng: -8.6291 },

	// === OCEANIA ===
	{ name: 'Wellington', country: 'New Zealand', lat: -41.2865, lng: 174.7762 },
	{ name: 'Christchurch', country: 'New Zealand', lat: -43.5321, lng: 172.6362 },
	{ name: 'Suva', country: 'Fiji', lat: -18.1416, lng: 178.4419 }
];

/**
 * Shared helper to add random jitter to coordinates (±0.15 degrees ≈ 15-20km)
 * Prevents map markers from stacking perfectly.
 */
const jitterLocation = (city: { lat: number; lng: number, name: string, country: string }) => {
	const JITTER_RANGE = 0.3; 
	return {
		...city,
		lat: city.lat + (Math.random() - 0.5) * JITTER_RANGE,
		lng: city.lng + (Math.random() - 0.5) * JITTER_RANGE
	};
};

/**
 * V5: 100 SDG-focused capacity slots with realistic locations and time patterns
 * Returns AvailabilitySlot[] (v5 schema)
 */
export function createExampleCapacitySlots(): AvailabilitySlot[] {
	const slots: AvailabilitySlot[] = [];
	const timezone = getLocalTimeZone();

	// Helper to create a capacity slot (v5 schema)
	const createCapacitySlot = (
		name: string,
		emoji: string,
		unit: string,
		quantity: number,
		city: { name: string; country: string; lat: number; lng: number },
		locationType: string = 'Specific',
		timePattern: any = null
	): AvailabilitySlot => {
		const time = timePattern || {
			allDay: true,
			recurrence: 'weekly',
			startTime: null,
			endTime: null
		};

		// V5: Use availability_window for time ranges if not all-day
		const availability_window = (time.allDay || !time.startTime || !time.endTime) ? undefined : {
			time_ranges: [{
				start_time: time.startTime,
				end_time: time.endTime
			}]
		};

		return {
			id: crypto.randomUUID(),
			name,
			emoji,
			unit,
			description: '',
			quantity,
			// V5 REQUIRED: type_id for multi-dimensional allocation
			type_id: 'general', // Default need type
			max_natural_div: Math.min(quantity, 10),
			min_allocation_percentage: 0.8,
			hidden_until_request_accepted: false,
			filter_rule: null,
			location_type: locationType,
			latitude: city.lat,
			longitude: city.lng,
			city: city.name,
			country: city.country,
			start_date: new Date().toISOString().split('T')[0],
			end_date: null,
			time_zone: timezone,
			recurrence: (time.recurrence === 'Weekends' ? 'weekly' : (time.recurrence || 'weekly')).toLowerCase() as any,
			// V5: Use availability_window instead of start_time/end_time
			availability_window
		};
	};

	// ============ SOUTH AMERICA CAPACITIES (50+) ============

	// Brazil (15 capacities)
	slots.push(
		createCapacitySlot('Community Lunch', '🍲', 'meals', 120, SOUTH_AMERICA_CITIES[0], 'Specific', {
			allDay: false,
			recurrence: 'daily',
			startTime: '12:00',
			endTime: '14:00'
		})
	);
	slots.push(
		createCapacitySlot('Water Filtration', '💧', 'liters', 5000, SOUTH_AMERICA_CITIES[1], 'Specific')
	);
	slots.push(
		createCapacitySlot(
			'Solar Panel Installation',
			'☀️',
			'panels',
			25,
			SOUTH_AMERICA_CITIES[2],
			'Specific'
		)
	);
	slots.push(
		createCapacitySlot('Literacy Classes', '📚', 'students', 30, SOUTH_AMERICA_CITIES[3], 'Specific', {
			allDay: false,
			recurrence: 'weekly',
			startTime: '09:00',
			endTime: '12:00'
		})
	);
	slots.push(
		createCapacitySlot(
			'Medical Consultation',
			'🏥',
			'appointments',
			40,
			SOUTH_AMERICA_CITIES[4],
			'Specific',
			{ allDay: false, recurrence: 'weekly', startTime: '08:00', endTime: '17:00' }
		)
	);
	slots.push(
		createCapacitySlot('Tree Planting', '🌳', 'saplings', 500, SOUTH_AMERICA_CITIES[5], 'Specific')
	);
	slots.push(
		createCapacitySlot('Waste Collection', '♻️', 'kg', 2000, SOUTH_AMERICA_CITIES[6], 'Specific')
	);
	slots.push(
		createCapacitySlot(
			'Fishing Training',
			'🐟',
			'participants',
			15,
			SOUTH_AMERICA_CITIES[7],
			'Specific'
		)
	);
	slots.push(
		createCapacitySlot('Coding Bootcamp', '💻', 'students', 25, SOUTH_AMERICA_CITIES[8], 'Specific')
	);
	slots.push(
		createCapacitySlot('Microfinance Loans', '💰', 'USD', 10000, SOUTH_AMERICA_CITIES[9], 'Specific')
	);
	slots.push(
		createCapacitySlot('Urban Garden', '🥬', 'plots', 50, SOUTH_AMERICA_CITIES[0], 'Specific')
	);
	slots.push(
		createCapacitySlot(
			'Women Empowerment',
			'👩',
			'workshops',
			8,
			SOUTH_AMERICA_CITIES[1],
			'Specific',
			{ allDay: false, recurrence: 'weekly', startTime: '15:00', endTime: '18:00' }
		)
	);
	slots.push(
		createCapacitySlot(
			'Clean Energy Access',
			'⚡',
			'households',
			100,
			SOUTH_AMERICA_CITIES[2],
			'Specific'
		)
	);
	slots.push(
		createCapacitySlot('Child Care', '👶', 'children', 20, SOUTH_AMERICA_CITIES[3], 'Specific', {
			allDay: false,
			recurrence: 'weekly',
			startTime: '07:00',
			endTime: '18:00'
		})
	);
	slots.push(
		createCapacitySlot('Mental Health Support', '🧠', 'sessions', 30, SOUTH_AMERICA_CITIES[4], 'Specific')
	);

	// Argentina (10 capacities)
	slots.push(
		createCapacitySlot(
			'Cooperative Bakery',
			'🍞',
			'loaves',
			200,
			SOUTH_AMERICA_CITIES[10],
			'Specific',
			{ allDay: false, recurrence: 'Daily', startTime: '06:00', endTime: '14:00' }
		)
	);
	slots.push(
		createCapacitySlot('Legal Aid', '⚖️', 'consultations', 15, SOUTH_AMERICA_CITIES[11], 'Specific')
	);
	slots.push(
		createCapacitySlot(
			'Agricultural Training',
			'🚜',
			'farmers',
			40,
			SOUTH_AMERICA_CITIES[12],
			'Specific'
		)
	);
	slots.push(
		createCapacitySlot('Bike Sharing', '🚲', 'bikes', 80, SOUTH_AMERICA_CITIES[13], 'Specific')
	);
	slots.push(
		createCapacitySlot('Youth Sports', '⚽', 'participants', 60, SOUTH_AMERICA_CITIES[14], 'Specific')
	);
	slots.push(
		createCapacitySlot(
			'Artisan Market',
			'🎨',
			'vendor spots',
			30,
			SOUTH_AMERICA_CITIES[15],
			'Specific',
			{ allDay: false, recurrence: 'Weekends', startTime: '10:00', endTime: '18:00' }
		)
	);
	slots.push(
		createCapacitySlot('Housing Renovation', '🏠', 'homes', 5, SOUTH_AMERICA_CITIES[16], 'Specific')
	);
	slots.push(
		createCapacitySlot(
			'Community Radio',
			'📻',
			'broadcast hours',
			168,
			SOUTH_AMERICA_CITIES[10],
			'Online'
		)
	);
	slots.push(
		createCapacitySlot('Textile Recycling', '👕', 'kg', 1000, SOUTH_AMERICA_CITIES[11], 'Specific')
	);
	slots.push(
		createCapacitySlot('Elder Care', '👴', 'seniors', 25, SOUTH_AMERICA_CITIES[12], 'Specific')
	);

	// Colombia (8 capacities)
	slots.push(
		createCapacitySlot('Peace Mediation', '🕊️', 'sessions', 20, SOUTH_AMERICA_CITIES[17], 'Specific')
	);
	slots.push(
		createCapacitySlot('Coffee Cooperative', '☕', 'kg', 500, SOUTH_AMERICA_CITIES[18], 'Specific')
	);
	slots.push(
		createCapacitySlot('Music Education', '🎵', 'students', 35, SOUTH_AMERICA_CITIES[19], 'Specific')
	);
	slots.push(
		createCapacitySlot('River Cleanup', '🌊', 'kg removed', 3000, SOUTH_AMERICA_CITIES[20], 'Specific')
	);
	slots.push(
		createCapacitySlot('Ecotourism', '🌴', 'visitors', 40, SOUTH_AMERICA_CITIES[21], 'Specific')
	);
	slots.push(
		createCapacitySlot('Digital Library', '📱', 'devices', 50, SOUTH_AMERICA_CITIES[17], 'Specific')
	);
	slots.push(
		createCapacitySlot(
			'Organic Market',
			'🥑',
			'vendor spots',
			25,
			SOUTH_AMERICA_CITIES[18],
			'Specific',
			{ allDay: false, recurrence: 'Weekly', startTime: '08:00', endTime: '14:00' }
		)
	);
	slots.push(
		createCapacitySlot(
			'Craft Workshop',
			'🧶',
			'participants',
			15,
			SOUTH_AMERICA_CITIES[19],
			'Specific'
		)
	);

	// Peru (6 capacities)
	slots.push(
		createCapacitySlot('Quinoa Farming', '🌾', 'hectares', 20, SOUTH_AMERICA_CITIES[23], 'Specific')
	);
	slots.push(
		createCapacitySlot('Tourism Guiding', '🗺️', 'tours', 10, SOUTH_AMERICA_CITIES[24], 'Specific', {
			allDay: true,
			recurrence: 'daily',
			startTime: null,
			endTime: null
		})
	);
	slots.push(
		createCapacitySlot(
			'Weaving Collective',
			'🧵',
			'textiles',
			100,
			SOUTH_AMERICA_CITIES[25],
			'Specific'
		)
	);
	slots.push(
		createCapacitySlot(
			'Water Infrastructure',
			'🚰',
			'connections',
			50,
			SOUTH_AMERICA_CITIES[23],
			'Specific'
		)
	);
	slots.push(
		createCapacitySlot(
			'Indigenous Knowledge',
			'📜',
			'workshops',
			12,
			SOUTH_AMERICA_CITIES[24],
			'Specific'
		)
	);
	slots.push(
		createCapacitySlot(
			'Rainforest Conservation',
			'🌲',
			'hectares',
			1000,
			SOUTH_AMERICA_CITIES[25],
			'Specific'
		)
	);

	// Chile (5 capacities)
	slots.push(
		createCapacitySlot(
			'Earthquake Preparedness',
			'🏗️',
			'trainings',
			20,
			SOUTH_AMERICA_CITIES[27],
			'Specific'
		)
	);
	slots.push(
		createCapacitySlot('Marine Research', '🐋', 'expeditions', 8, SOUTH_AMERICA_CITIES[28], 'Specific')
	);
	slots.push(
		createCapacitySlot('Wine Cooperative', '🍷', 'bottles', 2000, SOUTH_AMERICA_CITIES[29], 'Specific')
	);
	slots.push(
		createCapacitySlot(
			'Tech Innovation Hub',
			'🚀',
			'workspaces',
			30,
			SOUTH_AMERICA_CITIES[27],
			'Specific'
		)
	);
	slots.push(
		createCapacitySlot(
			'Mountain Conservation',
			'⛰️',
			'hectares',
			500,
			SOUTH_AMERICA_CITIES[30],
			'Specific'
		)
	);

	// Ecuador (4 capacities)
	slots.push(
		createCapacitySlot('Cacao Production', '🍫', 'kg', 800, SOUTH_AMERICA_CITIES[31], 'Specific')
	);
	slots.push(
		createCapacitySlot(
			'Biodiversity Study',
			'🦜',
			'researchers',
			12,
			SOUTH_AMERICA_CITIES[32],
			'Specific'
		)
	);
	slots.push(
		createCapacitySlot(
			'Indigenous Medicine',
			'🌿',
			'treatments',
			40,
			SOUTH_AMERICA_CITIES[33],
			'Specific'
		)
	);
	slots.push(
		createCapacitySlot('Volcano Monitoring', '🌋', 'sensors', 15, SOUTH_AMERICA_CITIES[31], 'Specific')
	);

	// Bolivia (3 capacities)
	slots.push(
		createCapacitySlot('Llama Wool Collective', '🦙', 'kg', 300, SOUTH_AMERICA_CITIES[34], 'Specific')
	);
	slots.push(
		createCapacitySlot('Salt Flat Tours', '✨', 'tours', 15, SOUTH_AMERICA_CITIES[35], 'Specific')
	);
	slots.push(
		createCapacitySlot(
			'Mining Alternative',
			'💎',
			'participants',
			25,
			SOUTH_AMERICA_CITIES[36],
			'Specific'
		)
	);

	// Venezuela (2 capacities)
	slots.push(
		createCapacitySlot('Food Distribution', '🥫', 'kg', 5000, SOUTH_AMERICA_CITIES[37], 'Specific')
	);
	slots.push(
		createCapacitySlot('Community Currency', '🪙', 'credits', 10000, SOUTH_AMERICA_CITIES[38], 'Online')
	);

	// Rest of South America (3 capacities)
	slots.push(
		createCapacitySlot(
			'Cross-Border Trade',
			'🤝',
			'transactions',
			50,
			SOUTH_AMERICA_CITIES[40],
			'Specific'
		)
	);
	slots.push(
		createCapacitySlot('Beach Cleanup', '🏖️', 'kg removed', 2000, SOUTH_AMERICA_CITIES[43], 'Specific')
	);
	slots.push(
		createCapacitySlot(
			'Indigenous Radio',
			'📡',
			'broadcast hours',
			120,
			SOUTH_AMERICA_CITIES[45],
			'Online'
		)
	);

	// ============ GLOBAL CAPACITIES (50) ============

	// Africa (15 capacities)
	slots.push(
		createCapacitySlot('Mobile Banking', '📱', 'accounts', 1000, GLOBAL_CITIES[0], 'Online')
	);
	slots.push(
		createCapacitySlot('Solar Kiosk', '☀️', 'charging stations', 10, GLOBAL_CITIES[1], 'Specific')
	);
	slots.push(
		createCapacitySlot('Maternal Health', '🤰', 'checkups', 50, GLOBAL_CITIES[2], 'Specific')
	);
	slots.push(
		createCapacitySlot('Drought Relief', '💧', 'liters', 10000, GLOBAL_CITIES[3], 'Specific')
	);
	slots.push(
		createCapacitySlot('School Meals', '🍽️', 'meals', 500, GLOBAL_CITIES[4], 'Specific', {
			allDay: false,
			recurrence: 'weekly',
			startTime: '12:00',
			endTime: '13:00'
		})
	);
	slots.push(
		createCapacitySlot('Livestock Vaccination', '🐄', 'animals', 200, GLOBAL_CITIES[5], 'Specific')
	);
	slots.push(
		createCapacitySlot('Malaria Prevention', '🦟', 'bed nets', 1000, GLOBAL_CITIES[6], 'Specific')
	);
	slots.push(
		createCapacitySlot('Women Literacy', '✍️', 'students', 40, GLOBAL_CITIES[7], 'Specific')
	);
	slots.push(
		createCapacitySlot('Desert Greening', '🌱', 'hectares', 100, GLOBAL_CITIES[0], 'Specific')
	);
	slots.push(
		createCapacitySlot('Handicraft Export', '🏺', 'products', 500, GLOBAL_CITIES[1], 'Online')
	);
	slots.push(
		createCapacitySlot('Clean Cookstoves', '🔥', 'stoves', 300, GLOBAL_CITIES[2], 'Specific')
	);
	slots.push(
		createCapacitySlot('Elephant Conservation', '🐘', 'hectares', 5000, GLOBAL_CITIES[3], 'Specific')
	);
	slots.push(
		createCapacitySlot('Fishing Rights', '🎣', 'licenses', 100, GLOBAL_CITIES[4], 'Specific')
	);
	slots.push(createCapacitySlot('Mobile Clinics', '🚑', 'visits', 30, GLOBAL_CITIES[5], 'Specific'));
	slots.push(
		createCapacitySlot('Storytelling Circle', '📖', 'sessions', 12, GLOBAL_CITIES[6], 'Specific')
	);

	// Asia (15 capacities)
	slots.push(
		createCapacitySlot('Monsoon Preparedness', '🌧️', 'households', 200, GLOBAL_CITIES[8], 'Specific')
	);
	slots.push(
		createCapacitySlot('Street Vendor Support', '🛒', 'vendors', 80, GLOBAL_CITIES[9], 'Specific')
	);
	slots.push(
		createCapacitySlot('Textile Worker Rights', '👗', 'workers', 150, GLOBAL_CITIES[10], 'Specific')
	);
	slots.push(
		createCapacitySlot('Mangrove Restoration', '🌊', 'hectares', 50, GLOBAL_CITIES[11], 'Specific')
	);
	slots.push(
		createCapacitySlot('Typhoon Shelter', '🏠', 'people', 300, GLOBAL_CITIES[12], 'Specific')
	);
	slots.push(
		createCapacitySlot('Rice Cooperative', '🍚', 'kg', 5000, GLOBAL_CITIES[13], 'Specific')
	);
	slots.push(
		createCapacitySlot('Coral Reef Protection', '🪸', 'hectares', 20, GLOBAL_CITIES[14], 'Specific')
	);
	slots.push(
		createCapacitySlot('Mountain Trails', '🥾', 'trails maintained', 10, GLOBAL_CITIES[15], 'Specific')
	);
	slots.push(
		createCapacitySlot('Tea Garden Collective', '🍵', 'kg', 1000, GLOBAL_CITIES[8], 'Specific')
	);
	slots.push(
		createCapacitySlot('Flood Warning System', '📢', 'villages', 50, GLOBAL_CITIES[9], 'Specific')
	);
	slots.push(
		createCapacitySlot('Spice Market', '🌶️', 'vendor spots', 40, GLOBAL_CITIES[10], 'Specific')
	);
	slots.push(
		createCapacitySlot('Tiger Conservation', '🐅', 'hectares', 10000, GLOBAL_CITIES[11], 'Specific')
	);
	slots.push(
		createCapacitySlot('Coconut Processing', '🥥', 'kg', 2000, GLOBAL_CITIES[12], 'Specific')
	);
	slots.push(
		createCapacitySlot('Meditation Center', '🧘', 'sessions', 20, GLOBAL_CITIES[13], 'Specific')
	);
	slots.push(
		createCapacitySlot('Earthquake Recovery', '🏗️', 'buildings', 15, GLOBAL_CITIES[14], 'Specific')
	);

	// Europe (8 capacities)
	slots.push(
		createCapacitySlot('Refugee Integration', '🤝', 'participants', 50, GLOBAL_CITIES[16], 'Specific')
	);
	slots.push(
		createCapacitySlot('Zero Waste Workshop', '♻️', 'participants', 30, GLOBAL_CITIES[17], 'Specific')
	);
	slots.push(
		createCapacitySlot('Community Garden', '🌻', 'plots', 40, GLOBAL_CITIES[18], 'Specific')
	);
	slots.push(
		createCapacitySlot('Bike Repair Collective', '🔧', 'repairs', 100, GLOBAL_CITIES[19], 'Specific')
	);
	slots.push(
		createCapacitySlot('Climate Strike', '📢', 'participants', 5000, GLOBAL_CITIES[20], 'Specific')
	);
	slots.push(
		createCapacitySlot('Cooperative Housing', '🏘️', 'units', 20, GLOBAL_CITIES[21], 'Specific')
	);
	slots.push(
		createCapacitySlot('Language Exchange', '🗣️', 'sessions', 25, GLOBAL_CITIES[16], 'Specific')
	);
	slots.push(
		createCapacitySlot('Permaculture Design', '🌿', 'projects', 12, GLOBAL_CITIES[17], 'Specific')
	);

	// Central America & Caribbean (6 capacities)
	slots.push(
		createCapacitySlot('Hurricane Relief', '🌀', 'families', 100, GLOBAL_CITIES[22], 'Specific')
	);
	slots.push(
		createCapacitySlot('Indigenous Crafts', '🎭', 'artisans', 30, GLOBAL_CITIES[23], 'Specific')
	);
	slots.push(
		createCapacitySlot('Coral Restoration', '🪸', 'coral pieces', 1000, GLOBAL_CITIES[24], 'Specific')
	);
	slots.push(
		createCapacitySlot('Agroforestry', '🌳', 'hectares', 80, GLOBAL_CITIES[25], 'Specific')
	);
	slots.push(
		createCapacitySlot('Rainwater Harvesting', '💧', 'systems', 40, GLOBAL_CITIES[26], 'Specific')
	);
	slots.push(
		createCapacitySlot('Traditional Dance', '💃', 'classes', 20, GLOBAL_CITIES[27], 'Specific')
	);

	// Oceania (3 capacities)
	slots.push(
		createCapacitySlot('Aboriginal Art', '🎨', 'workshops', 15, GLOBAL_CITIES[28], 'Specific')
	);
	slots.push(
		createCapacitySlot('Bushfire Recovery', '🔥', 'hectares', 500, GLOBAL_CITIES[29], 'Specific')
	);
	slots.push(
		createCapacitySlot('Māori Language', '📚', 'students', 25, GLOBAL_CITIES[30], 'Specific')
	);

	// Middle East (3 capacities)
	slots.push(
		createCapacitySlot('Historic Preservation', '🏛️', 'sites', 5, GLOBAL_CITIES[33], 'Specific')
	);

	// ============ EXPANDED GLOBAL CAPACITIES (Generated) ============
	// Automatically generate 2-3 slots for each new city to densely populate the map
	const CAPACITY_TEMPLATES = [
		// Technology & Digital
		{ name: 'Tech Mentorship', emoji: '💻', unit: 'hours', q: 40 },
		{ name: 'PCB Design', emoji: '🔌', unit: 'boards', q: 10 },
		{ name: 'Drone Repair', emoji: '🚁', unit: 'repairs', q: 5 },
		{ name: '3D Printing Lab', emoji: '🖨️', unit: 'hours', q: 100 },
		{ name: 'Cyber Security', emoji: '🔐', unit: 'audits', q: 3 },
		{ name: 'Data Science', emoji: '📊', unit: 'reports', q: 8 },
		{ name: 'App Testing', emoji: '📱', unit: 'devices', q: 50 },
		{ name: 'Game Design', emoji: '🎮', unit: 'workshops', q: 12 },
		{ name: 'Cloud Config', emoji: '☁️', unit: 'setups', q: 20 },
		{ name: 'AI Model Training', emoji: '🤖', unit: 'models', q: 5 },

		// Arts & Culture
		{ name: 'Creative Writing', emoji: '✍️', unit: 'workshops', q: 8 },
		{ name: 'Music Production', emoji: '🎵', unit: 'tracks', q: 5 },
		{ name: 'Mural Painting', emoji: '🎨', unit: 'walls', q: 2 },
		{ name: 'Pottery Classes', emoji: '🏺', unit: 'seats', q: 10 },
		{ name: 'Jazz Violin', emoji: '🎻', unit: 'lessons', q: 15 },
		{ name: 'Hip Hop Dance', emoji: '💃', unit: 'classes', q: 20 },
		{ name: 'Film Editing', emoji: '🎬', unit: 'hours', q: 30 },
		{ name: 'Poetry Slam', emoji: '🎤', unit: 'events', q: 4 },
		{ name: 'Calligraphy', emoji: '✒️', unit: 'scrolls', q: 100 },
		{ name: 'Weaving', emoji: '🧵', unit: 'textiles', q: 15 },

		// Sustainability & Environment
		{ name: 'Urban Farming', emoji: '🥬', unit: 'consultations', q: 15 },
		{ name: 'Solar Installation', emoji: '☀️', unit: 'panels', q: 50 },
		{ name: 'Permaculture', emoji: '🌿', unit: 'designs', q: 3 },
		{ name: 'Water Purification', emoji: '💧', unit: 'liters', q: 1000 },
		{ name: 'Beekeeping', emoji: '🐝', unit: 'hives', q: 10 },
		{ name: 'Composting', emoji: '🍂', unit: 'bins', q: 25 },
		{ name: 'Bike Repair', emoji: '🚲', unit: 'bikes', q: 20 },
		{ name: 'Upcycling Clothes', emoji: '👕', unit: 'garments', q: 40 },
		{ name: 'Seed Banking', emoji: '🌱', unit: 'packets', q: 200 },
		{ name: 'Mycology', emoji: '🍄', unit: 'kits', q: 30 },

		// Services & Community
		{ name: 'Start-up Legal Aid', emoji: '⚖️', unit: 'consultations', q: 12 },
		{ name: 'Mental Health Chat', emoji: '🧠', unit: 'sessions', q: 30 },
		{ name: 'Eco-Tourism Guide', emoji: '🗺️', unit: 'tours', q: 10 },
		{ name: 'Translation', emoji: '🗣️', unit: 'pages', q: 50 },
		{ name: 'Conflict Mediation', emoji: '🤝', unit: 'sessions', q: 5 },
		{ name: 'Event Planning', emoji: '📅', unit: 'events', q: 2 },
		{ name: 'Elderly Companion', emoji: '👵', unit: 'visits', q: 15 },
		{ name: 'Dog Walking', emoji: '🐕', unit: 'walks', q: 25 },
		{ name: 'Carpentry', emoji: '🔨', unit: 'projects', q: 4 },
		{ name: 'Plumbing', emoji: '🔧', unit: 'repairs', q: 8 },

		// Education
		{ name: 'Local Language', emoji: '🗣️', unit: 'lessons', q: 20 },
		{ name: 'Math Tutoring', emoji: '➗', unit: 'hours', q: 10 },
		{ name: 'Physics Labs', emoji: '⚛️', unit: 'experiments', q: 5 },
		{ name: 'History Lectures', emoji: '📜', unit: 'talks', q: 3 },
		{ name: 'Literacy Program', emoji: '📖', unit: 'students', q: 12 },
		{ name: 'Cooking Class', emoji: '🍳', unit: 'meals', q: 8 }
	];

	MORE_GLOBAL_CITIES.forEach((city, i) => {
		// Add 2 random capacities for each city with JITTER
		// Use a large prime stride to avoid repeating patterns if list length shares factors with cities length
		const stride = 17; 
		const template1 = CAPACITY_TEMPLATES[(i * stride) % CAPACITY_TEMPLATES.length];
		const template2 = CAPACITY_TEMPLATES[((i * stride) + 11) % CAPACITY_TEMPLATES.length];

		slots.push(createCapacitySlot(template1.name, template1.emoji, template1.unit, template1.q, jitterLocation(city), 'Specific'));
		slots.push(createCapacitySlot(template2.name, template2.emoji, template2.unit, template2.q, jitterLocation(city), 'Specific'));
	});

	console.log(
		`[EXAMPLE] Created ${slots.length} example capacity slots (${slots.filter((c, i) => i < 56).length} in South America)`
	);
	return slots;
}

/**
 * V5: 100 SDG-focused need slots with realistic locations and time patterns
 * Returns NeedSlot[] (v5 schema)
 * 
 * Symmetric to capacities but represents DEMAND rather than SUPPLY.
 */
export function createExampleNeedSlots(): NeedSlot[] {
	const slots: NeedSlot[] = [];
	const timezone = getLocalTimeZone();

	// Helper to create a need slot (v5 schema)
	const createNeedSlot = (
		name: string,
		emoji: string,
		unit: string,
		quantity: number,
		city: { name: string; country: string; lat: number; lng: number },
		locationType: string = 'Specific',
		timePattern: any = null
	): NeedSlot => {
		const time = timePattern || {
			allDay: true,
			recurrence: 'weekly',
			startTime: null,
			endTime: null
		};

		// V5: Use availability_window for time ranges if not all-day
		const availability_window: AvailabilityWindow | undefined = (time.allDay || !time.startTime || !time.endTime) ? undefined : {
			time_ranges: [{
				start_time: time.startTime,
				end_time: time.endTime
			}]
		};

		return {
			id: crypto.randomUUID(),
			name,
			emoji,
			unit,
			description: '',
			quantity,
			// V5 REQUIRED: type_id for multi-dimensional allocation
			type_id: 'general', // Default need type
			filter_rule: null,
			location_type: locationType,
			latitude: city.lat,
			longitude: city.lng,
			city: city.name,
			country: city.country,
			start_date: new Date().toISOString().split('T')[0],
			end_date: null,
			time_zone: timezone,
			recurrence: (time.recurrence || 'weekly').toLowerCase() as any,
			// V5: Use availability_window instead of start_time/end_time
			availability_window
		};
	};

	// Reuse cities but shift them slightly so blue/red markers don't perfectly overlap
	// Shift: +0.002 lat/lng is about 200m
	const SHIFT = 0.002;

	const shiftCity = (city: typeof SOUTH_AMERICA_CITIES[0]) => ({
		...city,
		lat: city.lat + SHIFT,
		lng: city.lng + SHIFT
	});

	// ============ NEEDS (Symmetric Generation) ============

	// Brazillian Needs
	slots.push(createNeedSlot('Emergency Food', '🥫', 'meals', 1500, shiftCity(SOUTH_AMERICA_CITIES[0]), 'Specific')); // Sao Paulo
	slots.push(createNeedSlot('Clean Water', '💧', 'liters', 10000, shiftCity(SOUTH_AMERICA_CITIES[1]), 'Specific')); // Rio
	slots.push(createNeedSlot('Medical Supplies', '🩹', 'boxes', 50, shiftCity(SOUTH_AMERICA_CITIES[4]), 'Specific')); // Fortaleza
	slots.push(createNeedSlot('School Books', '📚', 'books', 200, shiftCity(SOUTH_AMERICA_CITIES[3]), 'Specific')); // Salvador
	slots.push(createNeedSlot('Shelter Tents', '⛺', 'units', 30, shiftCity(SOUTH_AMERICA_CITIES[6]), 'Specific')); // Manaus
	slots.push(createNeedSlot('Technical Mentorship', '👨‍💻', 'hours', 40, shiftCity(SOUTH_AMERICA_CITIES[0]), 'Online')); // Sao Paulo

	// Argentinian Needs
	slots.push(createNeedSlot('Warm Clothing', '🧥', 'items', 300, shiftCity(SOUTH_AMERICA_CITIES[10]), 'Specific')); // Buenos Aires
	slots.push(createNeedSlot('Construction Tools', '🔨', 'sets', 20, shiftCity(SOUTH_AMERICA_CITIES[11]), 'Specific')); // Cordoba

	// Colombian Needs
	slots.push(createNeedSlot('Reforestation Seedlings', '🌱', 'seedlings', 5000, shiftCity(SOUTH_AMERICA_CITIES[17]), 'Specific')); // Bogota
	slots.push(createNeedSlot('Language Teachers', '🗣️', 'teachers', 10, shiftCity(SOUTH_AMERICA_CITIES[18]), 'Specific')); // Medellin

	// Global Random Needs
	slots.push(createNeedSlot('Disaster Relief', '⛑️', 'volunteers', 50, shiftCity(GLOBAL_CITIES[8]), 'Specific')); // Mumbai
	slots.push(createNeedSlot('Vaccines', '💉', 'doses', 1000, shiftCity(GLOBAL_CITIES[0]), 'Specific')); // Nairobi
	slots.push(createNeedSlot('Legal Defense', '⚖️', 'hours', 100, shiftCity(GLOBAL_CITIES[16]), 'Online')); // Berlin
	slots.push(createNeedSlot('Community Internet', '📡', 'routers', 15, shiftCity(GLOBAL_CITIES[22]), 'Specific')); // Mexico City

	// ============ EXPANDED GLOBAL NEEDS (Generated) ============
	const NEED_TEMPLATES = [
		// Infrastructure & Basic Needs
		{ name: 'Clean Water', emoji: '🚰', unit: 'liters', q: 5000 },
		{ name: 'Affordable Housing', emoji: '🏠', unit: 'units', q: 10 },
		{ name: 'Stable Electricity', emoji: '💡', unit: 'connex', q: 50 },
		{ name: 'Internet Access', emoji: '📡', unit: 'nodes', q: 25 },
		{ name: 'Sanitation', emoji: '🚽', unit: 'toilets', q: 100 },
		{ name: 'Road Repair', emoji: '🚧', unit: 'km', q: 5 },
		{ name: 'Waste Management', emoji: '♻️', unit: 'tons', q: 50 },
		{ name: 'Heating Fuel', emoji: '🔥', unit: 'liters', q: 500 },
		{ name: 'Public Transport', emoji: '🚌', unit: 'rides', q: 500 },
		{ name: 'Street Lighting', emoji: '🔦', unit: 'lights', q: 30 },

		// Medical & Health
		{ name: 'Vaccines', emoji: '💉', unit: 'doses', q: 200 },
		{ name: 'Insulin', emoji: '⚕️', unit: 'vials', q: 50 },
		{ name: 'Maternity Care', emoji: '🤰', unit: 'kits', q: 20 },
		{ name: 'Mosquito Nets', emoji: '🕸️', unit: 'nets', q: 500 },
		{ name: 'Dental Hygiene', emoji: '🦷', unit: 'brushes', q: 1000 },
		{ name: 'Mental Health', emoji: '🧠', unit: 'therapists', q: 5 },
		{ name: 'First Aid Kits', emoji: '⛑️', unit: 'kits', q: 100 },
		{ name: 'Prescription Glasses', emoji: '👓', unit: 'pairs', q: 50 },

		// Education & Knowledge
		{ name: 'Coding Education', emoji: '💻', unit: 'students', q: 25 },
		{ name: 'Textbooks', emoji: '📚', unit: 'books', q: 300 },
		{ name: 'School Supplies', emoji: '✏️', unit: 'sets', q: 150 },
		{ name: 'Laptops', emoji: '💻', unit: 'units', q: 10 },
		{ name: 'Science Equipment', emoji: '🔬', unit: 'pieces', q: 5 },
		{ name: 'Teacher Training', emoji: '🎓', unit: 'workshops', q: 2 },
		{ name: 'Library Access', emoji: '📖', unit: 'hours', q: 500 },

		// Economic & Social
		{ name: 'Micro-loans', emoji: '💰', unit: 'loans', q: 15 },
		{ name: 'Job Training', emoji: '🛠️', unit: 'courses', q: 8 },
		{ name: 'Legal Rep', emoji: '⚖️', unit: 'cases', q: 3 },
		{ name: 'Refugee Support', emoji: '⛺', unit: 'tents', q: 20 },
		{ name: 'Childcare', emoji: '👶', unit: 'slots', q: 10 },
		{ name: 'Senior Care', emoji: '👴', unit: 'hours', q: 100 },
		{ name: 'Translation', emoji: '🗣️', unit: 'hours', q: 20 },
		{ name: 'Artist Space', emoji: '🎨', unit: 'studios', q: 5 },

		// Environmental
		{ name: 'Reforestation', emoji: '🌲', unit: 'trees', q: 1000 },
		{ name: 'Plastic Recycling', emoji: '🥤', unit: 'bins', q: 50 },
		{ name: 'Soil Restoration', emoji: '🌱', unit: 'hectares', q: 2 },
		{ name: 'Renewable Energy', emoji: '⚡', unit: 'kWh', q: 1000 },
		{ name: 'Clean Air', emoji: '💨', unit: 'masks', q: 500 }
	];

	MORE_GLOBAL_CITIES.forEach((city, i) => {
		// Add 2 random needs for each city with JITTER
		// Use different stride than capacities to mix it up
		const stride = 13;
		const template1 = NEED_TEMPLATES[(i * stride) % NEED_TEMPLATES.length]; 
		const template2 = NEED_TEMPLATES[((i * stride) + 7) % NEED_TEMPLATES.length];

		slots.push(createNeedSlot(template1.name, template1.emoji, template1.unit, template1.q, jitterLocation(city), 'Specific'));
		slots.push(createNeedSlot(template2.name, template2.emoji, template2.unit, template2.q, jitterLocation(city), 'Specific'));
	});

	console.log(`[EXAMPLE] Created ${slots.length} example need slots`);
	return slots;
}

// V5: Expose to window for debugging
// Delay initialization to ensure all stores are initialized (prevents iOS Safari errors)
if (typeof window !== 'undefined') {
	setTimeout(() => {
		(window as any).populateWithExampleData = populateWithExampleData;
		(window as any).createExampleCapacitySlots = createExampleCapacitySlots;
		(window as any).createExampleNeedSlots = createExampleNeedSlots;

		console.log('[DEBUG] V5 Example functions exposed to window:');
		console.log('  - populateWithExampleData(rootNode)');
		console.log('  - createExampleCapacitySlots()');
		console.log('  - createExampleNeedSlots()');
	}, 0);
}
