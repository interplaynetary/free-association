// V5: Import from v5 stores and schemas
import type { RootNode, AvailabilitySlot, Commitment } from '@playnet/free-association/schemas';

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
		locationType: string = 'In-Person',
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
			recurrence: time.recurrence as any,
			// V5: Use availability_window instead of start_time/end_time
			availability_window
		};
	};

	// ============ SOUTH AMERICA CAPACITIES (50+) ============

	// Brazil (15 capacities)
	slots.push(
		createCapacitySlot('Community Lunch', '🍲', 'meals', 120, SOUTH_AMERICA_CITIES[0], 'In-Person', {
			allDay: false,
			recurrence: 'daily',
			startTime: '12:00',
			endTime: '14:00'
		})
	);
	slots.push(
		createCapacitySlot('Water Filtration', '💧', 'liters', 5000, SOUTH_AMERICA_CITIES[1], 'In-Person')
	);
	slots.push(
		createCapacitySlot(
			'Solar Panel Installation',
			'☀️',
			'panels',
			25,
			SOUTH_AMERICA_CITIES[2],
			'In-Person'
		)
	);
	slots.push(
		createCapacitySlot('Literacy Classes', '📚', 'students', 30, SOUTH_AMERICA_CITIES[3], 'In-Person', {
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
			'In-Person',
			{ allDay: false, recurrence: 'weekly', startTime: '08:00', endTime: '17:00' }
		)
	);
	slots.push(
		createCapacitySlot('Tree Planting', '🌳', 'saplings', 500, SOUTH_AMERICA_CITIES[5], 'Outdoor')
	);
	slots.push(
		createCapacitySlot('Waste Collection', '♻️', 'kg', 2000, SOUTH_AMERICA_CITIES[6], 'Mobile')
	);
	slots.push(
		createCapacitySlot(
			'Fishing Training',
			'🐟',
			'participants',
			15,
			SOUTH_AMERICA_CITIES[7],
			'In-Person'
		)
	);
	slots.push(
		createCapacitySlot('Coding Bootcamp', '💻', 'students', 25, SOUTH_AMERICA_CITIES[8], 'Hybrid')
	);
	slots.push(
		createCapacitySlot('Microfinance Loans', '💰', 'USD', 10000, SOUTH_AMERICA_CITIES[9], 'In-Person')
	);
	slots.push(
		createCapacitySlot('Urban Garden', '🥬', 'plots', 50, SOUTH_AMERICA_CITIES[0], 'Outdoor')
	);
	slots.push(
		createCapacitySlot(
			'Women Empowerment',
			'👩',
			'workshops',
			8,
			SOUTH_AMERICA_CITIES[1],
			'In-Person',
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
			'In-Person'
		)
	);
	slots.push(
		createCapacitySlot('Child Care', '👶', 'children', 20, SOUTH_AMERICA_CITIES[3], 'In-Person', {
			allDay: false,
			recurrence: 'weekly',
			startTime: '07:00',
			endTime: '18:00'
		})
	);
	slots.push(
		createCapacitySlot('Mental Health Support', '🧠', 'sessions', 30, SOUTH_AMERICA_CITIES[4], 'Hybrid')
	);

	// Argentina (10 capacities)
	slots.push(
		createCapacitySlot(
			'Cooperative Bakery',
			'🍞',
			'loaves',
			200,
			SOUTH_AMERICA_CITIES[10],
			'In-Person',
			{ allDay: false, recurrence: 'Daily', startTime: '06:00', endTime: '14:00' }
		)
	);
	slots.push(
		createCapacitySlot('Legal Aid', '⚖️', 'consultations', 15, SOUTH_AMERICA_CITIES[11], 'In-Person')
	);
	slots.push(
		createCapacitySlot(
			'Agricultural Training',
			'🚜',
			'farmers',
			40,
			SOUTH_AMERICA_CITIES[12],
			'In-Person'
		)
	);
	slots.push(
		createCapacitySlot('Bike Sharing', '🚲', 'bikes', 80, SOUTH_AMERICA_CITIES[13], 'Mobile')
	);
	slots.push(
		createCapacitySlot('Youth Sports', '⚽', 'participants', 60, SOUTH_AMERICA_CITIES[14], 'Outdoor')
	);
	slots.push(
		createCapacitySlot(
			'Artisan Market',
			'🎨',
			'vendor spots',
			30,
			SOUTH_AMERICA_CITIES[15],
			'Outdoor',
			{ allDay: false, recurrence: 'Weekends', startTime: '10:00', endTime: '18:00' }
		)
	);
	slots.push(
		createCapacitySlot('Housing Renovation', '🏠', 'homes', 5, SOUTH_AMERICA_CITIES[16], 'In-Person')
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
		createCapacitySlot('Textile Recycling', '👕', 'kg', 1000, SOUTH_AMERICA_CITIES[11], 'In-Person')
	);
	slots.push(
		createCapacitySlot('Elder Care', '👴', 'seniors', 25, SOUTH_AMERICA_CITIES[12], 'In-Person')
	);

	// Colombia (8 capacities)
	slots.push(
		createCapacitySlot('Peace Mediation', '🕊️', 'sessions', 20, SOUTH_AMERICA_CITIES[17], 'In-Person')
	);
	slots.push(
		createCapacitySlot('Coffee Cooperative', '☕', 'kg', 500, SOUTH_AMERICA_CITIES[18], 'In-Person')
	);
	slots.push(
		createCapacitySlot('Music Education', '🎵', 'students', 35, SOUTH_AMERICA_CITIES[19], 'In-Person')
	);
	slots.push(
		createCapacitySlot('River Cleanup', '🌊', 'kg removed', 3000, SOUTH_AMERICA_CITIES[20], 'Outdoor')
	);
	slots.push(
		createCapacitySlot('Ecotourism', '🌴', 'visitors', 40, SOUTH_AMERICA_CITIES[21], 'In-Person')
	);
	slots.push(
		createCapacitySlot('Digital Library', '📱', 'devices', 50, SOUTH_AMERICA_CITIES[17], 'In-Person')
	);
	slots.push(
		createCapacitySlot(
			'Organic Market',
			'🥑',
			'vendor spots',
			25,
			SOUTH_AMERICA_CITIES[18],
			'Outdoor',
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
			'In-Person'
		)
	);

	// Peru (6 capacities)
	slots.push(
		createCapacitySlot('Quinoa Farming', '🌾', 'hectares', 20, SOUTH_AMERICA_CITIES[23], 'Outdoor')
	);
	slots.push(
		createCapacitySlot('Tourism Guiding', '🗺️', 'tours', 10, SOUTH_AMERICA_CITIES[24], 'Outdoor', {
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
			'In-Person'
		)
	);
	slots.push(
		createCapacitySlot(
			'Water Infrastructure',
			'🚰',
			'connections',
			50,
			SOUTH_AMERICA_CITIES[23],
			'In-Person'
		)
	);
	slots.push(
		createCapacitySlot(
			'Indigenous Knowledge',
			'📜',
			'workshops',
			12,
			SOUTH_AMERICA_CITIES[24],
			'In-Person'
		)
	);
	slots.push(
		createCapacitySlot(
			'Rainforest Conservation',
			'🌲',
			'hectares',
			1000,
			SOUTH_AMERICA_CITIES[25],
			'Outdoor'
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
			'In-Person'
		)
	);
	slots.push(
		createCapacitySlot('Marine Research', '🐋', 'expeditions', 8, SOUTH_AMERICA_CITIES[28], 'Outdoor')
	);
	slots.push(
		createCapacitySlot('Wine Cooperative', '🍷', 'bottles', 2000, SOUTH_AMERICA_CITIES[29], 'In-Person')
	);
	slots.push(
		createCapacitySlot(
			'Tech Innovation Hub',
			'🚀',
			'workspaces',
			30,
			SOUTH_AMERICA_CITIES[27],
			'In-Person'
		)
	);
	slots.push(
		createCapacitySlot(
			'Mountain Conservation',
			'⛰️',
			'hectares',
			500,
			SOUTH_AMERICA_CITIES[30],
			'Outdoor'
		)
	);

	// Ecuador (4 capacities)
	slots.push(
		createCapacitySlot('Cacao Production', '🍫', 'kg', 800, SOUTH_AMERICA_CITIES[31], 'In-Person')
	);
	slots.push(
		createCapacitySlot(
			'Biodiversity Study',
			'🦜',
			'researchers',
			12,
			SOUTH_AMERICA_CITIES[32],
			'Outdoor'
		)
	);
	slots.push(
		createCapacitySlot(
			'Indigenous Medicine',
			'🌿',
			'treatments',
			40,
			SOUTH_AMERICA_CITIES[33],
			'In-Person'
		)
	);
	slots.push(
		createCapacitySlot('Volcano Monitoring', '🌋', 'sensors', 15, SOUTH_AMERICA_CITIES[31], 'Outdoor')
	);

	// Bolivia (3 capacities)
	slots.push(
		createCapacitySlot('Llama Wool Collective', '🦙', 'kg', 300, SOUTH_AMERICA_CITIES[34], 'In-Person')
	);
	slots.push(
		createCapacitySlot('Salt Flat Tours', '✨', 'tours', 15, SOUTH_AMERICA_CITIES[35], 'Outdoor')
	);
	slots.push(
		createCapacitySlot(
			'Mining Alternative',
			'💎',
			'participants',
			25,
			SOUTH_AMERICA_CITIES[36],
			'In-Person'
		)
	);

	// Venezuela (2 capacities)
	slots.push(
		createCapacitySlot('Food Distribution', '🥫', 'kg', 5000, SOUTH_AMERICA_CITIES[37], 'In-Person')
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
			'In-Person'
		)
	);
	slots.push(
		createCapacitySlot('Beach Cleanup', '🏖️', 'kg removed', 2000, SOUTH_AMERICA_CITIES[43], 'Outdoor')
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
		createCapacitySlot('Solar Kiosk', '☀️', 'charging stations', 10, GLOBAL_CITIES[1], 'In-Person')
	);
	slots.push(
		createCapacitySlot('Maternal Health', '🤰', 'checkups', 50, GLOBAL_CITIES[2], 'In-Person')
	);
	slots.push(
		createCapacitySlot('Drought Relief', '💧', 'liters', 10000, GLOBAL_CITIES[3], 'Mobile')
	);
	slots.push(
		createCapacitySlot('School Meals', '🍽️', 'meals', 500, GLOBAL_CITIES[4], 'In-Person', {
			allDay: false,
			recurrence: 'weekly',
			startTime: '12:00',
			endTime: '13:00'
		})
	);
	slots.push(
		createCapacitySlot('Livestock Vaccination', '🐄', 'animals', 200, GLOBAL_CITIES[5], 'Mobile')
	);
	slots.push(
		createCapacitySlot('Malaria Prevention', '🦟', 'bed nets', 1000, GLOBAL_CITIES[6], 'In-Person')
	);
	slots.push(
		createCapacitySlot('Women Literacy', '✍️', 'students', 40, GLOBAL_CITIES[7], 'In-Person')
	);
	slots.push(
		createCapacitySlot('Desert Greening', '🌱', 'hectares', 100, GLOBAL_CITIES[0], 'Outdoor')
	);
	slots.push(
		createCapacitySlot('Handicraft Export', '🏺', 'products', 500, GLOBAL_CITIES[1], 'Online')
	);
	slots.push(
		createCapacitySlot('Clean Cookstoves', '🔥', 'stoves', 300, GLOBAL_CITIES[2], 'In-Person')
	);
	slots.push(
		createCapacitySlot('Elephant Conservation', '🐘', 'hectares', 5000, GLOBAL_CITIES[3], 'Outdoor')
	);
	slots.push(
		createCapacitySlot('Fishing Rights', '🎣', 'licenses', 100, GLOBAL_CITIES[4], 'In-Person')
	);
	slots.push(createCapacitySlot('Mobile Clinics', '🚑', 'visits', 30, GLOBAL_CITIES[5], 'Mobile'));
	slots.push(
		createCapacitySlot('Storytelling Circle', '📖', 'sessions', 12, GLOBAL_CITIES[6], 'In-Person')
	);

	// Asia (15 capacities)
	slots.push(
		createCapacitySlot('Monsoon Preparedness', '🌧️', 'households', 200, GLOBAL_CITIES[8], 'In-Person')
	);
	slots.push(
		createCapacitySlot('Street Vendor Support', '🛒', 'vendors', 80, GLOBAL_CITIES[9], 'In-Person')
	);
	slots.push(
		createCapacitySlot('Textile Worker Rights', '👗', 'workers', 150, GLOBAL_CITIES[10], 'In-Person')
	);
	slots.push(
		createCapacitySlot('Mangrove Restoration', '🌊', 'hectares', 50, GLOBAL_CITIES[11], 'Outdoor')
	);
	slots.push(
		createCapacitySlot('Typhoon Shelter', '🏠', 'people', 300, GLOBAL_CITIES[12], 'In-Person')
	);
	slots.push(
		createCapacitySlot('Rice Cooperative', '🍚', 'kg', 5000, GLOBAL_CITIES[13], 'In-Person')
	);
	slots.push(
		createCapacitySlot('Coral Reef Protection', '🪸', 'hectares', 20, GLOBAL_CITIES[14], 'Outdoor')
	);
	slots.push(
		createCapacitySlot('Mountain Trails', '🥾', 'trails maintained', 10, GLOBAL_CITIES[15], 'Outdoor')
	);
	slots.push(
		createCapacitySlot('Tea Garden Collective', '🍵', 'kg', 1000, GLOBAL_CITIES[8], 'In-Person')
	);
	slots.push(
		createCapacitySlot('Flood Warning System', '📢', 'villages', 50, GLOBAL_CITIES[9], 'In-Person')
	);
	slots.push(
		createCapacitySlot('Spice Market', '🌶️', 'vendor spots', 40, GLOBAL_CITIES[10], 'In-Person')
	);
	slots.push(
		createCapacitySlot('Tiger Conservation', '🐅', 'hectares', 10000, GLOBAL_CITIES[11], 'Outdoor')
	);
	slots.push(
		createCapacitySlot('Coconut Processing', '🥥', 'kg', 2000, GLOBAL_CITIES[12], 'In-Person')
	);
	slots.push(
		createCapacitySlot('Meditation Center', '🧘', 'sessions', 20, GLOBAL_CITIES[13], 'In-Person')
	);
	slots.push(
		createCapacitySlot('Earthquake Recovery', '🏗️', 'buildings', 15, GLOBAL_CITIES[14], 'In-Person')
	);

	// Europe (8 capacities)
	slots.push(
		createCapacitySlot('Refugee Integration', '🤝', 'participants', 50, GLOBAL_CITIES[16], 'In-Person')
	);
	slots.push(
		createCapacitySlot('Zero Waste Workshop', '♻️', 'participants', 30, GLOBAL_CITIES[17], 'In-Person')
	);
	slots.push(
		createCapacitySlot('Community Garden', '🌻', 'plots', 40, GLOBAL_CITIES[18], 'Outdoor')
	);
	slots.push(
		createCapacitySlot('Bike Repair Collective', '🔧', 'repairs', 100, GLOBAL_CITIES[19], 'In-Person')
	);
	slots.push(
		createCapacitySlot('Climate Strike', '📢', 'participants', 5000, GLOBAL_CITIES[20], 'Outdoor')
	);
	slots.push(
		createCapacitySlot('Cooperative Housing', '🏘️', 'units', 20, GLOBAL_CITIES[21], 'In-Person')
	);
	slots.push(
		createCapacitySlot('Language Exchange', '🗣️', 'sessions', 25, GLOBAL_CITIES[16], 'Hybrid')
	);
	slots.push(
		createCapacitySlot('Permaculture Design', '🌿', 'projects', 12, GLOBAL_CITIES[17], 'In-Person')
	);

	// Central America & Caribbean (6 capacities)
	slots.push(
		createCapacitySlot('Hurricane Relief', '🌀', 'families', 100, GLOBAL_CITIES[22], 'In-Person')
	);
	slots.push(
		createCapacitySlot('Indigenous Crafts', '🎭', 'artisans', 30, GLOBAL_CITIES[23], 'In-Person')
	);
	slots.push(
		createCapacitySlot('Coral Restoration', '🪸', 'coral pieces', 1000, GLOBAL_CITIES[24], 'Outdoor')
	);
	slots.push(
		createCapacitySlot('Agroforestry', '🌳', 'hectares', 80, GLOBAL_CITIES[25], 'Outdoor')
	);
	slots.push(
		createCapacitySlot('Rainwater Harvesting', '💧', 'systems', 40, GLOBAL_CITIES[26], 'In-Person')
	);
	slots.push(
		createCapacitySlot('Traditional Dance', '💃', 'classes', 20, GLOBAL_CITIES[27], 'In-Person')
	);

	// Oceania (3 capacities)
	slots.push(
		createCapacitySlot('Aboriginal Art', '🎨', 'workshops', 15, GLOBAL_CITIES[28], 'In-Person')
	);
	slots.push(
		createCapacitySlot('Bushfire Recovery', '🔥', 'hectares', 500, GLOBAL_CITIES[29], 'Outdoor')
	);
	slots.push(
		createCapacitySlot('Māori Language', '📚', 'students', 25, GLOBAL_CITIES[30], 'In-Person')
	);

	// Middle East (3 capacities)
	slots.push(
		createCapacitySlot('Water Desalination', '💧', 'liters', 8000, GLOBAL_CITIES[31], 'In-Person')
	);
	slots.push(
		createCapacitySlot('Conflict Mediation', '☮️', 'sessions', 15, GLOBAL_CITIES[32], 'In-Person')
	);
	slots.push(
		createCapacitySlot('Historic Preservation', '🏛️', 'sites', 5, GLOBAL_CITIES[33], 'In-Person')
	);

	console.log(
		`[EXAMPLE] Created ${slots.length} example capacity slots (${slots.filter((c, i) => i < 56).length} in South America)`
	);
	return slots;
}

// V5: Expose to window for debugging
// Delay initialization to ensure all stores are initialized (prevents iOS Safari errors)
if (typeof window !== 'undefined') {
	setTimeout(() => {
		(window as any).populateWithExampleData = populateWithExampleData;
		(window as any).createExampleCapacitySlots = createExampleCapacitySlots;
		
		console.log('[DEBUG] V5 Example functions exposed to window:');
		console.log('  - populateWithExampleData(rootNode)');
		console.log('  - createExampleCapacitySlots()');
	}, 0);
}


