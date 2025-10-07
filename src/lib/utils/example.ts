import { addCapacity as addCapacityToCollection } from '$lib/protocol';
import type { RootNode, ProviderCapacity, CapacitiesCollection } from '$lib/schema';
import { userTree, userCapacities } from '$lib/state/core.svelte';
import { get } from 'svelte/store';
import { getLocalTimeZone } from '@internationalized/date';
import { populateSDGTree } from './sdg';

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
 * 100 SDG-focused capacities with realistic locations and time patterns
 */
export function createExampleCapacities(userPubKey: string): ProviderCapacity[] {
	const capacities: ProviderCapacity[] = [];
	const timezone = getLocalTimeZone();

	// Helper to create a capacity
	const createCapacity = (
		name: string,
		emoji: string,
		unit: string,
		quantity: number,
		city: { name: string; country: string; lat: number; lng: number },
		locationType: string = 'In-Person',
		timePattern: any = null
	): ProviderCapacity => {
		const time = timePattern || {
			allDay: true,
			recurrence: 'Weekly',
			startTime: null,
			endTime: null
		};

		return {
			id: crypto.randomUUID(),
			name,
			emoji,
			unit,
			description: '',
			max_natural_div: Math.min(quantity, 10),
			max_percentage_div: 0.8,
			hidden_until_request_accepted: false,
			owner_id: userPubKey,
			filter_rule: null,
			availability_slots: [
				{
					id: `slot-${Date.now()}-${Math.random().toString(36).substr(2, 9)}`,
					quantity,
					location_type: locationType,
					latitude: city.lat,
					longitude: city.lng,
					city: city.name,
					country: city.country,
					all_day: time.allDay,
					start_date: new Date().toISOString().split('T')[0],
					start_time: time.startTime,
					end_date: null,
					end_time: time.endTime,
					time_zone: timezone,
					recurrence: time.recurrence,
					custom_recurrence_repeat_every: null,
					custom_recurrence_repeat_unit: null,
					custom_recurrence_end_type: null,
					custom_recurrence_end_value: null
				}
			]
		};
	};

	// ============ SOUTH AMERICA CAPACITIES (50+) ============

	// Brazil (15 capacities)
	capacities.push(
		createCapacity('Community Lunch', '🍲', 'meals', 120, SOUTH_AMERICA_CITIES[0], 'In-Person', {
			allDay: false,
			recurrence: 'Daily',
			startTime: '12:00',
			endTime: '14:00'
		})
	);
	capacities.push(
		createCapacity('Water Filtration', '💧', 'liters', 5000, SOUTH_AMERICA_CITIES[1], 'In-Person')
	);
	capacities.push(
		createCapacity(
			'Solar Panel Installation',
			'☀️',
			'panels',
			25,
			SOUTH_AMERICA_CITIES[2],
			'In-Person'
		)
	);
	capacities.push(
		createCapacity('Literacy Classes', '📚', 'students', 30, SOUTH_AMERICA_CITIES[3], 'In-Person', {
			allDay: false,
			recurrence: 'Weekdays',
			startTime: '09:00',
			endTime: '12:00'
		})
	);
	capacities.push(
		createCapacity(
			'Medical Consultation',
			'🏥',
			'appointments',
			40,
			SOUTH_AMERICA_CITIES[4],
			'In-Person',
			{ allDay: false, recurrence: 'Weekdays', startTime: '08:00', endTime: '17:00' }
		)
	);
	capacities.push(
		createCapacity('Tree Planting', '🌳', 'saplings', 500, SOUTH_AMERICA_CITIES[5], 'Outdoor')
	);
	capacities.push(
		createCapacity('Waste Collection', '♻️', 'kg', 2000, SOUTH_AMERICA_CITIES[6], 'Mobile')
	);
	capacities.push(
		createCapacity(
			'Fishing Training',
			'🐟',
			'participants',
			15,
			SOUTH_AMERICA_CITIES[7],
			'In-Person'
		)
	);
	capacities.push(
		createCapacity('Coding Bootcamp', '💻', 'students', 25, SOUTH_AMERICA_CITIES[8], 'Hybrid')
	);
	capacities.push(
		createCapacity('Microfinance Loans', '💰', 'USD', 10000, SOUTH_AMERICA_CITIES[9], 'In-Person')
	);
	capacities.push(
		createCapacity('Urban Garden', '🥬', 'plots', 50, SOUTH_AMERICA_CITIES[0], 'Outdoor')
	);
	capacities.push(
		createCapacity(
			'Women Empowerment',
			'👩',
			'workshops',
			8,
			SOUTH_AMERICA_CITIES[1],
			'In-Person',
			{ allDay: false, recurrence: 'Weekly', startTime: '15:00', endTime: '18:00' }
		)
	);
	capacities.push(
		createCapacity(
			'Clean Energy Access',
			'⚡',
			'households',
			100,
			SOUTH_AMERICA_CITIES[2],
			'In-Person'
		)
	);
	capacities.push(
		createCapacity('Child Care', '👶', 'children', 20, SOUTH_AMERICA_CITIES[3], 'In-Person', {
			allDay: false,
			recurrence: 'Weekdays',
			startTime: '07:00',
			endTime: '18:00'
		})
	);
	capacities.push(
		createCapacity('Mental Health Support', '🧠', 'sessions', 30, SOUTH_AMERICA_CITIES[4], 'Hybrid')
	);

	// Argentina (10 capacities)
	capacities.push(
		createCapacity(
			'Cooperative Bakery',
			'🍞',
			'loaves',
			200,
			SOUTH_AMERICA_CITIES[10],
			'In-Person',
			{ allDay: false, recurrence: 'Daily', startTime: '06:00', endTime: '14:00' }
		)
	);
	capacities.push(
		createCapacity('Legal Aid', '⚖️', 'consultations', 15, SOUTH_AMERICA_CITIES[11], 'In-Person')
	);
	capacities.push(
		createCapacity(
			'Agricultural Training',
			'🚜',
			'farmers',
			40,
			SOUTH_AMERICA_CITIES[12],
			'In-Person'
		)
	);
	capacities.push(
		createCapacity('Bike Sharing', '🚲', 'bikes', 80, SOUTH_AMERICA_CITIES[13], 'Mobile')
	);
	capacities.push(
		createCapacity('Youth Sports', '⚽', 'participants', 60, SOUTH_AMERICA_CITIES[14], 'Outdoor')
	);
	capacities.push(
		createCapacity(
			'Artisan Market',
			'🎨',
			'vendor spots',
			30,
			SOUTH_AMERICA_CITIES[15],
			'Outdoor',
			{ allDay: false, recurrence: 'Weekends', startTime: '10:00', endTime: '18:00' }
		)
	);
	capacities.push(
		createCapacity('Housing Renovation', '🏠', 'homes', 5, SOUTH_AMERICA_CITIES[16], 'In-Person')
	);
	capacities.push(
		createCapacity(
			'Community Radio',
			'📻',
			'broadcast hours',
			168,
			SOUTH_AMERICA_CITIES[10],
			'Online'
		)
	);
	capacities.push(
		createCapacity('Textile Recycling', '👕', 'kg', 1000, SOUTH_AMERICA_CITIES[11], 'In-Person')
	);
	capacities.push(
		createCapacity('Elder Care', '👴', 'seniors', 25, SOUTH_AMERICA_CITIES[12], 'In-Person')
	);

	// Colombia (8 capacities)
	capacities.push(
		createCapacity('Peace Mediation', '🕊️', 'sessions', 20, SOUTH_AMERICA_CITIES[17], 'In-Person')
	);
	capacities.push(
		createCapacity('Coffee Cooperative', '☕', 'kg', 500, SOUTH_AMERICA_CITIES[18], 'In-Person')
	);
	capacities.push(
		createCapacity('Music Education', '🎵', 'students', 35, SOUTH_AMERICA_CITIES[19], 'In-Person')
	);
	capacities.push(
		createCapacity('River Cleanup', '🌊', 'kg removed', 3000, SOUTH_AMERICA_CITIES[20], 'Outdoor')
	);
	capacities.push(
		createCapacity('Ecotourism', '🌴', 'visitors', 40, SOUTH_AMERICA_CITIES[21], 'In-Person')
	);
	capacities.push(
		createCapacity('Digital Library', '📱', 'devices', 50, SOUTH_AMERICA_CITIES[17], 'In-Person')
	);
	capacities.push(
		createCapacity(
			'Organic Market',
			'🥑',
			'vendor spots',
			25,
			SOUTH_AMERICA_CITIES[18],
			'Outdoor',
			{ allDay: false, recurrence: 'Weekly', startTime: '08:00', endTime: '14:00' }
		)
	);
	capacities.push(
		createCapacity(
			'Craft Workshop',
			'🧶',
			'participants',
			15,
			SOUTH_AMERICA_CITIES[19],
			'In-Person'
		)
	);

	// Peru (6 capacities)
	capacities.push(
		createCapacity('Quinoa Farming', '🌾', 'hectares', 20, SOUTH_AMERICA_CITIES[23], 'Outdoor')
	);
	capacities.push(
		createCapacity('Tourism Guiding', '🗺️', 'tours', 10, SOUTH_AMERICA_CITIES[24], 'Outdoor', {
			allDay: true,
			recurrence: 'Daily',
			startTime: null,
			endTime: null
		})
	);
	capacities.push(
		createCapacity(
			'Weaving Collective',
			'🧵',
			'textiles',
			100,
			SOUTH_AMERICA_CITIES[25],
			'In-Person'
		)
	);
	capacities.push(
		createCapacity(
			'Water Infrastructure',
			'🚰',
			'connections',
			50,
			SOUTH_AMERICA_CITIES[23],
			'In-Person'
		)
	);
	capacities.push(
		createCapacity(
			'Indigenous Knowledge',
			'📜',
			'workshops',
			12,
			SOUTH_AMERICA_CITIES[24],
			'In-Person'
		)
	);
	capacities.push(
		createCapacity(
			'Rainforest Conservation',
			'🌲',
			'hectares',
			1000,
			SOUTH_AMERICA_CITIES[25],
			'Outdoor'
		)
	);

	// Chile (5 capacities)
	capacities.push(
		createCapacity(
			'Earthquake Preparedness',
			'🏗️',
			'trainings',
			20,
			SOUTH_AMERICA_CITIES[27],
			'In-Person'
		)
	);
	capacities.push(
		createCapacity('Marine Research', '🐋', 'expeditions', 8, SOUTH_AMERICA_CITIES[28], 'Outdoor')
	);
	capacities.push(
		createCapacity('Wine Cooperative', '🍷', 'bottles', 2000, SOUTH_AMERICA_CITIES[29], 'In-Person')
	);
	capacities.push(
		createCapacity(
			'Tech Innovation Hub',
			'🚀',
			'workspaces',
			30,
			SOUTH_AMERICA_CITIES[27],
			'In-Person'
		)
	);
	capacities.push(
		createCapacity(
			'Mountain Conservation',
			'⛰️',
			'hectares',
			500,
			SOUTH_AMERICA_CITIES[30],
			'Outdoor'
		)
	);

	// Ecuador (4 capacities)
	capacities.push(
		createCapacity('Cacao Production', '🍫', 'kg', 800, SOUTH_AMERICA_CITIES[31], 'In-Person')
	);
	capacities.push(
		createCapacity(
			'Biodiversity Study',
			'🦜',
			'researchers',
			12,
			SOUTH_AMERICA_CITIES[32],
			'Outdoor'
		)
	);
	capacities.push(
		createCapacity(
			'Indigenous Medicine',
			'🌿',
			'treatments',
			40,
			SOUTH_AMERICA_CITIES[33],
			'In-Person'
		)
	);
	capacities.push(
		createCapacity('Volcano Monitoring', '🌋', 'sensors', 15, SOUTH_AMERICA_CITIES[31], 'Outdoor')
	);

	// Bolivia (3 capacities)
	capacities.push(
		createCapacity('Llama Wool Collective', '🦙', 'kg', 300, SOUTH_AMERICA_CITIES[34], 'In-Person')
	);
	capacities.push(
		createCapacity('Salt Flat Tours', '✨', 'tours', 15, SOUTH_AMERICA_CITIES[35], 'Outdoor')
	);
	capacities.push(
		createCapacity(
			'Mining Alternative',
			'💎',
			'participants',
			25,
			SOUTH_AMERICA_CITIES[36],
			'In-Person'
		)
	);

	// Venezuela (2 capacities)
	capacities.push(
		createCapacity('Food Distribution', '🥫', 'kg', 5000, SOUTH_AMERICA_CITIES[37], 'In-Person')
	);
	capacities.push(
		createCapacity('Community Currency', '🪙', 'credits', 10000, SOUTH_AMERICA_CITIES[38], 'Online')
	);

	// Rest of South America (3 capacities)
	capacities.push(
		createCapacity(
			'Cross-Border Trade',
			'🤝',
			'transactions',
			50,
			SOUTH_AMERICA_CITIES[40],
			'In-Person'
		)
	);
	capacities.push(
		createCapacity('Beach Cleanup', '🏖️', 'kg removed', 2000, SOUTH_AMERICA_CITIES[43], 'Outdoor')
	);
	capacities.push(
		createCapacity(
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
	capacities.push(
		createCapacity('Mobile Banking', '📱', 'accounts', 1000, GLOBAL_CITIES[0], 'Online')
	);
	capacities.push(
		createCapacity('Solar Kiosk', '☀️', 'charging stations', 10, GLOBAL_CITIES[1], 'In-Person')
	);
	capacities.push(
		createCapacity('Maternal Health', '🤰', 'checkups', 50, GLOBAL_CITIES[2], 'In-Person')
	);
	capacities.push(
		createCapacity('Drought Relief', '💧', 'liters', 10000, GLOBAL_CITIES[3], 'Mobile')
	);
	capacities.push(
		createCapacity('School Meals', '🍽️', 'meals', 500, GLOBAL_CITIES[4], 'In-Person', {
			allDay: false,
			recurrence: 'Weekdays',
			startTime: '12:00',
			endTime: '13:00'
		})
	);
	capacities.push(
		createCapacity('Livestock Vaccination', '🐄', 'animals', 200, GLOBAL_CITIES[5], 'Mobile')
	);
	capacities.push(
		createCapacity('Malaria Prevention', '🦟', 'bed nets', 1000, GLOBAL_CITIES[6], 'In-Person')
	);
	capacities.push(
		createCapacity('Women Literacy', '✍️', 'students', 40, GLOBAL_CITIES[7], 'In-Person')
	);
	capacities.push(
		createCapacity('Desert Greening', '🌱', 'hectares', 100, GLOBAL_CITIES[0], 'Outdoor')
	);
	capacities.push(
		createCapacity('Handicraft Export', '🏺', 'products', 500, GLOBAL_CITIES[1], 'Online')
	);
	capacities.push(
		createCapacity('Clean Cookstoves', '🔥', 'stoves', 300, GLOBAL_CITIES[2], 'In-Person')
	);
	capacities.push(
		createCapacity('Elephant Conservation', '🐘', 'hectares', 5000, GLOBAL_CITIES[3], 'Outdoor')
	);
	capacities.push(
		createCapacity('Fishing Rights', '🎣', 'licenses', 100, GLOBAL_CITIES[4], 'In-Person')
	);
	capacities.push(createCapacity('Mobile Clinics', '🚑', 'visits', 30, GLOBAL_CITIES[5], 'Mobile'));
	capacities.push(
		createCapacity('Storytelling Circle', '📖', 'sessions', 12, GLOBAL_CITIES[6], 'In-Person')
	);

	// Asia (15 capacities)
	capacities.push(
		createCapacity('Monsoon Preparedness', '🌧️', 'households', 200, GLOBAL_CITIES[8], 'In-Person')
	);
	capacities.push(
		createCapacity('Street Vendor Support', '🛒', 'vendors', 80, GLOBAL_CITIES[9], 'In-Person')
	);
	capacities.push(
		createCapacity('Textile Worker Rights', '👗', 'workers', 150, GLOBAL_CITIES[10], 'In-Person')
	);
	capacities.push(
		createCapacity('Mangrove Restoration', '🌊', 'hectares', 50, GLOBAL_CITIES[11], 'Outdoor')
	);
	capacities.push(
		createCapacity('Typhoon Shelter', '🏠', 'people', 300, GLOBAL_CITIES[12], 'In-Person')
	);
	capacities.push(
		createCapacity('Rice Cooperative', '🍚', 'kg', 5000, GLOBAL_CITIES[13], 'In-Person')
	);
	capacities.push(
		createCapacity('Coral Reef Protection', '🪸', 'hectares', 20, GLOBAL_CITIES[14], 'Outdoor')
	);
	capacities.push(
		createCapacity('Mountain Trails', '🥾', 'trails maintained', 10, GLOBAL_CITIES[15], 'Outdoor')
	);
	capacities.push(
		createCapacity('Tea Garden Collective', '🍵', 'kg', 1000, GLOBAL_CITIES[8], 'In-Person')
	);
	capacities.push(
		createCapacity('Flood Warning System', '📢', 'villages', 50, GLOBAL_CITIES[9], 'In-Person')
	);
	capacities.push(
		createCapacity('Spice Market', '🌶️', 'vendor spots', 40, GLOBAL_CITIES[10], 'In-Person')
	);
	capacities.push(
		createCapacity('Tiger Conservation', '🐅', 'hectares', 10000, GLOBAL_CITIES[11], 'Outdoor')
	);
	capacities.push(
		createCapacity('Coconut Processing', '🥥', 'kg', 2000, GLOBAL_CITIES[12], 'In-Person')
	);
	capacities.push(
		createCapacity('Meditation Center', '🧘', 'sessions', 20, GLOBAL_CITIES[13], 'In-Person')
	);
	capacities.push(
		createCapacity('Earthquake Recovery', '🏗️', 'buildings', 15, GLOBAL_CITIES[14], 'In-Person')
	);

	// Europe (8 capacities)
	capacities.push(
		createCapacity('Refugee Integration', '🤝', 'participants', 50, GLOBAL_CITIES[16], 'In-Person')
	);
	capacities.push(
		createCapacity('Zero Waste Workshop', '♻️', 'participants', 30, GLOBAL_CITIES[17], 'In-Person')
	);
	capacities.push(
		createCapacity('Community Garden', '🌻', 'plots', 40, GLOBAL_CITIES[18], 'Outdoor')
	);
	capacities.push(
		createCapacity('Bike Repair Collective', '🔧', 'repairs', 100, GLOBAL_CITIES[19], 'In-Person')
	);
	capacities.push(
		createCapacity('Climate Strike', '📢', 'participants', 5000, GLOBAL_CITIES[20], 'Outdoor')
	);
	capacities.push(
		createCapacity('Cooperative Housing', '🏘️', 'units', 20, GLOBAL_CITIES[21], 'In-Person')
	);
	capacities.push(
		createCapacity('Language Exchange', '🗣️', 'sessions', 25, GLOBAL_CITIES[16], 'Hybrid')
	);
	capacities.push(
		createCapacity('Permaculture Design', '🌿', 'projects', 12, GLOBAL_CITIES[17], 'In-Person')
	);

	// Central America & Caribbean (6 capacities)
	capacities.push(
		createCapacity('Hurricane Relief', '🌀', 'families', 100, GLOBAL_CITIES[22], 'In-Person')
	);
	capacities.push(
		createCapacity('Indigenous Crafts', '🎭', 'artisans', 30, GLOBAL_CITIES[23], 'In-Person')
	);
	capacities.push(
		createCapacity('Coral Restoration', '🪸', 'coral pieces', 1000, GLOBAL_CITIES[24], 'Outdoor')
	);
	capacities.push(
		createCapacity('Agroforestry', '🌳', 'hectares', 80, GLOBAL_CITIES[25], 'Outdoor')
	);
	capacities.push(
		createCapacity('Rainwater Harvesting', '💧', 'systems', 40, GLOBAL_CITIES[26], 'In-Person')
	);
	capacities.push(
		createCapacity('Traditional Dance', '💃', 'classes', 20, GLOBAL_CITIES[27], 'In-Person')
	);

	// Oceania (3 capacities)
	capacities.push(
		createCapacity('Aboriginal Art', '🎨', 'workshops', 15, GLOBAL_CITIES[28], 'In-Person')
	);
	capacities.push(
		createCapacity('Bushfire Recovery', '🔥', 'hectares', 500, GLOBAL_CITIES[29], 'Outdoor')
	);
	capacities.push(
		createCapacity('Māori Language', '📚', 'students', 25, GLOBAL_CITIES[30], 'In-Person')
	);

	// Middle East (3 capacities)
	capacities.push(
		createCapacity('Water Desalination', '💧', 'liters', 8000, GLOBAL_CITIES[31], 'In-Person')
	);
	capacities.push(
		createCapacity('Conflict Mediation', '☮️', 'sessions', 15, GLOBAL_CITIES[32], 'In-Person')
	);
	capacities.push(
		createCapacity('Historic Preservation', '🏛️', 'sites', 5, GLOBAL_CITIES[33], 'In-Person')
	);

	console.log(
		`[EXAMPLE] Created ${capacities.length} example capacities (${capacities.filter((c, i) => i < 56).length} in South America)`
	);
	return capacities;
}

/**
 * Populate both tree and capacities with example data
 */
export function populateWithFullExampleData(userPubKey: string): void {
	console.log('[EXAMPLE] Populating with full SDG example data...');

	// Populate tree
	const currentTree = get(userTree);
	if (currentTree) {
		const populatedTree = populateWithExampleData(currentTree);
		userTree.set(populatedTree);
	}

	// Populate capacities
	const exampleCapacities = createExampleCapacities(userPubKey);
	const currentCapacities = get(userCapacities) || {};
	const newCapacities: CapacitiesCollection = { ...currentCapacities };

	exampleCapacities.forEach((capacity) => {
		addCapacityToCollection(newCapacities, capacity);
	});

	userCapacities.set(newCapacities);

	console.log(
		`[EXAMPLE] Full example data populated: tree + ${exampleCapacities.length} capacities`
	);
}

// Expose to window for debugging
if (typeof window !== 'undefined') {
	(window as any).populateWithExampleData = populateWithExampleData;
	(window as any).createExampleCapacities = createExampleCapacities;
	(window as any).populateWithFullExampleData = populateWithFullExampleData;

	// Add wrapper that uses current userTree
	(window as any).populateCurrentTreeWithExampleData = () => {
		const currentTree = get(userTree);
		if (!currentTree) {
			console.error('[DEBUG] No userTree available to populate with example data');
			return null;
		}
		console.log('[DEBUG] Populating current userTree with example data');
		const populatedTree = populateWithExampleData(currentTree);
		userTree.set(populatedTree);
		return populatedTree;
	};

	// Add wrapper to populate everything
	(window as any).populateEverything = () => {
		// Try multiple ways to get the user pub key
		let userPubKey = (window as any).user?.is?.pub;

		// Fallback: try getting from stores
		if (!userPubKey) {
			const { userPub } = require('$lib/state/gun.svelte');
			const { get: getStore } = require('svelte/store');
			userPubKey = getStore(userPub);
		}

		if (!userPubKey) {
			console.error('[DEBUG] No user logged in. Please log in first.');
			return;
		}

		console.log(`[DEBUG] Populating data for user: ${userPubKey.substring(0, 20)}...`);
		populateWithFullExampleData(userPubKey);
	};

	console.log('[DEBUG] Example functions exposed to window:');
	console.log('  - populateWithExampleData(rootNode)');
	console.log('  - createExampleCapacities(userPubKey)');
	console.log('  - populateWithFullExampleData(userPubKey)');
	console.log('  - populateCurrentTreeWithExampleData()');
	console.log('  - populateEverything()');
}
