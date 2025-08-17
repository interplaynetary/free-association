import { driver, type DriveStep, type Side } from 'driver.js';

// Home page tour steps
const homeTourSteps: DriveStep[] = [
	{
		popover: {
			title: 'Welcome to Free Association! 🌱',
			description:
				"A way to recognize contributions and create abundance together. Let's see how it works.",
			side: 'bottom' as Side,
			align: 'center'
		}
	},
	{
		element: '.breadcrumbs',
		popover: {
			title: 'Your Recognition Tree',
			description:
				'This shows where you are in your tree. You start at yourself - the root of all your recognition.',
			side: 'bottom' as Side,
			align: 'start'
		}
	},
	{
		popover: {
			title: 'Recognition Works Like This',
			description:
				'You have your total recognition to give. You decide how much goes to each person who contributes to your life.',
			side: 'bottom' as Side,
			align: 'center'
		}
	},
	{
		element: '.add-button',
		popover: {
			title: 'Add Contributors',
			description:
				'Add people, projects, or things that contribute to your wellbeing. Each gets a portion of your recognition.',
			side: 'bottom' as Side,
			align: 'center'
		}
	},
	{
		element: '.parent',
		popover: {
			title: 'Your Canvas',
			description:
				'Here you build your recognition tree. Each node is a contribution. Set how much recognition each deserves.',
			side: 'top' as Side,
			align: 'center'
		}
	},
	{
		popover: {
			title: 'Mutual Recognition',
			description:
				'When you recognize someone AND they recognize you, that creates mutual recognition. It takes both sides.',
			side: 'bottom' as Side,
			align: 'center'
		}
	},
	{
		popover: {
			title: 'The Balance',
			description:
				'If you value them highly but they barely value you, your mutual recognition stays small. Both sides matter.',
			side: 'bottom' as Side,
			align: 'center'
		}
	},
	{
		element: '.bars',
		popover: {
			title: 'Recognition Visualization',
			description:
				'These bars show how recognition flows in your network. The left shows your recognition distribution, the right shows mutual recognition that determines capacity sharing.',
			side: 'left' as Side,
			align: 'center'
		}
	},
	{
		popover: {
			title: 'How Sharing Works',
			description:
				'People share their capacities with those they mutually recognize. The stronger the mutual recognition, the larger the share.',
			side: 'bottom' as Side,
			align: 'center'
		}
	},
	{
		popover: {
			title: 'Why Honesty Works',
			description:
				'If you give false recognition, you miss out on real connections. Honest recognition leads to genuine abundance.',
			side: 'bottom' as Side,
			align: 'center'
		}
	},
	{
		element: '.search-button',
		popover: {
			title: 'Navigate & Search',
			description:
				'Find anyone in your growing tree of recognition. As networks grow, navigation helps you stay connected.',
			side: 'bottom' as Side,
			align: 'center'
		}
	},
	{
		popover: {
			title: 'Ready to Begin',
			description:
				'Free Association grows through honest recognition. Start by adding someone who truly contributes to your life!',
			side: 'bottom' as Side,
			align: 'center'
		}
	}
];

// Inventory page tour steps
const inventoryTourSteps: DriveStep[] = [
	{
		popover: {
			title: 'Welcome to Your Inventory! 📊',
			description:
				'This is where you manage your capacities and view what others share with you. Let me show you around!',
			side: 'bottom' as Side,
			align: 'center'
		}
	},
	{
		element: '.breadcrumbs',
		popover: {
			title: 'Navigation',
			description:
				'You can always return to your recognition tree by clicking on your name in the breadcrumbs.',
			side: 'bottom' as Side,
			align: 'start'
		}
	},
	{
		element: '.inventory-button',
		popover: {
			title: 'Inventory Access',
			description:
				'Click this button anytime to return to your inventory from anywhere in the app.',
			side: 'bottom' as Side,
			align: 'center'
		}
	},
	{
		element: '.mt-8',
		popover: {
			title: 'Network Map',
			description:
				'The map shows the geographical distribution of your network. You can see where your mutual recognition connections are located.',
			side: 'bottom' as Side,
			align: 'center'
		}
	},
	{
		element: 'h2',
		popover: {
			title: 'Your Capacities Section',
			description:
				'This section shows all the capacities you offer to your network. Capacities are things you can provide - like time, skills, or resources.',
			side: 'top' as Side,
			align: 'center'
		}
	},
	{
		element: '.capacities-list',
		popover: {
			title: 'Capacities List',
			description:
				"Here you can see all your current capacities. Each capacity card shows what you offer, when you're available, and where.",
			side: 'top' as Side,
			align: 'center'
		}
	},
	{
		element: '.add-btn',
		popover: {
			title: 'Add New Capacity',
			description:
				'Click this button to create a new capacity. Define what you can offer, set your availability schedule, and specify locations.',
			side: 'left' as Side,
			align: 'center'
		}
	},
	{
		popover: {
			title: 'Your Shares Section',
			description:
				'This section shows what others are sharing with you based on your mutual recognition. The stronger your mutual recognition, the more they share.',
			side: 'top' as Side,
			align: 'center',
			onNextClick: () => {
				// Scroll to shares section
				const sharesHeader = Array.from(document.querySelectorAll('h2')).find((h) =>
					h.textContent?.includes('Your Shares')
				);
				if (sharesHeader) {
					sharesHeader.scrollIntoView({ behavior: 'smooth', block: 'start' });
				}
				setTimeout(() => {
					driverObj.moveNext();
				}, 500);
			}
		}
	},
	{
		element: '.filter-bar',
		popover: {
			title: 'Search and Filter',
			description:
				'Use these controls to search for specific shares, filter by provider, and sort by different criteria to find what you need.',
			side: 'bottom' as Side,
			align: 'center'
		}
	},
	{
		element: '.search-input',
		popover: {
			title: 'Search Shares',
			description:
				"Type here to search for specific capacities or providers. This helps you quickly find what you're looking for in your network.",
			side: 'bottom' as Side,
			align: 'center'
		}
	},
	{
		element: '.shares-list',
		popover: {
			title: 'Available Shares',
			description:
				'Here you can see all the capacities that others are sharing with you. Click on any share to view details, availability, and location information.',
			side: 'top' as Side,
			align: 'center'
		}
	},
	{
		popover: {
			title: 'The Network Effect',
			description:
				"As you build stronger mutual recognition relationships, you'll see more capacities shared with you and your own capacities will reach more people.",
			side: 'bottom' as Side,
			align: 'center'
		}
	},
	{
		popover: {
			title: 'Ready to Explore!',
			description:
				'Your inventory grows as your network grows. Start by creating your first capacity or exploring what others are sharing with you!',
			side: 'bottom' as Side,
			align: 'center'
		}
	}
];

// Global reference to driver instance for async operations
let driverObj: any;

// Function to detect current page
function getCurrentPage(): string {
	if (typeof window === 'undefined') return 'home';

	const pathname = window.location.pathname;
	console.log('[TOUR DEBUG] Full pathname:', pathname);

	// Simple path detection - no base path manipulation needed
	if (pathname.includes('/inventory')) {
		return 'inventory';
	} else if (pathname.includes('/contacts')) {
		return 'contacts';
	} else {
		return 'home';
	}
}

// Helper function to check if element exists
function elementExists(selector: string): boolean {
	return document.querySelector(selector) !== null;
}

// Main tour function that detects page and starts appropriate tour
export function startTour() {
	const currentPage = getCurrentPage();

	// Debug logging
	console.log('[TOUR] Current page detected:', currentPage);
	console.log('[TOUR] Window location:', window.location.pathname);

	let steps;
	let doneBtnText;

	switch (currentPage) {
		case 'inventory':
			console.log('[TOUR] Starting inventory tour');
			// Filter out steps for elements that don't exist
			steps = inventoryTourSteps.filter((step) => {
				if (step.element && !elementExists(step.element)) {
					console.log('[TOUR] Skipping step for missing element:', step.element);
					return false;
				}
				return true;
			});
			doneBtnText = 'Start Managing!';
			break;
		case 'home':
		default:
			console.log('[TOUR] Starting home tour');
			// Filter out steps for elements that don't exist
			steps = homeTourSteps.filter((step) => {
				if (step.element && !elementExists(step.element)) {
					console.log('[TOUR] Skipping step for missing element:', step.element);
					return false;
				}
				return true;
			});
			doneBtnText = 'Start Creating!';
			break;
	}

	console.log('[TOUR] Final steps count:', steps.length);

	driverObj = driver({
		showProgress: true,
		nextBtnText: 'Next →',
		prevBtnText: '← Back',
		doneBtnText,
		steps
	});

	driverObj.drive();
}

// Legacy function for backward compatibility
export function startHomeTour() {
	driverObj = driver({
		showProgress: true,
		nextBtnText: 'Next →',
		prevBtnText: '← Back',
		doneBtnText: 'Start Creating!',
		steps: homeTourSteps
	});

	driverObj.drive();
}

// Specific function for inventory tour
export function startInventoryTour() {
	driverObj = driver({
		showProgress: true,
		nextBtnText: 'Next →',
		prevBtnText: '← Back',
		doneBtnText: 'Start Managing!',
		steps: inventoryTourSteps
	});

	driverObj.drive();
}

// Debug function to test page detection and element availability
export function debugPageDetection() {
	console.log('[DEBUG] Window location:', window.location);
	console.log('[DEBUG] Pathname:', window.location.pathname);
	console.log('[DEBUG] Detected page:', getCurrentPage());

	// Test both tours
	console.log('[DEBUG] Home tour steps:', homeTourSteps.length);
	console.log('[DEBUG] Inventory tour steps:', inventoryTourSteps.length);

	// Check which elements exist on current page
	const currentPage = getCurrentPage();
	const steps = currentPage === 'inventory' ? inventoryTourSteps : homeTourSteps;

	console.log('[DEBUG] Element availability check:');
	steps.forEach((step, index) => {
		if (step.element) {
			const exists = elementExists(step.element);
			console.log(
				`[DEBUG] Step ${index + 1}: ${step.element} - ${exists ? '✅ Found' : '❌ Missing'}`
			);
		} else {
			console.log(`[DEBUG] Step ${index + 1}: No element selector (generic popover) - ✅ OK`);
		}
	});
}
