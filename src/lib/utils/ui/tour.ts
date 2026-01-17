import { driver, type DriveStep, type Side } from 'driver.js';
import { globalState } from '$lib/global.svelte';

console.log('[TRACE] src/lib/utils/ui/tour.ts: <module scope>');

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
		element: '.view-content',
		popover: {
			title: 'Your Canvas',
			description:
				'Here you build your recognition tree. Each node is a contribution. Set how much recognition each deserves.',
			side: 'top' as Side,
			align: 'center'
		}
	},
	{
		element: '.treemap-node',
		popover: {
			title: 'Your First Node',
			description:
				'This is a node in your recognition tree. To edit its name, you first need to enable text editing mode.',
			side: 'bottom' as Side,
			align: 'center'
		}
	},
	{
		element: '.edit-button',
		popover: {
			title: 'Enable Text Editing',
			description:
				'Click this edit button first to enable text editing mode. When active, you can click on any node text to rename it.',
			side: 'top' as Side,
			align: 'center'
		}
	},
	{
		element: '.node-title',
		popover: {
			title: 'Edit Node Name',
			description:
				'Now with edit mode enabled, click on this text to rename the node. Give it a meaningful name that represents what this contribution is about.',
			side: 'bottom' as Side,
			align: 'center',
			onNextClick: () => {
				// Wait for user to potentially interact with the text
				setTimeout(() => {
					driverObj.moveNext();
				}, 1000);
			}
		}
	},
	{
		element: '.add-contributor-button',
		popover: {
			title: 'Add a Contributor',
			description:
				'Try clicking this + button to add someone who contributes to this node. This turns it into a contribution that recognizes specific people.',
			side: 'left' as Side,
			align: 'center',
			onNextClick: () => {
				// Wait for user to potentially interact with the contributor button
				setTimeout(() => {
					driverObj.moveNext();
				}, 1000);
			}
		}
	},
	{
		element: '.manual-fulfillment-slider',
		popover: {
			title: 'Set Fulfillment Level',
			description:
				'If you see a slider like this, you can drag it to set how fulfilled you feel by this contribution. This affects how much you value it.',
			side: 'top' as Side,
			align: 'center',
			onNextClick: () => {
				// Wait for user to potentially interact with the slider
				setTimeout(() => {
					driverObj.moveNext();
				}, 1000);
			}
		}
	},
	{
		element: '.treemap-node',
		popover: {
			title: 'Navigate Deeper',
			description:
				'You can click on any node (not on buttons/sliders) to zoom into it and create sub-nodes. This lets you break down contributions into smaller parts.',
			side: 'bottom' as Side,
			align: 'center'
		}
	},
	{
		element: '.bars',
		popover: {
			title: 'Contribution Recognition',
			description:
				'This bar shows how much recognition of contribution you attribute to each entity in your tree. Contributions towards goals that are more important to will receive a larger share! Allocation of capacities is proportional to your recognition of contribution!',
			side: 'left' as Side,
			align: 'center'
		}
	},
	{
		element: '.toolbar',
		popover: {
			title: 'Your Toolbar',
			description:
				'This toolbar at the bottom contains all the tools you need to manage your recognition tree. You already learned about the edit button - let me show you the other tools.',
			side: 'top' as Side,
			align: 'center'
		}
	},
	{
		element: '.add-button',
		popover: {
			title: 'Add New Nodes',
			description:
				'Click this button to add new nodes to your current location in the tree. Each new node represents a contribution or area of recognition.',
			side: 'top' as Side,
			align: 'center'
		}
	},
	{
		popover: {
			title: 'How Sharing Works',
			description:
				'People share their capacities with those they mutually recognize. The stronger the recognition, the larger the share.',
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
			side: 'top' as Side,
			align: 'center'
		}
	},
	{
		element: '.delete-button',
		popover: {
			title: 'Delete Mode',
			description:
				'Click this to enter delete mode. Then click on any node to delete it and all its children. Be careful!',
			side: 'top' as Side,
			align: 'center'
		}
	},
	{
		element: '.recompose-button',
		popover: {
			title: 'Recompose Mode',
			description:
				'This mode lets you drag nodes around to reorganize your tree. Click to enable, then drag nodes to move them.',
			side: 'top' as Side,
			align: 'center'
		}
	},
	{
		element: '.forest-button',
		popover: {
			title: 'Forest Subtrees',
			description:
				'Access subtrees from your network contributors. You can add their recognition structures to your own tree.',
			side: 'top' as Side,
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

// Map view tour steps
const mapTourSteps: DriveStep[] = [
	{
		popover: {
			title: 'Welcome to the Map View! 🌍',
			description:
				'This interactive map shows the geographical distribution of your recognition network.',
			side: 'bottom' as Side,
			align: 'center'
		}
	},
	{
		element: '.view-switcher-container',
		popover: {
			title: 'View Switchers',
			description:
				'Use these buttons to switch between Tree, Map, and Inventory views.',
			side: 'top' as Side,
			align: 'center'
		}
	},
	{
		popover: {
			title: 'Network Visualization',
			description:
				'The map displays markers for network members and their shared capacities. You can see where your recognition connections are located geographically.',
			side: 'bottom' as Side,
			align: 'center'
		}
	},
	{
		popover: {
			title: 'Ready to Explore!',
			description:
				'Zoom and pan the map to explore your network. As your contribution grows, so does your geographical reach!',
			side: 'bottom' as Side,
			align: 'center'
		}
	}
];

// Inventory view tour steps
const inventoryTourSteps: DriveStep[] = [
	{
		popover: {
			title: 'Welcome to Your Inventory! 📊',
			description:
				'This is where you manage your capacities (what you offer) and view what others share with you (needs).',
			side: 'bottom' as Side,
			align: 'center'
		}
	},
	{
		element: '.view-switcher-container',
		popover: {
			title: 'View Switchers',
			description:
				'Use this button to switch between Tree, Map, and Inventory views. Tap to cycle or hold to see the menu.',
			side: 'top' as Side,
			align: 'start'
		}
	},
	{
		element: '.inventory-type-toggle',
		popover: {
			title: 'Needs vs Capacities',
			description:
				'Switch between "Needs" (what you lack) and "Capacities" (what you offer). Your inventory shows slots for each type.',
			side: 'left' as Side,
			align: 'center'
		}
	},
	{
		element: '.inventory-draft-controls',
		popover: {
			title: 'Create & Draft',
			description:
				'This is your drafting area. Select a type, enter a name, and set quantities to create new slots.',
			side: 'top' as Side,
			align: 'center'
		}
	},
	{
		element: '.draft-input-name',
		popover: {
			title: 'Search & Create',
			description:
				'Type here to name a new slot OR to search your existing inventory. It serves both purposes!',
			side: 'top' as Side,
			align: 'center'
		}
	},
	{
		element: '.slots-list',
		popover: {
			title: 'Your Slots',
			description:
				'Here are your slots. Each card shows details and allocations. Click a card to edit it or view who is receiving/providing.',
			side: 'top' as Side,
			align: 'center'
		}
	},
	{
		popover: {
			title: 'Ready to Manage!',
			description:
				"Keep your inventory up to date to ensure the network knows what you need and what you can give. Honesty powers the flow!",
			side: 'bottom' as Side,
			align: 'center'
		}
	}
];

// Global reference to driver instance for async operations
let driverObj: any;

// LocalStorage key for saving tour progress
const TOUR_PROGRESS_KEY = 'free-association-tour-progress';

// Save tour progress to localStorage
function saveTourProgress(view: string, stepIndex: number) {
	console.log('[TRACE] [ENTER] src/lib/utils/ui/tour.ts: saveTourProgress', { view, stepIndex });
	if (typeof window === 'undefined') return;

	try {
		const progress = {
			view,
			stepIndex,
			timestamp: Date.now()
		};
		localStorage.setItem(TOUR_PROGRESS_KEY, JSON.stringify(progress));
		console.log('[TOUR] Saved progress:', progress);
	} catch (error) {
		console.warn('[TOUR] Failed to save progress:', error);
	}
	console.log('[TRACE] [EXIT] src/lib/utils/ui/tour.ts: saveTourProgress');
}

// Load tour progress from localStorage
function loadTourProgress(): { view: string; stepIndex: number } | null {
	console.log('[TRACE] [ENTER] src/lib/utils/ui/tour.ts: loadTourProgress');
	if (typeof window === 'undefined') return null;

	try {
		const saved = localStorage.getItem(TOUR_PROGRESS_KEY);
		if (!saved) return null;

		const progress = JSON.parse(saved);
		console.log('[TOUR] Loaded progress:', progress);

		// Optional: Clear progress if it's older than 7 days
		const weekInMs = 7 * 24 * 60 * 60 * 1000;
		if (Date.now() - progress.timestamp > weekInMs) {
			console.log('[TOUR] Progress expired, starting fresh');
			clearTourProgress();
			return null;
		}

		return progress;
	} catch (error) {
		console.warn('[TOUR] Failed to load progress:', error);
		return null;
	} finally {
		console.log('[TRACE] [EXIT] src/lib/utils/ui/tour.ts: loadTourProgress');
	}
}

// Clear tour progress from localStorage
function clearTourProgress() {
	console.log('[TRACE] [ENTER] src/lib/utils/ui/tour.ts: clearTourProgress');
	if (typeof window === 'undefined') return;

	try {
		localStorage.removeItem(TOUR_PROGRESS_KEY);
		console.log('[TOUR] Cleared progress');
	} catch (error) {
		console.warn('[TOUR] Failed to clear progress:', error);
	}
	console.log('[TRACE] [EXIT] src/lib/utils/ui/tour.ts: clearTourProgress');
}

// Function to detect current view
function getCurrentView(): string {
	console.log('[TRACE] [ENTER] src/lib/utils/ui/tour.ts: getCurrentView');
	if (typeof window === 'undefined') return 'tree';

	// Check the current view from globalState
	const currentView = globalState.currentView;
	console.log('[TOUR DEBUG] Current view:', currentView);

	// Map view types to tour types
	switch (currentView) {
		case 'inventory':
			return 'inventory';
		case 'map':
			return 'map';
		case 'tree':
		default:
			return 'tree';
	}
}

// Helper function to check if element exists
function elementExists(selector: string | Element | (() => Element)): boolean {
	if (typeof selector === 'string') {
		return document.querySelector(selector) !== null;
	} else if (typeof selector === 'function') {
		try {
			const element = selector();
			return element !== null;
		} catch {
			return false;
		}
	} else {
		return selector !== null;
	}
}

// Main tour function that detects view and starts appropriate tour
export function startTour() {
	console.log('[TRACE] [ENTER] src/lib/utils/ui/tour.ts: startTour');
	const currentView = getCurrentView();

	// Debug logging
	console.log('[TOUR] Current view detected:', currentView);
	console.log('[TOUR] Global state view:', globalState.currentView);

	// Check for saved progress
	const savedProgress = loadTourProgress();
	let startIndex = 0;

	// If we have saved progress for this view, resume from that step
	if (savedProgress && savedProgress.view === currentView) {
		startIndex = savedProgress.stepIndex;
		console.log('[TOUR] Resuming from step:', startIndex);
	}

	let steps;
	let doneBtnText;

	switch (currentView) {
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
		case 'map':
			console.log('[TOUR] Starting map tour');
			// Filter out steps for elements that don't exist
			steps = mapTourSteps.filter((step) => {
				if (step.element && !elementExists(step.element)) {
					console.log('[TOUR] Skipping step for missing element:', step.element);
					return false;
				}
				return true;
			});
			doneBtnText = 'Start Exploring!';
			break;
		case 'tree':
		default:
			console.log('[TOUR] Starting tree tour');
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

	// Ensure startIndex is within bounds
	if (startIndex >= steps.length) {
		startIndex = 0;
	}

	driverObj = driver({
		showProgress: true,
		nextBtnText: 'Next →',
		prevBtnText: '← Back',
		doneBtnText,
		steps,
		onNextClick: (element, step, options) => {
			// Save progress when moving to next step
			const currentStepIndex = options.state.activeIndex || 0;
			saveTourProgress(currentView, currentStepIndex + 1);

			// Call the default next behavior
			driverObj.moveNext();
		},
		onPrevClick: (element, step, options) => {
			// Save progress when moving to previous step
			const currentStepIndex = options.state.activeIndex || 0;
			saveTourProgress(currentView, Math.max(0, currentStepIndex - 1));

			// Call the default previous behavior
			driverObj.movePrevious();
		},
		onDestroyStarted: (element, step, options) => {
			// Check if tour was completed (at last step)
			const currentStepIndex = options.state.activeIndex || 0;
			if (currentStepIndex === steps.length - 1) {
				// Tour completed, clear progress
				console.log('[TOUR] Tour completed, clearing progress');
				clearTourProgress();
			} else {
				// Tour exited early, save progress for resumption
				console.log('[TOUR] Tour exited at step:', currentStepIndex);
				saveTourProgress(currentView, currentStepIndex);
			}

			// Destroy the driver instance
			if (driverObj) {
				driverObj.destroy();
			}
		}
	});

	// Start from saved index or beginning
	driverObj.drive(startIndex);
	console.log('[TRACE] [EXIT] src/lib/utils/ui/tour.ts: startTour');
}

// Legacy function for backward compatibility
export function startHomeTour() {
	console.log('[TRACE] [ENTER] src/lib/utils/ui/tour.ts: startHomeTour');
	driverObj = driver({
		showProgress: true,
		nextBtnText: 'Next →',
		prevBtnText: '← Back',
		doneBtnText: 'Start Creating!',
		steps: homeTourSteps
	});

	driverObj.drive();
	console.log('[TRACE] [EXIT] src/lib/utils/ui/tour.ts: startHomeTour');
}

// Specific function for map tour
export function startMapTour() {
	console.log('[TRACE] [ENTER] src/lib/utils/ui/tour.ts: startMapTour');
	driverObj = driver({
		showProgress: true,
		nextBtnText: 'Next →',
		prevBtnText: '← Back',
		doneBtnText: 'Start Exploring!',
		steps: mapTourSteps
	});

	driverObj.drive();
	console.log('[TRACE] [EXIT] src/lib/utils/ui/tour.ts: startMapTour');
}

// Specific function for inventory tour
export function startInventoryTour() {
	console.log('[TRACE] [ENTER] src/lib/utils/ui/tour.ts: startInventoryTour');
	driverObj = driver({
		showProgress: true,
		nextBtnText: 'Next →',
		prevBtnText: '← Back',
		doneBtnText: 'Start Managing!',
		steps: inventoryTourSteps
	});

	driverObj.drive();
	console.log('[TRACE] [EXIT] src/lib/utils/ui/tour.ts: startInventoryTour');
}

// Export function to manually reset tour progress (useful for starting over)
export function resetTourProgress() {
	console.log('[TRACE] [ENTER] src/lib/utils/ui/tour.ts: resetTourProgress');
	clearTourProgress();
	console.log('[TOUR] Tour progress manually reset');
	console.log('[TRACE] [EXIT] src/lib/utils/ui/tour.ts: resetTourProgress');
}

// Debug function to test view detection and element availability
export function debugViewDetection() {
	console.log('[TRACE] [ENTER] src/lib/utils/ui/tour.ts: debugViewDetection');
	console.log('[DEBUG] Window location:', window.location);
	console.log('[DEBUG] Global state view:', globalState.currentView);
	console.log('[DEBUG] Detected view:', getCurrentView());

	// Test all tours
	console.log('[DEBUG] Tree tour steps:', homeTourSteps.length);
	console.log('[DEBUG] Map tour steps:', mapTourSteps.length);
	console.log('[DEBUG] Inventory tour steps:', inventoryTourSteps.length);

	// Check which elements exist on current view
	const currentView = getCurrentView();
	let steps;
	if (currentView === 'inventory') {
		steps = inventoryTourSteps;
	} else if (currentView === 'map') {
		steps = mapTourSteps;
	} else {
		steps = homeTourSteps;
	}

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
	console.log('[TRACE] [EXIT] src/lib/utils/ui/tour.ts: debugViewDetection');
}

// Comprehensive verification function for tour elements
export function verifyTourElements() {
	console.log('[TRACE] [ENTER] src/lib/utils/ui/tour.ts: verifyTourElements');
	console.log('[TOUR-VERIFY] Starting comprehensive tour element verification...');

	const currentView = getCurrentView();
	let steps;
	if (currentView === 'inventory') {
		steps = inventoryTourSteps;
	} else if (currentView === 'map') {
		steps = mapTourSteps;
	} else {
		steps = homeTourSteps;
	}

	// Define expected elements for each view
	const expectedElements: Record<string, string[]> = {
		tree: [
			'.breadcrumbs',
			'.view-content',
			'.treemap-node',
			'.node-title',
			'.add-contributor-button',
			'.manual-fulfillment-slider',
			'.bars',
			'.toolbar',
			'.add-button',
			'.edit-button',
			'.search-button',
			'.delete-button',
			'.recompose-button',
			'.forest-button',
			'.view-switcher-container'
		],
		map: [
			'.breadcrumbs',
			'.view-switcher-container',
			'.toolbar',
			'.view-content'
		],
		// Inventory view tour steps
		inventory: [
			'.breadcrumbs',
			'.view-switcher-container',
			'.inventory-type-toggle',
			'.inventory-draft-controls',
			'.draft-input-name',
			'.slots-list'
		]
	};

	const elementsToCheck = expectedElements[currentView] || expectedElements.tree;

	console.log(`[TOUR-VERIFY] Checking ${elementsToCheck.length} expected elements for ${currentView} view:`);

	const results = {
		found: [] as string[],
		missing: [] as string[],
		total: elementsToCheck.length
	};

	elementsToCheck.forEach((selector: string) => {
		const exists = elementExists(selector);
		if (exists) {
			results.found.push(selector);
			console.log(`[TOUR-VERIFY] ✅ ${selector}`);
		} else {
			results.missing.push(selector);
			console.log(`[TOUR-VERIFY] ❌ ${selector}`);
		}
	});

	console.log(`[TOUR-VERIFY] Summary: ${results.found.length}/${results.total} elements found`);

	if (results.missing.length > 0) {
		console.warn(`[TOUR-VERIFY] Missing elements:`, results.missing);
		console.log(`[TOUR-VERIFY] Note: Some elements may not be visible until certain conditions are met (e.g., nodes exist, user is logged in, etc.)`);
	}

	// Check tour steps specifically
	console.log(`[TOUR-VERIFY] Checking ${steps.length} tour steps:`);
	let stepsMissing = 0;

	steps.forEach((step, index) => {
		if (step.element) {
			const exists = elementExists(step.element);
			if (!exists) {
				stepsMissing++;
				console.log(`[TOUR-VERIFY] Step ${index + 1} targets missing element: ${step.element}`);
			}
		}
	});

	console.log(`[TOUR-VERIFY] Tour readiness: ${steps.length - stepsMissing}/${steps.length} steps have valid targets`);

	console.log('[TRACE] [EXIT] src/lib/utils/ui/tour.ts: verifyTourElements');
	return {
		view: currentView,
		elements: results,
		tourSteps: {
			total: steps.length,
			valid: steps.length - stepsMissing,
			missing: stepsMissing
		}
	};
}
