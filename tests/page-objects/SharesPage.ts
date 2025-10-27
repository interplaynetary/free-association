import { type Page, type Locator } from '@playwright/test';
import { BasePage } from './BasePage';

/**
 * Page Object for Network Shares and Desire Expression
 * Handles viewing network capacities and expressing desires
 */
export class SharesPage extends BasePage {
	// Selectors
	private readonly selectors = {
		// Navigation
		sharesViewButton: '[data-testid="shares-view"], button:has-text("Shares")',
		inventoryViewButton: '[data-testid="inventory-view"], button:has-text("Inventory")',
		
		// Shares list
		sharesList: '[data-testid="shares-list"], .shares-container',
		shareCard: '[data-testid="share-card"], .share-item',
		
		// Network capacities
		networkCapacitiesList: '[data-testid="network-capacities"]',
		networkCapacityCard: '[data-testid="network-capacity-card"]',
		
		// Desire/composition
		desireInput: '[data-testid="desire-input"], input[name="desiredQuantity"]',
		expressDesireButton: '[data-testid="express-desire"], button:has-text("Express Desire")',
		desireAmount: '[data-testid="desire-amount"]',
		
		// Slot composition
		slotCompositionItem: '[data-testid="slot-composition"]',
		compositionQuantityInput: '[data-testid="composition-quantity"]',
		
		// Allocation info
		allocatedQuantity: '[data-testid="allocated-quantity"]',
		availableQuantity: '[data-testid="available-quantity"]',
		allocationShare: '[data-testid="allocation-share"]',
		allocationStatus: '[data-testid="allocation-status"]',
		
		// Tier indicators
		mutualTierIndicator: '[data-testid="mutual-tier"], .tier-mutual',
		nonMutualTierIndicator: '[data-testid="non-mutual-tier"], .tier-non-mutual',
		
		// Map view (for browsing capacities)
		mapContainer: '[data-testid="map-container"], .maplibregl-map',
		mapMarker: '[data-testid="map-marker"], .maplibregl-marker',
		mapSidePanel: '[data-testid="map-side-panel"]',
		
		// Filters
		filterByType: '[data-testid="filter-type"], select[name="filterType"]',
		filterByLocation: '[data-testid="filter-location"], select[name="filterLocation"]',
		filterByTime: '[data-testid="filter-time"], select[name="filterTime"]',
	};

	constructor(page: Page) {
		super(page);
	}

	/**
	 * Navigate to shares view
	 */
	async gotoShares(): Promise<void> {
		await this.goto('/');
		await this.wait(500);
		
		// Try to switch to shares view
		try {
			await this.page.locator(this.selectors.sharesViewButton).click();
			await this.wait(500);
		} catch {
			// Shares might be part of inventory view
			try {
				await this.page.locator(this.selectors.inventoryViewButton).click();
				await this.wait(500);
			} catch {
				// Already on the right view
			}
		}
	}

	/**
	 * Check if shares view is visible
	 */
	async isSharesViewVisible(): Promise<boolean> {
		return await this.isVisible(this.selectors.sharesList) || 
		       await this.isVisible(this.selectors.networkCapacitiesList);
	}

	/**
	 * Get all network capacity cards
	 */
	async getNetworkCapacities(): Promise<Locator[]> {
		return await this.page.locator(this.selectors.networkCapacityCard).all();
	}

	/**
	 * Get a network capacity by name
	 */
	async getNetworkCapacityByName(name: string): Promise<Locator | null> {
		try {
			return this.page.locator(this.selectors.networkCapacityCard)
				.filter({ hasText: name })
				.first();
		} catch {
			return null;
		}
	}

	/**
	 * Express desire for a capacity
	 */
	async expressDesire(capacityName: string, quantity: number): Promise<void> {
		// Find and click on the capacity
		const capacity = await this.getNetworkCapacityByName(capacityName);
		if (capacity) {
			await capacity.click();
			await this.wait(300);
		}
		
		// Enter desired quantity
		const desireInput = this.page.locator(this.selectors.desireInput).first();
		await desireInput.fill(quantity.toString());
		await this.wait(300);
		
		// Click express desire button if it exists
		try {
			await this.page.locator(this.selectors.expressDesireButton).first().click();
			await this.wait(500);
		} catch {
			// Desire might be saved automatically
			await this.page.keyboard.press('Enter');
			await this.wait(500);
		}
	}

	/**
	 * Get allocated quantity for a capacity
	 */
	async getAllocatedQuantity(capacityName: string): Promise<number> {
		const capacity = await this.getNetworkCapacityByName(capacityName);
		if (capacity) {
			await capacity.click();
			await this.wait(300);
			
			try {
				const text = await this.getTextContent(this.selectors.allocatedQuantity);
				const match = text.match(/(\d+(?:\.\d+)?)/);
				return match ? parseFloat(match[1]) : 0;
			} catch {
				return 0;
			}
		}
		return 0;
	}

	/**
	 * Get available quantity for a capacity
	 */
	async getAvailableQuantity(capacityName: string): Promise<number> {
		const capacity = await this.getNetworkCapacityByName(capacityName);
		if (capacity) {
			await capacity.click();
			await this.wait(300);
			
			try {
				const text = await this.getTextContent(this.selectors.availableQuantity);
				const match = text.match(/(\d+(?:\.\d+)?)/);
				return match ? parseFloat(match[1]) : 0;
			} catch {
				return 0;
			}
		}
		return 0;
	}

	/**
	 * Check if allocation is in mutual tier
	 */
	async isInMutualTier(capacityName: string): Promise<boolean> {
		const capacity = await this.getNetworkCapacityByName(capacityName);
		if (capacity) {
			await capacity.click();
			await this.wait(300);
			
			return await this.isVisible(this.selectors.mutualTierIndicator);
		}
		return false;
	}

	/**
	 * Get allocation share percentage
	 */
	async getAllocationShare(capacityName: string): Promise<number> {
		const capacity = await this.getNetworkCapacityByName(capacityName);
		if (capacity) {
			await capacity.click();
			await this.wait(300);
			
			try {
				const text = await this.getTextContent(this.selectors.allocationShare);
				const match = text.match(/(\d+(?:\.\d+)?)\s*%/);
				return match ? parseFloat(match[1]) : 0;
			} catch {
				return 0;
			}
		}
		return 0;
	}

	/**
	 * Browse network capacities on map
	 */
	async browseCapacitiesOnMap(): Promise<void> {
		// Switch to map view
		await this.page.locator('[data-testid="map-view"], button:has-text("Map")').click();
		await this.wait(1000); // Map takes time to load
		
		// Wait for map to be visible
		await this.waitForElement(this.selectors.mapContainer);
	}

	/**
	 * Get map markers count
	 */
	async getMapMarkersCount(): Promise<number> {
		await this.wait(1000); // Wait for markers to render
		return await this.page.locator(this.selectors.mapMarker).count();
	}

	/**
	 * Click on a map marker
	 */
	async clickMapMarker(index: number = 0): Promise<void> {
		const markers = await this.page.locator(this.selectors.mapMarker).all();
		if (markers.length > index) {
			await markers[index].click();
			await this.wait(500);
		}
	}

	/**
	 * Check if map side panel is visible
	 */
	async isMapSidePanelVisible(): Promise<boolean> {
		return await this.isVisible(this.selectors.mapSidePanel);
	}

	/**
	 * Filter capacities by type
	 */
	async filterByType(type: string): Promise<void> {
		try {
			await this.page.locator(this.selectors.filterByType).selectOption(type);
			await this.wait(500);
		} catch {
			// Filter might not be available
		}
	}

	/**
	 * Filter capacities by location
	 */
	async filterByLocation(location: string): Promise<void> {
		try {
			await this.page.locator(this.selectors.filterByLocation).selectOption(location);
			await this.wait(500);
		} catch {
			// Filter might not be available
		}
	}

	/**
	 * Get desire/need for a specific slot
	 */
	async getDesireForSlot(capacityName: string, slotIndex: number = 0): Promise<number> {
		const capacity = await this.getNetworkCapacityByName(capacityName);
		if (capacity) {
			await capacity.click();
			await this.wait(300);
			
			const compositions = await this.page.locator(this.selectors.slotCompositionItem).all();
			if (compositions.length > slotIndex) {
				const input = compositions[slotIndex].locator(this.selectors.compositionQuantityInput);
				const value = await input.inputValue();
				return parseFloat(value) || 0;
			}
		}
		return 0;
	}

	/**
	 * Update desire for a specific slot
	 */
	async updateDesireForSlot(
		capacityName: string, 
		slotIndex: number, 
		quantity: number
	): Promise<void> {
		const capacity = await this.getNetworkCapacityByName(capacityName);
		if (capacity) {
			await capacity.click();
			await this.wait(300);
			
			const compositions = await this.page.locator(this.selectors.slotCompositionItem).all();
			if (compositions.length > slotIndex) {
				const input = compositions[slotIndex].locator(this.selectors.compositionQuantityInput);
				await input.fill(quantity.toString());
				await this.wait(300);
			}
		}
	}
}

