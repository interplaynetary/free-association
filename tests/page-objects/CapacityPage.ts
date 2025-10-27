import { type Page, type Locator } from '@playwright/test';
import { BasePage } from './BasePage';

/**
 * Page Object for Capacity Declaration and Management
 * Handles creating capacities, adding availability slots, and configuring filters
 */
export class CapacityPage extends BasePage {
	// Selectors
	private readonly selectors = {
		// Navigation
		inventoryViewButton: '[data-testid="inventory-view"], button:has-text("Inventory")',
		capacitiesTab: '[data-testid="capacities-tab"], button:has-text("Capacities")',
		
		// Capacity list
		capacitiesList: '[data-testid="capacities-list"], .capacities-container',
		capacityCard: '[data-testid="capacity-card"], .capacity-item',
		newCapacityButton: '[data-testid="new-capacity"], button:has-text("New Capacity")',
		
		// Capacity form
		capacityNameInput: '[data-testid="capacity-name"], input[name="name"]',
		capacityEmojiInput: '[data-testid="capacity-emoji"], input[name="emoji"]',
		capacityUnitInput: '[data-testid="capacity-unit"], input[name="unit"]',
		capacityDescriptionInput: '[data-testid="capacity-description"], textarea[name="description"], input[name="description"]',
		saveCapacityButton: '[data-testid="save-capacity"], button:has-text("Save")',
		deleteCapacityButton: '[data-testid="delete-capacity"], button:has-text("Delete")',
		
		// Availability slots
		addSlotButton: '[data-testid="add-slot"], button:has-text("Add Slot")',
		slotsList: '[data-testid="slots-list"]',
		slotItem: '[data-testid="slot-item"]',
		
		// Slot configuration
		slotQuantityInput: '[data-testid="slot-quantity"], input[name="quantity"]',
		slotNeedTypeSelect: '[data-testid="slot-need-type"], select[name="needType"]',
		slotStartDateInput: '[data-testid="slot-start-date"], input[name="startDate"]',
		slotEndDateInput: '[data-testid="slot-end-date"], input[name="endDate"]',
		slotRecurrenceSelect: '[data-testid="slot-recurrence"], select[name="recurrence"]',
		
		// Location
		slotLocationTypeSelect: '[data-testid="slot-location-type"], select[name="locationType"]',
		slotCityInput: '[data-testid="slot-city"], input[name="city"]',
		slotCountrySelect: '[data-testid="slot-country"], select[name="country"]',
		slotOnlineLinkInput: '[data-testid="slot-online-link"], input[name="onlineLink"]',
		
		// Time configuration
		slotTimezoneSelect: '[data-testid="slot-timezone"], select[name="timezone"]',
		slotDayCheckbox: '[data-testid="slot-day"]',
		slotStartTimeInput: '[data-testid="slot-start-time"], input[name="startTime"]',
		slotEndTimeInput: '[data-testid="slot-end-time"], input[name="endTime"]',
		
		// Filters
		slotFilterSelect: '[data-testid="slot-filter"], select[name="filterRule"]',
		filterTypeSelect: '[data-testid="filter-type"], select[name="filterType"]',
		minMutualRecognitionInput: '[data-testid="min-mutual-recognition"], input[name="minMutualRecognition"]',
		onlyMutualCheckbox: '[data-testid="only-mutual"], input[name="onlyMutual"]',
		
		// Subtree filter
		subtreeFilterSelect: '[data-testid="subtree-filter"], select[name="subtreeFilter"]',
		
		// Slot actions
		saveSlotButton: '[data-testid="save-slot"], button:has-text("Save Slot")',
		deleteSlotButton: '[data-testid="delete-slot"], button:has-text("Delete Slot")',
	};

	constructor(page: Page) {
		super(page);
	}

	/**
	 * Navigate to the capacity/inventory view
	 */
	async gotoCapacities(): Promise<void> {
		await this.goto('/');
		await this.wait(500);
		
		// Switch to inventory view
		try {
			await this.page.locator(this.selectors.inventoryViewButton).click();
			await this.wait(500);
		} catch {
			// Already on inventory view
		}
		
		// Click capacities tab if it exists
		try {
			await this.page.locator(this.selectors.capacitiesTab).click();
			await this.wait(300);
		} catch {
			// Already on capacities tab or no tabs
		}
	}

	/**
	 * Check if capacities view is visible
	 */
	async isCapacitiesViewVisible(): Promise<boolean> {
		return await this.isVisible(this.selectors.capacitiesList);
	}

	/**
	 * Get all capacity cards
	 */
	async getCapacities(): Promise<Locator[]> {
		return await this.page.locator(this.selectors.capacityCard).all();
	}

	/**
	 * Get a capacity by name
	 */
	async getCapacityByName(name: string): Promise<Locator | null> {
		try {
			return this.page.locator(this.selectors.capacityCard)
				.filter({ hasText: name })
				.first();
		} catch {
			return null;
		}
	}

	/**
	 * Create a new capacity
	 */
	async createCapacity(
		name: string, 
		emoji: string = '📦', 
		unit: string = 'units', 
		description?: string
	): Promise<void> {
		// Click new capacity button
		await this.page.locator(this.selectors.newCapacityButton).click();
		await this.wait(300);
		
		// Fill in capacity details
		await this.fillField(this.selectors.capacityNameInput, name);
		
		try {
			await this.fillField(this.selectors.capacityEmojiInput, emoji);
		} catch {
			// Emoji field might not be editable
		}
		
		await this.fillField(this.selectors.capacityUnitInput, unit);
		
		if (description) {
			await this.fillField(this.selectors.capacityDescriptionInput, description);
		}
		
		// Save
		await this.page.locator(this.selectors.saveCapacityButton).click();
		await this.wait(500);
	}

	/**
	 * Delete a capacity
	 */
	async deleteCapacity(name: string): Promise<void> {
		const capacity = await this.getCapacityByName(name);
		if (capacity) {
			await capacity.click();
			await this.wait(300);
			
			await this.page.locator(this.selectors.deleteCapacityButton).click();
			
			// Confirm deletion if prompt appears
			this.page.once('dialog', dialog => {
				dialog.accept();
			});
			
			await this.wait(500);
		}
	}

	/**
	 * Add an availability slot to a capacity
	 */
	async addSlot(
		capacityName: string,
		config: {
			quantity: number;
			needType?: string;
			startDate?: string;
			recurrence?: string;
			locationType?: string;
			city?: string;
			country?: string;
			onlineLink?: string;
		}
	): Promise<void> {
		// Click on capacity
		const capacity = await this.getCapacityByName(capacityName);
		if (capacity) {
			await capacity.click();
			await this.wait(300);
		}
		
		// Click add slot button
		await this.page.locator(this.selectors.addSlotButton).click();
		await this.wait(300);
		
		// Fill in slot details
		await this.fillField(this.selectors.slotQuantityInput, config.quantity.toString());
		
		if (config.needType) {
			await this.page.locator(this.selectors.slotNeedTypeSelect).selectOption(config.needType);
		}
		
		if (config.startDate) {
			await this.fillField(this.selectors.slotStartDateInput, config.startDate);
		}
		
		if (config.recurrence) {
			await this.page.locator(this.selectors.slotRecurrenceSelect).selectOption(config.recurrence);
		}
		
		if (config.locationType) {
			await this.page.locator(this.selectors.slotLocationTypeSelect).selectOption(config.locationType);
		}
		
		if (config.city) {
			await this.fillField(this.selectors.slotCityInput, config.city);
		}
		
		if (config.country) {
			await this.page.locator(this.selectors.slotCountrySelect).selectOption(config.country);
		}
		
		if (config.onlineLink) {
			await this.fillField(this.selectors.slotOnlineLinkInput, config.onlineLink);
		}
		
		// Save slot
		try {
			await this.page.locator(this.selectors.saveSlotButton).click();
			await this.wait(500);
		} catch {
			// Slot might save automatically
		}
	}

	/**
	 * Configure a filter for a slot
	 */
	async configureSlotFilter(
		capacityName: string,
		slotIndex: number,
		filterConfig: {
			type: 'trust' | 'location' | 'allow_all' | 'deny_all';
			minMutualRecognition?: number;
			onlyMutual?: boolean;
		}
	): Promise<void> {
		// Click on capacity
		const capacity = await this.getCapacityByName(capacityName);
		if (capacity) {
			await capacity.click();
			await this.wait(300);
		}
		
		// Find the slot
		const slots = await this.page.locator(this.selectors.slotItem).all();
		if (slots.length > slotIndex) {
			await slots[slotIndex].click();
			await this.wait(300);
		}
		
		// Select filter type
		await this.page.locator(this.selectors.filterTypeSelect).selectOption(filterConfig.type);
		await this.wait(300);
		
		// Configure trust filter specifics
		if (filterConfig.type === 'trust') {
			if (filterConfig.minMutualRecognition !== undefined) {
				await this.fillField(
					this.selectors.minMutualRecognitionInput, 
					filterConfig.minMutualRecognition.toString()
				);
			}
			
			if (filterConfig.onlyMutual !== undefined) {
				const checkbox = this.page.locator(this.selectors.onlyMutualCheckbox);
				if (filterConfig.onlyMutual) {
					await checkbox.check();
				} else {
					await checkbox.uncheck();
				}
			}
		}
		
		await this.wait(300);
	}

	/**
	 * Get the number of slots for a capacity
	 */
	async getSlotCount(capacityName: string): Promise<number> {
		const capacity = await this.getCapacityByName(capacityName);
		if (capacity) {
			await capacity.click();
			await this.wait(300);
			
			return await this.page.locator(this.selectors.slotItem).count();
		}
		return 0;
	}

	/**
	 * Link capacity to a subtree
	 */
	async linkToSubtree(capacityName: string, subtreeName: string): Promise<void> {
		const capacity = await this.getCapacityByName(capacityName);
		if (capacity) {
			await capacity.click();
			await this.wait(300);
		}
		
		try {
			const subtreeSelect = this.page.locator(this.selectors.subtreeFilterSelect);
			await subtreeSelect.selectOption({ label: subtreeName });
			await this.wait(300);
		} catch {
			// Subtree filter might not be available
		}
	}
}

