import { type Page, type Locator } from '@playwright/test';
import { BasePage } from './BasePage';

/**
 * Page Object for Recognition Tree Management
 * Handles tree operations, node management, and contributor assignments
 */
export class RecognitionTreePage extends BasePage {
	// Selectors
	private readonly selectors = {
		// Tree view
		treeContainer: '[data-testid="tree-container"], .tree-view, [data-component="Parent"]',
		treeNode: '[data-testid="tree-node"], .tree-node, .child-node',
		rootNode: '[data-testid="root-node"]',
		
		// Node elements
		nodeName: '[data-testid="node-name"]',
		nodePoints: '[data-testid="node-points"]',
		nodeContributors: '[data-testid="node-contributors"]',
		nodeFulfillment: '[data-testid="node-fulfillment"]',
		
		// Toolbar actions
		addNodeButton: '[data-testid="add-node"], button:has-text("Add Node")',
		deleteNodeButton: '[data-testid="delete-node"], button:has-text("Delete")',
		editNodeButton: '[data-testid="edit-node"], button:has-text("Edit")',
		
		// Node editing
		nodeNameInput: '[data-testid="node-name-input"], input[name="name"]',
		nodePointsInput: '[data-testid="node-points-input"], input[name="points"]',
		nodeManualFulfillment: '[data-testid="manual-fulfillment"], input[name="manualFulfillment"]',
		saveButton: '[data-testid="save-node"], button:has-text("Save")',
		cancelButton: '[data-testid="cancel-edit"], button:has-text("Cancel")',
		
		// Contributors
		addContributorButton: '[data-testid="add-contributor"], button:has-text("Add Contributor")',
		contributorDropdown: '[data-testid="contributor-dropdown"], select, [role="combobox"]',
		contributorOption: '[data-testid="contributor-option"]',
		contributorList: '[data-testid="contributors-list"]',
		contributorItem: '[data-testid="contributor-item"]',
		removeContributorButton: '[data-testid="remove-contributor"]',
		
		// Anti-contributors
		addAntiContributorButton: '[data-testid="add-anti-contributor"], button:has-text("Anti")',
		antiContributorList: '[data-testid="anti-contributors-list"]',
		
		// Recognition bars
		recognitionBar: '[data-testid="recognition-bar"], .bar-component',
		barSegment: '[data-testid="bar-segment"]',
		
		// View switcher
		treeViewButton: '[data-testid="tree-view"], button:has-text("Tree")',
		inventoryViewButton: '[data-testid="inventory-view"], button:has-text("Inventory")',
		mapViewButton: '[data-testid="map-view"], button:has-text("Map")',
	};

	constructor(page: Page) {
		super(page);
	}

	/**
	 * Navigate to the tree view
	 */
	async gotoTreeView(): Promise<void> {
		await this.goto('/');
		await this.wait(500);
		
		// Try to click tree view button if not already on tree view
		try {
			await this.page.locator(this.selectors.treeViewButton).click();
			await this.wait(500);
		} catch {
			// Already on tree view
		}
	}

	/**
	 * Check if tree is visible
	 */
	async isTreeVisible(): Promise<boolean> {
		return await this.isVisible(this.selectors.treeContainer);
	}

	/**
	 * Get all tree nodes
	 */
	async getTreeNodes(): Promise<Locator[]> {
		return await this.page.locator(this.selectors.treeNode).all();
	}

	/**
	 * Get a specific node by name
	 */
	async getNodeByName(name: string): Promise<Locator | null> {
		try {
			return this.page.locator(this.selectors.treeNode)
				.filter({ hasText: name })
				.first();
		} catch {
			return null;
		}
	}

	/**
	 * Click on a tree node
	 */
	async clickNode(nodeName: string): Promise<void> {
		const node = await this.getNodeByName(nodeName);
		if (node) {
			await node.click();
			await this.wait(300);
		}
	}

	/**
	 * Add a new child node
	 */
	async addChildNode(parentName: string, childName: string, points: number = 100): Promise<void> {
		// Click parent node to select it
		await this.clickNode(parentName);
		
		// Click add node button
		await this.page.locator(this.selectors.addNodeButton).click();
		await this.wait(300);
		
		// Fill in node details
		await this.fillField(this.selectors.nodeNameInput, childName);
		await this.fillField(this.selectors.nodePointsInput, points.toString());
		
		// Save
		await this.page.locator(this.selectors.saveButton).click();
		await this.wait(500);
	}

	/**
	 * Update node properties
	 */
	async updateNode(nodeName: string, newName?: string, newPoints?: number): Promise<void> {
		await this.clickNode(nodeName);
		
		// Click edit button
		await this.page.locator(this.selectors.editNodeButton).click();
		await this.wait(300);
		
		// Update fields
		if (newName) {
			await this.fillField(this.selectors.nodeNameInput, newName);
		}
		
		if (newPoints !== undefined) {
			await this.fillField(this.selectors.nodePointsInput, newPoints.toString());
		}
		
		// Save
		await this.page.locator(this.selectors.saveButton).click();
		await this.wait(500);
	}

	/**
	 * Delete a node
	 */
	async deleteNode(nodeName: string): Promise<void> {
		await this.clickNode(nodeName);
		
		// Click delete button
		await this.page.locator(this.selectors.deleteNodeButton).click();
		await this.wait(300);
		
		// Confirm deletion if prompt appears
		this.page.once('dialog', dialog => {
			dialog.accept();
		});
		
		await this.wait(500);
	}

	/**
	 * Add a contributor to a node
	 */
	async addContributor(nodeName: string, contributorName: string): Promise<void> {
		await this.clickNode(nodeName);
		
		// Click add contributor button
		await this.page.locator(this.selectors.addContributorButton).first().click();
		await this.wait(300);
		
		// Select contributor from dropdown
		const dropdown = this.page.locator(this.selectors.contributorDropdown).first();
		await dropdown.click();
		
		// Try to find and click the contributor
		try {
			await this.page.locator(this.selectors.contributorOption)
				.filter({ hasText: contributorName })
				.first()
				.click();
		} catch {
			// Might need to type to filter or create new contact
			await dropdown.fill(contributorName);
			await this.page.keyboard.press('Enter');
		}
		
		await this.wait(500);
	}

	/**
	 * Get contributors of a node
	 */
	async getNodeContributors(nodeName: string): Promise<string[]> {
		await this.clickNode(nodeName);
		
		const contributorsLocator = this.page.locator(this.selectors.contributorItem);
		const count = await contributorsLocator.count();
		
		const contributors: string[] = [];
		for (let i = 0; i < count; i++) {
			const text = await contributorsLocator.nth(i).textContent();
			if (text) {
				contributors.push(text.trim());
			}
		}
		
		return contributors;
	}

	/**
	 * Remove a contributor from a node
	 */
	async removeContributor(nodeName: string, contributorName: string): Promise<void> {
		await this.clickNode(nodeName);
		
		// Find and click remove button for specific contributor
		const contributorItem = this.page.locator(this.selectors.contributorItem)
			.filter({ hasText: contributorName })
			.first();
		
		await contributorItem.locator(this.selectors.removeContributorButton).click();
		await this.wait(500);
	}

	/**
	 * Add an anti-contributor to a node
	 */
	async addAntiContributor(nodeName: string, contributorName: string): Promise<void> {
		await this.clickNode(nodeName);
		
		// Click add anti-contributor button
		await this.page.locator(this.selectors.addAntiContributorButton).first().click();
		await this.wait(300);
		
		// Select contributor
		const dropdown = this.page.locator(this.selectors.contributorDropdown).first();
		await dropdown.click();
		
		try {
			await this.page.locator(this.selectors.contributorOption)
				.filter({ hasText: contributorName })
				.first()
				.click();
		} catch {
			await dropdown.fill(contributorName);
			await this.page.keyboard.press('Enter');
		}
		
		await this.wait(500);
	}

	/**
	 * Get recognition shares from the recognition bar
	 */
	async getRecognitionShares(): Promise<Record<string, number>> {
		const shares: Record<string, number> = {};
		
		try {
			const segments = await this.page.locator(this.selectors.barSegment).all();
			
			for (const segment of segments) {
				const text = await segment.textContent();
				if (text) {
					// Parse "Alice 30%" format
					const match = text.match(/(\w+)\s+(\d+(?:\.\d+)?)%/);
					if (match) {
						const [, name, percentage] = match;
						shares[name] = parseFloat(percentage);
					}
				}
			}
		} catch {
			// Recognition bar might not be visible yet
		}
		
		return shares;
	}

	/**
	 * Switch to a different view
	 */
	async switchView(view: 'tree' | 'inventory' | 'map'): Promise<void> {
		const buttonSelector = view === 'tree' 
			? this.selectors.treeViewButton
			: view === 'inventory'
			? this.selectors.inventoryViewButton
			: this.selectors.mapViewButton;
		
		await this.page.locator(buttonSelector).click();
		await this.wait(500);
	}

	/**
	 * Set manual fulfillment for a node
	 */
	async setManualFulfillment(nodeName: string, percentage: number): Promise<void> {
		await this.clickNode(nodeName);
		
		// Click edit button
		await this.page.locator(this.selectors.editNodeButton).click();
		await this.wait(300);
		
		// Set manual fulfillment
		await this.fillField(this.selectors.nodeManualFulfillment, percentage.toString());
		
		// Save
		await this.page.locator(this.selectors.saveButton).click();
		await this.wait(500);
	}
}

