/**
 * TreeNode class - Represents a node in a tree structure for visualization
 * Used by TreeMap.svelte for hierarchical data visualization
 */
export class TreeNode {
  public id: string = '';
  public name: string;
  public points: number = 0;
  public children: Map<string, TreeNode> = new Map();
  public isContribution: boolean = false;
  public metadata: any = {};
  public contributors: Map<string, TreeNode> = new Map();
  public tags: string[] = [];
  public fulfillment: number = 0;
  public parentReference: string | null = null;
  
  constructor(name: string) {
    this.name = name;
  }
  
  /**
   * Add a child node to this node
   * @param id Unique identifier for the child
   * @param child The child node to add
   * @returns The added child node
   */
  public addChild(id: string, child: TreeNode): TreeNode {
    this.children.set(id, child);
    return child;
  }
  
  /**
   * Remove a child node by its id
   * @param id The id of the child to remove
   * @returns True if the child was removed, false otherwise
   */
  public removeChild(id: string): boolean {
    return this.children.delete(id);
  }
  
  /**
   * Get a child node by its id
   * @param id The id of the child to get
   * @returns The child node or undefined if not found
   */
  public getChild(id: string): TreeNode | undefined {
    return this.children.get(id);
  }
  
  /**
   * Get all child nodes
   * @returns Array of child nodes
   */
  public getChildren(): TreeNode[] {
    return Array.from(this.children.values());
  }
  
  /**
   * Add a contributor node to this node
   * @param id Unique identifier for the contributor
   * @param contributor The contributor node to add
   * @returns The added contributor node
   */
  public addContributor(id: string, contributor: TreeNode): TreeNode {
    this.contributors.set(id, contributor);
    return contributor;
  }
  
  /**
   * Remove a contributor by its id
   * @param id The id of the contributor to remove
   * @returns True if the contributor was removed, false otherwise
   */
  public removeContributor(id: string): boolean {
    return this.contributors.delete(id);
  }
  
  /**
   * Get a contributor node by its id
   * @param id The id of the contributor to get
   * @returns The contributor node or undefined if not found
   */
  public getContributor(id: string): TreeNode | undefined {
    return this.contributors.get(id);
  }
  
  /**
   * Get all contributor nodes
   * @returns Array of contributor nodes
   */
  public getContributors(): TreeNode[] {
    return Array.from(this.contributors.values());
  }
  
  /**
   * Add a tag to this node
   * @param tag The tag to add
   * @returns True if the tag was added, false if it already existed
   */
  public addTag(tag: string): boolean {
    if (this.tags.includes(tag)) {
      return false;
    }
    this.tags.push(tag);
    return true;
  }
  
  /**
   * Remove a tag from this node
   * @param tag The tag to remove
   * @returns True if the tag was removed, false if it didn't exist
   */
  public removeTag(tag: string): boolean {
    const index = this.tags.indexOf(tag);
    if (index === -1) {
      return false;
    }
    this.tags.splice(index, 1);
    return true;
  }
  
  /**
   * Check if this node has a specific tag
   * @param tag The tag to check
   * @returns True if the node has the tag, false otherwise
   */
  public hasTag(tag: string): boolean {
    return this.tags.includes(tag);
  }
  
  /**
   * Get the total points of this node and all its children
   * @returns The total points
   */
  public getTotalPoints(): number {
    let total = this.points;
    for (const child of this.children.values()) {
      total += child.getTotalPoints();
    }
    return total;
  }
  
  /**
   * Get only the contribution children
   * @returns Array of contribution child nodes
   */
  public getContributionChildren(): TreeNode[] {
    return this.getChildren().filter(child => child.isContribution);
  }
  
  /**
   * Get only the non-contribution children
   * @returns Array of non-contribution child nodes
   */
  public getNonContributionChildren(): TreeNode[] {
    return this.getChildren().filter(child => !child.isContribution);
  }
  
  /**
   * Calculate the fulfillment percentage for visualization
   * @returns A number between 0 and 1 representing fulfillment percentage
   */
  public calculateFulfillment(): number {
    if (this.children.size === 0) {
      return this.fulfillment;
    }
    
    const totalChildPoints = this.getTotalPoints() - this.points;
    if (totalChildPoints === 0) {
      return this.fulfillment;
    }
    
    // Calculate weighted fulfillment from children
    let contributionWeight = 0;
    let contributionFulfillment = 0;
    let nonContributionFulfillment = 0;
    
    const contributionChildren = this.getContributionChildren();
    const nonContributionChildren = this.getNonContributionChildren();
    
    // Calculate weights and fulfillment for contribution children
    for (const child of contributionChildren) {
      const childWeight = child.points / totalChildPoints;
      contributionWeight += childWeight;
      contributionFulfillment += child.calculateFulfillment() * childWeight;
    }
    
    // Calculate fulfillment for non-contribution children
    for (const child of nonContributionChildren) {
      const childWeight = child.points / totalChildPoints;
      nonContributionFulfillment += child.calculateFulfillment() * childWeight;
    }
    
    // Combine fulfillment values
    const calculated = (contributionFulfillment * contributionWeight) + 
      (nonContributionFulfillment * (1 - contributionWeight));
    
    // Combine with manual fulfillment
    return Math.min(1, Math.max(0, calculated + this.fulfillment));
  }
  
  /**
   * Create a deep clone of this node and its children
   * @returns A new TreeNode that is a deep clone of this one
   */
  public clone(): TreeNode {
    const clone = new TreeNode(this.name);
    clone.id = this.id;
    clone.points = this.points;
    clone.isContribution = this.isContribution;
    clone.metadata = { ...this.metadata };
    clone.tags = [...this.tags];
    clone.fulfillment = this.fulfillment;
    clone.parentReference = this.parentReference;
    
    // Clone children
    for (const [id, child] of this.children) {
      clone.addChild(id, child.clone());
    }
    
    // Clone contributors
    for (const [id, contributor] of this.contributors) {
      clone.addContributor(id, contributor.clone());
    }
    
    return clone;
  }
  
  /**
   * Convert this node and its children to a simple object
   * Useful for serialization
   * @returns A plain object representation of the tree
   */
  public toObject(): any {
    const children: any = {};
    for (const [id, child] of this.children) {
      children[id] = child.toObject();
    }
    
    const contributors: any = {};
    for (const [id, contributor] of this.contributors) {
      contributors[id] = contributor.toObject();
    }
    
    return {
      id: this.id,
      name: this.name,
      points: this.points,
      isContribution: this.isContribution,
      metadata: this.metadata,
      tags: this.tags,
      fulfillment: this.fulfillment,
      parentReference: this.parentReference,
      children,
      contributors
    };
  }
  
  /**
   * Create a TreeNode from a plain object
   * @param obj The object to convert from
   * @returns A new TreeNode
   */
  public static fromObject(obj: any): TreeNode {
    const node = new TreeNode(obj.name);
    node.id = obj.id || '';
    node.points = obj.points || 0;
    node.isContribution = obj.isContribution || false;
    node.metadata = obj.metadata || {};
    node.tags = obj.tags || [];
    node.fulfillment = obj.fulfillment || 0;
    node.parentReference = obj.parentReference || null;
    
    // Create children
    if (obj.children) {
      for (const [id, childObj] of Object.entries(obj.children)) {
        node.addChild(id, TreeNode.fromObject(childObj));
      }
    }
    
    // Create contributors
    if (obj.contributors) {
      for (const [id, contributorObj] of Object.entries(obj.contributors)) {
        node.addContributor(id, TreeNode.fromObject(contributorObj));
      }
    }
    
    return node;
  }
  
  /**
   * Create a TreeNode from a RecognitionStore
   * This static factory method serves as an adapter between the data layer and visualization
   * @param nodeData The node data from a RecognitionStore
   * @param id The id of the node
   * @returns A new TreeNode populated with data from the RecognitionStore
   */
  public static fromRecStore(nodeData: any, id: string = ''): TreeNode {
    const node = new TreeNode(nodeData.name || 'Unnamed');
    node.id = id;
    node.points = nodeData.points || 0;
    node.isContribution = nodeData.isContribution || false;
    node.fulfillment = nodeData.fulfilled || 0;
    
    // Store reference to parent if available
    if (nodeData.parent) {
      node.parentReference = typeof nodeData.parent === 'string' ? 
        nodeData.parent : 
        (nodeData.parent['#'] || null);
    }
    
    // Copy metadata but exclude specific known properties
    node.metadata = { ...nodeData };
    
    return node;
  }
} 