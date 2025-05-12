/**
 * Free Association Protocol - GunDB Schema & API Layer
 * 
 * This module provides a structured interface for interacting with GunDB
 * in the context of the Free Association Protocol.
 */

// Core Gun Setup
import Gun from 'gun/gun';
import 'gun/sea';
import 'gun/lib/then';
import 'gun/lib/radix';
import 'gun/lib/radisk';
import 'gun/lib/store';
import 'gun/lib/rindexed';

/** 
 * Symbol to mark that a value is a reference to a soul
 * This helps distinguish between actual values and references
 */
const REF = Symbol('reference');

/**
 * Main Gun singleton class
 * Follows the singleton pattern to ensure only one Gun instance
 */
class TreeDB {
  static instance;
  static Gun = Gun;
  static SEA = SEA;

  constructor(options = {}) {
    if (TreeDB.instance) return TreeDB.instance;
    
    // Initialize Gun with relay peers if provided
    this.gun = Gun(options.peers || []);
    this.user = this.gun.user();
    TreeDB.instance = this;

    // Setup hooks for debugging
    if (options.debug) {
      this.setupDebugHooks();
    }
  }

  /**
   * Base entry point with app scope
   * Isolates all data under the free-association namespace
   */
  get chain() {
    return new RootNode(this.gun.get('free-association'));
  }

  /**
   * Get user node with proper typing
   */
  get userNode() {
    if (!this.user.is) return null;
    return new UserNode(this.user);
  }

  /**
   * Login with username and password
   */
  async login(credentials) {
    return new Promise((resolve, reject) => {
      this.user.auth(credentials, (ack) => {
        if (ack.err) reject(new Error(ack.err));
        else resolve(this.userNode);
      });
    });
  }

  /**
   * Create a new user
   */
  async createUser(username, password) {
    return new Promise((resolve, reject) => {
      this.user.create(username, password, (ack) => {
        if (ack.err) reject(new Error(ack.err));
        else resolve(this.login({ alias: username, pass: password }));
      });
    });
  }

  /**
   * Logout the current user
   */
  logout() {
    this.user.leave();
    return Promise.resolve(true);
  }

  /**
   * Create a pair for authentication or encryption
   */
  async createPair() {
    return SEA.pair();
  }

  /**
   * Setup debug hooks for monitoring Gun activity
   */
  setupDebugHooks() {
    Gun.on('create', (root) => {
      console.log('Gun instance created');
      
      root.on('hi', event => {
        console.log(`Peer connected: ${event.url}`);
        this.to.next(event);
      });

      root.on('bye', event => {
        console.log(`Peer disconnected: ${event.url}`);
        this.to.next(event);
      });

      root.on('put', event => {
        console.log('Data put:', event);
        this.to.next(event);
      });

      this.to.next(root);
    });
  }
}

/**
 * Base node class to standardize Gun node interactions
 * Provides a clean and consistent interface for working with Gun nodes
 */
class GunNode {
  constructor(chain, options = {}) {
    this.chain = chain;
    this.certificate = options.certificate;
    this.iterates = options.iterates || GunNode;
    this.timeout = options.timeout || 5000;
    this._subscriptions = new Set();
  }

  /**
   * Put data with certificate if available
   * Handles certificate management and error reporting
   */
  put(value, callback) {
    try {
      const options = this.certificate ? 
        { opt: { cert: structuredClone(this.certificate) } } : undefined;
      
      // If value is a reference, convert to Gun reference format
      if (value && value[REF]) {
        return this.chain.put({ '#': value[REF] }, callback, options);
      }
      
      return this.chain.put(value, callback, options);
    } catch (error) {
      console.error('Error in put operation:', error);
      if (callback) callback({ err: error.message });
      return this.chain;
    }
  }

  /**
   * Safe get method that preserves options and typing
   * Maintains chain properties throughout navigation
   */
  get(key) {
    return new (this.constructor)(this.chain.get(key), {
      certificate: this.certificate,
      iterates: this.iterates,
      timeout: this.timeout
    });
  }

  /**
   * Safe map method for iteration with proper type preservation
   */
  map() {
    return new this.iterates(this.chain.map(), {
      certificate: this.certificate,
      timeout: this.timeout
    });
  }

  /**
   * Create a readable stream for the node with proper cleanup
   * Returns a stream that emits items as they become available
   */
  stream(options = {}) {
    let subscriptions = [];
    let closed = false;
    const timeout = options.timeout || this.timeout;
    const filter = options.filter || (() => true);
    let timeoutId;
    
    return new ReadableStream({
      start: (controller) => {
        // Keep track of pending operations
        let pending = false;

        // Handle empty case with timeout
        timeoutId = setTimeout(() => {
          if (subscriptions.length === 0 && !pending && !closed) {
            closed = true;
            controller.close();
          }
        }, timeout);
        
        // Mark that we're waiting for data
        pending = true;
        
        // Use on() to continuously get updates
        const subscription = this.chain.map().on((data, key, _, ev) => {
          // Skip metadata and null values
          if (key === '_' || key === '#' || data === null || data === undefined) return;
          
          // Apply filter if provided
          if (!filter({ data, key })) return;
          
          // Create node with proper type preservation
          const node = this.get(key);
          
          // Enqueue the item
          controller.enqueue({ data, key, node });
          
          // Track the subscription for cleanup
          if (!subscriptions.includes(ev)) {
            subscriptions.push(ev);
          }
          
          // No longer pending
          pending = false;
        });
        
        // Keep the subscription handler for direct cancellation
        subscriptions.push(subscription);
      },
      
      pull: (controller) => {
        // This is called when the consumer is ready for more data
        // Nothing to do here as Gun is push-based, not pull-based
      },
      
      cancel: (reason) => {
        // Clear timeout to prevent memory leaks
        if (timeoutId) {
          clearTimeout(timeoutId);
        }
        
        // Unsubscribe from all gun events when stream is cancelled
        subscriptions.forEach(ev => {
          if (ev && ev.off) ev.off();
        });
        
        // Clear the subscriptions array
        subscriptions = [];
        closed = true;
        
        console.log(`Stream cancelled: ${reason || 'No reason provided'}`);
      }
    });
  }

  /**
   * Get data once as a promise with timeout
   * Handles edge cases and timeouts
   */
  async once(timeout = this.timeout) {
    try {
      return await Promise.race([
        new Promise(resolve => {
          // Handle undefined responses properly
          this.chain.once(value => {
            if (value === undefined && this.chain._.get) {
              // If value is undefined but node has a soul, it might be empty but exist
              resolve(null);
            } else {
              resolve(value);
            }
          });
        }),
        new Promise((_, reject) => 
          setTimeout(() => reject(new Error(`Timeout after ${timeout}ms`)), timeout))
      ]);
    } catch (error) {
      console.warn(`once() timed out for:`, this.chain);
      return null;
    }
  }

  /**
   * Watch data with subscription pattern (reactive)
   * Returns a subscribable object compatible with reactive frameworks
   */
  watch(initial = null) {
    return {
      subscribe: (callback) => {
        let event;
        if (initial !== null) callback({ value: initial, key: '' });
        
        this.chain.on(function(value, key, _, ev) {
          event = ev;
          callback({ 
            value, 
            key, 
            node: new GunNode(this, { 
              certificate: this.certificate,
              iterates: this.iterates,
              timeout: this.timeout
            }) 
          });
        });
        
        // Add subscription to tracking set
        if (event) this._subscriptions.add(event);
        
        return () => {
          if (event) {
            event.off();
            this._subscriptions.delete(event);
          }
        };
      }
    };
  }

  /**
   * Helper to await the node safely with timeout
   */
  get value() {
    return this.once();
  }

  /**
   * Get node's soul if available
   */
  get soul() {
    return this.chain._.get;
  }
  
  /**
   * Create a reference to this node
   * Uses the REF symbol to mark it as a reference
   */
  get ref() {
    const soul = this.soul;
    if (!soul) return null;
    
    return { [REF]: soul };
  }
  
  /**
   * Delete this node (set to null)
   */
  delete() {
    return this.put(null);
  }

  /**
   * Sets a value only if the node doesn't exist yet
   */
  async setOnce(value) {
    const current = await this.once();
    if (current === undefined) {
      return this.put(value);
    }
    return current;
  }
  
  /**
   * Clean up all subscriptions
   */
  unsubscribeAll() {
    this._subscriptions.forEach(sub => sub.off());
    this._subscriptions.clear();
  }
  
  /**
   * Define a property that gets a specific attribute as a node
   * This is a helper for creating property getters
   */
  static property(name) {
    return function() {
      return this.get(name);
    };
  }
}

/**
 * Root node for the application
 * Entry point for all data access
 */
class RootNode extends GunNode {
  constructor(chain, options = {}) {
    super(chain, options);
  }
  
  /**
   * Get all trees in the system
   */
  trees() {
    return new TreeCollection(this.chain.get('trees'), {
      certificate: this.certificate,
      iterates: TreeNode
    });
  }
  
  /**
   * Get certificates storage
   */
  certificates() {
    return new GunNode(this.chain.get('certificates'), {
      certificate: this.certificate
    });
  }
  
  /**
   * Get users collection
   */
  users() {
    return new GunNode(this.chain.get('users'), {
      certificate: this.certificate,
      iterates: UserNode
    });
  }
}

/**
 * User node for managing user-specific data
 */
class UserNode extends GunNode {
  constructor(chain, options = {}) {
    super(chain, options);
  }

  /**
   * Get user's owned trees
   */
  trees() {
    return new TreeCollection(this.chain.get('trees'), {
      certificate: this.certificate,
      iterates: TreeNode
    });
  }

  /**
   * Profile information
   */
  profile() {
    return new ProfileNode(this.chain.get('profile'), {
      certificate: this.certificate
    });
  }

  /**
   * Get user's public key
   */
  get publicKey() {
    return this.chain.is?.pub;
  }
  
  /**
   * Get user's certificates
   */
  certificates() {
    return new GunNode(this.chain.get('certificates'), {
      certificate: this.certificate
    });
  }
  
  /**
   * Store a certificate for later use
   */
  async storeCertificate(name, certificate) {
    return this.certificates().get(name).put(certificate);
  }
  
  /**
   * Retrieve a stored certificate
   */
  async getCertificate(name) {
    return this.certificates().get(name).once();
  }
}

/**
 * Profile node for storing user profile information
 */
class ProfileNode extends GunNode {
  constructor(chain, options = {}) {
    super(chain, options);
  }

  // Define properties using the static helper
  get name() { return this.get('name'); }
  get avatar() { return this.get('avatar'); }
  get bio() { return this.get('bio'); }
  
  /**
   * Update profile with multiple fields at once
   */
  async update(profileData) {
    const updates = Object.entries(profileData).map(([key, value]) => 
      this.get(key).put(value)
    );
    
    return Promise.all(updates);
  }
}

/**
 * Tree node representation with fulfillment calculations
 */
class TreeNode extends GunNode {
  constructor(chain, options = {}) {
    super(chain, options);
  }

  // Tree metadata using the static helper
  get ownerId() { return this.get('ownerId'); }
  get name() { return this.get('name'); }
  get points() { return this.get('points'); }
  
  // Tree relationships
  get contributors() { return this.get('contributors'); }
  get children() { return this.get('children'); }
  
  /**
   * Tree capacities
   */
  capacities() {
    return new GunNode(this.chain.get('capacities'), {
      certificate: this.certificate,
      iterates: CapacityNode
    });
  }
  
  /**
   * Tree capacity shares
   */
  capacityShares() {
    return new GunNode(this.chain.get('capacityShares'), {
      certificate: this.certificate,
      iterates: CapacityShareNode
    });
  }
  
  /**
   * Get parent node if available
   * @returns {Promise<TreeNode|null>} Parent node or null for root nodes
   */
  async getParent() {
    // Root nodes don't have parents
    const ownData = await this.once();
    if (!ownData || !ownData.parentId) return null;
    
    // Find parent in the tree structure
    return new TreeNode(
      TreeDB.instance.chain.trees().get(ownData.parentId).chain,
      { certificate: this.certificate }
    );
  }
  
  /**
   * Tree calculations
   */
  async weight() {
    return Calculations.weight(this);
  }
  
  async fulfillment() {
    return Calculations.fulfilled(this);
  }
  
  /**
   * Calculate mutual fulfillment with another tree
   * @param {TreeNode} otherTree - The other tree to calculate with
   * @param {Object} forest - Forest of all trees
   * @returns {Promise<number>} Mutual fulfillment value (0-1)
   */
  async mutualFulfillment(otherTree, forest) {
    return Calculations.mutualFulfillment(this, otherTree, forest);
  }
  
  /**
   * Calculate provider shares at specified depth
   * @param {number} depth - Depth for calculation
   * @param {Object} forest - Forest of all trees
   * @returns {Promise<Object>} Map of tree IDs to shares
   */
  async providerShares(depth, forest) {
    return Calculations.providerShares(this, depth, forest);
  }
  
  /**
   * Find all capacities shared with this tree
   * @param {Object} forest - Forest of all trees
   * @returns {Promise<Array>} Array of capacity objects with source info
   */
  async findSharedCapacities(forest) {
    const results = [];
    const treeId = this.soul;
    
    // For each tree in the forest, check for shares that include this tree
    for (const [providerId, provider] of Object.entries(forest)) {
      if (providerId === treeId) continue; // Skip self
      
      // Get provider shares at depth 2
      const shares = await Calculations.providerShares(provider, 2, forest);
      const myShare = shares[treeId];
      
      if (!myShare || myShare <= 0) continue;
      
      // Get capacities from this provider
      const providerData = await provider.once();
      if (!providerData || !providerData.capacities) continue;
      
      // For each capacity, calculate my share
      for (const [capacityId, capacity] of Object.entries(providerData.capacities)) {
        if (capacityId === '_' || capacityId === '#') continue;
        
        const capacityQuantity = capacity.quantity || 0;
        const shareQuantity = Calculations.computeQuantityShare(
          capacity,
          myShare
        );
        
        if (shareQuantity > 0) {
          results.push({
            providerId,
            providerName: providerData.name || providerId,
            capacityId,
            capacityName: capacity.capacityName || capacityId,
            capacity,
            myShare,
            shareQuantity,
            totalQuantity: capacityQuantity,
            unit: capacity.unit || ''
          });
        }
      }
    }
    
    return results;
  }
  
  /**
   * Add a child node with proper ID generation and parent reference
   */
  addChild(childData) {
    const childId = childData.id || crypto.randomUUID();
    return this.children.get(childId).put({
      name: childData.name,
      points: childData.points,
      contributors: childData.contributors || [],
      manualFulfillment: childData.manualFulfillment,
      parentId: this.soul, // Store reference to parent
      createdAt: Date.now()
    });
  }

  /**
   * Get all descendants as a stream
   * Handles cycles in the graph with proper back-pressure support
   */
  descendantsStream(options = {}) {
    const stack = [];
    const visited = new Set();
    let closed = false;
    const maxDepth = options.maxDepth || Infinity;
    const filter = options.filter || (() => true);
    
    return new ReadableStream({
      start: (controller) => {
        // Start with this node
        stack.push({ node: this, depth: 0 });
        
        // Process will continue in pull
      },
      
      pull: async (controller) => {
        if (stack.length === 0 || closed) {
          controller.close();
          return;
        }
        
        // Get the next node from the stack
        const { node, depth } = stack.pop();
        const nodeSoul = node.soul;
        
        // Skip if already visited or beyond max depth
        if ((nodeSoul && visited.has(nodeSoul)) || depth > maxDepth) {
          // Request more data if available
          if (stack.length > 0) {
            return;
          } else {
            controller.close();
            return;
          }
        }
        
        // Mark as visited if it has a soul
        if (nodeSoul) {
          visited.add(nodeSoul);
        }
        
        // Apply filter if provided
        if (filter(node)) {
          // Enqueue the current node
          controller.enqueue(node);
        }
        
        try {
          // Get children and add to stack for next pull
          const childrenNode = node.children;
          const children = await childrenNode.once();
          
          if (children) {
            // Process in reverse order so we get depth-first in the right order
            const childKeys = Object.keys(children).filter(key => key !== '_' && key !== '#');
            
            for (let i = childKeys.length - 1; i >= 0; i--) {
              const childKey = childKeys[i];
              stack.push({ 
                node: childrenNode.get(childKey), 
                depth: depth + 1 
              });
            }
          }
        } catch (error) {
          console.warn('Error processing descendants', error);
          // Continue with the next node
        }
      },
      
      cancel: (reason) => {
        // Mark the stream as closed
        closed = true;
        stack.length = 0;
        visited.clear();
        console.log(`Descendants stream cancelled: ${reason || 'No reason provided'}`);
      }
    });
  }
  
  /**
   * Iterate through all descendants (depth-first)
   * Uses async generator pattern for clean usage with for-await-of
   * @param {Object} options - Stream options
   * @param {number} options.maxDepth - Maximum depth to traverse
   * @param {Function} options.filter - Filter function to apply to nodes
   */
  async *descendants(options = {}) {
    const reader = this.descendantsStream(options).getReader();
    
    try {
      while (true) {
        const { done, value } = await reader.read();
        if (done) break;
        yield value;
      }
    } finally {
      reader.releaseLock();
    }
  }
}

/**
 * Collection of trees with creation and retrieval methods
 */
class TreeCollection extends GunNode {
  constructor(chain, options = {}) {
    super(chain, options);
  }
  
  /**
   * Create a new tree with proper metadata
   */
  createTree(treeData) {
    const treeId = treeData.id || crypto.randomUUID();
    this.get(treeId).put({
      ownerId: TreeDB.instance.user.is.pub,
      name: treeData.name,
      points: treeData.points,
      contributors: treeData.contributors || [],
      manualFulfillment: treeData.manualFulfillment,
      createdAt: Date.now()
    });
    
    return new TreeNode(this.chain.get(treeId), {
      certificate: this.certificate
    });
  }
  
  /**
   * Get all trees as a reactive collection
   * Returns a subscribable object that maintains tree list
   */
  all() {
    return {
      subscribe: (callback) => {
        const trees = [];
        let events = [];
        
        const subscription = this.chain.map().on((data, key, _, ev) => {
          // Add to event tracking
          if (!events.includes(ev)) {
            events.push(ev);
          }
          
          // Skip metadata and empty data
          if (key === '_' || key === '#' || data === null || data === undefined) return;
          
          // Create properly typed tree node
          const treeNode = new TreeNode(this.chain.get(key), {
            certificate: this.certificate
          });
          
          // Update or add to trees array
          const index = trees.findIndex(t => t.key === key);
          if (index >= 0) {
            trees[index] = { key, data, node: treeNode };
          } else {
            trees.push({ key, data, node: treeNode });
          }
          
          // Notify subscriber with the latest list
          callback([...trees]); // Send a copy to prevent mutation issues
        });
        
        // Return unsubscribe function that properly cleans up
        return () => {
          events.forEach(ev => {
            if (ev && ev.off) ev.off();
          });
          events = [];
        };
      }
    };
  }
  
  /**
   * Find trees by a property value
   * Searches through all trees for matching property
   * @param {string} property - The property name to match
   * @param {any} value - The value to match against
   * @returns {Promise<TreeNode[]>} - Array of matching tree nodes
   */
  async findBy(property, value) {
    const results = [];
    const stream = this.stream({
      filter: ({ data }) => data && data[property] === value
    });
    
    const reader = stream.getReader();
    
    try {
      while (true) {
        const { done, value: item } = await reader.read();
        if (done) break;
        results.push(item.node);
      }
    } finally {
      reader.releaseLock();
    }
    
    return results;
  }
  
  /**
   * Get a tree by ID with proper typing
   */
  get(id) {
    return new TreeNode(this.chain.get(id), {
      certificate: this.certificate
    });
  }

  /**
   * Process all items in a stream with a callback function
   * @param {Function} callback - Function to call for each item
   * @param {Object} options - Stream options
   * @returns {Promise<void>}
   */
  async forEach(callback, options = {}) {
    const stream = this.stream(options);
    const reader = stream.getReader();
    
    try {
      while (true) {
        const { done, value } = await reader.read();
        if (done) break;
        await callback(value);
      }
    } finally {
      reader.releaseLock();
    }
  }
}

/**
 * Capacity node for managing sharable resources
 */
class CapacityNode extends GunNode {
  constructor(chain, options = {}) {
    super(chain, options);
  }
  
  // Capacity properties
  get capacityId() { return this.get('capacityId'); }
  get capacityName() { return this.get('capacityName'); }
  get quantity() { return this.get('quantity'); }
  get unit() { return this.get('unit'); }
  get maxDivisibility() { return this.get('maxDivisibility'); }
  get coordinates() { return this.get('coordinates'); }
  
  /**
   * Update capacity details
   */
  async update(capacityData) {
    const updates = Object.entries(capacityData).map(([key, value]) => 
      this.get(key).put(value)
    );
    
    return Promise.all(updates);
  }
  
  /**
   * Compute share quantity based on percentage
   * Applies divisibility constraints
   */
  async computeShareQuantity(percentage) {
    const [capacity, maxDiv] = await Promise.all([
      this.quantity.once(),
      this.maxDivisibility.once()
    ]);
    
    if (!capacity || !maxDiv) return 0;
    
    return Calculations.computeQuantityShare(
      { quantity: capacity, maxDivisibility: maxDiv },
      percentage
    );
  }
}

/**
 * Capacity share node for managing shares in capacities
 */
class CapacityShareNode extends GunNode {
  constructor(chain, options = {}) {
    super(chain, options);
  }
  
  // Essential properties
  get targetCapacity() { return this.get('targetCapacity'); }
  get sharePercentage() { return this.get('sharePercentage'); }
  get computedQuantity() { return this.get('computedQuantity'); }
  get targetTreeId() { return this.get('targetTreeId'); }
  get targetCapacityId() { return this.get('targetCapacityId'); }
  get createdAt() { return this.get('createdAt'); }
  
  /**
   * Resolve the target capacity node
   * @returns {Promise<CapacityNode>} The resolved capacity node
   */
  async resolveTargetCapacity() {
    // First try to get the soul reference
    const targetCapRef = await this.targetCapacity.once();
    if (targetCapRef && targetCapRef['#']) {
      return new CapacityNode(
        TreeDB.instance.gun.get(targetCapRef['#']),
        { certificate: this.certificate }
      );
    }
    
    // Fallback to tree/capacity IDs if available
    const [treeId, capacityId] = await Promise.all([
      this.targetTreeId.once(),
      this.targetCapacityId.once()
    ]);
    
    if (treeId && capacityId) {
      return TreeDB.instance.chain.trees().get(treeId).capacities().get(capacityId);
    }
    
    return null;
  }
  
  /**
   * Update share percentage and recalculate quantity
   * Uses functional composition for clarity
   * @param {number} percentage - New percentage value (0-1)
   * @returns {Promise<boolean>} Success indicator
   */
  async updatePercentage(percentage) {
    const capacityNode = await this.resolveTargetCapacity();
    if (!capacityNode) return false;
    
    // Get the computed quantity
    const computedQuantity = await capacityNode.computeShareQuantity(percentage);
    
    // Update both values atomically
    await Promise.all([
      this.sharePercentage.put(percentage),
      this.computedQuantity.put(computedQuantity)
    ]);
    
    return true;
  }
  
  /**
   * Calculate the actual quantity this share represents
   * Takes into account the current capacity quantity and share percentage
   * @returns {Promise<number>} The calculated quantity
   */
  async calculateActualQuantity() {
    const [percentage, existingComputed] = await Promise.all([
      this.sharePercentage.once(),
      this.computedQuantity.once()
    ]);
    
    // If we can't get the percentage, return existing computed quantity or 0
    if (percentage === undefined) return existingComputed || 0;
    
    // Try to get the capacity and recalculate
    const capacityNode = await this.resolveTargetCapacity();
    if (!capacityNode) return existingComputed || 0;
    
    return capacityNode.computeShareQuantity(percentage);
  }
  
  /**
   * Check if this share is valid (target capacity exists)
   * @returns {Promise<boolean>} Validity status
   */
  async isValid() {
    const capacityNode = await this.resolveTargetCapacity();
    return capacityNode !== null;
  }
  
  /**
   * Return a complete share object with all properties
   * Useful for UI rendering and calculations
   * @returns {Promise<Object>} Complete share object
   */
  async toCompleteObject() {
    const [
      targetCapRef,
      percentage,
      computed,
      treeId,
      capId,
      created
    ] = await Promise.all([
      this.targetCapacity.once(),
      this.sharePercentage.once(),
      this.computedQuantity.once(),
      this.targetTreeId.once(),
      this.targetCapacityId.once(),
      this.createdAt.once()
    ]);
    
    const capacityNode = await this.resolveTargetCapacity();
    let capacityData = null;
    let unit = '';
    
    if (capacityNode) {
      capacityData = await capacityNode.once();
      unit = capacityData?.unit || '';
    }
    
    return {
      sharePercentage: percentage || 0,
      computedQuantity: computed || 0,
      targetTreeId: treeId,
      targetCapacityId: capId,
      targetCapacityRef: targetCapRef,
      createdAt: created || Date.now(),
      valid: capacityNode !== null,
      capacityData,
      unit
    };
  }
}

/**
 * Certificate management for secure access control
 */
class CertificateManager {
  constructor() {
    this.treeDB = TreeDB.instance;
  }
  
  /**
   * Create a certificate for contributor trees
   */
  async createContributorCertificate(treeId, contributorTreeIds) {
    if (!this.treeDB.user.is) {
      throw new Error('User must be authenticated to create certificates');
    }
    
    const grantees = contributorTreeIds.map(id => ({ pub: id }));
    const writableSpaces = [{ '*': treeId }];
    const issuer = this.treeDB.user._.sea;
    
    return SEA.certify(grantees, writableSpaces, issuer);
  }
  
  /**
   * Store a certificate on the graph for future use
   */
  storeCertificate(treeId, certificate) {
    return this.treeDB.chain.certificates().get(treeId).put(certificate);
  }
  
  /**
   * Retrieve a certificate from the graph
   */
  async getCertificate(treeId) {
    return this.treeDB.chain.certificates().get(treeId).once();
  }
  
  /**
   * Create a certificate and store it in one operation
   */
  async createAndStoreCertificate(treeId, contributorTreeIds) {
    const certificate = await this.createContributorCertificate(treeId, contributorTreeIds);
    await this.storeCertificate(treeId, certificate);
    return certificate;
  }
}

/**
 * This class contains the core calculations from the Haskell implementation
 * Provides calculation methods for weight, fulfillment, and mutual fulfillment
 */
class Calculations {
  /**
   * Calculate total points from all children
   * @param {TreeNode} node - The node to calculate for
   * @returns {Promise<number>} - The total points
   */
  static async totalChildPoints(node) {
    // Get all children
    const childrenMap = await node.children.once() || {};
    
    // Calculate sum of points for all children
    let total = 0;
    const childKeys = Object.keys(childrenMap).filter(k => k !== '_' && k !== '#');
    
    // Map all children to their points and sum them
    const pointPromises = childKeys.map(async childKey => {
      const childNode = node.children.get(childKey);
      const childData = await childNode.once();
      if (!childData || !childData.points) return 0;
      return childData.points;
    });
    
    const points = await Promise.all(pointPromises);
    return points.reduce((sum, p) => sum + p, 0);
  }
  
  /**
   * Calculate a node's weight
   * Corresponds to the weight function in Haskell
   * @param {TreeNode} node - The node to calculate weight for
   * @returns {Promise<number>} - The weight value (0-1)
   */
  static async weight(node) {
    // Root node has weight 1.0
    const parentNode = await node.getParent();
    if (!parentNode) return 1.0;
    
    // Get current node's points and parent's total points
    const nodeData = await node.once();
    if (!nodeData) return 0;
    
    const currentPoints = nodeData.points || 0;
    const total = await this.totalChildPoints(parentNode);
    
    // Avoid division by zero
    if (total === 0) return 0;
    
    // Calculate parent weight recursively
    const parentWeight = await this.weight(parentNode);
    
    // Final weight calculation
    return (currentPoints / total) * parentWeight;
  }
  
  /**
   * Calculate share of parent (percentage of parent's total points)
   * @param {TreeNode} node - The node to calculate for
   * @returns {Promise<number>} - Share of parent (0-1)
   */
  static async shareOfParent(node) {
    // Root node has 100% share
    const parentNode = await node.getParent();
    if (!parentNode) return 1.0;
    
    // Get current node's points
    const nodeData = await node.once();
    if (!nodeData) return 0;
    
    const currentPoints = nodeData.points || 0;
    
    // Get parent's total points
    const total = await this.totalChildPoints(parentNode);
    
    // Avoid division by zero
    if (total === 0) return 0;
    
    // Calculate share
    return currentPoints / total;
  }
  
  /**
   * Check if node is a contribution node (has contributors and is not root)
   * @param {TreeNode} node - The node to check
   * @returns {Promise<boolean>} - True if node is a contribution node
   */
  static async isContribution(node) {
    const nodeData = await node.once();
    if (!nodeData) return false;
    
    const hasContributors = nodeData.contributors && 
      Array.isArray(nodeData.contributors) && 
      nodeData.contributors.length > 0;
    
    const isRoot = !(await node.getParent());
    
    return hasContributors && !isRoot;
  }
  
  /**
   * Check if node has direct contribution children
   * @param {TreeNode} node - The node to check
   * @returns {Promise<boolean>} - True if any child is a contribution node
   */
  static async hasDirectContributionChild(node) {
    const childrenMap = await node.children.once() || {};
    const childKeys = Object.keys(childrenMap).filter(k => k !== '_' && k !== '#');
    
    for (const childKey of childKeys) {
      const childNode = node.children.get(childKey);
      if (await this.isContribution(childNode)) {
        return true;
      }
    }
    
    return false;
  }
  
  /**
   * Check if node has non-contribution children
   * @param {TreeNode} node - The node to check
   * @returns {Promise<boolean>} - True if any child is not a contribution node
   */
  static async hasNonContributionChild(node) {
    const childrenMap = await node.children.once() || {};
    const childKeys = Object.keys(childrenMap).filter(k => k !== '_' && k !== '#');
    
    for (const childKey of childKeys) {
      const childNode = node.children.get(childKey);
      if (!(await this.isContribution(childNode))) {
        return true;
      }
    }
    
    return false;
  }
  
  /**
   * Calculate the proportion of total child points from contribution children
   * @param {TreeNode} node - The node to calculate for
   * @returns {Promise<number>} - Proportion (0-1)
   */
  static async contributionChildrenWeight(node) {
    const childrenMap = await node.children.once() || {};
    const childKeys = Object.keys(childrenMap).filter(k => k !== '_' && k !== '#');
    
    let contribWeight = 0;
    let totalWeight = 0;
    
    for (const childKey of childKeys) {
      const childNode = node.children.get(childKey);
      const childWeight = await this.weight(childNode);
      
      totalWeight += childWeight;
      
      if (await this.isContribution(childNode)) {
        contribWeight += childWeight;
      }
    }
    
    // Avoid division by zero
    if (totalWeight === 0) return 0;
    
    return contribWeight / totalWeight;
  }
  
  /**
   * Calculate fulfillment from non-contribution children
   * @param {TreeNode} node - The node to calculate for
   * @returns {Promise<number>} - Fulfillment value (0-1)
   */
  static async nonContributionChildrenFulfillment(node) {
    const childrenMap = await node.children.once() || {};
    const childKeys = Object.keys(childrenMap).filter(k => k !== '_' && k !== '#');
    
    // Get all children nodes
    const childNodes = childKeys.map(key => node.children.get(key));
    
    // Filter to non-contribution children
    const nonContribChildren = [];
    for (const child of childNodes) {
      if (!(await this.isContribution(child))) {
        nonContribChildren.push(child);
      }
    }
    
    if (nonContribChildren.length === 0) return 0;
    
    // Calculate weights and fulfillments
    const weights = await Promise.all(nonContribChildren.map(child => this.weight(child)));
    const fulfillments = await Promise.all(nonContribChildren.map(child => this.fulfilled(child)));
    
    // Calculate weighted fulfillments
    const weightedFulfillments = weights.map((w, i) => w * fulfillments[i]);
    const totalWeight = weights.reduce((sum, w) => sum + w, 0);
    
    // Avoid division by zero
    if (totalWeight === 0) return 0;
    
    // Return weighted average
    return weightedFulfillments.reduce((sum, wf) => sum + wf, 0) / totalWeight;
  }
  
  /**
   * Calculate the fulfillment of a node
   * Corresponds to the fulfilled function in Haskell
   * @param {TreeNode} node - The node to calculate for
   * @returns {Promise<number>} - Fulfillment value (0-1)
   */
  static async fulfilled(node) {
    const nodeData = await node.once();
    if (!nodeData) return 0;
    
    // Empty nodes (leaves)
    const hasChildren = nodeData.children && Object.keys(nodeData.children).some(k => k !== '_' && k !== '#');
    if (!hasChildren) {
      return await this.isContribution(node) ? 1.0 : 0.0;
    }
    
    // Nodes with manual fulfillment
    if (nodeData.manualFulfillment !== undefined && 
        await this.hasDirectContributionChild(node)) {
      
      // If only contribution children, use manual value directly
      if (!(await this.hasNonContributionChild(node))) {
        return nodeData.manualFulfillment;
      }
      
      // Mixed children, calculate weighted value
      const contribWeight = await this.contributionChildrenWeight(node);
      const nonContribFulfillment = await this.nonContributionChildrenFulfillment(node);
      
      return nodeData.manualFulfillment * contribWeight + 
             nonContribFulfillment * (1.0 - contribWeight);
    }
    
    // Normal nodes: sum of children's fulfillment * share
    const childrenMap = await node.children.once() || {};
    const childKeys = Object.keys(childrenMap).filter(k => k !== '_' && k !== '#');
    
    let totalFulfillment = 0;
    
    for (const childKey of childKeys) {
      const childNode = node.children.get(childKey);
      const childFulfillment = await this.fulfilled(childNode);
      const shareOfParent = await this.shareOfParent(childNode);
      
      totalFulfillment += childFulfillment * shareOfParent;
    }
    
    return totalFulfillment;
  }
  
  /**
   * Calculate the desire (unfulfilled need) of a node
   * @param {TreeNode} node - The node to calculate for
   * @returns {Promise<number>} - Desire value (0-1)
   */
  static async desire(node) {
    const fulfillment = await this.fulfilled(node);
    return 1.0 - fulfillment;
  }
  
  /**
   * Calculate mutual fulfillment between two nodes
   * @param {TreeNode} nodeA - First node
   * @param {TreeNode} nodeB - Second node
   * @param {Object} forest - Forest of all trees
   * @returns {Promise<number>} - Mutual fulfillment value (0-1)
   */
  static async mutualFulfillment(nodeA, nodeB, forest) {
    if (!nodeA || !nodeB || !forest) return 0;
    
    // Get node IDs
    const nodeAData = await nodeA.once();
    const nodeBData = await nodeB.once();
    
    if (!nodeAData || !nodeBData) return 0;
    
    const aId = nodeAData.id || nodeA.soul;
    const bId = nodeBData.id || nodeB.soul;
    
    if (!aId || !bId) return 0;
    
    // Calculate shares of general fulfillment
    const sharesA = await this.sharesOfGeneralFulfillmentMap(nodeA, forest);
    const sharesB = await this.sharesOfGeneralFulfillmentMap(nodeB, forest);
    
    // Get mutual values
    const aToB = sharesA[bId] || 0;
    const bToA = sharesB[aId] || 0;
    
    // Mutual fulfillment is the minimum of the two
    return Math.min(aToB, bToA);
  }
  
  /**
   * Calculate shares of general fulfillment map
   * @param {TreeNode} provider - Provider node
   * @param {Object} forest - Forest of all trees
   * @returns {Promise<Object>} - Map of node IDs to fulfillment shares
   */
  static async sharesOfGeneralFulfillmentMap(provider, forest) {
    // Get all trees in the forest
    const treePromises = Object.keys(forest).map(async treeId => {
      // For each tree, calculate the share of general fulfillment
      const tree = forest[treeId];
      const share = await this.shareOfGeneralFulfillment(provider, tree, forest);
      return { treeId, share };
    });
    
    const shares = await Promise.all(treePromises);
    
    // Filter out zero shares and create map
    const shareMap = {};
    shares.forEach(({ treeId, share }) => {
      if (share > 0) {
        shareMap[treeId] = share;
      }
    });
    
    // Normalize the map
    return this.normalizeShareMap(shareMap);
  }
  
  /**
   * Calculate share of general fulfillment
   * @param {TreeNode} target - Target node
   * @param {TreeNode} contributor - Contributor node
   * @param {Object} forest - Forest of all trees
   * @returns {Promise<number>} - Share value (0-1)
   */
  static async shareOfGeneralFulfillment(target, contributor, forest) {
    const contributorData = await contributor.once();
    if (!contributorData) return 0;
    
    const contribId = contributorData.id || contributor.soul;
    if (!contribId) return 0;
    
    // Get all descendants including self
    const descendantsStream = await target.descendantsStream();
    const reader = descendantsStream.getReader();
    
    let contributingNodes = [];
    let weightedTotal = 0;
    
    try {
      while (true) {
        const { done, value: node } = await reader.read();
        if (done) break;
        
        // Check if this node is a contribution node with this contributor
        const nodeData = await node.once();
        if (!nodeData) continue;
        
        const isContributionNode = await this.isContribution(node);
        const hasContributor = nodeData.contributors && 
          Array.isArray(nodeData.contributors) && 
          nodeData.contributors.includes(contribId);
        
        if (isContributionNode && hasContributor) {
          contributingNodes.push(node);
        }
      }
    } finally {
      reader.releaseLock();
    }
    
    // Calculate total contribution
    for (const node of contributingNodes) {
      const nodeWeight = await this.weight(node);
      const nodeFulfillment = await this.fulfilled(node);
      
      // Get number of contributors
      const nodeData = await node.once();
      const contributorCount = nodeData.contributors ? nodeData.contributors.length : 1;
      
      // Divide by number of contributors
      weightedTotal += (nodeWeight * nodeFulfillment) / contributorCount;
    }
    
    return weightedTotal;
  }
  
  /**
   * Calculate provider shares at specified depth
   * @param {TreeNode} provider - Provider node
   * @param {number} depth - Depth for calculation (1 = direct, >1 = indirect)
   * @param {Object} forest - Forest of all trees
   * @returns {Promise<Object>} - Map of node IDs to share values
   */
  static async providerShares(provider, depth, forest) {
    if (depth <= 1) {
      return this.depth1Shares(provider, forest);
    } else {
      // Start with depth 1 shares
      let shareMap = await this.depth1Shares(provider, forest);
      
      // Iterate to required depth
      for (let d = 2; d <= depth; d++) {
        shareMap = await this.depthStepShares(shareMap, forest);
      }
      
      return this.normalizeShareMap(shareMap);
    }
  }
  
  /**
   * Calculate direct (depth 1) provider shares
   * @param {TreeNode} provider - Provider node
   * @param {Object} forest - Forest of all trees
   * @returns {Promise<Object>} - Map of node IDs to direct share values
   */
  static async depth1Shares(provider, forest) {
    const providerData = await provider.once();
    if (!providerData || !providerData.contributors) {
      return {};
    }
    
    const contribs = providerData.contributors;
    const shareMap = {};
    
    // Calculate mutual fulfillment with each contributor
    for (const contribId of contribs) {
      if (!forest[contribId]) continue;
      
      const contributor = forest[contribId];
      const mutual = await this.mutualFulfillment(provider, contributor, forest);
      
      if (mutual > 0) {
        shareMap[contribId] = mutual;
      }
    }
    
    return this.normalizeShareMap(shareMap);
  }
  
  /**
   * Calculate one step deeper provider shares
   * @param {Object} currentShares - Current depth share map
   * @param {Object} forest - Forest of all trees
   * @returns {Promise<Object>} - Updated share map
   */
  static async depthStepShares(currentShares, forest) {
    const newShares = { ...currentShares };
    
    // For each recipient in current shares
    for (const recipientId of Object.keys(currentShares)) {
      if (!forest[recipientId]) continue;
      
      const recipientShare = currentShares[recipientId];
      if (recipientShare <= 0) continue;
      
      // Get recipient's depth 1 shares
      const recipient = forest[recipientId];
      const recipientDepth1 = await this.depth1Shares(recipient, forest);
      
      // Distribute current share to recipient's shares
      for (const secondaryId of Object.keys(recipientDepth1)) {
        const secondaryShare = recipientDepth1[secondaryId];
        const weightedShare = secondaryShare * recipientShare;
        
        // Add to existing share if present
        if (newShares[secondaryId]) {
          newShares[secondaryId] += weightedShare;
        } else {
          newShares[secondaryId] = weightedShare;
        }
      }
    }
    
    return this.normalizeShareMap(newShares);
  }
  
  /**
   * Normalize a share map so values sum to 1
   * @param {Object} shareMap - Map of IDs to share values
   * @returns {Object} - Normalized map
   */
  static normalizeShareMap(shareMap) {
    const total = Object.values(shareMap).reduce((sum, share) => sum + share, 0);
    
    if (total <= 0) return shareMap;
    
    const normalized = {};
    Object.entries(shareMap).forEach(([id, share]) => {
      normalized[id] = share / total;
    });
    
    return normalized;
  }
  
  /**
   * Calculate receiver's share from a specific capacity provider
   * @param {TreeNode} receiver - Receiver node
   * @param {TreeNode} provider - Provider node
   * @param {Object} capacity - Capacity object
   * @param {number} maxDepth - Maximum depth for calculation
   * @param {Object} forest - Forest of all trees
   * @returns {Promise<number>} - Share value (0-1)
   */
  static async receiverShareFrom(receiver, provider, capacity, maxDepth, forest) {
    const receiverData = await receiver.once();
    if (!receiverData) return 0;
    
    const receiverId = receiverData.id || receiver.soul;
    
    // Calculate provider shares at requested depth
    const providerShareMap = await this.providerShares(provider, maxDepth, forest);
    
    // Return receiver's share
    return providerShareMap[receiverId] || 0;
  }
  
  /**
   * Compute quantity share based on capacity and percentage
   * Direct port of the Haskell implementation's computeQuantityShare function
   * Uses functional composition pattern for clarity
   * @param {Object} capacity - Capacity object 
   * @param {number} percentage - Share percentage (0-1)
   * @returns {number} - Computed quantity
   */
  static computeQuantityShare(capacity, percentage) {
    if (!capacity) return 0;
    
    const quantity = capacity.quantity || 0;
    const maxDivisibility = capacity.maxDivisibility || { naturalDiv: 1, percentageDiv: 1 };
    
    // Functional composition of constraints
    return [
      // Step 1: Calculate raw quantity
      rawQuantity => {
        // Step 2: Apply percentage divisibility constraint
        const maxPercent = maxDivisibility.percentageDiv;
        return percentage > maxPercent
          ? Math.round(quantity * maxPercent)
          : rawQuantity;
      },
      // Step 3: Apply natural number divisibility constraint
      percentConstrained => {
        const naturalDiv = maxDivisibility.naturalDiv;
        return Math.floor(percentConstrained / naturalDiv) * naturalDiv;
      }
    ].reduce(
      // Apply each function in sequence
      (value, fn) => fn(value),
      // Initial value: raw quantity
      Math.round(quantity * percentage)
    );
  }
}

/**
 * Main API facade that implements the Free Association Protocol
 */
class FreeAssociationAPI {
  constructor(options = {}) {
    this.treeDB = new TreeDB({
      peers: options.peers || [],
      debug: options.debug || false
    });
    this.certificateManager = new CertificateManager();
    
    // Initialize caches
    this._treeCache = new Map();
    this._userCache = new Map();
  }
  
  /**
   * User authentication and creation
   */
  async login(username, password) {
    const user = await this.treeDB.login({ alias: username, pass: password });
    this._clearCaches();
    return user;
  }
  
  async createUser(username, password) {
    await this.treeDB.createUser(username, password);
    return this.login(username, password);
  }
  
  logout() {
    this._clearCaches();
    return this.treeDB.logout();
  }
  
  /**
   * Get the current authenticated user
   */
  get currentUser() {
    return this.treeDB.userNode;
  }
  
  /**
   * Tree management
   */
  async createTree(treeData) {
    if (!this.currentUser) {
      throw new Error('User must be authenticated to create a tree');
    }
    
    const tree = await this.currentUser.trees().createTree(treeData);
    this._clearCaches();
    return tree;
  }
  
  /**
   * Get a tree by ID with caching
   */
  getTree(treeId) {
    if (!this._treeCache.has(treeId)) {
      const tree = this.treeDB.chain.trees().get(treeId);
      this._treeCache.set(treeId, tree);
    }
    return this._treeCache.get(treeId);
  }
  
  async addChildNode(treeId, childData) {
    const tree = this.getTree(treeId);
    return tree.addChild(childData);
  }
  
  /**
   * Capacity management
   */
  async addCapacity(treeId, capacityData) {
    const tree = this.getTree(treeId);
    return tree.capacities().get(capacityData.capacityId).put(capacityData);
  }
  
  /**
   * Add a capacity share with proper reference handling
   */
  async addCapacityShare(treeId, shareId, shareData) {
    const tree = this.getTree(treeId);
    
    // Get target tree and capacity
    const targetTree = this.getTree(shareData.targetTreeId);
    const targetCapacityNode = targetTree.capacities().get(shareData.targetCapacityId);
    
    // Get capacity data
    const [capacityData, maxDivData] = await Promise.all([
      targetCapacityNode.get('quantity').once(),
      targetCapacityNode.get('maxDivisibility').once()
    ]);
    
    if (!capacityData) {
      throw new Error('Target capacity not found or has no quantity');
    }
    
    // Create capacity-like object for calculation
    const capacityObj = {
      quantity: capacityData,
      maxDivisibility: maxDivData || { naturalDiv: 1, percentageDiv: 1 }
    };
    
    // Compute quantity based on capacity constraints using functional approach
    const computedQuantity = Calculations.computeQuantityShare(
      capacityObj, 
      shareData.sharePercentage
    );
    
    // Store the capacity share with its metadata
    return tree.capacityShares().get(shareId).put({
      targetCapacity: targetCapacityNode.ref,
      sharePercentage: shareData.sharePercentage,
      computedQuantity,
      // Add metadata for better traceability
      createdAt: Date.now(),
      targetTreeId: shareData.targetTreeId,
      targetCapacityId: shareData.targetCapacityId
    });
  }
  
  /**
   * Calculation endpoints
   */
  async getWeight(treeId) {
    const tree = this.getTree(treeId);
    return tree.weight();
  }
  
  async getFulfillment(treeId) {
    const tree = this.getTree(treeId);
    return tree.fulfillment();
  }
  
  /**
   * Calculate mutual fulfillment between trees
   */
  async getMutualFulfillment(treeId1, treeId2) {
    const tree1 = this.getTree(treeId1);
    const tree2 = this.getTree(treeId2);
    const forest = await this.getForest();
    
    return tree1.mutualFulfillment(tree2, forest);
  }
  
  /**
   * Get provider's shares distributed to the network
   * @param {string} treeId - Provider tree ID
   * @param {number} depth - Depth for calculation (1=direct, >1=indirect)
   * @returns {Promise<Object>} - Map of tree IDs to share values
   */
  async getProviderShares(treeId, depth) {
    const tree = this.getTree(treeId);
    const forest = await this.getForest();
    
    return tree.providerShares(depth || 1, forest);
  }
  
  /**
   * Calculate receiver's share from a specific capacity provider
   * @param {string} receiverId - Receiver tree ID
   * @param {string} providerId - Provider tree ID
   * @param {string} capacityId - Capacity ID
   * @param {number} maxDepth - Maximum depth for calculation
   * @returns {Promise<Object>} - Detailed share information
   */
  async getReceiverShareFrom(receiverId, providerId, capacityId, maxDepth = 2) {
    const receiver = this.getTree(receiverId);
    const provider = this.getTree(providerId);
    const forest = await this.getForest();
    
    // Get the capacity
    const providerData = await provider.once();
    if (!providerData || !providerData.capacities) {
      return { share: 0, quantity: 0, unit: '' };
    }
    
    const capacity = providerData.capacities[capacityId];
    if (!capacity) {
      return { share: 0, quantity: 0, unit: '' };
    }
    
    // Calculate share using Haskell-inspired algorithm
    const share = await Calculations.receiverShareFrom(
      receiver, 
      provider, 
      capacity, 
      maxDepth, 
      forest
    );
    
    // Calculate quantity based on capacity constraints
    const quantity = Calculations.computeQuantityShare(capacity, share);
    
    // Return detailed information
    return {
      share,
      sharePercentage: share * 100,
      quantity,
      capacity,
      unit: capacity.unit || '',
      providerId,
      capacityId,
      receiverId
    };
  }
  
  /**
   * Get a forest of all trees
   * @returns {Promise<Object>} - Map of tree IDs to tree nodes
   */
  async getForest() {
    const trees = this.treeDB.chain.trees();
    const forest = {};
    
    // Stream through all trees
    const stream = trees.stream();
    const reader = stream.getReader();
    
    try {
      while (true) {
        const { done, value } = await reader.read();
        if (done) break;
        
        const treeNode = value.node;
        const treeData = await treeNode.once();
        
        if (treeData) {
          const treeId = treeData.id || treeNode.soul;
          forest[treeId] = treeNode;
        }
      }
    } finally {
      reader.releaseLock();
    }
    
    return forest;
  }
  
  /**
   * Clear all caches
   */
  _clearCaches() {
    this._treeCache.clear();
    this._userCache.clear();
  }
  
  /**
   * Cleanup all subscriptions
   */
  cleanup() {
    this._treeCache.forEach(tree => {
      if (tree.unsubscribeAll) tree.unsubscribeAll();
    });
    this._clearCaches();
  }

  /**
   * Get a person's total capacity shares across the network
   * Find all capacity shares available to a tree
   * @param {string} treeId - Tree ID to check
   * @returns {Promise<Array>} - Array of capacity share objects
   */
  async getPersonalCapacityShares(treeId) {
    const tree = this.getTree(treeId);
    const forest = await this.getForest();
    
    return tree.findSharedCapacities(forest);
  }
  
  /**
   * Create a new capacity for a tree
   * @param {string} treeId - Tree ID to add capacity to
   * @param {Object} capacityData - Capacity details
   * @returns {Promise<CapacityNode>} - The created capacity node
   */
  async createCapacity(treeId, capacityData) {
    // Ensure required fields
    const capacityId = capacityData.capacityId || crypto.randomUUID();
    const tree = this.getTree(treeId);
    
    // Ensure maxDivisibility is properly set
    const maxDivisibility = capacityData.maxDivisibility || {
      naturalDiv: 1,
      percentageDiv: 1.0
    };
    
    // Create standardized capacity object
    const capacity = {
      capacityId,
      capacityName: capacityData.capacityName || 'Unnamed Capacity',
      quantity: capacityData.quantity || 0,
      unit: capacityData.unit || 'units',
      shareDepth: capacityData.shareDepth || 2,
      expanded: capacityData.expanded !== undefined ? capacityData.expanded : true,
      coordinates: capacityData.coordinates || {
        locationType: 'Undefined',
        allDay: true,
        startDate: new Date().toISOString(),
        endDate: new Date().toISOString(),
        timeZone: 'UTC'
      },
      maxDivisibility,
      hiddenUntilRequestAccepted: capacityData.hiddenUntilRequestAccepted || false,
      createdAt: Date.now()
    };
    
    // Store the capacity
    await tree.capacities().get(capacityId).put(capacity);
    
    return tree.capacities().get(capacityId);
  }
  
  /**
   * Create a capacity share between trees
   * @param {string} receiverId - Receiver tree ID
   * @param {string} providerId - Provider tree ID
   * @param {string} capacityId - Capacity ID
   * @param {number} percentage - Share percentage (0-1)
   * @returns {Promise<Object>} - Share details
   */
  async createCapacityShare(receiverId, providerId, capacityId, percentage) {
    const shareId = crypto.randomUUID();
    const receiver = this.getTree(receiverId);
    const provider = this.getTree(providerId);
    
    // Get the capacity
    const capacityNode = await provider.capacities().get(capacityId);
    const capacityData = await capacityNode.once();
    
    if (!capacityData) {
      throw new Error('Capacity not found');
    }
    
    // Create share data
    const shareData = {
      targetTreeId: providerId,
      targetCapacityId: capacityId,
      sharePercentage: percentage
    };
    
    // Add the share
    await this.addCapacityShare(receiverId, shareId, shareData);
    
    // Return the created share
    return receiver.capacityShares().get(shareId).toCompleteObject();
  }
  
  /**
   * Update a capacity quantity
   * This will automatically update all computed share quantities
   * @param {string} treeId - Tree ID
   * @param {string} capacityId - Capacity ID
   * @param {number} newQuantity - New quantity
   * @returns {Promise<boolean>} - Success status
   */
  async updateCapacityQuantity(treeId, capacityId, newQuantity) {
    const tree = this.getTree(treeId);
    const capacityNode = tree.capacities().get(capacityId);
    
    // Update the quantity
    await capacityNode.get('quantity').put(newQuantity);
    
    // Get forest to find all trees sharing this capacity
    const forest = await this.getForest();
    
    // For each tree, find shares of this capacity and update the computed quantities
    const updatePromises = [];
    for (const [id, node] of Object.entries(forest)) {
      const sharesMap = await node.capacityShares().once() || {};
      
      // Filter to shares referencing this capacity
      for (const [shareId, share] of Object.entries(sharesMap)) {
        if (shareId === '_' || shareId === '#') continue;
        
        // Check if this share references our capacity
        if (share.targetTreeId === treeId && share.targetCapacityId === capacityId) {
          // Update the computed quantity
          const shareNode = node.capacityShares().get(shareId);
          updatePromises.push(shareNode.updatePercentage(share.sharePercentage));
        }
      }
    }
    
    // Wait for all updates to complete
    await Promise.all(updatePromises);
    
    return true;
  }
}

/**
 * Create a factory function that produces a chain of methods
 * This is inspired by the guide's recommendation for chainable methods
 */
function createAPI(options = {}) {
  const api = new FreeAssociationAPI(options);
  
  // Create a cleanup handler for when the app exits
  if (typeof window !== 'undefined') {
    window.addEventListener('beforeunload', () => api.cleanup());
  }
  
  return api;
}

// Export the API and core classes
export {
  TreeDB,
  GunNode,
  UserNode,
  TreeNode,
  TreeCollection,
  CapacityNode,
  CapacityShareNode,
  CertificateManager,
  FreeAssociationAPI,
  createAPI
};

// Export a singleton instance for easy access
export default createAPI();