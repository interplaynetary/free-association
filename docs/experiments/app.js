import { createTreemap } from './visualizations/TreeMap.js';
import { createPieChart } from './visualizations/PieChart.js';
import { D3Node } from './models/D3Node.js';

export class App {
    constructor(data) {
        console.log('=== INITIALIZATION ===');
        console.log('Constructor - Initial data:', data?.name);

        // 1. Create the Gun instance
        this.gun = Gun(['http://127.0.0.1:5500/gun']);
        console.log('Gun instance created');
        
        // 2. Create user instance
        this.user = this.gun.user();
        console.log('User instance created');

        // Generate a consistent alias from the data name
        const alias = data?.name || 'anonymous';
        console.log('Using alias:', alias);

        // First ensure we're logged out
        console.log('=== LOGOUT SEQUENCE ===');
        console.log('Current user state:', this.user.is);
        this.user.leave();
        console.log('Leave called, user state:', this.user.is);
        
        // Store data reference
        this.data = data;

        // Wait for logout to complete, then try auth sequence
        setTimeout(() => {
            console.log('=== AUTH SEQUENCE START ===');
            
            // IMPORTANT: Use a completely static password
            const staticPass = 'secure-password-for-' + alias + '-v1';
            console.log('Using static password with length:', staticPass.length);
            
            // Try to create first (will fail if exists, that's ok)
            console.log('Attempting user creation...');
            this.user.create(alias, staticPass, (createAck) => {
                console.log('Create response:', createAck);
                
                // Attempt auth regardless of create result
                console.log('Attempting authentication...');
                this.user.auth(alias, staticPass, (authAck) => {
                    console.log('Auth attempt response:', authAck);
                    
                    if (!authAck.err) {
                        console.log('Successfully authenticated!');
                        this.alias = this.user.is.pub;
                        this.loadOrInitializeData(this.data);
                    } else {
                        console.error('Auth failed:', authAck.err);
                        console.log('Current user state:', this.user.is);
                    }
                });
            });
        }, 100);

        // A local map of all contributor nodes, for D3 or other usage
        this.localContributors = new Map();

        // Store contributors in the user's private space instead of public
        this.contributors = this.user.get('contributors');

        this.addContributor(alias, { name: this.data.name });
        this._setupContributorListeners();

        // Log final constructor state
        console.log('=== CONSTRUCTOR COMPLETE ===');
        console.log('Final user state:', this.user.is);
    }

    /**
     * Private method - tries to authenticate the user.
     * If the user doesn't exist, create them. Then log in.
     */
    _initUser(alias, pass) {
        if (!alias || !pass) {
            console.error('Missing alias or pass:', { alias, pass });
            return;
        }
        console.log('Initializing user:', alias);
        
        // First try to authenticate
        this.user.auth(alias, pass, (ack) => {
            console.log('Auth attempt response:', ack);
            
            if (ack.err) {
                console.log('Auth failed, attempting creation');
                // If auth fails, create the user
                this.user.create(alias, pass, (createAck) => {
                    console.log('Create attempt response:', createAck);
                    
                    if (createAck.err) {
                        console.error('Error creating user:', createAck.err);
                    } else {
                        console.log('User created, attempting final auth');
                        this.user.auth(alias, pass, (finalAck) => {
                            console.log('Final auth response:', finalAck);
                            
                            if (!finalAck.err) {
                                this.alias = this.user.is.pub;
                                console.log('Successfully authenticated, initializing data');
                                this.loadOrInitializeData(this.data);
                            }
                        });
                    }
                });
            } else {
                console.log('Auth successful');
                this.alias = this.user.is.pub;
                this.loadOrInitializeData(this.data);
            }
        });
    }

    
    /**
     * Add a contributor to both:
     *  1) Our local map (D3, etc.)
     *  2) The Gun DB (so it's replicated)
     */
    addContributor(peerId, name) {
        // Create a local D3Node
        const node = new D3Node(name, null, [], peerId);
        this.localContributors.set(peerId, node);

        // Also store in Gun
        this.contributors.get(peerId).put({ name });
    }
    
    // 5. Listening for contributor changes
    _setupContributorListeners() {
        // "Map" over every peer in contributors
        this.contributors.map().once((data, peerId) => {
            if (!data) {
                console.log(`Contributor ${peerId} was removed or not found`);
                return;
            }
            console.log(`Contributor ${peerId} => `, data);
            // We could store data in a local map if we want:
            // e.g. this.peerDetails.set(peerId, data);
        });
    }

    getConnectedPeers() {
        const peers = this.gun.back('opt.peers');
        const connectedPeers = Object.values(peers).filter(peer => {
            return peer?.wire?.readyState === 1;
        });
        // connectedPeers.length => count of live connections
        return connectedPeers;
    }

    // New method to get a complete snapshot of the data state
    get dataSnapshot() {
        const snapshot = (node) => {
            const result = {
                name: node.name,
                points: node.points,
                types: node.types,
            };
            
            // Include children if they exist
            if (node.childrenArray) {
                result.children = node.childrenArray.map(child => snapshot(child));
            }
            
            return result;
        };
        
        return JSON.stringify(snapshot(this.data));
    }

    async loadOrInitializeData(initialData) {
        console.log('=== LOAD OR INITIALIZE DATA ===');
        console.log('Loading data for:', initialData?.name);
        console.log('Current user state:', this.user.is);
        
        try {
            // Try to load existing data from nodes path
            const rootRef = await this.user.get('root').once();
            console.log('Root reference:', rootRef);
            console.log('Root reference Alias:', rootRef.alias);

            if (rootRef) {
                console.log('Found existing root node:', rootRef.alias);
                const rootNode = await this.user.get('nodes').get(rootRef.alias).once();
                console.log('Root node data:', rootNode);       
                
                if (rootNode) {
                    console.log('Loading existing data');
                    this.data = await this.reconstructTree();
                } else {
                    console.log('Root node not found, initializing with:', initialData);
                    this.data = initialData;
                    await this.saveDataToGun(initialData);
                }
            } else {
                console.log('No root reference, initializing with:', initialData);
                this.data = initialData;
                await this.saveDataToGun(initialData);
            }
            
            if (!this.data) {
                console.log('No data after load/reconstruct, falling back to initial data');
                this.data = initialData;
                await this.saveDataToGun(initialData);
            }
            
            // Set up auto-save after data is loaded
            this.setupAutoSave();
            
            this.init();
        } catch (error) {
            console.error('Error in loadOrInitializeData:', error);
            this.data = initialData;
            await this.saveDataToGun(initialData);
            this.setupAutoSave();
            this.init();
        }
    }

    // Save data to Gun, preserving the tree structure
    saveDataToGun(rootNode) {
        console.log('\n=== SAVING TO GUN ===');
        console.log('Saving root node:', rootNode.name);
        
        const saveNode = (node, depth = 0) => {
            const indent = '  '.repeat(depth);
            const nodeRef = this.user.get('nodes').get(node.name);
            
            console.log(`${indent}Node: ${node.name}`);
            console.log(`${indent}├─ Points: ${node.points}`);
            console.log(`${indent}├─ Types: [${node.types.map(t => t.name).join(', ')}]`);
            console.log(`${indent}├─ IsContributor: ${node.isContributor}`);
            
            // Save node's direct properties
            nodeRef.put({
                name: node.name,
                points: node.points || 0,
                isContributor: Boolean(node.isContributor)
            });
            
            // Save children as references
            if (node.childrenArray && node.childrenArray.length) {
                console.log(`${indent}└─ Children: [${node.childrenArray.map(c => c.name).join(', ')}]`);
                
                // Create a proper children structure
                const children = {};
                node.childrenArray.forEach((child, i) => {
                    // Store each child's full data
                    children[child.name] = {
                        name: child.name,
                        points: child.points || 0,
                        isContributor: Boolean(child.isContributor),
                        index: i  // Preserve order
                    };
                });
                
                // Save children data
                nodeRef.get('children').put(children);
                
                // Recursively save each child's complete data
                node.childrenArray.forEach(child => saveNode(child, depth + 1));
            } else {
                console.log(`${indent}└─ No children`);
            }
        };
        
        // Save the root reference
        this.user.get('root').put({
            rootName: rootNode.name
        });
        
        // Start saving from root
        saveNode(rootNode);
        
        console.log('=== SAVE COMPLETE ===\n');
    }

    // Reconstruct the tree from flat data
    async reconstructTree() {
        console.log('\n=== RECONSTRUCTING TREE ===');
        const rootData = await this.user.get('root').once();
        console.log('Root data:', rootData);
        
        if (!rootData || !rootData.rootName) {
            console.log('No valid root name found in data:', rootData);
            return null;
        }
        
        console.log('Starting reconstruction from root name:', rootData.rootName);
        
        const reconstructNode = async (nodeName, depth = 0) => {
            if (!nodeName) {
                console.log('Invalid node name:', nodeName);
                return null;
            }
            
            const indent = '  '.repeat(depth);
            console.log(`${indent}Getting node data for:`, nodeName);
            
            const nodeRef = this.user.get('nodes').get(nodeName);
            const data = await nodeRef.once();
            
            if (!data || !data.name) {
                console.log(`${indent}No valid data found for node:`, nodeName);
                return null;
            }
            
            console.log(`${indent}Reconstructing node:`, data.name);
            
            // Create base node
            const node = new D3Node(data.name);
            node.setPoints(data.points || 0);
            
            // Load and add types
            const types = await nodeRef.get('types').once();
            if (types) {
                console.log(`${indent}├─ Loading types:`, types);
                Object.values(types)
                    .filter(t => t && t.name)
                    .forEach(type => {
                        const typeNode = new D3Node(type.name);
                        node.addType(typeNode);
                    });
            }
            
            // Load and add children
            const children = await nodeRef.get('children').once();
            if (children) {
                console.log(`${indent}├─ Loading children:`, children);
                const childPromises = Object.values(children)
                    .filter(child => child && child.name)
                    .map(child => reconstructNode(child.name, depth + 1));
                
                const reconstructedChildren = await Promise.all(childPromises);
                reconstructedChildren
                    .filter(child => child)
                    .forEach(child => {
                        node.addChild(child);
                    });
            }
            
            console.log(`${indent}└─ Node reconstruction complete:`, {
                name: node.name,
                types: node.types.map(t => t.name),
                children: node.childrenArray.map(c => c.name)
            });
            
            return node;
        };
        
        // Start reconstruction using the root name
        const result = await reconstructNode(rootData.rootName);
        
        if (!result) {
            console.log('Failed to reconstruct tree');
            return null;
        }
        
        console.log('=== RECONSTRUCTION COMPLETE ===\n');
        return result;
    }

    init() {
        console.log('=== INIT CALLED ===');
        console.log('Current user state:', this.user.is);
        console.log('Init called - creating treemap');
        // Initialize after successful auth
        this.gunContributors = this.user.get('contributors');
        const container = document.getElementById('treemap-container');

        const width = container.clientWidth;
        const height = container.clientHeight;

        console.log('Init - Creating treemap with data:', this.data.name);
        
        // Create treemap
        this.treemap = createTreemap(this.data, width, height);
        
        console.log('Init - Treemap created');
        
        // Store initial data state after treemap is created
        this.lastDataState = this.dataSnapshot;
        
        // Create initial pie chart
        this.updatePieChart();

        // Append visualizations
        container.appendChild(this.treemap.element);

        // Setup window resize handler
        window.addEventListener('resize', this.handleResize.bind(this));

        // Setup periodic checks for data changes
        setInterval(() => this.checkForDataChanges(), 100);
    }

    checkForDataChanges() {
        const currentDataState = this.dataSnapshot;
        if (currentDataState !== this.lastDataState) {
            console.log('Data change detected, updating pie chart');
            this.lastDataState = currentDataState;
            this.updatePieChart();
        }
    }

    get currentView() {
        return this.treemap.getCurrentView();
    }

    get currentViewData() {
        return this.currentView.data;
    }

    get currentData() {
        return this.treemap.getCurrentData();
    }

    handleResize() {
        console.log('Resize handler triggered');
        const container = document.getElementById('treemap-container');
        const width = container.clientWidth;
        const height = container.clientHeight;

        this.treemap.update(width, height);
    }

    updateVisualizations() {
        console.log('UpdateVisualizations started');
        const container = document.getElementById('treemap-container');
        const width = container.clientWidth;
        const height = container.clientHeight;
        
        // Store the current data path to restore zoom
        const path = [];
        let temp = this.currentViewData;
        console.log('Current view before update:', temp.name);
        while (temp !== this.data) {
            path.push(temp);
            temp = temp.parent;
        }
        
        // Clear and recreate treemap
        container.innerHTML = '';
        this.treemap = createTreemap(this.data, width, height);
        container.appendChild(this.treemap.element);
        
        // Restore zoom path using data references
        path.reverse().forEach(nodeData => {
            const correspondingNode = this.treemap.getRoot().descendants()
                .find(n => n.data === nodeData);
            if (correspondingNode) {
                this.treemap.zoomin(correspondingNode);
            }
        });

        // Update last data state after visualization update
        this.lastDataState = this.dataSnapshot;
    }

    updatePieChart() {
        const pieContainer = document.getElementById('pie-container');
        const currentData = this.currentData;
        pieContainer.innerHTML = '';
        const newPieChart = createPieChart(currentData);
        pieContainer.appendChild(newPieChart);
    }

    // This is our "RPC-like" request for shareOfGeneralFulfillment(peerId).
    // We'll send a message to the remotePeer telling them:
    // "Please compute shareOfGeneralFulfillment(callerPeerId)."
    requestShareOfGeneralFulfillment(remotePeerId, callerPeerId) {
        const conn = this.peer.connect(remotePeerId);
        conn.on('open', () => {
            const msg = {
                type: 'requestSOGF',
                callerPeerId,
            };
            conn.send(msg);
        });
    }

    /**
     * Example method: broadcast or store shareOfGeneralFulfillment in user space.
     * In this pattern, each user can store *their* computed SOGF about a target.
     */
    broadcastShareOfGeneralFulfillment(targetPeerAlias) {
        if (!this.user.is) {
            console.error('No user logged in, cannot broadcast SOGF');
            return;
        }

        // Suppose we have a local map from alias -> Node
        const targetNode = this.localContributors?.get(targetPeerAlias);
        if (!targetNode) {
            console.error(`No Node found for alias=${targetPeerAlias}`);
            return;
        }

        // Calculate SOGF
        const sogfValue = this.data.shareOfGeneralFulfillment(targetNode);

        // Put it under the user's personal "SOGF" branch,
        // e.g. user.get('SOGF').get(targetPeerAlias).put(sogfValue)
        this.user
            .get('SOGF')
            .get(targetPeerAlias)
            .put(sogfValue);

        // If you want it publicly readable, you could do:
        // this.gun.get('contributors').get(alias).get('SOGF').get(targetPeerAlias).put(sogfValue);

        console.log(`Broadcasting SOGF for ${targetPeerAlias} = ${sogfValue}`);
    }

    // Add an auto-save feature
    setupAutoSave() {
        console.log('Setting up auto-save');
        // Increase interval and add debouncing
        let saveTimeout;
        const saveData = () => {
            if (this.data) {
                console.log('Auto-saving data...');
                this.saveDataToGun(this.data);
            }
        };

        // Debounced auto-save every 30 seconds
        setInterval(() => {
            clearTimeout(saveTimeout);
            saveTimeout = setTimeout(saveData, 1000);
        }, 30000);
    }

    // Add a method to reconstruct arrays from Gun's object format
    reconstructArraysFromGun(gunData) {
        if (!gunData) return null;
        
        return {
            name: gunData.name,
            points: gunData.points || 0,
            types: Object.values(gunData.types || {}),
            children: Object.values(gunData.children || {}).map(child => ({
                name: child.name,
                points: child.points || 0,
                types: Object.values(child.types || {})
            }))
        };
    }
}