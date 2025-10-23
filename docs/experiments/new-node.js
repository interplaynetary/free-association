class Node {
    constructor(name, parent = null, scalarQuantity, types = []) {
        this.name = name;
        this.scalarQuantity = scalarQuantity;
        this.parent = parent;
        
        // Use composition system
        this.composes = new Map();  // Node -> Map(contributor -> points)
        this.isContributor = this.parent ? false : true;
        
        // Keep type system
        this.typeIndex = parent ? parent.root.typeIndex : new Map();
        
        if (types.length > 0) {
            types.forEach(type => {
                this.addType(type);
                if (type.isContributor) {
                    this.isContributor = true;
                }
            });
        }
    }

    // Add this node as an instance of the given type
    addType(type) {
        console.log(`\nAdding type ${type.name} to ${this.name}`);
        
        const root = this.root;
        if (!root.typeIndex.has(type)) {
            root.typeIndex.set(type, new Set());
        }
        root.typeIndex.get(type).add(this);
        
        // Debug verification
        console.log(`After adding type ${type.name} to ${this.name}:`);
        console.log(`- Instances:`, Array.from(root.typeIndex.get(type)).map(i => i.name));
        
        return this;
    }

    compose(name, points = 0, scalarQuantity, types = []) {
        if (this.parent && this.isContributor) {
            throw new Error(`Node ${this.name} is an instance of a contributor and cannot have children.`);
        }

        const child = new Node(name, this, scalarQuantity);
        
        if (Array.isArray(types)) {
            types.forEach(type => child.addType(type));
        } else if (types) {
            child.addType(types);
        }
        
        // Initialize composition relationship
        this.composes.set(child, new Map());
        
        // If points provided, set them as a contribution from this node
        if (points > 0) {
            this.setPointsForComposed(child, this, points);
        }
        
        return child;
    }

    // New composition methods
    setPointsForComposed(node, contributor, points) {
        if (!this.composes.has(node)) {
            this.composes.set(node, new Map());
        }
        this.composes.get(node).set(contributor, points);
        return this;
    }

    get totalCompositionPoints() {
        return Array.from(this.composes.values())
            .reduce((sum, pointsMap) => 
                sum + Array.from(pointsMap.values())
                    .reduce((a, b) => a + b, 0), 0);
    }

    get shareOfParentTotalCompositionPoints() {
        if (!this.parent) return 1;
        
        const myPoints = Array.from(this.parent.composes.get(this)?.values() || [])
            .reduce((a, b) => a + b, 0);
            
        return myPoints / this.parent.totalCompositionPoints;
    }

    get weight() {
        if (!this.parent) return 1;
        return this.shareOfParentTotalCompositionPoints * this.parent.weight;
    }

    removeChild(name) {
        const child = this.composes.get(name);
        if (child) {
            // Update points
            if (child.points > 0) {
                this.totalChildPoints -= child.points;
            }
            // Remove from type index
            this.root.typeIndex.forEach(instances => {
                instances.delete(child);
            });
            // Remove from children
            this.composes.delete(name);
        }
        return this;
    }

    get root() {
        return this.parent ? this.parent.root : this;
    }

    // Helper method to get all types in the system
    get types() {
        return Array.from(this.root.typeIndex.keys());
    }

    // Helper method to get all instances of a given type
    getInstances(type) {
        return this.root.typeIndex.get(type) || new Set();
    }

    // Calculate how much this node recognizes a given type/node
    shareOfGeneralContribution(node) {
        const instances = this.root.typeIndex.get(node) || new Set();
        return Array.from(instances).reduce((sum, instance) => {
            return sum + instance.weight;
        }, 0);
    }

    mutualShareOfGeneralContribution(node) {
        console.log(`\nDEBUG: Calculating mutual recognition between ${this.name} and ${node.name}`);
        console.log(`- ${this.name} types:`, this.types.map(t => t.name));
        console.log(`- ${node.name} instances:`, Array.from(this.getInstances(node)).map(i => i.name));
        
        // Calculate recognition in both directions
        const otherShareInMe = this.shareOfGeneralContribution(node);
        console.log(`- ${this.name}'s recognition of ${node.name}: ${otherShareInMe}`);
        
        const myShareInOther = node.shareOfGeneralContribution(this);
        console.log(`- ${node.name}'s recognition of ${this.name}: ${myShareInOther}`);
        
        // Mutual recognition is the minimum of both directions
        const mutual = Math.min(otherShareInMe, myShareInOther);
        console.log(`- Mutual recognition: ${mutual}`);
        
        return mutual;
    }

    // Calculate final mutualGeneralContribution of mutualShareOfGeneralContribution
    get mutualGeneralContribution() {
        // Get all types that have instances in the system
        const types = Array.from(this.root.typeIndex.keys())
            .filter(type => this.getInstances(type).size > 0);
        
        // Calculate mutual recognition for each type
        const mutualContributions = types.map(type => ({
            contributor: type,
            value: this.mutualShareOfGeneralContribution(type)
        })).filter(({ value }) => value > 0);  // Only keep positive recognitions

        // Calculate total for normalization
        const total = mutualContributions.reduce((sum, { value }) => sum + value, 0);
        
        // Create normalized mutualGeneralContribution
        console.log('\nCalculating mutualGeneralContribution:');
        const mutualGeneralContribution = new Map(
            mutualContributions.map(({ contributor, value }) => [
                contributor,
                total === 0 ? 0 : value / total
            ])
        );

        // Log final results
        console.log('\nFinal mutualGeneralContribution:');
        mutualGeneralContribution.forEach((share, contributor) => {
            console.log(`${contributor.name}: ${(share * 100).toFixed(1)}%`);
        });
        console.log( 'General mutual Contribution will serve as the bridge between Abstract and Concrete Time distribution and allow us to calculate Concrete Mutual Recognition of Mutual Relation.' );
        return mutualGeneralContribution;
    }
    shareOfMutualGeneralContribution(node){
        // share of communal production/contribution/total-social-product!
        // share in the part of the total product destined for individual consumption (means of subsistence)
        return this.mutualGeneralContribution.get(node);
    }
}


function testNodeSystem() {
    console.log("=== Testing Node Composition System ===\n");

    // Create basic structure
    const root = new Node("Root");
    console.log("1. Created root node:", root.name);

    // Test type system with mutual recognition
    const contributorType = new Node("Contributor");
    const workerType = new Node("Worker");
    console.log("\n2. Testing type system:");
    
    // Create nodes with their primary types
    const A = root.compose("A", 30, undefined, [contributorType]);
    const B = root.compose("B", 0, undefined, [workerType]);
    const C = root.compose("C", 40);
    
    // Set up mutual recognition by adding reciprocal types
    contributorType.compose("Root", 90, undefined, [root]);
    workerType.compose("Root", 20, undefined, [root]);

    console.log("Created nodes with types:");
    console.log(`- ${A.name} types:`, Array.from(root.typeIndex.keys()).filter(t => 
        root.typeIndex.get(t).has(A)).map(t => t.name));
    console.log(`- ${B.name} types:`, Array.from(root.typeIndex.keys()).filter(t => 
        root.typeIndex.get(t).has(B)).map(t => t.name));

    // Test composition points
    console.log("\n3. Testing composition points:");
    root.setPointsForComposed(B, A, 20);  // A contributes to B
    root.setPointsForComposed(B, root, 50);  // root contributes to B
    
    console.log("Composition points for B:");
    root.composes.get(B).forEach((points, contributor) => {
        console.log(`- From ${contributor.name}: ${points} points`);
    });

    // Test weight calculations
    console.log("\n4. Testing weight calculations:");
    console.log(`Total composition points in root: ${root.totalCompositionPoints}`);
    
    [A, B, C].forEach(node => {
        console.log(`\n${node.name}'s metrics:`);
        console.log(`- Share of parent total points: ${node.shareOfParentTotalCompositionPoints.toFixed(3)}`);
        console.log(`- Weight: ${node.weight.toFixed(3)}`);
    });

    // Test mutual recognition
    console.log("\n6. Testing mutual recognition:");
    const mutualAB = A.mutualShareOfGeneralContribution(B);
    console.log(`Mutual recognition between A and B: ${mutualAB.toFixed(3)}`);

    // Test final mutual general contribution
    console.log("\n7. Testing mutual general contribution:");
    const mutualGeneral = root.mutualGeneralContribution;
    console.log("Final mutual general contribution map:");
    mutualGeneral.forEach((value, contributor) => {
        console.log(`- ${contributor.name}: ${(value * 100).toFixed(1)}%`);
    });
}
// Run the tests
testNodeSystem();