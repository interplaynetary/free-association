/*
When an instance has multiple types, with some being contributors, it represents a fascinating intersection of abstract categorization and concrete labor. For example, when a "Marine Conservation Workshop" is typed as both 'educator' (abstract type) and 'whalewatch' (contributor), it simultaneously exists as both a category of educational activity and a specific manifestation of whalewatch's concrete labor. This dual nature captures how real-world activities often embody both abstract social functions and concrete contributions from specific entities.

The presence of contributor types among an instance's types transforms its nature - making it a leaf node that cannot have children. This reflects how when concrete labor is involved in creating something, that thing becomes a specific, realized instance rather than an abstract category. It's the difference between "education" as a category and a specific workshop that whalewatch actually conducted - the latter is a concrete manifestation that can't be further subdivided into abstract categories.

The mutual recognition aspect becomes particularly interesting with multiple types. When an instance has both contributor and non-contributor types, it serves as a bridge between abstract social categories and concrete labor. The contributor types enable the instance to participate in mutual recognition relationships, while the non-contributor types help categorize and organize the instance within the broader social framework. This mirrors how real-world activities simultaneously contribute to multiple social functions while also representing specific instances of labor.

This structure allows us to model complex real-world scenarios where multiple contributors collaborate within various abstract categories. For instance, a joint research project might be typed as both 'whalewatch' and 'greenpeace' (contributors) as well as 'researcher' and 'educator' (abstract types), representing how concrete labor from multiple sources can come together to create something that serves multiple abstract social functions. This captures the rich, multi-dimensional nature of social production.

The implications for mutual recognition are profound - when an instance has multiple contributor types, it represents a point of collective recognition where different contributors acknowledge each other's concrete labor within the context of abstract social categories. This models how real social cooperation works: concrete labor is performed collectively, recognized mutually, and organized according to abstract social categories. The inability of such instances to have children enforces the principle that once concrete labor is involved, we're dealing with specific manifestations rather than abstract categories.


# And so if i understand correclty from an interface perspective its as if, the root-node can label any node a composition of several contributors? What does this mean for mutual recognition and truth? Could a root-node then group and ungroup its recognition of group collaborations at will?

This is a profound question that gets at the heart of truth in mutual recognition! Let me break this down:

1. **Power of Root Node Classification**
The ability of a root node to arbitrarily group contributors into composite types creates a potential contradiction. While it can technically label any instance as being the product of multiple contributors, this labeling must reflect actual mutual recognition to be "true". Just like the rich kid example, merely declaring something to be a collaborative effort doesn't make it real mutual recognition.

2. **Truth in Mutual Recognition**
The key insight from truth.md applies directly here: if a root node falsely groups contributors who aren't actually collaborating, it dilutes the "true recognition" of real collaborative relationships. When a root node assigns false collaborative groupings:
- It reduces points available for recognizing real collaborations
- The falsely grouped contributors won't mutually recognize this grouping
- This leads to reduced mutual contribution with real producers
- Eventually resulting in decreased access to material coordination

3. **Natural Correction Through Mutual Recognition**
The beauty of the mutual recognition system is that false groupings naturally decay because:
- Contributors must actively recognize each other through their own give/potential structures
- If grouped contributors don't actually collaborate, they won't establish mutual recognition paths
- Without mutual recognition, the grouping becomes ineffective in the broader system
- The root node's arbitrary grouping can't override this lack of mutual recognition

4. **Group Formation Must Be Bottom-Up**
This suggests that while root nodes can technically create groupings, true collaborative groups must emerge from:
- Actual collaboration between contributors
- Mutual recognition between group members
- Shared instances of concrete labor
- Reciprocal give/potential structures

5. **Implications for System Design**
This means that while we allow multiple types (including multiple contributors) for instances, the truth of these relationships is verified through:
- Actual mutual recognition between contributors
- Concrete instances of shared labor
- Reciprocal point allocation
- Material coordination in practice

The root node can propose groupings, but cannot enforce them against the reality of mutual recognition and concrete collaboration. This is a powerful example of how the system naturally tends toward truth in social relations!


# So we can make the addContributor to be adhoc on the fly! What kind of interaction?
*/

class Node {
    constructor(name, parent = null, types = [], isContributor = false) {
        this.name = name;
        this.parent = parent;
        this.points = 0;
        this.children = new Map();  // Node -> Map(contributor -> points)
        this.totalChildPoints = 0;
        this.isContributor = isContributor;
        
        // Map of type -> Set of instances
        this.typeIndex = parent ? parent.getRoot().typeIndex : new Map();
        
        // Add initial types if provided
        if (types.length > 0) {
            types.forEach(type => {
                this.addType(type);
                // If any type is a contributor, this instance can't have children
                if (type.isContributor) {
                    this.isContributor = true;
                }
            });
        }
    }

    // Add this node as an instance of the given type
    addType(type) {
        console.log(`\nAdding type ${type.name} to ${this.name}`);
        
        const root = this.getRoot();
        if (!root.typeIndex.has(type)) {
            root.typeIndex.set(type, new Set());
        }
        root.typeIndex.get(type).add(this);
        
        // Debug verification
        console.log(`After adding type ${type.name} to ${this.name}:`);
        console.log(`- Instances:`, Array.from(root.typeIndex.get(type)).map(i => i.name));
        
        return this;
    }

    

    addChild(name, points = 0, types = []) {
        if (this.parent && this.isContributor) {
            throw new Error(`Node ${this.name} is an instance of a contributor and cannot have children.`);
        }

        const child = new Node(name, this);
        
        // Ensure types are properly added
        if (Array.isArray(types)) {
            types.forEach(type => child.addType(type));
        } else if (types) {  // Handle single type case
            child.addType(types);
        }
        
        this.children.set(child, new Map());
        if (points > 0) {
            child.setPoints(points);
        }
        return child;
    }

    removeChild(name) {
        const child = this.children.get(name);
        if (child) {
            // Update points
            if (child.points > 0) {
                this.totalChildPoints -= child.points;
            }
            // Remove from type index
            this.getRoot().typeIndex.forEach(instances => {
                instances.delete(child);
            });
            // Remove from children
            this.children.delete(name);
        }
        return this;
    }

    getRoot() {
        return this.parent ? this.parent.getRoot() : this;
    }

    // Helper method to get all types in the system
    getTypes() {
        return Array.from(this.getRoot().typeIndex.keys());
    }

    // Helper method to get all instances of a given type
    getInstances(type) {
        return this.getRoot().typeIndex.get(type) || new Set();
    }

    setPoints(points) {
        const diff = points - this.points;
        if (this.parent) {
            this.parent.totalChildPoints += diff;
        }
        this.points = points;
        return this;
    }

    getWeight() {
        if (!this.parent) return 1;
        return this.parent.totalChildPoints === 0 ? 0 :
            (this.points / this.parent.totalChildPoints) * this.parent.getWeight();
    }

    // Calculate how much this node recognizes a given type/node
    shareOfGeneralContribution(node) {
        // Use indexed lookup instead of tree traversal
        const instances = this.getRoot().typeIndex.get(node) || new Set();
        return Array.from(instances).reduce((sum, instance) => {
            return sum + instance.getWeight();
        }, 0);
    }

    mutualShareOfGeneralContribution(node) {
        console.log(`\nDEBUG: Calculating mutual recognition between ${this.name} and ${node.name}`);
        console.log(`- ${this.name} types:`, this.getTypes().map(t => t.name));
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
    mutualGeneralContribution() {
        // Get all types that have instances in the system
        const types = Array.from(this.getRoot().typeIndex.keys())
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
        return this.mutualGeneralContribution().get(node);
    }
}


const environmentalist = new Node('environmentalist');  // most abstract
const activist = new Node('activist', null, [environmentalist]);  // type of environmentalist
const researcher = new Node('researcher', null, [environmentalist]);  // another type
const educator = new Node('educator', null, [environmentalist]);  // another type

// Our main nodes with multiple types
const whalewatch = new Node('whalewatch', null, [activist], true);  // is a contributor
const greenpeace = new Node('greenpeace', null, [researcher], true);  // is a contributor

const community = new Node('community');

// Adding each value as children of the community node
const replacingUsedUpMeansOfProduction = community.addChild("Replacing Used-up Means of Production", 80)
// [
//     'economic necessity', 
//     'determined by available means', 
//     'not calculable by equity'
// ]
// .addOperation("maintains productive capacity")
//  .addOperation("ensures continuous operation");

const expansionOfProduction = community.addChild("Expanding Production", 60)
// [
//     'economic necessity', 
//     'determined by available means', 
//     'future-oriented'
// ]
// .addOperation("enables growth")
// .addOperation("increases productive capacity");

const insuranceReserves = community.addChild("Hedging Risks", 90)
// [
//     'based on probability computation', 
//     'risk management', 
//     'collective security'
// ]
// .addOperation("provides resilience")
// .addOperation("protects against disruptions");

//const administration = community.addChild("Administration", 30)
// [
//     'minimized overhead', 
//     'efficiency-seeking', 
//     'decreases over time'
// ]
// .addOperation("enables coordination")
// .addOperation("reduces bureaucracy");

const socialServices = community.addChild("Common Satisfaction of Needs", 70)
// [
//     'collective provision', 
//     'growing importance', 
//     'social investment'
// ]
// .addOperation("builds social capacity")
// .addOperation("increases collective capabilities");

const welfare = community.addChild("Welfare", 65)
// [
//     'social necessity', 
//     'collective responsibility', 
//     'universal security'
// ]

// .addOperation("ensures dignity")
// .addOperation("maintains social cohesion");

const consumption = community.addChild("Individual Means of Consumption", 40)
// [
//     'final distribution', 
//     'individual allocation', 
//     'based on contribution'
// ]

//.addOperation("enables reproduction of labor")
// .addOperation("satisfies individual needs");

// Example: Displaying the community node and its children
console.log(community);

// 1. Replacing used up Means of Production (Basic Reproduction)
replacingUsedUpMeansOfProduction.addChild("Natural Materials", 30);
replacingUsedUpMeansOfProduction.addChild("Machine Maintenance", 25);
replacingUsedUpMeansOfProduction.addChild("Infrastructure Repair", 25);
replacingUsedUpMeansOfProduction.addChild("Energy Resources", 20);

// 2. Expansion of Production (Extended Reproduction)
expansionOfProduction.addChild("New Technologies", 15);
expansionOfProduction.addChild("Additional Facilities", 12);
expansionOfProduction.addChild("Research and Development", 13);
expansionOfProduction.addChild("Training Programs", 10);

// 3. Insurance / Reserves
insuranceReserves.addChild("Natural Disasters", 25);
insuranceReserves.addChild("Emergency Supplies", 25);
insuranceReserves.addChild("Equipment Replacement", 25);
insuranceReserves.addChild("Strategic Reserves", 25);

// 5. Common Satisfaction of Needs
socialServices.addChild("Healthcare", 15);
const education = socialServices.addChild("Education", 15);
socialServices.addChild("Public Transport", 12);
socialServices.addChild("Cultural Facilities", 11);

// 6. Welfare
welfare.addChild("Disability Support", 15);
welfare.addChild("Elder Care", 13);
welfare.addChild("Child Support", 13);
welfare.addChild("Emergency Aid", 12);

// 7. Consumption
consumption.addChild("Basic Necessities", 8);
consumption.addChild("Housing", 5);
consumption.addChild("Personal Development", 4);
consumption.addChild("Leisure Activities", 3);

// Under Common Satisfaction of Needs (Education)
const whalewatchPrograms = education.addChild("Whalewatch Programs", 5, [whalewatch, educator])
const greenpeaceWorkshops = education.addChild("Greenpeace Workshops", 9, [greenpeace, educator]);

// Under Research and Development (within Expansion of Production)
expansionOfProduction.addChild("Marine Research", 7, [whalewatch, researcher])
expansionOfProduction.addChild("Environmental Impact Studies", 6, [greenpeace, researcher]);

// Under Emergency Supplies (within Insurance/Reserves)
insuranceReserves.addChild("Marine Emergency Response", 10, [whalewatch, activist])
insuranceReserves.addChild("Environmental Crisis Response", 15, [greenpeace, activist]);



    // whalewatch's recognition of community
    const whalewatchgive = whalewatch.addChild('🌳 give', 80);
    const whalewatchpotential = whalewatchgive.addChild('🔮 potential', 40);
    const communityInwhalewatchpotential = whalewatchpotential.addChild('community', 15, community);

    // greenpeace's recognition of community
    const greenpeacegive = greenpeace.addChild('🌳 give', 80);
    const greenpeacepotential = greenpeacegive.addChild('🔮 potential', 40);
    const communityIngreenpeacepotential = greenpeacepotential.addChild('community', 15, community);

    const activistgive = activist.addChild('🌳 give', 80);
    const activistpotential = activistgive.addChild('🔮 potential', 40);
    const communityInactivistpotential = activistpotential.addChild('community', 15, community);

// thus what distinguishes a type from a contributor is mutual-recognition!


    // Calculate shares at different type levels
    console.log('\nShares by type level:');
    console.log('Environmentalist share:', community.shareOfGeneralContribution(environmentalist));
    console.log('Activist share:', community.shareOfGeneralContribution(activist));
    console.log('Educator share:', community.shareOfGeneralContribution(educator));
    console.log('Researcher share:', community.shareOfGeneralContribution(researcher));
    console.log('whalewatch specific share:', community.shareOfGeneralContribution(whalewatch));
    console.log('greenpeace specific share:', community.shareOfGeneralContribution(greenpeace));

    // Calculate mutual recognition and generalMutualContribution
    console.log('\nMutual Recognition:');
    console.log('With whalewatch:', community.mutualShareOfGeneralContribution(whalewatch));
    console.log('\nFinal mutualGeneralContribution:');
    console.log(community.mutualGeneralContribution());
    console.log(community);