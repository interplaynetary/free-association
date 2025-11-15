// Now lets imagine:
/*
[] - distributed point-allocation: root.shareOfMutualGeneralContribution(contributor) 
determines a contributor's share of each rectangle 
(represented by an inner yellow rectangle scaled down to your % of its area)! 
Each smaller rectangle is a % of the total-points of the parent, 
and is a % of your total-allocations (yellow rectangles) across children! 

By clicking and holding a rectangle, you can see change the relative proportions of your yellow-rectangles!

The size of children of the parent is the total of all yellow-rectangles from all allocators!

keep track of points-distributed by who? Calculate share of points-left-to-distribute!
    Don't allow distribution to self? or-any-branches self has instances in?
*/

class Node {
    constructor(name, parent = null, type = null, isContributor = false) {
        this.name = name;
        this.parent = parent;
        this.points = 0;
        this.children = new Map();  // Node -> Map(contributor -> points)
        // using this modified structure, we can now calculate the shareOfContributorsPointsInChild(contributor,child)
        // shareOfContributorsPointsInChild()
        // shareOfContributorInChild() // weighted by shareOfGeneralContribution
        this.totalChildPoints = 0;  // Sum of direct children's points
        this.isContributor = isContributor;
        
        // Map of type -> Set of instances
        this.typeIndex = parent ? parent.getRoot().typeIndex : new Map();
        
        // Add type if provided
        if (type) {
            this.addType(type);
            // If the type is a contributor, this instance can't have children
            if (type.isContributor) {
                this.isContributor = true;
            }
        }
    }

    // Add this node as an instance of a single type
    addType(type) {
        console.log(`\nAdding type ${type.name} to ${this.name}`);
        
        // Remove from any existing type indices first
        this.getRoot().typeIndex.forEach((instances, existingType) => {
            if (instances.has(this)) {
                console.log(`Removing ${this.name} from type ${existingType.name}`);
                instances.delete(this);
            }
        });
        
        // Add to new type index
        const root = this.getRoot();
        if (!root.typeIndex.has(type)) {
            root.typeIndex.set(type, new Set());
        }
        root.typeIndex.get(type).add(this);
        
        console.log(`- ${type.name} instances:`, Array.from(root.typeIndex.get(type)).map(i => i.name).join(', '));
        return this;
    }

    addChild(name, points = 0, type = null) {
        // Only check isContributor restriction if this node is an instance (has a parent)
        // Contributors can have their own children when acting as a root
        if (this.parent && this.isContributor) {
            throw new Error(`Node ${this.name} is an instance of a contributor and cannot have children.`);
        }

        const child = new Node(name, this, type);
        this.children.set(child, new Map());        
        if (points > 0) {
            child.setPoints(points);
        }
        return child;
    }

    removeChild(node) {
        const child = this.children.get(node);
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

    getChildProportionOfSelf(child) {
        // Sum all contributor points for this child
        const childPoints = this.children.get(child);
        if (!childPoints) return 0;
        
        let childTotal = Array.from(childPoints.values()).reduce((sum, points) => sum + points, 0);
        
        // Sum all points across all children
        let total = 0;
        for (const [_, pointsMap] of this.children) {
            total += Array.from(pointsMap.values()).reduce((sum, points) => sum + points, 0);
        }
        
        return total > 0 ? childTotal / total : 0;
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

    setContributorPoints(child, contributor, points) {
        let contributorPoints = this.children.get(child);
        if (!contributorPoints) {
            contributorPoints = new Map();
            this.children.set(child, contributorPoints);
        }
        contributorPoints.set(contributor, points);
    }

    getContributorPoints(child, contributor) {
        const contributorPoints = this.children.get(child);
        if (contributorPoints && contributorPoints.has(contributor)) {
            return contributorPoints.get(contributor);
        }
        return 0;
    }

    // Method to calculate share of contributor's points in a child
    shareOfContributorsPointsInChild(contributor, child) {
        // Calculate total points for this contributor across ALL children
        let contributorTotalPoints = 0;
        for (const [childNode, pointsMap] of this.children.entries()) {
            contributorTotalPoints += pointsMap.get(contributor) || 0;
        }
        
        // Get points for specific child
        const contributorPointsInChild = this.getContributorPoints(child, contributor) || 0;
        
        // Debug logging
        console.log(`Points for ${contributor.name} in ${child.name}: ${contributorPointsInChild}`);
        console.log(`Total points for ${contributor.name}: ${contributorTotalPoints}`);
        
        return contributorTotalPoints > 0 ? contributorPointsInChild / contributorTotalPoints : 0;
    }
    contributorShareOfChildInSelf(contributor, child){
        return this.shareOfContributorsPointsInChild(contributor, child) * this.shareOfGeneralContribution(contributor) * this.getChildProportionOfSelf(child);
    }
}


const environmentalist = new Node('environmentalist');  // not a contributor
const activist = new Node('activist', null, environmentalist);  // not a contributor
const researcher = new Node('researcher', null, environmentalist);  // not a contributor
const educator = new Node('educator', null, environmentalist);  // not a contributor

// Our main nodes with multiple types
const whalewatch = new Node('whalewatch', null, activist, true);  // is a contributor
const greenpeace = new Node('greenpeace', null, researcher, true);  // is a contributor

const community = new Node('community');

// Adding each value as children of the community node
const replacingUsedUpMeansOfProduction = community.addChild("Replacing Used-up Means of Production", 80)

const expansionOfProduction = community.addChild("Expanding Production", 60)

const insuranceReserves = community.addChild("Hedging Risks", 90)

const socialServices = community.addChild("Common Satisfaction of Needs", 70)

const welfare = community.addChild("Welfare", 65)

const consumption = community.addChild("Individual Means of Consumption", 40)

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

// Instances of Types

// Under Common Satisfaction of Needs (Education)
const whalewatchPrograms = education.addChild("Whalewatch Programs", 5, whalewatch); // should pass
// whalewatchPrograms.addChild("Some Child"); // should fail - whalewatchPrograms is instance of contributor

const greenpeaceWorkshops = education.addChild("Greenpeace Workshops", 5, greenpeace);

// Under Research and Development (within Expansion of Production)
const marineResearch = expansionOfProduction.addChild("Marine Research", 7, whalewatch);
const environmentalImpactStudies = expansionOfProduction.addChild("Environmental Impact Studies", 6, greenpeace);

// Under Emergency Supplies (within Insurance/Reserves)
const marineEmergencyResponse = insuranceReserves.addChild("Marine Emergency Response", 10, whalewatch);
const environmentalCrisisResponse = insuranceReserves.addChild("Environmental Crisis Response", 15, greenpeace);

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

// ... existing code ...

// Setting contributor points for whalewatch's activities
education.setContributorPoints(whalewatchPrograms, whalewatch, 100);  // Heavy focus on education
expansionOfProduction.setContributorPoints(marineResearch, whalewatch, 50);  // Moderate research focus
insuranceReserves.setContributorPoints(marineEmergencyResponse, whalewatch, 30);  // Some emergency work

// Setting contributor points for greenpeace's activities
education.setContributorPoints(greenpeaceWorkshops, greenpeace, 60);  // Moderate education focus
expansionOfProduction.setContributorPoints(environmentalImpactStudies, greenpeace, 80);  // Heavy research focus
insuranceReserves.setContributorPoints(environmentalCrisisResponse, greenpeace, 90);  // Major emergency response focus

// Let's analyze the distribution
console.log('\nAnalyzing contributor distributions:');

// Whalewatch analysis
console.log('\nWhalewatch distribution:');
console.log('Share in Education:', education.shareOfContributorsPointsInChild(whalewatch, whalewatchPrograms)); // Should be ~0.56 (100/180)
console.log('Share in Research:', expansionOfProduction.shareOfContributorsPointsInChild(whalewatch, marineResearch)); // Should be ~0.28 (50/180)
console.log('Share in Emergency:', insuranceReserves.shareOfContributorsPointsInChild(whalewatch, marineEmergencyResponse)); // Should be ~0.17 (30/180)

// Greenpeace analysis
console.log('\nGreenpeace distribution:');
console.log('Share in Education:', education.shareOfContributorsPointsInChild(greenpeace, greenpeaceWorkshops)); // Should be ~0.26 (60/230)
console.log('Share in Research:', expansionOfProduction.shareOfContributorsPointsInChild(greenpeace, environmentalImpactStudies)); // Should be ~0.35 (80/230)
console.log('Share in Emergency:', insuranceReserves.shareOfContributorsPointsInChild(greenpeace, environmentalCrisisResponse)); // Should be ~0.39 (90/230)

// Let's calculate effective ownership shares
console.log('\nEffective ownership shares:');

console.log('\nWhalewatch effective ownership:');
console.log('In Education Programs:', education.contributorShareOfChildInSelf(whalewatch, whalewatchPrograms));
console.log('In Marine Research:', expansionOfProduction.contributorShareOfChildInSelf(whalewatch, marineResearch));
console.log('In Emergency Response:', insuranceReserves.contributorShareOfChildInSelf(whalewatch, marineEmergencyResponse));

console.log('\nGreenpeace effective ownership:');
console.log('In Workshops:', education.contributorShareOfChildInSelf(greenpeace, greenpeaceWorkshops));
console.log('In Impact Studies:', expansionOfProduction.contributorShareOfChildInSelf(greenpeace, environmentalImpactStudies));
console.log('In Crisis Response:', insuranceReserves.contributorShareOfChildInSelf(greenpeace, environmentalCrisisResponse));
