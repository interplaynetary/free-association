/*
# Commons Tree

Our commons-tree is has a root representing the whole of socially-necessary-production.
Each branch of the tree is a particular branch of production.

The tree is a composition of labor and definite quantities of useful-objects necessary for their activities; (compositions of labor/means of production)

The proportions in the tree represents the social-labour time that are required to (re)produce:
- composed-labor 
- the appearence of useful-objects in a production-process: this is the social-labor-required to provision those definite quantities of useful objects for use in the act of production when they are needed! (Means of Production, held in common! These objects come that portion of the social-stock that serves as fresh-means-of-production)

Our communal stock-book (inventory) consists of definite quantities of objects of utility that belong to us.

The total product of our community is a social product. 
- One portion serves as fresh means of production and remains social. 
    - This is a stock of objects which can be used to provision the use of those definite quantities of useful objects for use in the act of production when they are needed (think of it like a object-library+delivery-service)
- But another portion is consumed by the members as means of subsistence. 
        A distribution of this portion amongst them is consequently necessary. 
        The mode of this distribution will vary with the productive organisation of the community, and the degree of historical development attained by the producers. 
    - Let us assume, that the share of each individual producer in the means of subsistence is determined by his shareOfCommonsProduction. 
    
    shareOfCommonsProduction:
        - As a recursive proportion of the particular part of a particular branch:
            - Each node's share is its proportion of its parent's share
            - This creates a chain of proportions from leaf to root
            - Individual labor becomes part of composed labor
            - Composed labor itself becomes part of larger compositions
            - Until we reach the total social labor at the root
            - Thus each share is simultaneously:
                - A part of its immediate parent branch
                - A part of the whole social product
                
        - This recursive proportion would then play a double part:
            - Its apportionment in accordance with a definite social plan maintains the proper proportion between the different kinds of work to be done and the various wants of the community. 
            - On the other hand, it also serves as a measure of the portion of the common labour borne by each individual, and of his share in the part of the total product destined for individual consumption. 

All the relations between Society and the objects that form this wealth of our own creation, 
are here so simple and clear as to be intelligible without exertion.

And yet those relations contain all that is essential to the determination of value.

The social relations of the individual producers, 
with regard both to their labour and to its products, 
are in this case perfectly simple and intelligible,
and that with regard not only to production but also to distribution.

---- Why time?
The labour power of each individual, by its very nature, operates in this case merely  as a definite portion of the whole labour power of society, 
and therefore, the measure of the expenditure of individual labour power by its duration, appears here by its very nature as a social character of their labour.

Let us now picture to ourselves, by way of change, a community of free individuals, carrying on their work with the means of production in common, 
in which the labour power of all the different individuals is consciously applied as the combined labour power of the community.

It is of great interest that this form of recursive scalar and proportional composition - which necessarily gets smaller as one descends further into the tree of compositions of operations - is only a Universal pattern with respect to time, as not all compositions of operations have diminishing scalar quantities when one calculates the recursive-proportions with recursive-absolute-values attempting to view them as units of concrete resources!

The key insight is that time has a unique mathematical property: its subdivisions always maintain their proportional relationship to the whole, which makes it the perfect universal measure for social production. 

This is why we use time (both concrete and socially validated) as the basis for our system rather than tracking physical resources or ownership.

We are not think about how we allocate our time in terms of product/object production but in terms concrete-time spent in concrete-particular activities so as to fulfill all needs in the whole of socially-necessary-production.

All the branches of the commons-tree are the branches of production from the abstract-universal of abstract labor-time to be distributed into concrete-particular labor-time. Abstract-time to concrete-time.

We live in time! And by giving a form to our own labor-time we sculpt the labor-power of humanity.
*/

class UsefulTypeOfThing {
    constructor(name, typeProperties, unitsOfMeasure = []) {
        this.name = name;
        this.typeProperties = typeProperties;
        this.unitsOfMeasure = unitsOfMeasure;
        this.depreciation = new Map(); // Map(user -> %depreciationPerUse)
    }
}

class UsefulThing {
    constructor(name, type, instanceProperties) {
        this.name = name;
        this.type = type;
        this.instanceProperties = instanceProperties;
        this.uses = new Map();
    }
    recordUse(user) {
        this.uses.set(user, this.uses.get(user) + 1);
    }
}

// new Map() type -> instances

class Commons {
    constructor(name, parent = null, individualWorkingDayHours = 0, usefulObject = undefined, definiteQuantity = undefined) {
        this.name = name;
        this.parent = parent;
        this.commons = this.parent ? this.parent.commons : this;

        this.laborers = this.commons === this ? new Map() : this.commons.laborers;
        this.individualWorkingDayHours = individualWorkingDayHours;

        // stockBook is a map of objects of utility that belong to us, 
        this.stockBook = this.commons === this ? new Map() : this.commons.stockBook;
        
        // If Commons is a type of usefulObject of a definite quantity, we can refer to it in our stockBook as having a supply (that can be drawn down on!)
        this.usefulObject = usefulObject;
        this.definiteQuantity = definiteQuantity; 
        // the costs of a usefulObject in the stockBook, is based on this node's (determinate quantity of a useful-object's) shareOfCommonsProduction
        // i.e. the costs is based on the socially-necessary-labor-time of the useful-object's production!

        // we can divide the portions for means-of-production because it is the sum of all the quantities of those useful-objects used in production!

        // therefore the determinate quantities are themselves the definite-quantities of the total-social-product
        // Some portion of the stockBook is meansOfProduction, some is meansOfSubsistence

        // some childrens are DeterminateQuantities of Types of UsefulThings, some are labor-power
        // means of production is in common! And compositions are the share in that DeterminateQuantities of Types of UsefulThings!
        // some labor-power is composed! Others are not!

        // Rest of the initialization
        this.points = 0;
        this.composes = new Map();  // Node -> Map(commons -> points) // children
        this.individual = (this.composes.size === 0 && this.individualWorkingDayHours > 0) ? true : false;
    }

    declareSocialProduct(usefulObject, quantity) {
        // we can divide the portions for means-of-production because it is the sum of all the quantities of those useful-objects used in production!
        // therefore the determinate quantities are themselves the definite-quantities of the total-social-product
        this.addToStock(usefulObject, quantity);
    }

    get totalSocialProduct() {
        return Array.from(this.stockBook.values()).reduce((sum, quantity) => sum + quantity, 0);
    }

    addToStock(usefulObject, quantity) {
        // we can divide the portions for means-of-production because it is the sum of all the quantities of those useful-objects used in production!
        // therefore the determinate quantities are themselves the definite-quantities of the total-social-product
        const currentStock = this.stockBook.get(usefulObject) || 0;
        this.stockBook.set(usefulObject, currentStock + quantity);
    }

    get availableForIndividualConsumption() {
        if (this !== this.commons) return 0;
        
        // Total social product minus production requirements
        const totalProduct = this.totalSocialProduct;
        const productionNeeds = this.calculateTotalProductionRequirements();
        return totalProduct - productionNeeds;
    }

    get productionCost() {
        if (!this.usefulObject) return 0;
        
        // The cost of producing this definite quantity of useful object
        // is its share of the total social labor time!
        return this.shareOfCommonsProduction;
    }

    get unitCost() {
        // Cost per unit = total labor time / quantity produced
        return this.definiteQuantity > 0 
            ? this.productionCost / this.definiteQuantity 
            : 0;
    }

    getShareOfStock(usefulObject) {
        // we need to account for drawing down from the stock! -> according to the unitCost!

        // He draws from the social stock of means of consumption as much as the same amount of labor cost. 
        // The same amount of labor which he has given to society in one form, he receives back in another.
        const totalStock = this.stockBook.get(usefulObject) || 0;
        return totalStock * this.shareOfCommonsProduction;
    }

    drawFromStock(usefulObject, quantity) {
        const cost = this.unitCost * quantity;
        this.addToStock(usefulObject, -cost);
    }

      // social validation of socially-necessary-labor-time of labor/means-of-production compositions
    // Convert socialWorkingDay to a getter
    get socialWorkingDay() {
        return this.commons === this 
            ? Array.from(this.laborers.values()).reduce((sum, hours) => sum + hours, 0)
            : this.commons.socialWorkingDay;
    }

    get totalCompositionPoints() {
        return Array.from(this.composes.values())
            .reduce((sum, pointsMap) => 
                sum + Array.from(pointsMap.values())
                    .reduce((a, b) => a + b, 0), 0);
    }

    get shareOfParentTotalCompositionPoints() {
        // this determines one's time-share of the MOP of the parent!

        // this is also the weight of one's point-allocations in the parent!
        // one can not allocate points to oneself in parent!
        if (!this.parent) return 1;
        
        const myPoints = Array.from(this.parent.composes.get(this)?.values() || [])
            .reduce((a, b) => a + b, 0);
            
        return myPoints / this.parent.totalCompositionPoints;
    }

    get shareOfCommonsProduction() {
        // share of total social-labor, share of total social-product
        // recursive-proportions self to in commons by working backwards!

        // recursively from self to rootCommons! 
        //this.points / this.parent.totalCompositionPoints;
        // then multiplied by commons.socialWorkingDay()

        if (this.individualWorkingDayHours > 0) {
            return this.individualWorkingDayHours;
        }
        
        if (!this.parent) {
            return this.socialWorkingDay;
        }

        // Recursive calculation using shareOfParentTotalCompositionPoints
        return this.shareOfParentTotalCompositionPoints * this.parent.shareOfCommonsProduction;
    }

    get organicComposition(){
        // of all this composes!
        // What is the ratio of meansOfProduction to labor-power?
    }
}


// Test example for Commons proportional calculations
function testCommonsProportions() {
    console.log("=== Testing Commons Proportional Calculations ===\n");

    // Create root commons
    const rootCommons = new Commons("Root Commons");
    
    // Add some individual laborers with different working hours
    const alice = new Commons("Alice", rootCommons, 8);
    const bob = new Commons("Bob", rootCommons, 6);
    const charlie = new Commons("Charlie", rootCommons, 4);
    
    // Register laborers in root commons
    rootCommons.laborers.set(alice, 8);
    rootCommons.laborers.set(bob, 6);
    rootCommons.laborers.set(charlie, 4);
    
    // Create some sub-commons with compositions
    const bakery = new Commons("Bakery", rootCommons);
    rootCommons.composes.set(bakery, new Map([
        [alice, 40],
        [bob, 60]
    ]));
    
    const farm = new Commons("Farm", rootCommons);
    rootCommons.composes.set(farm, new Map([
        [bob, 30],
        [charlie, 70]
    ]));
    
    // Print initial setup
    console.log("Total Social Working Day:", rootCommons.socialWorkingDay, "hours");
    console.log("\nIndividual Working Days:");
    rootCommons.laborers.forEach((hours, laborer) => {
        console.log(`${laborer.name}: ${hours} hours`);
    });
    
    // Test proportional calculations
    console.log("\nShares of Commons Production:");
    
    [alice, bob, charlie, bakery, farm].forEach(node => {
        const share = node.shareOfCommonsProduction;
        console.log(`${node.name}'s share: ${(share).toFixed(2)} hours (${(share/rootCommons.socialWorkingDay * 100).toFixed(1)}%)`);
    });
    
    // Verify that shares at each level sum to 100%
    console.log("\nVerifying Level Totals:");
    
    // Check individual level
    const individualTotal = [alice, bob, charlie]
        .reduce((sum, node) => sum + node.shareOfCommonsProduction, 0);
    console.log(`Individual level total: ${(individualTotal/rootCommons.socialWorkingDay * 100).toFixed(1)}%`);
    
    // Check commons level
    const commonsTotal = [bakery, farm]
        .reduce((sum, node) => sum + node.shareOfCommonsProduction, 0);
    console.log(`Commons level total: ${(commonsTotal/rootCommons.socialWorkingDay * 100).toFixed(1)}%`);
}

// Run the test
testCommonsProportions();