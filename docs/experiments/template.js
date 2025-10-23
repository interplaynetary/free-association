/*
Ok so when compositions of operations are recursively-proportioned to be recursively-scaled which is the commons node's distribution of concrete-socially-necessary labor-time

The totality of the scalar quantities of commons nodes across all of society represents a definite amount of socially-necessary-labor-time for its (re)-production on average!

It represents a definite quantity of time!

It is of great interest that this form of recursive scalar and proportional composition 
- which necessarily gets smaller as one descends further into the tree of compositions of operations - 
is only a Universal pattern with respect to time, 
as not all compositions of operations have diminishing scalar quantities 
when one calculates the recursive-proportions with recursive-absolute-values
when viewed as units of concrete resources!

Only time has this particular feature! That it can be subdivided into smaller parts, and all the divisions of those divisions will always stand 
in relation to their larger composition as a % of their contributesTowards they were divided from! 

Time's unique property: subdivisions always relate proportionally to contributesTowards

concrete-time is contained within abstract-time

This also goes hand in hand with the idea that the means of production are held in common! 

We no longer refer to the ownership of resources or the generation of products as isolated objects! We no longer view creation in terms of the creation of quantities of objects!

We instead look at the act of social-production as a distribution of concrete-labor-time and a resulting concrete-mutually-validated-necessary-labor-time in general (on average) validated via general mutual-recognition of general-mutual-contribution

The concrete-mutually-validated-necessary-labor-time for its production in general (on average) is distributed across the general-mutual-recognition-distribution.

The key insight is that time has a unique mathematical property: its subdivisions always maintain their proportional relationship to the whole, which makes it the perfect universal measure for social production. This is why we use time (both concrete and socially validated) as the basis for our system rather than tracking physical resources or ownership.

We are not think about how we allocate our time in terms of product/object production but in terms concrete-time spent in concrete-particular activities in order to fulfill the needs of oneself and others in such a way that is socially validated via mutual-contribution

We are not thinking in terms of the production of quantities of objects but the socially-validated necessary-labor-time in compositions of concrete-labor needed to fulfill the social-and-material-dependencies (the needs/values/goals) necessary to reproduce the labour power capable of capable of fulfilling all social-indivudal desires.

All the branches of the tree from the commons node are the branches of production from the abstract-universal of abstract labor-time to be distributed into concrete-particular labor-time. Abstract-time to concrete-time

We live in time! And by giving a form to our own labor-time we sculpt the labor-power of humanity.



The composition of concrete-socially-validated-necessary-labor-times needed to socially-reproduce 
a determinate-quantity of a the concrete-labor & concrete-labour-power capable of transforming social-material reality 
such as to fulfill the social-and-material-dependencies (the needs/values/goals) 
necessary to to fulfill all social-indivudal desires.

// of all of society is represented across the totality of commons nodes!


*/



class CommonLabor {
    constructor(commons, desiredCommonsQuantity = undefined, actualCommonsQuantity = undefined, dependencies = [], points = 0, contributesTowards, type = null) {
        this.commons = commons; // Commons of the tree of production

        this.members = (this.commons === this) ? desiredCommonsQuantity : this.commons.desiredCommonsQuantity

        this.desiredCommonsQuantity = (this.commons === this) ? desiredCommonsQuantity : this.commons.desiredCommonsQuantity // this can be undefined to have unquantifed outputs
        
        // the means of production are utilized as time-shares 
        // exchanged for by one's socially-validated-necessary-labor-time!
        this.actualCommonsQuantity = (this.commons === this) ? actualCommonsQuantity : this.commons.actualCommonsQuantity
        // This creates a social-stock that can be drawn down upon! 
        // with Labor-Certificates? - or rather with a % of one's quantum of labor-time!
        
        /*
        Hospital Beds:
        - Desired: 1000
        - Actual: 800
        → Need more production

        Education:
        - Desired: undefined
        - Actual: undefined
        → Quality not quantity!

        Food:
        - Desired: 10000 meals
        - Actual: 12000 meals
        → Stock for future!
        */

        /*
            Concrete production of determinate quantities (when defined)
            "We need 1000 hospital beds"
            "We need 50 schools"
            "We need 10,000 meals"
            Non-quantifiable social activities (when undefined)
            Education and development
            Cultural activities
            Care work
            Community building
        */

        /*
        The distribution of the work within the family, and the regulation of the labour time of the several members, depend as well upon differences of age and sex as upon natural conditions varying with the seasons. The labour power of each individual, by its very nature, operates in this case merely as a definite portion of the whole labour power of the family, and therefore, the measure of the expenditure of individual labour power by its duration, appears here by its very nature as a social character of their labour.

        We will assume, but merely for the sake of a parallel with the production of commodities, that the share of each individual producer in the means of subsistence is determined by his labour time. 

        **Labour time would, in that case, play a double part.**

        Its apportionment in accordance with a definite social plan maintains the proper proportion between the different kinds of work to be done and the various wants of the community.  On the other hand, it also serves as a measure of the portion of the common labour borne by each individual, and of his share in the part of the total product destined for individual consumption. 
        
        
        Within the co-operative society based on common ownership of the means of production, the producers do not exchange their products; just as little does the labor employed on the products appear here as the value of these products, as a material quality possessed by them, since now, in contrast to capitalist society, individual labor no longer exists in an indirect fashion but directly as a component part of total labor. The phrase "proceeds of labor", objectionable also today on account of its ambiguity, thus loses all meaning.

        What we have to deal with here is a communist society, not as it has developed on its own foundations, but, on the contrary, just as it emerges from capitalist society; which is thus in every respect, economically, morally, and intellectually, still stamped with the birthmarks of the old society from whose womb it emerges. Accordingly, the individual producer receives back from society – after the deductions have been made – exactly what he gives to it. What he has given to it is his individual quantum of labor. For example, the social working day consists of the sum of the individual hours of work; the individual labor time of the individual producer is the part of the social working day contributed by him, his share in it. He receives a certificate from society that he has furnished such-and-such an amount of labor (after deducting his labor for the common funds); and with this certificate, he draws from the social stock of means of consumption as much as the same amount of labor cost. The same amount of labor which he has given to society in one form, he receives back in another.

        Here, obviously, the same principle prevails as that which regulates the exchange of commodities, as far as this is exchange of equal values. Content and form are changed, because under the altered circumstances no one can give anything except his labor, and because, on the other hand, nothing can pass to the ownership of individuals, except individual means of consumption. But as far as the distribution of the latter among the individual producers is concerned, the same principle prevails as in the exchange of commodity equivalents: a given amount of labor in one form is exchanged for an equal amount of labor in another form.
        */

        // The relationship between concrete and abstract time
        // is mediated through Social Validation via point-distribution!
        
        // Track which concrete labors require this labor
        this.contributesTowards = contributesTowards // Dependents (parents)

        // 1. The goal: reproduce concrete-labors/powers/capacity itself
        // Recursive composition of abstract-time into concrete time
        // Recursive composition of proportions of abstract-time (of a cyce in the commons node - working day) for social will realization
        this.dependencies = new Map(dependencies);  // Node -> Map(labors -> points)
        this.totalPointsOfAllDirectDependencies = 0; // Total social-validation-points

        // 2. Social validation of that labor
        this.points = points;  // social expression of average concrete-labor-time proportions

        // Two possible validation approaches:
        // - network-centrality for social-validation (when done centralized)
        // - general-mutual-contribution when done peer 2 peer
        
        // 4. The goal: self-actualize
        this.isContributor = this.contributesTowards ? false : true; // Individuals who contribute labor power

        // Indexes for efficient lookups in the labor network
        this.typeIndex = this.contributesTowards ? this.contributesTowards.getConcreteLabors().typeIndex : new Map();
        this.contributorIndex = this.contributesTowards ? this.contributesTowards.getConcreteLabors().contributorIndex : new Map();

        // Add type if provided
        if (type) {
            this.addType(type);
            if (type.isContributor) {
                this.isContributor = true;
            }
        }
    }

    // 3. socially-validated average labor-time is derived from social validation
    // the labour time that definite quantities of those objects have, on an average, cost us.
    // actual labor time emerges from validation
    getSociallyValidatedNecessaryLaborTime() {
        // which is one's share of the total social labor-time and one's share of the total-social-product
        if (!this.parent) return this.scalarQuantity || 1;
        
        // Weight now incorporates both points-based proportion AND scalar quantities
        const pointsProportion = this.parent.totalChildPoints === 0 ? 0 :
            (this.points / this.parent.totalChildPoints);
            
        // Scale by ratio of scalar quantities
        const scalarProportion = this.scalarQuantity / this.parent.scalarQuantity;
        
        return pointsProportion * scalarProportion * this.parent.getWeight();
    }

    organicComposition() {
        // Instead of c/v (constant capital / variable capital)
        // We calculate ratio of means of production time-shares to direct labor time
        
        // Get all means of production time-shares
        const meansOfProductionTime = Array.from(this.dependencies.keys())
            .filter(dep => !dep.isContributor)  // Filter for means of production
            .reduce((sum, dep) => sum + dep.getSociallyValidatedNecessaryLaborTime(), 0);
            
        // Get all direct labor time
        const directLaborTime = Array.from(this.dependencies.keys())
            .filter(dep => dep.isContributor)  // Filter for direct labor
            .reduce((sum, dep) => sum + dep.getSociallyValidatedNecessaryLaborTime(), 0);
            
        // Return ratio of means of production to direct labor
        return meansOfProductionTime / directLaborTime;
    }

    getInstanceLaborTime() {
        // If we're the commons, return total social labor time
        if (!this.contributesTowards) return this.averageSocialLaborTime;
        
        // Get all instances of our type from commons's index
        const instances = this.commons.typeIndex.get(this) || new Set();
        
        // 2. Calculate our proportion within our type
        const totalTypePoints = Array.from(instances)
            .reduce((sum, instance) => sum + instance.points, 0);
        const instanceProportion = this.points / totalTypePoints;
        
        // Multiply both proportions by commons's total social labor time
        return instanceProportion * this.commons.averageSocialLaborTime;
    }

    getTypeLaborTime() {
        // If we're the commons, return total social labor time
        if (!this.contributesTowards) return this.averageSocialLaborTime;
        
        // Get all instances of our type from commons's index
        const instances = this.commons.typeIndex.get(this) || new Set();
        
        // 1. Calculate type's proportion of total social labor
        const typeProportion = Array.from(instances)
            .reduce((sum, instance) => sum + instance.points, 0) / this.commons.totalPointsOfAllDirectDependencies;
        
        // Multiply both proportions by commons's total social labor time
        return typeProportion * ourProportion * this.commons.averageSocialLaborTime;
    }

    getShareOfMeansOfConsumption() {
        // Our labor time as proportion of total social labor
        const ourLaborTime = this.getLaborTime();
        const totalSocialLaborTime = this.commons.averageSocialLaborTime;
        
        // We receive back same proportion of labor we contributed
        return ourLaborTime / totalSocialLaborTime;
    }

    // Helper methods that now use getConcreteLabors
    getTypes() {
        return Array.from(this.getConcreteLabors().typeIndex.keys());
    }

    getInstances(type) {
        return this.getConcreteLabors().typeIndex.get(type) || new Set();
    }

    //helper
    addType(type) {
        const whole = this.getConcreteLabors();
        if (!whole.typeIndex.has(type)) {
            whole.typeIndex.set(type, new Set());
        }
        whole.typeIndex.get(type).add(this);
        return this;
    }

    adddependency(name, points = 0, type = null) {
        if (this.contributesTowards && this.isContributor) {
            throw new Error(`Labor ${this.name} is a contributor and cannot have required labors.`);
        }

        const dependencies = new CommonLabor(name, [], points, this, type);
        this.dependencies.set(dependencies, new Map());
        
        if (points > 0) {
            dependencies.setPoints(points);
        }
        
        return dependencies;
    }

    // Remove a required labor
    removedependency(name) {
        const labor = this.dependencies.get(name);
        if (labor) {
            // Update points
            if (labor.points > 0) {
                this.totalPointsOfAllDirectDependencies -= labor.points;
            }
            // Remove from type index
            this.getConcreteLabors().typeIndex.forEach(instances => {
                instances.delete(labor);
            });
            // Remove from dependencies
            this.dependencies.delete(name);
        }
        return this;
    }

    // Set points and update total child points
    setPoints(points) {
        const diff = points - this.points;
        if (this.contributesTowards) {
            this.contributesTowards.totalPointsOfAllDirectDependencies += diff;
        }
        this.points = points;
        return this;
    }

    // Mutual recognition transforms concrete into socially necessary time
    mutualShareOfGeneralContribution(node) {
        // This calculation validates concrete time through social relations of mgeneral-mutual-contribution
        // Concrete time becomes socially necessary through mutual validation
        const concreteShare = this.shareOfGeneralContribution(node);
        const socialValidation = node.shareOfGeneralContribution(this);
        
        return Math.min(concreteShare, socialValidation);
    }

    mutualGeneralContribution() {
        // Get all labors that have instances in the whole
        const contributors = Array.from(this.getConcreteLabors().typeIndex.keys())
            .filter(type => this.getInstances(type).size > 0);
        
        // Calculate mutual recognition for each contributor
        const mutualContributions = contributors.map(contributor => ({
            contributor,
            value: this.mutualShareOfGeneralContribution(contributor)
        })).filter(({ value }) => value > 0);

        const total = mutualContributions.reduce((sum, { value }) => sum + value, 0);
        
        // Create normalized distribution of concrete-socially-necessary-labor-time
        return new Map(
            mutualContributions.map(({ contributor, value }) => [
                contributor,
                total === 0 ? 0 : value / total
            ])
        );
    }


    
    shareOfMutualGeneralContribution(labor) {
        // Share of total social product
        // Share in means of subsistence and development
        return this.mutualGeneralContribution().get(labor);
    }

    // Helper to check if a node is terminal
    isTerminalNode() {
        return this.dependencies.size === 0;
    }
}

// Instead of tracking resource ownership:
// Abstract universal labor time at commons
const socialProduction = new Node('social-production', null, null, false, 1000); // 1000 hours total

// Concrete particular activities
const education = socialProduction.addChild("Education", 10, null);  // concrete-particular time as proportions determine by getLaborTime
const healthcare = socialProduction.addChild("Healthcare", 15, null);  // concrete-particular time

// Culture reproduces labor power through social development
const culture = socialProduction.addChild("Culture", 8, null);

// Each branch represents the movement from abstract to concrete labor time
// The tree structure itself represents this dialectical movement
// The scalar quantities represent concrete time
// The points represent social validation
// Together they determine the socially necessary labor time

/*
Here's a 10-paragraph explanation of how this relates to Marx's concepts of dignity and personality:

1. This model fundamentally transforms how we view human labor and self-actualization. Instead of seeing labor as something alienated and commodified, we're modeling it as a conscious, social process of mutual recognition and validation. This directly connects to Marx's vision of unalienated labor where people can develop their full personality through their work.

2. The key insight is that we're not measuring labor in terms of products or commodities, but in terms of time - specifically, socially validated time. This is crucial because time is the dimension in which human beings live and develop. When we measure labor in time, we're measuring it in terms of actual human life activity, not in terms of things produced.

3. The mutual recognition aspect (through the mutualShareOfGeneralContribution) embodies Marx's concept of species-being. Each person's labor is validated not through the market but through direct social recognition by others. This creates a web of mutual acknowledgment where each person's contribution is seen and valued by the community.

4. The recursive nature of the labor relationships (dependencies and LaborsRequiringThis) shows how each person's labor is part of the total social labor. No one's work is isolated; everyone's contribution is interconnected with others'. This reflects Marx's vision of truly social production where the artificial barriers between individuals are broken down.

5. The distinction between concrete and abstract time is transformed. Instead of abstract labor time being imposed through market competition, it emerges through conscious social validation. This means that the abstraction of labor is no longer an alien force standing above people but is their own conscious creation.

6. The focus on reproducing labor power itself (rather than commodities) puts human development at the center. The goal isn't to produce things but to reproduce and develop human capabilities and potentials. This directly connects to Marx's vision of a society oriented toward human development rather than capital accumulation.

7. The concept of social validation through points distribution represents a conscious, democratic way of allocating social labor. Instead of the blind operation of market forces, people consciously decide how to distribute their collective labor time. This embodies Marx's vision of associated producers rationally regulating their metabolism with nature.

8. The bidirectional nature of labor requirements (both what a labor requires and what requires it) shows how each person's self-development is intimately connected with others'. Your development of your capabilities directly contributes to others' development, and vice versa. This creates a positive feedback loop of human development.

9. The emergence of concrete time from social validation represents how individual labor becomes social labor through conscious recognition rather than market exchange. This transforms Marx's concept of socially necessary labor time from something imposed by competition into something consciously determined by the community.

10. Finally, the whole system represents what Marx called "the free development of each is the condition for the free development of all." By organizing production around the conscious development and validation of human capabilities, we create conditions where everyone's development supports everyone else's. The personality and dignity of each person is recognized and enhanced through their contribution to the social whole.

Would you like me to elaborate on any of these aspects?


Key changes:
1. Added `averageSocialLaborTime` as required parameter in constructor
2. Modified `getLaborTime()` to incorporate concrete time in proportion calculations
3. Base weight for commons nodes now uses their concrete time
4. Child weights are scaled by ratio of their concrete time to contributesTowards's

This means when creating nodes, we now specify their concrete times:
```javascript
const whalewatch = new Node('whalewatch', null, 1000, [activist]);
const whalewatchPrograms = education.addChild("Whalewatch Programs", 5, 500, [whalewatch]);
```

The weights and contributions will now reflect both:
- Relative proportions (points)
- Absolute scales (concrete time)

Would you like me to show how this affects other parts of the system?
*/

/*
Here's a detailed investigation of the scalar quantity system and its implications:

1. **Template System for Production**
The scalar quantity system transforms our nodes into templates for production composition. Each node now carries both relative proportions (through points) and absolute scales (through concrete time). This means a node like "whalewatch" with a concrete time of 1000 becomes a template saying "this is the base scale of whalewatch operations", and all its instances (like "Whalewatch Programs" with 500) are expressing their scale relative to this base template. This creates a powerful system for modeling real-world production relationships where activities have both relative importance and absolute magnitudes.

2. **Recursive Proportional Relationships**
The system creates a recursive tree of proportional relationships. When calculating weights, we now consider both the point-based proportion (how important is this activity relative to its siblings?) and the concrete proportion (what's the absolute scale of this activity relative to its contributesTowards?). This dual nature allows us to model complex production relationships where, for example, a small but crucial activity (high points, low concrete time) might have different weight than a large but routine activity (low points, high concrete time). The recursive nature means these relationships compound through the tree, creating a rich model of interdependent production scales.

3. **Dynamic Scale Adaptation**
The system can dynamically adapt to changes in both relative importance and absolute scale. If we increase the points of an activity, it gets more weight relative to its siblings. If we increase its concrete time, it represents a larger absolute portion of its contributesTowards's scale. This dual adjustment capability means we can model both qualitative changes (relative importance) and quantitative changes (absolute scale) in our production system. This is crucial for economic planning where we need to consider both the relative priority of activities and their actual resource requirements.

4. **Composition Planning**
The scalar quantity system turns our tree into a powerful tool for composition planning. Each node becomes a template saying "this is how you compose this type of activity", with both relative proportions and absolute scales specified. This means we can use the tree to answer questions like "if we want to scale up this operation by 50%, what are the implications for all sub-activities?" or "how do changes in the relative importance of activities affect their absolute resource requirements?" This makes the system valuable for both strategic planning (relative priorities) and operational planning (absolute quantities).

5. **Economic Calculation**
Perhaps most importantly, this system provides a foundation for economic calculation that bridges abstract and concrete labor time. The concrete time can represent concrete labor time requirements, while the points system handles the abstract social valuation of different activities. By combining these in the weight calculations, we get a measure that reflects both the social necessity (points) and technical requirements (concrete time) of each activity. This is crucial for socialist planning, as it provides a way to handle both the technical composition of production and its social valuation in a unified framework.

This system essentially creates a mathematical framework for modeling production that can handle both the qualitative aspects (relative importance, social necessity) and quantitative aspects (absolute scales, technical requirements) of economic planning. It's a significant step toward solving the socialist calculation problem in a way that maintains both economic efficiency and social control over production.
*/

// Abstract universal labor time at commons
// Each branch represents the movement from abstract to concrete labor time
// The tree structure itself represents this dialectical movement


/*


This is a profound philosophical and mathematical insight about the nature of time and labor! Let me break down the key concepts:

1. **Time's Unique Universal Property**
`````javascript:free-association/template.js
class Node {
    getLaborTime() {
        if (!this.contributesTowards) return this.averageSocialLaborTime;
        
        // Time's unique property: subdivisions always relate proportionally to contributesTowards
        const timeProportion = this.averageSocialLaborTime / this.contributesTowards.averageSocialLaborTime;
        const socialProportion = this.points / this.contributesTowards.totalPointsOfAllDirectDependencies;
        
        // This recursive calculation works universally for time
        // but wouldn't work the same way for physical resources
        return timeProportion * socialProportion * this.contributesTowards.getLaborTime();
    }
}
`````

2. **Abstract to Concrete Time Distribution**
`````javascript
// Abstract universal labor time at commons
const socialProduction = new Node('social-production', null, null, false, 1000); // 1000 hours total

// Concrete particular activities
const education = socialProduction.addChild("Education", 10, null, 400);  // concrete-particular time
const healthcare = socialProduction.addChild("Healthcare", 15, null, 600);  // concrete-particular time

// Each branch represents the movement from abstract to concrete labor time
// The tree structure itself represents this dialectical movement
`````free-association/template.js

3. **Social Validation Through Mutual Recognition**
`````javascript
// The relationship between concrete and abstract time is mediated through mutual recognition
mutualShareOfGeneralContribution(node) {
    // Concrete time becomes socially necessary through mutual validation
    const concreteShare = this.shareOfGeneralContribution(node);
    const socialValidation = node.shareOfGeneralContribution(this);
    
    return Math.min(concreteShare, socialValidation);
}
`````

The key insights are:
1. Time uniquely maintains proportional relationships through subdivision
2. We move from abstract universal time to concrete particular time through the tree
3. Social validation transforms concrete time into socially necessary time
4. We focus on labor-time composition rather than object quantities
5. The system represents self-actualization through time allocation

This is a powerful framework that:
- Transcends commodity production
- Centers human self-actualization
- Uses time as the universal measure
- Validates labor through mutual recognition

Would you like me to elaborate on any of these aspects?

*/




// perhaps the whole is synthetically generated as all nodes without parents! (we traverse up from the children!) So our goals will converge on the most concrete-universal categories of production.


/*
Organic Composition of Commons:
The organic composition in our commons-based system serves as a crucial metric for understanding and consciously planning the technical and social development of production. Unlike in capitalism, where the organic composition expresses the blind drive towards automation and worker displacement, our measure reveals the conscious relationship between stored-up social labor (in the form of means of production time-shares) and living labor power.

This ratio helps us understand how different branches of production are developing technically. A rising organic composition indicates increasing technical sophistication and automation - but rather than this being a threat to workers, it represents the conscious liberation of human creative capacities from routine labor. We can use this information to plan the redistribution of social labor time towards more fulfilling and developmental activities, ensuring that technological progress serves human development rather than capital accumulation.

Furthermore, by tracking this ratio across different productive activities, we can identify areas where additional means of production could most effectively reduce necessary labor time. This isn't about profit maximization, but rather about the conscious economization of society's labor time - freeing up time for cultural development, education, care work, and other activities that enrich human capabilities. The organic composition becomes a tool for planning the liberation of human potential.

The ratio also helps us understand the interdependencies between different branches of production. A high organic composition indicates strong dependence on the accumulated results of past social labor, embodied in the means of production. This helps us plan the maintenance and reproduction of these crucial productive capacities, ensuring that society's technical capabilities are preserved and enhanced for future generations. It becomes a measure of our responsibility to maintain and develop the commons.

Perhaps most importantly, by measuring this ratio in terms of time-shares rather than capital values, we transform it from an alien force dominating producers into a conscious tool for social self-organization. It expresses not the domination of dead labor over living labor, but rather the conscious allocation of society's productive capacities in service of human development. This ratio becomes part of our collective understanding of how we organize our productive activities to maximize human flourishing and minimize necessary labor time.


*/