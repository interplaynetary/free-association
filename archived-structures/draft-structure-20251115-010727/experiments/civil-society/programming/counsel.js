/*
Write a short recursive definition of a actor (in JS): with a mandate (from, to, mandate) where from is a list of other actor signatures, who have signed the mandate.

Another class: Counsel, has actor's voting power based on the number of actors they represent.

Having actor signatures will help us understand when two actors share support-basis in their mandates.
*/

class Actor {
    constructor(name) {
      this.name = name;
      this.mandates = []; // List of mandates this actor has received
    }
  
    // Create a mandate from a list of actors to this actor with specific powers
    createMandate(fromActors, mandatePowers) {
      const mandate = {
        from: fromActors,
        to: this,
        powers: mandatePowers,
        signatures: fromActors.map(actor => ({
          actor: actor,
          // Recursively get all actors who support this actor's mandate
          supportBasis: actor.getAllSupporters()
        }))
      };
      this.mandates.push(mandate);
      return mandate;
    }
  
    // Get all actors who have given mandates to this actor (recursive)
    getAllSupporters() {
      const supporters = new Set();
      
      for (const mandate of this.mandates) {
        for (const signer of mandate.signatures) {
          supporters.add(signer.actor);
          // Add all supporters of the signing actor
          signer.supportBasis.forEach(s => supporters.add(s));
        }
      }
      
      return supporters;
    }
  }
  
  class Council {
    constructor() {
      this.actors = new Set();
    }
  
    addActor(actor) {
      this.actors.add(actor);
    }
  
    // Calculate voting power based on unique supporters
    getVotingPower(actor) {
      return actor.getAllSupporters().size;
    }
  
    // Find overlapping support between two actors
    getSharedSupport(actor1, actor2) {
      const supporters1 = actor1.getAllSupporters();
      const supporters2 = actor2.getAllSupporters();
      return new Set([...supporters1].filter(x => supporters2.has(x)));
    }
  }
  
  // Example usage:
  const alice = new Actor("Alice");
  const bob = new Actor("Bob");
  const charlie = new Actor("Charlie");
  const dave = new Actor("Dave");
  
  // Bob and Charlie give mandate to Dave
  dave.createMandate([bob, charlie], ["speak", "vote"]);
  
  // Alice gives mandate to Bob
  bob.createMandate([alice], ["speak"]);
  
  const council = new Council();
  council.addActor(dave);
  council.addActor(bob);
  
  console.log(council.getVotingPower(dave)); // 3 (Bob, Charlie, Alice)
  console.log(council.getVotingPower(bob)); // 1 (Alice)
  console.log(council.getSharedSupport(dave, bob)); // Set with Alice