# Ideas

## Code Improvement
- Haskell: Use O3, we can seperate data/traversal from calculation. Usually when a lot of our types repeat (Forest -> TreeZipper), we can buse 

## AI
- conversation to value-graph. (Maybe using squiggly)

## Map
- Make markers display the location of capacities, and color the tags according to the depth at which one received access.

## Narrative: Charecters
- People can create charecters that they can play. (accounts) This makes it feel more approachable.




still the delete functionality is not working! we get a toast that says it was successful but it doesnt dissapear visually and when i reload the page it is still there, clearly indicating it was not deleted from Gun either!

is our Tree.ts correclty using GunNode.ts for deletion? removeChild? removeSubtree?

We have addChild in tree.ts but we dont have removeChild (which should actually recursively delete the whole subtree of the child and its children etc. from the bottom up to the child



// Can we simplify tree.ts somehow and make it more elgant?


// we should mantain a contact list of all the contributors we have ever added.
// Contact management
// The dropdown should be based on this (not the public)