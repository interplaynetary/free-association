Currently our Forest is just simulating global look up of user-ids in gun

GunNode is only useful for adding to a given user's tree at the moment

For anything in calculations where we have a Forest, we should just look up the
gun.soul to find the user (including ourselves!)

still the delete functionality is not working! we get a toast that says it was successful but it doesnt dissapear visually and when i reload the page it is still there, clearly indicating it was not deleted from Gun either!

is our Tree.ts correclty using GunNode.ts for deletion? removeChild? removeSubtree?

We have addChild in tree.ts but we dont have removeChild (which should actually recursively delete the whole subtree of the child and its children etc. from the bottom up to the child

// Can we simplify tree.ts somehow and make it more elgant?

// we should mantain a contact list of all the contributors we have ever added. // Contact management // The dropdown should be based on this (not the public)
