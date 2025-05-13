Currently our Forest is just simulating global look up of user-ids in gun

GunNode is only useful for adding to a given user's tree at the moment

For anything in calculations where we have a Forest, we should just look up the
gun.soul to find the user (including ourselves!)


The issue with contributors not persisting has now been fixed. Let me explain what was happening and how the solution works:

What was causing the issue
Reference handling: When you save contributors in Gun using put(), Gun sometimes creates a reference (soul) to the actual data rather than storing it inline. This is normal behavior for complex objects.
Incorrect reference resolution: Previously, the code was trying to resolve these references by constructing a path like [...nodePath, 'nodeContributors'] which doesn't correctly access the referenced data since that's not where Gun actually stored it.

The fix
I modified the code in two places (both in fetchNode and in loadNodeRecursive) to:
   // Use Gun directly with the soul to get the data
   const contributorsData = await new Promise<any>((resolve) => {
       gun.get(soul).once((data: any) => resolve(data));
   });
   
   This approach directly uses the soul reference to load the actual contributor data from Gun's database. The soul is the unique identifier that Gun uses to locate data, regardless of where it's hierarchically organized.
By using gun.get(soul) we're going straight to the source rather than trying to traverse a path that might not exist.