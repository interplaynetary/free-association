import { user, gun } from './Gun'

/**
 * Writes data to a specified path in Gun database.
 * 
 * @param paths Array of path segments to navigate to the target location
 * @param data Data to write at the target location
 * @param set If true, uses Gun's set() method; otherwise uses put()
 * @returns Gun reference to the written data
 */
export function writeToGunPath(paths = [], data = null, set = false) {
  if (paths.length === 0) {
    console.log('[FuncGun] Cannot write to empty path');
    return undefined;
  }

  console.log('[FuncGun] Starting path traversal for write:', paths);
  
  // Determine starting reference based on the path
  // If path starts with 'user', use user reference
  // Otherwise use gun for general data
  let gunRef;
  let startIndex = 0;
  
  if (paths[0] === 'user') {
    console.log('[FuncGun] Using user reference as starting point');
    gunRef = user;
    startIndex = 1; // Skip 'user' in the path
  } else if (paths[0] === 'nodes') {
    console.log('[FuncGun] Using gun reference for nodes');
    gunRef = gun;
  } else {
    console.log('[FuncGun] Using gun reference as general starting point');
    gunRef = gun;
  }
  
  console.log(`[FuncGun] Initial reference:`, gunRef);
  
  // Traverse through the path segments
  for (let i = startIndex; i < paths.length; i++) {
    const segment = paths[i];
    console.log(`[FuncGun] Traversing path segment [${i}]: '${segment}'`);
    
    // Debug current reference before getting next segment
    console.log(`[FuncGun] Current gunRef before .get('${segment}'):`, gunRef);
    
    // @ts-ignore - Gun types are complicated, but this works at runtime
    gunRef = gunRef.get(segment);
    
    // Debug new reference after getting segment
    console.log(`[FuncGun] New gunRef after .get('${segment}'):`, gunRef);
  }
  
  console.log(`[FuncGun] Final gunRef after traversal:`, gunRef);
  console.log(`[FuncGun] Writing to path [${paths.join('/')}]`, data);
  
  // Apply the operation based on set parameter
  if (set) {
    // @ts-ignore - Gun types are complicated, but this works at runtime
    console.log(`[FuncGun] Using set() with data:`, data);
    return gunRef.set(data);
  } else {
    // @ts-ignore - Gun types are complicated, but this works at runtime
    console.log(`[FuncGun] Using put() with data:`, data);
    return gunRef.put(data);
  }
}

/*
writeToGunPath([
  'nodes',
  'GunSoulID'
], {
    set: true,
    value: {
        id: 'GunSoulID',
        name: 'test',
        points: 100,
        manualFulfillment: 0,
        children: [
          'GunSoulID2'
        ],
        types: [
          'GunSoulID3'
        ]
    }
})
*/

/**
 * Reads data from a specified path in Gun database.
 * 
 * @param paths Array of path segments to navigate to the target location
 * @param subscribe If true, creates a persistent subscription with on(); otherwise uses once()
 * @returns Object containing the Gun reference, read function, and data (once updated)
 */
export function readFromGunPath(paths = [], subscribe = false) {
  if (paths.length === 0) {
    console.log('[FuncGun] Cannot read from empty path');
    return undefined;
  }

  console.log('[FuncGun] Starting read path traversal:', paths);
  
  // Determine starting reference based on the path
  // If path starts with 'user', use user reference
  // Otherwise use gun for general data
  let gunRef;
  let startIndex = 0;
  
  if (paths[0] === 'user') {
    console.log('[FuncGun] Using user reference as starting point for read');
    gunRef = user;
    startIndex = 1; // Skip 'user' in the path
  } else if (paths[0] === 'nodes') {
    console.log('[FuncGun] Using gun reference for nodes');
    gunRef = gun;
  } else {
    console.log('[FuncGun] Using gun reference as general starting point for read');
    gunRef = gun;
  }
  
  console.log(`[FuncGun] Initial read reference:`, gunRef);
  
  // Traverse through the path segments
  for (let i = startIndex; i < paths.length; i++) {
    const segment = paths[i];
    console.log(`[FuncGun] Reading path segment [${i}]: '${segment}'`);
    
    // Debug current reference before getting next segment
    console.log(`[FuncGun] Current read gunRef before .get('${segment}'):`, gunRef);
    
    // @ts-ignore - Gun types are complicated, but this works at runtime
    gunRef = gunRef.get(segment);
    
    // Debug new reference after getting segment
    console.log(`[FuncGun] New read gunRef after .get('${segment}'):`, gunRef);
  }
  
  console.log(`[FuncGun] Final read gunRef after traversal:`, gunRef);
  console.log(`[FuncGun] Reading from path [${paths.join('/')}], subscribe=${subscribe}`);
  
  const returnObject = { 
    paths: paths,
    gunNodeRef: gunRef,
    readFunction: null,
    data: undefined,
    key: undefined
  };

  console.log('[FuncGun] ReturnObject:', returnObject);
  
  if (subscribe) {
    // @ts-ignore - Gun types are complicated, but this works at runtime
    returnObject.readFunction = gunRef.on((data, key) => {
      console.log(`[FuncGun] Subscription update from [${paths.join('/')}]:`, { data, key });
      returnObject.data = data;
      returnObject.key = key;
    });
  } else {
    // @ts-ignore - Gun types are complicated, but this works at runtime
    returnObject.readFunction = gunRef.once((data, key) => {
      console.log(`[FuncGun] One-time read from [${paths.join('/')}]:`, { data, key });
      returnObject.data = data;
      returnObject.key = key;
    });
  }
  
  return returnObject;
}

/*
// Example: Create a new node
writeToGunPath(['nodes', 'user123'], {
    set: true,
    value: {
      id: 'user123',
      name: 'John Doe',
      points: 50,
      manualFulfillment: 0
    }
  });
  
  // Example: Update a property
  writeToGunPath(['nodes', 'user123'], {
    points: 75
  });


  // Create parent node
writeToGunPath(['nodes', 'parent1'], {
    set: true,
    value: { id: 'parent1', name: 'Parent', points: 0, manualFulfillment: 0 }
  });
  
  // Create child node
  writeToGunPath(['nodes', 'child1'], {
    set: true,
    value: { id: 'child1', name: 'Child', points: 0, manualFulfillment: 0 }
  });
  
  // Establish parent-child relationship
  writeToGunPath(['nodes', 'parent1', 'children'], {
    set: true,
    value: ['child1']
  });



// User A subscribes to a shared document
const docSubscription = readFromGunPath(['documents', 'doc123'], true);

// User A makes changes
writeToGunPath(['documents', 'doc123'], {
  content: 'Updated content from User A'
});

// User B automatically sees changes through their subscription
// docSubscription.data would update automatically
*/



// Create a reactive component that updates when data changes
export function createReactiveComponent(nodePath) {
    const subscription = readFromGunPath(['nodes', nodePath], true);
    
    return {
      getState: () => subscription.data,
      updatePoints: (newPoints) => {
        writeToGunPath(['nodes', nodePath], {
          points: newPoints
        });
      }
    };
  }
  
// const userComponent = createReactiveComponent('user123');


// Implement a simple transaction pattern
export async function transferPoints(fromId, toId, amount) {
    const fromRef = readFromGunPath(['nodes', fromId]);
    const toRef = readFromGunPath(['nodes', toId]);
    
    // Wait for data (in real code, use proper async handling)
    setTimeout(() => {
      if (fromRef.data && toRef.data && fromRef.data.points >= amount) {
        writeToGunPath(['nodes', fromId], {
          points: fromRef.data.points - amount
        });
        writeToGunPath(['nodes', toId], {
          points: toRef.data.points + amount
        });
        return true;
      }
      return false;
    }, 100);
  }

