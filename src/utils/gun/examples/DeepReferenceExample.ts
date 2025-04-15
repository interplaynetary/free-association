/**
 * DeepReferenceExample.ts
 * 
 * This file demonstrates the usage of the deep reference traversal and certificate management features
 * that we've added to our Gun wrapper classes.
 */

import { gun, createCertificate, storeCertificate, getCertificate, certificatePresets, CertificatePermission } from '../gunSetup';
import { GunNode } from '../GunNode';

// ===== DEEP REFERENCE TRAVERSAL EXAMPLE =====

export async function deepReferenceExample() {
  console.log("=== Deep Reference Traversal Example ===");
  
  // Create some test data with references
  const userNode = new GunNode(['users', 'alice']);
  const profileNode = new GunNode(['profiles', 'alice-profile']);
  const settingsNode = new GunNode(['settings', 'alice-settings']);
  
  // Set up the profile data
  await profileNode.put({
    name: 'Alice',
    bio: 'Software developer',
    location: 'New York'
  });
  
  // Set up the settings data
  await settingsNode.put({
    theme: 'dark',
    notifications: true,
    privacy: 'friends-only'
  });
  
  // Create the user with references to profile and settings
  const userSoul = await userNode.put({
    email: 'alice@example.com',
    created: Date.now()
  });
  
  // Get the souls of the other nodes
  const profileSoul = await profileNode.getSoul();
  const settingsSoul = await settingsNode.getSoul();
  
  if (profileSoul && settingsSoul) {
    // Store references to profile and settings
    await userNode.get('profile').put({ '#': profileSoul });
    await userNode.get('settings').put({ '#': settingsSoul });
    
    console.log("Created reference structure");
    
    // Demonstrate regular get (doesn't resolve references)
    console.log("\nRegular get (without resolving references):");
    const regularData = await userNode.once();
    console.log(JSON.stringify(regularData, null, 2));
    
    // Demonstrate deep get with different depths
    console.log("\nDeep get with depth 1 (resolves direct references):");
    const deepData1 = await userNode.deepGet(1);
    console.log(JSON.stringify(deepData1, null, 2));
    
    console.log("\nDeep get with depth 2 (resolves nested references):");
    const deepData2 = await userNode.deepGet(2);
    console.log(JSON.stringify(deepData2, null, 2));
    
    // Demonstrate deep subscription
    console.log("\nSetting up deep subscription (resolves references):");
    const cleanup = userNode.deepOn((data) => {
      console.log("Received update with resolved references:");
      console.log(JSON.stringify(data, null, 2));
    });
    
    // Update a referenced node to see the subscription in action
    console.log("\nUpdating profile (should trigger deep subscription):");
    await profileNode.get('name').put({ value: 'Alice Smith' });
    
    // Give time for subscription to fire
    await new Promise(resolve => setTimeout(resolve, 1000));
    
    // Clean up subscription
    cleanup();
  }
}

// ===== CERTIFICATE MANAGEMENT EXAMPLE =====

export async function certificateManagementExample() {
  console.log("\n=== Certificate Management Example ===");
  
  // Setup: Ensure we have a user authenticated
  const aliceAuth = await authenticateUser('alice', 'alicepassword');
  if (!aliceAuth) {
    console.error("Failed to authenticate Alice");
    return;
  }
  
  // Get Alice's public key
  const alicePub = (gun.user() as any).is.pub;
  console.log(`Alice authenticated with pub key: ${alicePub}`);
  
  // Create Bob (would normally be on another client)
  const bobAuth = await authenticateUser('bob', 'bobpassword');
  if (!bobAuth) {
    console.error("Failed to authenticate Bob");
    await logout(); // Log Alice out
    return;
  }
  
  // Get Bob's public key
  const bobPub = (gun.user() as any).is.pub;
  console.log(`Bob authenticated with pub key: ${bobPub}`);
  
  // Log back in as Alice to create certificates
  await logout();
  await authenticateUser('alice', 'alicepassword');
  
  // Create a certificate for Bob to write to Alice's "shared" path
  console.log("\nCreating write certificate for Bob...");
  const writeCert = await createCertificate(
    bobPub,
    ['shared'],
    CertificatePermission.WRITE,
    // Optional expiration in milliseconds (24 hours)
    24 * 60 * 60 * 1000
  );
  
  if (writeCert) {
    console.log("Certificate created successfully");
    
    // Store the certificate for later use
    await storeCertificate('bob-shared-access', writeCert, bobPub);
    console.log("Certificate stored in Alice's certificates collection");
    
    // Log in as Bob to use the certificate
    await logout();
    await authenticateUser('bob', 'bobpassword');
    
    // Create a node with the certificate
    const sharedNode = new GunNode([`~${alicePub}`, 'shared'], writeCert);
    
    // Use the certificate to write to Alice's graph
    console.log("\nBob using certificate to write to Alice's graph...");
    await sharedNode.get('message').put({ 
      text: 'Hello from Bob!',
      timestamp: Date.now()
    });
    
    console.log("Write completed with certificate");
    
    // Log back in as Alice to verify the data was written
    await logout();
    await authenticateUser('alice', 'alicepassword');
    
    // Check the shared node
    console.log("\nAlice checking the shared node...");
    const aliceSharedNode = new GunNode(['shared']);
    const sharedData = await aliceSharedNode.get('message').once();
    console.log("Message from Bob:", sharedData);
    
    // Clean up
    await logout();
  } else {
    console.error("Failed to create certificate");
    await logout();
  }
}

// Helper function to authenticate a user (create if doesn't exist)
async function authenticateUser(username: string, password: string): Promise<boolean> {
  return new Promise<boolean>((resolve) => {
    (gun.user() as any).auth(username, password, (ack: any) => {
      if (ack.err) {
        // User doesn't exist, create it
        (gun.user() as any).create(username, password, (createAck: any) => {
          if (createAck.err) {
            console.error(`Failed to create user ${username}:`, createAck.err);
            resolve(false);
          } else {
            // Now try to authenticate again
            (gun.user() as any).auth(username, password, (authAck: any) => {
              resolve(!authAck.err);
            });
          }
        });
      } else {
        // Successfully authenticated
        resolve(true);
      }
    });
  });
}

// Helper function to logout the current user
async function logout(): Promise<void> {
  (gun.user() as any).leave();
  // Give a moment for the logout to process
  await new Promise(resolve => setTimeout(resolve, 100));
}

// Run both examples sequentially
export async function runAllExamples() {
  try {
    await deepReferenceExample();
    await certificateManagementExample();
    console.log("\nAll examples completed successfully");
  } catch (error) {
    console.error("Error running examples:", error);
  }
} 