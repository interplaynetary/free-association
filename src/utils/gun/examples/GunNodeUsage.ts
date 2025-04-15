/**
 * GunNodeUsage.ts
 * 
 * This file demonstrates the recommended usage patterns for GunNode and GunSubscription
 * based on the principles from the Gun guides.
 */

import { gun, getNodeRef, customStore } from '../gunSetup';
import { GunNode } from '../GunNode';
import type { SubscriptionHandler } from '../gunSetup';

// ===== APPROACH 1: Direct Gun Access =====
// For simple cases where you don't need the abstraction
// This uses the built-in gun.chain.subscribe for Svelte compatibility

export function directGunExample() {
  // 1. Simple value binding
  const userNode = gun.get('users').get('alice');
  // In Svelte: $userNode will be reactive
  
  // 2. Writing data
  userNode.get('name').put('Alice');
  
  // 3. Reading once with timeout safety
  const readUserNameSafely = async () => {
    try {
      const name = await Promise.race([
        userNode.get('name'), 
        new Promise((_, reject) => setTimeout(() => reject(new Error('Timeout')), 5000))
      ]);
      return name;
    } catch (error) {
      console.error('Error reading user name:', error);
      return null;
    }
  };
  
  // 4. Iterating with map (be careful with this - can load a lot of data)
  const allUsers = gun.get('users');
  // In Svelte: $allUsers will be an array of [key, value] pairs
}

// ===== APPROACH 2: GunNode Abstraction =====
// For more complex scenarios where you want type safety and error handling

// Define specific node types
export class UserNode extends GunNode<{name: string, email: string}> {
  constructor(userId: string, certificate?: string) {
    super(['users', userId], certificate);
  }
  
  // Define specific methods for accessing common paths
  public name() {
    return this.get('name');
  }
  
  public email() {
    return this.get('email');
  }
  
  // Define a domain-specific action
  public async updateProfile(name: string, email: string): Promise<void> {
    try {
      await Promise.all([
        this.name().put({ value: name }),
        this.email().put({ value: email })
      ]);
      return Promise.resolve();
    } catch (error) {
      console.error('Failed to update profile:', error);
      return Promise.reject(error);
    }
  }
}

export class UsersCollection extends GunNode<Record<string, {name: string, email: string}>> {
  constructor() {
    super(['users']);
  }
  
  // Get a specific user
  public user(userId: string, certificate?: string): UserNode {
    return new UserNode(userId, certificate);
  }
  
  // Stream all users
  public streamAllUsers(handler: SubscriptionHandler<any>): () => void {
    return this.each(handler);
  }
  
  // Create a new user
  public async createUser(userId: string, name: string, email: string): Promise<UserNode> {
    const user = this.user(userId);
    await user.updateProfile(name, email);
    return user;
  }
}

// ===== APPROACH 3: Certificate-based Access Control =====

export async function certificateExample() {
  // 1. Define participants
  const adminPub = 'ADMIN_PUBLIC_KEY';
  const userPub = 'USER_PUBLIC_KEY';
  
  // 2. Creating a certificate (would be done by the admin)
  const certificateNode = gun.user().get('trust').get('certificates').get('user-edit');
  const certificate = await new Promise<string>((resolve) => {
    certificateNode.once((cert: any) => resolve(cert as string));
  });
  
  // 3. Using the certificate with our GunNode
  const userNode = new UserNode('alice').withCertificate(certificate);
  
  // 4. Put data with the certificate
  userNode.name().put({ value: 'Alice' });
  
  // The certificate is automatically passed down the chain
  userNode.email().put({ value: 'alice@example.com' });
}

// ===== APPROACH 4: Custom Store with Domain Logic =====

export function createUserStore(userId: string) {
  const userNode = getNodeRef(['users', userId]);
  
  // Custom store with additional methods
  return customStore(userNode, {
    // Add domain-specific methods
    updateName: (name: string) => userNode.get('name').put(name),
    updateEmail: (email: string) => userNode.get('email').put(email),
    delete: () => userNode.put(null)
  });
}

// ===== USAGE EXAMPLES =====

// Example 1: Direct usage in a component
export function exampleComponentUsage() {
  // Create a users collection
  const users = new UsersCollection();
  
  // Get a specific user
  const alice = users.user('alice');
  
  // Subscribe to user data changes
  const unsubscribe = alice.on(userData => {
    console.log('User data updated:', userData);
  });
  
  // Update user data
  alice.updateProfile('Alice Smith', 'alice@example.com')
    .then(() => console.log('Profile updated'))
    .catch(err => console.error('Update failed:', err));
  
  // Clean up when done
  // unsubscribe();
}

// Example 2: Working with streams for iteration
export async function exampleStreamUsage() {
  const users = new UsersCollection();
  
  // Get a stream of all users
  const userSubscription = users.stream().each();
  const reader = userSubscription.subscribe().getReader();
  
  // Process users one by one
  try {
    while (true) {
      const { value, done } = await reader.read();
      if (done) break;
      
      console.log('User:', value);
      
      // Do something with each user
      if (value._key && !value._removed) {
        console.log(`Processing user ${value._key}`);
      }
    }
  } catch (error) {
    console.error('Error processing users:', error);
  } finally {
    reader.releaseLock();
  }
}

// Example 3: Svelte integration example (pseudo-code)
/*
<script>
  import { UsersCollection } from './GunNodeUsage';
  import { onDestroy } from 'svelte';
  
  // Create a users collection
  const users = new UsersCollection();
  
  // Get a specific user and access raw Gun chain for Svelte reactivity
  const alice = users.user('alice').getChain();
  
  // Set up cleanup
  onDestroy(() => {
    // Gun will clean up automatically in Svelte when the component is destroyed
  });
  
  // Update user function
  function updateUser() {
    alice.get('name').put('Alice Smith');
  }
</script>

<div>
  <!-- Use reactive binding with Gun -->
  <h1>User: {$alice?.name || 'Loading...'}</h1>
  <button on:click={updateUser}>Update Name</button>
</div>
*/ 