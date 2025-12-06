/**
 * Secure Authentication Example
 * 
 * Demonstrates the Cap'n Web challenge-response authentication pattern
 * with the new SecureContext integration.
 * 
 * Flow:
 * 1. Client requests challenge from relay server
 * 2. Client signs challenge with their private key
 * 3. Server verifies signature
 * 4. Server returns session capability (unforgeable reference)
 * 5. All operations are automatically signed with SecureContext
 */

import { RelayServer } from '../relay-server';
import { generateKeypair } from '../identity/keypair';
import { SecureContext } from '../security/secure-context';
import { login } from '../restoration/login';
import type { Challenge } from '../identity/credentials';

/**
 * Example 1: Basic Challenge-Response Authentication
 */
export async function basicAuthenticationExample() {
  console.log('=== Basic Challenge-Response Authentication ===\n');

  // 1. Setup: Create relay server
  const relay = new RelayServer();
  console.log('✓ Relay server created\n');

  // 2. Client: Generate keypair
  const aliceKeypair = await generateKeypair();
  const alicePublicKey = JSON.stringify(aliceKeypair.publicKey);
  console.log('✓ Alice generated keypair');
  console.log('  Public key:', alicePublicKey.substring(0, 50) + '...\n');

  // 3. Client: Create secure context
  const aliceSecureContext = await SecureContext.create('alice', {
    keypair: aliceKeypair
  });
  console.log('✓ Alice created secure context\n');

  // 4. Authentication Flow:
  console.log('--- Authentication Flow ---\n');

  // Step 1: Request challenge
  const challenge: Challenge = relay.createChallenge();
  console.log('1. Alice requests challenge from relay');
  console.log('   Challenge nonce:', challenge.nonce);
  console.log('   Challenge entity:', challenge.entityId);
  console.log('   Challenge timestamp:', new Date(challenge.timestamp).toISOString() + '\n');

  // Step 2: Sign challenge with private key
  const challengeData = JSON.stringify(challenge);
  const signedChallenge = await aliceSecureContext.sign(challengeData);
  console.log('2. Alice signs challenge with private key');
  console.log('   Signature:', signedChallenge.signature.substring(0, 50) + '...\n');

  // Step 3: Authenticate and receive session capability
  const aliceSession = await relay.authenticate(
    challenge,
    signedChallenge.signature,
    alicePublicKey
  );
  console.log('3. Relay verifies signature and returns session capability');
  console.log('   ✓ Alice authenticated!\n');

  // 4. Use session capability
  console.log('--- Using Authenticated Session ---\n');

  // Alice can now call methods on her session
  await aliceSession.allocateRecognition('bob', 0.8);
  console.log('✓ Alice allocated 0.8 to Bob (signed automatically)');

  const bobRecognition = await aliceSession.getRecognition('alice', 'bob');
  console.log('✓ Alice → Bob recognition:', bobRecognition);

  console.log('\n=== Basic Authentication Complete ===\n');
}

/**
 * Example 2: Multiple Users with Secure Sessions
 */
export async function multiUserAuthenticationExample() {
  console.log('=== Multiple Users with Secure Sessions ===\n');

  const relay = new RelayServer();

  // Create three users with their own keypairs
  const users = ['alice', 'bob', 'charlie'];
  const contexts = new Map<string, SecureContext>();
  const sessions = new Map<string, any>();

  for (const userId of users) {
    // Generate keypair
    const keypair = await generateKeypair();
    const publicKey = JSON.stringify(keypair.publicKey);
    
    // Create secure context
    const secureContext = await SecureContext.create(userId, { keypair });
    contexts.set(userId, secureContext);

    // Authenticate
    const challenge = relay.createChallenge();
    const signedChallenge = await secureContext.sign(JSON.stringify(challenge));
    const session = await relay.authenticate(challenge, signedChallenge.signature, publicKey);
    sessions.set(userId, session);

    console.log(`✓ ${userId} authenticated with secure session`);
  }

  console.log('\n--- Users Allocating Recognition (All Signed) ---\n');

  // Alice allocates to others
  await sessions.get('alice')!.allocateRecognition('bob', 0.6);
  await sessions.get('alice')!.allocateRecognition('charlie', 0.4);
  console.log('Alice allocated: Bob (0.6), Charlie (0.4)');

  // Bob allocates to others
  await sessions.get('bob')!.allocateRecognition('alice', 0.5);
  await sessions.get('bob')!.allocateRecognition('charlie', 0.5);
  console.log('Bob allocated: Alice (0.5), Charlie (0.5)');

  // Charlie allocates to others
  await sessions.get('charlie')!.allocateRecognition('alice', 0.7);
  await sessions.get('charlie')!.allocateRecognition('bob', 0.3);
  console.log('Charlie allocated: Alice (0.7), Bob (0.3)');

  console.log('\n--- Mutual Recognition (All Operations Verified) ---\n');

  // Check mutual recognition between all pairs
  const pairs = [
    ['alice', 'bob'],
    ['alice', 'charlie'],
    ['bob', 'charlie']
  ];

  for (const [user1, user2] of pairs) {
    const mr = await sessions.get(user1)!.getMutualRecognition(user2);
    console.log(`${user1} ↔ ${user2}: ${mr.toFixed(2)}`);
  }

  console.log('\n=== Multi-User Authentication Complete ===\n');
}

/**
 * Example 3: Secure Login with State Restoration
 */
export async function secureLoginExample() {
  console.log('=== Secure Login with State Restoration ===\n');

  console.log('Alice logs in with email and password...\n');

  // This function:
  // 1. Derives keypair from password
  // 2. Discovers state replicas
  // 3. Fetches and verifies state fragments
  // 4. Creates SecureEntitySession with automatic signing
  // 5. Returns ready-to-use session
  const session = await login('alice@example.com', 'secure-password-123');

  console.log('✓ Alice logged in successfully');
  console.log('✓ State restored from replicas');
  console.log('✓ SecureContext initialized');
  console.log('✓ All operations will be signed automatically\n');

  // Use the session - all operations are signed!
  await session.allocateRecognition('bob', 0.9);
  console.log('✓ Allocated recognition (signed)');

  const mr = await session.getMutualRecognition('bob');
  console.log('✓ Mutual recognition:', mr.toFixed(2));

  console.log('\n💡 Key Benefits:');
  console.log('  • One-line login with state restoration');
  console.log('  • Automatic cryptographic signing');
  console.log('  • Data integrity verification');
  console.log('  • Capability-based authorization');

  console.log('\n=== Secure Login Complete ===\n');
}

/**
 * Example 4: Replay Attack Prevention
 */
export async function replayAttackPreventionExample() {
  console.log('=== Replay Attack Prevention ===\n');

  const relay = new RelayServer();

  // Alice authenticates normally
  const aliceKeypair = await generateKeypair();
  const alicePublicKey = JSON.stringify(aliceKeypair.publicKey);
  const aliceContext = await SecureContext.create('alice', { keypair: aliceKeypair });

  const challenge1 = relay.createChallenge();
  const signed1 = await aliceContext.sign(JSON.stringify(challenge1));
  const session = await relay.authenticate(challenge1, signed1.signature, alicePublicKey);

  console.log('✓ Alice authenticated successfully\n');

  // Attacker tries to replay the same challenge
  console.log('⚠️  Attacker intercepts challenge and tries to replay it...\n');

  try {
    // This should fail because the challenge was already used
    await relay.authenticate(challenge1, signed1.signature, alicePublicKey);
    console.log('❌ SECURITY BREACH: Replay attack succeeded!');
  } catch (error) {
    console.log('✓ Replay attack prevented!');
    console.log('  Error:', (error as Error).message);
  }

  console.log('\n💡 Security Features:');
  console.log('  • Each challenge can only be used once');
  console.log('  • Challenges expire after 5 minutes');
  console.log('  • Nonces prevent replay attacks');
  console.log('  • Signatures are cryptographically verified');

  console.log('\n=== Replay Attack Prevention Complete ===\n');
}

/**
 * Example 5: Invalid Signature Rejection
 */
export async function invalidSignatureExample() {
  console.log('=== Invalid Signature Rejection ===\n');

  const relay = new RelayServer();

  // Alice creates a valid challenge
  const aliceKeypair = await generateKeypair();
  const alicePublicKey = JSON.stringify(aliceKeypair.publicKey);
  const aliceContext = await SecureContext.create('alice', { keypair: aliceKeypair });

  const challenge = relay.createChallenge();
  const signedChallenge = await aliceContext.sign(JSON.stringify(challenge));

  console.log('✓ Alice created valid signed challenge\n');

  // Attacker tries to use a forged signature
  console.log('⚠️  Attacker tries to authenticate with forged signature...\n');

  try {
    const forgedSignature = 'forged-signature-12345';
    await relay.authenticate(challenge, forgedSignature, alicePublicKey);
    console.log('❌ SECURITY BREACH: Forged signature accepted!');
  } catch (error) {
    console.log('✓ Forged signature rejected!');
    console.log('  Error:', (error as Error).message);
  }

  // Attacker tries to use wrong public key
  console.log('\n⚠️  Attacker tries with wrong public key...\n');

  try {
    const bobKeypair = await generateKeypair();
    const bobPublicKey = JSON.stringify(bobKeypair.publicKey);
    
    // Valid signature but wrong public key
    await relay.authenticate(challenge, signedChallenge.signature, bobPublicKey);
    console.log('❌ SECURITY BREACH: Wrong public key accepted!');
  } catch (error) {
    console.log('✓ Wrong public key rejected!');
    console.log('  Error:', (error as Error).message);
  }

  console.log('\n💡 Security Features:');
  console.log('  • Ed25519 cryptographic signatures');
  console.log('  • Public key verification');
  console.log('  • Tamper-proof authentication');
  console.log('  • No password transmission');

  console.log('\n=== Invalid Signature Rejection Complete ===\n');
}

/**
 * Run all secure authentication examples
 */
export async function runSecureAuthExamples() {
  try {
    await basicAuthenticationExample();
    await multiUserAuthenticationExample();
    await secureLoginExample();
    await replayAttackPreventionExample();
    await invalidSignatureExample();
    
    console.log('✓✓✓ All Secure Authentication Examples Completed Successfully! ✓✓✓');
  } catch (error) {
    console.error('Error running secure authentication examples:', error);
    throw error;
  }
}

// Run if executed directly
if (typeof require !== 'undefined' && require.main === module) {
  runSecureAuthExamples();
}

