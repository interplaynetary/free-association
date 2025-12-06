/**
 * Simple Chat Example
 * 
 * Demonstrates recognition-based peer-to-peer chat.
 * 
 * Features:
 * - No explicit initialize() call - just works!
 * - Natural callback subscriptions
 * - Recognition-based message routing
 * - Automatic serialization
 */

import { newWebSocketSession, type EntityAPI } from '../../simple-api';
import type { RecognitionUpdate } from '../../api';

/**
 * Chat message
 */
interface ChatMessage {
  from: string;
  to: string;
  text: string;
  timestamp: number;
  recognition?: number; // MR at time of message
}

/**
 * Simple chat client
 */
class ChatClient {
  private api: EntityAPI;
  private entityId: string;
  private messages: ChatMessage[] = [];
  private onMessage?: (message: ChatMessage) => void;

  constructor(entityId: string, relayUrl: string) {
    this.entityId = entityId;
    this.api = newWebSocketSession(entityId, relayUrl);
  }

  /**
   * Start chat (no explicit initialize!)
   */
  async start() {
    // Subscribe to recognition updates
    await this.api.subscribe((update: RecognitionUpdate) => {
      console.log(`[${this.entityId}] Recognition update:`, update);
    });

    console.log(`[${this.entityId}] Chat started`);
  }

  /**
   * Send message to another user
   */
  async sendMessage(to: string, text: string) {
    // Get current mutual recognition (auto-initializes!)
    const recognition = await this.api.getMutualRecognition(to);

    const message: ChatMessage = {
      from: this.entityId,
      to,
      text,
      timestamp: Date.now(),
      recognition
    };

    this.messages.push(message);

    console.log(`[${this.entityId}] → ${to}: ${text} (MR: ${recognition.toFixed(2)})`);

    // Notify callback
    if (this.onMessage) {
      this.onMessage(message);
    }

    return message;
  }

  /**
   * Allocate recognition to another user
   */
  async recognizeUser(userId: string, amount: number) {
    await this.api.allocateRecognition(userId, amount);
    const mr = await this.api.getMutualRecognition(userId);
    console.log(`[${this.entityId}] Recognized ${userId}: ${amount} (MR now: ${mr.toFixed(2)})`);
  }

  /**
   * Get mutual recognition with user
   */
  async getMutualRecognition(userId: string): Promise<number> {
    return await this.api.getMutualRecognition(userId);
  }

  /**
   * Get message history
   */
  getMessages(): ChatMessage[] {
    return [...this.messages];
  }

  /**
   * Subscribe to incoming messages
   */
  onIncomingMessage(callback: (message: ChatMessage) => void) {
    this.onMessage = callback;
  }
}

/**
 * Example: Two users chatting
 */
export async function runChatExample() {
  console.log('=== Recognition-Based Chat Example ===\n');

  // Create two chat clients (no initialize needed!)
  const alice = new ChatClient('alice', 'ws://localhost:8080');
  const bob = new ChatClient('bob', 'ws://localhost:8080');

  // Start both clients
  await alice.start();
  await bob.start();

  console.log('\n--- Initial Recognition ---');

  // Alice recognizes Bob
  await alice.recognizeUser('bob', 0.7);

  // Bob recognizes Alice
  await bob.recognizeUser('alice', 0.5);

  // Check mutual recognition
  const mr = await alice.getMutualRecognition('bob');
  console.log(`\nMutual recognition: ${mr.toFixed(2)}`);

  console.log('\n--- Chat Messages ---');

  // Alice sends message to Bob
  await alice.sendMessage('bob', 'Hey Bob! How are you?');

  // Bob sends message to Alice
  await bob.sendMessage('alice', 'Hi Alice! I\'m great, thanks!');

  // Alice increases recognition
  await alice.recognizeUser('bob', 0.9);

  // Check new mutual recognition
  const newMR = await alice.getMutualRecognition('bob');
  console.log(`\nMutual recognition increased to: ${newMR.toFixed(2)}`);

  // More messages
  await alice.sendMessage('bob', 'Great to hear! Want to collaborate?');
  await bob.sendMessage('alice', 'Absolutely! Let\'s do it.');

  console.log('\n--- Message History ---');
  console.log('Alice\'s messages:', alice.getMessages().length);
  console.log('Bob\'s messages:', bob.getMessages().length);

  console.log('\n=== Chat Example Complete ===');
}

/**
 * Example: Group chat with recognition threshold
 */
export async function runGroupChatExample() {
  console.log('=== Group Chat with Recognition Threshold ===\n');

  const users = [
    new ChatClient('alice', 'ws://localhost:8080'),
    new ChatClient('bob', 'ws://localhost:8080'),
    new ChatClient('charlie', 'ws://localhost:8080'),
    new ChatClient('diana', 'ws://localhost:8080')
  ];

  // Start all users
  await Promise.all(users.map(u => u.start()));

  console.log('--- Building Recognition Network ---');

  // Create recognition network
  await users[0].recognizeUser('bob', 0.8);      // alice → bob
  await users[0].recognizeUser('charlie', 0.6);  // alice → charlie
  await users[0].recognizeUser('diana', 0.3);    // alice → diana

  await users[1].recognizeUser('alice', 0.7);    // bob → alice
  await users[1].recognizeUser('charlie', 0.9);  // bob → charlie
  await users[1].recognizeUser('diana', 0.4);    // bob → diana

  await users[2].recognizeUser('alice', 0.5);    // charlie → alice
  await users[2].recognizeUser('bob', 0.8);      // charlie → bob
  await users[2].recognizeUser('diana', 0.7);    // charlie → diana

  await users[3].recognizeUser('alice', 0.2);    // diana → alice
  await users[3].recognizeUser('bob', 0.5);      // diana → bob
  await users[3].recognizeUser('charlie', 0.6);  // diana → charlie

  console.log('\n--- Recognition Levels ---');

  // Check recognition levels
  for (let i = 0; i < users.length; i++) {
    for (let j = i + 1; j < users.length; j++) {
      const user1 = users[i];
      const user2 = users[j];
      const mr = await user1.getMutualRecognition(user2.entityId);
      console.log(`${user1.entityId} ↔ ${user2.entityId}: ${mr.toFixed(2)}`);
    }
  }

  console.log('\n--- Filtered Chat (MR > 0.5) ---');

  // Only send messages where MR > 0.5
  const threshold = 0.5;

  for (const user of users) {
    for (const other of users) {
      if (user === other) continue;
      
      const mr = await user.getMutualRecognition(other.entityId);
      if (mr >= threshold) {
        await user.sendMessage(other.entityId, `Hello! We have good MR: ${mr.toFixed(2)}`);
      }
    }
  }

  console.log('\n=== Group Chat Example Complete ===');
}

// Run if called directly
if (import.meta.url === `file://${process.argv[1]}`) {
  runChatExample()
    .then(() => runGroupChatExample())
    .catch(console.error);
}

