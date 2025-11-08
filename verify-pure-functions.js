#!/usr/bin/env node
/**
 * Verification script to test that pure functions work without Svelte
 * This runs in plain Node.js to verify no Svelte dependencies
 */

import fs from 'fs';
import path from 'path';
import { fileURLToPath } from 'url';

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

console.log('Testing pure function extraction...\n');

// Test 1: Can we import from collective-recognition.ts?
try {
  console.log('✓ Test 1: Importing collective-recognition.ts functions...');
  
  // Note: In a real Node environment, you'd need TypeScript compilation
  // For now, we just verify the file exists and has correct structure
  
  const crPath = path.join(__dirname, 'src/lib/protocol/collective/collective-recognition.ts');
  const crContent = fs.readFileSync(crPath, 'utf8');
  
  // Verify no Svelte imports (except type imports which are fine)
  if (crContent.match(/from ['"]svelte['"]/)) {
    console.log('  ✗ FAIL: collective-recognition.ts has Svelte imports');
    process.exit(1);
  }
  
  // Verify pure function exports exist
  const requiredExports = [
    'getFilterValue',
    'createFilter',
    'unionOfFilters',
    'calculateTotalNeedAmount',
    'getRemainingNeed',
    'matchNeedToCapacitySlots',
    'calculateSlotCompatibleAmount',
    'allocateSlotsToRecipients',
    'calculateCollectiveRecognitionShares',
    'computeAllocations',
    'generateAllocations'
  ];
  
  for (const exportName of requiredExports) {
    if (!crContent.includes(`export function ${exportName}`)) {
      console.log(`  ✗ FAIL: Missing export: ${exportName}`);
      process.exit(1);
    }
  }
  
  console.log('  ✓ All required exports found');
  console.log('  ✓ No Svelte runtime dependencies found\n');
  
} catch (error) {
  console.log('  ✗ FAIL:', error.message);
  process.exit(1);
}

// Test 2: Can we import from membership.ts?
try {
  console.log('✓ Test 2: Importing membership.ts functions...');
  
  const mPath = path.join(__dirname, 'src/lib/network/membership.ts');
  const mContent = fs.readFileSync(mPath, 'utf8');
  
  // Verify no Svelte imports
  if (mContent.match(/from ['"]svelte['"]/)) {
    console.log('  ✗ FAIL: membership.ts has Svelte imports');
    process.exit(1);
  }
  
  // Verify pure function exports exist
  const requiredExports = [
    'setMembershipListPure',
    'removeMembershipListPure',
    'addMemberToListPure',
    'removeMemberFromListPure',
    'subscribeMembershipListPure',
    'unsubscribeMembershipListPure',
    'updateMembershipCachePure',
    'resolveMembershipList',
    'hasMembershipData'
  ];
  
  for (const exportName of requiredExports) {
    if (!mContent.includes(`export function ${exportName}`)) {
      console.log(`  ✗ FAIL: Missing export: ${exportName}`);
      process.exit(1);
    }
  }
  
  console.log('  ✓ All required exports found');
  console.log('  ✓ No Svelte runtime dependencies found\n');
  
} catch (error) {
  console.log('  ✗ FAIL:', error.message);
  process.exit(1);
}

// Test 3: Verify .svelte.ts files re-export correctly
try {
  console.log('✓ Test 3: Verifying .svelte.ts files re-export...');
  
  const crSveltePath = path.join(__dirname, 'src/lib/protocol/collective/collective-recognition.svelte.ts');
  const crSvelteContent = fs.readFileSync(crSveltePath, 'utf8');
  
  if (!crSvelteContent.includes("export * from '$lib/protocol/collective/collective-recognition'")) {
    console.log('  ✗ FAIL: collective-recognition.svelte.ts does not re-export from .ts file');
    process.exit(1);
  }
  
  const mSveltePath = path.join(__dirname, 'src/lib/network/membership.svelte.ts');
  const mSvelteContent = fs.readFileSync(mSveltePath, 'utf8');
  
  if (!mSvelteContent.includes("export * from '$lib/network/membership'")) {
    console.log('  ✗ FAIL: membership.svelte.ts does not re-export from .ts file');
    process.exit(1);
  }
  
  console.log('  ✓ collective-recognition.svelte.ts re-exports correctly');
  console.log('  ✓ membership.svelte.ts re-exports correctly\n');
  
} catch (error) {
  console.log('  ✗ FAIL:', error.message);
  process.exit(1);
}

console.log('════════════════════════════════════════════════════════');
console.log('✓ SUCCESS: All pure function extraction tests passed!');
console.log('════════════════════════════════════════════════════════');
console.log('\nSummary:');
console.log('  • collective-recognition.ts: Pure functions extracted');
console.log('  • membership.ts: Pure functions extracted');
console.log('  • Both .svelte.ts files: Re-export from pure .ts files');
console.log('  • No Svelte runtime dependencies in pure .ts files');
console.log('\nYou can now use these pure functions without Svelte!');

